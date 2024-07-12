library(readr)
library(here)
library(tidyverse)
alldata <- read.csv(here("data","alldata.csv"), skip = 0)
dim(alldata) #[1] 6122 43

alldata <- alldata %>% 
  mutate(Class_scientific_name = case_when(
  Class_scientific_name == 'Mammal'~ 'Mammalia',
  Class_scientific_name == 'Branchiopods'~ 'Branchiopoda',
  Class_scientific_name == 'Brachiopoda'~ 'Branchiopoda',
  Class_scientific_name == 'Fish'~ 'Actinopterygii',
  Class_scientific_name == 'Amphibian'~ 'Amphibia',
  TRUE ~ Class_scientific_name))

mdata <- read.csv(here("data","main.csv"), encoding = "UTF-8", skip = 0)
dim(mdata) #[1] 6122 43

spdata <- read_csv(here("data","species.csv"), skip = 0)
ptdata <- read_csv(here("data","pfas_types.csv"), skip = 0) 
pidata <- read_csv(here("data","pfas_info.csv"), skip = 0) 
qdata <- read_csv(here("data", "amstar2.csv"), skip = 0)

mspdata <- left_join(mdata, spdata, by = "Study_ID")
dim(mspdata) 

ptidata <- left_join(ptdata, pidata, by = "PFAS_type")
dim(ptidata)

mpidata <- left_join(mdata, ptidata, by = "Study_ID")

mspidata <- left_join(mspdata, ptidata, by = "Study_ID")
dim(mspidata) #684 rows 55 columns

mspiqdata <- left_join(mspidata, qdata, by = "Study_ID")
dim(mspiqdata)

mspiqdata <- 
  mspiqdata %>%
  select(-ends_with("checked")) %>% # Remove columns ending with "checked" 
  select(-starts_with("Timestamp")) %>% # Remove "Timestamp" columns
  select(-starts_with("Initials")) %>%  # Remove columns starting with "Initials"
  select(-ends_with("comment")) %>% # Remove columns ending with "comment"
  mutate(First_author = stringr::str_replace(Author_year, "_\\d+", "")) %>% 
  mutate("Title" = Paper_title) %>% 
  mutate("Country" = Country_firstAuthor) %>% 
  mutate(Journal = tolower(Journal)) %>%  # Remove capital letters
  group_by(Title) %>%
  distinct(Study_ID, .keep_all = TRUE) 
dim(mspiqdata) #[1] 175  61

mspiqdata_pivot <-
  mspiqdata %>% 
  separate_rows(PFAS_type, sep=', ')

mdata <- 
  mpidata %>%
  select(-ends_with("checked")) %>% # Remove columns ending with "checked" 
  #select(-("Timestamp")) %>% # Remove "Timestamp" columns
  select(-starts_with("Initials")) %>%  # Remove columns starting with "Initials"
  select(-ends_with("comment")) %>% # Remove columns ending with "comment"
  mutate(First_author = stringr::str_replace(Author_year, "_\\d+", "")) %>% 
  mutate("Title" = Paper_title) %>% 
  mutate("Country" = Country_firstAuthor) %>% 
  select(c("First_author", "Title", "Publication_year", "Journal", "Country", "Human_animal_environment", "DOI", "PFAS_type")) %>%
  mutate(Journal = tolower(Journal))  # Remove capital letters
dim(mdata) #[1] 175  32

mdata_pivot <- mdata %>% 
  separate_rows(PFAS_type, sep=', ')
