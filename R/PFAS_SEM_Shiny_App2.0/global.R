library(readr)
library(here)
library(tidyverse)
alldata <- read.csv(here("data","alldata.csv"), skip = 0)
dim(alldata) #[1] 6122 43

mdata <- read.csv(here("data","main.csv"), encoding = "UTF-8", skip = 0)
dim(mdata) #[1] 6122 43
mdata <- 
  mdata %>%
  select(-ends_with("checked")) %>% # Remove columns ending with "checked" 
  select(-("Timestamp")) %>% # Remove "Timestamp" columns
  select(-starts_with("Initials")) %>%  # Remove columns starting with "Initials"
  select(-ends_with("comment")) %>% # Remove columns ending with "comment"
  mutate(First_author = sub("_\\d+", "", Author_year)) %>% 
  mutate("Title" = Paper_title) %>% 
  mutate("Country" = Country_firstAuthor) %>% 
  select(c("First_author", "Title", "Publication_year", "Journal", "Country", "Human_animal_environment", "DOI")) %>%
  mutate(Journal = tolower(Journal))  # Remove capital letters
dim(mdata) #[1] 175  32