library(dplyr)
# load data and construct final database ----
mdata <- read_csv(here("data","main.csv"), skip = 0)
dim(mdata) #175 rows 38 columns

# Data from .csv file - PFAS_types sheet:
ptdata <- read_csv(here("data","pfas_types.csv"), skip = 0) 
dim(ptdata) #175 rows 5 columns

# Changing to long format (one type per row - one or multiple rows per study):
ptdata <- ptdata %>% separate_rows(PFAS_type, sep=', ')
dim(ptdata) #1032 rows 5 columns

# Critical apprisal(AMSTAR2) from .csv file:
qdata <- read_csv(here("data", "amstar2.csv"), skip = 0)
dim(qdata) #175 rows 36 columns

# Data from .csv file - PFAS_info sheet:
pidata <- read_csv(here("data","pfas_info.csv"), skip = 0) 
dim(pidata) #39 rows 7 columns

# Data from .csv file - Species_info sheet
spdata <- read_csv(here("data","species.csv"), skip = 0) 
dim(spdata) #541 rows 5 columns

mdata_all_types <- read_csv(here("data","main_all_types_review.csv"), skip = 0) 
dim(mdata_all_types) #530 rows 21 columns

bib_data <- read.csv(here("data", "bib.csv"))
dim(bib_data) #169 rows 36 columns

bib_data_all_types <- read.csv(here("data", "bib_all_types.csv"))
dim(bib_data_all_types) #517 rows 36 columns


# Modify the 'mdata' dataframe
mdata <- 
  mdata %>%
  select(-ends_with("checked")) %>% # Remove columns ending with "checked" 
  select(-("Timestamp")) %>% # Remove "Timestamp" columns
  select(-starts_with("Initials")) %>%  # Remove columns starting with "Initials"
  select(-ends_with("comment")) %>% # Remove columns ending with "comment"
  drop_na(Author_year) %>% # Remove last two rows because empty
  mutate(Journal = tolower(Journal))  # Remove capital letters
dim(mdata) #183 rows 30 columns

# Modify the 'spdata' dataframe
spdata <- 
  spdata %>%
  select(-ends_with("checked")) %>% # Remove columns ending with "checked"
  select(-ends_with("comment")) # Remove columns ending with "comment"
dim(spdata) #541 rows 4 columns

# Modify the 'ptdata' dataframe
ptdata <- 
  ptdata %>%
  select(-ends_with("checked")) %>% # Remove columns ending with "checked"
  select(-ends_with("comment")) %>% # Remove columns ending with "comment"
  select(-("Timestamp")) %>% # Remove the "Timestamp" column
  select(-starts_with("Initials")) %>%# Remove columns starting with "Initials"
  mutate(PFAS_type = case_when( # Create a new column 'PFAS_type' with modified values
    PFAS_type == "6:2 Cl-PFESA/F-53B" ~ "6:2 Cl-PFESA",
    PFAS_type == "HFPO-DA/GenX" ~ "GenX",
    PFAS_type == "All PFAS" ~ "PFAS",
    TRUE ~ PFAS_type
  ))
dim(ptdata) #1090 rows 2 columns

# Modify the 'pidata' dataframe
pidata <- 
  pidata %>% 
  select(-ends_with("comment")) # Remove columns ending with "comment"

# Merge PFAS types dataframe with PFAS info dataframe and then with main dataframe
ptidata <- left_join(ptdata, pidata, by = "PFAS_type")
dim(ptidata) #1090 rows 7 columns
#names(ptidata)
#str(ptidata)
#View(ptidata)

mpidata <- left_join(mdata, ptidata, by = "Study_ID")
dim(mpidata) #1090 rows 36 columns
#names(mpidata)
#str(mpidata)
#View(mpidata)


my_data <- list(review = mdata, 
                species = spdata,
                AMSTAR2 = qdata, 
                PFAS = ptidata)

# generate a dictionary storing all variable - dataset pairs
vars_sets <- list()
j <- 1
for(set in my_data) {
  for(varname in names(set)[-1]) {
    vars_sets[varname] <- names(my_data)[j]
  }
  
  j <- j+1
}
vars_sets$Study_ID <- "review"

















