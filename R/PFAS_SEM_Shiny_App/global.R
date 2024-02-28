library(dplyr)
library(plotly)
library(here)
library(tidyverse)
library(data.table)
library(DT)
library(RColorBrewer)

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

# Set a personal palette
my_palette <- brewer.pal(n = 10, name = "Set3")

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

qdata <- 
  qdata %>%
  select(-ends_with("checked")) %>% # Remove columns ending with "checked"
  select(-ends_with("comment")) %>% # Remove columns ending with "comment"
  select(-("Timestamp")) %>% 
  select(-starts_with("Initials")) 

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
                PFAS = ptidata,
                biblio = bib_data,
                biblio_all = bib_data_all_types)

# Removing weird symbols
my_data$review$Paper_title <- str_replace_all(my_data$review$Paper_title, "â€”" , "—")
my_data$review$Paper_title <- str_replace_all(my_data$review$Paper_title, "â€\u0090" , "-")

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

# Creating histograms

review_trend <- mdata %>% 
  group_by(Review_type_claimed) %>% 
  count(Publication_year)

p1 <- review_trend %>%
  ggplot(aes(x = Publication_year,
             y = n
  )) +
  labs(title = expression(bold("a)"))) +
  geom_col(aes(
    fill = Review_type_claimed,
    color = "black"), 
    alpha = 0.5,
    width = 0.8,
    linewidth = 0.2) +
  geom_text(aes(fill = Review_type_claimed,
                label = n, 
                x = Publication_year,
                y = n), 
            size = 1.7,
            position = position_stack(vjust = 0.5),
            color = "black") +
  scale_fill_manual(values = my_palette,
                    breaks = c("comprehensive review",
                               "critical review",
                               "meta-analysis",
                               "review",
                               "scoping review",
                               "systematic evidence map",
                               "systematic review",
                               "systematic review and meta-analysis")) +
  scale_color_manual(values = my_palette,
                     breaks = c("comprehensive review",
                                "critical review",
                                "meta-analysis",
                                "review",
                                "scoping review",
                                "systematic evidence map",
                                "systematic review",
                                "systematic review and meta-analysis")) +
  scale_x_continuous(name = "Year",
                     breaks = seq(2008, 2022, by = 1),
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Number of reviews",
                     breaks = seq(0, 190, by = 10),
                     expand = c(0,1),
                     minor_breaks = NULL) +
  theme_light() +
  theme(plot.title = element_text(size = 10),
        legend.position = c(0, 1),
        legend.justification = c(-0.1, 1.1),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.2, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        axis.text.x = element_text(size = 8,
                                   margin = margin(t = 5, 
                                                   r = 0,
                                                   l = 0),
                                   angle = 0),
        axis.text.y = element_text(size = 8,
                                   margin = margin(t = 0, 
                                                   r = 5, 
                                                   l = 5)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(fill = "Review type claimed:")













