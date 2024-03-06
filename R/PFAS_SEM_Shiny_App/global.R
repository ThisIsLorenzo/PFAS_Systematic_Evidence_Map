library(dplyr)
library(plotly)
library(here)
library(tidyverse)
library(skimr)
library(data.table)
library(DT)
library(RColorBrewer)
library(bibliometrix)
library(circlize)

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
cbp1 <- c("#707070",
          "#999999",
          "#CCCCCC",
          "#F5F5F5")
cbp2 <- c("#A29F15",
            "#B8B8B8",
            "#3C7A89",
            "#938274")
cbp3 <- c("#D2664B",
          "#E6AF2E",
          "#B8B8B8")

cbp4 <- c("#588157",
          "#E6AF2E",
          "#DE6449",
          "#B8B8B8")

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

#Merge main dataframe with species info dataframe
mspdata <- left_join(mdata, spdata, by = "Study_ID")

my_data <- list(review = mdata,
                species = spdata,
                AMSTAR2 = qdata,
                PFAS = ptidata,
                biblio = bib_data,
                biblio_all = bib_data_all_types)

# Removing weird symbols
my_data$review$Paper_title <- str_replace_all(my_data$review$Paper_title, "â€”" , "—")
my_data$review$Paper_title <- str_replace_all(my_data$review$Paper_title, "â€\u0090" , "-")
my_data$review$Paper_title <- str_replace_all(my_data$review$Paper_title, "â€™" , "'")
my_data$review$Paper_title <- str_replace_all(my_data$review$Paper_title, "âˆ’" , "−")
my_data$review$Author_year <- str_replace_all(my_data$review$Author_year, "Ã³" , "ó")
my_data$review$Author_year <- str_replace_all(my_data$review$Author_year, "Ã©" , "é")
my_data$review$Author_year <- str_replace_all(my_data$review$Author_year, "Ã¡" , "á")
my_data$review$Author_year <- str_replace_all(my_data$review$Author_year, "Ã¤" , "ä")


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


# Make a list of variable available for MAPPING
choices_mapping <- function() {
  choices <- list(
    "*No variable selected*" = "NULL",
    "Review title" = "Paper_title",
    "Journal name" = "Journal",
    "Country of first author" = "Country_firstAuthor",
    "Type of review" = "Review_type_claimed",
    "Systematic approach" = "Systematic_approach",
    "Protocol" = "Protocol",
    "Meta-analysis" = "Meta_analysis",
    "Review subject" = "Human_animal_environment",
    "Type of environment" = "Environment_type",
    "Reporting guidelines" = "Reporting_guideline",
    "Conflict of interest statement" = "COI_statement",
    "potential Conflict of interest" = "COI_present",
    "Funding statement" = "Funding_statement",
    "No-profit funding" = "No_profit_funding",
    "For-profit funding" = "Industry_funding",
    "Raw data availability" = "Raw_data",
    "Analysis code availability" = "Analysis_code",
    "DOI" = "DOI",
    "PFAS type" = "PFAS_type",
    "PFAS full name" = "PFAS_full_namee",
    "Focus on PFAS?" = "PFAS_focus",
    "How many PFAS?" = "PFAS_one_many",
    "Species scientific name" = "Species_scientific_name",
    "Species common name" = "Species_common_name",
    "Classes scientific name" = "Class_scientific_name",
    "How many species?" = "Species_one_many",
    "Animal study type" = "Lab_domestic_wildlife_mixed"
  )
  return(choices)
}


# SUMMARY


# Creating histograms
# Time trends
# histplot1

review_trend <- mdata %>% 
  group_by(Review_type_claimed) %>% 
  count(Publication_year)

# Rename the column
review_trend <- review_trend %>% rename(Review_type = Review_type_claimed)

p1.1 <- review_trend %>%
  ggplot(aes(x = Publication_year,
             y = n
  )) +
  geom_col(aes(
    fill = Review_type),
    width = 0.8,
    linewidth = 0.2) +
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
        legend.position = "none",
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
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(fill = "Review type claimed:")

# histplot2

srma_trend <- mdata %>% 
  group_by(Meta_analysis) %>%
  count(Publication_year)

# Convert Human_animal_environment to factor with specified levels
srma_trend$Meta_analysis <- factor(srma_trend$Meta_analysis, 
                                      levels = c("Yes","No"))

p1.2 <- srma_trend %>%
  ggplot(aes(x = Publication_year,
             y = n
  )) +
  geom_col(aes(fill = Meta_analysis), 
           alpha = 0.8,
           width = 0.8,
           size = 0.2) +
  scale_x_continuous(name = "Year",
                     breaks = seq(2008, 2022, by = 1),
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Number of reviews",
                     breaks = seq(0, 190, by = 10),
                     expand = c(0,1),
                     minor_breaks = NULL) +
  scale_fill_manual(values = c("#DE6449", "#588157"),
                    breaks = c("No","Yes")) +
  scale_color_manual(values = cbp1,
                     breaks = c("No","Yes")) +
  theme_light() +
  theme(plot.title = element_text(size = 10),
        legend.position = "none",
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
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 9,
                                    hjust = 0.5),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(fill = "Quantitative synthesis:")


# histplot3


trend_subjects <- mdata %>% 
  filter(Human_animal_environment != "Plants") %>% 
  group_by(Human_animal_environment) %>% 
  count(Publication_year)

p1.3 <-  trend_subjects %>%
  ggplot(aes(x = Publication_year,
             y = n)) +
  geom_col(aes(
    fill = fct_relevel(Human_animal_environment, c("Mixed",
                                                   "Environment",
                                                   "Animals",
                                                   "Humans")),
    color = "black"), 
    alpha = 0.8,
    width = 0.8,
    size = 0.2) +
  scale_x_continuous(name = "Year",
                     breaks = seq(2008, 2022, by = 1),
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Number of reviews",
                     breaks = seq(0, 80, by = 10),
                     minor_breaks = NULL,
                     expand = c(0,1)
  ) +
  scale_color_manual(values = cbp2,
                     breaks=c("Humans", 
                              "Animals", 
                              "Environment",
                              "Mixed")) +
  scale_fill_manual(values = cbp2,
                    breaks=c("Humans", 
                             "Animals", 
                             "Environment",
                             "Mixed")) +
  theme_light() + 
  theme(plot.title = element_text(size = 10),
        legend.justification = c(-0.1, 1.1),
        legend.position = "none",
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
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(fill = "Review subject:")

legacy_novel_trend <- mpidata[!duplicated(mpidata[, c("Study_ID", "Legacy_novel")]), ]

legacy_novel_trend <- legacy_novel_trend %>%
  drop_na(Legacy_novel) %>% 
  group_by(Study_ID) %>%
  summarise(Legacy_novel = ifelse(n_distinct(Legacy_novel) > 1, "legacy and novel", Legacy_novel[1]),
            Publication_year = first(Publication_year))

# histplot4

legacy_novel_trend <- legacy_novel_trend %>% 
  group_by(Legacy_novel) %>%
  count(Publication_year)

p1.4 <- legacy_novel_trend %>%
  ggplot(aes(x = Publication_year,
             y = n
  )) +
  geom_col(aes(fill = fct_relevel(Legacy_novel, c("NS",
                                                  "novel",
                                                  "legacy and novel",
                                                  "legacy")),
               color = "black"), 
           alpha = 0.8,
           width = 0.8,
           size = 0.2) +
  scale_x_continuous(name = "Year",
                     breaks = seq(2008, 2022, by = 1),
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Number of reviews",
                     breaks = seq(0, 190, by = 10),
                     expand = c(0,1),
                     minor_breaks = NULL) +
  scale_fill_manual(values = c("#335C67", "#FFF3B0", "#E09F3E", "grey"),
                    breaks = c("legacy",
                               "legacy and novel",
                               "novel",
                               "NS")) +
  scale_color_manual(values = cbp1,
                     breaks = c("legacy",
                                "legacy and novel",
                                "novel",
                                "NS")) +
  theme_light() +
  theme(plot.title = element_text(size = 10),
        legend.justification = c(-0.1, 1.1),
        legend.position = "none",
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
        axis.title.x = element_text(size = 9,
                                    hjust = 0.5),
        axis.title.y = element_text(size = 9),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(fill = "PFAS reviewed:")

#Subjects
# histplot5

t_subject2 <- mdata %>%
  filter(Human_animal_environment != "NA") %>% # Filter out rows with "NA" in 'Human_animal_environment'
  count(Human_animal_environment, Review_type_claimed) %>% # Count occurrences based on 'Human_animal_environment' and 'Review_type_claimed'
  mutate(percentage = round(n / sum(n) * 100)) %>% # Calculate percentage of each count
  mutate(percentage = round( n / sum(n) * 100)) %>% # Calculate percentage of each count
  group_by(Human_animal_environment) %>% # Group by 'Human_animal_environment'
  mutate(n_tot = sum(n)) %>% # Calculate total count per 'Human_animal_environment'
  mutate(tot_percentage = sum(percentage)) # Calculate total percentage for each 'Human_animal_environment'

# Order the levels of 'Human_animal_environment' based on total count
t_subject2$Human_animal_environment <-
  factor(t_subject2$Human_animal_environment, levels = unique(t_subject2$Human_animal_environment[order(t_subject2$n_tot, decreasing = FALSE)]))

p1.5 <- ggplot(t_subject2, aes(x = Human_animal_environment,
                       y = percentage)) +
  geom_col(aes(fill = Review_type_claimed,
               color = "black"), 
           alpha = 0.8,
           width = 0.8,
           size = 0.2) +
  theme_light() +
  coord_flip()+
  scale_x_discrete(name = "Review subject") +
  scale_y_continuous(name = "Percentage of reviews (%)",
                     breaks = seq(0, 80, by = 10),
                     expand = c(0,1),
                     limits = c(0,80)) +
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
                                "critical review",
                                "meta-analysis",
                                "review",
                                "scoping review",
                                "systematic evidence map",
                                "systematic review",
                                "systematic review and meta-analysis")) +
  geom_text(aes(fill = Review_type_claimed,
                label = paste0(n), x = Human_animal_environment), 
            position = position_stack(vjust = 0.5), 
            size = 2.2) + 
  theme(title = element_text(size = 10),
        legend.position = "none",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.15, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.y =  element_text(size = 8),
        axis.text.x =  element_text(size = 8),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(fill = "Review type claimed:")

# histplot6

t_reviewtypes2 <- mdata %>%
  filter(Review_type_claimed != "NA") %>% #filter out NA
  count(Review_type_claimed, Human_animal_environment) %>%
  mutate(percentage = round( n / sum(n) * 100)) %>% 
  group_by(Review_type_claimed) %>% 
  mutate(n_tot = sum(n)) %>% 
  mutate(tot_percentage = sum(percentage))

t_reviewtypes2 <- t_reviewtypes2 %>%
  mutate(Review_type_claimed = case_when(
    Review_type_claimed == "meta-analysis" ~ "MA",
    Review_type_claimed == "systematic review" ~ "SR",
    Review_type_claimed == "systematic evidence map" ~ "SEM",
    Review_type_claimed == "systematic review and meta-analysis" ~ "SR and MA",
    Review_type_claimed == "comprehensive review" ~ "CoR",
    Review_type_claimed == "scoping review" ~ "ScR",
    Review_type_claimed == "critical review" ~ "CrR",
    TRUE ~ Review_type_claimed
  ))

t_reviewtypes2$Review_type_claimed <-
  factor(t_reviewtypes2$Review_type_claimed, levels = unique(t_reviewtypes2$Review_type_claimed[order(t_reviewtypes2$n_tot, decreasing = FALSE)]))

p1.6 <- ggplot(t_reviewtypes2, aes(x = Review_type_claimed,
                                    y = percentage)) +
  geom_col(aes(fill = fct_relevel(Human_animal_environment, c("Mixed",
                                                              "Environment", 
                                                              "Animals", 
                                                              "Humans")),
               color = "black"), 
           alpha = 0.5,
           width = 0.8,
           size = 0.2) +
  theme_light() +
  coord_flip()+
  scale_x_discrete(name = "Review type claimed") +
  scale_y_continuous(name = "Percentage of reviews (%)",
                     breaks = seq(0, 40, by = 10),
                     expand = c(0,1),
                     limits = c(0,40)) +
  scale_fill_manual(values=cbp2,
                    breaks=c("Humans", 
                             "Animals", 
                             "Environment",
                             "Mixed")) +
  scale_color_manual(values=cbp2,
                     breaks=c("Humans", 
                              "Animals", 
                              "Environment",
                              "Mixed")) +
  geom_text(aes(fill = fct_relevel(Human_animal_environment, c("Mixed",
                                                               "Environment", 
                                                               "Animals", 
                                                               "Humans")),
                label = paste0(n), x = Review_type_claimed), 
            position = position_stack(vjust = 0.5), 
            size = 2.2) + 
  geom_text(aes(label = paste("n =", sprintf("%.0f",n_tot)),
                x = Review_type_claimed,
                y =  max(tot_percentage)), 
            position = position_stack(vjust = 1.1), 
            size = 2.5,
            color = "black") +
  theme(title = element_text(size = 10),
        legend.position = "none",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.15, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(fill = "Review subject:")

# histplot7
t_sys_appr2 <-
  mdata %>%
  filter(Systematic_approach != "NA") %>% 
  count(Human_animal_environment, Systematic_approach) %>%
  arrange(desc(n)) %>% 
  mutate(percentage = round( n / sum(n) * 100)) %>% 
  group_by(Human_animal_environment) %>% 
  mutate(n_tot = sum(n))

t_sys_appr2$Human_animal_environment <-
  factor(t_sys_appr2$Human_animal_environment, levels = unique(t_sys_appr2$Human_animal_environment[order(t_sys_appr2$n_tot, decreasing = FALSE)]))

p1.7 <- 
  ggplot(t_sys_appr2, aes(x = Human_animal_environment,
                          y = percentage)) +
  geom_col(aes(fill = fct_relevel(Systematic_approach, c("Yes", "No")),
               color = "black"), 
           alpha = 0.5,
           width = 0.8,
           size = 0.2) +
  scale_fill_manual(values=c("#DE6449", "#588157"),
                    breaks=c("No", "Yes")) +
  scale_color_manual(values=c("#DE6449", "#588157"),
                     breaks=c("No", "Yes")) +
  theme_light() +
  coord_flip() +
  scale_x_discrete(name = "Systematic approach") +
  scale_y_continuous(name = "Percentage of reviews (%)",
                     breaks = seq(0, 70, by = 10),
                     expand = c(0,1),
                     limits = c(0,70)) +
  geom_text(aes(fill = fct_relevel(Systematic_approach, c("Yes", "No")),
                label = paste0(n), x = Human_animal_environment), 
            position = position_stack(vjust = 0.5), 
            size = 2.2) + 
  theme(title = element_text(size = 10),
        legend.position = "none",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.15, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(fill = "Systematic method:")

# histplot8
t_sys_appr3 <-
  mdata %>%
  filter(Systematic_approach != "NA") %>% 
  count(Review_type_claimed, Systematic_approach) %>%
  arrange(desc(n)) %>% 
  mutate(percentage = round( n / sum(n) * 100)) %>% 
  group_by(Review_type_claimed) %>% 
  mutate(n_tot = sum(n))

t_sys_appr3$Review_type_claimed <-
  factor(t_sys_appr3$Review_type_claimed, levels = unique(t_sys_appr3$Review_type_claimed[order(t_sys_appr3$n_tot, decreasing = FALSE)]))

p1.8 <- 
  ggplot(t_sys_appr3, aes(x = Review_type_claimed,
                          y = percentage)) +
  geom_col(aes(fill = fct_relevel(Systematic_approach, c("Yes", "No")),
               color = "black"), 
           alpha = 0.5,
           width = 0.8,
           size = 0.2) +
  scale_fill_manual(values=c("#DE6449", "#588157"),
                    breaks=c("No", "Yes")) +
  scale_color_manual(values=c("#DE6449", "#588157"),
                     breaks=c("No", "Yes")) +
  theme_light() +
  coord_flip() +
  scale_x_discrete(name = "Systematic approach") +
  scale_y_continuous(name = "Percentage of reviews (%)",
                     breaks = seq(0, 35, by = 10),
                     expand = c(0,1),
                     limits = c(0,35)) +
  geom_text(aes(fill = fct_relevel(Systematic_approach, c("Yes", "No")),
                label = paste0(n), x = Review_type_claimed), 
            position = position_stack(vjust = 0.5), 
            size = 2.2) + 
  theme(title = element_text(size = 10),
        legend.position = "none",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.15, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(fill = "Systematic method:")

# PFAS
# histplot9
t2_PFASfocus <-
  mdata %>%
  count(PFAS_focus, PFAS_one_many) %>%
  arrange(desc(n)) %>% 
  filter(PFAS_focus != "NA") %>%  #filter out NA
  mutate(percentage = round( n / sum(n) * 100))

t2_PFASfocus$PFAS_one_many <- ifelse(t2_PFASfocus$PFAS_one_many == 'Not specified', 'NS', t2_PFASfocus$PFAS_one_many) #changing 'not specified' values into 'NS'

t2_PFASfocus <- t2_PFASfocus %>%
  mutate(PFAS_focus = case_when(
    PFAS_focus == "Yes" ~ "PFAS",
    PFAS_focus == "No" ~ "POPs"
  ))

p1.9 <- 
  ggplot(t2_PFASfocus, aes(x = PFAS_focus,
                           y = percentage)) +
  geom_col(aes(fill = fct_relevel(PFAS_one_many, c("NS",
                                                   "Multiple",
                                                   "One")),
               color = "black"), 
           alpha = 0.5,
           width = 0.8,
           size = 0.2) + 
  scale_fill_manual(values = cbp3, 
                    breaks=c('One', 
                             'Multiple',
                             'NS')) +
  scale_color_manual(values = cbp3,
                     breaks=c('One', 
                              'Multiple',
                              'NS')) +
  scale_x_discrete(name = "Review focus") +
  scale_y_continuous(name = "Percentage of reviews (%)",
                     breaks = seq(0, 70, by = 10),
                     expand = c(0,1),
                     limits = c(0,70)) +
  theme_light() +
  coord_flip() +
  geom_text(aes(fill = fct_relevel(PFAS_one_many, c("NS",
                                                    "Multiple", 
                                                    "One")),
                label = paste0(n), x = PFAS_focus),
            position = position_stack(vjust = 0.5),
            size = 2.2) +
  theme(
    title = element_text(size = 10),
    legend.position = "none",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8), 
    legend.key.size = unit(0.2, "cm"),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.background = element_rect(fill = alpha("white", 0.7)),
    panel.grid.major = element_line(color = "grey", 
                                    linewidth  = 0.2, 
                                    linetype = "dashed"),
    panel.grid.minor = element_line(color = "grey", 
                                    linewidth  = 0.2, 
                                    linetype = "dotted")
  ) +
  labs(fill = "PFAS reviewed:")

#histplot10
t1 <-
  mdata %>%
  count(Human_animal_environment, PFAS_one_many) %>% 
  mutate(percentage = round( n / sum(n) * 100)) %>% 
  mutate(PFAS_one_many = case_when(
    PFAS_one_many == "Not specified" ~ "NS",
    TRUE ~ PFAS_one_many
  ))

# Convert Human_animal_environment to factor with specified levels
t1$Human_animal_environment <- factor(t1$Human_animal_environment, 
                                      levels = c("Mixed", "Environment", "Animals", "Humans"))
# Rename the column
t1 <- t1 %>% rename(Subject = Human_animal_environment)

p1.10 <- ggplot(t1, aes(x = PFAS_one_many,
                      y = percentage)) +
  coord_flip()  +
  geom_col(aes(fill = Subject),
           alpha = 0.5,
           width = 0.8,
           size = 0.2) +
  #geom_text(aes(label = n), position = position_stack(vjust = 0.2)) +
  scale_fill_manual(values=cbp2,
                    breaks=c("Humans", 
                             "Animals",
                             "Environment",
                             "Mixed")) +
  scale_color_manual(values = cbp2,
                     breaks=c("Humans", 
                              "Animals", 
                              "Environment",
                              "Mixed")) +
  theme_light() +  
  theme(title = element_text(size = 10),
        legend.position = "none",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.2, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text = element_text(size = 8),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  ylab("Article count") +
  scale_x_discrete(name = "PFAS reviewed") +
  scale_y_continuous(name = "Percentage of reviews (%)",
                     breaks = seq(0, 70, by = 10),
                     expand = c(0,1),
                     limits = c(0,70)) +
  geom_text(aes(fill = fct_relevel(Subject, c("Mixed",
                                                               "Environment",
                                                               "Animals",
                                                               "Humans")),
                label = paste0(n), x = PFAS_one_many),
            position = position_stack(vjust = 0.5),
            size = 2.2) +
  labs(fill = "Review subject:")

# histplot11
t_PFAStype <- ptidata %>%
  filter(PFAS_type != "NA") %>% #filter out NA
  count(PFAS_type) %>%
  mutate(percent = (n / 175 * 100)) #109 is the number of SRs included in this review

t_PFAStype$PFAS_type <-
  factor(t_PFAStype$PFAS_type, levels = t_PFAStype$PFAS_type[order(t_PFAStype$percent, decreasing = FALSE)])

PFAS_levels_order <-
  t_PFAStype$PFAS_type[order(t_PFAStype$percent, decreasing = FALSE)] #save for another plot

#str(mpidata)
t_PFAStype2 <-
  mpidata %>%
  count(PFAS_type, Human_animal_environment) %>%
  arrange(desc(n)) %>%
  filter(PFAS_type != "NA") %>%  #filter out NA
  mutate(percentage = ( n / 175 * 100))

#t_PFAStype$PFAS_type <- factor(t_PFAStype$PFAS_type, levels = unique(t_PFAStype$PFAS_type[order(t_PFAStype$n, decreasing = FALSE)]))
t_PFAStype2$PFAS_type <-
  factor(t_PFAStype2$PFAS_type, levels = PFAS_levels_order) #use order from the previous graph


p1.11 <-
  ggplot(t_PFAStype2,
         aes(x = PFAS_type,
             y = percentage)) +
  coord_flip()  +
  geom_col(aes(fill = fct_relevel(Human_animal_environment, c("Mixed",
                                                              "Environment",
                                                              "Animals",
                                                              "Humans")),
               color = "black"), 
           alpha = 0.5,
           width = 0.8,
           size = 0.2) +
  theme_light() +
  scale_fill_manual(values = cbp2,
                    breaks = c("Humans",
                               "Animals",
                               "Environment",
                               "Mixed")) +
  scale_color_manual(values = cbp2,
                     breaks = c("Humans", 
                                "Animals", 
                                "Environment",
                                "Mixed")) +
  scale_x_discrete(name = "PFAS types") +
  scale_y_continuous(name = "Percentage of reviews (%)",
                     breaks = seq(0, 80, by = 10),
                     expand = c(0,1),
                     limits = c(0,80)) +
  geom_text(aes(fill = fct_relevel(Human_animal_environment, c("Mixed",
                                                               "Environment",
                                                               "Animals",
                                                               "Humans")),
                label = paste0(n), x = PFAS_type),
            position = position_stack(vjust = 0.5),
            size = 2.2) +
  theme(
    title = element_text(size = 10),
    legend.position = "none",
    legend.background = element_rect(fill = alpha("white", 0.7)),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.2, "cm"),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 8),
    panel.grid.major = element_line(color = "grey", 
                                    linewidth  = 0.2, 
                                    linetype = "dashed"),
    panel.grid.minor = element_line(color = "grey", 
                                    linewidth  = 0.2, 
                                    linetype = "dotted")) +
  labs(fill = "Review subject:")



## CRITICAL APPRAISAL - histplot6

#only select columns with assessment codes (drop comments and empty rows)
qtable <- 
  qdata %>%
  select(starts_with("Q") & !ends_with("_Comment"))  

#simiplify column names to Q+number format
names(qtable) <- 
  gsub("\\..*", "", names(qtable)) 

#simplify all the answers to short strings (version for a single column: gsub(" =.*", "", qtable$Q1)):
qtable <- 
  apply(qtable, 2, function(y) as.character(gsub(" =.*", "", y)))

#save studies in a new vector
studies <- 
  qdata$Study_ID[!is.na(qdata$Study_ID)]

#convert to long format data frame with Study_ID
qtable_long <-
  data.frame(study = studies,
             question = rep(colnames(qtable),each = length(studies)),
             score = as.vector(qtable), stringsAsFactors = TRUE) #make long format table

rownames(qtable_long) <- NULL
qtable_long$question <- 
  factor(qtable_long$question, levels(qtable_long$question)[rev(c(1,9:16,2:8))]) #setting the order of levels - by Q-number

#add a column with verbal expression of scores:   
qtable_long <- qtable_long %>% 
  mutate(score_word = case_when(
    score == "1" ~ "High",
    score == "0.5" ~ "Medium",
    score == "0" ~ "Low",
    score == "N/A" ~ "NA",
    TRUE ~ as.character(score)
  ))

levels(qtable_long$score_word) <- c("Low",
                                    "Medium",
                                    "High", 
                                    "NA")

qtable_long <- qtable_long %>% 
  mutate(label = case_when(
    question == "Q1" ~ "Q1: research question and criteria",
    question == "Q2" ~ "Q2: a priori protocol",
    question == "Q3" ~ "Q3: included studies design",
    question == "Q4" ~ "Q4: comprehensive search",
    question == "Q5" ~ "Q5: selection duplicated",
    question == "Q6" ~ "Q6: extraction duplicated",
    question == "Q7" ~ "Q7: list of excluded studies",
    question == "Q8" ~ "Q8: summary of included studies",
    question == "Q9" ~ "Q9: critical appraisal",
    question == "Q10" ~ "Q10: included studies' funding",
    question == "Q11" ~ "Q11: appropriate meta-analysis",
    question == "Q12" ~ "Q12: evaluation of the RoB impacts",
    question == "Q13" ~ "Q13: RoB in individual studies",
    question == "Q14" ~ "Q14: variability investigation",
    question == "Q15" ~ "Q15: publication bias",
    question == "Q16" ~ "Q16: conflict of interest"
  ))

qtable_long$label <-
  factor(qtable_long$label, levels = unique(qtable_long$label[order(qtable_long$question, decreasing = FALSE)]))

q1 <-
  ggplot(data = qtable_long) +
  geom_bar(mapping = aes(x = label, 
                         fill = fct_relevel(score_word, c("NA", "Low",
                                                          "Medium",
                                                          "High"))),
           alpha = 0.8,
           width = 0.8,
           size = 0.2, 
           position = "fill",
           color = "black") + 
  coord_flip(ylim = c(0, 1)) +
  guides(fill = guide_legend(reverse = F)) +
  scale_fill_manual("Methodological quality:",
                    values = cbp4,
                    breaks = c("High",
                               "Medium",
                               "Low",
                               "NA")) +
  scale_y_continuous(name = "Percentage of reviews",
                     labels = scales::percent, 
                     expand = c(0.02,0)) +
  scale_x_discrete(name = "AMSTAR2 Question",
                   expand = c(0,0)) +
  theme(axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 9, 
                                   color = "black",
                                   hjust = 0.5),
        axis.text.y = element_text(size = 9, 
                                   color = "black",
                                   vjust = 0.5 ),
        axis.line.x = element_line(linewidth = 0.5,
                                   colour = "black", 
                                   linetype = "solid"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.background = element_rect(linetype = "solid", 
                                         colour = "black",
                                         size = 0.2),
        legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 9))

# GEOMLINE 1

rep_guideline <- mdata %>% 
  group_by(Reporting_guideline) %>% 
  count(Publication_year) %>% 
  mutate(cumulative = cumsum(n))

q2.1 <-  rep_guideline %>%
  ggplot(aes(x = Publication_year,
             y = cumulative,
             color = reorder(Reporting_guideline, -cumulative))) +
  geom_line(size = 0.3) + 
  geom_point(aes(color = Reporting_guideline),
             size = 1.5, 
             shape = 19, 
             fill = "white") +
  scale_x_continuous(name = "Year",
                     breaks = seq(2008, 2022, by = 1),
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Number of reviews",
                     breaks = seq(0, 150, by = 15),
                     limits = c(0,150),
                     minor_breaks = NULL
  ) +
  scale_color_manual(values = c("Yes" = "#588157",
                                "No" = "#DE6449")) +
  theme_light() + 
  theme(plot.title = element_text(size = 9),
        legend.position = "none",
        legend.justification = c(-0.1, 1.1),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.2, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        axis.text.x = element_text(size = 8,
                                   margin = margin(t = 5, 
                                                   r = 0,
                                                   l = 0),
                                   angle = 45),
        axis.text.y = element_text(size = 8,
                                   margin = margin(t = 0, 
                                                   r = 5, 
                                                   l = 5)),
        axis.title.x = element_text(size = 9,
                                    hjust = 0.5),
        axis.title.y = element_text(size = 9),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(color = "Reporting guideline")

# GEOMLINE 2
coi_statement <- mdata %>% 
  group_by(COI_statement) %>% 
  count(Publication_year) %>% 
  mutate(cumulative = cumsum(n))

q2.2 <-  coi_statement %>%
  ggplot(aes(x = Publication_year,
             y = cumulative,
             color = reorder(COI_statement, -cumulative))) +
  geom_line(size = 0.3) + 
  geom_point(aes(color = COI_statement),
             size = 1.8,
             shape = 19,
             fill = "white") +
  geom_text(aes(label = n, x = Publication_year, y = cumulative), 
            size = 2, 
            hjust = 1.2,
            vjust = -0.5) +
  scale_x_continuous(name = "Year",
                     breaks = seq(2008, 2022, by = 1),
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Number of reviews",
                     breaks = seq(0, 150, by = 15),
                     limits = c(0,150),
                     minor_breaks = NULL
  ) +
  scale_color_manual(values = c("Yes" = "#588157",
                                "No" = "#DE6449")) +
  theme_light() + 
  theme(plot.title = element_text(size = 9),
        legend.position = "none",
        legend.justification = c(-0.1, 1.1),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        axis.text.x = element_text(size = 7,
                                   margin = margin(t = 5, 
                                                   r = 0,
                                                   l = 0),
                                   angle = 45),
        axis.text.y = element_text(size = 8,
                                   margin = margin(t = 0, 
                                                   r = 5, 
                                                   l = 5)),
        axis.title.x = element_text(size = 9,
                                    hjust = 0.5),
        axis.title.y = element_text(size = 9),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(color = "COI statement")

# GEOMLINE 3
coi_present <- mdata %>% 
  group_by(COI_present) %>% 
  count(Publication_year) %>% 
  mutate(cumulative = cumsum(n))

coi_present$COI_present[is.na(coi_present$COI_present)] <- "NP"

q2.3 <-  coi_present %>%
  ggplot(aes(x = Publication_year,
             y = cumulative,
             color = reorder(COI_present, -cumulative))) +
  geom_line(size = 0.3) + 
  geom_point(aes(color = COI_present), 
             size = 1.8, 
             shape = 19, 
             fill = "white") +
  geom_text(aes(label = n, x = Publication_year, y = cumulative), 
            size = 2, 
            hjust = 1.2,
            vjust = -0.5) +
  scale_x_continuous(name = "Year",
                     breaks = seq(2008, 2022, by = 1),
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Number of reviews",
                     breaks = seq(0, 150, by = 15),
                     limits = c(0,150),
                     minor_breaks = NULL
  ) +
  scale_color_manual(values = c("Yes" = "#588157",
                                "No" = "#DE6449",
                                "NP" = "#B8B8B8")) +
  theme_light() +
  theme(plot.title = element_text(size = 9),
        legend.position = "none",
        legend.justification = c(-0.1, 1.1),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        axis.text.x = element_text(size = 7,
                                   margin = margin(t = 5, 
                                                   r = 0,
                                                   l = 0),
                                   angle = 45),
        axis.text.y = element_text(size = 8,
                                   margin = margin(t = 0, 
                                                   r = 5, 
                                                   l = 5)),
        axis.title.x = element_text(size = 9,
                                    hjust = 0.5),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(color = "COI present")

# GEOMLINE 4
funding_statement <- mdata %>% 
  group_by(Funding_statement) %>% 
  count(Publication_year) %>% 
  mutate(cumulative = cumsum(n))

q2.4 <-  funding_statement %>%
  ggplot(aes(x = Publication_year,
             y = cumulative,
             color = reorder(Funding_statement, -cumulative))) +
  geom_line(size = 0.3) + 
  geom_point(aes(color = Funding_statement),
             size = 1.8, 
             shape = 19,
             fill = "white") +
  geom_text(aes(label = n, x = Publication_year, y = cumulative), 
            size = 2, 
            hjust = 1.2,
            vjust = -0.5) +
  scale_x_continuous(name = "Year",
                     breaks = seq(2008, 2022, by = 1),
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Number of reviews",
                     breaks = seq(0, 150, by = 15),
                     limits = c(0,150),
                     minor_breaks = NULL
  ) +
  scale_color_manual(values = c("Yes" = "#588157",
                                "No" = "#DE6449")) +
  theme_light() +
  theme(plot.title = element_text(size = 9),
        legend.position = "none",
        legend.justification = c(-0.1, 1.1),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        axis.text.x = element_text(size = 7,
                                   margin = margin(t = 5, 
                                                   r = 0,
                                                   l = 0),
                                   angle = 45),
        axis.text.y = element_text(size = 8,
                                   margin = margin(t = 0, 
                                                   r = 5, 
                                                   l = 5)),
        axis.title.x = element_text(size = 9,
                                    hjust = 0.5),
        axis.title.y = element_text(size = 9),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(color = "Funding statement")

# GEOMLINE 5
funding1 <- mdata %>% 
  filter(No_profit_funding == "Yes") %>% 
  group_by(No_profit_funding) %>%
  count(Publication_year) %>% 
  mutate(cumulative1 = cumsum(n))

funding2 <- mdata %>% 
  filter(Industry_funding == "Yes") %>% 
  group_by(Industry_funding) %>%
  mutate(For_profit_funding = Industry_funding) %>% 
  select(-Industry_funding) %>% 
  count(Publication_year) %>% 
  mutate(cumulative2 = cumsum(n))

df <- rbind(funding1, funding2)

df$type <- ifelse(is.na(df$No_profit_funding), "For_profit_funding", "No_profit_funding")

df$cumulative <- ifelse(is.na(df$cumulative1), df$cumulative2, df$cumulative1)

df <- 
  df %>% 
  mutate(type = case_when(
    type == "No_profit_funding" ~ "Nonprofit organization",
    type == "For_profit_funding" ~ "For-profit organization",
    TRUE ~ type
  ))

q2.5 <-  df %>%
  ggplot(aes(x = Publication_year,
             y = cumulative,
             color = reorder(type, -cumulative))) +
  geom_line(size = 0.3) + 
  geom_point(aes(color = type),
             size = 1.8, 
             shape = 19,
             fill = "white") +
  geom_text(aes(label = n, x = Publication_year, y = cumulative), 
            size = 2, 
            hjust = 1.2,
            vjust = -0.5) +
  scale_x_continuous(name = "Year",
                     breaks = seq(2008, 2022, by = 1),
                     minor_breaks = NULL) +
  scale_y_continuous(name = "Number of reviews",
                     breaks = seq(0, 150, by = 15),
                     limits = c(0,150),
                     minor_breaks = NULL
  ) +
  scale_color_manual(values = c("Nonprofit organization" = "#028090",
                                "For-profit organization" = "#D17A22")) +
  theme_light() +
  theme(plot.title = element_text(size = 9),
        legend.position = "none",
        legend.justification = c(-0.1, 1.1),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill = alpha("white", 0.7)),
        axis.text.x = element_text(size = 7,
                                   margin = margin(t = 5, 
                                                   r = 0,
                                                   l = 0),
                                   angle = 45),
        axis.text.y = element_text(size = 8,
                                   margin = margin(t = 0, 
                                                   r = 5, 
                                                   l = 5)),
        axis.title.x = element_text(size = 9,
                                    hjust = 0.5),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dashed"),
        panel.grid.minor = element_line(color = "grey", 
                                        linewidth  = 0.2, 
                                        linetype = "dotted")) +
  labs(color = "Funding source:")