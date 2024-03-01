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

# SUMMARY


# Creating histograms

# histplot1

review_trend <- mdata %>% 
  group_by(Review_type_claimed) %>% 
  count(Publication_year)

p1.1 <- review_trend %>%
  ggplot(aes(x = Publication_year,
             y = n
  )) +
  geom_col(aes(
    fill = Review_type_claimed,
    color = "black"), 
    alpha = 0.8,
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

p1.2 <- srma_trend %>%
  ggplot(aes(x = Publication_year,
             y = n
  )) +
  geom_col(aes(fill = fct_relevel(Meta_analysis, c("Yes","No")),
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