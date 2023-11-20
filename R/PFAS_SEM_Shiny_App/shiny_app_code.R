library(shiny)
library(DT)
library(here)
library(tidyverse)
library(here)
library(readr)
library(tidyverse)
library(curl)


is.notwholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  {
    ### function checks of a value is NOT a "whole" number
    ### in the sense it contains meaningless decimal information
    ### due to floeting point accurracy issues
    
    if (is.numeric(x)) !all(abs(x - round(x)) < tol) else FALSE
  }


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


all_dataframes <- list(review = mdata, 
                       species = spdata,
                       AMSTAR2 = qdata, 
                       PFAS = ptidata
)


# generate a dictionary storing all variable - dataset pairs
vars_sets <- list()
j <- 1
for(set in all_dataframes) {
  for(varname in names(set)[-1]) {
    vars_sets[varname] <- names(all_dataframes)[j]
  }
  
  j <- j+1
}
vars_sets$Study_ID <- "review"

ui <- fluidPage(
  
  tags$head(
    tags$script(HTML('
        $(window).resize(function(event){
          var w = $(this).width();
          var h = $(this).height();
          var obj = {width: w, height: h};
          Shiny.onInputChange("pltChange", obj);
        });
      '))
  ),
  
  titlePanel("Data from: A research synthesis of humans, animals, and environmental compartments exposed to PFAS: A systematic evidence map and bibliometric analysis"),
  
  sidebarLayout(
    sidebarPanel("Data selection",
                 selectInput("dataSelector1",
                             label = "Select first variable of interest",
                             choices = list("*No variable selected*" = "NULL",
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
                                            
                                            "Species scientific name" = "species_name",
                                            "Species common name" = "Species_common_name",
                                            "Classes scientific name" = "Class_scientific_name",
                                            "How many species?" = "Species_one_many",
                                            "Animal study type" = "Lab_domestic_wildlife_mixed"
                             ),
                             selected = "Paper_title"),
                 
                 selectInput("dataSelector2",
                             label = "Select second variable of interest",
                             choices = list("*No variable selected*" = "NULL",
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
                                            
                                            "Species scientific name" = "species_name",
                                            "Species common name" = "Species_common_name",
                                            "Classes scientific name" = "Class_scientific_name",
                                            "How many species?" = "Species_one_many",
                                            "Animal study type" = "Lab_domestic_wildlife_mixed"
                             ),
                             selected = "Journal"),
                 
                 selectInput("dataSelector3",
                             label = "Select third variable of interest",
                             choices = list("*No variable selected*" = "NULL",
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
                                            
                                            "Species scientific name" = "species_name",
                                            "Species common name" = "Species_common_name",
                                            "Classes scientific name" = "Class_scientific_name",
                                            "How many species?" = "Species_one_many",
                                            "Animal study type" = "Lab_domestic_wildlife_mixed"
                             ),
                             selected = "Meta_analysis"),
                 
                 selectInput("dataSelector4",
                             label = "Select fourth variable of interest",
                             choices = list("*No variable selected*" = "NULL",
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
                                            
                                            "Species scientific name" = "species_name",
                                            "Species common name" = "Species_common_name",
                                            "Classes scientific name" = "Class_scientific_name",
                                            "How many species?" = "Species_one_many",
                                            "Animal study type" = "Lab_domestic_wildlife_mixed"
                             ),
                             selected = "Human_animal_environment"),
                 width = 3
    ),
    mainPanel("DATABASE",
              # textOutput("selected_var")
              dataTableOutput("gapminder_table")
    )
  )
)


server <- function(input, output, session) {
  
  # variables always present
  always_vars <- c("Study_ID", "Publication_year")
  
  re_selected_vars <- reactive({
    # collect all selected variables
    selected_vars <- 
      c(always_vars,
        input$dataSelector1,
        input$dataSelector2,
        input$dataSelector3,
        input$dataSelector4)
  })
  
  
  re_relevant_sets <- reactive({
    relevant_sets <- sapply(re_selected_vars(), USE.NAMES = F,
                            FUN = function(x) {
                              vars_sets[x][[1]]
                            })
    relevant_sets[sapply(relevant_sets, is.null)] <- NULL
    unlist(unique(relevant_sets))
  })
  
  re_database <- reactive({
    current_dataframes <- all_dataframes[re_relevant_sets()]
    database <- Reduce(dplyr::left_join, current_dataframes) %>%
      select(re_selected_vars()[re_selected_vars() != "NULL"])
    as.data.frame(database)
  })
  
  observeEvent(input$pltChange, {
    
    
    output$gapminder_table <- renderDataTable(
      
      # render table
      datatable(re_database(),
                filter = 'top',
                rownames = FALSE,
                extensions = "Buttons",
                options = list(pageLength = 50,
                               dom = "Blfrtip",
                               autoWidth = TRUE,
                               bAutoWidth = FALSE,
                               buttons = c('copy', 'csv', 'excel', 'pdf'),
                               columnDefs = list(list(className = 'dt-left', targets = '_all'))))
    )
    
    # output$selected_var <- renderText({
    #   paste("You selected", names(re_database()))
    # })
    
    
  }, ignoreNULL = FALSE)
}


shinyApp(ui = ui, server = server)