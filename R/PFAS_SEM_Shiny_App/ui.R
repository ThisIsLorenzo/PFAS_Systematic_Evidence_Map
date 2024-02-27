library(shiny)
library(shinydashboard)

fluidPage(
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
  dashboardPage(
  dashboardHeader(title = "A research synthesis of humans, animals, and environmental compartments exposed to PFAS: A systematic evidence map and bibliometric analysis of secondary literature",
                  titleWidth = 1500,
                  tags$li(class="dropdown", tags$a(href="https://github.com/ThisIsLorenzo/PFAS_Systematic_Evidence_Map", icon("github"), "GitHub Repo", target="_blank")),
                  tags$li(class="dropdown", tags$a(href="https://www.i-deel.org/", "I-Deel Lab", target="_blank"))
                  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem(text = "Dataset", tabName = "data", icon = icon("database")),
      menuItem(text = "Visualization", tabName = "viz", icon = icon("chart-line")),
      selectInput(inputId = "var1", label = "Select the variable", choices = mdata, selected = "Paper_title"),
      menuItem(text = "World Map", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              tabBox(id = "t1", width = 12,
                     tabPanel(title = "About", icon = icon("address-card"),
                              fluidRow(
                                column(width = 8, tags$img(src="Fig.1.png", widht = 300, height = 600),
                                       tags$a("Workflow"), align = "center"),
                                column(width = 4, tags$br(),
                                       tags$p("ADD INFORMATION ABOUT THE MAP HERE")
                                       ))),
                     tabPanel(title = "Data", icon = icon("address-card"), sidebarPanel("Data selection",
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
                     mainPanel(# textOutput("selected_var")
                               dataTableOutput("gapminder_table")
                     )),
                     tabPanel(title = "Structure", icon = icon("address-card"), h2("tabPanel-3 placeholder UI")),
                     tabPanel(title = "Summary Stats", icon = icon("address-card"), h2("tabPanel-4 placeholder UI"))
                     )
              ),
      tabItem(tabName = "viz",
              tabBox(id="t2", width = 12,
                     tabPanel(title = "", value = "trends", h4("tabPanel placeholder")),
                     tabPanel(title = "", value = "distribution", h4("tabPanel placeholder")),
                     tabPanel(title = "", value = "blabla", h4("tabPanel placeholder")),
                     tabPanel(title = "", value = "blabla", h4("tabPanel placeholder")))
              ),
      tabItem(tabName = "map",
              box(h4("placeholder UI")))
      )
    )
  )
)
