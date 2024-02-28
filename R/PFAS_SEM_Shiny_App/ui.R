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
  dashboardHeader(title = "Ricolfi et al., 2024",
                  titleWidth = 200,
                  tags$li(class="dropdown", tags$a(href="https://github.com/ThisIsLorenzo/PFAS_Systematic_Evidence_Map", tags$img(src = "github.png", height = "20px", width = "20px"), "GitHub Repo", target="_blank")),
                  tags$li(class="dropdown", tags$a(href="https://www.livingpfas.org/", tags$img(src = "LivingPFAS.png", height = "20px", width = "20px"), "Living PFAS", target = "_blank")),
                  tags$li(class="dropdown", tags$a(href="https://www.i-deel.org/", tags$img(src = "ideel.png", height = "20px", width = "20px"), "I-Deel Lab", target = "_blank"))
                  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem(text = "Dataset", tabName = "data", icon = icon("database")),
      # conditionalPanel(
      #   condition = "input.sidebar === 'data'",
      # selectInput(inputId = "table", label = "Select Table", choices = c("All"="my_data", "Mapping"="mdata", "Appraisal"="qdata", "Bibliometrics"="bib_data"), selected = "mdata")),
      menuItem(text = "Visualization", tabName = "viz", icon = icon("chart-line")),
      # conditionalPanel(
      #   condition = "input.sidebar === 'viz'",
      # selectInput(inputId = "table", label = "Select Table", choices = c("Mapping"="mdata", "Appraisal"="qdata", "Bibliometrics"="bib_data"), selected = "mdata")),
      menuItem(text = "World Map", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              tabBox(id = "t1", width = 12,
                     tabPanel(title = "About", icon = icon("address-card"),
                              fluidRow(
                                column(width = 12, align = "center",
                                       tags$img(src="title.png")
                                       ),
                                column(width = 12, align = "center", 
                                       tags$p(style = "font-size: 32px; margin-bottom: 20px;",
                                              "Welcome to our Shiny App!")
                                       ),
                                column(width = 12, align = "center",
                                       tags$img(src="shiny-logo.png", height = "90px", width = "160px")
                                       ),
                                column(width = 12, align = "center", 
                                       tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                              "your gateway to exploring our comprehensive systematic evidence map with ease and interactivity!")
                                       ),
                                column(width = 12,  
                                       tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                              "Dive into our database effortlessly, empowering you to browse, filter, and extract insights tailored to your interests.")
                                       ),
                                column(width = 12, 
                                       tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                              "Navigate through our repository with intuitive controls, enabling you to pinpoint specific categories, such as reviews focusing on human studies, with just a few clicks. Whether you're seeking broad overviews or detailed analyses, our app streamlines your search process.")
                                       ),
                                column(width = 12, 
                                       tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                              "Beyond exploration, leverage our app's functionality to download the entirety of our database or selectively extract subsets that align with your research objectives. Unleash the power of visualization by generating dynamic graphs that illuminate patterns and trends within the data.")
                                       ),
                                column(width = 12, 
                                       tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                              "Experience the fusion of accessibility and depth in our Shiny App, designed to empower users in their exploration and utilization of our systematic evidence map.")
                                       )
                                )
                              ),
                     tabPanel(title = "Structure", verbatimTextOutput("structure"), icon = icon("uncharted"),
                              fluidRow(
                                column(width = 12, 
                                       tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                              "The Structure of this Shiny App mirrors the organization of our systematic evidence map, ensuring an intuitive and straightforward user experience. In the adjacent tab, you'll discover a concise summary of key statistics, while in the subsequent tab, you'll have access to an extensive datatable containing all the data encompassed within our systematic evidence map. To maintain navigational ease, we've structured our database according to the three primary objectives outlined in our map. To refresh your memory on the map's structure, refer to the accompanying figure.")
                                       ),
                                column(width = 12, align = "center",
                                       tags$a("Workflow illustrating the main research questions, objectives, data sources, and analysis approaches.")
                                ),
                                column(width = 12,align = "center",
                                       tags$img(src="Fig.1.png")
                                       )
                                )),
                     tabPanel(title = "Summary Stats", verbatimTextOutput("summary"), icon = icon("chart-pie")),
                     tabPanel(title = "Data", icon = icon("table"), sidebarPanel("Data selection",
                                                                                        selectInput("dataSelector1",
                                                                                                    label = "Select 1st variable of interest",
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
                                                                                                    selected = "*No variable selected*"),
                                                                                        
                                                                                        selectInput("dataSelector2",
                                                                                                    label = "Select 2nd variable of interest",
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
                                                                                                    selected = "*No variable selected*"),
                                                                                        selectInput("dataSelector3",
                                                                                                    label = "Select 3rd variable of interest",
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
                                                                                                    selected = "*No variable selected*"),
                                                                                        selectInput("dataSelector4",
                                                                                                    label = "Select 4th variable of interest",
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
                                                                                                    selected = "*No variable selected*"),
                                                                                 selectInput("dataSelector5",
                                                                                                    label = "Select 5th variable of interest",
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
                                                                                                    selected = "*No variable selected*"),
                                                                                 selectInput("dataSelector6",
                                                                                             label = "Select 6th variable of interest",
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
                                                                                             selected = "*No variable selected*"),
                                                                                 selectInput("dataSelector7",
                                                                                             label = "Select 7th variable of interest",
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
                                                                                             selected = "*No variable selected*"),
                                                                                        width = 3
                     ),
                     mainPanel(# textOutput("selected_var")
                               dataTableOutput("gapminder_table")
                               )
                     )
              )),
      tabItem(tabName = "viz",
              tabBox(id="t2", width = 12,
                     tabPanel(title = "Mapping",
                              tabsetPanel(
                                tabPanel(title = "Time Trends", h4("graph")),
                                tabPanel(title = "Subject", h4("graph")),
                                tabPanel(title = "PFAS", h4("graph"))
                                )
                              ),
                     tabPanel(title = "Appraisal",
                              tabsetPanel(
                                tabPanel(title = "AMSTAR2", h4("graph")),
                                tabPanel(title = "Time Trends", h4("graph"))
                                )
                              ),
                     tabPanel(title = "Bibliometrics", 
                              tabsetPanel(
                                tabPanel(title = "Country collaborations", h4("graph"))
                                )
                              )
                     )
              ),
      tabItem(tabName = "map",
              box(h4("placeholder UI")))
      )
    )
  )
)
