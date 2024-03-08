library(shiny)
library(shinydashboard)

fluidPage(
  tags$head(
    tags$script(HTML('
        $(window).resize(function(event){
          var w = $(this).width();
          var h = $(this).height();
          var obj = {width: w, height: h};
          Shiny.onInputChange("pltChange", obj)
          });
      ')),
    tags$style(HTML('
      /* Adjusting the background color of the tabPanel to cover the entire page */
      .tab-content {
        background-color: #f2f2f2;
        min-height: calc(5000vh - 80px);
      }
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
        menuItem(text = "Visualization", tabName = "viz", icon = icon("chart-line"))
        # conditionalPanel(
        #   condition = "input.sidebar === 'viz'",
        # selectInput(inputId = "table", label = "Select Table", choices = c("Mapping"="mdata", "Appraisal"="qdata", "Bibliometrics"="bib_data"), selected = "mdata"))
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
                                         tags$p(style = "font-size: 40px; margin-bottom: 20px;",
                                                "Welcome to our Shiny App!")
                                  ),
                                  column(width = 12, align = "center",
                                         tags$img(src="shiny-logo.png", height = "90px", width = "160px")
                                  ),
                                  column(width = 12, align = "center", 
                                         tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                "Your gateway to exploring our comprehensive systematic evidence map with ease and interactivity!")
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
                                  ),
                                  column(width = 12,align = "center",
                                         tags$img(src="Fig.1.png")
                                  ),
                                  column(width = 12, align = "center",
                                         tags$a("Workflow illustrating the main research questions, objectives, data sources, and analysis approaches.")
                                  )
                                )
                       ),
                       tabPanel(title = "Structure", icon = icon("uncharted"),
                                fluidRow(
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "Below is an overview of the structure and content of the database")
                                  ),
                                  column(width=12, verbatimTextOutput("structure")
                                  )
                                )
                       ),
                       tabPanel(title = "Summary Stats", icon = icon("chart-pie"),
                                tabsetPanel(
                                  tabPanel("Mapping",
                                           fluidRow(
                                             column(width = 6, verbatimTextOutput("summary0.3")),
                                             column(width = 6, verbatimTextOutput("summary0.4")),
                                             column(width = 6, verbatimTextOutput("summary0.5")),
                                             column(width = 6, verbatimTextOutput("summary0.6")),
                                             column(width = 6, verbatimTextOutput("summary0.7")),
                                             column(width = 6, verbatimTextOutput("summary0.8")),
                                             column(width = 6, verbatimTextOutput("summary1")),
                                             column(width = 6, verbatimTextOutput("summary1.1"))
                                             )
                                           ),
                                  tabPanel("Appraisal",
                                           fluidRow(
                                             column(width = 6, verbatimTextOutput("summary2")),
                                             column(width = 6, verbatimTextOutput("summary2.1")),
                                             column(width = 6, verbatimTextOutput("summary2.2")),
                                             column(width = 6, verbatimTextOutput("summary2.3")),
                                             column(width = 6, verbatimTextOutput("summary2.4")),
                                             column(width = 6, verbatimTextOutput("summary2.5")),
                                             column(width = 6, verbatimTextOutput("summary2.6")),
                                             column(width = 6, verbatimTextOutput("summary2.7")),
                                             column(width = 6, verbatimTextOutput("summary2.8")),
                                             column(width = 6, verbatimTextOutput("summary2.9")),
                                             column(width = 6, verbatimTextOutput("summary2.10")),
                                             column(width = 6, verbatimTextOutput("summary2.11"))
                                             )
                                           ),
                                  tabPanel("Bibliometrics",
                                           fluidRow(
                                             column(width = 6, verbatimTextOutput("summary0.1")),
                                             column(width = 6, verbatimTextOutput("summary0.2")),
                                             column(width = 6, verbatimTextOutput("summary3"))
                                             )
                                           )
                                )
                       ),
                       tabPanel(title = "Data", icon = icon("table"),
                                tabsetPanel(
                                  tabPanel("Mapping",
                                           sidebarPanel("Data selection",
                                                        selectInput("dataSelector1",
                                                                    label = "Select 1st variable of interest",
                                                                    choices = choices_mapping(),
                                                                    selected = "*No variable selected*"),
                                                        selectInput("dataSelector2",
                                                                    label = "Select 2nd variable of interest",
                                                                    choices = choices_mapping(),
                                                                    selected = "*No variable selected*"),
                                                        selectInput("dataSelector3",
                                                                    label = "Select 3rd variable of interest",
                                                                    choices = choices_mapping(),
                                                                    selected = "*No variable selected*"),
                                                        selectInput("dataSelector4",
                                                                    label = "Select 4th variable of interest",
                                                                    choices = choices_mapping(),
                                                                    selected = "*No variable selected*"),
                                                        selectInput("dataSelector5",
                                                                    label = "Select 5th variable of interest",
                                                                    choices = choices_mapping(),
                                                                    selected = "*No variable selected*"),
                                                        selectInput("dataSelector6",
                                                                    label = "Select 6th variable of interest",
                                                                    choices = choices_mapping(),
                                                                    selected = "*No variable selected*"),
                                                        selectInput("dataSelector7",
                                                                    label = "Select 7th variable of interest",
                                                                    choices = choices_mapping(),
                                                                    selected = "*No variable selected*"),
                                                        width = 3
                                           ),
                                           mainPanel(
                                             dataTableOutput("gapminder_table")
                                           )
                                  ),
                                  tabPanel("Appraisal", h4("table")),
                                  tabPanel("Bibliometrics", h4("table")),
                                  tabPanel("Whole database", h4("table"))
                                ),
                       )
                )),
        tabItem(tabName = "viz",
                tabBox(id="t2", width = 12,
                       tabPanel(title = "Mapping",icon = icon("map"),
                                tabsetPanel(
                                  tabPanel(title = "Time Trends",
                                           plotlyOutput("histplot1"),
                                           plotlyOutput("histplot2"),
                                           plotlyOutput("histplot3"),
                                           plotlyOutput("histplot4")),
                                  tabPanel(title = "Subject",
                                           fluidRow(
                                             column(width=6, plotlyOutput("histplot5")), 
                                             column(width=6, plotlyOutput("histplot7")), 
                                             column(width=6, plotlyOutput("histplot6")),
                                             column(width=6, plotlyOutput("histplot8")),
                                             column(width=6,
                                                    tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                           "SR = systematic review, CrR = critical review, SR and MA = systematic review and meta-analysis, MA = meta-analysis, CoR = comprehensive review, ScR = scoping review, SEM = systematic evidence review"))
                                           )
                                  ),
                                  tabPanel(title = "PFAS",
                                           fluidRow(
                                             column(width=6, plotlyOutput("histplot9")), 
                                             column(width=6, plotlyOutput("histplot10")),
                                             column(width=6, plotlyOutput("histplot11"))
                                           )
                                  )
                                )
                       ),
                       tabPanel(title = "Appraisal", icon = icon("clipboard-question"),
                                tabsetPanel(
                                  tabPanel(title = "AMSTAR2", 
                                           plotlyOutput("histplot12")),
                                  tabPanel(title = "Time Trends",
                                           fluidRow(
                                             column(width=6, plotlyOutput("geomline1")),
                                             column(width=6, plotlyOutput("geomline2")),
                                             column(width=6, plotlyOutput("geomline3")),
                                             column(width=6, plotlyOutput("geomline4")),
                                             column(width=6, plotlyOutput("geomline5"))
                                           )
                                  )
                                )
                       ),
                       tabPanel(title = "Bibliometrics", icon = icon("connectdevelop"),
                                tabsetPanel(
                                  tabPanel(title = "Country collaborations", tags$img(src="Fig.8.pdf"))
                                )
                       )
                )
        )
      )
    )
  )
)