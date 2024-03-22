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
        min-height: calc(5000vh - 200px);
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
                                                "Explore our database effortlessly, allowing you to easily browse, filter, and uncover insights tailored to your interests.")
                                  ),
                                  column(width = 12, 
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "Navigate through our repository seamlessly with intuitive controls. With just a few clicks, you can pinpoint specific categories, such as reviews focusing on human studies. Whether you're looking for broad overviews or detailed analyses, our app simplifies your search process.")
                                  ),
                                  column(width = 12, 
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "In addition to exploration, our app offers functionality to download the entire database or selectively extract subsets that match your research objectives. Dive into dynamic graphs that reveal patterns and trends within the data.")
                                  ),
                                  column(width = 12, 
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "Experience the perfect blend of accessibility and depth in our Shiny App. Designed to empower users in exploring and utilizing our systematic evidence map, it's your go-to tool for comprehensive research.")
                                  )
                                )
                       ),
                       tabPanel(title = "Guide", icon = icon("info"),
                                fluidRow(
                                  column(width = 12, align = "center", 
                                         tags$p(style = "font-size: 34px; margin-bottom: 20px;",
                                                "Navigating the Shiny App made easy:")
                                  ),
                                  column(width = 12, align = "center", 
                                         tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                "1. Explore two main components: Dataset and Visualization")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "Simply click on either option from the tab on the left to access their content seamlessly.")
                                  ),
                                  column(width = 12,align = "center",
                                         tags$img(src="screenshot_2.png")
                                  ),
                                  column(width = 12, align = "center",
                                         tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                "")
                                  ),
                                  column(width = 12, align = "center",
                                         tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                "2. Navigate Dataset's content")
                                  ),
                                  column(width = 12,align = "center",
                                         tags$img(src="screenshot_4.png")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "About: Get acquainted with our website's purpose and features.")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "Guide: Navigate seamlessly with our user-friendly guide.")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "Structure: Dive into the raw structure of our dataset.")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "Summary Stats: Explore basic statistics covering mapping, appraisal, and bibliometrics.")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "Data: Access raw data for individual objectives and the entire database.")
                                  ),
                                  column(width = 12, align = "center",
                                         tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                "")
                                  ),
                                  column(width = 12, align = "center",
                                         tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                "3. Navigate Visualization's content")
                                  ),
                                  column(width = 12,align = "center",
                                         tags$img(src="screenshot_6.png")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "Our visualization mirrors the map's objectives, with each objective further segmented into analytical groupings for easy comprehension.")
                                  ),
                                  column(width = 12, align = "center",
                                         tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                "")
                                  ),
                                  column(width = 12, align = "center",
                                         tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                "4. Discover additional resources")
                                  ),
                                  column(width = 12,align = "center",
                                         tags$img(src="screenshot_3.png")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "GitHub Repo: Direct access to our systematic evidence map's repository on GitHub. Here, you'll find all the raw data and files utilized in our analysis.")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "Living PFAS: Dive into comprehensive information about our projects on PFAS environmental pollutants. Supported by the National Health and Medical Research Council (NHMRC) Targeted Research Grant (APP1185002).")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "I-Deel Lab: Visit our lab's personal website to get acquainted with our team, research projects, academic publications, and exciting opportunities to collaborate. Don't miss out on our insightful blog posts!")
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
                                             column(width = 6, DTOutput("summary0.3")),
                                             column(width = 6, DTOutput("summary0.4")),
                                             column(width = 6, DTOutput("summary0.5")),
                                             column(width = 6, DTOutput("summary0.6")),
                                             column(width = 6, DTOutput("summary0.7")),
                                             column(width = 6, DTOutput("summary0.8")),
                                             column(width = 6, DTOutput("summary0.9")),
                                             column(width = 6, DTOutput("summary1")),
                                             column(width = 6, DTOutput("summary1.1")),
                                             column(width = 6, DTOutput("summary1.2")),
                                             column(width = 6, DTOutput("summary1.3"))
                                             )
                                           ),
                                  tabPanel("Appraisal",
                                           fluidRow(
                                             column(width = 6, DTOutput("summary2")),
                                             column(width = 6, DTOutput("summary2.1")),
                                             column(width = 6, DTOutput("summary2.2")),
                                             column(width = 6, DTOutput("summary2.3")),
                                             column(width = 6, DTOutput("summary2.4")),
                                             column(width = 6, DTOutput("summary2.5")),
                                             column(width = 6, DTOutput("summary2.6")),
                                             column(width = 6, DTOutput("summary2.7")),
                                             column(width = 6, DTOutput("summary2.8")),
                                             column(width = 6, DTOutput("summary2.9")),
                                             column(width = 6, DTOutput("summary2.10")),
                                             column(width = 6, DTOutput("summary2.11")),
                                             column(width = 6, DTOutput("summary2.12")),
                                             column(width = 6, DTOutput("summary2.13")),
                                             column(width = 6, DTOutput("summary2.14")),
                                             column(width = 6, DTOutput("summary2.15")),
                                             column(width = 6, DTOutput("summary2.16"))
                                             )
                                           ),
                                  tabPanel("Bibliometrics",
                                           fluidRow(
                                             column(width = 6, DTOutput("summary0.1")),
                                             column(width = 6, DTOutput("summary0.2")),
                                             column(width = 6, DTOutput("summary3"))
                                             )
                                           )
                                )
                       ),
                       tabPanel(title = "Data", icon = icon("table"),
                                tabsetPanel(
                                  tabPanel("Mapping",
                                           fluidRow(
                                             column(width = 12,
                                                    tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                           icon("warning"),
                                                           "Note: If the datatable isn't visible, you may need to toggle between maximizing and restoring the window size in your browser to display the table.")
                                             ),
                                           ),
                                           mainPanel(
                                             DTOutput("mapping_table")
                                           )),
                                  tabPanel("Appraisal", 
                                           mainPanel(
                                             DTOutput("appraisal_table")
                                           )),
                                  tabPanel("Bibliometrics", 
                                           mainPanel(
                                             DTOutput("biblio_table")
                                           )),
                                  tabPanel("Whole database",
                                           mainPanel(
                                             DTOutput("whole_d_table")
                                           )
                                           )
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
                                fluidRow(
                                  column(width = 12,
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                icon("warning"),
                                                "Work in progress...")
                                  ),
                                )
                       )
                )
        )
      )
    )
  )
)