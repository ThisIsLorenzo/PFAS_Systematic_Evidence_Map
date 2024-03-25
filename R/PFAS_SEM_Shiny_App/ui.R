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
                                         tags$p(style = "font-size: 40px; margin-bottom: 20px;",
                                                "Welcome to our Shiny App!")
                                  ),
                                  column(width = 12, align = "center",
                                         tags$img(src="shiny-logo.png", height = "100px", width = "200px")
                                  ),
                                  column(width = 12, align = "center", 
                                         tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                "Your gateway to exploring our systematic evidence map with ease and interactivity!")
                                  ),
                                  column(width = 12, align = "center", 
                                         tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                "This app is a browsable, interactive, and user-friendly version of the following project:")
                                  ),
                                  column(width = 12, align = "center", 
                                         tags$p(style = "font-size: 30px; margin-bottom: 20px;",
                                                "A research synthesis of humans animals, and environmental compartments exposed to PFAS: A systematic evidence map and bibliometric analysis of secondary literature")
                                  ),
                                  column(width = 12, align = "center", 
                                         tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                "Authors:")
                                  ),
                                  column(width = 12, align = "center", 
                                         tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                "Lorenzo Ricolfi, Catharina Vendl, Jennifer Bräunig, Matthew D. Taylor, Daniel Hesselson, G. Gregory Neely, Malgorzata Lagisz, Shinichi Nakagawa")
                                  ),
                                  # column(width = 12, align = "center",
                                  #        tags$img(src="title.png")
                                  # ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                "You can find the published research protocol at the following link: https://www.sciencedirect.com/science/article/pii/S0160412021005985")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                "Note: we will add here the link to the article once it is published")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                "What does this app do?")
                                  ),
                                  column(width = 12,  
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "⦿ Effortlessly explore our database with easy browsing, filtering, and tailored insights. Navigate seamlessly with intuitive controls.")
                                  ),
                                  column(width = 12, 
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "⦿ Quickly pinpoint specific categories like human studies. Whether you need broad overviews or detailed analyses, our app streamlines your search.")
                                  ),
                                  column(width = 12, 
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "⦿ Download the entire database or select subsets matching your research goals.")
                                  ),
                                  column(width = 12, 
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "⦿ Dive into dynamic graphs revealing trends. Our Shiny App combines accessibility and depth, empowering users to explore our evidence map effectively for comprehensive research.")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                "What's next?")
                                  ),
                                  column(width = 12,  
                                         tags$p(style = "font-size: 20px; margin-bottom: 20px;",
                                                "Go to the Guide tab to learn how to navigate our app!")
                                  ),
                                  column(width = 12,align = "center",
                                         tags$img(src="screenshot_1.png")
                                  )
                                )
                       ),
                       tabPanel(title = "Guide", icon = icon("info"),
                                fluidRow(
                                  column(width = 12, align = "center", 
                                         tags$p(style = "font-size: 34px; margin-bottom: 20px;",
                                                "Navigating the Shiny App made easy")
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
                                                "The Dataset component is divided into five sub-pages:")
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
                                         tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                "Below is an overview of the raw structure and content of the database.")
                                  ),
                                  column(width = 12,
                                         tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                "The database is organized into six objectives, each of which is a data table containing information extracted from the systematic review included in the systematic evidence map.")
                                  ),
                                  column(width=12, verbatimTextOutput("structure")
                                  )
                                )
                       ),
                       tabPanel(title = "Summary Stats", icon = icon("chart-pie"),
                                tabsetPanel(
                                  tabPanel("Mapping",
                                           fluidRow(
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "The tables below provide summarized data for particular variables associated with the mapping objective of the systematic evidence map.")
                                             ),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Note: some of the table have many entries. Use the 'previous' and 'next' buttons to navigate them.")
                                             ),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Type of review claimed and number of reviews:")
                                             ),
                                             column(width = 12, DTOutput("summary0.3")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Number of reviews conducted with and without a systematic literature search approach:")
                                             ),
                                             column(width = 12, DTOutput("summary0.4")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Number of reviews with or without a research protocol pre-registered a priori:")
                                             ),
                                             column(width = 12, DTOutput("summary0.5")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Number of reviews with or without a quantitative synthesis:")
                                             ),
                                             column(width = 12, DTOutput("summary0.6")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Number of reviews on the three main subjects and a mix of these:")
                                             ),
                                             column(width = 12, DTOutput("summary0.7")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Number of reviews on various animal species:")
                                             ),
                                             column(width = 12, DTOutput("summary0.8")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Number of reviews with or without a focus on PFAS and not on POPs in general:")
                                             ),
                                             column(width = 12, DTOutput("summary0.9")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Number of reviews investigating one or multiple PFAS:")
                                             ),
                                             column(width = 12, DTOutput("summary1")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Number of reviews using or not using a PECOs framwork:")
                                             ),
                                             column(width = 12, DTOutput("summary1.1")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Number of reviews on various outcomes:")
                                             ),
                                             column(width = 12, DTOutput("summary1.2")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 22px; margin-bottom: 20px;",
                                                           "Number of reviews on the main six outcome categories:")
                                             ),
                                             column(width = 12, DTOutput("summary1.3"))
                                             )
                                           ),
                                  tabPanel("Appraisal",
                                           fluidRow(
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "The tables below provide summarized data for particular variables associated with the critical appraisal objective of the systematic evidence map.")
                                             ),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 1: Are the research questions and inclusion criteria for the review clearly delineated?")
                                             ),
                                             column(width = 12, DTOutput("summary2")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 2: Did the report of the review contain an explicit statement that the review methods were established prior to the conduct of the review and did the report justify any significant deviations from the protocol?")
                                             ),
                                             column(width = 12, DTOutput("summary2.1")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 3: Did the review authors explain their selection of the study designs for inclusion in the review?")
                                             ),
                                             column(width = 12, DTOutput("summary2.2")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 4: Did the review authors use a comprehensive literature search strategy?")
                                             ),
                                             column(width = 12, DTOutput("summary2.3")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 5: Did the review authors perform study selection in duplicate?")
                                             ),
                                             column(width = 12, DTOutput("summary2.4")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 6: Did the review authors perform data extraction in duplicate?")
                                             ),
                                             column(width = 12, DTOutput("summary2.5")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 7: Did the review authors provide a list of excluded studies and justify the exclusions?")
                                             ),
                                             column(width = 12, DTOutput("summary2.6")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 8: Did the review authors describe the included studies in adequate detail?")
                                             ),
                                             column(width = 12, DTOutput("summary2.7")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 9: Did the review authors use a satisfactory technique for assessing the risk of bias (RoB) in individual studies that were included in the review?")
                                             ),
                                             column(width = 12, DTOutput("summary2.8")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 10: Did the review authors report on the sources of funding for the studies included in the review?")
                                             ),
                                             column(width = 12, DTOutput("summary2.9")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 11: If meta-analysis was performed did the review authors use appropriate methods for statistical combination of results?")
                                             ),
                                             column(width = 12, DTOutput("summary2.10")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 12: If meta-analysis was performed, did the review authors assess the potential impact of RoB in individual studies on the results of the meta-analysis or other evidence synthesis?")
                                             ),
                                             column(width = 12, DTOutput("summary2.11")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 13: Did the review authors account for RoB in individual studies when interpreting/ discussing the results of the review?")
                                             ),
                                             column(width = 12, DTOutput("summary2.12")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 14: Did the review authors provide a satisfactory explanation for, and discussion of, any heterogeneity observed in the results of the review?")
                                             ),
                                             column(width = 12, DTOutput("summary2.13")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 15: If they performed quantitative synthesis did the review authors carry out an adequate investigation of publication bias (small study bias) and discuss its likely impact on the results of the review?")
                                             ),
                                             column(width = 12, DTOutput("summary2.14")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "Item 16: Did the review authors report any potential sources of conflict of interest, including any funding they received for conducting the review?")
                                             ),
                                             column(width = 12, DTOutput("summary2.15"))
                                             )
                                           ),
                                  tabPanel("Bibliometrics",
                                           fluidRow(
                                             column(width = 12,
                                                    tags$p(style = "font-size: 26px; margin-bottom: 20px;",
                                                           "The tables below provide summarized data for particular variables associated with the bibliometrics objective of the systematic evidence map.")
                                             ),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                           "Number of reviews and publishing journal:")
                                             ),
                                             column(width = 12, DTOutput("summary0.1")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                           "Number of reviews and country of the first author:")
                                             ),
                                             column(width = 12, DTOutput("summary0.2")),
                                             column(width = 12,
                                                    tags$p(style = "font-size: 24px; margin-bottom: 20px;",
                                                           "Articles with the most total citations:")
                                             ),
                                             column(width = 12, DTOutput("summary3"))
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
                                                "Work is currently in progress on this page. We apologize for any inconvenience, and we assure you that it will be available shortly. Thank you for your patience.")
                                  ),
                                )
                       )
                )
        )
      )
    )
  )
)