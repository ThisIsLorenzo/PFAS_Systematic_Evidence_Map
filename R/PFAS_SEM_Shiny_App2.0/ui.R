library(shiny)
library(dplyr)
library(purrr)
library(shinydashboard)
library(highcharter)
library(DT)

fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  navbarPage("Shiny App",
             tabPanel("Home",
                      sidebarLayout(
                        sidebarPanel(
                          h3("A research synthesis of humans, animals, and environmental compartments exposed to PFAS"),
                          h4("A systematic evidence map and bibliometric analysis of secondary literature"),
                          h4("______________________"),
                          h4("Filters:"),
                          selectInput(
                            inputId = "inHuman_animal_environment",
                            label = "Subject:",
                            choices = unique(alldata$Human_animal_environment),
                            selected = "Humans"
                          ),
                          
                          selectInput(
                            inputId = "inYearMin",
                            label = "Start year:",
                            choices = sort(unique(alldata$Publication_year))[1:length(unique(alldata$Publication_year))-1],
                            selected = min(alldata$Publication_year)
                          ),
                          
                          selectInput(
                            inputId = "inYearMax",
                            label = "End year:",
                            choices = sort(unique(alldata$Publication_year))[2:length(unique(alldata$Publication_year))],
                            selected = max(alldata$Publication_year)
                          ),
                          width = 3
                        ),
                        mainPanel(
                          tags$h3("Latest stats:"),
                          tags$div(
                            tags$div(
                              tags$p("#Reviews"),
                              textOutput(outputId = "outNReviews")
                            ) %>% tagAppendAttributes(class = "stat-card"),
                            tags$div(
                              tags$p("Main outcome category"),
                              textOutput(outputId = "outmainOutcome")
                            ) %>% tagAppendAttributes(class = "stat-card"),
                            tags$div(
                              tags$p("Main review type"),
                              textOutput(outputId = "outmainReviewtype")
                            ) %>% tagAppendAttributes(class = "stat-card"),
                            tags$div(
                              tags$p("Main journal"),
                              textOutput(outputId = "outmainJournal")
                            ) %>% tagAppendAttributes(class = "stat-card")
                          ) %>% tagAppendAttributes(class = "stat-card-container"),
                          
                          tags$div(
                            tags$h3("Summary stats:"),
                            tags$div(
                              tags$div(
                                style = "display: flex; justify-content: center;",
                                tags$div(
                                highchartOutput(outputId = "chartN_Reviews",  height = 400),
                                style = "width: 100%; max-width: 600px; margin: 10px;"
                              ) %>% tagAppendAttributes(class = "chart-card"),
                              tags$div(
                                highchartOutput(outputId = "chartN_metaanalyses",  height = 400),
                                style = "width: 100%; max-width: 600px; margin: 10px;"
                              ) %>% tagAppendAttributes(class = "chart-card")
                              ),
                              tags$div(
                                highchartOutput(outputId = "chartPFAS", height = 500),
                                style = "width: 100%; margin: 10px;"
                              ) %>% tagAppendAttributes(class = "chart-card"),
                            ) %>% tagAppendAttributes(class = "base-charts-container")
                          ) %>% tagAppendAttributes(class = "card-container"),
                          
                          width = 9
                        ) %>% tagAppendAttributes(class = "main-container")
                      )
             ),
             tabPanel("Database",
                      sidebarLayout(
                      sidebarPanel(
                        h3("A research synthesis of humans, animals, and environmental compartments exposed to PFAS"),
                        h4("A systematic evidence map and bibliometric analysis of secondary literature"),
                        h4("______________________"),
                        h4("Filters:"),
                        selectInput(
                          inputId = "inHuman_animal_environment",
                          label = "Subject:",
                          choices = unique(alldata$Human_animal_environment),
                          selected = "Humans"
                        ),
                        
                        selectInput(
                          inputId = "inYearMin",
                          label = "Start year:",
                          choices = sort(unique(alldata$Publication_year))[1:length(unique(alldata$Publication_year))-1],
                          selected = min(alldata$Publication_year)
                        ),
                        
                        selectInput(
                          inputId = "inYearMax",
                          label = "End year:",
                          choices = sort(unique(alldata$Publication_year))[2:length(unique(alldata$Publication_year))],
                          selected = max(alldata$Publication_year)
                        ),
                        width = 3
                      ),
                      mainPanel(
                        tags$div(
                          tags$p("Number of reviews selected:"),
                          textOutput(outputId = "outNReviews2")
                        ) %>% tagAppendAttributes(class = "stat-card"),
                        tags$div(
                          tags$h3("Reviews selected:"),
                          tags$div(
                            tags$div(
                              DTOutput("dataset")
                            )
                          )
                        ),
                        ) %>% tagAppendAttributes(class = "card-container")
                      )
             ),
             tabPanel("External Links",
                      dashboardHeader(
                        tags$li(class="dropdown", 
                                h3("Visit our GitHub Repository"),
                                h4("Here you can find our raw data and analysis code"),
                                tags$a(href="https://github.com/ThisIsLorenzo/PFAS_Systematic_Evidence_Map", 
                                       tags$img(src = "github.png", height = "60px", width = "60px"), 
                                       "GitHub Repo", target="_blank"), style = "margin-right: 1000px;", style = "margin-bottom: 80px;"),
                        tags$li(class="dropdown", 
                                h3("Visit our Living PFAS Web Site"),
                                h4("Here you can find out more about our projects on PFAS contamination"),
                                tags$a(href="https://www.livingpfas.org/", 
                                       tags$img(src = "LivingPFAS.png", height = "60px", width = "60px"), 
                                       "Living PFAS", target = "_blank"), style = "margin-right: 1000px;", style = "margin-bottom: 80px;"),
                        tags$li(class="dropdown", 
                                h3("Visit our Lab Web Site"),
                                h4("Here you can find out more about our lab"),
                                tags$a(href="https://www.i-deel.org/", 
                                       tags$img(src = "ideel.png", height = "60px", width = "60px"), 
                                       "I-Deel Lab", target = "_blank"), style = "margin-right: 1000px;", style = "margin-bottom: 80px;")
                        )
                      )
             )
)