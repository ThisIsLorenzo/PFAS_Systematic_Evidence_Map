library(shiny)
library(dplyr)
library(purrr)
library(shinydashboard)
library(highcharter)

fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  sidebarLayout(
    sidebarPanel(
      titlePanel("PFAS SEM Shiny"),
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
            highchartOutput(outputId = "chartN_Reviews",  height = 500)
          ) %>% tagAppendAttributes(class = "chart-card"),
          tags$div(
            highchartOutput(outputId = "chartPFAS", height = 500)
          ) %>% tagAppendAttributes(class = "chart-card"),
        ) %>% tagAppendAttributes(class = "base-charts-container")
      ) %>% tagAppendAttributes(class = "card-container"),
      
      width = 9
      ) %>% tagAppendAttributes(class = "main-container")
    )
  )