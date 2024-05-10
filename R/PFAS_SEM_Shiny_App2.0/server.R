function(input, output) {
  data_cards <- reactive({
    alldata %>% 
      filter(
        Human_animal_environment == input$inHuman_animal_environment,
        between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
      )  %>% 
      summarise(
        nReviews = n_distinct(Study_ID),
        mainOutcome = .$Outcome_category %>%
          table() %>%
          which.max() %>%
          names(),
        mainReviewtype = .$Review_type_claimed %>%
          table() %>%
          which.max() %>%
          names(),
        mainJournal = .$Journal %>%
          table() %>%
          which.max() %>%
          names()
      )
  })
  
  data_chart1 <- reactive({
    alldata %>% 
      filter(
        Human_animal_environment == input$inHuman_animal_environment,
        between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
      )  %>%
      distinct(Study_ID, Publication_year) %>%
      count(Publication_year)
  })
  
  data_chart2 <- reactive({
    alldata %>% 
      filter(
        Human_animal_environment == input$inHuman_animal_environment,
        between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
      )  %>%
      filter(PFAS_type != "NA") %>% 
      distinct(Study_ID, PFAS_type) %>%
      count(PFAS_type)
  })
  
  
  
  output$outNReviews <- renderText({
    data_cards()$nReviews
  })
  
  output$outmainOutcome <- renderText({
    data_cards()$mainOutcome
  })
  
  output$outmainReviewtype <- renderText({
    data_cards()$mainReviewtype
  })
  
  output$outmainJournal <- renderText({
    data_cards()$mainJournal
  })
  
  output$chartN_Reviews <- renderHighchart({
    hchart(data_chart1(), "line", hcaes(x = Publication_year, y = n), color = "#0198f9", name = "Number of reviews") |>
      hc_title(text = "Number of reviews per year", align = "left") |>
      hc_xAxis(title = list(text = "Year")) |>
      hc_yAxis(title = list(text = "Number of reviews"))
  })
  
  output$chartPFAS <- renderHighchart({
    hchart(data_chart2(), "column", hcaes(x = PFAS_type, y = n), color = "#800000", name = "Number of reviews") |>
      hc_title(text = "Number of reviews per compound", align = "left") |>
      hc_xAxis(title = list(text = "Year")) |>
      hc_yAxis(title = list(text = "Number of reviews"))
  })
}
