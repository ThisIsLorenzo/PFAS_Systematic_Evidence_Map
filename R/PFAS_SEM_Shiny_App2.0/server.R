function(input, output) {
  data_cards <- reactive({
    alldata %>%
        filter(
          Human_animal_environment == input$inHuman_animal_environment,
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
      summarise(
        nReviews = n_distinct(Study_ID),
        mainOutcome = names(which.max(table(Outcome_category))),
        mainReviewtype = names(which.max(table(Review_type_claimed))),
        mainJournal = names(which.max(table(Journal)))
      )
  })
  
  data_base <- reactive({
    mdata %>%
      filter(
        Human_animal_environment == input$inHuman_animal_environment,
        between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
      ) %>% 
      rename(Subject = Human_animal_environment) %>% 
      rename("Year of publication" = Publication_year) %>% 
      mutate(Title = str_replace_all(Title, "â€”" , "—")) %>% 
      mutate(Title = str_replace_all(Title, "â€\u0090" , "-")) %>% 
      mutate(Title = str_replace_all(Title, "â€™" , "'")) %>% 
      mutate(Title = str_replace_all(Title, "âˆ’" , "−")) %>% 
      mutate(First_author = str_replace_all(First_author, "Ã³" , "ó")) %>% 
      mutate(First_author = str_replace_all(First_author, "Ã©" , "é")) %>% 
      mutate(First_author = str_replace_all(First_author, "Ã¡" , "á")) %>% 
      mutate(First_author = str_replace_all(First_author, "Ã¤" , "ä"))
  })
  
  
  data_chart1 <- reactive({
    alldata %>% 
    filter(
      input$inHuman_animal_environment == Human_animal_environment,
      between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
    )  %>%
      distinct(Study_ID, Publication_year) %>%
      count(Publication_year)
  })
  
  data_chart2 <- reactive({
    alldata %>% 
      filter(
        input$inHuman_animal_environment == Human_animal_environment,
        between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax)),
        Meta_analysis == "Yes"
      )  %>%
      distinct(Study_ID, Publication_year) %>%
      count(Publication_year) 
  })
  
  data_chart3 <- reactive({
    alldata %>% 
      filter(
        input$inHuman_animal_environment == Human_animal_environment,
        between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
      )  %>%
      filter(PFAS_type != "NA") %>% 
      distinct(Study_ID, PFAS_type) %>%
      count(PFAS_type) %>% 
      arrange(desc(n))
  })
  
  
  output$dataset <- renderDT(
    
    # render table
    data_base(),
    filter = 'top',
    rownames = FALSE,
    extensions = "Buttons",
    options = list(pageLength = 15,
                   dom = "Bfrtp",
                   width = '100%',
                   autoWidth = TRUE,
                   scrollX = TRUE,
                   encoding = "UTF-8",
                   buttons = list(
                     list(extend = 'copy', text = 'Copy'),
                     list(extend = 'csv', text = 'CSV'),
                     list(extend = 'excel', text = 'Excel'),
                     list(extend = 'pdf', text = 'PDF'),
                     list(extend = 'pageLength', text = 'Entries')
                   )
    )
  )
  
  
  output$outNReviews <- renderText({
    data_cards()$nReviews
  })
  
  output$outNReviews2 <- renderText({
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
  
  output$chartN_metaanalyses <- renderHighchart({
    hchart(data_chart2(), "line", hcaes(x = Publication_year, y = n), color = "green", name = "Number of meta-analyses") |>
      hc_title(text = "Number of meta-analyses per year", align = "left") |>
      hc_xAxis(title = list(text = "Year")) |>
      hc_yAxis(title = list(text = "Number of meta-analyses"))
  })
  
  output$chartPFAS <- renderHighchart({
    hchart(data_chart3(), "column", hcaes(x = PFAS_type, y = n), color = "#800000", name = "Number of reviews") |>
      hc_title(text = "Number of reviews per compound", align = "left") |>
      hc_xAxis(title = list(text = "Compound")) |>
      hc_yAxis(title = list(text = "Number of reviews"))
  })
}
