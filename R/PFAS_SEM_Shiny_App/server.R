function(input, output) {
  
  data_cards <- reactive({
    if (input$inHuman_animal_environment == "All") {
      # No filtering by Human_animal_environment
      alldata %>%
        dplyr::filter(
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
      summarise(
        nReviews = n_distinct(Study_ID),
        mainOutcome = names(which.max(table(Outcome_category))),
        mainReviewtype = names(which.max(table(Review_type_claimed))),
        mainJournal = names(which.max(table(Journal)))
      )
    } else {
      alldata %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment,
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
        summarise(
          nReviews = n_distinct(Study_ID),
          mainOutcome = names(which.max(table(Outcome_category))),
          mainReviewtype = names(which.max(table(Review_type_claimed))),
          mainJournal = names(which.max(table(Journal)))
        )
    }
  })
  
  data_base_summary <- reactive({
    if (input$inHuman_animal_environment2 == "All" & input$inPFAS == "All") {
      # No filtering by Human_animal_environment and PFAS types
      mdata_pivot %>%
        dplyr::filter(
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>% 
      summarise(
        nReviews = n_distinct(Title)) 
    }
    
    else if (input$inHuman_animal_environment2 != "All" & input$inPFAS == "All") {
      mdata_pivot %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment2,
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>%
        summarise(
          nReviews = n_distinct(Title))
    }
    
    else if (input$inHuman_animal_environment2 == "All" & input$inPFAS != "All"){
      mdata_pivot %>%
        dplyr::filter(
          PFAS_type == input$inPFAS,
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>%
        summarise(
          nReviews = n_distinct(Title))
    }
    else {
      mdata_pivot %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment2,
          PFAS_type == input$inPFAS,
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>%
        summarise(
          nReviews = n_distinct(Title))
    }
  })
  
  data_base_semplified <- reactive({
    if (input$inHuman_animal_environment2 == "All" & input$inPFAS == "All") {
      # No filtering by Human_animal_environment
      mspiqdata %>%
        dplyr::filter(
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>%
        mutate(Title = str_replace_all(Title, "â€”" , "—")) %>% 
        mutate(Title = str_replace_all(Title, "â€\u0090" , "-")) %>% 
        mutate(Title = str_replace_all(Title, "â€™" , "'")) %>% 
        mutate(Title = str_replace_all(Title, "âˆ’" , "−")) %>% 
        mutate(First_author = str_replace_all(First_author, "�" , "ó")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã³" , "ó")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã©" , "é")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¡" , "á")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¤" , "ä")) %>% 
        rename(Subject = Human_animal_environment) %>% 
        rename("Year of publication" = Publication_year) %>%
        rename("First author" = First_author) %>% 
        select("First author", "Title", "Country", "Year of publication", "Subject", "DOI") 
    } 
    
    else if (input$inHuman_animal_environment2 != "All" & input$inPFAS == "All") {
      mspiqdata %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment2,
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>% 
        mutate(Title = str_replace_all(Title, "â€”" , "—")) %>% 
        mutate(Title = str_replace_all(Title, "â€\u0090" , "-")) %>% 
        mutate(Title = str_replace_all(Title, "â€™" , "'")) %>% 
        mutate(Title = str_replace_all(Title, "âˆ’" , "−")) %>% 
        mutate(First_author = str_replace_all(First_author, "�" , "ó")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã³" , "ó")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã©" , "é")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¡" , "á")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¤" , "ä")) %>% 
        rename(Subject = Human_animal_environment) %>% 
        rename("Year of publication" = Publication_year) %>%
        rename("First author" = First_author) %>% 
        select("First author", "Title", "Country", "Year of publication", "Subject", "DOI")
    }
    
    else if (input$inHuman_animal_environment2 == "All" & input$inPFAS != "All") {
      mspiqdata_pivot %>%
        dplyr::filter(
          PFAS_type == input$inPFAS,
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>% 
        mutate(Title = str_replace_all(Title, "â€”" , "—")) %>% 
        mutate(Title = str_replace_all(Title, "â€\u0090" , "-")) %>% 
        mutate(Title = str_replace_all(Title, "â€™" , "'")) %>% 
        mutate(Title = str_replace_all(Title, "âˆ’" , "−")) %>% 
        mutate(First_author = str_replace_all(First_author, "�" , "ó")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã³" , "ó")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã©" , "é")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¡" , "á")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¤" , "ä")) %>% 
        rename(Subject = Human_animal_environment) %>% 
        rename("Year of publication" = Publication_year) %>%
        rename("First author" = First_author) %>% 
        select("First author", "Title", "Country", "Year of publication", "Subject", "DOI")
    }
    
    else {
      mspiqdata_pivot %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment2,
          PFAS_type == input$inPFAS,
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>% 
        mutate(Title = str_replace_all(Title, "â€”" , "—")) %>% 
        mutate(Title = str_replace_all(Title, "â€\u0090" , "-")) %>% 
        mutate(Title = str_replace_all(Title, "â€™" , "'")) %>% 
        mutate(Title = str_replace_all(Title, "âˆ’" , "−")) %>% 
        mutate(First_author = str_replace_all(First_author, "�" , "ó")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã³" , "ó")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã©" , "é")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¡" , "á")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¤" , "ä")) %>% 
        rename(Subject = Human_animal_environment) %>% 
        rename("Year of publication" = Publication_year) %>%
        rename("First author" = First_author) %>% 
        select("First author", "Title", "Country", "Year of publication", "Subject", "DOI")
    }
  })
  
  
  data_base <- reactive({
    if (input$inHuman_animal_environment2 == "All" & input$inPFAS == "All") {
      # No filtering by Human_animal_environment
      mspiqdata %>%
        dplyr::filter(
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>%
        mutate(Title = str_replace_all(Title, "â€”" , "—")) %>% 
        mutate(Title = str_replace_all(Title, "â€\u0090" , "-")) %>% 
        mutate(Title = str_replace_all(Title, "â€™" , "'")) %>% 
        mutate(Title = str_replace_all(Title, "âˆ’" , "−")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã³" , "ó")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã©" , "é")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¡" , "á")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¤" , "ä")) %>% 
        rename(Subject = Human_animal_environment) %>% 
        rename("Year of publication" = Publication_year) %>%
        rename("First author" = First_author) %>% 
        select("First author", "Title", "Country", everything()) %>% 
        select(-c("Study_ID", "Author_year", "Paper_title", "Country_firstAuthor"))
    } 
    
    else if (input$inHuman_animal_environment2 != "All" & input$inPFAS == "All"){
      mspiqdata %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment2,
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>% 
        mutate(Title = str_replace_all(Title, "â€”" , "—")) %>% 
        mutate(Title = str_replace_all(Title, "â€\u0090" , "-")) %>% 
        mutate(Title = str_replace_all(Title, "â€™" , "'")) %>% 
        mutate(Title = str_replace_all(Title, "âˆ’" , "−")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã³" , "ó")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã©" , "é")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¡" , "á")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¤" , "ä")) %>% 
        rename(Subject = Human_animal_environment) %>% 
        rename("Year of publication" = Publication_year) %>%
        rename("First author" = First_author) %>% 
        select("First author", "Title", "Country", everything()) %>% 
        select(-c("Study_ID", "Author_year", "Paper_title", "Country_firstAuthor"))
    }
    
    else if (input$inHuman_animal_environment2 == "All" & input$inPFAS != "All"){
      mspiqdata_pivot %>%
        dplyr::filter(
          PFAS_type == input$inPFAS,
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>% 
        mutate(Title = str_replace_all(Title, "â€”" , "—")) %>% 
        mutate(Title = str_replace_all(Title, "â€\u0090" , "-")) %>% 
        mutate(Title = str_replace_all(Title, "â€™" , "'")) %>% 
        mutate(Title = str_replace_all(Title, "âˆ’" , "−")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã³" , "ó")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã©" , "é")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¡" , "á")) %>% 
        mutate(First_author = str_replace_all(First_author, "Ã¤" , "ä")) %>% 
        rename(Subject = Human_animal_environment) %>% 
        rename("Year of publication" = Publication_year) %>%
        rename("First author" = First_author) %>% 
        select("First author", "Title", "Country", everything()) %>% 
        select(-c("Study_ID", "Author_year", "Paper_title", "Country_firstAuthor"))
    }
    
    else {
      mspiqdata_pivot %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment2,
          PFAS_type == input$inPFAS,
          between(Publication_year, as.integer(input$inYearMin2), as.integer(input$inYearMax2))
        ) %>% 
      mutate(Title = str_replace_all(Title, "â€”" , "—")) %>% 
      mutate(Title = str_replace_all(Title, "â€\u0090" , "-")) %>% 
      mutate(Title = str_replace_all(Title, "â€™" , "'")) %>% 
      mutate(Title = str_replace_all(Title, "âˆ’" , "−")) %>% 
      mutate(First_author = str_replace_all(First_author, "Ã³" , "ó")) %>% 
      mutate(First_author = str_replace_all(First_author, "Ã©" , "é")) %>% 
      mutate(First_author = str_replace_all(First_author, "Ã¡" , "á")) %>% 
      mutate(First_author = str_replace_all(First_author, "Ã¤" , "ä")) %>% 
      rename(Subject = Human_animal_environment) %>% 
      rename("Year of publication" = Publication_year) %>%
      rename("First author" = First_author) %>% 
      select("First author", "Title", "Country", everything()) %>% 
      select(-c("Study_ID", "Author_year", "Paper_title", "Country_firstAuthor"))
    }
  })
  
  
  data_chart1 <- reactive({
    if (input$inHuman_animal_environment == "All") {
      # No filtering by Human_animal_environment
      alldata %>%
        dplyr::filter(
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
        distinct(Study_ID, Publication_year) %>%
        count(Publication_year)
    } else {
      alldata %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment,
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
        distinct(Study_ID, Publication_year) %>%
        count(Publication_year)
    }
  })
  
  data_chart2 <- reactive({
    if (input$inHuman_animal_environment == "All") {
      # No filtering by Human_animal_environment
      alldata %>%
        dplyr::filter(
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
        filter(Meta_analysis == "Yes") %>% 
        distinct(Study_ID, Publication_year) %>%
        count(Publication_year)
    } else {
      alldata %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment,
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
        filter(Meta_analysis == "Yes") %>%
        distinct(Study_ID, Publication_year) %>%
        count(Publication_year)
    }
  })
  
  pie_chartMA <- reactive({
    if (input$inHuman_animal_environment == "All") {
      # No filtering by Human_animal_environment
      alldata %>%
        dplyr::filter(
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
        distinct(Study_ID, Meta_analysis) %>%
        count(Meta_analysis) %>%
        arrange(desc(n))
    } else {
      alldata %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment,
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
        distinct(Study_ID, Meta_analysis) %>%
        count(Meta_analysis) %>%
        arrange(desc(n))
    }
  })
  
  pie_chartPFAS <- reactive({
    if (input$inHuman_animal_environment == "All") {
      # No filtering by Human_animal_environment
      alldata %>%
        dplyr::filter(
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
        dplyr::filter(PFAS_type != "NA") %>% 
        distinct(Study_ID, PFAS_type) %>%
        count(PFAS_type) %>% 
        arrange(desc(n))
    } else {
      alldata %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment,
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
        dplyr::filter(PFAS_type != "NA") %>% 
        distinct(Study_ID, PFAS_type) %>%
        count(PFAS_type) %>% 
        arrange(desc(n))
    }
  })
  
  pie_chartSpecies <- reactive({
    if (input$inHuman_animal_environment == "All") {
      # No filtering by Human_animal_environment
      alldata %>%
        dplyr::filter(
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
        mutate(Class_scientific_name = if_else(is.na(Class_scientific_name), "No animals reviewed", Class_scientific_name)) %>%
        dplyr::filter(Class_scientific_name != "na") %>% 
        distinct(Study_ID, Class_scientific_name) %>%
        count(Class_scientific_name) %>%
        arrange(desc(n))
    } else {
      alldata %>%
        dplyr::filter(
          Human_animal_environment == input$inHuman_animal_environment,
          between(Publication_year, as.integer(input$inYearMin), as.integer(input$inYearMax))
        ) %>%
        mutate(Class_scientific_name = if_else(is.na(Class_scientific_name), "No animals reviewed", Class_scientific_name)) %>%
        dplyr::filter(Class_scientific_name != "na") %>% 
        distinct(Study_ID, Class_scientific_name) %>%
        count(Class_scientific_name) %>%
        arrange(desc(n))
    }
  })
  
  output$dataset_semplified <- renderDT(
    
    # render table
    data_base_semplified(),
    filter = 'top',
    rownames = FALSE,
    extensions = "Buttons",
    options = list(pageLength = 10,
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
  
  
  output$dataset <- renderDT(
    
    # render table
    data_base(),
    filter = 'top',
    rownames = FALSE,
    extensions = "Buttons",
    options = list(pageLength = 10,
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
    data_base_summary()$nReviews
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
      hc_title(text = "Number of reviews including a meta-analysis per year", align = "left") |>
      hc_xAxis(title = list(text = "Year")) |>
      hc_yAxis(title = list(text = "Number of meta-analyses"))
  })
  
  output$pie_chartMA <- renderHighchart({
    hchart(pie_chartMA(), "pie", hcaes(x = c("No", "Yes"), y = n, fill = Meta_analysis)) |>
      hc_title(text = "Does the review include a meta-analysis?", align = "left") |>
      hc_plotOptions(
        pie = list(
          innerSize = '50%'  # This sets the inner size to 50%, making it a donut chart
        )
      ) |>
      hc_xAxis(title = NULL) |>
      hc_yAxis(title = list(text = "")) %>% 
      hc_tooltip(pointFormat = "<b>{point.y}</b>")
  })
  
  output$pie_chartPFAS <- renderHighchart({
    hchart(pie_chartPFAS(), "pie", hcaes(x = PFAS_type, y = n, fill = PFAS_type)) |>
      hc_title(text = "Number of reviews per compound", align = "left") |>
      hc_plotOptions(
        pie = list(
          innerSize = '30%'  # This sets the inner size to 50%, making it a donut chart
        )
      ) |>
      hc_xAxis(title = NULL) |>
      hc_yAxis(title = list(text = "")) %>% 
      hc_tooltip(pointFormat = "<b>{point.y}</b>")
  })
  
  output$pie_chartSpecies <- renderHighchart({
    hchart(pie_chartSpecies(), "pie", hcaes(x = Class_scientific_name, y = n, fill = Class_scientific_name)) |>
      hc_title(text = "Number of reviews per class of species", align = "left") |>
      hc_plotOptions(
        pie = list(
          innerSize = '30%'  # This sets the inner size to 50%, making it a donut chart
        )
      ) |>
      hc_xAxis(title = NULL) |>
      hc_yAxis(title = list(text = "")) %>% 
      hc_tooltip(pointFormat = "<b>{point.y}</b>")
  })
  
  
}
