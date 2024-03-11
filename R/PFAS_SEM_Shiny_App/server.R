function(input, output, session) {
  
  # variables always present
  always_vars <- c("Author_year", "Paper_title", "Publication_year")
  
  re_selected_vars <- reactive({
    # collect all selected variables
    selected_vars <- 
      c(always_vars,
        input$dataSelector1,
        input$dataSelector2,
        input$dataSelector3,
        input$dataSelector4,
        input$dataSelector5,
        input$dataSelector6,
        input$dataSelector7)
    })
  
  
  re_relevant_sets <- reactive({
    relevant_sets <- sapply(re_selected_vars(), USE.NAMES = F,
                            FUN = function(x) {
                              vars_sets[x][[1]]
                            })
    
    relevant_sets[sapply(relevant_sets, is.null)] <- NULL
    unlist(unique(relevant_sets))
  })
  
  re_database <- reactive({
    current_dataframes <- my_data[re_relevant_sets()]
    database <- Reduce(dplyr::left_join, current_dataframes) %>%
      select(re_selected_vars()[re_selected_vars() != "NULL"])
    as.data.frame(database)
  })
  
  
  observeEvent(input$pltChange, {
    
    
    output$gapminder_table <- renderDataTable(
      
      # render table
      datatable(re_database(),
                filter = 'top',
                rownames = FALSE,
                extensions = "Buttons",
                options = list(pageLength = 200,
                               dom = "Bfrtip",
                               autoWidth = TRUE,
                               bAutoWidth = FALSE,
                               scrollX = TRUE,
                               buttons = list(
                                 list(extend = 'copy', text = 'Copy'),
                                 list(extend = 'csv', text = 'CSV'),
                                 list(extend = 'excel', text = 'Excel'),
                                 list(extend = 'pdf', text = 'PDF'),
                                 list(extend = 'pageLength', text = 'Entries')
                               ),
                               columnDefs = list(list(className = 'dt-left', targets = '_all'))),
                callback = JS('table.page.len(-1).draw();')
                )
      )
    
    
    # For Summary Output - Mapping
    output$summary0.1 <- renderPrint({
      journal_counts <- table(mdata$Journal)
      sorted_counts <- sort(journal_counts, decreasing = TRUE)
      top_10_journals <- head(sorted_counts, 10)
      cat("Top 10 Most Abundant Journals:\n\n")
      print(top_10_journals)
    })
    
    output$summary0.2 <- renderPrint({
      country_counts <- table(mdata$Country_firstAuthor)
      sorted_counts2 <- sort(country_counts, decreasing = TRUE)
      top_10_countries <- head(sorted_counts2, 10)
      cat("Top 10 Country of first author:\n\n")
      print(top_10_countries)
    })
    
    output$summary0.3 <- renderPrint({
      review_counts <- table(mdata$Review_type_claimed)
      sorted_counts3 <- sort(review_counts, decreasing = TRUE)
      top_10_review_types <- head(sorted_counts3, 10)
      cat("Number of review types:\n\n")
      print(top_10_review_types)
    })
    
    output$summary0.4 <- renderPrint({
      sa_counts <- table(mdata$Systematic_approach)
      cat("Number of reviews with or without a systematic approach:\n\n")
      print(sa_counts)
    })
    
    output$summary0.5 <- renderPrint({
      protocol_counts <- table(mdata$Protocol)
      cat("Number of reviews with or without a research protocol:\n\n")
      print(protocol_counts)
    })
    
    output$summary0.6 <- renderPrint({
      ma_counts <- table(mdata$Meta_analysis)
      cat("Number of reviews with or without a meta-analysis:\n\n")
      print(ma_counts)
    })
    
    output$summary0.7 <- renderPrint({
      sub_counts <- table(mdata$Human_animal_environment)
      cat("Number of reviews on humans, animals, or the environment:\n\n")
      print(sub_counts)
    })
    
    output$summary0.8 <- renderPrint({
      species_counts <- table(spdata$Species_scientific_name)
      sorted_counts4 <- sort(species_counts, decreasing = TRUE)
      top_10_species <- head(sorted_counts4, 10)
      cat("Top 10 Most reviewed species:\n\n")
      print(top_10_species)
    })
    
    output$summary1 <- renderPrint({
      my_data$review %>% 
        skim()
    })
    
    output$summary1.1 <- renderPrint({
      my_data$species %>% 
        skim()
    })
    
    # For Summary Output - Appraisal
    output$summary2 <- renderPrint({
      q1_counts <- table(qdata$`Q1. Are the research questions and inclusion criteria for the review clearly delineated?`)
      cat("Number of reviews with high, medium, or low score for appraisal question #1:\n\n")
      print(q1_counts)
    })
    
    output$summary2.1 <- renderPrint({
      q2_counts <- table(qdata$`Q2. Did the report of the review contain an explicit statement that the review methods were established prior to the conduct of the review and did the report justify any significant deviations from the protocol?`)
      cat("Number of reviews with high, medium, or low score for appraisal question #2:\n\n")
      print(q2_counts)
    })
    
    output$summary2.2 <- renderPrint({
      q3_counts <- table(qdata$`Q3. Did the review authors explain their selection of the study designs for inclusion in the review?`)
      cat("Number of reviews with high, medium, or low score for appraisal question #3:\n\n")
      print(q3_counts)
    })
    
    output$summary2.3 <- renderPrint({
      q4_counts <- table(qdata$`Q4. Did the review authors use a comprehensive literature search strategy?`)
      cat("Number of reviews with high, medium, or low score for appraisal question #4:\n\n")
      print(q4_counts)
    })
    
    output$summary2.4 <- renderPrint({
      q5_counts <- table(qdata$`Q5. Did the review authors perform study selection in duplicate?`)
      cat("Number of reviews with high, medium, or low score for appraisal question #5:\n\n")
      print(q5_counts)
    })
    
    output$summary2.5 <- renderPrint({
      q6_counts <- table(qdata$`Q6. Did the review authors perform data extraction in duplicate?`)
      cat("Number of reviews with high, medium, or low score for appraisal question #6:\n\n")
      print(q6_counts)
    })
    
    output$summary2.6 <- renderPrint({
      q7_counts <- table(qdata$`Q7. Did the review authors provide a list of excluded studies and justify the exclusions?`)
      cat("Number of reviews with high, medium, or low score for appraisal question #7:\n\n")
      print(q7_counts)
    })
    
    output$summary2.7 <- renderPrint({
      q8_counts <- table(qdata$`Q8. Did the review authors describe the included studies in adequate detail?`)
      cat("Number of reviews with high, medium, or low score for appraisal question #8:\n\n")
      print(q8_counts)
    })
    
    output$summary2.8 <- renderPrint({
      q9_counts <- table(qdata$`Q9. Did the review authors use a satisfactory technique for assessing the risk of bias (RoB) in individual studies that were included in the review?`)
      cat("Number of reviews with high, medium, or low score for appraisal question #9:\n\n")
      print(q9_counts)
    })
    
    output$summary2.9 <- renderPrint({
      q10_counts <- table(qdata$`Q10. Did the review authors report on the sources of funding for the studies included in the review?`)
      cat("Number of reviews with high, medium, or low score for appraisal question #10:\n\n")
      print(q10_counts)
    })
    
    output$summary2.10 <- renderPrint({
      q11_counts <- table(qdata$`Q11. If meta-analysis was performed did the review authors use appropriate methods for statistical combination of results?`)
      cat("Number of reviews with high, medium, or low score for appraisal question #11:\n\n")
      print(q11_counts)
    })
    
    output$summary2.11 <- renderTable({
      mqdata_result
    })
    
    
    
    # For Summary Output - Bibliometrics
    output$summary3 <- renderPrint({
      my_data$biblio %>% 
        skim()
    })
    
    
    
    # For Structure output
    output$structure <- renderPrint({
      my_data %>% 
        str()
    })
    
    # Plots
    # MAPPING
    # Timetrends
    output$histplot1 <- renderPlotly({
      p1.1
    })
    
    output$histplot2 <- renderPlotly({
      p1.2
    })
    
    output$histplot3 <- renderPlotly({
      p1.3
    })
    
    output$histplot4 <- renderPlotly({
      p1.4
    })
    
    # Subjects
    output$histplot5 <- renderPlotly({
      p1.5
    })
    
    output$histplot6 <- renderPlotly({
      p1.6
    })
    
    output$histplot7 <- renderPlotly({
      p1.7
    })
    
    output$histplot8 <- renderPlotly({
      p1.8
    })
    
    # PFAS
    output$histplot9 <- renderPlotly({
      p1.9
    })
    
    output$histplot10 <- renderPlotly({
      p1.10
    })
    
    output$histplot11 <- renderPlotly({
      p1.11
    })
    
    
    #CRITICAL APPRAISAL
    output$histplot12 <- renderPlotly({
      q1
    })
    
    output$geomline1 <- renderPlotly({
      q2.1
    })
    
    output$geomline2 <- renderPlotly({
      q2.2
    })
    
    output$geomline3 <- renderPlotly({
      q2.3
    })
    
    output$geomline4 <- renderPlotly({
      q2.4
    })
    
    output$geomline5 <- renderPlotly({
      q2.5
    })
    
    # BIBLIOMETRICS
    
    
    
  }, ignoreNULL = FALSE)
}
