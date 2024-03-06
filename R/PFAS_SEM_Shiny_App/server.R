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
    database <- Reduce(dplyr::left_join, current_dataframes)
    
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
                callback = JS('table.page.len(-1).draw();'))
    )
    
    
    # For Summary Output - Mapping
    output$summary1 <- renderPrint({
      mdata %>% 
        skim()
    })
    
    output$summary1.1 <- renderPrint({
      spdata %>% 
        skim()
    })
    
    # For Summary Output - Appraisal
    output$summary2 <- renderPrint({
      qdata %>% 
        skim()
    })
    
    # For Summary Output - Bibliometrics
    output$summary3 <- renderPrint({
      bib_data %>% 
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
    
    
    
  }, ignoreNULL = FALSE)
}
