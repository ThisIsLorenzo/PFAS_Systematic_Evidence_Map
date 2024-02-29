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
        input$dataSelector6)
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
    
    # For Summary Output
    output$summary <- renderUI({
      mdata2 <- mdata %>%
        separate(Author_year, c("First_author", "Publication_year"), sep = "_") %>%
        mutate(First_author = str_replace_all(First_author, "First_author", "First author"),
               Publication_year = str_replace_all(Publication_year, "Publication_year", "Publication year")) %>%
        filter(First_author != "NA")
      
      summary_table <- knitr::kable(select(mdata2,
                                           First_author,
                                           Publication_year,
                                           Paper_title,
                                           DOI), caption = "Table of included SRs") %>%
        kableExtra::kable_paper(bootstrap_options = "striped", full_width = FALSE)
      
      HTML(summary_table)
    })
    
    # For Structure output
    output$structure <- renderPrint({
      my_data %>% 
        str()
    })
    
    # stacked histogram
    output$histplot1 <- renderPlotly({
      p1
    })
    
    output$histplot2 <- renderPlotly({
      p2
    })
    
    output$histplot3 <- renderPlotly({
      p3
    })
    
    output$histplot4 <- renderPlotly({
      p4
    })
    
    output$histplot5 <- renderPlotly({
      p5
    })
    
    output$histplot6 <- renderPlotly({
      q1
    })
    
    
  }, ignoreNULL = FALSE)
}
