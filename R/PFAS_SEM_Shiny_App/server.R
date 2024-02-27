function(input, output, session) {
  
  # variables always present
  always_vars <- c("Study_ID", "Publication_year")
  
  re_selected_vars <- reactive({
    # collect all selected variables
    selected_vars <- 
      c(always_vars,
        input$dataSelector1,
        input$dataSelector2,
        input$dataSelector3,
        input$dataSelector4)
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
                options = list(pageLength = 50,
                               dom = "Blfrtip",
                               autoWidth = TRUE,
                               bAutoWidth = FALSE,
                               buttons = c('copy', 'csv', 'excel', 'pdf'),
                               columnDefs = list(list(className = 'dt-left', targets = '_all'))))
    )
    
    # output$selected_var <- renderText({
    #   paste("You selected", names(re_database()))
    # })
    
    
  }, ignoreNULL = FALSE)
}
