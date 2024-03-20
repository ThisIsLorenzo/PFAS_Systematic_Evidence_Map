function(input, output, session) {
  
  js <- "
function(cell) {
  var $cell = $(cell);
  $cell.contents().wrapAll('<div class=\\\"content\\\"></div>');
  var $content = $cell.find('.content');
  $cell.append($('<button>Read more</button>'));
  $btn = $cell.find('button');
  $content.css({
    height: '50px',
    overflow: 'hidden'
  });
  $cell.data('isLess', true);
  $btn.click(function () {
    var isLess = $cell.data('isLess');
    $content.css('height', isLess ? 'auto' : '50px');
    $(this).text(isLess ? 'Read less' : 'Read more');
    $cell.data('isLess', !isLess);
  });
}
"

custom_css <- "
  .custom-row-height tr td {
    height: 20px; /* Adjust height as needed */
    padding: 5px; /* Optional: Adjust padding */
  }
"

  
  mapping_database <- reactive({
    m_database <- my_data$review
    
    as.data.frame(m_database)
  })
  
  observeEvent(input$pltChange, {
    
    
    output$mapping_table <- renderDT(
      
      # render table
      mapping_database(),
      filter = 'top',
      rownames = FALSE,
      extensions = "Buttons",
      options = list(pageLength = 10,
                     dom = "Bfrtp",
                     width = '100%',
                     autoWidth = TRUE,
                     buttons = list(
                       list(extend = 'copy', text = 'Copy'),
                       list(extend = 'csv', text = 'CSV'),
                       list(extend = 'excel', text = 'Excel'),
                       list(extend = 'pdf', text = 'PDF'),
                       list(extend = 'pageLength', text = 'Entries')
                       )
                     )
      )
  })
  
    appraisal_database <- reactive({
      a_database <- my_data$AMSTAR2
      as.data.frame(a_database)
    })
  
    
    observeEvent(input$pltChange, {
      
      
      output$appraisal_table <- renderDT(
        
        # render table
        appraisal_database(),
        filter = 'top',
        rownames = FALSE,
        extensions = "Buttons",
        options = list(pageLength = 10,
                       dom = "Bfrtip",
                       width = '100%',
                       autoWidth = TRUE,
                       buttons = list(
                         list(extend = 'copy', text = 'Copy'),
                         list(extend = 'csv', text = 'CSV'),
                         list(extend = 'excel', text = 'Excel'),
                         list(extend = 'pdf', text = 'PDF'),
                         list(extend = 'pageLength', text = 'Entries')
                       ))
      )
      
    })
    
    biblio_database <- reactive({
      b_database <- my_data$biblio
      as.data.frame(b_database)
    })
    
    
    observeEvent(input$pltChange, {
      
      
      output$biblio_table <- renderDT(
        
        # render table
        biblio_database(),
        filter = 'top',
        rownames = FALSE,
        extensions = "Buttons",
        options = list(pageLength = 10,
                       dom = "Bfrtip",
                       width = '100%',
                       autoWidth = TRUE,
                       buttons = list(
                         list(extend = 'copy', text = 'Copy'),
                         list(extend = 'csv', text = 'CSV'),
                         list(extend = 'excel', text = 'Excel'),
                         list(extend = 'pdf', text = 'PDF'),
                         list(extend = 'pageLength', text = 'Entries')
                       ))
      )
      
    })
    
    whole_database <- reactive({
      whole_database <- Reduce(function(x, y) left_join(x, y, by = "Study_ID"), my_data_no_biblio)
      as.data.frame(whole_database)
    })
    
    
    observeEvent(input$pltChange, {
      
      
      output$whole_d_table <- renderDT(
        
        # render table
        whole_database(),
        filter = 'top',
        rownames = FALSE,
        extensions = "Buttons",
        options = list(pageLength = 10,
                       dom = "Bfrtip",
                       width = '100%',
                       autoWidth = TRUE,
                       buttons = list(
                         list(extend = 'copy', text = 'Copy'),
                         list(extend = 'csv', text = 'CSV'),
                         list(extend = 'excel', text = 'Excel'),
                         list(extend = 'pdf', text = 'PDF'),
                         list(extend = 'pageLength', text = 'Entries')
                       ))
      )
    
      
    
    # For Summary Output - Mapping
    output$summary0.1 <- renderDT({
      journal_counts <- table(mdata$Journal)
      sorted_counts <- sort(journal_counts, decreasing = TRUE)
      data.frame(Journal = names(sorted_counts), Number_of_reviews = as.vector(sorted_counts))
    }, rownames = FALSE)
    
    output$summary0.2 <- renderDT({
      country_counts <- table(mdata$Country_firstAuthor)
      sorted_counts2 <- sort(country_counts, decreasing = TRUE)
      data.frame(Country_first_author = names(sorted_counts2), Number_of_reviews = as.vector(sorted_counts2))
    }, rownames = FALSE)
    
    output$summary0.3 <- renderDT({
      review_counts <- table(mdata$Review_type_claimed)
      sorted_counts3 <- sort(review_counts, decreasing = TRUE)
      data.frame(Review_Type = names(sorted_counts3), Number_of_reviews = as.vector(sorted_counts3))
    }, rownames = FALSE)
    
    output$summary0.4 <- renderDT({
      sa_counts <- table(mdata$Systematic_approach)
      data.frame(Systematic_approach = names(sa_counts), Number_of_reviews = as.vector(sa_counts))
    }, rownames = FALSE)
    
    output$summary0.5 <- renderDT({
      protocol_counts <- table(mdata$Protocol)
      data.frame(Protocol = names(protocol_counts), Number_of_reviews = as.vector(protocol_counts))
    }, rownames = FALSE)
    
    output$summary0.6 <- renderDT({
      ma_counts <- table(mdata$Meta_analysis)
      data.frame(Meta_analysis = names(ma_counts), Number_of_reviews = as.vector(ma_counts))
    }, rownames = FALSE)
    
    output$summary0.7 <- renderDT({
      sub_counts <- table(mdata$Human_animal_environment)
      data.frame(Subject = names(sub_counts), Number_of_reviews = as.vector(sub_counts))
    }, rownames = FALSE)
    
    output$summary0.8 <- renderDT({
      species_counts <- spdata[!is.na(spdata$Species_scientific_name), ]
      species_counts <- table(species_counts$Species_scientific_name)
      sorted_counts4 <- sort(species_counts, decreasing = TRUE)
      data.frame(Species = names(sorted_counts4), Number_of_reviews = as.vector(sorted_counts4))
    }, rownames = FALSE)
    
    output$summary0.9 <- renderDT({
      pfas_focus <- table(mdata$PFAS_focus)
      data.frame(PFAS_focus = names(pfas_focus), Number_of_reviews = as.vector(pfas_focus))
    }, rownames = FALSE)
    
    output$summary1 <- renderDT({
      pfas_count <- table(mdata$PFAS_one_many)
      data.frame(Compounds_reviewed = names(pfas_count), Number_of_reviews = as.vector(pfas_count))
    }, rownames = FALSE)
    
    output$summary1.1 <- renderDT({
      pecos_count <- table(mdata$Outcomes_PECOS)
      data.frame(Reviews_using_a_PECOs_framework = names(pecos_count), Number_of_reviews = as.vector(pecos_count))
    }, rownames = FALSE)
    
    output$summary1.2 <- renderDT({
      outcomes_counts <- table(mdata$Outcomes)
      sorted_counts5 <- sort(outcomes_counts, decreasing = TRUE)
      data.frame(Reviews_outcomes = names(sorted_counts5), Number_of_reviews = as.vector(sorted_counts5))
    }, rownames = FALSE)
    
    output$summary1.3 <- renderDT({
      outcomes_cat_counts <- table(mdata$Outcome_category)
      sorted_counts6 <- sort(outcomes_cat_counts, decreasing = TRUE)
      data.frame( Reviews_outcome_categories = names(sorted_counts6), Number_of_reviews = as.vector(sorted_counts6))
    }, rownames = FALSE)
    
    
    
    # For Summary Output - Appraisal
    output$summary2 <- renderDT({
      q1_counts <- table(qdata$`Q1. Are the research questions and inclusion criteria for the review clearly delineated?`)
      data.frame( Item_1 = names(q1_counts), Number_of_reviews = as.vector(q1_counts))
    }, rownames = FALSE)
    
    output$summary2.1 <- renderDT({
      q2_counts <- table(qdata$`Q2. Did the report of the review contain an explicit statement that the review methods were established prior to the conduct of the review and did the report justify any significant deviations from the protocol?`)
      data.frame( Item_2 = names(q2_counts), Number_of_reviews = as.vector(q2_counts))
    }, rownames = FALSE)
    
    output$summary2.2 <- renderDT({
      q3_counts <- table(qdata$`Q3. Did the review authors explain their selection of the study designs for inclusion in the review?`)
      data.frame( Item_3 = names(q3_counts), Number_of_reviews = as.vector(q3_counts))
    }, rownames = FALSE)
    
    output$summary2.3 <- renderDT({
      q4_counts <- table(qdata$`Q4. Did the review authors use a comprehensive literature search strategy?`)
      data.frame( Item_4 = names(q4_counts), Number_of_reviews = as.vector(q4_counts))
    }, rownames = FALSE)
    
    output$summary2.4 <- renderDT({
      q5_counts <- table(qdata$`Q5. Did the review authors perform study selection in duplicate?`)
      data.frame( Item_5 = names(q5_counts), Number_of_reviews = as.vector(q5_counts))
    }, rownames = FALSE)
    
    output$summary2.5 <- renderDT({
      q6_counts <- table(qdata$`Q6. Did the review authors perform data extraction in duplicate?`)
      data.frame( Item_6 = names(q6_counts), Number_of_reviews = as.vector(q6_counts))
    }, rownames = FALSE)
    
    output$summary2.6 <- renderDT({
      q7_counts <- table(qdata$`Q7. Did the review authors provide a list of excluded studies and justify the exclusions?`)
      data.frame( Item_7 = names(q7_counts), Number_of_reviews = as.vector(q7_counts))
    }, rownames = FALSE)
    
    output$summary2.7 <- renderDT({
      q8_counts <- table(qdata$`Q8. Did the review authors describe the included studies in adequate detail?`)
      data.frame( Item_8 = names(q8_counts), Number_of_reviews = as.vector(q8_counts))
    }, rownames = FALSE)
    
    output$summary2.8 <- renderDT({
      q9_counts <- table(qdata$`Q9. Did the review authors use a satisfactory technique for assessing the risk of bias (RoB) in individual studies that were included in the review?`)
      data.frame( Item_9 = names(q9_counts), Number_of_reviews = as.vector(q9_counts))
    }, rownames = FALSE)
    
    output$summary2.9 <- renderDT({
      q10_counts <- table(qdata$`Q10. Did the review authors report on the sources of funding for the studies included in the review?`)
      data.frame( Item_10 = names(q10_counts), Number_of_reviews = as.vector(q10_counts))
    }, rownames = FALSE)
    
    output$summary2.10 <- renderDT({
      q11_counts <- table(qdata$`Q11. If meta-analysis was performed did the review authors use appropriate methods for statistical combination of results?`)
      data.frame( Item_11 = names(q11_counts), Number_of_reviews = as.vector(q11_counts))
    }, rownames = FALSE)
    
    output$summary2.11 <- renderDT({
      q12_counts <- table(qdata$`Q12. If meta-analysis was performed, did the review authors assess the potential impact of RoB in individual studies on the results of the meta-analysis or other evidence synthesis?`)
      data.frame( Item_12 = names(q12_counts), Number_of_reviews = as.vector(q12_counts))
    }, rownames = FALSE)
    output$summary2.12 <- renderDT({
      q13_counts <- table(qdata$`Q13. Did the review authors account for RoB in individual studies when interpreting/ discussing the results of the review?`)
      data.frame( Item_13 = names(q13_counts), Number_of_reviews = as.vector(q13_counts))
    }, rownames = FALSE)
    
    output$summary2.13 <- renderDT({
      q14_counts <- table(qdata$`Q14. Did the review authors provide a satisfactory explanation for, and discussion of, any heterogeneity observed in the results of the review?`)
      data.frame( Item_14 = names(q14_counts), Number_of_reviews = as.vector(q14_counts))
    }, rownames = FALSE)
    
    output$summary2.14 <- renderDT({
      q15_counts <- table(qdata$`Q15. If they performed quantitative synthesis did the review authors carry out an adequate investigation of publication bias (small study bias) and discuss its likely impact on the results of the review?`)
      data.frame( Item_15 = names(q15_counts), Number_of_reviews = as.vector(q15_counts))
    }, rownames = FALSE)
    
    output$summary2.15 <- renderDT({
      q16_counts <- table(qdata$`Q16. Did the review authors report any potential sources of conflict of interest, including any funding they received for conducting the review?`)
      data.frame( Item_5 = names(q16_counts), Number_of_reviews = as.vector(q16_counts))
    }, rownames = FALSE)
    
    
    # For Summary Output - Bibliometrics
    
    output$summary3 <- renderDT({
      results1 <- biblioAnalysis(bib_data)
      Reviews_citations <- as.data.frame(results1[["MostCitedPapers"]])
      colnames(Reviews_citations)[colnames(Reviews_citations) == "Paper         "] <- "Paper"
      data.frame( Article = Reviews_citations$Paper,
                  DOI = Reviews_citations$DOI,
                  Citations = Reviews_citations$TC,
                  Citation_per_year = Reviews_citations$TCperYear,
                  N_citations = Reviews_citations$NTC
                  )
    }, rownames = FALSE)
    
    
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
    
    output$netmatrix <- renderPlotly({
      NetMatrix_country_plot
    })
    
  }, ignoreNULL = FALSE)
}
