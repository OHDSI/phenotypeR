server <- function(input, output, session) {

  # achilles results -----
  working_achilles_data <- reactive({
      working_result <- result |>
      filter(cdm_name %in% input$achilles_cdm_name,
             group_level %in% input$achilles_codelist_name,
             strata_level %in% input$achilles_codelist_domain)

      if(isFALSE(input$achilles_records)){
        working_result <- working_result %>%
          filter(estimate_name != "record_count")
      }

      if(isFALSE(input$achilles_subjects)) {
        working_result <- working_result %>%
          filter(estimate_name != "person_count")
      }

       working_result

  })

  output$gt_achilles_code_count <- render_gt({
    validate(
      need(nrow(working_achilles_data()) >0, "No result found")
    )
      CodelistGenerator::tableAchillesCodeUse(working_achilles_data(),
                                              type = "gt")
    })
  output$raw_achilles_code <- renderDataTable({
    validate(
      need(nrow(working_achilles_data()) >0, "No result found")
    )
   table <-  CodelistGenerator::tableAchillesCodeUse(working_achilles_data(),
                                            type = "tibble")
   names(table) <- gsub("\\[.*?\\]", "", names(table))
   names(table) <- str_replace_all(names(table), "CDM name\n", " ")
   names(table) <- str_replace_all(names(table), "\n", ": ")

   if(length(names(table))>=7){
     table[[7]] <- as.numeric(table[[7]])
   }
   if(length(names(table))>=8){
     table[[8]] <- as.numeric(table[[7]])
   }

   datatable(table, rownames= FALSE)

  })
  # output type depends on input
  output$table_achilles_code_count <- renderUI({
      if (input$achilles_table_type == "tidy") {
        gt_output("gt_achilles_code_count")
      } else {
        dataTableOutput("raw_achilles_code")
      }
    })

  # orphan code results -----
  working_orphan_data <- reactive({
    working_result <- result |>
      filter(cdm_name %in% input$orphan_cdm_name,
             group_level %in% input$orphan_codelist_name,
             strata_level %in% input$orphan_codelist_domain)

    if(isFALSE(input$orphan_records)){
      working_result <- working_result %>%
        filter(estimate_name != "record_count")
    }

    if(isFALSE(input$orphan_subjects)) {
      working_result <- working_result %>%
        filter(estimate_name != "person_count")
    }

    working_result

  })

  output$gt_orphan_code_count <- render_gt({
    validate(
      need(nrow(working_orphan_data()) >0, "No result found")
    )
    CodelistGenerator::tableOrphanCodes(working_orphan_data(),
                                            type = "gt")
  })
  output$raw_orphan_code <- renderDataTable({
    validate(
      need(nrow(working_orphan_data()) >0, "No result found")
    )
    table <-  CodelistGenerator::tableOrphanCodes(working_orphan_data(),
                                                      type = "tibble")
    names(table) <- gsub("\\[.*?\\]", "", names(table))
    names(table) <- str_replace_all(names(table), "CDM name\n", " ")
    names(table) <- str_replace_all(names(table), "\n", ": ")

    if(length(names(table))>=7){
      table[[7]] <- as.numeric(table[[7]])
    }
    if(length(names(table))>=8){
      table[[8]] <- as.numeric(table[[7]])
    }

    datatable(table, rownames= FALSE)

  })
  # output type depends on input
  output$table_orphan_code_count <- renderUI({
    if (input$orphan_table_type == "tidy") {
      gt_output("gt_orphan_code_count")
    } else {
      dataTableOutput("raw_orphan_code")
    }
  })


  # cohort_count results -----
  working_cohort_count_data <- reactive({
    working_result <- result |>
      filter(cdm_name %in% input$cohort_count_cdm_name,
             group_level %in% input$cohort_count_cohort_name)

    if(isFALSE(input$cohort_count_records)){
      working_result <- working_result %>%
        filter(variable_name != "Number records")
    }

    if(isFALSE(input$cohort_count_subjects)) {
      working_result <- working_result %>%
        filter(variable_name != "Number subjects")
    }

    working_result

  })

  output$gt_cohort_count_code_count <- render_gt({
    validate(
      need(nrow(working_cohort_count_data()) >0, "No result found")
    )
    CohortCharacteristics::tableCohortCount(working_cohort_count_data(),
                                            type = "gt")
  })
  output$raw_cohort_count_code <- renderDataTable({
    validate(
      need(nrow(working_cohort_count_data()) >0, "No result found")
    )
    table <-  CohortCharacteristics::tableCohortCount(working_cohort_count_data(),
                                                      type = "tibble")
    names(table) <- gsub("\\[.*?\\]", "", names(table))
    names(table) <- str_replace_all(names(table), "CDM name\n", " ")
    names(table) <- str_replace_all(names(table), "\n", ": ")

    if(length(names(table))>=7){
      table[[7]] <- as.numeric(table[[7]])
    }
    if(length(names(table))>=8){
      table[[8]] <- as.numeric(table[[7]])
    }

    datatable(table, rownames= FALSE)

  })
  # output type depends on input
  output$table_cohort_count_code_count <- renderUI({
    if (input$cohort_count_table_type == "tidy") {
      gt_output("gt_cohort_count_code_count")
    } else {
      dataTableOutput("raw_cohort_count_code")
    }
  })

  # index events ------
  working_index_events_data <- reactive({

    working_result <- result |>
       visOmopResults::filterSettings(result_type == "cohort_code_use") |>
       visOmopResults::splitGroup() |>
       filter(cdm_name %in% input$index_events_cdm_name,
              cohort_name %in% input$index_events_cohort_name,
              codelist_name %in% input$index_events_codelist_name) |>
      visOmopResults::uniteGroup(cols = c("cohort_name", "codelist_name"))
    working_result <- omopgenerics::newSummarisedResult(working_result,
                                                        settings = omopgenerics::settings(result))

    if(isFALSE(input$index_events_records)){
      working_result <- working_result %>%
        filter(estimate_name != "record_count")
    }

    if(isFALSE(input$index_events_subjects)) {
      working_result <- working_result %>%
        filter(estimate_name != "person_count")
    }

    working_result

  })

  output$gt_index_events_code_count <- render_gt({
    validate(
      need(nrow(working_index_events_data()) >0, "No result found")
    )
    CodelistGenerator::tableCohortCodeUse(working_index_events_data(),
                                            type = "gt")
  })
  output$raw_index_events_code <- renderDataTable({
    validate(
      need(nrow(working_index_events_data()) >0, "No result found")
    )
    table <-  CodelistGenerator::tableCohortCodeUse(working_index_events_data(),
                                                      type = "tibble")
    names(table) <- gsub("\\[.*?\\]", "", names(table))
    names(table) <- str_replace_all(names(table), "CDM name\n", " ")
    names(table) <- str_replace_all(names(table), "\n", ": ")

    if(length(names(table))>=7){
      table[[7]] <- as.numeric(table[[7]])
    }
    if(length(names(table))>=8){
      table[[8]] <- as.numeric(table[[7]])
    }

    datatable(table, rownames= FALSE)

  })
  # output type depends on input
  output$table_index_events_code_count <- renderUI({
    if (input$index_events_table_type == "tidy") {
      gt_output("gt_index_events_code_count")
    } else {
      dataTableOutput("raw_index_events_code")
    }
  })



  # attrition results -----
  working_attrition_data <- reactive({
    if(nrow(result) > 0){
      result |>
        visOmopResults::filterSettings(result_type == "cohort_attrition")  |>
        visOmopResults::filterSettings(cohort_name  %in% input$attrition_cohort_name)
    } else {
      result
    }
  })
  output$gt_cohort_attrition <- render_gt({
    validate(
      need(nrow(working_attrition_data()) >0, "No result found")
    )
    CohortCharacteristics::tableCohortAttrition(working_attrition_data())
  })
  output$gg_cohort_attrition <- renderGrViz({
    validate(
      need(nrow(working_attrition_data()) >0, "No result found")
    )
    CohortCharacteristics::plotCohortAttrition(working_attrition_data())
  })

  output$gt_cohort_overlap <- render_gt({
    validate(
      need(nrow(working_attrition_data()) >0, "No result found")
    )
    CohortCharacteristics::tableCohortOverlap(result)
  })
  output$gg_cohort_overlap <- renderPlotly({
    validate(
      need(nrow(working_attrition_data()) >0, "No result found")
    )
   plotly::ggplotly(CohortCharacteristics::plotCohortOverlap(result))
  })

  output$gt_cohort_timing <- render_gt({
    validate(
      need(nrow(working_attrition_data()) >0, "No result found")
    )
    CohortCharacteristics::tableCohortTiming(result)
  })

  output$gg_cohort_timing <- renderPlotly({
    validate(
      need(nrow(working_attrition_data()) >0, "No result found")
    )
    plotly::ggplotly(CohortCharacteristics::plotCohortTiming(result))
  })


  }
