server <- function(input, output, session) {

  # achilles results
  working_achilles_data <- reactive({
      result |>
      filter(cdm_name %in% input$achilles_cdm_name,
             group_level %in% input$achilles_codelist_name)
  })
  output$gt_achilles_code_count <- render_gt({
    validate(
      need(nrow(working_achilles_data()) >0, "No result found")
    )
      CodelistGenerator::tableAchillesCodeUse(working_achilles_data(),
                                              type = "gt")
    })
  output$gt_achilles_code_flextable <- renderUI({
    validate(
      need(nrow(working_achilles_data()) >0, "No result found")
    )
    CodelistGenerator::tableAchillesCodeUse(working_achilles_data(),
                                            type = "flextable") %>%
      autofit() %>%
      htmltools_value()
  })
  output$gt_achilles_code_tibble <- renderTable({
    validate(
      need(nrow(working_achilles_data()) >0, "No result found")
    )
    CodelistGenerator::tableAchillesCodeUse(working_achilles_data(),
                                            type = "tibble")
  })
  # output type depends on input
  output$table_achilles_code_count <- renderUI({
      if (input$achilles_table_type == "gt") {
        tableOutput("gt_achilles_code_count")
      } else if (input$achilles_table_type == "flextable"){
        uiOutput("gt_achilles_code_flextable")
      } else {
        gt_output("gt_achilles_code_tibble")
      }
    })

  output$gt_index_events <- render_gt({
    validate(
      need(nrow(result) >0, "No result found")
    )
    CodelistGenerator::tableCohortCodeUse(result)
  })

  output$gt_orphan_codes <- render_gt({
    validate(
      need(nrow(result) >0, "No result found")
    )
    CodelistGenerator::tableOrphanCodes(result)
  })

  output$gt_cohort_counts <- render_gt({
    validate(
      need(nrow(result) >0, "No result found")
    )
    CohortCharacteristics::tableCohortCount(result)
  })


  working_attrition_data <- reactive({
    if(nrow(result) > 0){
      result |>
        visOmopResults::filterSettings(result_type == "cohort_attrition")  |>
        visOmopResults::filterSettings(cohort_name  == input$attrition_cohort_name)
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
