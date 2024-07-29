server <- function(input, output, session) {

  output$gt_achilles_code_count <- render_gt({
      CodelistGenerator::tableAchillesCodeUse(result)
    })

  output$gt_index_events <- render_gt({
    CodelistGenerator::tableCohortCodeUse(result)
  })

  output$gt_orphan_codes <- render_gt({
    CodelistGenerator::tableOrphanCodes(result)
  })

  output$gt_cohort_counts <- render_gt({
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
    CohortCharacteristics::tableCohortAttrition(working_attrition_data())
  })
  output$gg_cohort_attrition <- renderGrViz({
    CohortCharacteristics::plotCohortAttrition(working_attrition_data())
  })

  output$gt_cohort_overlap <- render_gt({
    CohortCharacteristics::tableCohortOverlap(result)
  })
  output$gg_cohort_overlap <- renderPlotly({
   plotly::ggplotly(CohortCharacteristics::plotCohortOverlap(result))
  })

  output$gt_cohort_timing <- render_gt({
    CohortCharacteristics::tableCohortTiming(result)
  })
  output$gg_cohort_timing <- renderPlotly({
    plotly::ggplotly(CohortCharacteristics::plotCohortTiming(result))
  })


  }
