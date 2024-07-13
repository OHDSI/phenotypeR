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

  }
