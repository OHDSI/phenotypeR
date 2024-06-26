test_that("basic working example with one cohort", {

  skip_on_cran()

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockConditionOccurrence() |>
    omock::mockDrugExposure() |>
    omock::mockCohort(name = "my_cohort")

  db <- DBI::dbConnect(duckdb::duckdb())
  cdm <- CDMConnector::copyCdmTo(con = db, cdm = cdm_local,
                                 schema ="main", overwrite = TRUE)
  my_result <- cdm$my_cohort |> cohortDiagnostics()

  shiny_app <- shinyDiagnostics(result = my_result)
  app <- shinytest2::AppDriver$new(shiny_app,
                                   name = "diagnostics")
  app$expect_values()


})
