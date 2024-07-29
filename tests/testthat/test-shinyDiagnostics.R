test_that("basic working example with one cohort", {

  skip_on_cran()

  # empty result - should still work without error
  expect_no_error(shinyDiagnostics(result = omopgenerics::emptySummarisedResult()))

  # with results
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockConditionOccurrence() |>
    omock::mockDrugExposure() |>
    omock::mockObservation() |>
    omock::mockMeasurement() |>
    omock::mockCohort(name = "my_cohort")
  cdm_local$visit_occurrence <- dplyr::tibble(
    person_id = 1L,
    visit_occurrence_id = 1L,
    visit_concept_id = 1L,
    visit_start_date = as.Date("2000-01-01"),
    visit_end_date = as.Date("2000-01-01"),
    visit_type_concept_id = 1L
  )
  cdm_local$procedure_occurrence <- dplyr::tibble(
    person_id = 1L,
    procedure_occurrence_id = 1L,
    procedure_concept_id = 1L,
    procedure_date = as.Date("2000-01-01"),
    procedure_type_concept_id = 1L
  )

  db <- DBI::dbConnect(duckdb::duckdb())
  cdm <- CDMConnector::copyCdmTo(con = db, cdm = cdm_local,
                                 schema ="main", overwrite = TRUE)

  # # only codelist diagnostic results
  # my_result_code_diag <- cdm$my_cohort |> codelistDiagnostics()
  # shiny_app <- shinyDiagnostics(result = my_result_code_diag)
  # expect_no_error(shiny_app)

  # only chort diagnostic results (mock does not have ac)
  my_result_cohort_diag <- cdm$my_cohort |> cohortDiagnostics()
  shiny_app <- shinyDiagnostics(result = my_result_cohort_diag)
  expect_no_error(shiny_app)

  ## all results
  # my_result <- omopgenerics::bind(my_result_code_diag, my_result_cohort_diag)
  # shiny_app <- shinyDiagnostics(result = my_result)
  # expect_no_error(shiny_app)

})
