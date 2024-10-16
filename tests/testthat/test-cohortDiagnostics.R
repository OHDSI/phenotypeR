test_that("run with a single cohort", {
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

  expect_no_error(result <- cdm$my_cohort |>
    cohortDiagnostics())

  # check density is being calculated
  expect_true(any(stringr::str_detect(
    omopgenerics::settings(result) |>
      dplyr::pull("result_type"),
    "table")))

  # cohort and timing and overlap should have been skipped
  expect_false(any("summarise_cohort_overlap" ==
   omopgenerics::settings(result) |>
    dplyr::pull("result_type")))

})

test_that("run with multiple cohorts", {
  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = 100) |>
    omock::mockObservationPeriod() |>
    omock::mockConditionOccurrence() |>
    omock::mockDrugExposure() |>
    omock::mockObservation() |>
    omock::mockMeasurement() |>
    omock::mockCohort(name = "my_cohort", numberCohorts = 2)
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
  expect_no_error(result <- cdm$my_cohort |>
                    cohortDiagnostics())

  # check density is being calculated
  expect_true(any(stringr::str_detect(
    omopgenerics::settings(result) |>
      dplyr::pull("result_type"),
    "table")))

  # cohort and timing and overlap should have been estimated now we have more than one cohort
  expect_true(any(stringr::str_detect(
    omopgenerics::settings(result) |>
      dplyr::pull("result_type"),
    "cohort_overlap")))
  expect_true(any(stringr::str_detect(
                   omopgenerics::settings(result) |>
                    dplyr::pull("result_type"),
                   "cohort_timing")))
})

test_that("check all expected analyses are present in results", {

})

test_that("check input validation", {

})

test_that("check edge cases", {
  # check behaviour if cohort table has no records

  # check behaviour if one cohort has no records but others do
})

test_that("check table and plotting functionality work", {
  # check the functions do not throw errors
  # (these tests don't check whether the plots look nice)

})
