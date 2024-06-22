test_that("basic working example with one cohort", {

  cdm_local <- omock::mockCdmReference() |>
    omock::mockPerson(nPerson = 10) |>
    omock::mockObservationPeriod() |>
    omock::mockCohort(name = "my_cohort")

  db <- DBI::dbConnect(duckdb::duckdb())
  cdm <- CDMConnector::copyCdmTo(con = db, cdm = cdm_local,
                                 schema ="main", overwrite = TRUE)
  my_result <- cdm$my_cohort |> phenotypeCohort()
  reportPhenotype(result = my_result)

})

test_that("basic working example with two cohorts", {

})
