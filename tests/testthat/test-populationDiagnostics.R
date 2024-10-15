test_that("population incidence and prevalence", {
  cdm <- IncidencePrevalence::mockIncidencePrevalenceRef(sampleSize = 1000)
  cdm <- IncidencePrevalence::generateDenominatorCohortSet(cdm, name = "denom")
  expect_no_error(pop_diag <- populationDiagnostics(cohort = cdm$outcome,
                                    populationSample = 250))

  CDMConnector::cdm_disconnect(cdm)

  })
