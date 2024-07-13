test_that("postgres test", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_POSTGRESQL_DBNAME") == "")

  db <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
                       host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
                       user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                       password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
    write_schema = c(schema =  Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
                     prefix = "incp_"),
    achilles_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  )

  cdm$asthma <- CohortConstructor::conceptCohort(cdm = cdm,
                                                  conceptSet = list("asthma" = 317009),
                                                  name = "asthma")
  diclofenac_codes <- CodelistGenerator::getDrugIngredientCodes(cdm,
                                       name = c("diclofenac"))
  cdm$diclofenac <- CohortConstructor::conceptCohort(cdm = cdm,
                                                conceptSet = diclofenac_codes,
                                                name = "diclofenac")
  cdm <- omopgenerics::bind(cdm$asthma, cdm$diclofenac, name = "my_cohort")

  result_code_diag <- codelistDiagnostics(cdm$my_cohort)
  expect_true("cohort_code_use" %in%
              omopgenerics::settings(result_code_diag)$result_type)

  result_cohort_diag <- cohortDiagnostics(cdm$my_cohort)
  expect_no_error(reportDiagnostics(result = result_code_diag))
  expect_no_error(shinyDiagnostics(result = result_cohort_diag))

  CDMConnector::cdm_disconnect(cdm = cdm)

})
