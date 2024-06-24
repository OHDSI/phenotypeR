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
                     prefix = "incp_")
  )

  cdm$gi_bleed <- CohortConstructor::conceptCohort(cdm = cdm,
                                                  conceptSet = list("gi_bleed" = 192671),
                                                  name = "gi_bleed")
  drug_codes <- CodelistGenerator::getDrugIngredientCodes(cdm,
                                       name = c("diclofenac",
                                                "acetaminophen"))
  cdm$drugs <- CohortConstructor::conceptCohort(cdm = cdm,
                                                   conceptSet = drug_codes,
                                                   name = "drugs")
  cdm <- omopgenerics::bind(cdm$gi_bleed, cdm$drugs, name = "my_cohort")

  result_code_diag <- codelistDiagnostics(cdm$my_cohort)
  result_cohort_diag <- cohortDiagnostics(cdm$my_cohort)
  expect_no_error(reportDiagnostics(result = result_code_diag))
  expect_no_error(shinyDiagnostics(result = result_cohort_diag))

  CDMConnector::cdm_disconnect(cdm = cdm)

})
