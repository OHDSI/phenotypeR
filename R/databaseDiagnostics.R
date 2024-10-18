#' Database diagnostics
#'
#' @param cdm CDM reference
#'
#' @return A summarised result
#' @export
#'
#' @examples
#' \donttest{
#'   cdm_local <- omock::mockCdmReference() |>
#'     omock::mockPerson(nPerson = 100) |>
#'     omock::mockObservationPeriod() |>
#'     omock::mockConditionOccurrence() |>
#'     omock::mockDrugExposure() |>
#'     omock::mockObservation() |>
#'     omock::mockMeasurement() |>
#'     omock::mockCohort(name = "my_cohort", numberCohorts = 2)
#'  db <- DBI::dbConnect(duckdb::duckdb())
#'  cdm <- CDMConnector::copyCdmTo(con = db,
#'                                 cdm = cdm_local,
#'                                 schema ="main",
#'                                 overwrite = TRUE)
#'  db_diag <- databaseDiagnostics(cdm)
#'  CDMConnector::cdmDisconnect(cdm = cdm)
#' }
databaseDiagnostics <- function(cdm){

results <- list()
results[["snap"]] <- OmopSketch::summariseOmopSnapshot(cdm)
results[["obs_period"]] <- OmopSketch::summariseObservationPeriod(cdm$observation_period)
results <- results |>
  vctrs::list_drop_empty() |>
  omopgenerics::bind() |>
  omopgenerics::newSummarisedResult()

results

}
