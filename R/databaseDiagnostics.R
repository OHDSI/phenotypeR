

#' Database diagnostics
#'
#' @param cdm CDM reference
#'
#' @return
#' @export
#'
#' @examples
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
