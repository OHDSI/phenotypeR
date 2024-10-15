
#' Phenotype a cohort
#'
#' @inheritParams cohortDoc
#' @param databaseDiagnostics If TRUE, database diagnostics will be run.
#' @param codelistDiagnostics If TRUE, codelist diagnostics will be run.
#' @param cohortDiagnostics If TRUE, cohort diagnostics will be run.
#' @param populationDiagnostics If TRUE, population diagnostics will be run.
#' @inheritParams populationSampleDoc
#' @param matchedDiagnostics If TRUE, cohort to population
#' diagnostics will be run.
#' @inheritParams matchedSampleDoc
#'
#' @return A summarised result
#' @export
#'
#' @examples
phenotypeDiagnostics <- function(cohort,
                                 databaseDiagnostics = TRUE,
                                 codelistDiagnostics = TRUE,
                                 cohortDiagnostics = TRUE,
                                 populationDiagnostics = TRUE,
                                 populationSample = 1000000,
                                 matchedDiagnostics = TRUE,
                                 matchedSample = 1000) {

  cdm <- omopgenerics::cdmReference(cohort)

  results <- list()
  if (isTRUE(databaseDiagnostics)) {
    cli::cli("Running database diagnostics")
    results[["db_diag"]] <- databaseDiagnostics(cdm)
  }
  if (isTRUE(codelistDiagnostics)) {
    cli::cli("Running codelist diagnostics")
    results[["code_diag"]] <- codelistDiagnostics(cohort)
  }
  if (isTRUE(cohortDiagnostics)) {
    cli::cli("Running cohort diagnostics")
    results[["cohort_diag"]] <- cohortDiagnostics(cohort)
  }
  if (isTRUE(populationDiagnostics)) {
    cli::cli("Running population diagnostics")
    results[["pop_diag"]] <- populationDiagnostics(cohort,
                                                   populationSample = populationSample)
  }
  if (isTRUE(matchedDiagnostics)) {
    cli::cli("Running matched diagnostics")
    results[["matched_diag"]] <- matchedDiagnostics(cohort,
                                                    matchedSample  = matchedSample
    )
  }

  cli::cli("Combining results")
  results <- results |>
    vctrs::list_drop_empty() |>
    omopgenerics::bind()

  if (is.null(results)) {
    results <- omopgenerics::emptySummarisedResult()
  }

  results
}
