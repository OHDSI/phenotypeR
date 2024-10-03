
#' Phenotype a cohort
#'
#' @param cohort Cohort
#' @param databaseDiagnostics If TRUE, database diagnostics will be run.
#' @param codelistDiagnostics If TRUE, codelist diagnostics will be run.
#' @param cohortDiagnostics If TRUE, cohort diagnostics will be run.
#' @param populationDiagnostics If TRUE, population diagnostics will be run.
#' @param populationSample N of people from the cdm to sample. If NULL no
#' sampling will be performed
#' @param matchedDiagnostics If TRUE, cohort to population
#' diagnostics will be run.
#' @param nSample The number of people to take a random sample for matching to
#' the database population. If NULL, no sampling will be performed and the
#' entire cohorts will be used.
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
                                 nSample = 1000) {

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
      nSample = nSample
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
