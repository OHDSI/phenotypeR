
#' Phenotype a cohort
#'
#' @param cohort Cohort
#' @param nSample The number of people to take a random sample for matching to
#' the database population. If NULL, no sampling will be performed and the
#' entire cohorts will be used.
#'
#' @return A summarised result
#' @export
#'
#' @examples
phenotype <- function(cohort,
                      nSample = 1000){

  cdm <- omopgenerics::cdmReference(cohort)

  results <- list()

  results[["db_diag"]] <- databaseDiagnostics(cdm)
  results[["code_diag"]] <- codelistDiagnostics(cohort)
  results[["cohort_diag"]] <- cohortDiagnostics(cohort)
  results[["cohort_to_pop_diag"]] <- cohortToPopulationDiagnostics(cohort,
                                                                   nSample = nSample)

  results <- results |>
    vctrs::list_drop_empty() |>
    omopgenerics::bind()

  results

}
