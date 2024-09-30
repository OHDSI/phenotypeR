
#' Phenotype a cohort
#'
#' @param cohort Cohort
#' @param databaseDiagnostics If TRUE, database diagnostics will be run.
#' @param codelistDiagnostics If TRUE, codelist diagnostics will be run.
#' @param cohortDiagnostics If TRUE, cohort diagnostics will be run.
#' @param cohortToPopulationDiagnostics If TRUE, cohort to population
#' diagnostics will be run.
#' @param nSample The number of people to take a random sample for matching to
#' the database population. If NULL, no sampling will be performed and the
#' entire cohorts will be used.
#'
#' @return A summarised result
#' @export
#'
#' @examples
phenotype <- function(cohort,
                      databaseDiagnostics = TRUE,
                      codelistDiagnostics = TRUE,
                      cohortDiagnostics = TRUE,
                      cohortToPopulationDiagnostics = TRUE,
                      nSample = 1000){

  cdm <- omopgenerics::cdmReference(cohort)

  results <- list()
  if(isTRUE(databaseDiagnostics)){
    results[["db_diag"]] <- databaseDiagnostics(cdm)
  }
  if(isTRUE(codelistDiagnostics)){
  results[["code_diag"]] <- codelistDiagnostics(cohort)
  }
  if(isTRUE(cohortDiagnostics)){
  results[["cohort_diag"]] <- cohortDiagnostics(cohort)
  }
  if(isTRUE(cohortToPopulationDiagnostics)){
  results[["cohort_to_pop_diag"]] <- cohortToPopulationDiagnostics(cohort,
                                                                   nSample = nSample)
  }

  results <- results |>
    vctrs::list_drop_empty() |>
    omopgenerics::bind()

  if(is.null(results)){
    results <- omopgenerics::emptySummarisedResult()
  }

  results

}
