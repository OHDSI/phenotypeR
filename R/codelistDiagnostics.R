#' Run codelist-level diagnostics
#'
#' @param cohort A cohort table in a cdm reference. The cohort_codelist
#' attribute must be populated. The cdm reference must contain achilles
#' tables as these will be used for deriving concept counts.
#'
#' @return A summarised result
#' @export
#'
#' @examples
codelistDiagnostics <- function(cohort){

  cdm <- omopgenerics::cdmReference(cohort)
  if(!"achilles_results" %in% names(cdm)){
    cli::cli_warn(
      c("The CDM reference containing the cohort must also contain achilles tables.",
        "Returning an empty summarised result.")
    )
    return(omopgenerics::emptySummarisedResult())
  }
  cohortTable <- omopgenerics::tableName(cohort)
  cohortIds <- omopgenerics::settings(cohort) |>
    dplyr::select("cohort_definition_id") |>
    dplyr::pull()

  if(is.null(attr(cdm[[cohortTable]], "cohort_codelist"))){
    cli::cli_warn(message =
                    c("cohort_codelist attribute for cohort not found",
                      "i" = "Returning an empty result"))
    return(omopgenerics::emptySummarisedResult())
  }

  results <- list()
  results[[1]] <- omopgenerics::emptySummarisedResult()

  cli::cli_bullets(c("*" = "Getting codelists from cohorts"))
  # get all cohort codelists
  all_codelists <- omopgenerics::emptyCodelist()
  for(i in seq_along(cohortIds)){
    all_codelists <- purrr::flatten(list(
      all_codelists,
      omopgenerics::cohortCodelist(cdm[[cohortTable]], cohortIds[[i]])
    )) |>
      omopgenerics::newCodelist()
  }

  cli::cli_bullets(c("*" = "Getting code counts in database based on achilles"))
  results[[paste0("achilles_code_use")]] <- CodelistGenerator::summariseAchillesCodeUse(x = all_codelists, cdm = cdm)

  cli::cli_bullets(c("*" = "Getting orphan concepts"))
  results[[paste0("orphan_codes", i)]] <- CodelistGenerator::summariseOrphanCodes(
      x = all_codelists,
      cdm = cdm
    )

  results <- results |>
    vctrs::list_drop_empty() |>
    omopgenerics::bind()

  results
}
