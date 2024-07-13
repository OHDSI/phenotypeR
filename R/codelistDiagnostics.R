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
  cl <- length(cohortIds) - 1
  if(cl == 0){
    cl <- NULL
  }
  # get all cohort codelists
  all_codelists <- omopgenerics::cohortCodelist(cdm[[cohortTable]],
                                                cohortIds[[1]])
  for(i in seq_along(cl)){
    all_codelists <- purrr::flatten(list(
      all_codelists,
      omopgenerics::cohortCodelist(cdm[[cohortTable]], cohortIds[[i+1]])
    )) |>
      omopgenerics::newCodelist()
  }

  cli::cli_bullets(c("*" = "Getting code counts in database based on achilles"))
  results[[paste0("achilles_code_use")]] <- CodelistGenerator::summariseAchillesCodeUse(x = all_codelists, cdm = cdm)

  cli::cli_bullets(c("*" = "Getting index event breakdown"))
  for(i in seq_along(cohortIds)){
    results[[paste0("index_event_", i)]] <- CodelistGenerator::summariseCohortCodeUse(
      x = omopgenerics::cohortCodelist(cdm[[cohortTable]], cohortIds[[i]]),
      cdm = cdm,
      cohortTable = cohortTable,
      cohortId = cohortIds[[i]],
      timing = "entry",
      countBy = c("record", "person"),
      byConcept = TRUE
    )
  }

  cli::cli_bullets(c("*" = "Getting orphan concepts"))
  results[[paste0("orphan_codes", i)]] <- CodelistGenerator::summariseOrphanCodes(
      x = all_codelists,
      cdm = cdm
    )

  results <- vctrs::list_drop_empty(results)

  results <- omopgenerics::bind(results)

  results
}
