
#' Run codelist-level diagnostics
#'
#' @param cohort A cohort table in a cdm reference. The cohort_codelist
#' attribute must be populated.
#'
#' @return A summarised result
#' @export
#'
#' @examples
codelistDiagnostics <- function(cohort){

  cdm <- omopgenerics::cdmReference(cohort)
  cohortName <- omopgenerics::tableName(cohort)
  cohortIds <- omopgenerics::settings(cohort) |>
    dplyr::select("cohort_definition_id") |>
    dplyr::pull()

  if(is.null(attr(cdm, "cohort_codelist"))){
    cli::cli_warn(message =
                    c("cohort_codelist attribute for cohort not found",
                      "i" = "Returning an empty result"))
    return(omopgenerics::emptySummarisedResult())
  }

  results <- list()
  results[[1]] <- omopgenerics::emptySummarisedResult()

  cli::cli_bullets(c("*" = "Getting code counts in database"))

  cl <- length(cohortIds) - 1
  all_codelists <- omopgenerics::cohortCodelist(cdm[[cohortName]],
                                                cohortIds[[1]])
  for(i in seq_along(cl)){
    all_codelists <- purrr::flatten(list(
      all_codelists,
      omopgenerics::cohortCodelist(cdm[[cohortName]], cohortIds[[i+1]])
    )) |>
      omopgenerics::newCodelist()
  }
  results[[paste0("code_use")]] <- CodelistGenerator::summariseCodeUse(
    all_codelists, cdm = cdm)

  omopgenerics::cohortCodelist(cdm[[cohortName]], cohortIds[[i]])

  cli::cli_bullets(c("*" = "Getting index event breakdown"))
  for(i in seq_along(cohortIds)){
    results[[paste0("index_event_", i)]] <- CodelistGenerator::summariseCohortCodeUse(
      x = omopgenerics::cohortCodelist(cdm[[cohortName]], cohortIds[[i]]),
      cdm = cdm,
      cohortTable = cohortName,
      cohortId = cohortIds[[i]],
      timing = "entry",
      countBy = c("record", "person"),
      byConcept = TRUE
    )
  }

  cli::cli_bullets(c("*" = "Getting orphan concepts"))
  for(i in seq_along(cohortIds)){
    results[[paste0("index_event_", i)]] <- CodelistGenerator::summariseOrphanCodes(
      x = all_codelists,
      cdm = cdm,
      domains = CodelistGenerator::getDomains(cdm)
    )
  }

  results <- results |>
    vctrs::list_drop_empty() |>
    omopgenerics::bind(results)

  results
}
