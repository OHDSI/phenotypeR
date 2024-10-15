#' Run cohort-level diagnostics
#'
#' @inheritParams cohortDoc
#' @param strata A list of variables to stratify results. These variables must
#' have been added as additional columns in the cohort table.
#'
#' @return A summarised result
#' @export
#'
#' @examples
cohortDiagnostics <- function(cohort,
                              strata = list()){

  cdm <- omopgenerics::cdmReference(cohort)
  cohortName <- omopgenerics::tableName(cohort)
  cohortIds <- omopgenerics::settings(cohort) |>
    dplyr::select("cohort_definition_id") |>
    dplyr::pull()

  results <- list()

  cli::cli_bullets(c("*" = "Getting cohort summary"))
  results[["cohort_summary"]] <- cdm[[cohortName]] |>
    CohortCharacteristics::summariseCharacteristics(
      strata = strata,
      tableIntersectCount = list(
        "Number visits prior year" = list(
          tableName = "visit_occurrence",
          window = c(-365, -1)
        )
      )
    )

  cli::cli_bullets(c("*" = "Getting cohort attrition"))
  results[["cohort_attrition"]] <- cdm[[cohortName]] |>
    CohortCharacteristics::summariseCohortAttrition()

  if(length(cohortIds) > 1){
    cli::cli_bullets(c("*" = "Getting cohort overlap"))
    results[["cohort_overlap"]] <-  cdm[[cohortName]] |>
      CohortCharacteristics::summariseCohortOverlap(strata = strata)

    cli::cli_bullets(c("*" = "Getting cohort timing"))
    results[["cohort_timing"]] <- cdm[[cohortName]] |>
      CohortCharacteristics::summariseCohortTiming(strata = strata,
                                                   density = TRUE)
    }

  results <- results |>
    vctrs::list_drop_empty() |>
    omopgenerics::bind() |>
    omopgenerics::newSummarisedResult()

  results
}
