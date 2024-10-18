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
#' \donttest{
#'   cdm_local <- omock::mockCdmReference() |>
#'    omock::mockPerson(nPerson = 100) |>
#'    omock::mockObservationPeriod() |>
#'    omock::mockConditionOccurrence() |>
#'    omock::mockDrugExposure() |>
#'    omock::mockObservation() |>
#'    omock::mockMeasurement() |>
#'    omock::mockCohort(name = "my_cohort")
#'    cdm_local$visit_occurrence <- dplyr::tibble(
#'     person_id = 1L,
#'     visit_occurrence_id = 1L,
#'     visit_concept_id = 1L,
#'     visit_start_date = as.Date("2000-01-01"),
#'     visit_end_date = as.Date("2000-01-01"),
#'     visit_type_concept_id = 1L
#'     )
#'     cdm_local$procedure_occurrence <- dplyr::tibble(
#'       person_id = 1L,
#'       procedure_occurrence_id = 1L,
#'       procedure_concept_id = 1L,
#'       procedure_date = as.Date("2000-01-01"),
#'       procedure_type_concept_id = 1L
#'       )
#'  db <- DBI::dbConnect(duckdb::duckdb())
#'  cdm <- CDMConnector::copyCdmTo(con = db,
#'                                 cdm = cdm_local,
#'                                 schema ="main",
#'                                 overwrite = TRUE)
#'
#'  cdm$my_cohort |> cohortDiagnostics()
#'  CDMConnector::cdmDisconnect(cdm = cdm)
#' }
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
