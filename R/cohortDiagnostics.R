#' Run cohort-level diagnostics
#'
#' @param cohort Cohort table in a cdm reference
#' @param strata A list of variables to stratify results. These variables must
#' have been added as additional columns in the cohort table.
#' @param matchCohort TRUE or FALSE. If TRUE age and sex matched cohorts will
#' be added for each cohort in the cohort table
#' @param nSample The number of people to take a random sample for running
#' large scale characteristics
#'
#' @return A summarised result
#' @export
#'
#' @examples
cohortDiagnostics <- function(cohort,
                              strata = list(),
                              matchCohort = TRUE,
                              nSample = NULL){

  cdm <- omopgenerics::cdmReference(cohort)
  cohortName <- omopgenerics::tableName(cohort)
  cohortIds <- omopgenerics::settings(cohort) |>
    dplyr::select("cohort_definition_id") |>
    dplyr::pull()

  results <- list()

  cli::cli_bullets(c("*" = "Getting cohort counts"))
  results[["cohort_counts"]] <- cdm[[cohortName]] |>
    CohortCharacteristics::summariseCohortCount(strata = strata)
  attr(results[["cohort_counts"]], "settings")$result_id <- 1L

  cli::cli_bullets(c("*" = "Getting cohort attrition"))
  results[["cohort_attrition"]] <- cdm[[cohortName]] |>
    CohortCharacteristics::summariseCohortAttrition()
  attr(results[["cohort_attrition"]], "settings")$result_id <- 2L

  if(length(cohortIds) > 1){
    cli::cli_bullets(c("*" = "Getting cohort overlap"))
    results[["cohort_overlap"]] <-  cdm[[cohortName]] |>
      CohortCharacteristics::summariseCohortOverlap(strata = strata)
    attr(results[["cohort_overlap"]], "settings")$result_id <- 3L

    cli::cli_bullets(c("*" = "Getting cohort timing"))
    results[["cohort_timing"]] <- cdm[[cohortName]] |>
      CohortCharacteristics::summariseCohortTiming(strata = strata,
                                                   density = TRUE)
    attr(results[["cohort_timing"]], "settings")$result_id <- 4L
    }

    cli::cli_bullets(c("*" = "Getting cohort summary"))
    results[["cohort_summary"]] <- cdm[[cohortName]] %>%
      dplyr::mutate(days_in_cohort = as.integer(!!CDMConnector::datediff(
        start = "cohort_start_date", end = "cohort_end_date", interval = "day"
      ))) |>
      PatientProfiles::addDemographics() |>
      CohortCharacteristics::summariseCharacteristics(
        strata = strata,
        tableIntersectCount = list(
          "Number visits prior year" = list(
            tableName = "visit_occurrence",
            window = c(-365, -1)
          )
        ),
        otherVariables = "days_in_cohort",
        otherVariablesEstimates = c("min", "q25", "median", "q75", "max")
      )
    attr(results[["cohort_summary"]], "settings")$result_id <- 5L

  cli::cli_bullets(c("*" = "{.strong Generating a age and sex matched cohorts}"))
  matchedCohortTable <- paste0(omopgenerics::tableName(cdm[[cohortName]]),
                               "_matched")
  cdm[[matchedCohortTable]] <- CohortConstructor::matchCohorts(cdm[[cohortName]],
                                                               name = matchedCohortTable)
  attr(results[["cohort_summary"]], "settings")$result_id <- 6L


  cli::cli_bullets(c("*" = "{.strong Running large scale characterisation}"))
  results[["lsc"]] <- CohortCharacteristics::summariseLargeScaleCharacteristics(
    cohort = cdm[[matchedCohortTable]],
    strata = strata,
    window = list(c(-Inf, -366), c(-365, -31),
                  c(-30, -1), c(0, 0),
                  c(1, 30), c(31, 365),
                  c(366, Inf)),
    eventInWindow = c("condition_occurrence", "visit_occurrence",
                      "measurement", "procedure_occurrence",
                      "observation"),
    episodeInWindow = c("drug_exposure"),
    minimumFrequency = 0.0005
  )
  attr(results[["lsc"]], "settings")$result_id <- attr(results[["lsc"]], "settings")$result_id + 7L

  results <- results |>
    vctrs::list_drop_empty() |>
    omopgenerics::bind() |>
    omopgenerics::newSummarisedResult()

  results
}
