
#' Run cohort-level diagnostics
#'
#' @param cohort Cohort table
#'
#' @return A summarised result
#' @export
#'
#' @examples
cohortDiagnostics <- function(cohort){

  cdm <- omopgenerics::cdmReference(cohort)
  cohortName <- omopgenerics::tableName(cohort)
  cohortIds <- omopgenerics::settings(cohort) |>
    dplyr::select("cohort_definition_id") |>
    dplyr::pull()

  results <- list()

  age_groups = lapply(as.list(1:length(seq(0, 110, 5))),
                      function(k, x1 = seq(0, 110, 5), x2 = seq(4, 120, 5)) {
                        c(x1[k], x2[k])})

  cli::cli_bullets(c("*" = "Getting cohort counts"))
  results[["cohort_counts"]] <- cdm[[cohortName]] |>
    CohortCharacteristics::summariseCohortCount()

  cli::cli_bullets(c("*" = "Getting cohort attrition"))
  results[["cohort_counts"]] <- cdm[[cohortName]] |>
    CohortCharacteristics::summariseCohortAttrition()

  cli::cli_bullets(c("*" = "Getting cohort summary"))
  results[["cohort_summary"]] <- cdm[[cohortName]] %>%
    dplyr::mutate(days_in_cohort = as.integer(!!CDMConnector::datediff(
      start = "cohort_start_date", end = "cohort_end_date", interval = "day"
    ))) |>
    PatientProfiles::addDemographics() |>
    CohortCharacteristics::summariseCharacteristics(
      strata = c("sex"),
      ageGroup = age_groups,
      # tableIntersectCount = list(
      #   "Number visits prior year" = list(
      #     tableName = "visit_occurrence",
      #     window = c(-365, -1)
      #   )
      # ),
      otherVariables = "days_in_cohort",
      otherVariablesEstimates = c("min", "q25", "median", "q75", "max")
    )

  if(length(omopgenerics::settings(cdm[[cohortName]]) |>
            dplyr::pull("cohort_definition_id")) > 1){
    cli::cli_bullets(c("*" = "Getting cohort overlap"))
    results[["cohort_overlap"]] <- CohortCharacteristics::summariseCohortOverlap(
      cdm[[cohortName]])

    cli::cli_bullets(c("*" = "Getting cohort timing"))
    results[["cohort_timing"]] <- CohortCharacteristics::summariseCohortTiming(cdm[[cohortName]],
                                                                               density = TRUE)
  } else {
    cli::cli_bullets(c("*" = "Only one cohort in settings - skipping cohort overlap and timing"))
  }


  cli::cli_bullets(c("*" = "{.strong Generating a age and sex matched cohorts}"))
  matchedCohortTable <- paste0(omopgenerics::tableName(cdm[[cohortName]]),
                               "_matched")
  cdm[[matchedCohortTable]] <- CohortConstructor::matchCohorts(cdm[[cohortName]],
                                                               name = matchedCohortTable)


  cli::cli_bullets(c("*" = "{.strong Running large scale characterisation}"))


  results[["lsc"]] <- CohortCharacteristics::summariseLargeScaleCharacteristics(
    cohort = cdm[[matchedCohortTable]],
    window = list(c(-Inf, -366), c(-365, -31),
                  c(-30, -1), c(0, 0),
                  c(1, 30), c(31, 365),
                  c(366, Inf)),
    eventInWindow = c("condition_occurrence"),
    # eventInWindow = c("condition_occurrence", "visit_occurrence",
    #                   "measurement", "procedure_occurrence",
    #                   "observation"),
    episodeInWindow = c("drug_exposure"),
    minimumFrequency = 0.0005
  )

  results <- vctrs::list_drop_empty(results)
  results <- omopgenerics::bind(results)

  results
}
