#' Phenotype a set of cohorts
#'
#' @param cohort Cohort table
#'
#' @return A summarised result
#' @export
#'
#' @examples
phenotypeCohort <- function(cohort){

  cdm <- omopgenerics::cdmReference(cohort)
  cohortName <- omopgenerics::tableName(cohort)

  results <- list()

  age_groups = lapply(as.list(1:length(seq(0, 110, 5))),
                      function(k, x1 = seq(0, 110, 5), x2 = seq(4, 120, 5)) {
                        c(x1[k], x2[k])})

  cli::cli_bullets(c("*" = "Getting cdm summary"))
  results[["cdm_summary"]] <- summary(cdm)

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


  # cli::cli_bullets(c("*" = "{.strong Generating the match sample}"))
  # cdm$sample <- cdm[[cohort_table]]  |>
  #   slice_sample(n = 1000, by = cohort_definition_id) |>
  #   compute()

  results <- omopgenerics::bind(results)

  results
}
