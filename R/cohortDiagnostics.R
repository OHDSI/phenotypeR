#' Run cohort-level diagnostics
#'
#' @inheritParams cohortDoc
#'
#' @return A summarised result
#' @export
#'
cohortDiagnostics <- function(cohort){

  cdm <- omopgenerics::cdmReference(cohort)
  cohortName <- omopgenerics::tableName(cohort)
  cohortIds <- omopgenerics::settings(cohort) |>
    dplyr::select("cohort_definition_id") |>
    dplyr::pull()

  prefix <- omopgenerics::tmpPrefix()
  tempCohortName  <- paste0(prefix, cohortName)
  results <- list()

  cdm[[tempCohortName]]  <- cdm[[cohortName]] |>
    PatientProfiles::addAge(ageGroup = list(c(0, 17), c(18, 64), c(65, 150))) |>
    PatientProfiles::addSex() |>
    CDMConnector::compute(name = tempCohortName, temporary = FALSE)

  cli::cli_bullets(c("*" = "Getting cohort summary"))
  results[["cohort_summary"]] <- cdm[[tempCohortName]] |>
    CohortCharacteristics::summariseCharacteristics(
      strata = list("age_group", "sex"),
      tableIntersectCount = list(
        "Number visits prior year" = list(
          tableName = "visit_occurrence",
          window = c(-365, -1)
        )
      )
    )

  cli::cli_bullets(c("*" = "Getting age density"))
  results[["cohort_density"]] <- cdm[[tempCohortName]] |>
    PatientProfiles::summariseResult(
      strata    = "sex",
      includeOverallStrata = FALSE,
      variables = "age",
      estimates = "density") |>
    suppressMessages()

  omopgenerics::dropTable(cdm, dplyr::starts_with(prefix))

  cli::cli_bullets(c("*" = "Getting cohort attrition"))
  results[["cohort_attrition"]] <- cdm[[cohortName]] |>
    CohortCharacteristics::summariseCohortAttrition()

  if(length(cohortIds) > 1){
    cli::cli_bullets(c("*" = "Getting cohort overlap"))
    results[["cohort_overlap"]] <-  cdm[[cohortName]] |>
      CohortCharacteristics::summariseCohortOverlap()

    cli::cli_bullets(c("*" = "Getting cohort timing"))
    results[["cohort_timing"]] <- cdm[[cohortName]] |>
      CohortCharacteristics::summariseCohortTiming(estimates = "density")
    }

  results <- results |>
    vctrs::list_drop_empty() |>
    omopgenerics::bind() |>
    omopgenerics::newSummarisedResult()

  results
}
