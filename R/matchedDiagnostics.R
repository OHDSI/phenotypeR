#' Compare characteristics of cohort matched to database population
#'
#' @inheritParams cohortDoc
#' @inheritParams matchedSampleDoc
#'
#' @return A summarised result
#' @export
#'
#' @examples
matchedDiagnostics <- function(cohort,
                               matchedSample = 1000){

  omopgenerics::assertNumeric(matchedSample, min = 1)

  cdm <- omopgenerics::cdmReference(cohort)
  cohortName <- omopgenerics::tableName(cohort)
  cohortIds <- omopgenerics::settings(cohort) |>
    dplyr::select("cohort_definition_id") |>
    dplyr::pull()

  results <- list()
  matchedCohortTable <- paste0(omopgenerics::tableName(cdm[[cohortName]]),
                               "_matched")

  if(!is.null(matchedSample)){
  cli::cli_bullets(c("*" = "{.strong Taking {matchedSample} person sample of cohorts}"))
  cdm[[matchedCohortTable]] <- CohortConstructor::sampleCohorts(cdm[[cohortName]],
                                   n = matchedSample,
                                   name = matchedCohortTable)
  } else {
    cdm[[matchedCohortTable]] <- cdm[[cohortName]]
  }

  cli::cli_bullets(c("*" = "{.strong Generating a age and sex matched cohorts}"))
  cdm[[matchedCohortTable]] <- CohortConstructor::matchCohorts(cdm[[matchedCohortTable]],
                                     name = matchedCohortTable)

  results[["cohort_summary"]] <- cdm[[matchedCohortTable]] |>
    CohortCharacteristics::summariseCharacteristics(
      tableIntersectCount = list(
        "Number visits prior year" = list(
          tableName = "visit_occurrence",
          window = c(-365, -1)
        )
      )
    )


  cli::cli_bullets(c("*" = "{.strong Running large scale characterisation}"))
  results[["lsc"]] <- CohortCharacteristics::summariseLargeScaleCharacteristics(
    cohort = cdm[[matchedCohortTable]],
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

  results <- results |>
    vctrs::list_drop_empty() |>
    omopgenerics::bind() |>
    omopgenerics::newSummarisedResult()

  results
}
