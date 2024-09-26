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


 # cli::cli_bullets(c("*" = "{.strong Creating denominator for incidence and prevalence}"))
 # denominatorTable <- omopgenerics::uniqueTableName()
 # cdm <- IncidencePrevalence::generateDenominatorCohortSet(
 #   cdm = cdm,
 #   name = denominatorTable,
 #   ageGroup = list(c(0,17),
 #                   c(18,64),
 #                   c(65,199)),
 #   sex = c("Male", "Female", "Both"),
 #   daysPriorObservation = c(0, 180)
 # )
 #
 # cli::cli_bullets(c("*" = "{.strong Estimating incidence}"))
 # results[["incidence"]] <- IncidencePrevalence::estimateIncidence(
 #   cdm = cdm,
 #   denominatorTable = denominatorTable,
 #   outcomeTable = cohortName,
 #   interval = "years",
 #   repeatedEvents = c(TRUE, FALSE),
 #   outcomeWashout = c(0, Inf),
 #   completeDatabaseIntervals = c(TRUE, FALSE),
 #   minCellCount = 0)
 #
 # cli::cli_bullets(c("*" = "{.strong Estimating prevalence}"))
 # results[["prevalence"]] <- IncidencePrevalence::estimatePeriodPrevalence(
 #   cdm = cdm,
 #   denominatorTable = denominatorTable,
 #   outcomeTable = cohortName,
 #   interval = "years",
 #   completeDatabaseIntervals = c(TRUE, FALSE),
 #   fullContribution = c(TRUE, FALSE),
 #   minCellCount = 0)

  cli::cli_bullets(c("*" = "{.strong Generating a age and sex matched cohorts}"))
  matchedCohortTable <- paste0(omopgenerics::tableName(cdm[[cohortName]]),
                               "_matched")
  cdm[[matchedCohortTable]] <- CohortConstructor::matchCohorts(cdm[[cohortName]],
                                                               name = matchedCohortTable)

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

  results <- results |>
    vctrs::list_drop_empty() |>
    omopgenerics::bind() |>
    omopgenerics::newSummarisedResult()

  results
}
