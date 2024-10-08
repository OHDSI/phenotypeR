#' Run cohort-level diagnostics
#'
#' @param cohort Cohort table in a cdm reference
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


  results <- results |>
    vctrs::list_drop_empty() |>
    omopgenerics::bind() |>
    omopgenerics::newSummarisedResult()

  results
}
