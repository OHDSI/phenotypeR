
#' Run codelist-level diagnostics
#'
#' @param cohort Cohort table
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


  results <- list()

  age_groups = lapply(as.list(1:length(seq(0, 110, 5))),
                      function(k, x1 = seq(0, 110, 5), x2 = seq(4, 120, 5)) {
                        c(x1[k], x2[k])})

  cli::cli_bullets(c("*" = "Getting code counts in database"))

  cli::cli_bullets(c("*" = "Getting index event breakdown"))
  for(i in seq_along(cohortIds)){
    results[[paste0("index_event_", i)]] <- CodelistGenerator::summariseCohortCodeUse(
      x = omopgenerics::cohortCodelist(cdm[[cohortName]], cohortIds[[i]]),
      cdm = cdm,
      cohortTable = cohortName,
      cohortId = cohortIds[[i]],
      timing = "entry",
      countBy = c("record", "person"),
      byConcept = TRUE,
      byYear = FALSE,
      bySex = FALSE,
      ageGroup = NULL
    )
  }

  cli::cli_bullets(c("*" = "Getting orphan concepts"))

  results <- vctrs::list_drop_empty(results)
  results <- omopgenerics::bind(results)

  results
}
