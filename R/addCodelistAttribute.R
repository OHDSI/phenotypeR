#' Adds the cohort_codelist attribute to a cohort
#'
#' @param cohort Cohort table in a cdm reference
#' @param codelist Named list of concepts
#' @param cohortName For each element of the codelist, the name of the cohort in
#' `cohort` to which the codelist refers
#'
#' @export
#'
#' @examples
#'

addCodelistAttribute <- function(cohort,
                                 codelist,
                                 cohortName = names(codelist)) {
  # checks
  cohort <- omopgenerics::validateCohortArgument(cohort = cohort)
  omopgenerics::assertList(codelist, named = TRUE)
  codelist <- omopgenerics::validateConceptSetArgument(codelist)
  omopgenerics::assertCharacter(cohortName)
  set <- omopgenerics::settings(cohort)
  int <- intersect(set$cohort_name, cohortName)
  if (length(int) == 0) {
    cli::cli_abort("`cohortName` elements and cohort names in `cohort` don't match.")
  }
  if (length(cohortName) != length(codelist)) {
    cli::cli_abort("`cohortName` and `codelist` must have the same length.")
  }
  if (!is.null(attr(cohort, "cohort_codelist"))) {
    cli::cli_warn("`cohort_codelist` will be overwritten.")
  }

  cohortCodelist <- dplyr::tibble("cohort_name" = cohortName, codelist_name = names(codelist)) |>
    dplyr::inner_join(
      lapply(codelist, dplyr::as_tibble) |> dplyr::bind_rows(.id = "codelist_name"),
      by = "codelist_name"
    ) |>
    dplyr::inner_join(set, by = "cohort_name") |>
    dplyr::mutate("type" = "index event", "value" = as.integer(.data$value)) |>
    dplyr::select(
      "cohort_definition_id","codelist_name", "concept_id" = "value", "type"
    )

  cohort <- cohort |>
    omopgenerics::newCohortTable(cohortCodelistRef = cohortCodelist)

  return(cohort)
}
