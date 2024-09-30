
#' Create a shiny app summarising your phenotyping results
#'
#' @param result A summarised result
#' @param directory Directory where to save report
#'
#' @return A shiny app
#' @export
#'
#' @examples
shinyDiagnostics <- function(result,
                             directory = here::here()){

  rlang::check_installed("omopViewer")

  result |>
    omopViewer::exportStaticApp(
      directory = directory,
      background = getBackground(result)
    )
}

getBackground <- function(result) {

  cohorts <- result |>
    visOmopResults::filterSettings(.data$table_name == "my_cohort") |>
    dplyr::distinct(.data$group_name, .data$group_level) |>
    visOmopResults::splitGroup()

  if ("cohort_name" %in% colnames(cohorts)) {
    cohorts <- c(
      "title" = "**Cohorts**",
      "body" = "The diagnostic results cover the following cohorts: {paste0(unique(cohorts$cohort_name), collapse = ', ')}" |> glue::glue()
    )
  } else {
    cohorts <- character()
  }

  codelists <- result |>
    visOmopResults::filterSettings(.data$result_type == "cohort_code_use") |>
    dplyr::distinct(.data$group_name, .data$group_level) |>
    visOmopResults::splitGroup()

  if ("codelist_name" %in% colnames(codelists)) {
    codelists <- c(
      "title" = "**Codelists**",
      "body" = "Diagnostics have been generated for these codelists: {paste0(unique(codelists$codelist_name), collapse = ', ')}" |> glue::glue()
    )
  } else {
    codelists <- character()
  }

  databases <- result |>
    dplyr::filter(!is.na(.data$cdm_name)) |>
    dplyr::pull("cdm_name") |>
    unique()

  if (length(databases) > 0) {
    databases <- c(
      "title" = "**Databases**",
      "body" = "The results are based on data from the following databases: {paste0(databases, collapse = ', ')}" |> glue::glue()
    )
  } else {
    databases <- character()
  }

  resTypes <- settings(result) |>
    dplyr::filter(!is.na(.data$result_type)) |>
    dplyr::pull("result_type") |>
    unique()

  if (length(resTypes) > 0) {
    resTypes <- c(
      "title" = "**Results**",
      "body" = "The following results are available: {paste0(resTypes, collapse = ', ')}" |> glue::glue()
    )
  } else {
    resTypes <- character()
  }

  c(
    "header" = "phenotypeR Diagnostics",
    cohorts,
    codelists,
    databases,
    resTypes,
    "footer" = "This Shiny App presents results generated using the [phenotypeR](https://ohdsi.github.io/phenotypeR/) package (version {as.character(utils::packageVersion('phenotypeR'))})." |> glue::glue()
  )
}
