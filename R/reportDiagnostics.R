
#' Create a report summarising your phenotyping results
#'
#' @param result A summarised result
#' @param directory Directory where to save report
#'
#' @return A report
#' @export
#'
#' @examples
reportDiagnostics <- function(result,
                              directory = here::here()){

    input <- system.file("rmd", "phenotype_report.Rmd",
                         package = "phenotypeR")

    cohortNames <- result |>
      visOmopResults::addSettings() |>
      omopgenerics::filter(.data$result_type == "summarised_characteristics") |>
      dplyr::filter(.data$group_level != "overall") |>
      dplyr::select("group_level") |>
      dplyr::distinct() |>
      dplyr::pull()
    cohortNames <- paste0(cohortNames, collapse = "; ")
    workingTitle <- paste('phenotypeR results for cohort', cohortNames)

    rmarkdown::render(input = input,
                      params = list(title = workingTitle,
                                    result = result),
                      output_file = "report.html",
                      output_dir = directory,
                      clean = TRUE)
}
