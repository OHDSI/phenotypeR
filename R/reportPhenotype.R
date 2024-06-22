
#' Create a report summarising your phenotyping results
#'
#' @param result A summarised result
#'
#' @return A report
#' @export
#'
#' @examples
reportPhenotype <- function(result,
                            dir = here::here()){

    input <- system.file("rmd", "phenotype_report.Rmd",
                         package = "phenotypeR")

    cohortNames <- result |>
      dplyr::filter(group_level != "overall") |>
      dplyr::select("group_level") |>
      dplyr::distinct() |>
      dplyr::pull()
    cohortNames <- paste0(cohortNames, collapse = "; ")
    workingTitle <- paste('phenotypeR results for cohort', cohortNames)

    rmarkdown::render(input = input,
                      params = list(title = workingTitle,
                                    result = result),
                      output_file = "report.html",
                      output_dir = dir,
                      clean = TRUE)
}
