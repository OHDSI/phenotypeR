
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

  file.copy(from = system.file("shiny",
                            package = "phenotypeR"),
            to = directory,
            recursive = TRUE,
            overwrite = TRUE)

  omopgenerics::exportSummarisedResult(result,
                                       fileName = "result.csv",
                                       path = here::here(directory, "shiny", "data"))

  shiny::shinyAppDir(here::here(directory, "shiny"))

}
