
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
    omopViewer::exportStaticApp(directory = directory)

}
