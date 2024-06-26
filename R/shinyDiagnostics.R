
#' Create a shiny app summarising your phenotyping results
#'
#' @param result A summarised result
#'
#' @return A shiny app
#' @export
#'
#' @examples
shinyDiagnostics <- function(result){

  shiny::shinyAppDir(system.file("shiny",
                                  package = "phenotypeR"))

}
