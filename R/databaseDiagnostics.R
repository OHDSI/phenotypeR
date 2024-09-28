

#' Database diagnostics
#'
#' @param cdm CDM reference
#'
#' @return
#' @export
#'
#' @examples
databaseDiagnostics <- function(cdm){

OmopSketch::summariseOmopSnapshot(cdm)

}
