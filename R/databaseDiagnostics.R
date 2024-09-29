

#' Database diagnostics
#'
#' @param cdm CDM reference
#'
#' @return
#' @noRd
databaseDiagnostics <- function(cdm){

OmopSketch::summariseOmopSnapshot(cdm)

}
