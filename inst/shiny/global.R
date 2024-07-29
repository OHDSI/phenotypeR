# Libraries ----
library(omopgenerics)
library(readr)
library(here)
library(gt)
library(plotly)
library(DiagrammeR)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)

# load data ----
if(dir.exists(here::here("data"))){
  result <- omopgenerics::importSummarisedResult(
    path = here::here("data"))
} else if(dir.exists(here::here("shiny", "data"))) {
  result <- omopgenerics::importSummarisedResult(
    path = here::here("shiny", "data"))
} else {
  cli::cli_warn("No results file found")
  result <- omopgenerics::emptySummarisedResult()
}
result <- omopgenerics::newSummarisedResult(result)

if(nrow(result) > 0){
  cohort_names <- sort(unique(result |>
                                visOmopResults::filterSettings(result_type == "cohort_attrition") |>
                                dplyr::pull("group_level")))
} else {
  cohort_names <- NULL
}

