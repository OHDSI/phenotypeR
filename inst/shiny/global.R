# Libraries ----
library(omopgenerics)
library(dplyr)
library(readr)
library(here)
library(gt)
library(DT)
library(flextable)
library(plotly)
library(DiagrammeR)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)
library(stringr)

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


databases <-sort(unique(result$cdm_name))

if(nrow(result) > 0){
  codelist_names <- sort(unique(result |>
                                  visOmopResults::filterSettings(result_type == "achilles_code_use") |>
                                  dplyr::pull("group_level")))
  codelist_domains <- sort(unique(result |>
                                  visOmopResults::filterSettings(result_type == "achilles_code_use") |>
                                  dplyr::pull("strata_level")))

  cohort_names <- sort(unique(result |>
                                visOmopResults::filterSettings(result_type == "cohort_attrition") |>
                                dplyr::pull("group_level")))
} else {
  cohort_names <- NULL
}

