# Libraries ----
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
if(file.exists(here::here("shiny", "data", "result.csv"))){
  result <- read_csv(here::here("shiny", "data", "result.csv"),
                     col_types = c(.default = "c"))
} else {
  cli::cli_warn("No results file found")
  result <- omopgenerics::emptySummarisedResult()
}
result <- omopgenerics::newSummarisedResult(result)


cohort_names <- sort(unique(result |>
              visOmopResults::filterSettings(result_type == "cohort_attrition") |>
              dplyr::pull("group_level")))
