# Libraries ----
library(readr)
library(here)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)

# load data ----
result <- read_csv(here::here("shiny", "data", "result.csv"))
result <- omopgenerics::newSummarisedResult(result)
