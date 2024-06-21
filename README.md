
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CohortConstructor <img src="man/figures/logo.png" align="right" height="180"/>

# phenotypeR

<!-- badges: start -->
<!-- badges: end -->

The goal of phenotypeR is to …

## Installation

You can install the development version of phenotypeR like so:

…..

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CDMConnector)
library(phenotypeR)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.3

con <- DBI::dbConnect(duckdb::duckdb(dbdir = CDMConnector::eunomia_dir()))
cdm <- CDMConnector::cdm_from_con(con = con,
                      cdm_schema = "main",
                      write_schema = "main")

cdm$gibleed <- CohortConstructor::conceptCohort(cdm = cdm,
                                   conceptSet = list(gibleed = 192671L),
                                   name = "gibleed")

result <- cdm$gibleed |>
   phenotypeCohort()

result |> 
  glimpse()
#> Rows: 180
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,…
#> $ cdm_name         <chr> "Synthea synthetic health database", "Synthea synthet…
#> $ group_name       <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ group_level      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "snapshot_date", "person_count", "observation_period_…
#> $ variable_level   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ estimate_name    <chr> "value", "count", "count", "source_name", "version", …
#> $ estimate_type    <chr> "date", "integer", "integer", "character", "character…
#> $ estimate_value   <chr> "2024-06-21", "2694", "5343", "Synthea synthetic heal…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```
