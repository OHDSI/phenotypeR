
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phenotypeR <img src="man/figures/logo.png" align="right" height="180"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/oxford-pharmacoepi/phenotypeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/oxford-pharmacoepi/phenotypeR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The phenotypeR package takes a cohort table in a cdm reference and runs
a series of analyses, including summaries of:  
- code counts in the cdm,  
- counts of codes that could have led to cohort inclusion,  
- cohort overlap and timing (when there is more than one cohort),  
- trends in incidence and prevalence,  
- the demographics of the individuals in a cohort, and  
- the cohort compared to indivduals matched on age and sex.

Once you have obtained the results, you can view them in  
- a shiny application, and/ or  
- a parameterised report

## Installation

You can install the development version of phenotypeR like so:

…..

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CDMConnector)
library(phenotypeR)
library(CohortConstructor)
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.3

con <- DBI::dbConnect(duckdb::duckdb(dbdir = CDMConnector::eunomia_dir()))
cdm <- CDMConnector::cdm_from_con(con = con,
                      cdm_schema = "main",
                      write_schema = "main")

cdm$gibleed <- conceptCohort(cdm = cdm,
                                   conceptSet = list(gibleed = 192671L),
                                   name = "gibleed")

result <- cdm$gibleed |>
   phenotypeCohort()

result
#> # A tibble: 180 × 13
#>    result_id cdm_name            group_name group_level strata_name strata_level
#>        <int> <chr>               <chr>      <chr>       <chr>       <chr>       
#>  1         1 Synthea synthetic … overall    overall     overall     overall     
#>  2         1 Synthea synthetic … overall    overall     overall     overall     
#>  3         1 Synthea synthetic … overall    overall     overall     overall     
#>  4         1 Synthea synthetic … overall    overall     overall     overall     
#>  5         1 Synthea synthetic … overall    overall     overall     overall     
#>  6         1 Synthea synthetic … overall    overall     overall     overall     
#>  7         1 Synthea synthetic … overall    overall     overall     overall     
#>  8         1 Synthea synthetic … overall    overall     overall     overall     
#>  9         1 Synthea synthetic … overall    overall     overall     overall     
#> 10         1 Synthea synthetic … overall    overall     overall     overall     
#> # ℹ 170 more rows
#> # ℹ 7 more variables: variable_name <chr>, variable_level <chr>,
#> #   estimate_name <chr>, estimate_type <chr>, estimate_value <chr>,
#> #   additional_name <chr>, additional_level <chr>
```
