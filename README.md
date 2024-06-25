
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phenotypeR <img src="man/figures/logo.png" align="right" height="180"/>

<!-- badges: start -->

> **This package is under development and not yet ready for use.**

[![R-CMD-check](https://github.com/oxford-pharmacoepi/phenotypeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/oxford-pharmacoepi/phenotypeR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The phenotypeR package supports the assessment of cohorts

Codelist-level diagnostics help to answer questions like what concepts
from our codelist are used in the database? What concepts were present
led to individuals’ entry in the cohort? Are there any concepts being
used in the database that we didn’t include in our codelist but maybe we
should have?

Cohort-level diagnostics help to answer questions like how many
individuals did we include in our cohort and how many were excluded
because of our inclusion criteria? If we have multiple cohorts, is there
overlap between them and when do people enter one cohort relative to
another? What is the incidence of cohort entry and what is the
prevalence of the cohort in the database? What are the characteristics
of those people in the cohort, and how do they compare to people similar
in terms of age and sex?

## Installation

You can install phenotypeR from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("oxford-pharmacoepi/phenotypeR")
```

## Codelist diagnostics

``` r
library(omopgenerics)
library(CDMConnector)
library(phenotypeR)
library(CohortConstructor)
library(dplyr)

con <- DBI::dbConnect(duckdb::duckdb(dbdir = CDMConnector::eunomia_dir()))
cdm <- CDMConnector::cdm_from_con(con = con,
                      cdm_schema = "main",
                      write_schema = "main")

cdm$gibleed <- conceptCohort(cdm = cdm,
                                   conceptSet = list(gibleed = 192671L),
                                   name = "gibleed")

gibleed_code_diag <- cdm$gibleed |>
   codelistDiagnostics()
gibleed_code_diag |> 
  glimpse()
#> Rows: 4
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1
#> $ cdm_name         <chr> "Synthea synthetic health database", "Synthea synthet…
#> $ group_name       <chr> "cohort_name &&& codelist_name", "cohort_name &&& cod…
#> $ group_level      <chr> "gibleed &&& gibleed", "gibleed &&& gibleed", "giblee…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall"
#> $ strata_level     <chr> "overall", "overall", "overall", "overall"
#> $ variable_name    <chr> "overall", "Gastrointestinal hemorrhage", "overall", …
#> $ variable_level   <chr> NA, "192671", NA, "192671"
#> $ estimate_name    <chr> "record_count", "record_count", "person_count", "pers…
#> $ estimate_type    <chr> "integer", "integer", "integer", "integer"
#> $ estimate_value   <chr> "479", "479", "479", "479"
#> $ additional_name  <chr> "overall", "source_concept_name &&& source_concept_id…
#> $ additional_level <chr> "overall", "Gastrointestinal hemorrhage, unspecified …
```

## Cohort diagnostics

``` r
gibleed_cohort_diag <- cdm$gibleed |>
   cohortDiagnostics()
gibleed_cohort_diag |> 
  glimpse()
#> Rows: 1,814
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
#> $ cdm_name         <chr> "Synthea synthetic health database", "Synthea synthet…
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "gibleed", "gibleed", "gibleed", "gibleed", "gibleed"…
#> $ strata_name      <chr> "reason", "reason", "reason", "reason", "overall", "o…
#> $ strata_level     <chr> "Initial qualifying events", "Initial qualifying even…
#> $ variable_name    <chr> "number_records", "number_subjects", "excluded_record…
#> $ variable_level   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ estimate_name    <chr> "count", "count", "count", "count", "count", "count",…
#> $ estimate_type    <chr> "integer", "integer", "integer", "integer", "integer"…
#> $ estimate_value   <chr> "479", "479", "0", "0", "479", "479", "1944-01-20", "…
#> $ additional_name  <chr> "reason_id", "reason_id", "reason_id", "reason_id", "…
#> $ additional_level <chr> "1", "1", "1", "1", "overall", "overall", "overall", …
```

## Combining results

``` r
diagnostics <- bind(gibleed_cohort_diag) |> 
  suppress(minCellCount = 5)
diagnostics |> 
  glimpse()
#> Rows: 1,814
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
#> $ cdm_name         <chr> "Synthea synthetic health database", "Synthea synthet…
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "gibleed", "gibleed", "gibleed", "gibleed", "gibleed"…
#> $ strata_name      <chr> "reason", "reason", "reason", "reason", "overall", "o…
#> $ strata_level     <chr> "Initial qualifying events", "Initial qualifying even…
#> $ variable_name    <chr> "number_records", "number_subjects", "excluded_record…
#> $ variable_level   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ estimate_name    <chr> "count", "count", "count", "count", "count", "count",…
#> $ estimate_type    <chr> "integer", "integer", "integer", "integer", "integer"…
#> $ estimate_value   <chr> "479", "479", "0", "0", "479", "479", "1944-01-20", "…
#> $ additional_name  <chr> "reason_id", "reason_id", "reason_id", "reason_id", "…
#> $ additional_level <chr> "1", "1", "1", "1", "overall", "overall", "overall", …
```
