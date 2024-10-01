
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phenotypeR <img src="man/figures/logo.png" align="right" height="180"/>

<!-- badges: start -->

> **This package is under development and not yet ready for use.**

[![R-CMD-check](https://github.com/ohdsi/phenotypeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ohdsi/phenotypeR/actions/workflows/R-CMD-check.yaml)
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
#> Warning: package 'dplyr' was built under R version 4.2.3

con <- DBI::dbConnect(duckdb::duckdb(dbdir = CDMConnector::eunomia_dir()))
cdm <- CDMConnector::cdm_from_con(con = con,
                      cdm_schema = "main",
                      write_schema = "main")

cdm$gibleed <- conceptCohort(cdm = cdm,
                                   conceptSet = list(gibleed = 192671L),
                                   name = "gibleed")

result <- cdm$gibleed |>
   phenotypeDiagnostics()
#> Warning: The CDM reference containing the cohort must also contain achilles tables.
#> Returning only index event breakdown.
result |> 
  glimpse()
#> Rows: 2,998
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3,…
#> $ cdm_name         <chr> "Synthea synthetic health database", "Synthea synthet…
#> $ group_name       <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ group_level      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "general", "general", "observation_period", "cdm", "g…
#> $ variable_level   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ estimate_name    <chr> "snapshot_date", "person_count", "count", "source_nam…
#> $ estimate_type    <chr> "date", "integer", "integer", "character", "character…
#> $ estimate_value   <chr> "2024-10-01", "2694", "5343", "Synthea synthetic heal…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```
