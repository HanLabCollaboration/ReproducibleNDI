<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/HanLabCollaboration/ReproducibleNDI/graph/badge.svg)](https://app.codecov.io/gh/HanLabCollaboration/ReproducibleNDI)
<!-- badges: end -->

# ReproducibleNDI

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

The goal of `ReproducibleNDI` is to provide a simple, tidy interface for
calculating the **Neighborhood Deprivation Index (NDI)**, a composite
measure of neighborhood-level socioeconomic status (SES), using data from
the US Census Bureau's American Community Survey (ACS).

## What is the Neighborhood Deprivation Index?

The Neighborhood Deprivation Index is a composite score summarizing
socioeconomic deprivation at the neighborhood level from a set of ACS
variables. This package automatically fetches the required ACS data and
computes the index across flexible geographies (county, tract, block
group).

Several NDI methodologies exist; the `type` parameter selects which one
to compute. Currently implemented:

- **`type = "messer"`** — Messer et al. (2006), following the
  implementation of the [`ndi`](https://CRAN.R-project.org/package=ndi)
  package. A single principal component is extracted from 8 ACS-derived
  component variables (occupation, crowding, poverty, female-headed
  households, public assistance, income under \$30k, education,
  unemployment).

`type = "powell_wiley"` is planned next.

## Usage

``` r
library(ReproducibleNDI)

# Requires a Census API key:
# tidycensus::census_api_key("YOUR_KEY_HERE", install = TRUE)

# Messer NDI for California tracts (2022 ACS5), PCA fit within the state
ndi_ca <- computeNDI(
  type   = "messer",
  geo    = "tract",
  year   = 2022,
  states = "CA",
  scope  = "state",
  impute = TRUE
)

head(ndi_ca$df_ndi)       # NDIImputed, NDIImputedQuartile, annotation
head(ndi_ca$df_ndi_raw)   # baseline NDI (no imputation) for comparison
```

Key options (mirroring `ReproducibleYostIndex`):

- **`scope`**: `"national"` (one PCA over all geographies), `"state"`,
  or `"county"` (PCA fit separately within each group).
- **`impute`**: spatial mean imputation of missing components using
  queen contiguity, optionally weighted by total population
  (`weight_var`).
- **`return_format`**: `"detailed"` (list of data frames + PCA object)
  or `"minimal"` (one data frame).

## Status

This package is in early development.

## Installation

You can install the development version of `ReproducibleNDI` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("HanLabCollaboration/ReproducibleNDI")
```
