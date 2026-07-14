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
variables. This package aims to automatically fetch the required ACS data
and compute the index across flexible geographies (county, tract, block
group).

## Status

This package is in early development. The scaffold is in place; NDI
computation functions are being added.

## Installation

You can install the development version of `ReproducibleNDI` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("HanLabCollaboration/ReproducibleNDI")
```
