
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EarthquakeAnalysis

<!-- badges: start -->
<!-- badges: end -->

The goal of EarthquakeAnalysis is to complete the MSDR Capstone project
on Coursera.

This capstone project will be centered around a dataset obtained from
the U.S. National Oceanographic and Atmospheric Administration (NOAA) on
significant earthquakes around the world.

## Installation

You can install the development version of EarthquakeAnalysis like so:

``` r
library(devtools)
install_github("BFavetto/EarthquakeAnalysis")
library(EarthquakeAnalysis)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dplyr)
library(ggplot2)

data("NOAAdata")

NOAAdata %>% eq_clean_data() %>%
  eq_location_clean() %>%
  filter(Country %in% "ITALY" &
               lubridate::year(Date) >= 2000) %>%
  eq_map(annot_col = "Date")
```
