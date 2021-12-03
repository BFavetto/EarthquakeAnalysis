
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EarthquakeAnalysis

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/BFavetto/EarthquakeAnalysis.svg?branch=main)](https://travis-ci.com/BFavetto/EarthquakeAnalysis)
[![R-CMD-check](https://github.com/BFavetto/EarthquakeAnalysis/workflows/R-CMD-check/badge.svg)](https://github.com/BFavetto/EarthquakeAnalysis/actions)
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

This is a basic example which shows you how to plot a map with recent
earthquakes in Italy:

``` r
library(dplyr)
library(ggplot2)

# load the dataset
data("NOAAdata")

NOAAdata %>% eq_clean_data() %>%
  eq_location_clean() %>%
  filter(Country %in% "ITALY" &
               lubridate::year(Date) >= 2000) %>%
  eq_map(annot_col = "Date")
```
