# nrvtools

<!-- badges: start -->
[![R-CMD-check](https://github.com/FOR-CAST/nrvtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FOR-CAST/nrvtools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Provides tools for post-processing and summarizing outputs from Natural Range of Variability simulations.
Given vegetation type maps (VTM), stand age maps (SAM), time since fire (TSF) maps, and reporting polygons (`sf` objects), it calculates landscape and patch metrics, leading vegetation cover by age class, and large-patch area distributions across simulation replicates.
Results can be summarised as tabular outputs, 'X over time' plots, box-and-whisker plots, and histograms.
Designed for use with the [NRV_summary](https://github.com/FOR-CAST/NRV_summary) module.

## Installation

You can install `nrvtools` from GitHub using:

``` r
# install.packages("remotes")
pak::pkg_install("FOR-CAST/nrvtools")
```
