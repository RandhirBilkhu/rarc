
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datools <img src='man/figures/datools.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

datools is an internal R package developed by Data Analytics team at
Odyssey Re in order to house functions/tools which may be useful for
actuaries.

## Installation

``` r
install.packages("datools") ### TBD currently only installable via remote repo

remotes::install_git("https://OdysseyGroup@dev.azure.com/OdysseyGroup/DataAnalytics/_git/DATools.git",
                     credentials=git2r::cred_user_pass("user", "password"))

```

### RARC

a set of functions to help calculate Risk Adjusted Rate change at
underlying policy level from submission data.

### TBD
