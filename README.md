
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ograrc <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

ograrc is an internal R package developed by Data Analytics team at
Odyssey Re in order to house functions/tools which may be useful for
actuaries when calculating rarc from submission data.

## Installation

``` r
install.packages("ograrc") ### TBD currently only installable via remote repo



```

### Goals

ograrc provides a set of functions to help calculate Risk Adjusted Rate
change at underlying policy level from submission data.

### High level process/requirements

  - minimum fields: premium, inception date, limit, attachment, insured
    names/policy id (to create a key)
  - dataset should be structured and tabular ( does not require to be
    column conformed)
  - should have at least some renewals

### Walkthrough Example

In this example, an example data set is used to walk through a
calculation of rate change for illustration.

The example below shows a subset of a cedant’s data where there are two
insureds, “berlin” and “frankfurt” which have been written in 2019 and
renewed in 2020. The names vary slightly from year to year. We can use
one of the pre-processing functions `standardise_names` to create a key
from insured names which can be used to link renewals.

``` r
library(ograrc)

example <- data.table::data.table(
              insured = c("berlin", "frankfurt" , "berlin, inc", "frankfurt inc") , 
              inception = c("01/01/2019" , "01/06/2019" , "01/01/2020", "01/06/2020"),   
              premium = c(100,120,110,100),
              Limit = c(1000, 500, 1000,400),
              attach = c(100,100,50,50),
              industry = c("retail" , "pharma" , "retail" , "pharma")
              
              )
```

``` r
kableExtra::kable(example)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

insured

</th>

<th style="text-align:left;">

inception

</th>

<th style="text-align:right;">

premium

</th>

<th style="text-align:right;">

Limit

</th>

<th style="text-align:right;">

attach

</th>

<th style="text-align:left;">

industry

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

berlin

</td>

<td style="text-align:left;">

01/01/2019

</td>

<td style="text-align:right;">

100

</td>

<td style="text-align:right;">

1000

</td>

<td style="text-align:right;">

100

</td>

<td style="text-align:left;">

retail

</td>

</tr>

<tr>

<td style="text-align:left;">

frankfurt

</td>

<td style="text-align:left;">

01/06/2019

</td>

<td style="text-align:right;">

120

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:right;">

100

</td>

<td style="text-align:left;">

pharma

</td>

</tr>

<tr>

<td style="text-align:left;">

berlin, inc

</td>

<td style="text-align:left;">

01/01/2020

</td>

<td style="text-align:right;">

110

</td>

<td style="text-align:right;">

1000

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

retail

</td>

</tr>

<tr>

<td style="text-align:left;">

frankfurt inc

</td>

<td style="text-align:left;">

01/06/2020

</td>

<td style="text-align:right;">

100

</td>

<td style="text-align:right;">

400

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

pharma

</td>

</tr>

</tbody>

</table>

### Simple Data Cleansing

We can call `convert_data_type` to convert fields to correct data types
for R.

``` r
str(example)
#> Classes 'data.table' and 'data.frame':   4 obs. of  6 variables:
#>  $ insured  : chr  "berlin" "frankfurt" "berlin, inc" "frankfurt inc"
#>  $ inception: chr  "01/01/2019" "01/06/2019" "01/01/2020" "01/06/2020"
#>  $ premium  : num  100 120 110 100
#>  $ Limit    : num  1000 500 1000 400
#>  $ attach   : num  100 100 50 50
#>  $ industry : chr  "retail" "pharma" "retail" "pharma"
#>  - attr(*, ".internal.selfref")=<externalptr>
example <- convert_data_type(example, num_cols = c("premium" , "Limit", "attach"), date_cols = "inception" , factor_cols = "industry" )
#> ✓ numeric and factor fields converted
#> ✓ date fields conformed

str(example)
#> Classes 'data.table' and 'data.frame':   4 obs. of  7 variables:
#>  $ insured           : chr  "berlin" "frankfurt" "berlin, inc" "frankfurt inc"
#>  $ inception         : chr  "01/01/2019" "01/06/2019" "01/01/2020" "01/06/2020"
#>  $ premium           : num  100 120 110 100
#>  $ Limit             : num  1000 500 1000 400
#>  $ attach            : num  100 100 50 50
#>  $ industry          : Factor w/ 2 levels "pharma","retail": 2 1 2 1
#>  $ conform  inception: Date, format: "2019-01-01" "2019-01-06" ...
#>  - attr(*, ".internal.selfref")=<externalptr>
```

### Additional Pre-processing

Next we can preprocess the data and add additional features required for
calculating RARC by policy quarter. This includes:

  - adding a feature for QY in the form YYYYQQ
  - standardising the names and creating a field `std_Insured_name` to
    generate a key which can be used to link renewals.
  - Note : an alternative would be to use a repeating part of
    policy\_ids to also generate a key

### Adjusting For Limit/Attachment changes

Lastly we require a feature for LEV ( Limited Expected Value) which will
be used to adjust rate changes for changes in limit and attachment
point. The LEV is calculated via a lognormal curve with parameters mu
and sigma. K signifies the moment of the distribution.

``` r
example[, policy_year := lubridate::year(`conform  inception`) ]
example <- add_qy(example , policy_year, `conform  inception` , add_miss= TRUE)

example <- standardise_names(example , name_col = "insured" , clean_patt = "inc")
#> ✓ name field preprocessed and standardised
example <- calc_lev(example, Limit , attach , k=1 , mu =15.2, sigma=1.74)
```

### Calculate RARC

Now we can call `tidy_rarc` function to calculate rate change and return
a long form dataset of results

``` r
library(data.table)
example_rarc <- tidy_rarc(example, premium_type = "premium", col_key = c("insured", "industry"))
#> ✓ calculated rarc by period
#> ✓ reshaped output data into tidy format
#> ✓ calculated weights for RARC

kableExtra::kable(example_rarc)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

insured

</th>

<th style="text-align:left;">

industry

</th>

<th style="text-align:left;">

Policy\_Period

</th>

<th style="text-align:right;">

LEV\_Chg

</th>

<th style="text-align:right;">

Prem\_Chg

</th>

<th style="text-align:right;">

Premium

</th>

<th style="text-align:right;">

RARC

</th>

<th style="text-align:right;">

RARC\_WT

</th>

<th style="text-align:right;">

Restated\_Expiry

</th>

<th style="text-align:right;">

Restated\_Expiry\_Wt

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

berlin

</td>

<td style="text-align:left;">

retail

</td>

<td style="text-align:left;">

2019Q1

</td>

<td style="text-align:right;">

0.0

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

100

</td>

<td style="text-align:right;">

0.000000

</td>

<td style="text-align:right;">

0.0000

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.000000

</td>

</tr>

<tr>

<td style="text-align:left;">

berlin

</td>

<td style="text-align:left;">

retail

</td>

<td style="text-align:left;">

2020Q1

</td>

<td style="text-align:right;">

1.0

</td>

<td style="text-align:right;">

1.1000000

</td>

<td style="text-align:right;">

110

</td>

<td style="text-align:right;">

1.100000

</td>

<td style="text-align:right;">

121.0000

</td>

<td style="text-align:right;">

100

</td>

<td style="text-align:right;">

9.999994

</td>

</tr>

<tr>

<td style="text-align:left;">

frankfurt

</td>

<td style="text-align:left;">

pharma

</td>

<td style="text-align:left;">

2019Q1

</td>

<td style="text-align:right;">

0.0

</td>

<td style="text-align:right;">

0.0000000

</td>

<td style="text-align:right;">

120

</td>

<td style="text-align:right;">

0.000000

</td>

<td style="text-align:right;">

0.0000

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.000000

</td>

</tr>

<tr>

<td style="text-align:left;">

frankfurt

</td>

<td style="text-align:left;">

pharma

</td>

<td style="text-align:left;">

2020Q1

</td>

<td style="text-align:right;">

0.8

</td>

<td style="text-align:right;">

0.8333333

</td>

<td style="text-align:right;">

100

</td>

<td style="text-align:right;">

1.041667

</td>

<td style="text-align:right;">

104.1667

</td>

<td style="text-align:right;">

96

</td>

<td style="text-align:right;">

3.999996

</td>

</tr>

</tbody>

</table>

### Summarise RARC by qtr and industry

It is now easy to summarise the detailed output by whatever segmentation
is required - shown below by policy period

``` r
output <-example_rarc[ , .(Policy_Count = .N , Premium = sum(Premium), `OG Wtd RARC` = sum(Restated_Expiry_Wt)/sum(Restated_Expiry) ,
                                                mean_RARC_all = mean(RARC) -1,
                                                sd_err_rarc =sd(RARC)/sqrt(.N)), by= .(Policy_Period, industry) ][
                                                    order(Policy_Period, industry)][Policy_Period=="2020Q1"]
kableExtra::kable(output)
```

<table>

<thead>

<tr>

<th style="text-align:left;">

Policy\_Period

</th>

<th style="text-align:left;">

industry

</th>

<th style="text-align:right;">

Policy\_Count

</th>

<th style="text-align:right;">

Premium

</th>

<th style="text-align:right;">

OG Wtd RARC

</th>

<th style="text-align:right;">

mean\_RARC\_all

</th>

<th style="text-align:right;">

sd\_err\_rarc

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

2020Q1

</td>

<td style="text-align:left;">

pharma

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

100

</td>

<td style="text-align:right;">

0.0416666

</td>

<td style="text-align:right;">

0.0416666

</td>

<td style="text-align:right;">

NA

</td>

</tr>

<tr>

<td style="text-align:left;">

2020Q1

</td>

<td style="text-align:left;">

retail

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

110

</td>

<td style="text-align:right;">

0.0999999

</td>

<td style="text-align:right;">

0.0999999

</td>

<td style="text-align:right;">

NA

</td>

</tr>

</tbody>

</table>
