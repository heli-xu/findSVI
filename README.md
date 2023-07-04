
<!-- README.md is generated from README.Rmd. Please edit that file -->

# findSVI <a href="https://heli-xu.github.io/findSVI/"><img src="man/figures/logo.png" align="right" height="139"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/heli-xu/findSVI/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/heli-xu/findSVI/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of findSVI is to calculate regional [CDC/ATSDR Social
Vulnerability Index
(SVI)](https://www.atsdr.cdc.gov/placeandhealth/svi/index.html) at a
geographic level of interest using US census data from American
Community Survey.

## Overview

CDC/ATSDR releases SVI biannually at the counties/census tracts level
for US or an individual state (which can be downloaded
[here](https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html)).
findSVI aims to support more flexible and specific SVI analysis with
additional options for years (2012-2021) and geographic levels (e.g.,
ZCTA/places, combining multiple states).

To find SVI for one or multiple year-state pair(s):

- `find_svi()`: retrieves US census data (Census API key required) and
  calculates SVI based on [CDC/ATSDR SVI
  documentation](https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html)
  for each year-state pair at the same geography level.

In most cases, `find_svi()` would be the easiest option. If you’d like
to include simple feature geometry or have more customized requests for
census data retrieval (e.g., different geography level for each
year-state pair, multiple states for one year), you can process
individual entry using the following:

- `get_census_data()`: retrieves US census data (Census API key
  required);
- `get_svi()`: calculates SVI from the census data supplied.

Essentially, `find_svi()` is a wrapper function for `get_census_data()`
and `get_svi()` that also supports iteration over 1-year-and-1-state
pairs at the same geography level.

## Installation

You can install the development version of findSVI from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("heli-xu/findSVI")
```

## Usage

``` r
library(findSVI)
library(dplyr)

summarise_results <- find_svi(
  year = c(2017, 2018),
  state = c("NJ", "PA"),
  geography = "county"
)
summarise_results %>% head(10)
```

    #> # A tibble: 10 × 8
    #>    GEOID RPL_theme1 RPL_theme2 RPL_theme3 RPL_theme4 RPL_themes  year state
    #>    <chr>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl> <dbl> <chr>
    #>  1 34001       0.95       0.8        0.65       1          0.95  2017 NJ   
    #>  2 34003       0.2        0.3        0.55       0.45       0.25  2017 NJ   
    #>  3 34005       0.3        0.5        0.35       0.4        0.3   2017 NJ   
    #>  4 34007       0.7        0.9        0.55       0.6        0.75  2017 NJ   
    #>  5 34009       0.65       0.6        0.1        0.55       0.45  2017 NJ   
    #>  6 34011       1          0.95       0.75       0.95       1     2017 NJ   
    #>  7 34013       0.9        0.75       0.85       0.85       0.9   2017 NJ   
    #>  8 34015       0.35       0.7        0.15       0.15       0.35  2017 NJ   
    #>  9 34017       0.75       0          1          0.75       0.7   2017 NJ   
    #> 10 34019       0          0.05       0.05       0.1        0     2017 NJ

(first ten rows shown)

``` r
data <- get_census_data(2020, "county", "PA")
data[1:10, 1:10]
```

    #> # A tibble: 10 × 10
    #>    GEOID NAME    B0600…¹ B0600…² B0900…³ B0900…⁴ B1101…⁵ B1101…⁶ B1101…⁷ B1101…⁸
    #>    <chr> <chr>     <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    #>  1 42001 Adams …    7788     602   20663      NA    1237     215     482     171
    #>  2 42003 Allegh…   45708    1713  228296      49   24311    1147    5378     525
    #>  3 42005 Armstr…    3973     305   12516       9     912     161     247      85
    #>  4 42007 Beaver…    7546     640   31915      NA    3380     380     787     174
    #>  5 42009 Bedfor…    3996     317    9386      11     468      99     213      50
    #>  6 42011 Berks …   36488    1356   93714      44    8812     662    1695     304
    #>  7 42013 Blair …    7292     679   24920      19    2552     363     544     169
    #>  8 42015 Bradfo…    4395     362   13358      NA     969     177     428     117
    #>  9 42017 Bucks …   25651    1306  128008      53    8222     749    3174     581
    #> 10 42019 Butler…    6118     468   37577      NA    2121     337     813     198
    #> # … with abbreviated variable names ¹​B06009_002E, ²​B06009_002M, ³​B09001_001E,
    #> #   ⁴​B09001_001M, ⁵​B11012_010E, ⁶​B11012_010M, ⁷​B11012_015E, ⁸​B11012_015M

(first ten rows and columns shown)

``` r
result <- get_svi(2020, data)
restult[1:10, 1:10]
```

    #> # A tibble: 10 × 10
    #>    GEOID NAME      E_TOT…¹   E_HU   E_HH E_POV…² E_UNEMP E_HBURD E_NOH…³ E_UNI…⁴
    #>    <chr> <chr>       <dbl>  <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    #>  1 42001 Adams Co…  102627  42525  39628   13573    2049    9088    7788    5656
    #>  2 42003 Alleghen… 1218380 602416 545695  212117   32041  133524   45708   46333
    #>  3 42005 Armstron…   65356  32852  28035   13566    1735    5719    3973    2632
    #>  4 42007 Beaver C…  164781  79587  72086   28766    4249   15764    7546    6242
    #>  5 42009 Bedford …   48154  24405  19930   10130    1033    3952    3996    3310
    #>  6 42011 Berks Co…  419062 167514 156389   77317   12196   40982   36488   25627
    #>  7 42013 Blair Co…  122495  56960  51647   27397    2765   12146    7292    6155
    #>  8 42015 Bradford…   60721  30691  25084   13731    1331    5520    4395    3992
    #>  9 42017 Bucks Co…  627668 251373 240763   59899   14477   57197   25651   25208
    #> 10 42019 Butler C…  187798  84106  77725   24141    4498   15043    6118    6151
    #> # … with abbreviated variable names ¹​E_TOTPOP, ²​E_POV150, ³​E_NOHSDP, ⁴​E_UNINSUR

(first ten rows and columns shown)
