
<!-- README.md is generated from README.Rmd. Please edit that file -->

# findSVI

<!-- badges: start -->

[![R-CMD-check](https://github.com/heli-xu/findSVI/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/heli-xu/findSVI/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of findSVI is to calculate regional [CDC/ATSDR Social
Vulnerability Index
(SVI)](https://www.atsdr.cdc.gov/placeandhealth/svi/index.html) at a
geographic level of interest using US census data from American
Community Survey.

## Overview

findSVI includes two major steps:

- `get_census_data()`: retrieving US census data (Census API key
  required);
- `get_svi()`: calculating SVI based on [CDC/ATSDR SVI
  documentation](https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html).

CDC/ATSDR releases SVI biannually at the counties/census tracts level
for US or an individual state (which can be downloaded
[here](https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html)).
findSVI aims to support more flexible and specific SVI analysis with
additional options for years (2014-2021) and geographic levels (eg.
ZCTA/places, combining multiple states).

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
data <- get_census_data(2020, "county", "PA")
data[1:10, 1:10]
```

    #>    GEOID                           NAME B06009_002E B06009_002M B09001_001E
    #> 1  42001     Adams County, Pennsylvania        7788         602       20663
    #> 2  42003 Allegheny County, Pennsylvania       45708        1713      228296
    #> 3  42005 Armstrong County, Pennsylvania        3973         305       12516
    #> 4  42007    Beaver County, Pennsylvania        7546         640       31915
    #> 5  42009   Bedford County, Pennsylvania        3996         317        9386
    #> 6  42011     Berks County, Pennsylvania       36488        1356       93714
    #> 7  42013     Blair County, Pennsylvania        7292         679       24920
    #> 8  42015  Bradford County, Pennsylvania        4395         362       13358
    #> 9  42017     Bucks County, Pennsylvania       25651        1306      128008
    #> 10 42019    Butler County, Pennsylvania        6118         468       37577
    #>    B09001_001M B11012_010E B11012_010M B11012_015E B11012_015M
    #> 1           NA        1237         215         482         171
    #> 2           49       24311        1147        5378         525
    #> 3            9         912         161         247          85
    #> 4           NA        3380         380         787         174
    #> 5           11         468          99         213          50
    #> 6           44        8812         662        1695         304
    #> 7           19        2552         363         544         169
    #> 8           NA         969         177         428         117
    #> 9           53        8222         749        3174         581
    #> 10          NA        2121         337         813         198

(first ten rows and columns shown)

``` r
result <- get_svi(2020, data)
restult[1:10, 1:10]
```

    #>    GEOID                           NAME E_TOTPOP   E_HU   E_HH E_POV150 E_UNEMP
    #> 1  42001     Adams County, Pennsylvania   102627  42525  39628    13573    2049
    #> 2  42003 Allegheny County, Pennsylvania  1218380 602416 545695   212117   32041
    #> 3  42005 Armstrong County, Pennsylvania    65356  32852  28035    13566    1735
    #> 4  42007    Beaver County, Pennsylvania   164781  79587  72086    28766    4249
    #> 5  42009   Bedford County, Pennsylvania    48154  24405  19930    10130    1033
    #> 6  42011     Berks County, Pennsylvania   419062 167514 156389    77317   12196
    #> 7  42013     Blair County, Pennsylvania   122495  56960  51647    27397    2765
    #> 8  42015  Bradford County, Pennsylvania    60721  30691  25084    13731    1331
    #> 9  42017     Bucks County, Pennsylvania   627668 251373 240763    59899   14477
    #> 10 42019    Butler County, Pennsylvania   187798  84106  77725    24141    4498
    #>    E_HBURD E_NOHSDP E_UNINSUR
    #> 1     9088     7788      5656
    #> 2   133524    45708     46333
    #> 3     5719     3973      2632
    #> 4    15764     7546      6242
    #> 5     3952     3996      3310
    #> 6    40982    36488     25627
    #> 7    12146     7292      6155
    #> 8     5520     4395      3992
    #> 9    57197    25651     25208
    #> 10   15043     6118      6151

(first ten rows and columns shown)
