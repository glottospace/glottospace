
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glottospace

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of glottospace is to … The R package glottospace helps useRs to
do geospatial analyses on linguistic and cultural data. Examples
include: matching linguistic datasets to their location, calculating
distances between languages based on their spatial location or
linguistic features, visualizing linguistic data on a map.

ADD some examples here with r code

## Installation

You can install the development version of glottospace from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SietzeN/glottospace")
```

## Workflow of glottospace

### gather

Download data from different sources, generate empty data structures
that help you to add data in a structured way.

### check

Interactive quality checks of user-provided data. Quickly check data
coverage, missing values, etc.

### clean

Clean data where necessary, recode values, select columns/remove
redundant

### integrate

, and integrating them to use their full potential. Join user-provided
data with glottodata, add locations

### explore

search: search languages, filter all languages in a certain country

### enrich

add data, extract elevation of each language

### calculate

geodistances: calculating distances between languages, nearest
languages, etc. glottodistances: calculating similarities between
languages based on linguistic/cultural features

### visualize

plotting linguistic and cultural data on a map plotting distances
between languages

## Some basic concepts: data formats

The glottospace package supports three main data formats: glottodata,
glottobase, glottospace

1.  glottodata. This is user-provided data that consist of a glottocode
    and any number of linguistic and/or cultural variables (features).

``` r
glottodata <- get_glottodata()
head(glottodata)
```

2.  glottospace. This returns points or polygons for each language in
    [glottolog](https://glottolog.org/).

``` r
glottospace <- glottospace::get_glottospace()
head(glottospace)
#> Simple feature collection with 6 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 36.5721 ymin: -9.03389 xmax: 146.992 ymax: 5.95034
#> Geodetic CRS:  WGS 84
#>   glottocode                  geometry
#> 1   aari1239   POINT (36.5721 5.95034)
#> 2   aasa1238  POINT (36.8648 -4.00679)
#> 3   abad1241  POINT (146.992 -9.03389)
#> 4   abag1245  POINT (145.665 -6.12028)
#> 5   abai1240   POINT (118.306 5.55394)
#> 6   abai1241 POINT (116.1625 3.524226)
```

3.  glottobase. This is a boosted/enriched version of
    [glottolog](https://glottolog.org/).

``` r
glottobase <- glottospace::get_glottobase()
colnames(glottobase)
#>  [1] "glottocode"          "family_id"           "parent_id"          
#>  [4] "name"                "isocode"             "child_dialect_count"
#>  [7] "country_ids"         "family_name"         "isolate"            
#> [10] "family_size"         "family_size_rank"    "country"            
#> [13] "continent"           "region"              "geometry"
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(glottospace)
#> 
#> Attaching package: 'glottospace'
#> The following object is masked _by_ '.GlobalEnv':
#> 
#>     glottobase
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/master/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
