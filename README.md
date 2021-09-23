
<!-- README.md is generated from README.Rmd. Please edit that file -->

# glottospace

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## R package for the geospatial analysis of linguistic and cultural data.

    Examples include: matching linguistic datasets to their location, 
    calculating distances between languages based on their spatial location or linguistic features, 
    visualizing linguistic data on a map.

## Installation

You can install the development version of glottospace from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SietzeN/glottospace")
```

## Introduction to glottospace

### Plotting language locations on a map

Imagine you’re working with languages in a particular region, and want
to visualize them on a map.You could for example filter all languages in
South America, and show which ones of them are isolate languages:

``` r
library(glottospace)
## Filter by continent
glottopoints <- glottofilter(continent = "South America")
## Plot point data:
glottomap(glottodata = glottopoints, color = "isolate")
```

<img src="man/figures/README-glottospoints_sa-1.png" width="100%" />

Languages are often represented with points, while in reality the
speakers of a language can inhabit vast areas. Glottospace works with
point and polygon data. When polygon data is not available, you can
interpolate the points and plot those.

``` r
# Interpolate points to polygons:
glottopols <- points2pols(glottopoints, method = "voronoi", continent = "South America")
#> Warning: attribute variables are assumed to be spatially constant throughout all
#> geometries
# Plot polygon data:
glottomap(glottodata = glottopols, color = "family_size_rank")
```

<img src="man/figures/README-glottopols_sa-1.png" width="100%" />

### Adding contextual data for specific languages

Imagine you are interested in a particular set of languages, and want to
add some contextual information.

``` r
# Search languages:
glottosearch(find = "Dutsch")
#> Simple feature collection with 6 features and 14 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -58.5626 ymin: -4.19566 xmax: 152.183 ymax: 52.16
#> Geodetic CRS:  WGS 84
#>      glottocode family_id parent_id                   name isocode
#> 763    berb1259  indo1319  zeeu1239   Berbice Creole Dutch     brc
#> 1681   dutc1256  indo1319  glob1241                  Dutch     nld
#> 4263   midd1321  indo1319  midd1347           Middle Dutch     dum
#> 5155   oldd1237  indo1319  macr1270 Old Dutch-Old Frankish     odt
#> 6163   skep1238  indo1319  zeeu1239     Skepi Creole Dutch     skw
#> 7044   unse1236  indo1319  glob1243           Unserdeutsch     uln
#>      child_dialect_count                      country_ids   family_name isolate
#> 763                    0                               GY Indo-European   FALSE
#> 1681                  12 AW BE BQ BR CW DE GF GY NL SR SX Indo-European   FALSE
#> 4263                   0                               NL Indo-European   FALSE
#> 5155                   2                               NL Indo-European   FALSE
#> 6163                   0                               GY Indo-European   FALSE
#> 7044                   0                            AU PG Indo-European   FALSE
#>      family_size family_size_rank     country     continent         region
#> 763          583               55      Guyana South America  South America
#> 1681         583               55 Netherlands        Europe Western Europe
#> 4263         583               55 Netherlands        Europe Western Europe
#> 5155         583               55 Netherlands        Europe Western Europe
#> 6163         583               55      Guyana South America  South America
#> 7044         583               55        <NA>          <NA>           <NA>
#>                      geometry
#> 763   POINT (-58.0002 5.3829)
#> 1681             POINT (5 52)
#> 4263      POINT (5.34 51.656)
#> 5155        POINT (5.2 52.16)
#> 6163 POINT (-58.5626 6.05191)
#> 7044 POINT (152.183 -4.19566)
```

There are 6 glots that resemble our search term. Let’s check on
[glottolog](https://glottolog.org/) if Dutch is the language we’re
looking for.

``` r
glottocode_online("dutc1256")
```

Yes, that’s the one. Let’s check where Dutch is located in comparison to
other languages in the Netherlands.

``` r
dutchies <- glottofilter(country = "Netherlands")
```

Get and extract environmental data.

``` r
elevation <- get_geodata(download = "elevation", country = "Netherlands")
#> Warning in showSRID(uprojargs, format = "PROJ", multiline = "NO", prefer_proj
#> = prefer_proj): Discarded datum Unknown based on WGS84 ellipsoid in Proj4
#> definition
elevdutchies <- extractgeodata(glottodata = dutchies, geodata = elevation)
#> geodata extracted
```

Let’s plot the elevation of the Dutchies:

``` r
glottomap(glottodata = elevdutchies, color = "elevation", label = "name", ptsize = 0.85)
#> Variable(s) "elevation" contains positive and negative values, so midpoint is set to 0. Set midpoint = NA to show the full spectrum of the color palette.
```

<img src="man/figures/README-map_dutchies-1.png" width="100%" />

## Workflow of glottospace

### getdata

glottoload/glottoget and geoget/geoload Download linguistic, cultural,
geographic, and environmental data. Generate empty data structures that
help you to add your own data in a structured way.

### glottocheck

Interactive quality checks of user-provided data. Quickly check data
coverage, missing values, etc.

### glottoclean & glottoboost

Clean data where necessary, recode values, select columns/remove
redundant. Upgrade data.

### glottojoin

Join user-provided glottodata with other dataset., and integrating them
to use their full potential.

### glottosearch & glottofilter

searchdata: filter, select, query search: search languages, filter all
languages in a certain country

### glottodist

Quantify differences and similarities between languages glottodistances:
calculating similarities between languages based on linguistic/cultural
features

### glottoplot

Visualizing differences (distances) between languages based on
linguistic, cultural, and environmental features.

### glottospace

Make glottodata spatial, add coordinates, return point or polygons for
languages.

### geodata & geotools

add data, extract elevation of each language

### geodist

geodistances: calculating distances between languages, nearest
languages, etc.

### glottomap

Visualizing linguistic and cultural data on a map

### glottosave

Save output generated by glottospace (data, figures, maps, etc.). Output
that has been exported from the glottospace package can be read from
other software package, or loaded again with getglottodata.

## Some basic concepts: data formats

The glottospace package has three built-in datasets: glottobase,
glottospace, and userdata.

``` r
data(package = "glottospace")
```

1.  glottobase: This is a boosted/enriched version of
    [glottolog](https://glottolog.org/) data. It is used as reference
    dataset in several functions. The glottocodes are used as unique IDs
    to match different data sources.

``` r
data("glottobase")
# For more information about glottobase
?glottobase
colnames(glottobase)
#>  [1] "glottocode"          "family_id"           "parent_id"          
#>  [4] "name"                "isocode"             "child_dialect_count"
#>  [7] "country_ids"         "family_name"         "isolate"            
#> [10] "family_size"         "family_size_rank"    "country"            
#> [13] "continent"           "region"              "geometry"
```

2.  glottospace. A simple dataset with glottocodes and a geometry colum.
    The glottocodes are used to match a language to a location.

``` r
glottospace <- get_glottospace()
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

3.  userdata. This is user-provided data that consist of a glottocode
    and any number of linguistic and/or cultural variables (features).

``` r
glottodata <- get_glottodata(meta = FALSE)
glottodata
```
