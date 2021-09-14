# glottospace

# STEPS
# library(devtools)
# document()
# load_all()
# install(force = TRUE) # ALWAYS INSTALL FROM HERE (PACKAGE), NOT FROM OTHER SCRIPT.

# FIXME
# Write wrapper geodist (i.e. make sure it works for points and lines, etc.)
# geodist only relevant between points and lines, not pols and lines
# buffer should be equal area, not necessarily equidistant.

# Visualization: change colorby to color, sizeby to size, etc? Maybe not..



# FUNCTIONS TO ADD -------------------------------------------------------------------
# get_glottodata: https://stackoverflow.com/questions/30177053/using-a-trycatch-block-to-read-a-csv-and-an-excel-file

# get_dplace: see isolates project
# Extend readme with examples: https://r-pkgs.org/release.html?q=readme#readme
# add a logo: https://rdrr.io/github/r-lib/usethis/man/use_logo.html
# Add example datasets
# - Does a language have a dictionary and a grammar, or multiple. Plot for multiple languages. From glottolog_source.bib.zip
#  - gs_join: join other data to glottolog, or extract glottolog coordinates and add them to external data.

# General: argument matching of functions (indicate choices): https://cran.r-project.org/web/packages/strex/vignettes/argument-matching.html

# See orange book for other data sources to add (discussion with Rik May 18, grammars, WALS, etc.)
# ethnolog contains information on number of speakers
# WALS contains information on language features.

# glottofilter: by location (bbox and drawing on plot)

# Points to raster:
# Density/richness interpolation, number of languages in a grid cell.Fig 1b: interpolated richness: https://zenodo.org/record/821360
# point density analysis: https://rspatial.org/raster/analysis/8-pointpat.html
# point density: https://cran.r-project.org/web/packages/pointdensityP/pointdensityP.pdf
# spatial interpolation (kriging?)
# see also: https://geocompr.github.io/geocompkg/articles/point-pattern.html

# Quote from https://rspatial.org/raster/sdm/4_sdm_envdata.html:
# Extract multiple points in a radius as a potential means for dealing with mismatch between location accuracy and grid cell size.
# If one would make 10 datasets that represent 10 equally valid “samples” of the environment in that radius,
# that could be then used to fit 10 models and explore the effect of uncertainty in location.

# So far, I've only focussed on geographic space (in contrast to environmental space): IDW, Voronoi, Kriging, etc.
# Geographic NULL models: https://rspatial.org/raster/sdm/7_sdm_NULLmodels.html
# Voronoi and buffer already implemented.
# Convex hulls (e.g. around isolates)
# circles function from dismo package (similar to buffer, but overlapping and raster output)
# For presence absence data (e.g. isolates), package gstat provides functions: geoIDW and voronoiHull %>%


# TODO
# normalize path. Change backward slashes to forward slashes, make platform independent.
# featuredist (can be used to calculate environmental or linguistic distances)
# Check if something is a dist object, if it is, convert to distmat

# function to drop spatial and drop units (for statistical/non-spatial analyses, see isolates script STAT preparation)
# extend file saving. Both images, maps, objects, but also tables.

