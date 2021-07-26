# glottospace

# STEPS
# To install package: install(force = TRUE)
# ALWAYS INSTALL FROM HERE, NOT FROM OTHER SCRIPT.

# FUNCTIONS TO ADD -------------------------------------------------------------------

# - Does a language have a dictionary and a grammar, or multiple. Plot for multiple languages. From glottolog_source.bib.zip
#  - gs_join: join other data to glottolog, or extract glottolog coordinates and add them to external data.

# General: argument matching of functions (indicate choices): https://cran.r-project.org/web/packages/strex/vignettes/argument-matching.html

# See orange book for other data sources to add (discussion with Rik May 18, grammars, WALS, etc.)
# ethnolog contains information on number of speakers
# WALS contains information on language features.

# gs_filter: by location (bbox and drawing on plot)

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
# Remove sign languages, unclassified, dummy families (Urban 2021)
# extend file saving. Both images, maps, objects, but also tables.
# Family_name column for plotting: 'isolate', if you plot families als isolates will get the same colour.
