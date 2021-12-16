
# glottospace -------------------------------------------------------------


# STEPS
# library(devtools)
# document()
# load_all()
# install(force = TRUE, args=c("--no-multiarch")) # ALWAYS INSTALL FROM HERE (PACKAGE), NOT FROM OTHER SCRIPT.
# knit README

# glottoget -----------------------------------------------------------------

# xfun::gsub_dir(dir = "C:/Users/sjnor/surfdrive/PROJECTS_SN/SAPPHIRE/R/glottospace/R", pattern = "glottosimplify_meta", replacement = "glottosimplify_dropmeta")

# glottocheck ---------------------------------------------------------------
# TODO: Add high-level function glottocheck, first check: glottodata, glottosubdata, metadata

# cleandata ---------------------------------------------------------------
# FIXME: glottoclean_recodemissing replace naniar, results in errors when building

# glottocreate ------------------------------------------------------------
# TODO: write wrapper: glottocreate (glottosubdata or glottodata)
# Perhaps replace glottocreate_structuretable with glottodata_createstructuretable, that feels more logical, because it's an operation on glottodata
# Be more explicit about tables and lists: glottodata can be stored in a table or in a list of tables.

# glottojoin ----------------------------------------------------------------


# glottofilter --------------------------------------------------------------
# TODO: glottofilter: by location (bbox and drawing on plot)
# TODO: make more tolerant: if no country/continent/region found, message did you mean: country?
# make sure that's not case-senisitive 'Europe' vs. 'europe'
# TODO: glottofilter: Make more flexible country or continent, if user specifies country = "South America" this should also work.
# FIXME: error with expression


# glottosimplify --------------------------------------------------------------
# TODO: drop languages based on conditions, for example, remove all Indo-European languages.


# glottosearch ------------------------------------------------------------


# glottodistance ----------------------------------------------------------
# featuredist (can be used to calculate environmental or linguistic distances)
# FIXME: currently colnames is hardcoded, perhaps it should be the first column by default, unless a name is provided. Create clear error message to indicate this.

# glottospace -------------------------------------------------------------
# TODO: check whether glottodata has meta, if so only glottodata should be made spatial (not meta columns)

# glottosimplify --------------------------------------------------------------


# geodata & geotools -----------------------------------------------------------------
# FIXME: After rasterextraction, active geometry is set to points, this should remain polygons.
# TODO: glottodata_dropspatial: drop spatial and drop units (for statistical/non-spatial analyses, see isolates script STAT preparation)
# mergevec: overwrite = FALSE

# TODO: points2pols: Make more flexible country or continent, if user specifies country = "South America" this should also work.
# Or would it be possible to do that automatically: check unique values in country and continent.
# If there are many countries and one continent, mask by continent.
# If there is one or a few countries, mask by country

# geodistance -------------------------------------------------------------
# FIXME: Write wrapper geodist (i.e. make sure it works for points and lines, etc.)
# FIXME: geodist, save by default as dist, not as 'units'.
# geodist only relevant between points and lines, not pols and lines
# buffer should be equal area, not necessarily equidistant.




# glottomap & glottoplot ---------------------------------------------------------------
# FIXME: 3d plot nmds
# bbox expansion: default true, but optional.
# administrative units of lower level
# Center map: https://stackoverflow.com/questions/10620862/use-different-center-than-the-prime-meridian-in-plotting-a-world-map
# use: coord_map(xlim = c(-180, 180)) or coord_map(xlim = c(0, 360)) instead of scale_x_continuous(limits = c(0, 360))
# Plot on a sphere (Lambert Azimuthal Equal Area): # # https://proj.org/operations/projections/laea.html
# # https://stackoverflow.com/questions/43207947/whole-earth-polygon-for-world-map-in-ggplot2-and-sf
# Choose colors when plotting
# Reorder legend (or more specifically, set apart one)
# https://stackoverflow.com/questions/26102012/plotting-world-map-in-orthographic-projection-is-giving-non-finite-points
# Center map on Pacific (New Guinea) , or provide coordinates.

# save & export -----------------------------------------------------------
# TODO: overwrite = TRUE/FALSE

# general -----------------------------------------------------------------
# TODO: add a logo: https://rdrr.io/github/r-lib/usethis/man/use_logo.html
# TODO: argument matching of functions (indicate choices): https://cran.r-project.org/web/packages/strex/vignettes/argument-matching.html
# TODO: normalize path. Change backward slashes to forward slashes, make platform independent.

# optional extensions ---------------------------------------------------
# circles function from dismo package (similar to buffer, but overlapping and raster output)
# For presence absence data (e.g. isolates), package gstat provides functions: geoIDW and voronoiHull %>%

# Quote from https://rspatial.org/raster/sdm/4_sdm_envdata.html:
# Extract multiple points in a radius as a potential means for dealing with mismatch between location accuracy and grid cell size.
# If one would make 10 datasets that represent 10 equally valid “samples” of the environment in that radius,
# that could be then used to fit 10 models and explore the effect of uncertainty in location.

# Points to raster:
# Density/richness interpolation, number of languages in a grid cell.Fig 1b: interpolated richness: https://zenodo.org/record/821360
# point density analysis: https://rspatial.org/raster/analysis/8-pointpat.html
# point density: https://cran.r-project.org/web/packages/pointdensityP/pointdensityP.pdf
# spatial interpolation (kriging?)
# see also: https://geocompr.github.io/geocompkg/articles/point-pattern.html


# REQUESTS ----------------------------------------------------------------

# TODO: Match datasources of glottologdata to languages with: head(glottolog_cldf$sources[,"LGCODE"])
# TODO: glottoget_dplace: see isolates project
# TODO: Does a language have a dictionary and a grammar, or multiple. From glottolog_source.bib.zip
# TODO: Add data sources: ethnolog contains information on number of speakers





