
# glottospace -------------------------------------------------------------


# STEPS
# library(devtools)
# document()
# load_all()
# install(force = TRUE) # ALWAYS INSTALL FROM HERE (PACKAGE), NOT FROM OTHER SCRIPT.


# warnings ----------------------------------------------------------------
# glottospace\R\get_geodata.R:16] @examples requires a value
# glottospace\R\get_geodata.R:37] @examples requires a value
# glottospace\R\glottologbooster.R:98] @examples requires a value
# glottospace\R\glottosave.R:8] @param requires name and description
# glottospace\R\glottosave.R:14] @examples requires a value
# glottospace\R\glottosave.R:45] @param requires name and description
# glottospace\R\glottosearch.R:35] @param requires name and description
# glottospace\R\glottosearch.R:108] @examples requires a value
# glottospace\R\helpers_geo.R:49] @param requires name and description
# glottospace\R\helpers_geo.R:50] @param requires name and description
# glottospace\R\helpers_geo.R:57] @examples requires a value
# glottospace\R\helpers_geo.R:80] @param requires name and description
# glottospace\R\helpers_geo.R:81] @param requires name and description
# glottospace\R\helpers_geo.R:82] @param requires name and description
# glottospace\R\helpers_geo.R:89] @examples requires a value
# glottospace\R\joindata.R:50] @param requires name and description
# glottospace\R\joindata.R:56] @examples requires a value
# glottospace\R\joindata.R:69] @param requires name and description
# glottospace\R\joindata.R:75] @examples requires a value
# glottospace\R\plotdistance_nmds.R:5] @param requires name and description
# glottospace\R\plotdistance_nmds.R:6] @param requires name and description
# glottospace\R\plotdistance_nmds.R:49] @examples requires a value
# glottospace\R\plotdistance_nmds.R:58] @param requires name and description
# glottospace\R\plotdistance_nmds.R:59] @param requires name and description
# glottospace\R\plotdistance_nmds.R:60] @param requires name and description
# glottospace\R\plotdistance_nmds.R:61] @param requires name and description

# getdata -----------------------------------------------------------------
# FIXME: get_glottolog: https://stackoverflow.com/questions/30177053/using-a-trycatch-block-to-read-a-csv-and-an-excel-file
# TODO: function name remove underscore? getglottobase, getglottodata
# TODO: get_dplace: see isolates project
# TODO: Does a language have a dictionary and a grammar, or multiple. From glottolog_source.bib.zip
# TODO: Add data sources: ethnolog contains information on number of speakers
# TODO: Add data sources: WALS contains information on language features.



# checkdata ---------------------------------------------------------------


# cleandata ---------------------------------------------------------------


# joindata ----------------------------------------------------------------

# searchdata --------------------------------------------------------------
# TODO: glottofilter: by location (bbox and drawing on plot)


# glottodistance ----------------------------------------------------------
# featuredist (can be used to calculate environmental or linguistic distances)

# geodata -----------------------------------------------------------------
# FIXME: After rasterextraction, active geometry is set to points, this should remain polygons.
# TODO: glottodata_dropspatial: drop spatial and drop units (for statistical/non-spatial analyses, see isolates script STAT preparation)

# geodistance -------------------------------------------------------------
# FIXME: Write wrapper geodist (i.e. make sure it works for points and lines, etc.)
# geodist only relevant between points and lines, not pols and lines
# buffer should be equal area, not necessarily equidistant.



# visualize ---------------------------------------------------------------

# save & export -----------------------------------------------------------
# TODO: extend file saving. Both images, maps, objects, but also tables.

# general -----------------------------------------------------------------
# TODO: add a logo: https://rdrr.io/github/r-lib/usethis/man/use_logo.html
# TODO: argument matching of functions (indicate choices): https://cran.r-project.org/web/packages/strex/vignettes/argument-matching.html
# TODO: normalize path. Change backward slashes to forward slashes, make platform independent.

# Documentation: # @family <> tag , follow readme..





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




