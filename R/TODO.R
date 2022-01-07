
# glottospace -------------------------------------------------------------


# STEPS
# library(devtools)
# document()
# load_all()
# install(force = TRUE, args=c("--no-multiarch")) # ALWAYS INSTALL FROM HERE (PACKAGE), NOT FROM OTHER SCRIPT.
# knit README

# xfun::gsub_dir(dir = "C:/Users/sjnor/surfdrive/PROJECTS_SN/Rpackages/glottospace/R", pattern = "glottocode_exists", replacement = "glottocode_exists")


# General tasks:
# Remove functions from index with @NoRd and @keywords internal : https://community.rstudio.com/t/keywords-internal-vs-nord/35119
# TODO: argument matching of functions (indicate choices): https://cran.r-project.org/web/packages/strex/vignettes/argument-matching.html


# glottoget -----------------------------------------------------------------
# wals: labels of variables
# wals is rarely updated, so no need to check for updates as regularly as glottolog.

# glottocheck ---------------------------------------------------------------
# TODO: Add high-level function glottocheck, first check: glottodata, glottosubdata, metadata

# glottoclean ---------------------------------------------------------------
# FIXME: glottorecode_missing replace naniar, results in errors when building

# glottocreate ------------------------------------------------------------
# TODO: write wrapper: glottocreate (glottosubdata or glottodata)
# Perhaps replace glottocreate_structuretable with glottodata_createstructuretable, that feels more logical, because it's an operation on glottodata
# Be more explicit about tables and lists: glottodata can be stored in a table or in a list of tables.

# glottojoin ----------------------------------------------------------------


# glottofilter --------------------------------------------------------------
# TODO: make more tolerant: if no country/continent/region found, message did you mean: country?
# make sure that's not case-senisitive 'Europe' vs. 'europe'
# TODO: glottofilter: Make more flexible country or continent, if user specifies country = "South America" this should also work.
# FIXME: error with expression

# glottosimplify --------------------------------------------------------------

# glottosearch ------------------------------------------------------------


# glottodistance ----------------------------------------------------------
# FIXME: currently colnames is hardcoded, perhaps it should be the first column by default, unless a name is provided. Create clear error message to indicate this.

# glottospace -------------------------------------------------------------
# TODO: check whether glottodata has meta, if so only glottodata should be made spatial (not meta columns)
# FIXME: Was it in glottospace function that active geometry is changed? Should not be duplicated, one should be removed, or active should be set to appropriate geom


# glottosimplify --------------------------------------------------------------

# glottospace -------------------------------------------------------------
# Clean-up script and simplify.
# TODO: points2pols: Make more flexible country or continent, if user specifies country = "South America" this should also work.
# Or would it be possible to do that automatically: check unique values in country and continent.
# If there are many countries and one continent, mask by continent.
# If there is one or a few countries, mask by country




# glottospotlight ---------------------------------------------------------
# Unnessecarily complicated. Would be much easier to assign NA to all languages that are not the focus.


# glottomap & glottoplot ---------------------------------------------------------------
# add naviewer (glottocheck) to glottoplot?
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
# rivers, check whether file exists locally and store in package folder.


# If user specifies a country/continent that's not in the database, give a more informative error message. For example, show a list of continents.
# Example: country = Australia, continent = "Oceania".
# Typing glottomap(continent = "Oceania") is not really pretty because spans 180.

# save & export -----------------------------------------------------------
# TODO: overwrite = TRUE/FALSE


# REQUESTS ----------------------------------------------------------------

# TODO: Match datasources of glottologdata to languages with: head(glottolog_cldf$sources[,"LGCODE"])
# TODO: glottoget_dplace: see isolates project
# TODO: Does a language have a dictionary and a grammar, or multiple. From glottolog_source.bib.zip
# TODO: Add data sources: ethnolog contains information on number of speakers





