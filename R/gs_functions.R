# glottospace

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

# GENERAL FUNCTIONS ---------------------------------------------------------------

gs_polygonize <- function(points = NULL, type = "buffer", radius = 10, country = NULL, continent = NULL){
  if(!st_is_longlat(points)){stop("Please provide points in lon/lat")}
  # Alternative could be to convert to equidistant projection: https://epsg.io/54032
  epsg_utm <- lonlat2UTM(st_coordinates(points))
  pts <- st_transform(points, st_crs(epsg_utm))
  if(type == "buffer"){
    message(paste0('Creating buffer within a radius of ', radius, ' km.'))
    radius <- radius*1000 # convert km to meters because unit of st_buffer should be meters (crs is transformed to utm, in case lon/lat it would have been degrees.).
    pols <- st_buffer(x = pts, dist = radius)
  }
  if(type == "voronoi" | type == "thiessen"){
    # Interpolate categorical data (e.g. family)
    # https://rspatial.org/raster/analysis/4-interpolation.html
    # https://r-spatial.github.io/sf/reference/geos_unary.html
    if (sum( (!is.null(country)) + (!is.null(continent)) ) > 1) {
      stop("Please supply either continent or continent, noth both")
    }
    country <- ne_countries(country = country, continent = continent, returnclass = "sf", scale = "medium")
    country <- st_transform(country, st_crs(epsg_utm))
    pols <- st_collection_extract(st_voronoi(do.call(c, st_geometry(pts))))
    # st_crs(pols) <- st_crs(pts)
    pols <- st_set_crs(x = pols, value = st_crs(pts))
    # match them to points:
    pts$pols <- pols[unlist(st_intersects(pts, pols))]
    pols <- st_set_geometry(pts, "pols")
    pols <- st_intersection(pols, country) # crop to country boundaries
  }

  # Convert back to WGS84
  pols <- st_transform(pols, crs = 4326)

  # Check
  # library(leaflet)
  #
  # leaflet() %>%
  #   addTiles() %>%
  #   addMeasure(primaryLengthUnit = "meters") %>%
  #   addMarkers(data = points) %>%
  #   addPolygons(data = pols)

  return(pols)
}

gs_geodist <- function(points = NULL, lines = NULL, fun = "great circle", return = "units", label = "name", radius = 100){
  # Perhaps split 'return' into 'class' and 'summary'???
  if (!require(units)) {install.packages('units')}
  library(units)
  fun <- tolower(str_replace_all(fun, "[[:punct:]]", " ")  )

  if(fun %in% c("gc", "crow", "great circle", "great circle distance", "haversine", "haversine distance")){
    if(!is.null(points) & is.null(lines)){
      if(!st_is_longlat(points)) {
        points <- st_transform(x = points, crs = "EPSG:4326")
      }
      geodist <- st_distance(x = points)
      geodist <- geodist %>% set_units(km) # convert with set_units because geodist %>% "/"(1000) would not keep the correct units class
      rownames(geodist) <- points[, label, drop = T]
      colnames(geodist) <- points[, label, drop = T]
      geodist <- round(geodist)
    } else if(!is.null(points) & !is.null(lines)){
      if(!st_is_longlat(points)) {
        points <- st_transform(x = points, crs = "EPSG:4326")
      }
      if(!st_is_longlat(lines)) {
        lines <- st_transform(x = lines, crs = "EPSG:4326")
      }
      nearest <- st_nearest_feature(x = points, y = lines)
      geodist <- st_distance(x = points, y = lines[nearest,], by_element = TRUE)
      geodist <- geodist %>% set_units(km)
      geodist <- round(geodist)
      if(return == "sf"){
        points[ , "dist"] <- geodist
        geodistsf <- points
      } else{names(geodist) <- points[, "name", drop = T]
      }


    }
  }

  if(fun %in% c("lc", "wolf", "least cost", "least cost distance")){
    # TO ADD:
    # gdistance R package: Van Etten 2017
    # topoDistance R package; Wang 2020
    # Least Cost Topographic Path: Taking into account both hanitat suitability and topography.
    # Least Cost Path: Only habitat suitability
    # Shortest topographic path: only topography.
    # Topographic distances account for the additional distance, beyond horizontal distance, imposed by topographic relief and, therefore, capture the full overland distance an organism must move between geographic locations.
  }

  if(fun %in% c("river", "fish", "riverdist", "river distance")){
    # This is distance along river, not to river (will be implemented in gs_datageo)
    # TO ADD:
    # https://cran.r-project.org/web/packages/riverdist/vignettes/riverdist_vignette.html
  }

  if(fun %in% c("resistance", "drunkard", "random walk", "commute distance")){
    # implemented in gdistance
    # TO ADD:
    # https://robbymarrotte.weebly.com/blog/running-circuitscape-in-r-windows-os
  }

  if(return == "units"){
    out <- geodist
    message("Matrix of class 'units' returned. Distances are in km.")
  }
  if(return == "sf"){
    out <- geodistsf
    message("Class 'sf' returned. Distances are in km.")
  }
  if(return == "dist" | return == "distmat"){
    out <- as.dist(geodist)
    message("Distance matrix returned (default). Distances are in km.")
  } else if(return == "matrix"){
    out <- as.matrix(geodist)
    message("Matrix returned. Distances are in km.")
  } else if(return == "graph"){
    out <- reshape2::melt(as.matrix(geodist), na.rm = TRUE)
    colnames(out)[1] <- "lang1"
    colnames(out)[2] <- "lang2"
    colnames(out)[3] <- "dist"
    message("Graph returned")
  }

  if(return == "mean"){
    totdist <- rowSums(as.matrix(geodist))
    out <- totdist / (nrow(geodist) - 1)
    message("Mean distance to all other points in the sample returned.")
  } else if(return == "radius"){
    out <- apply(geodist, 1, function(x) {
      sum(x < radius) - 1 # Subtract 1 to exclude the point itself
    })
    message(paste0("Number of points within radius a radius of ", radius, " (km) returned."))
  } else if(return == "nearest"){
    # Calculate nearest distance
    nn_dist <- apply(geodist, 1, function(x) {
      return(sort(x, partial = 2)[2])
    })
    # Get index for nearest distance
    nn_id <- apply(geodist, 1, function(x) { order(x, decreasing=F)[2] })
    out <- data.frame("nn" = sa$glottocode[nn_id], "nn_dist" = nn_dist)
  }
  return(out)
}

gs_map <- function(points = NULL, pols = NULL, map = "dynamic", colorby = "family_name", label = "name", ...){
  if (!require(tmap)) {install.packages('tmap')}
  library(tmap)
  if (!require(tmaptools)) {install.packages('tmaptools')}
  library(tmaptools)
  tmap_options(max.categories = 100)
  if(map == "dynamic"){
    tmap_mode("view")
    out <- tm_basemap("Esri.WorldTopoMap") +
      # tm_basemap("Esri.WorlGrayCanvas") +
      # tm_basemap("OpenStreetMap") +
      {if(!is_empty(pols))
        tm_shape(pols) +
          tm_polygons(id = label, col = colorby)} +
      {if(!is_empty(points))
        tm_shape(points) +
          tm_symbols(id = label, col = colorby, scale = .95, alpha = .85) }
  }

  if(map == "static"){
    if (!require(OpenStreetMap)) {install.packages('OpenStreetMap')}
    library(OpenStreetMap)
    if (!require(rJava)) {install.packages('rJava')}
    library(rJava)
    tmap_mode("plot")
    basemap <- read_osm(points, ext = 1.1)
    out <- tm_shape(basemap) + tm_rgb() +
      {if(!is_empty(pols))
        tm_shape(pols) +
          tm_polygons(col = colorby)} +
      {if(!is_empty(points))
        tm_shape(points) +
          tm_symbols(col = colorby, scale = .95, alpha = .85) } +
      {if(!is_empty(label)) tm_text(text = label, size = 0.75, auto.placement = TRUE)} +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_legend(legend.outside = TRUE)
  }

  if(map == "simple"){
    data("World") #tmap
    worldp <- st_transform(World, crs = st_crs(points))
    if(st_is_longlat(worldp) ){
      countries <- worldp[points, ]
    }

    if(!st_is_longlat(worldp) ){
      # Added to solve this error: Invalid number of points in LinearRing found 2 - must be 0 or >= 4.
      countries <- worldp[points, ]
      worldp$nc <- stringr::str_count(worldp$geometry, ",")
      worldp = filter(worldp, nc == 0 | nc >= 4)
      countries <- worldp[points, ]
    }

    tmap_mode("plot")
    out <- tm_shape(countries) +
      tm_polygons() +
      # tm_borders("grey", lwd = .5) +
      tm_graticules(col = "grey60") +
      tm_text("name", size = "AREA") +
      {if(!is_empty(pols))
        tm_shape(pols) +
          tm_polygons(id = label, col = colorby)} +
      {if(!is_empty(points))
        tm_shape(points) +
          tm_symbols(id = label, col = colorby, scale = .95, alpha = .85) } +
      tm_scale_bar(position = c("left", "bottom")) +
      tm_legend(legend.outside = TRUE)
  }

  if(map == "measure"){
    if (!require(leaflet)) {install.packages('leaflet')}
    library(leaflet)
    if(!is.null(points) & is.null(pols)){
      out <- leaflet() %>%
        addTiles() %>%
        addMeasure(primaryLengthUnit = "kilometers") %>%
        addMarkers(data = points)
    }
    if(is.null(points) & !is.null(pols)){
      out <- leaflet() %>%
        addTiles() %>%
        addMeasure(primaryLengthUnit = "kilometers") %>%
        addPolygons(data = pols)
    }
    if(!is.null(points) & !is.null(pols)){
      out <- leaflet() %>%
        addTiles() %>%
        addMeasure(primaryLengthUnit = "kilometers") %>%
        addMarkers(data = points) %>%
        addPolygons(data = pols)
    }

  }

  return(out)
}

gs_view <- function(...){
  data <- gs_data(...)
  data <- gs_upgrade(data = data, ...)
  points <- gs_filter(data = data, ...)
  out <- gs_map(points = points, ...)
  return(out)
}

gs_save <- function(object = NULL, filename = NULL){

  if((class(object) == "tmap")[1]){
    filename <- sub(pattern = "(.*)\\..*$", replacement = "\\1", filename)
    tmap_save(object, filename = filename)
  }
  if((class(object) == "sf")[1]){
    if (!require(tools)) {install.packages('tools')}
    library(tools)
    # if no file extension: gpkg
    if(file_ext(filename) == ""){
      st_write(obj = object, dsn = paste0(filename, ".gpkg"),
               append = FALSE)
    } else {
      st_write(obj = object, dsn = filename,
               append = FALSE)
    }
  }
}

# From https://github.com/SimonGreenhill/rcldf/blob/master/R/rcldf.R
# Function 'cldf' developed by Simon Greenhill
cldf <- function(mdpath) {
  # mdpath <- resolve_path(mdpath)
  dir <- dirname(mdpath)
  o <- structure(list(tables = list()), class = "cldf")
  o$metadata <- jsonlite::fromJSON(mdpath)
  o$name <- dir
  o$type <- o$metadata$`dc:conformsTo`

  # load sources
  o$sources <- tryCatch({ read_bib(dir, o$metadata$`dc:source`) })

  for (i in 1:nrow(o$metadata$tables)) {
    filename <- file.path(dir, o$metadata$tables[i, "url"])
    table <- get_tablename(o$metadata$tables[i, "url"])
    cols <- get_table_schema(o$metadata$tables[i, "tableSchema"]$columns)

    o[["tables"]][[table]] <- vroom::vroom(
      filename, delim=",", col_names = TRUE, col_types = cols$cols, quote = '"'
    )
  }
  o
}

# From Github Simon Greenhill
resolve_path <- function(path) {
  path <- base::normalizePath(path, mustWork = FALSE)
  if (file.exists(path) & endsWith(path, ".json")) {
    # given a metadata.json file
    mdfile <- path
  } else if (dir.exists(path)) {
    # given a dirname, try find the metadata file.
    mdfile <- list.files(path, "*.json", full.names = TRUE)
  } else if (!file.exists(path)) {
    stop(sprintf("Path %s does not exist", path))
  } else {
    stop(
      "Need either a metadata.json file or a directory with metadata.json"
    )
  }
  mdfile
}

# From Github Simon Greenhill
read_bib <- function(dir, bib="sources.bib"){
  if (is.null(bib)) return(NA)
  bib <- file.path(dir, bib)
  if (!file.exists(bib)) return(NA)
  bib2df::bib2df(bib)
}

## From https://geocompr.robinlovelace.net/reproj-geo-data.html
lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

gs_extract <- function(data = NULL, ...){
  if (!require(strex)) {install.packages('strex')}
  library(strex)

  # # Either supply object, or character.
  # if(is.character(data)){
  #   data <- gs_data(name = data, region = region, res = res)
  # } else if(is.object(data)){
  #   data <- data
  # }

  if(class(data)[1] == "RasterLayer" | class(data)[1] == "RasterStack"){
    out <- gs_extractras(data = data, ...)
  }

  if(class(data)[1] == "sf"){
    out <- gs_extractvec(data = data, ...)
  }

  return(out)
}

gs_extractras <- function(points = NULL, pols = NULL, data = NULL, add = T, radius = NULL, fun = NULL, ...){

  if(!is.null(radius)){
    # To add: check if data are in lat/lon (i.e. not projected), otherwise throw error/warning.
    cat(paste0('Extracting Values within a radius of ', radius, ' km.'))
    radius <- radius*1000 # convert km to meters because unit of raster::extract should be meters.
    if(is.null(fun)){
      stop('Please indicate how you want to summarize the values within the specified radius.
    Provide an argument to fun, as in fun = mean (e.g. median, min, max, sd, modal).
         Or specify user-defined function, as in fun = function(x){mean(x,na.rm=T)} ')
    }
  }

  if(!is_empty(points)){
    # Default is to add covariates to points after extraction
    extracted <- raster::extract(x = data, y = points, buffer = radius, fun = fun)
    if(add == T){
      points[,names(data)] <- extracted # optional TO ADD name_fun
    } else{
      points <- extracted
    }
  }
  # if(!is_empty(pols)){
  #   # Is redundant, because already implemented in raster::extract
  # }
  message("
  Data extracted")
  return(points)

}

gs_extractvec <- function(points = NULL, pols = NULL, data = NULL, add = T, radius = NULL, fun = NULL, ...){

  # Either supply object, or character.
  if(is.character(data)){
    data <- gs_data(name = data, ...)
  } else if(is.object(data)){
    data <- data
  }

  if(!is_empty(points) & is.null(radius)){
    extracted <- sf::st_intersection(x = points, y = data)
    if(add == T){
      # Default is to add covariates to points after extraction
      points <- extracted # optional TO ADD name_fun
    } else{
      points <- extracted[,names(st_drop_geometry(data))]
    }
  }

  if(!is_empty(points) & !is.null(radius)){

    # To add: check if data are in lat/lon (i.e. not projected), otherwise throw error/warning.
    if(is.null(fun)){
      stop('Please indicate how you want to summarize the values within the specified radius.
    Currently, only length is implemented for vector data ')
    }

    # alternative approach: st_join(x = points, y = rivers, join = st_is_within_distance, dist = 10)
    # convert to equidistant world projection: https://epsg.io/54032
    pols  <- gs_polygonize(points = points, type = "buffer", radius = radius)

    if(fun == "totlength"){
      int <- st_intersects(x = pols, y = data)
      int <- lengths(int) > 0
      extracted <- sf::st_intersection(x = pols, y = data)
      extracted$len <- st_length(extracted)
      extracted <- extracted %>% group_by(glottocode) %>% summarise(totlength = sum(len))
      extracted <- st_drop_geometry(extracted)
      extracted$totlength <- extracted$totlength %>% units::set_units(km)
      extracted <- round(extracted$totlength)

      tmpvec <- rep(NA, nrow(pols))
      tmpvec <- ifelse(int, extracted, 0)
      extracted <- tmpvec

    }
    if(add == T){
      # Default is to add covariates to points after extraction
      points$totlength <- extracted # optional TO ADD name_fun
    } else{
      points <- extracted
    }
  }


  # ecoregion proportion
  # # https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r

  #   # To be implemented: If polygons are provided
  # if(!is_empty(pols)){
  # inter <- st_intersection(x = pols, y = data) # https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r
  # }

  # To be implemented: if radius argument is provided, create buffer.
  # if(!is.null(radius)){
  #   # To add: check if data are in lat/lon (i.e. not projected), otherwise throw error/warning.
  #   message(paste0('Extracting Values within a radius of ', radius, ' km.'))
  #   buffer <- gs_polygonize(points = points, type = "buffer", radius = radius)
  # #   if(is.null(fun)){
  # #   #   stop('Please indicate how you want to summarize the values within the specified radius.
  # #   # Provide an argument to fun, as in fun = mean (e.g. median, min, max, sd, modal).
  # #   #      Or specify user-defined function, as in fun = function(x){mean(x,na.rm=T)} ')
  # #   }
  # }

  #
  message("
  Data extracted")
  return(points)

}

gs_datageo <- function(name = NULL, region = NULL, res = NULL, path = NULL, selection = "default"){

  if (!require(raster)) {install.packages('raster')}
  library(raster)
  if (!require(sp)) {install.packages('sp')}
  library(sp)
  if (!require(strex)) {install.packages('strex')}
  library(strex)

  if(!is.null(region)){match_arg(region, c("Global", "South America", "Amazon basin"))}
  if(!is.null(res)){match_arg(res, c("1km", "250m", "90m"))}

  # Layers ###
  if(!is.null(path)){
    if(file.exists(path)){
      if(tools::file_ext(path)[1] == "shp"){
        out <- st_read(path)
        cat(paste("Loading shapefile:", name))
      } else{# Assume raster
        out <- raster::raster(path)
        cat(paste("Loading raster:", name, "at", res, "resolution for:", region_))
      }
    }
  }

  if(name == "roughness"){
    if(res == "250m" & region == "Global"){file <- "dtm_roughness_merit.dem_m_250m_s0..0cm_2018_v1.0.tif"}
    path <- gs_geopath(pathgroup = "Topography/Geomorpho90m", res = res, region = region, file = file)
    ras <- raster::raster(path)
    cat(paste("Loading raster:", name, "at", res, "resolution for:", region, "\n"))
    if(!is.null(ras)){
      names(ras) <- name
      out <- ras
      cat(paste0("Roughness merit dem loaded (", file, ")"))
    }
  }

  if(name == "elevation"){
    if(res == "250m" & region == "South America"){file <- "SRTM250mSA.tif"}
    path <- gs_geopath(pathgroup = "Topography/SRTM", res = res, region = region, file = file)
    ras <- raster::raster(path)
    cat(paste("Loading raster:", name, "at", res, "resolution for:", region, "\n"))
    if(!is.null(ras)){
      names(ras) <- name
      out <- ras
      cat(paste0("SRTM data loaded (", file, ")"))
    }
  }

  if(name == "nutrients"){
    if(res == "1km" & region == "Global"){file <- "sq1.asc"} else {file <- ""}
    path <- gs_geopath(pathgroup = "Soils/HWSD", res = res, region = region, file = file)
    ras <- raster::raster(path)
    cat(paste("Loading raster:", name, "at", res, "resolution for:", region, "\n"))
    if(!is.null(ras)){
      crs(ras) <- CRS('+init=EPSG:4326')
      names(ras) <- name
      out <- ras
      cat(paste0("Nutrient availability loaded (", file, ")"))
      cat(paste("
    Legend:
    1 = no significant constraint
    2 = moderate constraints
    3 = severe constraints
    4 = very severe constraints
    5 = mainly non-soil
    6 = permafrost area
    7 = water bodies
              \n \n"))
    }
  }

  if(name == "worldclim"){
    if(res == "1km" & region == "South America"){file <- paste("wc2.0_bio_30s_", sprintf("%02d", seq(1,19)), '.tif', sep='')}
    paths <- gs_geopath(pathgroup = "Climate/WORLDCLIM2", res = "1km", region = "South America", file = file)
    stack <- raster::stack(paths)
    names(stack) <- paste0("bio", sprintf("%02d", seq(1,19)))
    message(paste("Loading raster:", name, "at", res, "resolution for:", region))
    if (region == "Global"){
      stack <- raster::getData(name = "worldclim", var = "bio", res = 10)
    }
    if(selection == "default"){
      ras <- stack
    } else {
      ras <- stack[[selection]]
    }
    if(!is.null(ras)){
      out <- ras
      cat(paste("WorldClim 2.0 data loaded (", file, ") \n"))
    }
  }

  if(name == "ecoregion"){
    file <- "wwf_terr_ecos.shp"
    path <- gs_geopath(pathgroup = "Biodiversity/wwf_terr_ecos", file = file)
    vec <- st_read(path)
    cat(paste("Loading shapefile:", name))
    if(!is.null(vec)){
      if(selection == "default"){
        vec <- vec[, "ECO_NAME"]
        colnames(vec)[1] <- name
      } else if(selection != "default" & is.character(selection)){
        vec <- vec[, selection]
      } else if(selection == "all" | is.null(selection)){
        vec <- vec
      }
      out <- vec
      cat(paste0("Terrestrial Ecoregions of the World loaded (", file, ")"))
    }
  }

  if(name == "rivervect"){
    if(region == "South America"){file <- "South America_constant_Dd.gpkg"}
    path <- gs_geopath(pathgroup = "Hydrography/drainage density", region = region, file = file)
    vec <- st_read(path)
    cat(paste("Loading shapefile:", name))
    if(!is.null(vec)){
      if(selection == "default"){
        vec <- vec
      } else if(selection != "default" & is.character(selection)){
        vec <- vec[, selection]
      } else if(selection == "all" | is.null(selection)){
        vec <- vec
      }
      out <- vec
      cat(paste0("River network constant Dd loaded (", file, ") for region: ", region))
    }
  }

  if(name == "riverras"){
    # TO DO: reclass data (using raster::reclassify), different possibilities. For example reclass all pixels to either water or land (to calculate number of water pixels in buffer, not accurate if riverwidth is smaller than cell size). Or keep only centerlines (to calculate sum of river width in focal area, although then line features might be better).

    if(res == "90m" & region == "South America"){file <- "merit_width_SA_90m.tif"} else {file <- ""}
    path <- gs_geopath(pathgroup = "Hydrography/MERIT Hydro/river width", res = res, region = region, file = file)
    ras <- raster::raster(path)
    cat(paste("Loading raster:", name, "at", res, "resolution for:", region))
    if(!is.null(ras)){
      names(ras) <- name
      message(paste0("River width loaded (", file, ") for region:", region))
      out <- ras
      cat(paste("The values larger than 0 represents the river width at the channel centerlines.
      The value -1 represents non-centerline water pixels, and the value 0 corresponds to the non-water pixels.
The undefined pixels (oceans) are represented by the value -9999."))
    }
  }

  return(out)
}

gs_unpack <- function(path = NULL){
  ftar <- list.files(path = path, pattern = ".tar", full.names = TRUE)
  if(!purrr::is_empty(ftar)){
    lapply(ftar, untar, exdir = path)
    f <- ftar
    ext <- ".tar"
  }

  fzip <- list.files(path = path, pattern = ".zip", full.names = TRUE)
  if(!purrr::is_empty(fzip)){
    lapply(fzip, unzip, exdir = path)
    f <- fzip
    ext <- ".zip"
  }

  message(paste(length(f), ext, " files unpacked to:", path))
}

gs_mosaic <- function(path = NULL, outfile = "mosaic.tif", overwrite = FALSE, na.rm = TRUE, subdirs = TRUE){
  # Mosaic tiles
  if(xfun::file_ext(outfile) == ""){
    outfile <- paste0(outfile, ".tif")
  }

  tifls <- list.files(path = path, pattern = ".tif", recursive = subdirs)
  tifls <- paste(path, tifls, sep = "/")

  rasls <- lapply(tifls, raster::raster)

  rasls$fun <- mean
  names(rasls)[1:2] <- c('x', 'y')
  rasls$filename <- paste(path, outfile, sep = "/")
  rasls$na.rm <- na.rm
  rasls$overwrite <- overwrite

  message(paste0("Creating mosaic from ", length(tifls), " .tif files. Output (", outfile, ") stored in: ", path))
  do.call(raster::mosaic, rasls)
}

gs_mergevec <- function(paths = NULL, selection = NULL, outfile = NULL, overwrite = FALSE){
  # Merge multiple shapefiles
  layers <- lapply(paths, st_read)
  out <- sf::st_as_sf(data.table::rbindlist(layers))         # MUCH faster than: layers <- do.call(rbind, layers)
  message(paste("Loading and merging", length(paths), " files."))
  if(!is.null(selection)){out <- out[,selection]}
  if(!is.null(outfile)){gs_save(object = out, filename = outfile)}
  return(out)
}





gs_geopath <- function(pathgroup = NULL, res = NULL, region = NULL, file = NULL){

  if (!require(stringi)) {install.packages('stringi')}
  library(stringi)
  globpath <- "D:/Global"
  path <- stringi::stri_join(globpath, pathgroup, res, region, file, sep = "/", ignore_null = TRUE)

  if(!file.exists(path)[1]){message(paste("Not found:", path))}

  # resolutions <- paste(list.dirs(paste(globpath, pathgroup, sep = "/") , full.names = F, recursive = F), collapse = ", ")
  # message(paste0("The following resolutions are available in ", pathgroup, ": ", resolutions))

  data <- list.dirs(paste(globpath, pathgroup, sep = "/") , full.names = F, recursive = T)
  data <- data[grepl('/', data)]
  cat(paste0("The following data are available in ", pathgroup, ": \n "))
  cat(paste(data, sep = "\n"))
  cat("\n")

  #
  # regions <- paste(list.dirs(paste(globpath, pathgroup, res, sep = "/") , full.names = F, recursive = F), collapse = ", ")
  # message(paste0("At the specified resolution (", res, "), ", pathgroup, " contains the following region(s): ", regions))


  return(path)


}




# VISUALIZATIONS OF GEO AND LANG ------------------------------------------

gs_viewnmds <- function(nmds_res, filename = NULL, dist = NULL, view = "nmds", rm.na = TRUE){

  nmds <- nmds_res[[1]]
  nmds_res <- nmds_res[[2]]

  if(view == "scree"){
    conmat <- as.matrix(dist)
    goeveg::dimcheckMDS(matrix = conmat)
  }

  if(view == "stress"){
    stressplot(nmds_res) # large scatter around line? Original dissimilarities not well preserved in reduced number of dimensions
  }


  if(view == "nmds"){
    if(nmds$ndim == 2){
      # V3: compare families and yucuna and tanimuka
      p <- ggplot(data = nmds_res, aes(colour = groups)) +
        # ggpubr::stat_chull(aes(x=NMDS1,y=NMDS2),show.legend = F, alpha = 0.8, size = 1, linetype = 2) +
        stat_ellipse(type="t", aes(x=NMDS1,y=NMDS2),level = 0.95, show.legend = F, alpha = 0.5, size = 0.75, linetype = 2) +
        geom_point(aes(x=NMDS1,y=NMDS2,size = groups), alpha = 0.55) + # add the point markers
        scale_colour_manual(values=c("Arawakan" = "tomato1", "Yucuna" = "red3", "Tucanoan" = "royalblue", "Tanimuca" = "navy")) +
        scale_size_manual(values=c(2,5,2,5)) +
        coord_equal() +
        labs(title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"), x = "NMDS1", y = "NMDS2") +
        theme_bw()

      ggsave(plot = p, filename = filename)

      p
    }

    if(nmds$ndim == 3){
      nmdsplot <- plot_ly(data = nmds_res, x = ~NMDS1, y = ~NMDS2, z = ~NMDS3,
                          type="scatter3d", mode="markers", color = ~groups,
                          colors = c("Arawakan" = "tomato1", "Yucuna" = "red3",
                                     "Tucanoan" = "royalblue", "Tanimuca" = "navy"), hoverinfo = "text",
                          text = rownames(nmds_res))

      nmdsplot <- nmdsplot %>% layout(
        title = paste0("NMDS (k = ", nmds$ndim, ", stress = ", round(nmds$stress, 2), ")"),
        scene = list(
          xaxis = list(title = "NMDS1"),
          yaxis = list(title = "NMDS2"),
          zaxis = list(title = "NMDS3")
        ))



      nmdsplot
      saveWidget(nmdsplot, title = "SAPPHIRE - NMDS TAME 3d", "nmdsplot.html")      }

  }



}


