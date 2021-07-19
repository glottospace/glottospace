
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


