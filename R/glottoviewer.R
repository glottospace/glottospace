
glottomap <- function(){
  if(is.null(label)){label <- "glottocode"}
}

# glottopoints <- glottofilter(continent = "South America")
# glottopols <- points2pols(glottopoints, interpolation = "voronoi", continent = "South America")
# glottomap_dynamic(glottodata = glottopols, label = "glottocode", colorby = "family_size_rank")
glottomap_dynamic <- function(glottodata, label, colorby = NULL){
    suppressMessages(tmap::tmap_mode("view"))

    tmap::tm_basemap("Esri.WorldTopoMap") +
        {if(is_polygon(glottodata))
        tmap::tm_shape(glottodata) +
          tmap::tm_polygons(id = label, col = colorby)} +
      {if(is_point(glottodata))
        tmap::tm_shape(glottodata) +
          tmap::tm_symbols(id = label, col = colorby, scale = .95, alpha = .85) }
  }

#
# glottoviewer <- function(...){
#   data <- gs_data(...)
#   data <- gs_upgrade(data = data, ...)
#   points <- gs_filter(data = data, ...)
#   out <- gs_map(points = points, ...)
#   return(out)
# }
#
# # TODO: maybe add raster::plot(rasvec) ?
#
# glottomap <- function(points = NULL, pols = NULL, map = "dynamic", colorby = "family_name", label = "name", ...){
#   if (!require(tmap)) {install.packages('tmap')}
#   library(tmap)
#   if (!require(tmaptools)) {install.packages('tmaptools')}
#   library(tmaptools)
#   tmap_options(max.categories = 100)
#   if(map == "dynamic"){
#     tmap_mode("view")
#     out <- tm_basemap("Esri.WorldTopoMap") +
#       # tm_basemap("Esri.WorlGrayCanvas") +
#       # tm_basemap("OpenStreetMap") +
#       {if(!is_empty(pols))
#         tm_shape(pols) +
#           tm_polygons(id = label, col = colorby)} +
#       {if(!is_empty(points))
#         tm_shape(points) +
#           tm_symbols(id = label, col = colorby, scale = .95, alpha = .85) }
#   }
#
#   if(map == "static"){
#     if (!require(OpenStreetMap)) {install.packages('OpenStreetMap')}
#     library(OpenStreetMap)
#     if (!require(rJava)) {install.packages('rJava')}
#     library(rJava)
#     tmap_mode("plot")
#     basemap <- read_osm(points, ext = 1.1)
#     out <- tm_shape(basemap) + tm_rgb() +
#       {if(!is_empty(pols))
#         tm_shape(pols) +
#           tm_polygons(col = colorby)} +
#       {if(!is_empty(points))
#         tm_shape(points) +
#           tm_symbols(col = colorby, scale = .95, alpha = .85) } +
#       {if(!is_empty(label)) tm_text(text = label, size = 0.75, auto.placement = TRUE)} +
#       tm_scale_bar(position = c("left", "bottom")) +
#       tm_legend(legend.outside = TRUE)
#   }
#
#   if(map == "simple"){
#     data("World") #tmap
#     worldp <- st_transform(World, crs = st_crs(points))
#     if(st_is_longlat(worldp) ){
#       countries <- worldp[points, ]
#     }
#
#     if(!st_is_longlat(worldp) ){
#       # Added to solve this error: Invalid number of points in LinearRing found 2 - must be 0 or >= 4.
#       countries <- worldp[points, ]
#       worldp$nc <- stringr::str_count(worldp$geometry, ",")
#       worldp = filter(worldp, nc == 0 | nc >= 4)
#       countries <- worldp[points, ]
#     }
#
#     tmap_mode("plot")
#     out <- tm_shape(countries) +
#       tm_polygons() +
#       # tm_borders("grey", lwd = .5) +
#       tm_graticules(col = "grey60") +
#       tm_text("name", size = "AREA") +
#       {if(!is_empty(pols))
#         tm_shape(pols) +
#           tm_polygons(id = label, col = colorby)} +
#       {if(!is_empty(points))
#         tm_shape(points) +
#           tm_symbols(id = label, col = colorby, scale = .95, alpha = .85) } +
#       tm_scale_bar(position = c("left", "bottom")) +
#       tm_legend(legend.outside = TRUE)
#   }
#
#   if(map == "measure"){
#     if (!require(leaflet)) {install.packages('leaflet')}
#     library(leaflet)
#     if(!is.null(points) & is.null(pols)){
#       out <- leaflet() %>%
#         addTiles() %>%
#         addMeasure(primaryLengthUnit = "kilometers") %>%
#         addMarkers(data = points)
#     }
#     if(is.null(points) & !is.null(pols)){
#       out <- leaflet() %>%
#         addTiles() %>%
#         addMeasure(primaryLengthUnit = "kilometers") %>%
#         addPolygons(data = pols)
#     }
#     if(!is.null(points) & !is.null(pols)){
#       out <- leaflet() %>%
#         addTiles() %>%
#         addMeasure(primaryLengthUnit = "kilometers") %>%
#         addMarkers(data = points) %>%
#         addPolygons(data = pols)
#     }
#
#   }
#
#   return(out)
# }
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
