
#' glottomap
#'
#' Create dynamic, static and interactive maps from glottodata
#'
#' @param glottodata User-provided glottodata (spatial)
#' @param colorby column name or index to be used to color features (optional)
#' @param label Column name or index to be used to label features (optional)
#' @param type One of: "static", "dynamic", "interactive", or "simple"
#'
#' @return
#' @export
#'
#' @examples
#' glottopoints <- glottofilter(continent = "South America")
#' glottopols <- points2pols(glottopoints, interpolation = "voronoi", continent = "South America")
#' glottomap_dynamic(glottodata = glottopols, label = "glottocode", colorby = "family_size_rank")
glottomap <- function(glottodata, colorby = NULL, label = NULL, type = NULL){
  if(is.null(type)){type <- "dynamic"}
  if(is.null(label)){label <- "glottocode"}
  if(is.null(colorby)){colorby <- "family_name"}

  if(type == "dynamic"){
    glottomap_dynamic(glottodata = glottodata, label = label, colorby = colorby)
  }

}

#' glottomap_dynamic
#'
#' @param glottodata User-provided glottodata
#' @param colorby column name or index to be used to color features (optional)
#' @param label Column name or index to be used to label features (optional)
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#' glottomap_dynamic(glottodata)
glottomap_dynamic <- function(glottodata, label, colorby){
    suppressMessages(tmap::tmap_mode("view"))

    tmap::tm_basemap("Esri.WorldTopoMap") +
        {if(is_polygon(glottodata))
        tmap::tm_shape(glottodata) +
          tmap::tm_polygons(id = label, col = colorby)} +
      {if(is_point(glottodata))
        tmap::tm_shape(glottodata) +
          tmap::tm_symbols(id = label, col = colorby, scale = .95, alpha = .85) }
  }

#' glottomap_static
#'
#' @param glottodata User-provided glottodata
#' @param colorby column name or index to be used to color features (optional)
#' @param label Column name or index to be used to label features (optional)
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#' glottomap_dynamic(glottodata)
glottomap_static <- function(glottodata, label, colorby){
  suppressMessages(tmap::tmap_mode("plot"))

      basemap <- OpenStreetMap::read_osm(points, ext = 1.1)
      out <- tmap::tm_shape(basemap) + tmap::tm_rgb() +
        {if(is_polygon(glottodata))
          tmap::tm_shape(glottodata) +
            tmap::tm_polygons(col = colorby)} +
        {if(is_point(glottodata))
          tmap::tm_shape(glottodata) +
            tmap::tm_symbols(col = colorby, scale = .95, alpha = .85) } +
        {if(!is_empty(label)) tmap::tm_text(text = label, size = 0.75, auto.placement = TRUE)} +
        tmap::tm_scale_bar(position = c("left", "bottom")) +
        tmap::tm_legend(legend.outside = TRUE)
}

#' glottomap_geodata
#'
#' @param geodata
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#' glottomap_geodata(geodata)
glottomap_geodata <- function(geodata){
  if(is_raster(geodata) ){
    raster::plot(geodata)
  }
  if(is_sf(geodata) ){
    sf::plot(geodata)
  }
}



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
