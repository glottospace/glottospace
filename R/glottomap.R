
#' glottomap
#'
#' Create dynamic, static and interactive maps from glottodata
#'
#' @param glottodata User-provided glottodata (spatial)
#' @param colorby column name or index to be used to color features (optional)
#' @param label Column name or index to be used to label features (optional)
#' @param type One of: "static", "dynamic", or "interactive"
#'
#' @return
#' @export
#'
#' @examples
#' glottopoints <- glottofilter(continent = "South America")
#' glottopols <- points2pols(glottopoints, method = "voronoi", continent = "South America")
#' glottomap(glottodata = glottopols, label = "glottocode", colorby = "family_size_rank")
glottomap <- function(glottodata, colorby = NULL, label = NULL, type = NULL){
  if(is.null(type)){type <- "dynamic"}

  if(type == "dynamic"){
    map <- glottomap_dynamic(glottodata = glottodata, label = label, colorby = colorby)
  }

  if(type == "static"){
    map <- glottomap_static(glottodata = glottodata, label = label, colorby = colorby)
  }
return(map)

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
#' Say something about choice for equal-area projection. Refer to McNew, Derungs, Moran ()
#'
#' @param glottodata User-provided glottodata
#' @param color column name or index to be used to color features (optional), or a color "black"
#' @param label Column name or index to be used to label features (optional)
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#' glottodata <- glottofilter(continent = "South America")
#' glottodata <- glottofilter(country = "Netherlands")
#' glottomap_static(glottodata)
glottomap_static <- function(glottodata, label, color){
  suppressMessages(tmap::tmap_mode("plot"))


  # # World tmap: perhaps add topography? https://cran.microsoft.com/snapshot/2019-01-06/web/packages/tmap/vignettes/tmap-getstarted.html
  # data("land")

  # rnaturalearth: WORKS!! https://github.com/r-spatial/sf/issues/541
  basemap <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  wrld_wrap <- sf::st_wrap_dateline(basemap, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
  wrld_proj <- sf::st_transform(wrld_wrap, "+proj=eck4")
  wrld_proj <- sf::st_make_valid(wrld_proj)
  # plot(st_geometry(wrld_proj), col="transparent")

  glottodata_proj <- sf::st_transform(glottodata, crs = "+proj=eck4")

  bbox <- sf::st_bbox(glottodata_proj)
  bbox <- bbox_expand(bbox, f = 0.1)

  tmap::tm_shape(wrld_proj, bbox = bbox) + tmap::tm_fill(col = "gray60", alpha = .40) + tmap::tm_borders(lwd=1.2) +
    {if(is_polygon(glottodata))
      tmap::tm_shape(glottodata) +
        tmap::tm_polygons(col = color)} +
    {if(is_point(glottodata))
      tmap::tm_shape(glottodata) +
        tmap::tm_symbols(col = color, scale = .65, alpha = .85) } +
    {if(!purrr::is_empty(label)) tmap::tm_text(text = label, size = 0.75, auto.placement = TRUE)} +
    tmap::tm_legend(legend.outside = TRUE) + tmap::tm_layout(bg.color = "skyblue")
}


# glottomap_static_topographic <- function(){
#   data("land")
#   landp <- sf::st_transform(land, crs = st_crs(glottodata))
#   gdbbox <- sf::st_bbox(glottodata)
#   landp <- sf::st_crop(landp, gdbbox)
#
#   tmap::tm_shape(landp) + tmap::tm_raster("elevation") +
#     {if(is_polygon(glottodata))
#       tmap::tm_shape(glottodata) +
#         tmap::tm_polygons(col = colorby)} +
#     {if(is_point(glottodata))
#       tmap::tm_shape(glottodata) +
#         tmap::tm_symbols(col = colorby, scale = .95, alpha = .85) } +
#     {if(!purrr::is_empty(label)) tmap::tm_text(text = label, size = 0.75, auto.placement = TRUE)} +
#     tmap::tm_scale_bar(position = c("left", "bottom")) +
#     tmap::tm_legend(legend.outside = TRUE)
#
# }

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
