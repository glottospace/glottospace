
#' glottomap
#'
#' Create dynamic, static and interactive maps from glottodata.
#'
#' @param glottodata Optional, user-provided glottodata. In case no glottodata is provided, you can pass arguments directly to glottofilter.
#' @param color column name or index to be used to color features (optional)
#' @param label Column name or index to be used to label features (optional)
#' @param type One of: "static", "dynamic", or "interactive". Defaults to
#'   "static" if nothing is provided.
#' @param ptsize Size of points between 0 and 1
#' @param lbsize Size of labels between 0 and 1
#' @param transparency Transparency of points between 0 (very transparent) and 1 (not transparent)
#' @param ... Arguments to pass to glottofilter in case glottodata is empty
#' @family <glottomap>
#' @seealso geomap
#' @return
#' @export
#'
#' @examples
#' glottomap(country = "Netherlands")
#'
#' glottopoints <- glottofilter(continent = "South America")
#' glottopols <- glottospace(glottopoints, method = "voronoi", continent = "South America")
#' glottomap(glottodata = glottopols, color = "family_size_rank")
#'
#' glottodata <- glottoget_remote()
#' families <- glottodata %>% dplyr::count(family_name, sort = TRUE)
#'
#' # highlight 10 largest families:
#' glottodata <- glottospotlight(glottodata = glottodata, spotcol =
#' "family_name", spotlight = families$family_name[1:10], spotcontrast = "family_name", bgcontrast = "family_name")
#'
#' # Or, place 10 largest families in background
#' glottodata <- glottospotlight(glottodata = glottodata, spotcol =
#' "family_name", spotlight = families$family_name[-c(1:10)], spotcontrast = "family_name", bgcontrast = "family_name")
#' glottomap(glottodata, color = "color")
glottomap <- function(glottodata = NULL, color = NULL, label = NULL, type = NULL, ptsize = NULL, transparency = NULL, lbsize = NULL, ...){
  if(is.null(type)){type <- "static"}

  if(is.null(glottodata)){
    glottodata <- glottofilter(...)
    if(mapview::npts(glottodata) == 1 & type == "static"){ #added to solve issue with countries that are not polygons in naturalearthdata (they consist of a single point)
      # glottodata <- sf::st_buffer(glottodata, dist = 0)
      type <- "dynamic"
      message("The country you are trying to plot is too small for a static map, returning a dynamic map instead.")
    }

  }
  if(is.null(ptsize)){ptsize <- 0.35}
  if(is.null(lbsize)){lbsize <- 0.75}
  if(is.null(transparency)){transparency <- 0.65}
  if(!is_sf(glottodata) ) {glottodata <- join_glottospace(glottodata)}


  if(is.null(color)){color <- "black"}

  if(type == "dynamic"){
    map <- glottomap_dynamic(glottodata = glottodata, label = label, color = color, ptsize = ptsize, transparency = transparency)
  }

  if(type == "static"){
    map <- glottomap_static(glottodata = glottodata, label = label, color = color, ptsize = ptsize, transparency = transparency)
  }
return(map)

}

#' Create a dynamic map with glottodata
#'
#'
#'
#' @param glottodata User-provided glottodata (either glottopoints or glottopols)
#' @param label Column name or index to be used to label features (optional)
#' @param color Column name or index to be used to color features (optional), or a color "black"
#' @param ptsize Size of points between 0 and 1
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#' glottomap_dynamic(glottodata)
glottomap_dynamic <- function(glottodata, label, color, ptsize, transparency){
    suppressMessages(tmap::tmap_mode("view"))

    tmap::tm_basemap("Esri.WorldTopoMap") +
        {if(is_polygon(glottodata))
        tmap::tm_shape(glottodata) +
          tmap::tm_polygons(id = label, col = color)} +
      {if(is_point(glottodata))
        tmap::tm_shape(glottodata) +
          tmap::tm_symbols(id = label, col = color, scale = ptsize, alpha = transparency) }
  }

#' Create a static map with glottodata
#'
#' This function returns a static map with glottodata. Data is projected using the equal-area Eckert IV projection (following McNew et al. 2018). See \url{https://en.wikipedia.org/wiki/Eckert_IV_projection}.
#'
#' @param glottodata User-provided glottodata (either glottopoints or glottopols)
#' @param color column name or index to be used to color features (optional), or a color "black"
#' @param label Column name or index to be used to label features (optional)
#' @param ptsize Point size between 0 and 1
#' @param lbsize Label size between 0 an 1
#'
#' @return
#' @keywords internal
#' @export
#'
#' @examples
#' glottodata <- glottofilter(continent = "South America")
#' glottodata <- glottofilter(country = "Netherlands")
#' glottomap_static(glottodata)
glottomap_static <- function(glottodata, label, color, ptsize, lbsize, transparency){
  suppressMessages(tmap::tmap_mode("plot"))

  basemap <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  wrld_wrap <- sf::st_wrap_dateline(basemap, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
  wrld_proj <- sf::st_transform(wrld_wrap, "+proj=eck4")
  wrld_proj <- sf::st_make_valid(wrld_proj)
  wrld_proj <- sf::st_geometry(wrld_proj)

  glottodata <- sf::st_make_valid(glottodata)
  glottodata_wrap <- sf::st_wrap_dateline(glottodata, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
  glottodata_proj <- sf::st_transform(glottodata_wrap, crs = "+proj=eck4")

  bbox <- sf::st_bbox(glottodata_proj)
  bboxe <- bbox_expand(bbox, f = 0.1)
  wrld_projbb <- sf::st_crop(wrld_proj, bboxe)

  tmap::tm_shape(wrld_projbb) + tmap::tm_fill(col = "white", alpha = 1) + tmap::tm_borders(lwd=1.2) +
    {if(is_polygon(glottodata_proj))
      tmap::tm_shape(glottodata_proj) +
        tmap::tm_polygons(col = color)} +
    {if(is_point(glottodata_proj))
      tmap::tm_shape(glottodata_proj) +
        tmap::tm_symbols(col = color, scale = ptsize, alpha = transparency) } +
    {if(!purrr::is_empty(label)) tmap::tm_text(text = label, size = lbsize, auto.placement = TRUE)} +
    tmap::tm_legend(legend.outside = TRUE) + tmap::tm_layout(bg.color = "grey85", inner.margins = c(0,0,0,0))
}



#' Map environmental data
#'
#' @param geodata
#'
#' @return
#' @keywords internal
#' @export
#' @family <geodata><glottomap><geotools>
#'
#' @examples
#' nl <- geoget(download = "elevation", country = c("Netherlands"))
#' plotgeodata(nl)
geomap <- function(geodata){
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
