
#' Create static and dynamic maps from glottodata
#'
#' Create static and dynamic maps from glottodata
#'
#' @param glottodata Optional, user-provided glottodata. In case no glottodata is provided, you can pass arguments directly to glottofilter.
#' @param color glottovar, column name, or column index to be used to color features (optional). Run glottovars() to see glottovars
#' @param label glottovar, column name, or column index to be used to label features (optional). Run glottovars() to see glottovars
#' @param type One of: "static" or "dynamic". Default is "static".
#' @param ptsize Size of points between 0 and 1
#' @param lbsize Size of labels between 0 and 1
#' @param alpha Transparency of points between 0 (very transparent) and 1 (not transparent)
#' @param palette Color palette, see glottocolpal("all") for possible options, and run glottocolpal("turbo") to see what it looks like (replace it with palette name).
#' Alternatively, you could also run tmaptools::palette_explorer(), RColorBrewer::display.brewer.all(), ?viridisLite::viridis, or scales::show_col(viridisLite::viridis(n=20))
#' @param rivers Do you want to plot rivers (only for static maps)?
#' @param nclass Preferred number of classes (default is 5)
#' @param numcat Do numbers represent categories? For example, if your dataset consists of 0 and 1, you might want to set this to TRUE.
#' @param crs Coordinate Reference System (only for static maps). Default is World Eckert IV (https://epsg.io/54012)
#' @param filename Optional filename if you want to save resulting map
#' @param ... Additional parameters to glottofilter
#'
#' @family <glottomap>
#' @return
#' @export
#'
#' @examples
#' glottomap(country = "Netherlands")
#'
#' glottopoints <- glottofilter(continent = "South America")
#' glottopols <- glottospace(glottopoints, method = "voronoi", continent = "South America")
#' glottomap(glottodata = glottopols, color = "family_size_rank")
#' glottomap(glottodata = glottopols, color = "family", palette = "turbo", type = "dynamic", label = "name")
#'
#' glottodata <- glottoget()
#' families <- glottodata %>% dplyr::count(family, sort = TRUE)
#'
#' # highlight 10 largest families:
#' glottodata <- glottospotlight(glottodata = glottodata, spotcol =
#' "family", spotlight = families$family[1:10], spotcontrast = "family", bgcontrast = "family")
#'
#' # Or, place 10 largest families in background
#' glottodata <- glottospotlight(glottodata = glottodata, spotcol =
#' "family", spotlight = families$family[-c(1:10)], spotcontrast = "family", bgcontrast = "family")
#' glottomap(glottodata, color = "color")
glottomap <- function(glottodata = NULL, color = NULL, label = NULL, type = NULL, ptsize = NULL, alpha = NULL, lbsize = NULL, palette = NULL, rivers = FALSE, nclass = NULL, numcat = FALSE, filename = NULL, crs = NULL, shiftrussia = FALSE, ...){
  palette <- glottocolpal(palette = palette)
  if(is.null(type)){type <- "static"}

  if(is.null(glottodata)){
    glottodata <- glottofilter(...)
    if(mapview::npts(glottodata) == 1 & type == "static"){ #added to solve issue with countries that are not polygons in naturalearthdata (they consist of a single point)
      # glottodata <- sf::st_buffer(glottodata, dist = 0)
      type <- "dynamic"
      message("The country you are trying to plot is too small for a static map, returning a dynamic map instead.")
    }

  }

  if(!is.null(color) ){
    nrcat <- nrow(unique(glottosimplify(glottodata[,color])))
    if(nrcat > 30){
      tmap::tmap_options(max.categories = nrcat)
    }
  }

  if(is.null(lbsize) & type == "static"){lbsize <- 0.75}
  if(!is.null(lbsize) & type == "dynamic"){lbsize <- NULL}
  if(is.null(alpha)){alpha <- 0.55}
  if(!is_sf(glottodata) ) {glottodata <- glottojoin_space(glottodata)}


  if(is.null(color)){color <- "black"}

  if(type == "dynamic"){
    map <- glottomap_dynamic(glottodata = glottodata, label = label, color = color, ptsize = ptsize, alpha = alpha, palette = palette, nclass = nclass, numcat = numcat)
  }

  if(type == "static"){
    map <- glottomap_static(glottodata = glottodata, label = label, color = color, ptsize = ptsize, lbsize = lbsize, alpha = alpha, palette = palette, rivers = rivers, nclass = nclass, numcat = numcat, crs = crs)
  }

  if(!is.null(filename)){
    glottosave(map, filename)
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
#' @noRd
#' @export
#'
#' @examples
#' glottodata <- glottofilter(continent = "South America")
#' glottodata <- glottofilter(country = "Netherlands")
#' glottomap_dynamic(glottodata)
glottomap_dynamic <- function(glottodata, label = NULL, color = NULL, ptsize = NULL, alpha = 1, palette = NULL, nclass = NULL, numcat = FALSE){
    suppressMessages(tmap::tmap_mode("view"))
  if(is.null(ptsize)){ptsize <- 0.08}
  if(is.null(label)){label <- NA}

    tmap::tm_basemap("Esri.WorldTopoMap") +
        {if(is_polygon(glottodata))
        tmap::tm_shape(glottodata) +
          tmap::tm_polygons(id = label, col = color, alpha = alpha, palette = palette,
                            n = {ifelse(is.null(nclass), 5, nclass)}, style = {ifelse(numcat == TRUE, "cat", "pretty")})} +
      {if(is_point(glottodata))
        tmap::tm_shape(glottodata) +
          tmap::tm_dots(id = label, col = color, size = ptsize, alpha = alpha, palette = palette,
                           n = {ifelse(is.null(nclass), 5, nclass)}, style = {ifelse(numcat == TRUE, "cat", "pretty")}) } +
      {if(glottospotlight_legend(glottodata)[[1]]){tmap::tm_add_legend(col = glottospotlight_legend(glottodata)$col, labels = glottospotlight_legend(glottodata)$labels)} }

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
#' @param alpha Transparency of points between 0 (very transparent) and 1 (not transparent)
#' @param palette Color palette, see glottocolpal("all") for possible options
#' @param rivers Do you want to plot rivers?
#' @param nclass Preferred number of classes (default is 5)
#' @param numcat Do numbers represent categories? For example, if your dataset consists of 0 and 1, you might want to set this to TRUE.
#' @param crs Coordinate Reference System, specified using an EPSG code (https://epsg.io/). Default is World Eckert IV (https://epsg.io/54012)
#'
#' @return
#' @keywords internal
#' @export
#' @noRd
#'
#' @examples
#' glottodata <- glottofilter(continent = "South America")
#' glottodata <- glottofilter(country = c("Netherlands", "Germany", "Belgium") )
#' glottomap_static(glottodata)
glottomap_static <- function(glottodata, label = NULL, color = NULL, ptsize = 1, lbsize = NULL, alpha = 1, palette = NULL, rivers = FALSE, nclass = NULL, numcat = FALSE, crs = NULL){
  suppressMessages(tmap::tmap_mode("plot"))
  if(is.null(ptsize)){ptsize <- 0.5}
  if(is.null(crs)){crs <- "ESRI:54012"} # https://epsg.io/54012

  # wrld_proj <- geoget_basemap(crs = "+proj=eck4", attributes = FALSE) # function migrated to geospace package
  wrld_basemap <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  wrld_wrap <- sf::st_wrap_dateline(wrld_basemap, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
  wrld_proj <- sf::st_transform(wrld_wrap, crs = crs)
  wrld_proj <- wrld_proj %>% sf::st_make_valid()
  wrld_proj <- wrld_proj %>% sf::st_geometry()


  # glottodata <- sf::st_make_valid(glottodata) # This converts some points to GEOMETRYCOLLECTION and therefore results in errors later on.
  glottodata_wrap <- sf::st_wrap_dateline(glottodata, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
  glottodata_proj <- sf::st_transform(glottodata_wrap, crs = crs)

  if(rivers == TRUE){
    rivers_name <- 'ne_10m_rivers_lake_centerlines'
    rivers_path <- glottofiles_makepath(paste0(rivers_name, ".shp"))
    if(file.exists(rivers_path)){
      rivers10 <- rnaturalearth::ne_load(scale = 10, type = 'rivers_lake_centerlines',
                                                              category = 'physical', returnclass = "sf",
                                                              destdir = glottofiles_cachedir())
    } else {
      rivers10 <- rnaturalearth::ne_download(scale = 10, type = 'rivers_lake_centerlines',
                                                            category = 'physical', returnclass = "sf",
                                                            destdir = glottofiles_cachedir())
    }
     rivers_proj <- sf::st_transform(rivers10, crs = crs)
  }

  bbox <- sf::st_bbox(glottodata_proj)
  bboxe <- bbox_expand(bbox, f = 0.1)
  wrld_projbb <- sf::st_crop(wrld_proj, bboxe)

  tmap::tm_shape(wrld_projbb) + tmap::tm_fill(col = "white", alpha = 1) + tmap::tm_borders(lwd=1.2) +
    {if(rivers == TRUE){tmap::tm_shape(rivers_proj) +
        tmap::tm_lines(col = "lightblue")} } +
    {if(is_polygon(glottodata_proj))
      tmap::tm_shape(glottodata_proj) +
        tmap::tm_polygons(col = color, alpha = alpha, palette = palette,
                          n = {ifelse(is.null(nclass), 5, nclass)}, style = {ifelse(numcat == TRUE, "cat", "pretty")})} +
    {if(is_point(glottodata_proj))
      tmap::tm_shape(glottodata_proj) +
        tmap::tm_dots(col = color, size = ptsize, alpha = alpha, palette = palette,
                         n = {ifelse(is.null(nclass), 5, nclass)}, style = {ifelse(numcat == TRUE, "cat", "pretty")}) } +
    {if(!purrr::is_empty(label)) tmap::tm_text(text = label, size = lbsize, auto.placement = TRUE)} +
    tmap::tm_legend(legend.outside = TRUE) + tmap::tm_layout(bg.color = "grey85", inner.margins = c(0,0,0,0)) +
    {if(glottospotlight_legend(glottodata)[[1]]){tmap::tm_add_legend(col = glottospotlight_legend(glottodata)$col, labels = glottospotlight_legend(glottodata)$labels)} }
}

glottomap_static_ggplot <- function(glottodata){
  world <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  world <- world %>% st_make_valid()

  # From here: https://stackoverflow.com/questions/56146735/visual-bug-when-changing-robinson-projections-central-meridian-with-ggplot2
  # define a long & slim polygon that overlaps the meridian line & set its CRS to match
  # that of world
  polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                       c(0, 90),
                                       c(0, -90),
                                       c(-0.0001, -90),
                                       c(-0.0001, 90)))) %>%
    st_sfc() %>%
    st_set_crs(4326)

  # modify world dataset to remove overlapping portions with world's polygons
  world2 <- world %>% st_difference(polygon)

  # perform transformation on modified version of world dataset
  # Note +lon_0=180 instead of 0
  world_robinson <- st_transform(world2,
                                 crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

  # plot
  ggplot() +
    geom_sf(data = world_robinson) +
    geom_sf(data = glottodata, aes(color = family)) +
    theme(legend.position = "none")


}





