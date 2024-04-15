
#' Create static and dynamic maps from glottodata, or select languages from a map
#'
#' With this function you can easily create static and dynamic maps from glottodata (by setting type to 'static' or 'dynamic').
#' Alternatively, by specifying type = "filter", you can interactively select languages by drawing a shape around them (mode = "draw"; default) or by clicking on them (mode = "click"). See ?glottofiltermap for more details.
#'
#' @param glottodata Optional, user-provided glottodata. In case no glottodata is provided, you can pass arguments directly to glottofilter.
#' @param color glottovar, column name, or column index to be used to color features (optional). See 'Details' below.
#' @param label glottovar, column name, or column index to be used to label features (optional). See 'Details' below.
#' @param type One of: "static", "dynamic", or "filter". Default is "static".
#' @param ptsize Size of points between 0 and 1
#' @param lbsize Size of labels between 0 and 1
#' @param alpha Transparency of points between 0 (very transparent) and 1 (not transparent)
#' @param palette Color palette, see glottocolpal("all") for possible options, and run glottocolpal("turbo") to see what it looks like (replace it with palette name).
#' Alternatively, you could also run tmaptools::palette_explorer(), RColorBrewer::display.brewer.all(), ?viridisLite::viridis, or scales::show_col(viridisLite::viridis(n=20))
#' @param nclass Preferred number of classes (default is 5)
#' @param mode In case type = "filter", you can choose here whether you want to interactively select languages by clicking on them (mode = 'click', default) or by drawing a shape around them (mode = 'draw').
#' @param projection For static maps, you can choose one of the following: 'eqarea' (equal-area Eckert IV, default), 'pacific' (Pacific-centered), or any other Coordinate Reference System, specified using an EPSG code (https://epsg.io/), for example: "ESRI:54009".
#' @param filename Optional filename if you want to save resulting map
#' @param glotto_title Optional, the title of legend, the default value is the name of the argument color.
#' @param basemap The default basemap is "country", which gives the borders of countries.
#' Alternatively, the basemap can be set to be "hydro-basin",
#' this gives global \href{https://www.hydrosheds.org/products/hydrobasins}{hydro-basins} (Level 03).
#'
#' @param ... Additional parameters to glottofilter
#' @param rivers Do you want to plot rivers?
#'
#' @evalRd glottovars()
#' @return a map created from a glotto(sub)data object and can be saved with glottosave()
#' @export
#'
#' @examples
#' \dontrun{
#' glottomap(country = "Netherlands")
#'
#' glottopoints <- glottofilter(continent = "South America")
#' glottopols <- glottospace(glottopoints, method = "voronoi")
#' glottomap(glottodata = glottopols, color = "family_size_rank")
#' glottomap(glottodata = glottopols, color = "family", palette = "turbo",
#' type = "dynamic", label = "name")
#'
#' glottodata <- glottoget()
#' families <- dplyr::count(glottodata, family, sort = TRUE)
#'
#' # highlight 10 largest families:
#' glottodata <- glottospotlight(glottodata = glottodata, spotcol =
#' "family", spotlight = families$family[1:10], spotcontrast = "family")
#'
#' # Or, place 10 largest families in background
#' glottodata <- glottospotlight(glottodata = glottodata, spotcol =
#' "family", spotlight = families$family[-c(1:10)], spotcontrast = "family")
#' glottomap(glottodata, color = "legend")
#'
#' # Interactive selection by clicking on languages:
#' selected <- glottomap(continent = "South America", type = "filter")
#' glottomap(selected)
#'
#' # Interactive selection by drawing a shape:
#' selected <- glottomap(continent = "South America", type = "filter", mode = "draw")
#' glottomap(selected)
#' }
glottomap <- function(glottodata = NULL, color = NULL, label = NULL, type = NULL, ptsize = NULL, alpha = NULL, lbsize = NULL,
                      palette = NA, rivers = FALSE, nclass = NULL, filename = NULL, projection = NULL,
                      glotto_title = NULL, mode = NULL, basemap = "country",...){
  rlang::check_installed("tmap", reason = "to use `glottomap()`", version = "3.9")
 # palette <- glottocolpal(palette = palette)
  if(is.null(type)){type <- "static"}

  if(!is.null(glottodata)){
    if(!is_sf(glottodata) ) {
      glottodata <- glottosimplify(glottodata)
      glottodata <- glottojoin_base(glottodata)
    }
  } else {
    glottodata <- glottofilter(...)
    if(length(sf::st_geometry(glottodata)) == 1 & type == "static"){ #added to solve issue with countries that are not polygons in naturalearthdata (they consist of a single point)
      # glottodata <- sf::st_buffer(glottodata, dist = 0)
      type <- "dynamic"
      message("The country you are trying to plot is too small for a static map, returning a dynamic map instead.")
    }
  }

  if(type == "dynamic"){
    map <- glottomap_dynamic(glottodata = glottodata, label = label, color = color, ptsize = ptsize, alpha = alpha, nclass = nclass,
                             palette = palette, lbsize=lbsize,
                             glotto_title = glotto_title, basemap = basemap,
                             rivers = rivers)
  }

  if(type == "static"){
    map <- glottomap_static(glottodata = glottodata, label = label, color = color, ptsize = ptsize, lbsize = lbsize,
                            alpha = alpha, palette = palette, rivers = rivers, nclass = nclass, projection = projection,
                            glotto_title = glotto_title, basemap = basemap)
  }

  if(type == "filter"){
    map <- glottofiltermap(glottodata = glottodata, mode = mode)
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
#' @noRd
#'
#'
#' @examples
#' \dontrun{
#' glottodata <- glottofilter(continent = "South America")
#' glottodata <- glottofilter(country = "Netherlands")
#' glottomap_dynamic(glottodata)
#' }
glottomap_dynamic <- function(glottodata, color = NULL, ptsize = NULL, alpha = NULL, nclass=NULL, palette = NA,
                              label = NULL, lbsize=NULL, glotto_title = NULL, basemap = "country",
                              rivers = FALSE){
  suppressMessages(tmap::tmap_mode("view"))
  if(is.null(ptsize)){ptsize <- 0.8}
  if(is.null(label)){label <- NA}
  if(is.null(alpha)){alpha <- 0.55}
  if(is.null(lbsize)){lbsize <- .5}
  if(!is.null(color)){
    if (color %in% colnames(glottodata)){
      nrcat <- nrow(unique(glottosimplify(glottodata[,color])))
      if(nrcat > 30){
        tmap::tmap_options(max.categories = nrcat)
      }}}
   else{
    color <- "black"
  }

  if(rivers == TRUE){
    invisible(readline(prompt="Are you sure you want to download rivers from naturalearth? \n Press [enter] to continue"))
    rivers10 <- rnaturalearth::ne_download(scale = 10, type = 'rivers_lake_centerlines',
                                           category = 'physical', returnclass = "sf")
    rivers_proj <- sf::st_transform(rivers10)
  }
  {if(basemap == "country"){
    # tmap::tm_basemap("Esri.WorldTopoMap") +
    tmap::tm_shape(glottospace::worldpol) +
      tmap::tm_polygons(fill_alpha = 0.1)
    } else if (basemap == "hydro-basin"){
      sf::sf_use_s2(FALSE)
      glottodata_wrap <- sf::st_wrap_dateline(glottodata, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
      glottodata_proj <- sf::st_transform(glottodata_wrap, crs = sf::st_crs(global_basins))
      # bbox <- sf::st_bbox(glottodata_proj)
      # bboxe <- bbox_expand(bbox, f = 0.1)
      # hbsn_projbb <- sf::st_crop(global_basins, bboxe)

      tmap::tm_basemap("Esri.WorldTopoMap") +
        global_basins |>
        # sf::st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE) |>
        # sf::st_make_valid() |>
        tmap::tm_shape() +
        tmap::tm_polygons(fill = "white",
                          fill_alpha = 0,
                          fill.scale = tmap::tm_scale_categorical(),
                          lwd=1.5)
      }
    } +
    # tmap::tm_shape(global_basins) +
    {if(is_polygon(glottodata))
    {tmap::tm_shape(glottodata) +
        tmap::tm_polygons(
          fill = color,
          fill.scale = tmap::tm_scale_categorical(
            values=palette,
            n.max = {ifelse(is.null(nclass), 5, nclass)},
            # label.na = "BG"
            ),
          fill_alpha = alpha,
          fill.legend = tmap::tm_legend(title=glotto_title)
        )}} +
    {if(is_point(glottodata))
    {
      if(glottospotlight_legend(glottodata)[[1]] && color == "legend"){tmap::tm_shape(glottodata) +
          tmap::tm_dots(
            fill = color,
            fill.scale = tmap::tm_scale_categorical(
              values = palette,
              n.max = {ifelse(is.null(nclass), 5, nclass)},
              label.na = "The rest"
              ),
            fill.legend = tmap::tm_legend(title = glotto_title),
            fill_alpha = alpha,
            size = ptsize,
            # size.legend = tm_legend(title = legend_size)
          )}
      else{tmap::tm_shape(glottodata) +
          tmap::tm_dots(
            fill = color,
            fill.scale = tmap::tm_scale_categorical(
              values = palette,
              n.max = {ifelse(is.null(nclass), 5, nclass)},
              # label.na = "BG"
              ),
            fill.legend = tmap::tm_legend(title = glotto_title),
            fill_alpha = alpha,
            size = ptsize,
            # size.legend = tm_legend(title = legend_size)
          )}}
    } +
    {if(rivers == TRUE){tmap::tm_shape(rivers_proj)} +
        tmap::tm_lines(col = "lightblue",
                       col.scale = 1)} +
    tmap::tm_text(text = label,
                  # text.legend = tmap::tm_legend(title = legend_text),
                  size = lbsize,
                  # size.scale = tmap::tm_scale()
                  remove.overlap = TRUE
                  )
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
#' @param projection One of: 'eqarea' (equal-area Eckert IV, default), 'pacific' (Pacific-centered), or any other Coordinate Reference System, specified using an EPSG code (https://epsg.io/).
#'
#'
#' @noRd
#'
#'
#' @examples
#' \dontrun{
#' glottodata <- glottofilter(continent = "South America")
#' glottodata <- glottofilter(country = c("Netherlands", "Germany", "Belgium") )
#' glottomap_static(glottodata)
#' }
glottomap_static <- function(glottodata, projection = NULL, label = NULL, color = NULL, ptsize = 1, lbsize = NULL,
                             nclass = NULL, alpha = 1, palette = NA, rivers = FALSE, glotto_title = NULL, basemap = "country"){
  if(is.null(projection)){projection <- "eqarea"}
  if(is.null(lbsize)){lbsize <- 0.75}
  if(!is.null(color) && (color %in% colnames(glottodata))){
    nrcat <- nrow(unique(glottosimplify(glottodata[,color])))
    if(nrcat > 30){
      tmap::tmap_options(max.categories = nrcat)
    }
  }

  if(projection == "pacific" | projection == "Pacific" | projection == "pacific-centered" | projection == "Pacific-centered"){
    glottomap_static_pacific(glottodata = glottodata, color = color, palette = palette, ptsize = ptsize,
                             nclass = nclass, alpha = alpha, rivers = rivers, basemap = basemap,
                             glotto_title = glotto_title)
  } else if(projection == "eqarea" | projection == "equal-area" | projection == "equalarea"){
    glottomap_static_crs(glottodata, crs = NULL, label = label, color = color, ptsize = ptsize, lbsize = lbsize,
                         alpha = alpha, palette = palette, rivers = rivers, nclass = nclass, glotto_title = glotto_title, basemap = basemap)
  } else {
    glottomap_static_crs(glottodata, crs = projection, label = label, color = color, ptsize = ptsize, lbsize = lbsize,
                         alpha = alpha,  palette = palette, rivers = rivers, nclass = nclass, glotto_title = glotto_title, basemap = basemap)
  }
}



#' Create a static (equal-area) map with glottodata
#'
#' This function returns a static map with glottodata. Data is projected using the equal-area Eckert IV projection (following McNew et al. 2018). See \url{https://epsg.io/54012} and \url{https://en.wikipedia.org/wiki/Eckert_IV_projection}.
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
#'
#' @noRd
#'
#'
#' @examples
#' \dontrun{
#' glottodata <- glottofilter(continent = "South America")
#' glottodata <- glottofilter(country = c("Netherlands", "Germany", "Belgium") )
#' glottomap_static_crs(glottodata)
#' }
glottomap_static_crs <- function(glottodata, label = NULL, color = NULL, ptsize = NULL, lbsize = NULL, alpha = NULL, palette = NA,
                                 rivers = FALSE, nclass = NULL, crs = NULL, glotto_title = NULL, basemap = "country"){
  suppressMessages(tmap::tmap_mode("plot"))
  if(is.null(ptsize)){ptsize <- 0.5}
  if(is.null(crs)){crs <- "ESRI:54012"} # https://epsg.io/54012
  if(is.null(color)){color <- "black"}
  if(is.null(alpha)){alpha <- 0.55}

  # wrld_proj <- geoget_basemap(crs = "+proj=eck4", attributes = FALSE) # function migrated to geospace package
  if (basemap == "country"){
    wrld_basemap <- glottospace::worldpol
    #wrld_basemap <- sf::st_collection_extract(global_basins_robinson) %>%
    #  sf::st_simplify(dTolerance = 1e3)
    wrld_wrap <- sf::st_wrap_dateline(wrld_basemap, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
    wrld_proj <- sf::st_transform(wrld_wrap, crs = crs)
    wrld_proj <- wrld_proj %>% sf::st_make_valid()
    # wrld_proj <- wrld_proj %>% sf::st_geometry()
  } else if (basemap == "hydro-basin"){
    # wrld_basemap <- sf::st_collection_extract(global_basins)
    # wrld_wrap <- sf::st_wrap_dateline(wrld_basemap, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
    # wrld_proj <- sf::st_transform(wrld_wrap, crs = crs)
    # wrld_proj <- wrld_proj %>% sf::st_make_valid()
    # wrld_proj <- wrld_proj %>% sf::st_geometry()
    wrld_proj <- global_basins |>
      sf::st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE) |>
      sf::st_transform(crs = crs) |>
      sf::st_make_valid()
  }




  # glottodata <- sf::st_make_valid(glottodata) # This converts some points to GEOMETRYCOLLECTION and therefore results in errors later on.
  glottodata_wrap <- sf::st_wrap_dateline(glottodata, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)
  glottodata_proj <- sf::st_transform(glottodata_wrap, crs = crs)

  if(rivers == TRUE){
    invisible(readline(prompt="Are you sure you want to download rivers from naturalearth? \n Press [enter] to continue"))
    rivers10 <- rnaturalearth::ne_download(scale = 10, type = 'rivers_lake_centerlines',
                                           category = 'physical', returnclass = "sf")
    rivers_proj <- sf::st_transform(rivers10, crs = crs)
  }

  bbox <- sf::st_bbox(glottodata_proj)
  bboxe <- bbox_expand(bbox, f = 0.1)
  wrld_projbb <- sf::st_crop(wrld_proj, bboxe)

  #
  tmap::tm_shape(wrld_projbb) +
    tmap::tm_polygons(fill = "white",
                      fill_alpha = 1,
                      fill.scale = tmap::tm_scale_categorical(),
                      # fill.legend = tmap::tm_legend_hide(),
                      lwd=1.5) +
    {if(rivers == TRUE){tmap::tm_shape(rivers_proj)} +
        tmap::tm_lines(col = "lightblue",
                       col.scale = 1)} +
    {if(is_polygon(glottodata_proj)){tmap::tm_shape(glottodata_proj) +
        tmap::tm_polygons(fill=color,
                          fill.scale = tmap::tm_scale_categorical(values = palette,
                                                      n.max = {ifelse(is.null(nclass), 5, nclass)}),
                          fill_alpha = alpha,
                          fill.legend = tmap::tm_legend(title = glotto_title)
        )}} +
    {if(is_point(glottodata_proj))
      if(glottospotlight_legend(glottodata)[[1]] && color == "legend"){
        tmap::tm_shape(glottodata_proj) +
          tmap::tm_dots(fill = "legend",
                        fill.scale = tmap::tm_scale_categorical(
                          values = palette,
                          # values = glottospotlight_legend(glottodata_proj)$col,
                          n.max = {ifelse(is.null(nclass), 5, nclass)},
                          label.na = "The rest"
                          ),
                        fill_alpha = alpha,
                        fill.legend = tmap::tm_legend(title = glotto_title),
                        size = ptsize
                        # size.scale = tmap::tm_scale(values=glotto_size_values),
                        # size.legend = tmap::tm_legend(title = glotto_size_title)
          )}
      else{
        tmap::tm_shape(glottodata_proj) +
          tmap::tm_dots(fill = color,
                        fill.scale = tmap::tm_scale_categorical(
                          values = palette,
                          n.max = {ifelse(is.null(nclass), 5, nclass)},
                          # label.na = "BG"
                          ),
                        fill_alpha = alpha,
                        fill.legend = tmap::tm_legend(title = glotto_title,
                                                      legend.outside = TRUE
                                                      ),
                        size = ptsize
                        # size.scale = tmap::tm_scale(values=glotto_size_values),
                        # size.legend = tmap::tm_legend(title = glotto_size_title)
          )}} +
    {if(!purrr::is_empty(label)){
      if(is.null(lbsize)){lbsize <- 1}
      tmap::tm_text(text = label,
                    size = lbsize,
                    size.scale = tmap::tm_scale(),
                    # remove.overlap = TRUE
                    # auto.placement = TRUE
                    )

    }} +
   # tmap::tm_legend(legend.outside = TRUE) +
    tmap::tm_layout(bg.color = "gray99",
                    inner.margins = c(0,0,0,0),
                    legend.text.size = .8
                    )
}



#' Create a static (Pacific-centered) world map with glottodata
#'
#' This function returns a static map with glottodata. Data is projected using the Robinson projection centered at the Pacific.
#'
#' @param glottodata
#'
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' glottodata <- glottofilter(location = "Australia")
#' glottomap_static_pacific(glottodata, color = "family")
#' }
glottomap_static_pacific <- function(glottodata, color = NULL, rivers = FALSE, ptsize = NULL,
                                     nclass = NULL, palette = NA, alpha = NULL, basemap = "country",
                                     glotto_title = NULL){
  suppressMessages(tmap::tmap_mode("plot"))
  if(is.null(color)){color <- "black"}
  if(is.null(ptsize)){ptsize <- 1}
  if(is.null(alpha)){alpha <- 0.55}

  if (basemap == "country"){
    world <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  } else if (basemap == "hydro-basin"){
    world <- global_basins %>% sf::st_make_valid()
  }
  world <- world %>% sf::st_make_valid()

  polygon <- sf::st_polygon(x = list(rbind(c(-0.0001, 90), c(0, 90), c(0, -90), c(-0.0001, -90), c(-0.0001, 90)))) %>%
    sf::st_sfc() %>%
    sf::st_set_crs(sf::st_crs(world))

  # modify world dataset to remove overlapping portions with world's polygons
  world2 <- world %>% sf::st_difference(polygon)
  # perform transformation on modified version of world dataset
  # Note +lon_0=180 instead of 0
  world_robinson <- sf::st_transform(world2,
                                     crs = '+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')


  if(rivers == TRUE){
    invisible(readline(prompt="Are you sure you want to download rivers from naturalearth? \n Press [enter] to continue"))
    rivers10 <- rnaturalearth::ne_download(scale = 10, type = 'rivers_lake_centerlines',
                                           category = 'physical', returnclass = "sf")
    rivers10 <- suppressWarnings(rivers10 %>% sf::st_difference(polygon) )
  }

  if(!is.null(color)){ncolr <- length(unique(glottodata[[color]]))}

  # plot
  # ggplot2::ggplot() +
  #   ggplot2::geom_sf(data = world_robinson, fill = "white") +
  #   {if(rivers == TRUE){ggplot2::geom_sf(data = rivers10, color = "lightblue" ) }} +
  #   {if(is.null(color)){ggplot2::geom_sf(data = glottodata, ggplot2::aes(), size = ptsize, alpha = alpha )
  #     } else{ggplot2::geom_sf(data = glottodata, ggplot2::aes(color = .data[[color]]), size = ptsize, alpha = alpha )}
  #     } + {if(!is.null(palette)){ggplot2::scale_color_manual(values = glottocolpal(palette, ncolr = ncolr ))}} +
  #     ggplot2::theme(legend.position = "none",
  #                  plot.background = ggplot2::element_rect(fill = "white"),
  #                  panel.background = ggplot2::element_rect(fill = "grey85"),
  #                  panel.grid = ggplot2::element_line(colour = "white"))

  tmap::tm_shape(world_robinson) +
    tmap::tm_polygons(fill = "white",
                      fill_alpha = 1,
                      fill.scale = tmap::tm_scale(),
                      lwd=1) +
    tmap::tm_graticules(col = "white",
                        n.x = 10,
                        n.y = 10) +
    tmap::tm_scalebar(breaks = c(0, 100, 200)) +
    tmap::tm_layout(bg.color = "lightgrey",
              basemap.alpha = .4,
              outer.bg.color = "white") +
    tmap::tm_shape(glottodata) +
    tmap::tm_dots(fill = color,
                  fill.scale = tmap::tm_scale_categorical(
                    values = palette,
                    n.max = {ifelse(is.null(nclass), 5, nclass)},
                    ),
                  fill_alpha = alpha,
                  fill.legend = tmap::tm_legend(title = glotto_title,
                                          legend.outside = TRUE
                                          ),
                  size = ptsize
                  # size.scale = tm_scale_continuous(values=c(0, 1)),
                  # size.legend = tm_legend(title = glotto_size_title)
    )
}

#' Show location of glottocode on globe
#'
#' @param glottocode
#'
#' @noRd
#' @examples
#' \dontrun{
#' glottomap_glottocode("yucu1253")
#' }
glottomap_glottocode <- function(glottocode){
  rlang::check_installed("s2", reason = "to use `glottomap_glottocode()`")
  language <- glottofilter(glottocode = glottocode)
  lon0 = sf::st_coordinates(language)[1]
  lat0 = sf::st_coordinates(language)[2]
  language <- s2::as_s2_geography(paste0("POINT(", lon0, " ", lat0, ")") )

  earth = s2::as_s2_geography(TRUE)
  continents = s2::s2_data_countries()
  oceans = s2::s2_difference(earth, s2::s2_union_agg(continents))
  b = s2::s2_buffer_cells(language, 9800000) # visible half
  i = s2::s2_intersection(b, oceans) # visible ocean
  continents = s2::s2_intersection(b, continents)
  plot(sf::st_transform(sf::st_as_sfc(i), paste0("+proj=ortho +lat_0=",lat0, " +lon_0=",lon0) ), col = 'lightblue')
  plot(sf::st_transform(sf::st_as_sfc(continents), paste0("+proj=ortho +lat_0=",lat0, " +lon_0=",lon0) ), col = "lightgrey", add = TRUE)
  plot(sf::st_transform(sf::st_as_sfc(language), paste0("+proj=ortho +lat_0=",lat0, " +lon_0=",lon0) ), col = "darkred", pch = 1, cex = 3, lwd = 2, add = TRUE)
}



#' Title
#'
#' @param glottodata a glottodata is an object of sf with geometry type as `POINT`
#' @param r a numerica number, the radius of buffers of all the points in glottodata, the default unit is "100km"
#' @param maxscale a numeric number, maximum value of the rips filtration, the default unit is "100km"
#' @param is_animate if TRUE, it will generate a GIF file, if FALSE, it will generate a tmap plot,
#' the default value is FALSE
#' @param length.out the amount of images to be generated in GIF file when `is_animate = TRUE`,
#' the default value is `20`
#' @param movie.name name of the GIF file, the default value is "filtration.gif"
#'
#' @return if `is_animate = FALSE` return a tmap, if `is_animate = TRUE` return a GIF file
#' @export
#'
#' @examples
#' glottopoints <- glottofilter(continent = "South America")
#' awk <- glottopoints[glottopoints$family == "Arawakan", ]
#' glottomap_rips_filt(glottodata = awk, r = 6, maxscale = 8)
#' \dontrun{
#' glottomap_rips_filt(glottodata = awk, r = 6, maxscale = 8, is_animate=TRUE)
#' }
glottomap_rips_filt <- function(glottodata, r=0, maxscale, is_animate=FALSE, length.out = 20,
                                movie.name="filtration.gif"){
  if (is_animate){
    glottomap_rips_filt_animate(glottodata = glottodata, r = r, maxscale = maxscale,
                                          length.out = length.out, movie.name = movie.name)
  } else if (!is_animate){
    glottomap_rips_filt_static(glottodata = glottodata, r = r, maxscale = maxscale)
  }
}



#' Title
#'
#' @param glottodata a glottodata with geometry type of `POINT`
#' @param r the radius of buffers of all the points in glottodata, the unit of `r` is "100km"
#' @param maxscale a numeric number, maximum value of the rips filtration
#'
#' @return a tmap
#' @noRd
#'
glottomap_rips_filt_static <- function(glottodata, r=0, maxscale){
  if (all(sf::st_is(glottodata, "POINT")) != TRUE){
    stop("The geometry types of glottodata must be 'POINT'.")
  } else{
    glottogmtry <- sf::st_geometry(glottodata)

    buffers <- sf::st_buffer(glottogmtry, dist =  units::set_units(r, "100km") / 2)


    bbox <- sf::st_bbox(glottogmtry)
    bboxe <- bbox_expand(bbox, f = 0.1)
    wrld_projbb <- sf::st_crop(sf::st_geometry(glottospace::worldpol), bboxe)

    plt <- tmap::tm_shape(wrld_projbb) +
      tmap::tm_polygons(fill_alpha=.5, fill="lightgrey") +
      tmap::tm_shape(buffers) +
      tmap::tm_polygons(fill_alpha = 0.1, col = "blue", col_alpha = 0.3, fill = "blue")

    dist_mtx <- units::set_units(sf::st_distance(glottogmtry), "100km")
    filt <- TDA::ripsFiltration(dist_mtx, maxdimension = 1,
                                maxscale = maxscale, dist = "arbitrary")


    polygons_r <- filt[["cmplx"]][filt$values <= r]

    mult_plg <- polygons_r |>
      lapply(
        FUN = function(x){
          if (length(x) == 2){
            glottogmtry[x] |>
              sf::st_combine() |>
              sf::st_cast("LINESTRING")
          } else if (length(x) == 3){
            glottogmtry[x] |>
              sf::st_combine() |>
              sf::st_cast("POLYGON")
          }
        }
      )

    mult_plg <- Filter(Negate(is.null), mult_plg)

    if (length(mult_plg) > 0){
      is_plg <- mult_plg |>
        sapply(FUN = function(x){
          sf::st_is(x, "POLYGON")
        })

      if (any(is_plg)){
        is_plg <- which(is_plg)
        plg_lst <- mult_plg[is_plg]
        mult_plg_sfc <- sf::st_union(do.call("c", plg_lst)) |>
          sf::st_make_valid()
      } else {
        mult_plg_sfc <- NULL
      }

      is_line <- mult_plg |>
        sapply(FUN = function(x){
          sf::st_is(x, "LINESTRING")
        })

      if (any(is_line)){
        is_line <- which(is_line)
        line_lst <- mult_plg[is_line]
        mult_line_sfc <- sf::st_combine(do.call("c", line_lst))
      } else {
        mult_line_sfc <- NULL
      }
    } else {
      mult_plg_sfc <- NULL
      mult_line_sfc <- NULL
    }

    plt <- plt +
      {if (!is.null(mult_plg_sfc)){
        tmap::tm_shape(mult_plg_sfc) +
          tmap::tm_polygons(fill = "pink")
      }} +
      {if (!is.null(mult_line_sfc)){
        tmap::tm_shape(mult_line_sfc) +
          tmap::tm_lines(col = "red")
      }} +
      tmap::tm_shape(glottogmtry) +
      tmap::tm_dots(size = .2)

    return(plt)
  }
}

#' Title
#'
#' @param mult_plg a list of POLYGON and LINESTRING
#' @param r_filt a vector of length of persistent radius
#' @param r_seq a vector of truncated radius
#' @param idx_1 the first index of radius in `r_seq`
#' @param idx_2 the second index of radius in `r_seq`
#'
#' @return a list of two elements
#' @noRd
#'
cmplx_filt <- function(mult_plg=NULL, r_filt=NULL, r_seq=NULL, idx_1=NULL, idx_2=NULL){
  cmplxes <- list()
  cmplxes[[1]] <- NA
  cmplxes[[2]] <- NA

  if (length(mult_plg) > 0){
    filt_idx <- which((r_filt < r_seq[idx_2]) & (r_filt >= r_seq[idx_1]))

    if(length(filt_idx) > 0){
      gmtry <- mult_plg[filt_idx]

      is_plg <- gmtry |>
        sapply(FUN = function(x){
          sf::st_is(x, "POLYGON")
        })

      if (any(is_plg) != FALSE){
        is_plg <- which(is_plg)
        plg_lst <- gmtry[is_plg]
        mult_plg_sfc <- sf::st_union(do.call("c", plg_lst)) |>
          sf::st_make_valid()
      } else{
        mult_plg_sfc <- NA
      }

      is_line <- gmtry |>
        sapply(FUN = function(x){
          sf::st_is(x, "LINESTRING")
        })

      if (any(is_line) != FALSE){
        is_line <- which(is_line)
        line_lst <- gmtry[is_line]
        mult_line_sfc <- sf::st_combine(do.call("c", line_lst))
      } else{
        mult_line_sfc <- NA
      }

      cmplxes[[1]] <- mult_plg_sfc
      cmplxes[[2]] <- mult_line_sfc

    }
  }
  return(cmplxes)
}

#' Title
#'
#' @param glottodata a glottodata with geometry type of `POINT`
#' @param r the radius of buffers of all the points in glottodata, the unit of `r` is "100km"
#' @param maxscale a numeric number, maximum value of the rips filtration
#' @param length.out the amount of images to be generated in GIF file when `is_animate = TRUE`, the default value is `20`
#' @param movie.name name of the GIF file, the default value is "filtration.gif"
#'
#' @return a GIF file
#' @noRd
#'
glottomap_rips_filt_animate <- function(glottodata, r=0, maxscale, length.out = 20,
                                           movie.name="filtration.gif"){
  if (all(sf::st_is(glottodata, "POINT")) != TRUE){
    stop("The geometry types of glottodata must be 'POINT'.")
  } else{
    glottogmtry <- sf::st_geometry(glottodata)
    dist_mtx <- units::set_units(sf::st_distance(glottogmtry), "100km")
    filt <- TDA::ripsFiltration(dist_mtx, maxdimension = 1,
                                maxscale = maxscale, dist = "arbitrary")

    polygons_r <- filt[["cmplx"]][filt$values <= r]

    mult_plg <- polygons_r |>
      lapply(
        FUN = function(x){
          if (length(x) == 2){
            glottogmtry[x] |>
              sf::st_combine() |>
              sf::st_cast("LINESTRING")
          } else if (length(x) == 3){
            glottogmtry[x] |>
              sf::st_combine() |>
              sf::st_cast("POLYGON")
          }
        }
      )

    mult_plg <- Filter(Negate(is.null), mult_plg)
    if (length(mult_plg) > 0){
      r_filt <- filt[["values"]][filt$values < r][which(
        mult_plg |>
          sapply(
            FUN = function(x){
              !is.null(x)
            }))]
    } else{
      r_filt <- NULL
    }
    r_seq <- seq(0, r, length.out = length.out)

    bbox <- sf::st_bbox(glottogmtry)
    bboxe <- bbox_expand(bbox, f = 0.1)
    wrld_projbb <- sf::st_crop(sf::st_geometry(glottospace::worldpol), bboxe)

    plt <- tmap::tm_shape(wrld_projbb) +
      tmap::tm_polygons(fill_alpha=.5, fill="lightgrey")

    rips_cmplx <- list()
    rips_cmplx[[1]] <- NA
    rips_cmplx[[2]] <- NA

    buffers_0 <- sf::st_buffer(glottogmtry, dist = units::set_units(r_seq[1], "100km") / 2)

    plt_0 <- plt +
      tmap::tm_shape(buffers_0) +
      tmap::tm_polygons(fill_alpha = 0.1, col = "blue", col_alpha = 0.3, fill = "blue") +
      tmap::tm_shape(glottogmtry) +
      tmap::tm_dots(size = .1)

    animation::saveGIF({
      print(plt_0)
      pb <- utils::txtProgressBar(min = 0, max = length(r_seq)-1, initial = 0, style = 3)
      for (i in 1:(length(r_seq)-1)){
        rips_cmplx_update <- cmplx_filt(mult_plg = mult_plg, r_filt = r_filt, r_seq = r_seq, idx_1 = i, idx_2 = i+1)
        if (!is.na(rips_cmplx[[1]]) && !is.na(rips_cmplx_update[[1]])){
          rips_cmplx[[1]] <- sf::st_union(rips_cmplx[[1]], rips_cmplx_update[[1]]) |>
            sf::st_make_valid()
        } else if (is.na(rips_cmplx[[1]]) && !is.na(rips_cmplx_update[[1]])){
          rips_cmplx[[1]] <- rips_cmplx_update[[1]]
        }

        if (!is.na(rips_cmplx[[2]]) && !is.na(rips_cmplx_update[[2]])){
          rips_cmplx[[2]] <- sf::st_union(rips_cmplx[[2]], rips_cmplx_update[[2]]) |>
            sf::st_make_valid()
        } else if (is.na(rips_cmplx[[2]]) && !is.na(rips_cmplx_update[[2]])){
          rips_cmplx[[2]] <- rips_cmplx_update[[2]]
        }

        buffers <- sf::st_buffer(glottogmtry, dist = units::set_units(r_seq[i+1], "100km") / 2)

        output <- plt +
          tmap::tm_shape(buffers) +
          tmap::tm_polygons(fill_alpha = 0.1, col = "blue", col_alpha = 0.3, fill = "blue") +
          {if(!is.na(rips_cmplx[[1]])){
            tmap::tm_shape(rips_cmplx[[1]]) +
              tmap::tm_polygons(fill = "pink")
          }} +
          {if(!is.na(rips_cmplx[[2]])){
            tmap::tm_shape(rips_cmplx[[2]]) +
              tmap::tm_lines(col = "red")
          }} +
          tmap::tm_shape(glottogmtry) +
          tmap::tm_dots(size = .1)

        print(output)
        utils::setTxtProgressBar(pb = pb, i)
      }
      close(pb)
    },  movie.name = movie.name)
  }
}

#' Title
#'
#' @param glottodata a glottodata is an object of sf with geometry type as `POINT`
#' @param maxscale a numeric number, maximum value of the rips filtration, the default unit is "100km"
#'
#' @return a ggplot2 map
#' @export
#'
#' @examples
#' glottopoints <- glottofilter(continent = "South America")
#' awk <- glottopoints[glottopoints$family == "Arawakan", ]
#' glottomap_persist_diagram(awk, maxscale = 15)
glottomap_persist_diagram <- function(glottodata, maxscale){
  if (all(sf::st_is(glottodata, "POINT")) != TRUE){
    stop("The geometry types of glottodata must be 'POINT'.")
  } else{
    glottogmtry <- sf::st_geometry(glottodata)

    dist_mtx <- units::set_units(sf::st_distance(glottogmtry), value="100km")
    rips <- TDA::ripsDiag(dist_mtx, maxdimension = 1,
                          maxscale = maxscale, dist = "arbitrary")
    rips_hom <- rips$diagram
    class(rips_hom) <- "matrix"
    rips_hom_df <- data.frame(rips_hom)
    rips_hom_df$dimension <- as.factor( rips_hom_df$dimension)

    scale_lim <- max(rips_hom) * 1.01
    p <- # ggplot2::ggplot(data = rips_hom_df, ggplot2::aes(Birth, Death, col=dimension, shape=dimension)) +
      ggplot2::ggplot(data = rips_hom_df, ggplot2::aes_string(x = "Birth", y = "Death", col= "dimension", shape= "dimension")) +
      ggplot2::xlim(0, scale_lim) +
      ggplot2::ylim(0, scale_lim) +
      ggplot2::geom_abline(slope = 1, intercept = 0) +
      ggplot2::xlab("Birth (100 km)") +
      ggplot2::ylab("Death (100 km)") +
      ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                     panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank()) +
      ggplot2::geom_point(ggplot2::aes_string(x = "Birth",
                                              y = "Death")) +
      ggplot2::coord_fixed(ratio = 1)
      # ggplot2::ggtitle(title_text) +
      # ggplot2::theme(plot.title = ggplot2::element_text(color = "black", hjust = title_hjust, vjust = title_vjust, size=title_size))
    p

  }
}



























