#' Get geographic data
#'
#' Load spatial data either from local path (supports both raster and vector
#' formats), or download from remote server.
#'
#' @param geodata Either a filepath to locally stored geodata, or the name of geodata to download ("elevation", "basemap", or "climate").
#' @param crs Coordinate Reference System to transform to (default is Eckert IV).
#' @param attributes Whether to keep polygon attributes, or return only geometries
#' @param continent Optional filter by continent
#' @param country Optional filter by country
#' @param region Optional filter by region
#' @param union Should the polygons be merged into a single polygon with resolved boundaries?
#' @param plot Should the resulting polygon(s) be plotted
#' @param antarctica Should Antarctica be kept?
#'
#' @return
#' @export
#' @family <geodata>
#'
#' @examples
#' From local path (not run):
#' geoget(geodata = "D:/data/vector.shp")
#' geoget(geodata = "D:/data/raster.tif")
#'
#' Download:
#' geoget(geodata = "elevation", country = "Netherlands")
#' geoget(geodata = "climate")
geoget <- function(geodata, crs = NULL, attributes = FALSE, continent = NULL, country = NULL, region = NULL, union = FALSE, plot = FALSE, antarctica = FALSE){
  geodownload <- c("climate", "elevation")
  if(geodata %in% geodownload){
    geodata <- geoget_remote(download = geodata, country = country)
  } else if (geodata == "basemap") {
    geodata <- geoget_basemap(crs = crs, attributes = attributes, continent = continent, country = country, region = region, union = union, plot = plot, antarctica = antarctica)
  } else {
    geodata <- geoget_path(filepath = geodata)
  }

  return(geodata)
}


#' Get spatial data from local path
#'
#' Open spatial data from a local path (supports both raster and vector formats).
#'
#' @param filepath Path to raster (RasterLayer or RasterStack) or vector data
#'
#' @keywords internal
#' @return
#' @export
#' @family <geodata>
geoget_path <- function(filepath){

  if(!file.exists(filepath)){stop("Path not found")}


  # vec <- try(sf::st_read(filepath), silent = TRUE)
  # if(all(class(vec) != "try-error")){message("Vector data loaded")}
  vec <- tryCatch(sf::st_read(dsn = filepath), error=function(e){})
  if(!is.null(vec)){ras <- NULL
  } else {
    ras <- tryCatch(raster::raster(x = filepath), error=function(e){})
    if(!is.null(ras) & ras@file@nbands != 1) {
      ras <- tryCatch(raster::stack(x = filepath), error=function(e){})
    }
  }

  if(is.null(vec) & is.null(ras)){
    stop(paste0("Cannot open ", filepath))
    }

  if(is.null(vec) & !is.null(ras)){
    rasvec <- ras
    message("Raster data loaded")
    }

if(!is.null(vec) & is.null(ras)){
    rasvec <- vec
    message("Vector data loaded")
    }

  return(rasvec)

}

#' Download geodata
#'
#' @param download name of data to download ("climate" or "elevation")
#' @param country Required in case of "elevation" to crop the data
#' @family <geodata>
#' @keywords internal
#' @return
#' @export
#'
geoget_remote <- function(download, country = NULL){
  if(download == "climate"){
  geodata <- raster::getData(name = "worldclim", var = "bio", res = 10)
  }
  if(download == "elevation"){
    geodata <- raster::getData(name = "alt", country = country)
    names(geodata) <- "elevation"
  }
  if(download == "basemap"){

  }
  geodata
}

#' Get basemap polygon for the entire world or specific region
#'
#' Download, prepare and (optionally) filter Natural Earth data.
#'
#' @param crs Coordinate Reference System to transform to (default is Eckert IV)
#' @param attributes Whether to keep polygon attributes, or return only geometries
#' @param continent Optional filter by continent
#' @param country Optional filter by country
#' @param region Optional filter by region
#' @param union Should the polygons be merged into a single polygon with resolved boundaries?
#' @param plot Should the resulting polygon(s) be plotted
#' @param antarctica Should Antarctica be kept?
#'
#' @return
#' @export
#' @family <geodata>
#'
#' @examples
#' geoget_basemap(plot = TRUE, union = TRUE)
geoget_basemap <- function(crs = NULL, attributes = FALSE, continent = NULL, country = NULL, region = NULL, union = FALSE, plot = FALSE, antarctica = FALSE){

  wrld_basemap <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
  wrld_wrap <- sf::st_wrap_dateline(wrld_basemap, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)

  if(is.null(crs)){crs <- "+proj=eck4"}
  wrld_proj <- sf::st_transform(wrld_wrap, crs = crs)

  basemap <- wrld_proj %>% sf::st_make_valid()

  if(!purrr::is_empty(continent )){
    selection <- continent
    basemap <- basemap %>%
      dplyr::filter(continent %in% selection)
  }
  if(!purrr::is_empty(country )){
    selection <- country
    basemap <- basemap %>%
      dplyr::filter(name %in% selection)
  }
  if(!purrr::is_empty(region )){
    selection <- region
    basemap <- basemap %>%
      dplyr::filter(region %in% selection)
  }

  if(antarctica == FALSE){
    basemap <- basemap %>%
      dplyr::filter(geounit != "Antarctica") %>%
      sf::st_make_valid()
  }

  if(attributes == FALSE){
    basemap <- basemap %>% sf::st_geometry()
  }

  if(union == TRUE){
    basemap <- basemap %>%
      lwgeom::st_snap_to_grid(size = 0.01) %>% # snap to grid of 0.01 meter, otherwise lines within polygons remain after st_union
      sf::st_make_valid() %>%
      sf::st_union()
  }

  if(plot == TRUE){
    print(tmap::tm_shape(basemap) + tmap::tm_polygons())
  }
return(basemap)
}


