#' Get geographic data
#'
#' Load spatial data either from local path (supports both raster and vector
#' formats), or download from remote server.
#' @param geodata Either a filepath to locally stored geodata, or the name of
#'   geodata to download ("elevation" or "climate"). This calls raster::getData.
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
geoget <- function(geodata, country = NULL){
  geodownload <- c("climate", "elevation")
  if(geodata %in% geodownload){
    geodata <- geoget_remote(download = geodata, country = country)
  } else {
    geodata <- geoget_path(path = geodata)
  }

  return(geodata)
}


#' Get spatial data from local path
#'
#' Open spatial data from a local path (supports both raster and vector formats).
#'
#' @param path Path to raster (RasterLayer or RasterStack) or vector data
#'
#' @keywords internal
#' @return
#' @export
#' @family <geodata>
geoget_path <- function(path){

  if(!file.exists(path)){stop("Path not found")}


  # vec <- try(sf::st_read(path), silent = TRUE)
  # if(all(class(vec) != "try-error")){message("Vector data loaded")}
  vec <- tryCatch(sf::st_read(path), error=function(e){})
  if(!is.null(vec)){ras <- NULL
  } else {
    ras <- tryCatch(raster::raster(path), error=function(e){})
    if(!is.null(ras) & ras@file@nbands != 1) {
      ras <- tryCatch(raster::stack(path), error=function(e){})
    }
  }

  if(is.null(vec) & is.null(ras)){
    stop(paste0("Cannot open ", path))
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
  geodata
}



