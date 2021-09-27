#' Get geographic data
#'
#' Load spatial data either from local path (supports both raster and vector formats), or download from remote server.
#' @param path Path to geodata
#' @param download Name of geodata to download (wrapper around raster::getData). Currently, the following are supported: "elevation" and "climate"
#'
#' @return
#' @export
#' @family <geodata>
#'
#' @examples
#' From local path (not run):
#' get_geodata(path = "D:/data/vector.shp")
#' get_geodata(path = "D:/data/raster.tif")
#'
#' Download:
#' get_geodata(download = "elevation", country = "Netherlands")
#' get_geodata(download = "climate")
get_geodata <- function(path = NULL, download = NULL, country = NULL){
  if(!is.null(path) & is.null(download)){
    geodata <- get_geodata_path(path = path)
  }
  if(is.null(path) & !is.null(download)){
    geodata <- get_geodata_download(download = download, country = country)
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
get_geodata_path <- function(path){

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
#' @param country In some cases, this is required to crop the data
#' @family <geodata>
#' @keywords internal
#' @return
#' @export
#'
get_geodata_download <- function(download, country = NULL){
  if(download == "climate"){
  raster::getData(name = "worldclim", var = "bio", res = 10)
  }
  if(download == "elevation"){
    geodata <- raster::getData(name = "alt", country = country)
    names(geodata) <- "elevation"
    geodata
  }
}



