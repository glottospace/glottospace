
path <- "D:/Global/Biodiversity/wwf_terr_ecos/wwf_terr_ecos.shp"
path <- "D:/Global/Topography/Geomorpho90m/250m/Global/dtm_roughness_merit.dem_m_250m_s0..0cm_2018_v1.0.tif"

#' Open geodata (raster or vector format)
#'
#' @param path Path to raster or vector file
#' @param name Optional
#'
#' @return
#' @export
#'
#' @examples
get_geodata <- function(path, name = NULL){
  rasvec <- get_rasvec(path = path)

  if(!is.null(rasveclist$raster)){
    if(!is.null(name)){names(ras) <- name}
  }

  if(!is.null(rasveclist$vector)){

  }
}

#' Try to open raster/vector data and return list of two
#'
#' @param path Path to raster or vector data
#'
#' @return A list of two
#'
#' @examples
get_rasvec <- function(path){

  if(!file.exists(path)){stop("Path not found")}


  # vec <- try(sf::st_read(path), silent = TRUE)
  # if(all(class(vec) != "try-error")){message("Vector data loaded")}
  vec <- tryCatch(sf::st_read(path), error=function(e){})

  if(!is.null(vec)){message("Vector data loaded")
  } else {
    ras <- tryCatch(raster::raster(path), error=function(e){})
    if(!is.null(ras)){message("Raster data loaded")}
    }

  if(is.null(vec) & is.null(ras)){
    stop(paste0("Cannot open ", path))}

  rasvec <- unlist(list(ras, vec))[[1]]

}



# gs_datageo <- function(name = NULL, region = NULL, res = NULL, path = NULL, selection = "default"){
#

#
#   if(name == "roughness"){
#     if(res == "250m" & region == "Global"){file <- "dtm_roughness_merit.dem_m_250m_s0..0cm_2018_v1.0.tif"}
#     path <- gs_geopath(pathgroup = "Topography/Geomorpho90m", res = res, region = region, file = file)
#     ras <- raster::raster(path)
#     cat(paste("Loading raster:", name, "at", res, "resolution for:", region, "\n"))
#     if(!is.null(ras)){
#       names(ras) <- name
#       out <- ras
#       cat(paste0("Roughness merit dem loaded (", file, ")"))
#     }
#   }
#
#   if(name == "elevation"){
#     if(res == "250m" & region == "South America"){file <- "SRTM250mSA.tif"}
#     path <- gs_geopath(pathgroup = "Topography/SRTM", res = res, region = region, file = file)
#     ras <- raster::raster(path)
#     cat(paste("Loading raster:", name, "at", res, "resolution for:", region, "\n"))
#     if(!is.null(ras)){
#       names(ras) <- name
#       out <- ras
#       cat(paste0("SRTM data loaded (", file, ")"))
#     }
#   }
#
#   if(name == "nutrients"){
#     if(res == "1km" & region == "Global"){file <- "sq1.asc"} else {file <- ""}
#     path <- gs_geopath(pathgroup = "Soils/HWSD", res = res, region = region, file = file)
#     ras <- raster::raster(path)
#     cat(paste("Loading raster:", name, "at", res, "resolution for:", region, "\n"))
#     if(!is.null(ras)){
#       crs(ras) <- CRS('+init=EPSG:4326')
#       names(ras) <- name
#       out <- ras
#       cat(paste0("Nutrient availability loaded (", file, ")"))
#       cat(paste("
#     Legend:
#     1 = no significant constraint
#     2 = moderate constraints
#     3 = severe constraints
#     4 = very severe constraints
#     5 = mainly non-soil
#     6 = permafrost area
#     7 = water bodies
#               \n \n"))
#     }
#   }
#
#   if(name == "worldclim"){
#     if(res == "1km" & region == "South America"){file <- paste("wc2.0_bio_30s_", sprintf("%02d", seq(1,19)), '.tif', sep='')}
#     paths <- gs_geopath(pathgroup = "Climate/WORLDCLIM2", res = "1km", region = "South America", file = file)
#     stack <- raster::stack(paths)
#     names(stack) <- paste0("bio", sprintf("%02d", seq(1,19)))
#     message(paste("Loading raster:", name, "at", res, "resolution for:", region))
#     if (region == "Global"){
#       stack <- raster::getData(name = "worldclim", var = "bio", res = 10)
#     }
#     if(selection == "default"){
#       ras <- stack
#     } else {
#       ras <- stack[[selection]]
#     }
#     if(!is.null(ras)){
#       out <- ras
#       cat(paste("WorldClim 2.0 data loaded (", file, ") \n"))
#     }
#   }
#
#   if(name == "ecoregion"){
#     file <- "wwf_terr_ecos.shp"
#     path <- gs_geopath(pathgroup = "Biodiversity/wwf_terr_ecos", file = file)
#     vec <- st_read(path)
#     cat(paste("Loading shapefile:", name))
#     if(!is.null(vec)){
#       if(selection == "default"){
#         vec <- vec[, "ECO_NAME"]
#         colnames(vec)[1] <- name
#       } else if(selection != "default" & is.character(selection)){
#         vec <- vec[, selection]
#       } else if(selection == "all" | is.null(selection)){
#         vec <- vec
#       }
#       out <- vec
#       cat(paste0("Terrestrial Ecoregions of the World loaded (", file, ")"))
#     }
#   }
#
#   if(name == "rivervect"){
#     if(region == "South America"){file <- "South America_constant_Dd.gpkg"}
#     path <- gs_geopath(pathgroup = "Hydrography/drainage density", region = region, file = file)
#     vec <- st_read(path)
#     cat(paste("Loading shapefile:", name))
#     if(!is.null(vec)){
#       if(selection == "default"){
#         vec <- vec
#       } else if(selection != "default" & is.character(selection)){
#         vec <- vec[, selection]
#       } else if(selection == "all" | is.null(selection)){
#         vec <- vec
#       }
#       out <- vec
#       cat(paste0("River network constant Dd loaded (", file, ") for region: ", region))
#     }
#   }
#
