

#' Join glottodata with other objects, datasets, or databases.
#'
#' @param glottodata glottodata or glottosubdata
#' @param with Optional: glottodata (class data.frame), a dist object (class dist), or the name of a glottodatabase ("glottobase" or "glottospace")
#' @param id By default, data is joined by a column named "glottocode" or "glottosubcode". In case you want to join using another column, the column name should be specified.
#' @param na.rm Only used when joining with a dist object. By default NAs are kept.
#' @param type In case two glottodata objects are joined, you can specify the type of join: "left" (default), "right", "full", or "inner"
#'
#' @seealso glottosplit
#'
#' @export
#' @return glottodata or glottosubdata, either with or without metatables. Object is returned as a data.frame or list, depending on the input.
#'
#' @examples
#' \donttest{
#' glottodata <- glottoget("demodata")
#' glottodata_space <- glottojoin(glottodata, with = "glottospace")
#' glottodata_base <- glottojoin(glottodata, with = "glottobase")
#'
#' # Join with a dist object
#' glottodata <- glottoget("demodata", meta = TRUE)
#' dist <- glottodist(glottodata)
#' glottodata_dist <- glottojoin(glottodata, with = dist)
#'
#' # Join glottosubdata tables:
#' glottosubdata <- glottocreate(glottocodes = c("yucu1253", "tani1257"),
#' variables = 3, groups = c("a", "b"), n = 2, meta = FALSE)
#' glottodatatable <- glottojoin(glottodata = glottosubdata)
#' }
glottojoin <- function(glottodata, with = NULL, id = NULL, na.rm = FALSE, type = "left"){
  if(glottocheck_isglottosubdata(glottodata) ){
    if(is.null(with)){# join glottosubdata
    return(glottojoin_subdata(glottosubdata = glottodata) )
  } else if(is_dist(with)){
    return(glottojoin_dist(glottodata = glottodata, id = id, glottodist = with, na.rm = na.rm) )
  } else if (glottocheck_hasmeta(with) & is.null(id)){
    return(glottojoin_meta(glottodata = glottodata, glottometa = with) )
  } else if (glottocheck_isstructure(with)){
    return(glottojoin_structure(glottodata = glottodata, structure = with) )
  } else if(glottocheck_hasmeta(with) & is.null(id) & is.null(type)){
    return(c("data" = list(glottodata), with) )
  } else {
    return(glottojoin_data(glottodata = glottodata, with = with, type = type, id = id))
  }
  }

    if(glottocheck_isglottodata(glottodata) & !is.null(with)){
    glottodata <- glottosplit(glottodata)[[1]]
      if(is_dist(with)){
        return(glottojoin_dist(glottodata = glottodata, id = id, glottodist = with, na.rm = na.rm) )
      } else if(glottocheck_hasmeta(with)){
        return( glottojoin_meta(glottodata = glottodata, glottometa = with) )
      } else if(glottocheck_isstructure(with)){
        return(glottojoin_structure(glottodata = glottodata, structure = with) )
      } else if(glottocheck_isglottodata(with)){
        return(glottojoin_data(glottodata = glottodata, with = with, type = type) )
      } else if(is.character(with)){
          if(with == "glottobase"){
            return(glottojoin_base(glottodata = glottodata, id = id) )
          } else if(with == "glottospace"){
            return(glottojoin_space(glottodata = glottodata, id = id) )
          }
      } else if(glottocheck_hasmeta(with) & is.null(id) & is.null(type)){
        return(c("data" = list(glottodata), with) )
      } else {
        return(glottojoin_data(glottodata = glottodata, with = with, type = type, id = id))
      }
  }

}

#' Join glottodata with a dist object
#'
#' @param glottodata User-provided glottodata
#' @param glottodist A glottodist object
#' @param id By default, 'glottocode' or 'glottosubcode' is used as id. However, if id is specified, join between data and dist object will be done based on another column.
#' @param na.rm Default is to keep NAs.
#' @return Data frame
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' dist <- glottodist(glottodata)
#' glottodata_dist <- glottojoin_dist(glottodata = glottodata, glottodist = dist)
#'
#' # After joining, you can subset the distance columns by using the IDs:
#' glottodata_dist[, glottodata_dist$glottocode]
#'
#' glottosubdata <- glottoget("demosubdata", meta = TRUE)
#' dist <- glottodist(glottodata = glottosubdata)
#' glottodata_dist <- glottojoin_dist(glottodata = glottosubdata, glottodist = dist, na.rm = TRUE)
glottojoin_dist <- function(glottodata, glottodist, id = NULL, na.rm = FALSE){

  glottodata <- glottosimplify(glottodata)

  if(is.null(id)){
  id <- glottocheck_id(glottodata)
  }

  if(na.rm == TRUE){
    distmat <- glottoclean_dist_rmna(glottodist = glottodist)
  } else{
    distmat <- contransform_distmat(dist = glottodist)
  }

  distdf <- as.data.frame(distmat)
  distdf <- tibble::rownames_to_column(distdf, var = "id")

  colnames(glottodata)[colnames(glottodata) == id] <- "id"

  dfjoin <- dplyr::inner_join(glottodata, distdf, by = "id") # not using by = c("a" = "b") because only works with character strings and not with id object
  colnames(dfjoin)[colnames(dfjoin) == "id"] <- id
  return(dfjoin)

}

#' Join glottodata with glottobase
#'
#' Keeps all rows from glottodata. Can be used to add features from glottobase.
#'
#' @param glottodata User-provided glottodata
#' @param id Optional, name of column with glottocodes
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = FALSE)
#' glottojoin_base(glottodata)
glottojoin_base <- function(glottodata, id = NULL){
  id <- contrans_id2gc(id)
  glottobase <- glottoget_glottobase()
  glottodata <- dplyr::left_join(x = glottodata, y = glottobase, by = id)
  sf::st_sf(glottodata)
}


#' Add coordinates to glottodata.
#'
#' Join glottodata with glottospace (keeps all rows from glottodata). It is recommended to use the function glottospace().
#'
#'
#' @param glottodata User-provided glottodata
#' @param id Optional, name of column with glottocodes
#' @noRd
#' @seealso glottospace_addcoords glottospace_addcoords
#' @examples
#' glottodata <- glottoget("demodata", meta = FALSE)
#' glottojoin_space(glottodata)
glottojoin_space <- function(glottodata, id = NULL){
  id <- contrans_id2gc(id)
  if(!is_sf(glottodata)){
  # glottodata <- merge(x = glottospace, y = glottodata, by = id, all.y = TRUE, all.x = FALSE)
  glottospace <- glottoget_glottospace()
  glottodata <- dplyr::left_join(x = glottodata, y = glottospace, by = id)
  glottodata <- sf::st_sf(glottodata)
  } else {
  message("Object glottodata is already spatial")
  }
  glottodata
}

#' Join glottosubdata (a list of glottodata tables for multiple languages) into a single glottodata object
#'
#'
#' @param glottosubdata glottosubdata object. Column names across languages should be identical.
#'
#' @return A glottosubdata object
#' @noRd
#' @examples
#' glottosubdata <- glottoget("demosubdata")
#' glottojoin_subdata(glottosubdata = glottosubdata)
glottojoin_subdata <- function(glottosubdata){
  if(glottocheck_isglottosubdata_complex(glottosubdata)){

  splitted <- glottosplitmergemeta(glottosubdata)
  glottosubdata <- splitted[[1]]

  glottocheck_lscolcount(glottosubdata) # stops if number of columns is not identical
  # glottosubdata <- do.call("rbind", glottosubdata) # This changes the class of the object to glottodata which is not desirable. Alternative approaches: data.table::rbindlist or plyr::rbind.fill
  glottosubdata <- plyr::rbind.fill(glottosubdata)
  glottosubdata <- tibble::remove_rownames(glottosubdata)

  if(any(!is.na(splitted[[2]]))){glottosubdata <- c("glottosubdata" = list(glottosubdata), splitted[[2]]) }
  }
  return(glottosubdata)

}

#' Join two glottodata tables.
#'
#' Join two glottodata tables.
#'
#'
#' @param glottodata User-provided glottodata
#' @param with User-provided glottodata
#' @param id Optional, name of column with glottocodes
#'
#' @noRd
#' @seealso glottospace_addcoords
#' @examples
#' glottodatax <- glottoget("demodata")
#' glottodatay <- glottodatax[, c(1,2)]
#' glottodatay[,2] <- c("x", "y", "z", "z","y", "x")
#' colnames(glottodatay)[2] <- "var004"
#' glottojoin_data(glottodatax, glottodatay)
glottojoin_data <- function(glottodata, with, type = "left", id = NULL, ...){
  id <- contrans_id2gc(id)
  if(type == "left"){
    joined <- dplyr::left_join(x = glottodata, y = with, by = id, ...)
  }
  if(type == "right"){
    joined <- dplyr::right_join(x = glottodata, y = with, by = id, ...)
  }
  if(type == "inner"){
    joined <- dplyr::inner_join(x = glottodata, y = with, by = id, ...)
  }
  if(type == "full"){
    joined <- dplyr::full_join(x = glottodata, y = with, by = id, ...)
  }
  return(joined)
}

#' Join glottodata with glottometa
#'
#' @param glottodata A glottodata table, or a glottodata list with one table.
#' @param glottometa A glottometa table, or a glottometa list
#' @param name Optional name(s) of glottometa tables
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottometa <- glottodata[-1]
#' glottodata <- glottodata[[1]]
#' glottojoin_meta(glottodata, glottometa)
glottojoin_meta <- function(glottodata, glottometa){

  stopifnot(glottocheck_hasmeta(glottometa))

  if(glottocheck_isglottodata(glottodata)){
      if(is_sf(glottodata)){
    glottodata <- list("glottodata" = glottodata)
  } else if(!is_list(glottodata)){
    glottodata <- list("glottodata" = glottodata)
    names(glottodata) <- "glottodata"
  }
  } else if(glottocheck_isglottosubdata(glottodata)){
    if(is_sf(glottodata)){
      glottodata <- list("glottosubdata" = glottodata)
    } else if(!is_list(glottodata)){
      glottodata <- list("glottosubdata" = glottodata)
      names(glottodata) <- "glottosubdata"
    }
  } else{
    stop("Input is not glottodata or glottosubdata")
  }

  c(glottodata, glottometa)


}


#' Join glottodata with structure table
#'
#' @param glottodata glottodata/glottosubdata table
#' @param glottometa A structure table
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' structure <- glottodata[["structure"]]
#' glottodata <- glottosimplify(glottodata)
#' glottojoin_structure(glottodata, structure)
glottojoin_structure <- function(glottodata, structure){

  glottodata <- glottosimplify(glottodata)

  if(glottocheck_isglottodata(glottodata)){
    if(is_sf(glottodata)){
      glottodata <- list("glottodata" = glottodata)
    } else if(!is_list(glottodata)){
      glottodata <- list("glottodata" = glottodata)
      names(glottodata) <- "glottodata"
    }
  } else if(glottocheck_isglottosubdata(glottodata)){
    if(is_sf(glottodata)){
      glottodata <- list("glottosubdata" = glottodata)
    } else if(!is_list(glottodata)){
      glottodata <- list("glottosubdata" = glottodata)
      names(glottodata) <- "glottosubdata"
    }
  } else{
    stop("Input is not glottodata or glottosubdata")
  }

  structure <- list("structure" = structure)

  c(glottodata, structure)


}
