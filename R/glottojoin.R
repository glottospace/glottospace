

#' Join glottodata with other objects, datasets, or databases.
#'
#' @param glottodata glottodata or glottosubdata
#' @param with Optional: glottodata (class data.frame), a dist object (class dist), or the name of a glottodatabase ("glottobase" or "glottospace")
#' @param id By default, data is joined by a column named "glottocode" or "glottosubcode". In case you want to join using another column, the column name should be specified.
#' @param rm.na Only used when joining with a dist object. By default NAs are kept.
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
glottojoin <- function(glottodata, with = NULL, id = NULL, rm.na = FALSE, type = "left"){
  if(glottocheck_isglottosubdata(glottodata) ){
    if(is.null(with)){# join glottosubdata
    joined <- glottojoin_subdata(glottosubdata = glottodata)
  } else if (glottocheck_hasmeta(with) & is.null(id)){
    joined <- glottojoin_meta(glottodata = glottodata, glottometa = with)
  } else {
    message("Unable to join glottosubdata with this type of data.")
  }
  return(joined)
  } else if(glottocheck_isglottodata(glottodata) & !is.null(with)){
    glottodata <- glottosplit(glottodata)[[1]]
      if(is_dist(with)){
      joined <- glottojoin_dist(glottodata = glottodata, id = id, dist = with, rm.na = rm.na)
      } else if(glottocheck_hasmeta(with)){
      joined <- glottojoin_meta(glottodata = glottodata, glottometa = with)
      } else if(glottocheck_isglottodata(with)){
        joined <- glottojoin_data(glottodata = glottodata, with = with, type = type)
      } else if(is.character(with)){
          if(with == "glottobase"){
            joined <- glottojoin_base(glottodata = glottodata, id = id)
          } else if(with == "glottospace"){
            joined <- glottojoin_space(glottodata = glottodata, id = id)
          }
      } else(message("Unable to join glottodata with this type of data.") )
    return(joined)
  } else if (!is.null(with) ){
    message("Input data is not glottodata or glottosubdata. Trying to join anyway. ")
    if(glottocheck_hasmeta(with) & is.null(id)){
      joined <- c("data" = list(glottodata), with)
    } else {
    joined <- glottojoin_data(glottodata = glottodata, with = with, type = type, id = id)
    }
    return(joined)
  }


}

#' Join glottodata with a dist object
#'
#' @param glottodata User-provided glottodata
#' @param dist A dist object
#' @param rm.na Default is to keep NAs.
#' @param id Column with IDs in \code{glottodata} that match names of dist object. By default, the "glottocode" column is used.
#' @return Data frame
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' dist <- glottodist(glottodata)
#' glottodata_dist <- glottojoin_dist(glottodata = glottodata, dist = dist)
#'
#' # After joining, you can subset the distance columns by using the IDs:
#' glottodata_dist[, glottodata_dist$glottocode]
glottojoin_dist <- function(glottodata, id = NULL, dist, rm.na = FALSE){

  glottodata <- glottosimplify(glottodata)

  id <- contrans_id2gc(id)
  distmat <- as.matrix(dist)

  if(rm.na == TRUE){
    rowna <- rowSums(is.na(distmat))
    colna <- colSums(is.na(distmat))

    rmcol <- which(colSums(is.na(distmat)) > min(colna))
    rmrow <- which(rowSums(is.na(distmat)) > min(rowna))

    if(!purrr::is_empty(rmcol)){  distmat <- distmat[,-rmcol] }
    if(!purrr::is_empty(rmrow)){  distmat <- distmat[-rmrow,] }
  }

  distdf <- as.data.frame(distmat)
  data.table::setDT(distdf, keep.rownames = "id")

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
  glottospace <- glottoget_glottospace()
  glottodata <- dplyr::left_join(x = glottodata, y = glottospace, by = id)
  sf::st_sf(glottodata)
}

#' Join glottosubdata (a list of glottodata tables for multiple languages) into a single glottodata object
#'
#'
#' @param glottosubdata A list of glottodata objects. Column names across languages should be identical.
#'
#' @return A single glottodata object
#' @noRd
#' @examples
#' glottosubdata <- glottoget("demosubdata")
#' glottojoin_subdata(glottosubdata = glottosubdata)
glottojoin_subdata <- function(glottosubdata){

  splitted <- glottosplitmergemeta(glottosubdata)
  glottodata <- splitted[[1]]

  glottocheck_lscolcount(glottodata) # stops if number of columns is not identical
  glottodata <- do.call("rbind", glottodata) # alternative approaches: data.table::rbindlist or plyr::rbind.fill
  glottodata <- tibble::remove_rownames(glottodata)

  if(any(!is.na(splitted[[2]]))){glottodata <- c("data" = list(glottodata), splitted[[2]]) }

  return(glottodata)

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

  stopifnot(glottocheck_isglottodata(glottodata) | glottocheck_isglottosubdata(glottodata) )
  stopifnot(glottocheck_hasmeta(glottometa))

  if(is_sf(glottodata)){
    glottodata <- list("glottodata" = glottodata)
  } else if(!is_list(glottodata)){
    glottodata <- list("glottodata" = glottodata)
    names(glottodata) <- "glottodata"
  }

  c(glottodata, glottometa)


}
