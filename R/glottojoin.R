

#' Join glottodata with other objects, datasets, or databases.
#'
#' @param glottodata A glottodata table or glottodata list.
#' @param with Optional: glottodata (class data.frame), a dist object (class dist), or the name of a glottodatabase ("glottobase" or "glottospace")
#' @param id By default, data is joined by a column named "glottocode". If the glottocodes are in another column, the column name should be specified.
#' @param rm.na Only used when joining with a dist object. By default NAs are kept.
#'
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottoget_path()
#' glottodata <- glottojoin(glottodata, with = "glottospace")
#' glottodata <- glottojoin(glottodata, with = "glottobase")
#'
#' # Join with a dist object
#' dist <- geodist(glottodata)
#' glottodatadist <- glottojoin(glottodata, with = dist)
#'
#' # Join a list of glottodata tables:
#' glottodatalist <- glottocreate_subdata(glottocodes = c("yucu1253", "tani1257"), variables = 3, groups = c("a", "b"), n = 2, meta = FALSE)
#' glottodatatable <- glottojoin(glottodata = glottodatalist)
#'
glottojoin <- function(glottodata, with = NULL, id = NULL, rm.na = FALSE){
  if(is_list(glottodata) & is.null(with)){
    joined <- join_glottodatalist(glottodatalist = glottodata)
  } else if(!is.null(with)){
    if(is_dist(with)){
    joined <- join_glottodist(glottodata = glottodata, id = id, dist = with, rm.na = rm.na)
    } else if(is_list(with)){
    joined <- join_glottometa(glottodata = glottodata, glottometa = with)
  } else if(with == "glottobase"){
    joined <- join_glottobase(glottodata = glottodata, id = id)
  } else if(with == "glottospace"){
    joined <- join_glottospace(glottodata = glottodata, id = id)
  }  else if(is.data.frame(glottodata) & is.data.frame(with)){
    joined <- join_glottodata(glottodata = glottodata, with = with)
  } else(message("Class of input data not supported.") )
  }
return(joined)
}

#' Join glottodata with a dist object
#'
#' @param glottodata User-provided glottodata
#' @param dist A dist object
#' @param rm.na Default is to keep NAs.
#' @param id Column with IDs in \code{glottodata} that match names of dist object. By default, the "glottocode" column is used.
#'
#' @return Data frame
#' @export
#'
#' @examples
#' distdf <- join_glottodist(glottodata = glottodata, dist = dist)
#'
#' After joining, you can subset the distance columns by using the IDs:
#' distdf[, distdf$glottocode]
join_glottodist <- function(glottodata, id = NULL, dist, rm.na = FALSE){
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
#'
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottoget_path(meta = FALSE)
#' join_glottobase(glottodata)
join_glottobase <- function(glottodata, id = NULL){
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
#' @keywords internal
#' @return
#' @export
#' @seealso glottodata_makespatial glottodata_addcoords
#' @examples
#' glottodata <- glottoget_path(meta = FALSE)
#' join_glottospace(glottodata)
join_glottospace <- function(glottodata, id = NULL){
  id <- contrans_id2gc(id)
  glottospace <- glottoget_glottospace()
  glottodata <- dplyr::left_join(x = glottodata, y = glottospace, by = id)
  sf::st_sf(glottodata)
}

#' Join glottosubdata (a list of glottodata tables for multiple languages) into a single glottodata object
#'
#'
#' @param glottodatalist A list of glottodata objects. Column names across languages should be identical.
#'
#' @return A single glottodata object
#' @export
#'
#' @examples
#' glottodatalist <- glottoget_glottodata("glottosubdata.xlsx", meta = FALSE)
#' join_glottodata(glottodatalist = glottodatalist)
join_glottodatalist <- function(glottodatalist){
  checkdata_lscolcount(glottodatalist) # stops if number of columns is not identical
  do.call("rbind", glottodatalist) # alternative approaches: data.table::rbindlist or plyr::rbind.fill
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
#' @keywords internal
#' @return
#' @export
#' @seealso glottodata_makespatial glottodata_addcoords
#' @examples
#' glottodatax <- glottoget_path(meta = FALSE)
#' glottodatay <- glottoget_path(meta = FALSE)
#' join_glottodata(glottodatax, glottodatay)
join_glottodata <- function(glottodata, with, id = NULL){
  id <- contrans_id2gc(id)
  dplyr::left_join(x = glottodata, y = with, by = id)
}

#' Join glottodata with glottometa
#'
#' @param glottodata A glottodata table, or a glottodata list with one table.
#' @param glottometa A glottometa table, or a glottometa list
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottoget_path(meta = TRUE)
#' glottometa <- glottodata[-1]
#' glottodata <- glottodata[[1]]
#' join_glottometa(glottodata, glottometa)
join_glottometa <- function(glottodata, glottometa, name = NULL){

  if(is_sf(glottodata)){
    glottodata <- list("glottodata" = glottodata)
  } else if(!is_list(glottodata)){
    glottodata <- list("glottodata" = glottodata)
    names(glottodata) <- "glottodata"
  }

  if(!is_list(glottometa)){
    glottometa <- list(glottometa)
    names(glottometa) <- name
  }

  c(glottodata, glottometa)


}
