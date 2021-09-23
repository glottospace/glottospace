#' Join glottodata with a dist object
#'
#' @param data A data.frame
#' @param dist A dist object
#' @param rm.na Default is to remove NAs.
#' @param id Column with IDs in \code{data} that match names of dist object. By default, the "glottocode" column is used.
#'
#' @return Data frame
#' @export
#'
#' @examples
#' path <- "C:/Users/sjnor/surfdrive/Projecten en schrijfsels/Papers in progress/Isolates/output/south america all.gpkg"
#' sa <- sf::st_read(dsn = path)
#' dist <- pointdist_bird(sa, label = "glottocode")
#' distdf <- joindatadist(data = sa, idcol = "glottocode", dist = dist)
#'
#' After joining, you can subset the distance columns by using the IDs:
#' distdf[, distdf$glottocode]
join_glottodist <- function(glottodata, id = NULL, dist, rm.na = TRUE){
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
  data.table::setDT(glottodata, keep.rownames = "id")

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
#' glottodata <- get_glottodata(meta = FALSE)
#' join_glottobase(glottodata)
join_glottobase <- function(glottodata, id = NULL){
  id <- contrans_id2gc(id)
  glottobase <- get_glottobase()
  dplyr::left_join(x = glottodata, y = glottobase, by = id)
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
#' glottodata <- get_glottodata(meta = FALSE)
#' join_glottospace(glottodata)
join_glottospace <- function(glottodata, id = NULL){
  id <- contrans_id2gc(id)
  glottospace <- get_glottospace()
  dplyr::left_join(x = glottodata, y = glottospace, by = id)
}

#' Join glottodata for multiple languages into a single glottodata object
#'
#'
#' @param langlist A list of glottodata objects. Column names across languages should be identical.
#'
#' @return A single glottodata object
#' @export
#'
#' @examples
#' glottodata <- get_glottodata("glottosubdata.xlsx", meta = FALSE)
#' join_glottodata(langlist = glottodata)
join_glottodata <- function(langlist){
  checkdata_lscolcount(langlist) # stops if number of columns is not identical
  do.call("rbind", langlist) # alternative approaches: data.table::rbindlist or plyr::rbind.fill
}


