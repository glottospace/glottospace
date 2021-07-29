#' Join a dataset with a dist object
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
join_glottodist <- function(data, id = "glottocode", dist, rm.na = TRUE){

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

  colnames(data)[colnames(data) == id] <- "id"
  data.table::setDT(data, keep.rownames = "id")

  dfjoin <- dplyr::inner_join(data, distdf, by = "id") # not using by = c("a" = "b") because only works with character strings and not with id object
  colnames(dfjoin)[colnames(dfjoin) == "id"] <- id
  return(dfjoin)

}

join_glottodata <- function(data, glottodata, id = "glottocode"){
  dplyr::left_join(x = data, y = glottodata, by = id)
}
