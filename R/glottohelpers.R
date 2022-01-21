#' Create empty distance matrix
#'
#' @param names
#'
#' @return
#' @export
#' @noRd
#' @examples
#' gs_emptydistmat(names = glottocodes)
gs_emptydistmat <- function(names){
  outputmat <- matrix(data = NA,
                      nrow = length(names),
                      ncol = length(names),
                      dimnames = list(names, names))
  return(outputmat)
}


as.ordfact <- function(x = NULL, levels = NULL){ # alternatively, use: https://forcats.tidyverse.org/
  dtf <- as.factor(as.matrix(x))
  lvl <- unlist(strsplit(x = levels, split = '[,;]+'))
  dtot <- factor(x = dtf, levels = lvl, ordered = TRUE)
  df <- as.data.frame(dtot)
  return(df)
}

geodata_rename_column <- function(geodata, oldname, newname){
  geodata %>%
  dplyr::mutate() %>% # https://github.com/r-spatial/sf/issues/1472 Empty mutate() also sets geom to last column
  dplyr::rename(newname = oldname)
}

glottovars <- function(){
  colnames(glottoget("glottobase"))
}

recode_df <- function(data, old, new){
  apply(data, MARGIN = 2, FUN = recode_vector, old = old, new = new)
}

recode_vector <- function(vector, old, new){
  vector[which(vector %in% old)] <- new
  return(vector)
}

#' Find R source script of a function
#'
#' @param funcname Either a function object (without brackets) or a character string
#'
#' @return
#' @noRd
funcsource <- function(funcname){
  if(is.character(funcname)){funcname <- get(funcname)}
  # https://stackoverflow.com/questions/32747242/find-the-source-file-containing-r-function-definition/32749240#32749240
  srcfile <- attr(attr(funcname,"srcref"),"srcfile")
  srcfile$filename
}

contrans_id2gc <- function(id){
  ifelse(is.null(id), id <- "glottocode", id)
}

is_false <- function(x) {
  # NAs are kept
  x == FALSE
}

is_true <- function(x) {
  # NAs are kept
  x == TRUE
}

is_dist <- function(object){
  any(class(object) == "dist")
}

is_list <- function(object){
  any(class(object) == "list")
}

contrans_tb2df <- function(glottodata){
  if(tibble::is_tibble(glottodata)){
    return(as.data.frame(glottodata))
  } else {
    return(glottodata)
  }

}

#' Sum of vector elements
#'
#' Calculate sum of all vector elements that are not NA. If all elements are NA, NA will be returned (in contrast to sum).
#'
#' @param v A vector
#' @noRd
sumna <- function(v){
  if(all(is.na(v)) ){
    return(NA)
  } else {
    return(sum(v, na.rm = TRUE))
  }
  # ifelse(all(is.na(v)), NA, sum(v, na.rm = TRUE))
}

#' Select non-NA elements from vector
#'
#' Select non-NA elements from vector. If all elements are NA, NA will be returned.
#' @noRd
#' @param v
#'
nonna <- function(v, max1 = FALSE){
  sel <- v[!is.na(v)]
  if(purrr::is_empty(sel)){sel <- NA}
  if(length(sel) > 1 & max1 == TRUE){stop("More than one element is non-NA, specify max1 = FALSE if you want to allow for multiple matches.")}
  return(sel)
}

unpack <- function(path = NULL){
  ftar <- list.files(path = path, pattern = ".tar", full.names = TRUE)
  if(!purrr::is_empty(ftar)){
    lapply(ftar, untar, exdir = path)
    f <- ftar
    ext <- ".tar"
  }

  fzip <- list.files(path = path, pattern = ".zip", full.names = TRUE)
  if(!purrr::is_empty(fzip)){
    lapply(fzip, unzip, exdir = path)
    f <- fzip
    ext <- ".zip"
  }

  message(paste(length(f), ext, " files unpacked to:", path))
}


