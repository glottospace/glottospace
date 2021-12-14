
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

contrans_char2obj <- function(){}

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

# xfun::gsub_dir(dir = "C:/Users/sjnor/surfdrive/PROJECTS_SN/SAPPHIRE/R/glottospace/R", pattern = "dummy", replacement = "demo")

is_dist <- function(object){
  any(class(object) == "dist")
}

is_list <- function(object){
  class(object) == "list"
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
#' @param v
#'
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
#'
#' @param v
#'
nonna <- function(v, max1 = FALSE){
  sel <- v[!is.na(v)]
  if(purrr::is_empty(sel)){sel <- NA}
  if(length(sel) > 1 & max1 == TRUE){stop("More than one element is non-NA, specify max1 = FALSE if you want to allow for multiple matches.")}
  return(sel)
}


