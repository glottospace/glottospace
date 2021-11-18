
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
