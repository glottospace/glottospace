#' Add S3 class to object
#'
#'
#' @noRd
#' @examples
#'
#' glottoget("demodata")
add_class <- function(object, class){
  oldunique <- class(object)[class(object) %nin% class]
  class(object) <- c(class, oldunique)
  invisible(object)
}

#' Conditionally add glottoclass
#'
#' @param data
#'
#'
#' @noRd
contrans_glottoclass <- function(data){
  if(glottocheck_isglottodata(data) & !is_sf(data)){
    data <- add_class(data, "glottodata")
  } else if(glottocheck_isglottosubdata(data) & !is_sf(data)){
    data <- add_class(data, "glottosubdata")
  } else if(glottocheck_isglottodata(data) & is_sf(data)){
    data <- add_class(data, c("glottodata", "glottospace") )
  } else if(glottocheck_isglottosubdata(data) & is_sf(data)){
    data <- add_class(data, c("glottosubdata", "glottospace") )
  }
  return(data)
}
