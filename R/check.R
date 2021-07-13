#' Check whether rows have missing IDs.
#'
#' @param data User glottodata
#' @param idcol Column index or name with IDs (glottocodes, glottosubcodes, or glottosubsubcodes)
#' @param messages TRUE/FALSE: should messages be generated?
#'
#' @return
#' @export
#'
#' @examples
check_idmissing <- function(data, idcol, messages = TRUE){

  idmissing <- nrow(data[is.na(data[,idcol]),] )

  if(idmissing > 0){
    if(messages == TRUE){
      message(paste(idmissing, ' rows with missing ID'))
      invisible(TRUE)} else{return(TRUE)}

  } else{
    if(messages == TRUE){
      message("No missing IDs")
      invisible(FALSE)} else{return(FALSE)}
  }
}

#' Check wether all IDs are unique
#'
#' @param data User glottodata
#' @param idcol Column index or name with IDs (glottocodes, glottosubcodes, or glottosubsubcodes)
#' @param messages TRUE/FALSE: should messages be generated?
#'
#' @return
#' @export
#'
#' @examples
check_idunique <- function(data, idcol, messages = TRUE){
  # Check whether ids are unique
  freqtab <- data.frame(table(data[,idcol]))
  colnames(freqtab)[1] <- "id"
  colnames(freqtab)[2] <- "n"

  if(any(freqtab$n > 1)){
    if(messages == TRUE){
      duplicate <- freqtab[freqtab$n > 1, ]
      message('IDs are not unique. The following ids have duplicates:')
      print(duplicate)
      invisible(FALSE)
    } else{return(FALSE)}
  } else {
    if(messages == TRUE){message("No duplicate IDs.")
      invisible(TRUE)
    } else {return(TRUE)}

    }
}

check_varlevels <- function(data, messages = TRUE){
  lslevels <- lapply(data, unique)
  varlevels <- unlist(lapply(lslevels, paste, collapse=" , "), use.names = FALSE)
  varlevels <- data.frame(variable = colnames(data), levels = varlevels)

  if(messages == TRUE){
    View(varlevels)
    invisible(lslevels)
  } else{return(lslevels)}

}

check_glottocodes <- function(data, idcol, messages = TRUE){
  existing <- glottocode_exists(data[[idcol]])
  if(sum(!existing) > 0){
    if(messages == TRUE){message("Not all IDs are valid glottocodes \n The following IDs are not found in glottolog: \n")}
    data[!existing,idcol, drop = TRUE]
  } else {
    if(messages == TRUE){message("All IDs are valid glottocodes")}
  }


}
