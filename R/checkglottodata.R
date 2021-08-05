#' Quality check of user-provided glottodata
#'
#' Go through a user-provided glottodataset and check:
#' - No missing IDs
#' - No duplicate IDs
#' - All variables have at least two levels
#' - All glottocodes are valid
#'
#' @param glottodata User-provided glottodata
#' @param id Column index or column name with IDs (glottocodes, glottosubcodes, or glottosubsubcodes). Default is "glottocode".
#' @param show If TRUE (default) a data viewer will be opened to show the levels of each variable (including NAs)
#'
#' @return
#' @export
#'
#' @examples
#' dpath <- "C:/Users/sjnor/surfdrive/Shared/SAPPHIRE/Output/Presentations/Isolates_WS-Athens/data/dbase_isolates_V2.xlsx"
#' glottodata <- get_sheetdata(path = dpath, sheets = 1)
#' checkglottodata(glottodata = glottodata, id = 1)
#'
#' checkglottodata(glottodata = glottodata)
#' lapply(glottodatalist, checkglottodata)
checkglottodata <- function(glottodata, show = TRUE){
  id <- "glottocode"
  checkdata_glottocol(glottodata = glottodata)
  checkdata_idmissing(data = glottodata, id = id)
  checkdata_idunique(data = glottodata, id = id)
  checkdata_twolevels(data = glottodata)
  checkdata_glottocodes(data = glottodata, id = id)
  if(show == TRUE){
  checkdata_varlevels(data = glottodata)
  }
}

#' Quality check of user-provided glottosubdata
#'
#' @param glottosubdata
#'
#' @return
#' @export
#'
#' @examples
#' checkglottosubdata(glottosubdata = glottosubdata)
#' lapply(glottosubdatalist, checkglottosubdata)
checkglottosubdata <- function(glottosubdata){
  id <- "glottosubcode"

}

#' Check whether rows have missing IDs.
#'
#' @param data User glottodata
#' @param id Column index or name with IDs (glottocodes, glottosubcodes, or glottosubsubcodes)
#'
#' @return Besides diagnostic messages, this function invisibly returns TRUE if check is passed (no missing IDs) and FALSE otherwise
#' @keywords internal
#' @export
#'
#' @examples
#' suppressMessages(check_idmissing(data = data, id = 1))
checkdata_idmissing <- function(data, id){

  idmissing <- nrow(data[is.na(data[,id]),] )

  if(idmissing > 0){
      message(paste(idmissing, ' rows with missing ID'))
      invisible(FALSE)
  } else{
      message("No missing IDs")
      invisible(TRUE)
  }
}

#' Check whether all IDs are unique
#'
#' Some unicorns might be found...
#'
#' @param data User glottodata
#' @param id Column index or name with IDs (glottocodes, glottosubcodes, or glottosubsubcodes)
#'
#' @return Besides diagnostic messages, this function invisibly returns TRUE if check is passed (all IDs are unique) and FALSE otherwise
#' @keywords internal
#' @export
#'
#' @examples
#' checkdata_idunique(data = glottodata, id = "glottocode")
checkdata_idunique <- function(data, id){
  freqtab <- data.frame(table(data[,id]))
  colnames(freqtab)[1] <- "id"
  colnames(freqtab)[2] <- "n"

  if(any(freqtab$n > 1)){
      duplicate <- freqtab[freqtab$n > 1, ]
      message('IDs are not unique. The following ids have duplicates:')
      message(paste(duplicate$id, ": ", duplicate$n, "\n"))
      invisible(FALSE)
  } else {
      message("No duplicate IDs.")
      invisible(TRUE)
  }
}

#' Check whether each variable has at least two levels (excluding NA).
#'
#' This function checks whether each variable has at least two levels (excluding NA). Use function checkall_varlevels to get an overview of the levels in each variable.
#' @param data
#'
#' @return Besides diagnostic messages, this function invisibly returns TRUE if check is passed (all IDs are unique) and FALSE otherwise
#' @export
#' @keywords internal
#'
#' @examples
#' suppressMessages(checkall_twolevels(data = data))
checkdata_twolevels <- function(data){
  lslevels <- lapply(data, unique)
  lslevels <- lapply(lslevels, factor, exclude = "NA")

  # summary of data:
  lslevelsrmna <- lapply(lslevels, levels)
  lslevelscount <- lapply(lslevelsrmna, length)
  lslevels_lgl <- lapply(lslevelscount, function(x) {x < 2})
  lslevdf <- t(as.data.frame(lslevels_lgl))

  # How many variables have less than two levels? Which variables?
  totbelow2 <- sum(lslevdf)
  namesbelow2 <- paste(rownames(lslevdf)[lslevdf], collapse = ", ")

  if(totbelow2 != 0){
    message(paste0("There are ", totbelow2, " variables with less than two levels (excluding NA): ", namesbelow2))
    invisible(FALSE)
  } else {
    message("All variables have two or more levels (excluding NA)")
    invisible(TRUE)
  }
}

#' Show levels of all variables.
#'
#' This function shows the levels of all variables and is mainly intended to provide users with a quick overview of their data.
#' @param data
#'
#' @return Opens a data viewer showing the levels of each variable (including NA)
#' @export
#' @keywords internal
#'
#' @examples
#' suppressMessages(check_varlevels(data = data))
checkdata_varlevels <- function(data){
  lslevels <- lapply(data, unique)
  lslevels <- lapply(lslevels, factor)

  varlevels <- unlist(lapply(lslevels, paste, collapse=" , "), use.names = FALSE)
  varlevels <- data.frame(variable = colnames(data), levels = varlevels)

  invisible(lslevels)
  View(varlevels)
}

#' Check whether all IDs are valid glottocodes.
#'
#' This is a wrapper around glottocode_exists
#'
#' @param data
#' @param id
#'
#' @return Besides diagnostic messages, this function invisibly returns TRUE if check is passed (all IDs are unique) and FALSE otherwise
#' @export
#' @keywords internal
#'
#' @examples
checkdata_glottocodes <- function(glottodata, id, messages = TRUE){
  existing <- glottocode_exists(glottodata[[id]])
  if(sum(!existing) > 0){
    message("Not all IDs are valid glottocodes \n The following IDs are not found in glottolog: \n")
  } else {
    message("All IDs are valid glottocodes")
  }
  invisible(all(existing))
}

#' Check whether number of columns are identical across all glottodata objects in a list
#'
#' @param langlist
#'
#' @return
#' @keywords internal
#'
#' @examples
checkdata_lscolcount <- function(langlist){
  colcount <- lapply(X = langlist, FUN = function(x){length(colnames(x))})
  colcount <- unlist(colcount, recursive = F)

  if(is.null(names(langlist))){names(langlist) <- seq_len(length(langlist))}

  if(length(unique(colcount)) > 1){
    message(paste(names(langlist), ": ", colcount, "\n"))
    stop('Not all languages have same number of features \n', call. = FALSE)
  }

}

checkdata_glottocol <- function(glottodata){
  n <- sum(colnames(glottodata) == "glottocode")
  if(n == 1){
    invisible(TRUE)
  } else if(n == 0){
    message("No glottocode column found. Please rename your ID column to 'glottocode'.")
    invisible(FALSE)
    } else if(n > 1){message("There are more than one columns with the name glottocode. Please remove/rename redundant column(s).")
    invisible(FALSE)
    }
}
