#' Quality check of user-provided glottodata or glottosubdata
#'
#' This function first checks whether a dataset is glottodata or glottosubdata, and depending on the result calls glottocheck_data or glottocheck_subdata.
#'
#' It subsequently checks whether:
#' - one column exists with the name "glottocode"
#' - there are rows without a glottocode  (missing IDs)
#' - there are rows with duplicated glottocodes (duplicate IDs)
#' - all variables have at least two levels
#' - all glottocodes are valid
#'
#' @param glottodata User-provided glottodata
#' @param diagnostic If TRUE (default) a data viewer will be opened to show the levels of each variable (including NAs), and a data coverage plot will be shown.
#'
#' @family <glottocheck>
#' @return
#' @export
#'
#' @examples
#' glottocheck(glottodata)
glottocheck <- function(glottodata, diagnostic = TRUE){
  if(glottocheck_isglottosubdata(glottodata) == FALSE){
    glottocheck_data(glottodata = glottodata, diagnostic = diagnostic)
  } else{
    glottocheck_subdata(glottosubdata = glottodata, diagnostic = diagnostic)
  }
}

#' Quality check of user-provided glottodata
#'
#' Go through a user-provided glottodataset and check whether:
#' - one column exists with the name "glottocode"
#' - there are rows without a glottocode  (missing IDs)
#' - there are rows with duplicated glottocodes (duplicate IDs)
#' - all variables have at least two levels
#' - all glottocodes are valid
#'
#' @param glottodata User-provided glottodata
#' @param diagnostic If TRUE (default) a data viewer will be opened to show the levels of each variable (including NAs), and a data coverage plot will be shown.
#'
#' @family <glottocheck>
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottoget_glottodata(meta = FALSE)
#' glottocheck_data(glottodata = glottodata)
glottocheck_data <- function(glottodata, diagnostic = TRUE){
  id <- "glottocode"
  checkdata_glottocol(glottodata = glottodata)
  checkdata_idmissing(data = glottodata, id = id)
  checkdata_idunique(data = glottodata, id = id)
  checkdata_twolevels(data = glottodata)
  checkdata_glottocodes(glottodata = glottodata)
    checkdata_colmissing(data = glottodata, id = id, diagnostic = diagnostic)
    checkdata_rowmissing(data = glottodata, id = id, diagnostic = diagnostic)
  if(diagnostic == TRUE){
    checkdata_varlevels(data = glottodata)
    naviewer(data = glottodata, id = id)
  }
}

#' Quality check of user-provided glottosubdata
#'
#' @param glottosubdata User-provided glottosubdata
#' @param diagnostic If TRUE (default) a data viewer will be opened to show the levels of each variable (including NAs), and a data coverage plot will be shown.
#'
#' @return
#' @export
#' @family <glottocheck>
#' @examples
#' glottosubdata <- glottoget_glottodata(meta = FALSE, demo = "glottosubdata")
#' glottosubdata <- glottojoin_data(glottosubdata)
#' glottocheck_subdata(glottosubdata)
#'
#' Better to glottojoin_data first, instead of the following approach (because checks only within each language for duplicates etc.)
#' lapply(glottosubdata, glottocheck_subdata)
glottocheck_subdata <- function(glottosubdata, diagnostic = TRUE){
  id <- "glottosubcode"
  checkdata_glottosubcol(glottosubdata = glottosubdata)
  checkdata_idmissing(data = glottosubdata, id = id)
  checkdata_idunique(data = glottosubdata, id = id)
  checkdata_twolevels(data = glottosubdata)
  checkdata_glottosubcodes(glottosubdata = glottosubdata)
  checkdata_colmissing(data = glottosubdata, id = id, diagnostic = diagnostic)
  checkdata_rowmissing(data = glottosubdata, id = id, diagnostic = diagnostic)
  if(diagnostic == TRUE){
    checkdata_varlevels(data = glottosubdata)
    naviewer(data = glottosubdata, id = id)
  }
}

#' Check metadata of glottodata
#'
#' @param glottodata
#'
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottoget_glottodata()
glottocheck_metadata <- function(glottodata){
  tablenames <- paste(names(glottodata), collapse = ", ")
  message( paste("This glottodataset contains the folowing tables:", tablenames) )

  if(glottocheck_hasstructure(glottodata)){
    glottocheck_metatypes(glottodata)
    glottocheck_metaweights(glottodata)
  } else {message("No structure table found in glottodata")}
}


#' Check whether glottodata contains metadata
#'
#' In fact, this function only checks whether glottodata contains a structure table, because the structure table is the only table that is required by some glottospace functions. All other tables are for humans, not computers;-).
#'
#' @param glottodata
#'
#' @return
#' @export
#' @aliases glottocheck_hasstructure
#' @family <glottocheck>
#' @examples
#' glottocheck_hasmeta(glottodata)
glottocheck_hasmeta <- glottocheck_hasstructure <- function(glottodata){
  is_list(glottodata) & any(names(glottodata) %in% "structure")
}

glottocheck_metatypes <- function(glottodata){
  if(!all(glottodata$structure$type %in% glottocreate_lookuptable()[,"type_lookup"]) ){
    message("Some types were not recognized, maybe there was a spelling error? Type glottocreate_lookuptable() to see the possible levels.")
  } else{message("All types recognized")}
}

glottocheck_metaweights <- function(glottodata){
  if(any(is.na(glottodata$structure$weight))){message("Some of the weights are NA. If you want to weigh all variables equally, please set each of them to 1.")
  } else{message("All weights are specified")}
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
  # readline(prompt="Do you want to view the levels of each variable (this might take a few seconds)? \n Press [enter] to view or [esc] to skip")
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
#' @param glottodata User-provided glottodata
#'
#' @return Besides diagnostic messages, this function invisibly returns TRUE if check is passed (all IDs are unique) and FALSE otherwise
#' @export
#' @keywords internal
#'
checkdata_glottocodes <- function(glottodata){
  existing <- glottocode_exists(glottodata[["glottocode"]])
  if(sum(!existing) > 0){
    message("Not all IDs are valid glottocodes \n The following glottocodes are not found in glottolog: \n")
  } else {
    message("All IDs are valid glottocodes")
  }
  invisible(all(existing))
}

checkdata_glottosubcodes <- function(glottosubdata){
  glottosubcode_valid()
}

#' Check whether number of columns are identical across all glottodata objects in a list
#'
#' @param langlist
#'
#' @return Returns error message if number of columns is not identical, and invisibly returns TRUE otherwise.
#' @keywords internal
#'
#' @examples
#' glottosubdata <- glottocreate_subdata(glottocodes = c("yucu1253", "tani1257"), variables = 3, groups = c("a", "b"), n = 5)
#' langlist <- glottosubdata[c(1,2)]
#' checkdata_lscolcount(langlist) # invisibly returns TRUE
checkdata_lscolcount <- function(langlist){
  colcount <- lapply(X = langlist, FUN = function(x){length(colnames(x))})
  colcount <- unlist(colcount, recursive = F)

  if(is.null(names(langlist))){names(langlist) <- seq_len(length(langlist))}

  if(length(unique(colcount)) > 1){
    message(paste(names(langlist), ": ", colcount, "\n"))
    stop('Not all languages have same number of features \n', call. = FALSE)
  } else {
    invisible(TRUE)
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

checkdata_glottosubcol <- function(glottosubdata){
  n <- sum(colnames(glottosubdata) == "glottosubcode")
  if(n == 1){
    invisible(TRUE)
  } else if(n == 0){
    message("No glottosubcode column found. Please rename your ID column to 'glottosubcode'.")
    invisible(FALSE)
  } else if(n > 1){message("There are more than one columns with the name glottosubcode. Please remove/rename redundant column(s).")
    invisible(FALSE)
  }
}

checkdata_rowmissing <- function(data, id, diagnostic = FALSE){
  datamissing <- tibble::column_to_rownames(data, var = id)
  datamissing$count <- rowSums(is.na(datamissing) )
  if(any(datamissing$count != 0)){
    message("Some rows have missing data.")
  if(diagnostic == TRUE){print(datamissing[datamissing$count != 0, "count", drop = FALSE])}
  }
}

checkdata_colmissing <- function(data, id, diagnostic = FALSE){
  datamissing <- tibble::column_to_rownames(data, var = id)
  datamissing <- rbind(datamissing, "count" = colSums(is.na(datamissing) ) )
  if(any(datamissing["count", ] != 0)){
    message("Some columns have missing data.")
  if(diagnostic == TRUE){print(datamissing["count", datamissing["count", ] != 0, drop = FALSE])}
  }
}

#' Show data coverage (view NAs)
#'
#' This function plots the NAs in a dataset. If you used another coding to
#' specify missing data, you should run \code{cleandata_recodemissing} or \code{cleanglottodata} first. If you'd
#' like some more advanced ways of handling NAs, you might check out the
#' \code{naniar} package.
#'
#' @param data Any dataset
#' @param id column name with IDs
#'
#' @return
#' @export
#'
#' @examples
#' data <- glottoget_glottodata(meta = FALSE)
#' naviewer(data, id = "glottocode")
naviewer <- function(data, id = NULL){
  if(!is.null(id)){
  datamissing <- data[,colnames(data) != id ]
  } else {
    datamissing <- data
  }
  datamissing[is.na(datamissing)] <- "nodata"

  datamissing[datamissing != "nodata" ] <- "data"
  datamissing[datamissing == "nodata" ] <- "NA"


  datamissing <- as.matrix(sapply(datamissing, as.character))
  rownames(datamissing) <- data[,id]

  datamissing <- datamissing %>%
    as.data.frame() %>%
    tibble::rownames_to_column(id) %>%
    tidyr::pivot_longer(-c(glottocode), names_to = "variable", values_to = "coverage")

  ggplot2::ggplot(data = datamissing, ggplot2::aes(x=variable, y=glottocode, fill=coverage) ) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(labels = c("data", "NA"), values = c("navy", "darkred"))
}

#' Guess whether a list of glottodata tables is glottosubdata
#'
#' @param glottodata User-provided glottodata
#'
#' @return
#' @export
#'
#' @examples
#' glottocheck_isglottosubdata(glottodata)
glottocheck_isglottosubdata <- function(glottodata){
    class(glottodata) == "list" &
    !any(names(glottodata) %in% "glottodata") &
    !purrr::is_empty(colnames(glottodata[[1]])[1] == "glottosubcode")
}

