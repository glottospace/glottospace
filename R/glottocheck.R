#' Quality check of glottodata or glottosubdata
#'
#' This function first checks whether a dataset is glottodata or glottosubdata, and depending on the result calls glottocheck_data or glottocheck_subdata.
#'
#' It subsequently checks whether:
#' \itemize{
#'   \item one column exists with the name "glottocode"
#'   \item there are rows without a glottocode  (missing IDs)
#'   \item there are rows with duplicated glottocodes (duplicate IDs)
#'   \item all variables have at least two levels
#'   \item all glottocodes are valid
#' }
#'
#' @param glottodata User-provided glottodata
#' @param diagnostic If TRUE (default) a data viewer will be opened to show the levels of each variable (including NAs), and a data coverage plot will be shown.
#' @param checkmeta Should metadata be checked as well?
#'
#' @family <glottocheck>
#'
#' @export
#' @return Diagnostic messages highlighting potential issues with glottodata or glottosubdata.
#' @examples
#' \donttest{
#' glottodata <- glottoget("demodata")
#' glottocheck(glottodata, diagnostic = FALSE)
#' }
glottocheck <- function(glottodata, diagnostic = TRUE, checkmeta = TRUE){
  if(glottocheck_isglottosubdata(glottodata) == FALSE){
    glottocheck_data(glottodata = glottodata, diagnostic = diagnostic)
  } else{
    glottocheck_subdata(glottosubdata = glottodata, diagnostic = diagnostic)
  }

  if(checkmeta == TRUE){
    if(glottocheck_hasmeta(glottodata)){
      glottocheck_metadata(glottodata)
    } else {
      message("glottodata does not contain metadata")
    }
  }

}

#' Guess id of glottodata
#'
#' Guess whether the id of glottodata is glottocode or glottosubcode
#'
#' @param glottodata glottodata to check
#' @param id optional, if id is specified, the function will return original id
#' @noRd
#' @return A character string with either 'glottocode' or 'glottosubcode'. If
#'   none of these are present in the data, an error will be thrown.
#'
glottocheck_id <- function(glottodata, id = NULL){
  glottodata <- glottosimplify(glottodata)
  if(is.null(id)){
    if("glottocode" %in% colnames(glottodata) & "glottosubcode" %nin% colnames(glottodata)){
      id <- "glottocode"
      message("glottocode used as id")
    } else if("glottocode" %nin% colnames(glottodata) & "glottosubcode" %in% colnames(glottodata)){
      id <- "glottosubcode"
      message("glottosubcode used as id")
    } else if(all(c("glottocode", "glottosubcode") %in% colnames(glottodata)) ){
      id <- "glottocode"
      message("Data contains glottocodes AND glottosubcodes, glottocode used as id. If this is not what you want, please specify id.")
    } else if(all(c("glottocode", "glottosubcode") %nin% colnames(glottodata)) ){
      stop("Please provide an id, or add a 'glottocode' or 'glottosubcode' column to your data")
    }
  }
  id
}

#' Quality check of user-provided glottodata
#'
#' Go through a user-provided glottodataset and check whether:
#' \itemize{
#'   \item one column exists with the name "glottocode"
#'   \item there are rows without a glottocode  (missing IDs)
#'   \item there are rows with duplicated glottocodes (duplicate IDs)
#'   \item all variables have at least two levels
#'   \item all glottocodes are valid
#' }
#'
#' @param glottodata User-provided glottodata
#' @param diagnostic If TRUE (default) a data viewer will be opened to show the levels of each variable (including NAs), and a data coverage plot will be shown.
#' @noRd
#'
#' @family <glottocheck>
#'
#'
#' @examples
#' \donttest{
#' glottodata <- glottoget("demodata")
#' glottocheck_data(glottodata, diagnostic = FALSE)
#' }
glottocheck_data <- function(glottodata, diagnostic = TRUE){
  glottodata <- glottosimplify(glottodata)
  id <- "glottocode"
  glottocheck_glottocol(glottodata = glottodata)
  glottocheck_idmissing(data = glottodata, id = id)
  glottocheck_idunique(data = glottodata, id = id)
  glottocheck_twolevels(data = glottodata)
  glottocheck_glottocodes(glottodata = glottodata)
    glottocheck_colmissing(data = glottodata, id = id, diagnostic = diagnostic)
    glottocheck_rowmissing(data = glottodata, id = id, diagnostic = diagnostic)
  if(diagnostic == TRUE){
    glottocheck_varlevels(data = glottodata)
    glottoplot_naviewer(data = glottodata, id = id)
  }
}

#' Quality check of user-provided glottosubdata
#'
#' @param glottosubdata User-provided glottosubdata
#' @param diagnostic If TRUE (default) a data viewer will be opened to show the levels of each variable (including NAs), and a data coverage plot will be shown.
#' @noRd
#'
#' @family <glottocheck>
#' @examples
#' glottosubdata <- glottoget("demosubdata")
#' glottocheck_subdata(glottosubdata, diagnostic = FALSE)
glottocheck_subdata <- function(glottosubdata, diagnostic = TRUE){
  glottosubdata <- glottosimplify(glottosubdata)
  id <- "glottosubcode"
  glottocheck_glottosubcol(glottosubdata = glottosubdata)
  glottocheck_idmissing(data = glottosubdata, id = id)
  glottocheck_idunique(data = glottosubdata, id = id)
  glottocheck_twolevels(data = glottosubdata)
  glottocheck_glottosubcodes(glottosubdata = glottosubdata)
  glottocheck_colmissing(data = glottosubdata, id = id, diagnostic = diagnostic)
  glottocheck_rowmissing(data = glottosubdata, id = id, diagnostic = diagnostic)
  if(diagnostic == TRUE){
    glottocheck_varlevels(data = glottosubdata)
    glottoplot_naviewer(data = glottosubdata, id = id)
  }
}

#' Check metadata of glottodata
#'
#' @param glottodata glottodata
#'
#'
#' @noRd
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottocheck_metadata(glottodata)
glottocheck_metadata <- function(glottodata){
  tablenames <- paste(names(glottodata), collapse = ", ")
  message( paste("This glottodataset contains the folowing tables:", tablenames) )

  if(glottocheck_hasstructure(glottodata)){
    glottocheck_metatypes(glottodata)
    glottocheck_metaweights(glottodata)
    glottocheck_metanames(glottodata)
  } else {message("No structure table found in glottodata")}
}

#' Check whether glottodata contains structure table / metadata
#'
#' In fact, this function only checks whether glottodata contains a structure table, because the structure table is the only table that is required by some glottospace functions. All other tables are for humans, not computers;-).
#'
#' @param glottodata glottodata
#'
#'
#' @aliases glottocheck_hasstructure
#' @noRd
#' @family <glottocheck>
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottocheck_hasmeta(glottodata)
glottocheck_hasmeta <- glottocheck_hasstructure <- function(glottodata){
  any(is_list(glottodata)) & any(names(glottodata) %in% "structure")
}

#' Check whether glottodata contains a sample table
#'
#'
#' @param glottodata glottodata
#'
#' @noRd
#' @family <glottocheck>
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottocheck_hassample(glottodata)
glottocheck_hassample <- function(glottodata){
  any(is_list(glottodata)) & any(names(glottodata) %in% "sample")
}

#' Check whether object is likely to be a structure table
#'
#'
#' @param structure structure table
#'
#' @noRd
#' @family <glottocheck>
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' structure <- glottodata[["structure"]]
#' glottocheck_isstructure(structure)
glottocheck_isstructure <- function(structure){
  if(!is.null(colnames(structure))){
    return(all(c("varname", "type", "weight") %in% colnames(structure) ))
  } else {
    return(FALSE)
  }
}


glottocheck_metatypes <- function(glottodata){
  if(!all(glottodata$structure$type %in% glottocreate_lookuptable()[,"type_lookup"]) ){
    message(paste0("The following types were not recognized: ",
            unique(glottodata$structure$type[glottodata$structure$type %nin% glottocreate_lookuptable()[,"type_lookup"]]),
            "\n maybe there was a spelling error? Check the lookup table to see the possible levels."))

  } else{message("All types recognized")}
}

glottocheck_metanames <- function(glottodata){

  structure <- glottodata[["structure"]]


  data <- glottosimplify(glottodata)

  if(glottocheck_isstructure(structure)){
    strucvars <- structure$varname
    strucnindat <- strucvars[strucvars %nin% colnames(data)]
    datninstruc <- colnames(data)[colnames(data) %nin% strucvars]
    if(!purrr::is_empty(strucnindat)){
      message(paste0("The following variables are in the structure table, but there are no such columns in the data: ", paste0(strucnindat, collapse = ", "), "\n Please check whether the spelling is identical, remove the rows from the structure table, or add the columns to the data. \n\n"))
    }
    if(!purrr::is_empty(datninstruc)){
      message(paste0("The following variables are in the data, but there are no such columns variables defined in the structure table: ", paste0(datninstruc, collapse = ", "), "\n Please check whether the spelling is identical, remove the rows from the structure table, or add the columns to the data. \n\n"))
    }
  } else{
    message("Structure table does not have the correct format. Please check whether the first column is called 'varname' ")
  }
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
#' @noRd
#'
glottocheck_idmissing <- function(data, id){

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
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = FALSE)
#' glottocheck_idunique(data = glottodata, id = "glottocode")
glottocheck_idunique <- function(data, id){
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
#' This function checks whether each variable has at least two levels (excluding NA). Use function glottocheck_varlevels to get an overview of the levels in each variable.
#' @param data
#'
#' @return Besides diagnostic messages, this function invisibly returns TRUE if check is passed (all variables have at least two levels) and FALSE otherwise
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = FALSE)
#' glottocheck_twolevels(data = glottodata)
glottocheck_twolevels <- function(data){
  data <- as.data.frame(data)
  lslevels <- lapply(data, unique)
  lslevels <- lapply(lslevels, factor, exclude = NA)

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
#'
#' @param data Glottodata
#' @param diagnostic Whether diagnostic messages should be shown
#'
#' @return Opens a data viewer showing the levels of each variable (including NA)
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = FALSE)
#' glottocheck_varlevels(data = glottodata, diagnostic = FALSE)
glottocheck_varlevels <- function(data, diagnostic = TRUE){
  # readline(prompt="Do you want to view the levels of each variable (this might take a few seconds)? \n Press [enter] to view or [esc] to skip")
  lslevels <- lapply(data, unique)
  lslevels <- lapply(lslevels, factor)

  varlevels <- unlist(lapply(lslevels, paste, collapse=" , "), use.names = FALSE)
  varlevels <- data.frame(variable = colnames(data), levels = varlevels)

  invisible(lslevels)
  if(diagnostic == TRUE){utils::View(varlevels)}
}

#' Check whether all IDs are valid glottocodes.
#'
#' This is a wrapper around glottocode_exists
#'
#' @param glottodata User-provided glottodata
#'
#' @return Besides diagnostic messages, this function invisibly returns TRUE if check is passed and FALSE otherwise
#' @noRd
#' @examples
#' glottodata <- glottoget("demodata")
#' glottocheck_glottocodes(glottodata)
glottocheck_glottocodes <- function(glottodata){
  glottodata <- glottosimplify(glottodata)
  glottodata <- glottodata[!is.na(glottodata[["glottocode"]]), ]
  message("Checking ", nrow(glottodata), " glottocodes...")
  existing <- glottocode_exists(glottodata[["glottocode"]])
  if(sum(!existing) > 0){
    message("Not all IDs are valid glottocodes \n The following glottocodes are not found in glottolog (checked at the language level): \n")
    nexist <- paste(glottodata[!existing,"glottocode", drop = TRUE], collapse = ", ")
    message(nexist)
  } else {
    message("All IDs are valid glottocodes")
  }
  invisible(all(existing))
}

#' Check whether all IDs are valid glottosubcodes.
#'
#' This is a wrapper around glottosubcode_valid
#'
#' @param glottosubdata glottosubdata
#'
#' @return Besides diagnostic messages, this function invisibly returns TRUE if check is passed and FALSE otherwise
#' @noRd
glottocheck_glottosubcodes <- function(glottosubdata){
  glottosubdata <- glottosimplify(glottosubdata)
  v <- glottosubcode_valid(glottosubdata$glottosubcode)
  if(v == TRUE){message("All glottosubcodes are valid.")}
  invisible(v)
}

#' Check whether number of columns are identical across all glottodata objects in a list
#'
#' @param langlist
#'
#' @return Returns error message if number of columns is not identical, and invisibly returns TRUE otherwise.
#' @noRd
#' @examples
#' glottosubdata <- glottocreate(glottocodes = c("yucu1253", "tani1257"),
#'                    variables = 3, groups = c("a", "b"), n = 5)
#' langlist <- glottosubdata[c(1,2)]
#' glottocheck_lscolcount(langlist) # invisibly returns TRUE
glottocheck_lscolcount <- function(langlist){
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

#' Check whether there is one column named 'glottocode'
#'
#' @param glottodata A glottodata table
#'
#' @noRd
#'
glottocheck_glottocol <- function(glottodata){
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

#' Check whether there is one column named 'glottosubcode'
#'
#' @param glottosubdata A glottodata list
#' @noRd
glottocheck_glottosubcol <- function(glottosubdata){
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

#' Check whether rows have missing data
#'
#' Provides a message in case there are missing data, otherwise returns NULL
#'
#' @param data A dataset
#' @param id Column name or index with unique id's
#' @param diagnostic Whether diagnostic messages should be shown
#' @param na.rm Whether rows without id should be removed.
#' @noRd
glottocheck_rowmissing <- function(data, id, diagnostic = FALSE, na.rm = TRUE){
  if(na.rm == TRUE){data <- data[!is.na(data[[id]]), ]}
  datamissing <- tibble::column_to_rownames(data, var = id)
  datamissing$count <- rowSums(is.na(datamissing) )
  if(any(datamissing$count != 0)){
    message("Some rows have missing data.")
  if(diagnostic == TRUE){printmessage(datamissing[datamissing$count != 0, "count", drop = FALSE])}
  }
}

#' Check whether columns have missing data
#'
#' Provides a message in case there are missing data, otherwise returns NULL
#'
#' @param data A dataset
#' @param id Column name or index with unique id's
#' @param diagnostic Whether diagnostic messages should be shown
#' @param na.rm Whether rows without id should be removed.
#' @noRd
glottocheck_colmissing <- function(data, id, diagnostic = FALSE, na.rm = TRUE){
  if(na.rm == TRUE){data <- data[!is.na(data[[id]]), ]}
  datamissing <- tibble::column_to_rownames(data, var = id)
  datamissing <- rbind(datamissing, "count" = colSums(is.na(datamissing) ) )
  if(any(datamissing["count", ] != 0)){
    message("Some columns have missing data.")
  if(diagnostic == TRUE){printmessage(datamissing["count", datamissing["count", ] != 0, drop = FALSE])}
  }
}

#' Guess whether an object is glottosubdata (and not glottodata)
#'
#' @param glottodata User-provided glottodata
#'
#' @noRd
#' @examples
#' glottosubdata <- glottoget("demosubdata")
#' glottocheck_isglottosubdata(glottosubdata)
#'
#' glottosubdata <- glottosimplify(glottosubdata)
#' glottocheck_isglottosubdata(glottosubdata)
glottocheck_isglottosubdata <- function(glottosubdata){
 glottocheck_isglottosubdata_complex(glottosubdata) | glottocheck_isglottosubdata_simplified(glottosubdata) | inherits(x = glottosubdata, what = "glottosubdata")
  }

#' Guess whether an object is (simplified) glottosubdata
#'
#' @param glottodata User-provided glottodata
#'
#' @noRd
#' @examples
#' glottosubdata <- glottoget("demosubdata")
#'
#' glottosubdata <- glottosimplify(glottosubdata)
#' glottocheck_isglottosubdata_simplified(glottosubdata)
glottocheck_isglottosubdata_simplified <- function(glottosubdata){
  if(inherits(glottosubdata, what = "list")  ){
    if(any(names(glottosubdata) %in% "glottosubdata") ){
      return(!purrr::is_empty(colnames(glottosubdata[[1]])[1] == "glottosubcode") )
    } else { # 'glottosubdata' does not exist in names
      return(FALSE)
    }
  } else{ # doesn't inherit list
    if(!purrr::is_empty(colnames(glottosubdata)[1])){
      return(colnames(glottosubdata)[1] == "glottosubcode")
    } else {
      return(FALSE)
    }
  }
}

#' Guess whether an object is (complex) glottosubdata
#'
#' @param glottosubdata glottosubdata
#'
#' @noRd
#' @examples
#' glottosubdata <- glottoget("demosubdata", meta = TRUE)
#'
#' glottocheck_isglottosubdata_complex(glottosubdata)
glottocheck_isglottosubdata_complex <- function(glottosubdata){
  if(inherits(glottosubdata, what = "list")  ){
    if(all(names(glottosubdata) %nin% c("glottosubdata", "glottodata") ) ){
      return(!purrr::is_empty(colnames(glottosubdata[[1]])[1] == "glottosubcode") )
    } else { # 'glottosubdata' or 'glottodata' does exist in names
      return(FALSE)
    }
  } else{ # doesn't inherit list
      return(FALSE)
  }
}

#' Guess whether an object is glottodata (and not glottosubdata)
#'
#' @param glottodata glottodata
#'
#' @noRd
#' @examples
#' glottodata <- glottoget("demodata", meta = FALSE)
#' glottocheck_isglottodata(glottodata)
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottocheck_isglottodata(glottodata)
glottocheck_isglottodata <- function(glottodata){

  glottodata <- contrans_tb2df(glottodata)
  if(inherits(glottodata, what = "list")){
    if("glottodata" %in% names(glottodata)){
      if(!purrr::is_empty(colnames(glottodata[["glottodata"]])[1])){
        return(colnames(glottodata[["glottodata"]])[1] == "glottocode")
      } else {
        return(FALSE)
      }
    } else {
      return(FALSE)
    }
  } else {
    if(!purrr::is_empty(colnames(glottodata)[1])){
      return(colnames(glottodata)[1] == "glottocode")
    } else {
      return(FALSE)
    }
  }

}




