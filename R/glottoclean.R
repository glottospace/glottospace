
#' Clean glottodata
#'
#' This function is a wrapper around glottorecode. This function has some built in default values that are being recoded.
#' For example, "No" is recoded to FALSE and "?" is recoded to NA.
#' Use glottorecode directly if you don't want to use these defaults.
#'
#' @param glottodata glottodata list
#' @param tona Optional additional values to recode to NA (besides default)
#' @param tofalse Optional additional values to recode to FALSE (besides default)
#' @param totrue Optional additional values to recode to TRUE (besides default)
#'
#'
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodata <- glottoclean(glottodata)
glottoclean <- function(glottodata, tona = NULL, tofalse = NULL, totrue = NULL){

  all2false <- c("n", "N", "No", "no", "NO", 0)
  all2true <- c("y", "Y", "Yes", "yes", "YES", 1)
  all2na <- c("NA", "N A", "N/A", "#N/A", "NA ", " NA", "N /A", "N / A", " N / A", "N / A ", "na", "n a", "n/a",
                  "na ", " na", "n /a", "n / a", " a / a", "n / a ", "NULL", "null", "", "\\?", "\\*", "\\.")

  if(!is.null(tona)){all2na <- c(all2na, tona)}
  if(!is.null(tofalse)){all2false <- c(all2false, tofalse)}
  if(!is.null(totrue)){all2true <- c(all2true, totrue)}

  glottodata <- glottorecode(glottodata = glottodata,
                tofalse = all2false,
                totrue = all2true,
                tona = all2na)

  return(glottodata)
}

#' Recode values across a glottodataset
#'
#' This function recodes values within a glottodataset to NA or TRUE/FALSE. Recoding is done based on column types in the structure table. Run glottocreate_structuretable() to create one.
#'
#' @param glottodata glottodata list
#' @param tona Values to recode to NA
#' @param tofalse Values to recode to FALSE
#' @param totrue Values to recode to TRUE
#' @keywords internal
#' @family <glottorecode><glottoclean>
#'
#' @export
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodata <- glottorecode(glottodata, tona = c("?", "missing"))
glottorecode <- function(glottodata, tofalse = NULL, totrue = NULL, tona = NULL){

  if(!is.null(tona)){
    glottodata <- glottorecode_missing(glottodata, tona = tona)
  }

  if(!is.null(tofalse) | !is.null(totrue)){
    glottodata <- glottorecode_logical(glottodata = glottodata, tofalse = tofalse, totrue = totrue)
  }
 glottodata
}

#' Recode missing values to NA
#'
#' @param glottodata User-provided glottodata
#' @param tona Optional, additional values to recode to NA
#' @family <glottorecode>
#'
#' @export
#'
#' @keywords internal
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottorecode_missing(glottodata, tona = "?")
glottorecode_missing <- function(glottodata, tona){

  stopifnot(glottocheck_isglottodata(glottodata))

  splitted <- glottosplit(glottodata)
  glottodata <- splitted[[1]]

  glottodata <- data.frame(lapply(glottodata, recode_tona, tona = tona))

  message("Missing values recoded to NA \n")

  if(any(!is.na(splitted[[2]]))){glottodata <- glottojoin(glottodata = glottodata, with = splitted[[2]])}

  glottodata
}

#' Recode character columns to TRUE/FALSE
#'
#' @param glottodata glottodata list
#' @keywords internal
#'
#' @export
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottorecode_logical(glottodata, totrue = c("y", "Y", 1), tofalse = c("n", "N", 0))
glottorecode_logical <- function(glottodata, totrue = NULL, tofalse = NULL){
  # maybe better to do with tribble lookup table https://r-pkgs.org/package-within.html
  # tribble lookup could be used for a function recodefact (factors)
  # other approach could be used for a funciton recodenum (numeric)

  if(!glottocheck_hasstructure(glottodata) ){
    stop("Please provide a structure table with at least a type column. Run glottocreate_structuretable() to create it.")
  }

  structure <- glottodata[["structure"]]
  types <- structure$type
  cbinary <- structure$varname[which(types == "asymm" | types == "symm")]

  data <- glottodata[["glottodata"]]

  if(!is.null(cbinary)){
    bindat <- data[, cbinary]
    if(!is.null(totrue)){bindat <- recode_df(data = bindat, old = totrue, new = TRUE) }
    if(!is.null(tofalse)){bindat <- recode_df(data = bindat, old = tofalse, new = FALSE) }
    bindat <- apply(bindat, 2, as.logical)
    data[, cbinary] <- bindat
    message("Values in binary columns (symm/asymm) recoded to TRUE/FALSE \n")
  }
  glottodata[["glottodata"]] <- data
  glottodata
}

#' Fix colnames of excel files in which colnames refer to another cell
#'
#' @param glottodata glottodata or glottosubdata
#' @export
#'
#'
glottoclean_colnamerepair <- function(glottodata){
  if(glottocheck_isglottodata(glottodata)){
    splitted <- glottosplitmergemeta(glottodata)
    glottodata <- glottoclean_colnamerepair_table(splitted[[1]])
    glottodata <- glottosplitmergemeta(glottodata = glottodata, splitted = splitted)
  } else if(glottocheck_isglottodata(glottodata)){
    splitted <- glottosplitmergemeta(glottodata)
    glottodata <- lapply(splitted[[1]], glottoclean_colnamerepair_table)
    glottodata <- glottosplitmergemeta(glottodata = glottodata, splitted = splitted)
  }
  glottodata
}

glottoclean_colnamerepair_table <- function(table){
  colnames(table) <- gsub("\\...[0-9]*$","",colnames(table))
  table
}
