
#' Clean glottodata
#'
#' This function is a wrapper around glottorecode. This function has some built in default values that are being recoded.
#' For example, if column type is 'symm' or 'asymm', values such as "No" and 0 are recoded to FALSE and "?" is recoded to NA.
#' Use glottorecode directly if you don't want to use these defaults.
#'
#' @param glottodata glottodata (either a list or a data.frame)
#' @param tona Optional additional values to recode to NA (besides default)
#' @param tofalse Optional additional values to recode to FALSE (besides default)
#' @param totrue Optional additional values to recode to TRUE (besides default)
#' @param structure Optional structure table (should be provided if glottodata doesn't contain any)
#' @param id By default, glottoclean looks for a column named 'glottocode', if the id is in a different column, this should be specified.
#'
#' @return A cleaned-up version of the original glottodata object (either a list or a data.frame, depending on the input)
#' @export
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodata <- glottoclean(glottodata)
#'
#' glottosubdata <- glottoget("demosubdata", meta = TRUE)
#' glottodata <- glottoclean(glottosubdata)
glottoclean <- function(glottodata, structure = NULL, tona = NULL, tofalse = NULL, totrue = NULL, id = NULL){

  # Convert to glottodata
  if(glottocheck_isglottosubdata(glottodata)){
    glottodata <- glottojoin(glottodata)
  } else if(!glottocheck_isglottodata(glottodata)){
    message("glottodata object does not adhere to glottodata/glottosubdata format. Use glottocreate() or glottoconvert().")
  }

  # Simplified glottodata table and separate structure table
  if(glottocheck_hasmeta(glottodata) & is.null(structure)){
    splitted <- glottosplitmergemeta(glottodata)
    glottodata <- splitted[[1]]
    structure <- splitted[[2]][["structure"]]
  } else if(glottocheck_hasmeta(glottodata) & !is.null(structure)){
    glottodata <- glottosimplify(glottodata)
    splitted <- NULL
  } else if(!glottocheck_hasmeta(glottodata) & is.null(structure)){
    stop("structure table not found, please add one to glottodata or provide it separately.")
  }

  all2false <- glottoclean_all2false()
  all2true <- glottoclean_all2true()
  all2na <- glottoclean_all2na()

  if(!is.null(tona)){all2na <- c(all2na, tona)}
  if(!is.null(tofalse)){all2false <- c(all2false, tofalse)}
  if(!is.null(totrue)){all2true <- c(all2true, totrue)}

  glottodata <- glottorecode(glottodata = glottodata,
                structure = structure,
                tofalse = all2false,
                totrue = all2true,
                tona = all2na)

  glottodata <- glottoclean_type(glottodata = glottodata, structure = structure, id = id)

  if(!is.null(splitted)){
    glottodata <- glottosplitmergemeta(glottodata = glottodata, splitted = splitted)
  }
  invisible(glottodata)
}

glottoclean_all2false <- function(){
  c("n", "N", "No", "no", "NO", 0, 0.0, "F", "FALSE", "False", "false", "0", "0.0")
}

glottoclean_all2true <- function(){
  c("y", "Y", "Yes", "yes", "YES", 1, 1.0, "T", "TRUE", "True", "true", "1", "1.0")
}

glottoclean_all2na <- function(){
  c("NA", "N A", "N/A", "#N/A", "NA ", " NA", "N /A", "N / A", " N / A", "N / A ", "na", "n a", "n/a",
    "na ", " na", "n /a", "n / a", " a / a", "n / a ", "NULL", "null", "", "\\?", "\\*", "\\.")
}

#' Set types in glottodata
#'
#' @param glottodata glottodata without meta tables
#' @param structure structure table
#' @param id By default, glottoclean_type looks for a column named 'glottocode', if the id is in a different column, this should be specified.
#'
#' @return a simplified glottodata table (tibble) with column types specified
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottoclean_type(glottodata, structure = glottodata[["structure"]])
glottoclean_type <- function(glottodata, structure, id = NULL){

  glottodata <- glottosimplify(glottodata)
  if(is.null(structure) ){
    stop("Please provide a structure table.")
  }

  # Set id:
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

  duplo <- sum(duplicated(glottodata) | duplicated(glottodata, fromLast = TRUE))
  if(duplo != 0){
    message(paste0("This glottodata contains ", duplo, " rows which have at least one duplicate, and ", nrow(glottodata) - duplo, " unique rows \n"))
    message(paste0("When plotting, you will see ",  nrow(glottodata) - sum(duplicated(glottodata)), " points (unique rows + one of each duplicate) \n"))
  }

  # structure table:
  if(!("varname" %in% colnames(structure) ) ){
    colnames(structure)[1] <- "varname"
    message("The structure table does not contain a 'varname' column, using the first column instead.")
  }

  rownames(glottodata) <- NULL
  glottodata <- tibble::column_to_rownames(glottodata, id)

  if(length(colnames(glottodata)) != length(structure$varname) ){
    message(paste("The number of variables in ", ifelse(id == "glottocode", "glottodata", "glottosubdata"), "differs from the number of variables in the structure table") )
    nostruc <- colnames(glottodata)[colnames(glottodata) %nin% structure$varname]
    novar <- structure$varname[structure$varname %nin% colnames(glottodata)]
    if(!purrr::is_empty(nostruc)){
      message(paste0("The following variables exist in the data, but are not defined in the structure table (and will be ignored): ", nostruc))
    }
    if(!purrr::is_empty(novar)){
      message(paste0("The following variables are defined in the structure table but do not exist in the data (and will be ignored): ", novar))
    }
  }
  structure <- suppressMessages(dplyr::left_join(data.frame("varname" = colnames(glottodata)), structure))

  # type
  if(!("type" %in% colnames(structure) ) ){
    stop('No type column found in structure. Please add a type column.')
  }

  dropvars <- which(structure$type %nin% glottocreate_lookuptable()$type_lookup )
  if(!purrr::is_empty(dropvars)){
    dropvarnames <- paste0(colnames(glottodata)[dropvars], collapse = ",")
    message(paste0("The following variables are ignored in distance calculation (their type is not one of the pre-specified types): \n", dropvarnames))
    glottodata <- glottodata[,-dropvars]
    structure <- structure[-dropvars, ]
  }

  symm <- which(structure$type == "symm")
  asymm <- which(structure$type == "asymm")
  numer <- which(structure$type == "numeric")
  fact <- which(structure$type == "factor")
  ordfact <- which(structure$type == "ordered")
  ordratio <- which(structure$type == "ordratio")
  logratio <- which(structure$type == "logratio")

  # levels
  if(any(colnames(structure) == "levels")){
    levels <- structure$levels
  }

  cbinary <- c(symm, asymm)

  # set type
  glottodata[cbinary] <- lapply(glottodata[cbinary], as.logical)
  glottodata[numer] <- lapply(glottodata[numer], as.numeric)
  glottodata[fact] <- lapply(glottodata[fact], as.factor)
  if(!purrr::is_empty(ordfact)){
    glottodata[ordfact] <- mapply(FUN = as.ordfact, x = glottodata[ordfact], levels = levels[ordfact])
  }
  glottodata[ordratio] <- lapply(glottodata[ordratio], as.numeric)
  glottodata[logratio] <- lapply(glottodata[logratio], as.numeric)

  glottodata <- tibble::rownames_to_column(glottodata, id)
  glottodata <- tibble::as_tibble(glottodata)

  # weights
  if(all(is.na(structure$weight))){
    weights <- rep(1, nrow(structure))
    message('All weights are NA. Default is to weight all variables equally: all weights set to 1')
  } else{
    weights <- as.numeric(structure$weight)
    if(!purrr::is_empty(weights[is.na(weights)])){
      weights[is.na(weights)]  <- 1
      message('Some weights are NA. Missing weights set to 1')
    }
  }

  glottodata

}

#' Recode values across a glottodataset
#'
#' This function recodes values within a glottodataset to NA or TRUE/FALSE. Recoding is done based on column types in the structure table. Run glottocreate_structuretable() to create one.
#'
#' @param glottodata glottodata list
#' @param tona Values to recode to NA
#' @param tofalse Values to recode to FALSE
#' @param totrue Values to recode to TRUE
#' @noRd
#' @family <glottorecode><glottoclean>
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodata <- glottorecode(glottodata, tona = c("?", "missing"))
glottorecode <- function(glottodata, structure = NULL, tofalse = NULL, totrue = NULL, tona = NULL){

  if(!is.null(tofalse) | !is.null(totrue)){
    glottodata <- glottorecode_logical(glottodata = glottodata, structure = structure, tofalse = tofalse, totrue = totrue)
  }

  if(!is.null(tona)){
    glottodata <- glottorecode_missing(glottodata, tona = tona)
  }

 glottodata
}

#' Recode character columns to TRUE/FALSE
#'
#' @param structure structure table
#' @param totrue values to recode to TRUE
#' @param tofalse values to recode to FALSE
#' @param glottodata glottodata list
#'
#' @noRd
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottorecode_logical(glottodata, totrue = c("y", "Y", 1), tofalse = c("n", "N", 0))
glottorecode_logical <- function(glottodata, structure = NULL, totrue = NULL, tofalse = NULL){

  if(!glottocheck_hasstructure(glottodata) & is.null(structure)){
    stop("Please provide a structure table with at least a type column. Run glottocreate_structuretable() to create it.")
  } else if(is.null(structure)){
    splitted <- glottosplitmergemeta(glottodata)
    data <- splitted[[1]]
    structure <- splitted[[2]][["structure"]]
  } else{ # structure is provided
    splitted <- glottosplitmergemeta(glottodata)
    data <- splitted[[1]]
  }

  types <- structure$type
  cbinary <- structure$varname[which(types == "asymm" | types == "symm")]

  if(!purrr::is_empty(cbinary)){
    bindat <- data[, cbinary]
    # Prepare message about what will be converted:
      allevmat <- sapply(lapply(bindat, as.factor), levels)
      allevuniq <- unique(unlist(allevmat))
      notlog <- allevuniq[allevuniq %nin% c(totrue, tofalse)]
      if(!purrr::is_empty(notlog)){
        message("\n\n For some variables of type 'symm' and 'asymm' it is unclear whether they are TRUE or FALSE. \n If you do want to convert them, you should specify 'totrue' and 'tofalse'. \n\n The following values are not converted to TRUE or FALSE, but are set to NA:")
        printmessage(paste(notlog, collapse = ", "))
        bindat <- recode_df(data = bindat, old = notlog, new = NA)
      }
    if(!is.null(totrue)){bindat <- recode_df(data = bindat, old = totrue, new = TRUE) }
    if(!is.null(tofalse)){bindat <- recode_df(data = bindat, old = tofalse, new = FALSE) }
    bindat <- apply(bindat, 2, as.logical)
    data[, cbinary] <- bindat
    message("Values in binary columns (symm/asymm) recoded to TRUE/FALSE \n")
  }
  glottosplitmergemeta(glottodata = data, splitted = splitted)
}

#' Recode missing values to NA
#'
#' @param glottodata glottodata
#' @param tona Optional, additional values to recode to NA
#' @family <glottorecode>
#' @noRd
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottorecode_missing(glottodata, tona = "?")
glottorecode_missing <- function(glottodata, tona){

  if(glottocheck_isglottodata(glottodata)){
    splitted <- glottosplitmergemeta(glottodata)
    glottodata <- splitted[[1]]
  } else{
    splitted <- NULL
  }

  if(!purrr::is_empty(row.names(glottodata)) ){
    glottodata <- tibble::rownames_to_column(glottodata, var = "rowname")
    glottodata <- data.frame(lapply(glottodata, recode_tona, tona = tona)) # this drops row names
    glottodata <- tibble::column_to_rownames(glottodata, var = "rowname")
  } else {
    glottodata <- data.frame(lapply(glottodata, recode_tona, tona = tona))
  }


  message("Missing values recoded to NA \n")

  if(!is.null(splitted)){
    glottodata <- glottosplitmergemeta(glottodata = glottodata, splitted = splitted)
  }
  glottodata
}

#' Fix colnames of excel files in which colnames refer to another cell
#'
#' @param glottodata glottodata or glottosubdata
#' @noRd
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
