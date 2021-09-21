#' Clean glottodata
#'
#' @param glottodata User-provided glottodata (list or data.frame)
#'
#' @return
#' @export
#'
#' @examples
#' # list:
#' glottodata_list <- get_glottodata()
#' glottodata_list <- cleanglottodata(glottodata_list)
#' glottodata_list[[1]] <- cleanglottodata(glottodata_list[[1]])
#'
#' # data.frame:
#' glottodata <- get_glottodata(meta = FALSE)
#' glottodata <- cleanglottodata(glottodata)
cleanglottodata <- function(glottodata, structure = NULL){
  if(checkmetadata_hasstructure(glottodata) ){
    glottodata[["glottodata"]] <- cleandata_recodemissing(glottodata[["glottodata"]])
    glottodata[["glottodata"]] <- cleandata_recodelogical(glottodata = glottodata[["glottodata"]], structure = glottodata[["structure"]])
    glottodata

  } else {
    if(is.null(structure)){stop("Please provide a structure data.frame with at least a type column. Run create_structuresheet() to create it.")}
        glottodata <- cleandata_recodemissing(glottodata)
        glottodata <- cleandata_recodelogical(glottodata, structure)
        glottodata
    }
}



#' Reclass missing values to NA
#'
#' @param glottodata User-provided glottodata
#' @param rec Optional, additional values to recode to NA
#'
#' @return
#' @export
#'
#' @examples
cleandata_recodemissing <- function(glottodata, rec = NULL){
  data <- glottodata[,-1]
  na_strings <- c(naniar::common_na_strings, "?", rec)
  data %>%
    naniar::replace_with_na_all(condition = ~. %in% na_strings)
  message("Missing values recoded to NA \n")
  cbind(glottodata[,1, drop = FALSE], data)
}

cleandata_recodelogical <- function(glottodata, structure){
  types <- structure$type
  cbinary <- structure$varname[which(types == "asymm" | types == "symm")]

  if(!is.null(cbinary)){
    bindat <- glottodata[, cbinary]
    bindat[bindat == "Y" | bindat == "y" | bindat == 1] <- TRUE
    bindat[bindat == "N" | bindat == "n" | bindat == 0] <- FALSE
    glottodata[, cbinary] <- bindat
    message("Values in binary columns (symm/asymm) recoded to TRUE/FALSE \n")
  }
glottodata
}



