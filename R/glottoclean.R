#' Clean glottodata
#'
#' @param glottodata User-provided glottodata (list or data.frame)
#' @family <glottoclean>
#' @return
#' @export
#'
#' @examples
#' # list:
#' glottodata_list <- glottoget()
#' glottodata_list <- glottoclean(glottodata_list)
#' glottodata_list[[1]] <- glottoclean(glottodata_list[[1]])
#'
#' # data.frame:
#' glottodata <- glottoget(meta = TRUE)
#' glottodata <- glottoclean(glottodata)
glottoclean <- function(glottodata, structure = NULL){
  if(checkmetadata_hasstructure(glottodata) ){
    # glottodata[["glottodata"]] <- glottoclean_recodemissing(glottodata[["glottodata"]])
    glottodata[["glottodata"]] <- glottoclean_recodelogical(glottodata = glottodata[["glottodata"]], structure = glottodata[["structure"]])
    glottodata

  } else {
    if(is.null(structure)){stop("Please provide a structure table with at least a type column. Run create_structuretable() to create it.")}
        # glottodata <- glottoclean_recodemissing(glottodata)
        glottodata <- glottoclean_recodelogical(glottodata, structure)
        glottodata
    }
}



#' Recode missing values to NA
#'
#' @param glottodata User-provided glottodata
#' @param rec Optional, additional values to recode to NA
#' @family <glottoclean>
#' @return
#' @export
#'
#' @examples
#' glottodata <- glottoget()
#' glottoclean_recodemissing(glottodata, rec = "N")
glottoclean_recodemissing <- function(glottodata, rec = NULL){
  # maybe better to do with tribble lookup table https://r-pkgs.org/package-within.html
  data <- glottodata[,-1]
  na_strings <- c(naniar::common_na_strings, "?", rec)
  data <- data %>%
    naniar::replace_with_na_all(condition = ~. %in% na_strings)
  message("Missing values recoded to NA \n")
  cbind(glottodata[,1, drop = FALSE], data)
}

#' Recode logical columns to TRUE/FALSE
#'
#' @param glottodata User-provided glottodata
#' @param structure A glottodata structure table
#' @keywords internal
#' @return
#' @export
#'
glottoclean_recodelogical <- function(glottodata, structure){
  # maybe better to do with tribble lookup table https://r-pkgs.org/package-within.html
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



