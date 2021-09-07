#' Create empty glottodata for specified glottocodes and variables.
#'
#' Output is saved as an excel file with the following sheets: glottodata, structure, metadata, README
#'
#' @param glottocodes Character vector of glottocodes
#' @param variable Either a vector with variable names, or a number indicating the total number of variable names to be generated (excluding glottocode).
#' @param path
#'
#' @return
#' @export
#'
#' @examples
createglottodata <- function(glottocodes, variables, path,
                             maintainer = NULL, email = NULL, citation = NULL, url = NULL){
 if(!all(glottocode_exists(glottocodes)) ){stop("Not all glottocodes are valid. Use glottocode_exists() to check which ones. ")}

  if(is.numeric(variables)){
    varnames <- paste0("v", variables)
  } else {
    varnames <- variables
  }

  # empty workbook
  wb <- openxlsx::createWorkbook()

  glottodata <- create_glottosheet(glottocodes = glottocodes, varnames = varnames)
  # openxlsx::addWorksheet(glottodata, "glottosheet")
  # openxlsx::writeData(glottodata, sheet = "glottosheet", x = glottodata, startCol = 1)

  structure <- create_structuresheet(glottocodes = glottocodes, varnames = varnames)
  metadata <- create_metasheet(varnames = varnames)
  readme <- create_readmesheet(maintainer = maintainer, email = email, citation = email, url = url)

  sheetlist <- list(glottodata, structure, metadata, readme)

  lapply(seq_along(sheetlist), function(i){
    openxlsx::addWorksheet(wb=glottowb, sheetName = names(sheetlist[i]))
    openxlsx::writeData(glottowb, sheet = i, sheetlist[[i]][-length(sheetlist[[i]])])
  })


  # dropdown
  # https://stackoverflow.com/questions/29898269/possible-to-write-excel-formulas-or-data-validation-using-r

}

create_glottosheet <- function(glottocodes, varnames){
  colnames <- c("glottocode", varnames)
  glottodata <- data.frame(matrix(nrow=length(glottocodes),ncol=length(colnames))) # alternative: tbl <- colnames %>% purrr::map_dfc(setNames, object = list(character()))
  colnames(glottodata) <- colnames
  glottodata[,"glottocode"] <- glottocodes
  glottodata
}

create_structuresheet <- function(glottocodes, varnames){
structure <- data.frame(matrix(nrow = length(varnames), ncol = 6) )
colnames(structure) <- c("varname", "type", "levels", "weight", "groups", "subgroups")
structure[,"varname"] <- varnames
structure[,"weight"] <- 1
structure
}

create_metasheet <- function(varnames){
  metadata <- data.frame(matrix(nrow = length(varnames), ncol = 5) )
  colnames(metadata) <- c("varname", "description", "source", "contributor", "remarks")
  metadata
}

create_readmesheet <- function(maintainer = NULL, email = NULL, citation = NULL, url = NULL ){
  readme <- data.frame(matrix(nrow = 5, ncol = 2) )
  readme[,1] <- c("maintainer", "email", "citation", "url", "This database was created using the glottospace R package")
  readme[,2] <- c(ifelse(is.null(maintainer), NA, maintainer),
                  ifelse(is.null(email), NA, email),
                  ifelse(is.null(citation), NA, citation),
                  ifelse(is.null(url), NA, url),
                  paste0("version " ,packageVersion("glottospace") ) )
  readme
}
