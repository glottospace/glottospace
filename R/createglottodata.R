#' Create empty glottodata for specified glottocodes and variables.
#'
#' Output is saved as an excel file with the following sheets: glottodata, structure, metadata, readme
#'
#' @param variables Either a vector with variable names, or a single number indicating the total number of variable columns to be generated
#' @param filename
#' @param ... Other parameters passed to create_readmesheet(maintainer, email, citation, url)
#' @param glottocodes Character vector of glottocodes
#'
#' @return
#' @export
#'
#' @examples
#' createglottodata(glottocodes = c("yucu1253", "tani1257"), variables = 3, filename = "glottodata.xlsx")
createglottodata <- function(glottocodes, variables, filename = NULL, ...){
 if(!all(glottocode_exists(glottocodes)) ){stop("Not all glottocodes are valid. Use glottocode_exists() to check which ones. ")}

  if(is.numeric(variables) & length(variables) == 1){
    varnames <- paste0("var", sprintf("%03d", seq(1,variables)))
  } else {
    varnames <- variables
  }

  glottodata <- create_glottosheet(glottocodes = glottocodes, varnames = varnames)
  structure <- create_structuresheet(glottocodes = glottocodes, varnames = varnames)
  metadata <- create_metasheet(varnames = varnames)
  references <- create_refsheet(glottocodes = glottocodes, varnames = varnames)
  readme <- create_readmesheet(...) # for testing remove ... readme <- create_readmesheet()

  lookup <- create_lookupsheet()

  sheetlist <- list("glottodata" = glottodata,
                    "structure" = structure,
                    "metadata" = metadata,
                    "references" = references,
                    "readme" = readme,
                    "lookup" = lookup)


  if(!is.null(filename)){
    # check if path exists, if subfolder doesn't exist, it doesn't write.
  writexl::write_xlsx(sheetlist, path = filename) # works better than openxlsx, which omitted some columns..
  # message that file was saved
  }

  sheetlist
}

#' Create empty glottosubdata for specified glottocodes, groups and variables.
#'
#' glottosubcodes are created in the following form: glottocode_group_record.
#' For example: abcd1234_aaa_0001, abcd1234_aaa_0002, abcd1234_bbb_0001, abcd1234_bbb_0002
#'
#' Output is saved as an excel file with the following sheets: glottodata, structure, metadata, readme
#'
#' @param variables Either a vector with variable names, or a single number indicating the total number of variable columns to be generated
#' @param filename
#' @param ... Other parameters passed to create_readmesheet(maintainer, email, citation, url)
#' @param glottocodes Character vector of glottocodes
#' @param groups Character vector of group names
#' @param n Number of records to be assigned to each group
#'
#' @return
#' @export
#'
#' @examples
#' createglottosubdata(glottocodes = c("yucu1253", "tani1257"), variables = 3, groups = c("a", "b"), n = 5, filename = "glottosubdata.xlsx")
createglottosubdata <- function(glottocodes, variables, filename = NULL, groups, n = NULL, ...){
  if(!all(glottocode_exists(glottocodes)) ){stop("Not all glottocodes are valid. Use glottocode_exists() to check which ones. ")}

  if(is.numeric(variables) & length(variables) == 1){
    varnames <- paste0("var", sprintf("%03d", seq(1,variables)))
  } else {
    varnames <- variables
  }

  glottosublist <- vector(mode='list', length= length(glottocodes))

  for(i in seq(glottocodes)){
  glottosubcodes <- create_glottosubcodes(glottocode = glottocodes[i], groups = groups, n = n)
  glottosubdata <- create_glottosheet(glottocodes = glottosubcodes, varnames = varnames)
  colnames(glottosubdata)[1] <- "glottosubcode"
  glottosublist[[i]] <- glottosubdata
  names(glottosublist)[[i]] <- glottocodes[i]
  }

  structure <- create_structuresheet(glottocodes = glottocodes, varnames = varnames)
  metadata <- create_metasheet(varnames = varnames)
  references <- create_refsheet(glottocodes = glottocodes, varnames = varnames)

  readme <- create_readmesheet(...) # for testing remove ... readme <- create_readmesheet()

  lookup <- create_lookupsheet()

  sheetlist <- list("structure" = structure,
                    "metadata" = metadata,
                    "references" = references,
                    "readme" = readme,
                    "lookup" = lookup)

  glottosubsheets <- c(glottosublist, sheetlist)

  if(!is.null(filename)){
    # check if path exists, if subfolder doesn't exist, it doesn't write.
    writexl::write_xlsx(glottosubsheets, path = filename) # works better than openxlsx, which omitted some columns..
    # message that file was saved
  }

  glottosubsheets
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
  metadata <- data.frame(matrix(nrow = length(varnames), ncol = 6) )
  colnames(metadata) <- c("varname", "description", "reference", "page", "contributor", "remarks")
  metadata[,"varname"] <- varnames
  metadata
}

create_refsheet <- function(glottocodes, varnames){
  references <- data.frame(matrix(nrow = length(glottocodes), ncol = (length(varnames)*2)+1 ) )
  colnames(references) <- c("glottocode", paste(rep(varnames, each = 2) , c("ref", "page"), sep = "_") )
  references[,"glottocode"] <- glottocodes
  references
}

create_readmesheet <- function(maintainer = NULL, email = NULL, citation = NULL, url = NULL ){
  readme <- data.frame(matrix(nrow = 5, ncol = 2) )
  readme[,1] <- c("maintainer", "email", "citation", "url", "This database was created using the glottospace R package")
  readme[,2] <- c(ifelse(is.null(maintainer), NA, maintainer),
                  ifelse(purrr::is_empty(email), NA, email),
                  ifelse(is.null(citation), NA, citation),
                  ifelse(is.null(url), NA, url),
                  paste0("version " ,packageVersion("glottospace") ) )
  colnames(readme) <- NULL
  readme
}

create_lookupsheet <- function(){
  lookup <- data.frame(matrix(nrow = 7, ncol = 2) )
  lookup[,1] <- c("symm",
                  "asymm",
                  "factor",
                  "ordered",
                  "numeric",
                  "ordratio",
                  "logratio")
  lookup[,2] <- c("Symmetric binary: positive match is considered the same as a negative match. Variable has three possible values: Y, N, NA where NA stands for missing value",
                  "Asymmetric binary: positive match and negative match are not equal. Variable has three possible values: Y, N, NA where NA stands for missing value",
                  "Nominal variables. Levels should be given by a letter or abbreviation. For example: A, B, C. NA stands for missing value",
                  "Ordinal variables. NA stands for missing value",
                  "Interval scaled variables. Can have any numeric value and NA",
                  "Ratio scaled variables. Setting type to ordratio indicates that values should be treated as ordinal variables",
                  "Ratio scaled variables. Setting type to logratio indicates that values should be log transformed in subsequent analyses")
  colnames(lookup) <- c("type_lookup", "type_legend")
  lookup
}

#' Create glottosubcodes
#'
#' Generate glottosubcodes for a single glottocode
#' @param glottocode A glottocode
#' @param groups Vector of group names (optional)
#' @param n Number of records in each group
#'
#' @return
#' @export
#'
#' @examples
#' create_glottosubcodes(glottocode = "yucu1253", groups = c("a", "b"), n = 5)
create_glottosubcodes <- function(glottocode, groups = NULL, n){
  if(length(glottocode) != 1){stop("Please provide a single glottocode")}
  n <- ifelse(!exists("n"), 1, n)
  glottogroups <- paste(glottocode, groups, sep = "_")
  glottosubcodes <- 1:(n * length(glottogroups))


  for(i in seq(1,length(glottogroups))){
    glottosubcodes[(i*n - n) + seq(1,n)] <- paste(glottogroups[i], sprintf("%04d", seq(1,n)), sep = "_")
  }

  glottosubcodes

}
