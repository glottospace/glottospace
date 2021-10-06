


#' Create empty glottodata for specified glottocodes and variables.
#'
#' Output is a list with one data.frame for glottodata (and a number of metatables if meta = TRUE). Output can  also be saved as an excel file.
#'
#' @param variables Either a vector with variable names, or a single number indicating the total number of variable columns to be generated
#' @param filename Optional name of excel file where to store glottodata
#' @param ... Other parameters passed to glottocreate_readmetable(maintainer, email, citation, url)
#' @param glottocodes Character vector of glottocodes
#' @param meta By default, meta tables are created. Use meta=FALSE to exclude them.
#' @param simplify By default, if only one table is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#'
#' @return
#' @export
#' @family <glottocreate><glottodata>
#' @examples
#' glottocreate_data(glottocodes = c("yucu1253", "tani1257"), variables = 3, filename = "glottodata.xlsx")
#' glottocreate_data(glottocodes = c("yucu1253", "tani1257"), variables = 3, filename = "glottodata_simple.xlsx", meta = FALSE)
glottocreate_data <- function(glottocodes, variables, filename = NULL, meta = TRUE, simplify = TRUE, ...){
 if(!all(glottocode_exists(glottocodes)) ){stop("Not all glottocodes are valid. Use glottocode_exists() to check which ones. ")}

  if(is.numeric(variables) & length(variables) == 1){
    varnames <- paste0("var", sprintf("%03d", seq(1,variables)))
  } else {
    varnames <- variables
  }

  glottodata <- glottocreate_glottotable(glottocodes = glottocodes, varnames = varnames)
  if(meta == TRUE){
  structure <- glottocreate_structuretable(glottocodes = glottocodes, varnames = varnames)
  metadata <- glottocreate_metatable(varnames = varnames)
  references <- glottocreate_reftable(glottocodes = glottocodes, varnames = varnames)
  readme <- glottocreate_readmetable(...) # for testing remove ... readme <- glottocreate_readmetable()

  lookup <- glottocreate_lookuptable()

  tablelist <- list("glottodata" = glottodata,
                    "structure" = structure,
                    "metadata" = metadata,
                    "references" = references,
                    "readme" = readme,
                    "lookup" = lookup)
  } else {
    tablelist <- list("glottodata" = glottodata)
  }


  if(!is.null(filename)){
    # check if path exists, if subfolder doesn't exist, it doesn't write.
    if(tools::file_ext(filename) == ""){filename <- paste0(filename, ".xlsx")}
    writexl::write_xlsx(tablelist, path = filename) # works better than openxlsx, which omitted some columns..
    message(paste("Glottodata saved: ", filename))
  }

  if(simplify == TRUE & length(tablelist) == 1 & any(class(tablelist) == "list") ){
    tablelist <- tablelist[[1]]
  }

  tablelist
}

#' Create empty glottosubdata for specified glottocodes, groups and variables.
#'
#' glottosubcodes are created in the following form: glottocode_group_record.
#' For example: abcd1234_aaa_0001, abcd1234_aaa_0002, abcd1234_bbb_0001, abcd1234_bbb_0002
#'
#' Output is a list with one data.frame per language (and a number of metatables if meta = TRUE). Output can  also be saved as an excel file.
#'
#' @param variables Either a vector with variable names, or a single number indicating the total number of variable columns to be generated
#' @param filename  Optional name of excel file where to store glottodata
#' @param ... Other parameters passed to glottocreate_readmetable(maintainer, email, citation, url)
#' @param glottocodes Character vector of glottocodes
#' @param groups Character vector of group names
#' @param n Number of records to be assigned to each group
#' @param meta Should metatables be created?
#'
#' @return A list with a data.frame for each languages (and metadata if meta = TRUE)
#' @export
#'
#' @family <glottoget_path><glottocreate>
#'
#' @examples
#' glottocreate_subdata(glottocodes = c("yucu1253", "tani1257"), variables = 3, groups = c("a", "b"), n = 5, filename = "glottosubdata.xlsx")
glottocreate_subdata <- function(glottocodes, variables, filename = NULL, groups, n = NULL, meta = TRUE, ...){
  if(!all(glottocode_exists(glottocodes)) ){stop("Not all glottocodes are valid. Use glottocode_exists() to check which ones. ")}

  if(is.numeric(variables) & length(variables) == 1){
    varnames <- paste0("var", sprintf("%03d", seq(1,variables)))
  } else {
    varnames <- variables
  }

  glottosublist <- vector(mode='list', length= length(glottocodes))

  for(i in seq(glottocodes)){
  glottosubcodes <- glottocreate_glottosubcodes(glottocode = glottocodes[i], groups = groups, n = n)
  glottosubdata <- glottocreate_glottotable(glottocodes = glottosubcodes, varnames = varnames)
  colnames(glottosubdata)[1] <- "glottosubcode"
  glottosublist[[i]] <- glottosubdata
  names(glottosublist)[[i]] <- glottocodes[i]
  }

  if(meta == TRUE){
  structure <- glottocreate_structuretable(glottocodes = glottocodes, varnames = varnames)
  metadata <- glottocreate_metatable(varnames = varnames)
  references <- glottocreate_reftable(glottocodes = glottocodes, varnames = varnames)

  readme <- glottocreate_readmetable(...) # for testing remove ... readme <- glottocreate_readmetable()

  lookup <- glottocreate_lookuptable()

  tablelist <- list("structure" = structure,
                    "metadata" = metadata,
                    "references" = references,
                    "readme" = readme,
                    "lookup" = lookup)

  glottosubtables <- c(glottosublist, tablelist)
  } else {
    glottosubtables <- glottosublist
  }

  if(!is.null(filename)){
    # check if path exists, if subfolder doesn't exist, it doesn't write.
    if(tools::file_ext(filename) == ""){filename <- paste0(filename, ".xlsx")}
    writexl::write_xlsx(glottosubtables, path = filename) # works better than openxlsx, which omitted some columns..
    message(paste("Glottosubdata saved: ", filename))
  }

  glottosubtables
}



glottocreate_glottotable <- function(glottocodes, varnames){
  colnames <- c("glottocode", varnames)
  glottodata <- data.frame(matrix(nrow=length(glottocodes),ncol=length(colnames))) # alternative: tbl <- colnames %>% purrr::map_dfc(setNames, object = list(character()))
  colnames(glottodata) <- colnames
  glottodata[,"glottocode"] <- glottocodes
  glottodata
}

#' create structure table for glottodata
#'
#' @param glottocodes Character vector of glottocodes
#' @param varnames Character vector of variable names
#'
#' @return
#' @export
#'
#' @examples
#' glottocreate_structuretable(glottocodes = c("yucu1253", "tani1257"), varnames = c("var001", "var002", "var003"))
glottocreate_structuretable <- function(glottocodes, varnames = NULL){
structure <- data.frame(matrix(nrow = length(varnames), ncol = 6) )
colnames(structure) <- c("varname", "type", "levels", "weight", "groups", "subgroups")
if(!is.null(varnames)){
structure[,"varname"] <- varnames
structure[,"weight"] <- 1
}
structure
}

glottocreate_metatable <- function(varnames){
  metadata <- data.frame(matrix(nrow = length(varnames), ncol = 6) )
  colnames(metadata) <- c("varname", "description", "reference", "page", "contributor", "remarks")
  metadata[,"varname"] <- varnames
  metadata
}

glottocreate_reftable <- function(glottocodes, varnames){
  references <- data.frame(matrix(nrow = length(glottocodes), ncol = (length(varnames)*2)+1 ) )
  colnames(references) <- c("glottocode", paste(rep(varnames, each = 2) , c("ref", "page"), sep = "_") )
  references[,"glottocode"] <- glottocodes
  references
}

glottocreate_readmetable <- function(maintainer = NULL, email = NULL, citation = NULL, url = NULL ){
  readme <- data.frame(matrix(nrow = 5, ncol = 2) )
  readme[,1] <- c("maintainer", "email", "citation", "url", "This database was created using the glottospace R package")
  readme[,2] <- c(ifelse(is.null(maintainer), NA, maintainer),
                  ifelse(purrr::is_empty(email), NA, email),
                  ifelse(is.null(citation), NA, citation),
                  ifelse(is.null(url), NA, url),
                  paste0("version " ,packageVersion("glottospace") ) )
  colnames(readme) <- c("info", "value")
  readme
}

glottocreate_lookuptable <- function(){
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
#' glottocreate_glottosubcodes(glottocode = "yucu1253", groups = c("a", "b"), n = 5)
glottocreate_glottosubcodes <- function(glottocode, groups = NULL, n){
  if(length(glottocode) != 1){stop("Please provide a single glottocode")}
  n <- ifelse(!exists("n"), 1, n)
  glottogroups <- paste(glottocode, groups, sep = "_")
  glottosubcodes <- 1:(n * length(glottogroups))


  for(i in seq(1,length(glottogroups))){
    glottosubcodes[(i*n - n) + seq(1,n)] <- paste(glottogroups[i], sprintf("%04d", seq(1,n)), sep = "_")
  }

  glottosubcodes

}

glottocreate_dummydata <- function(){
  dummy <- glottocreate_data(glottocodes = c("yucu1253", "tani1257", "ticu1245", "orej1242", "nade1244", "mara1409"), variables = 3)
  dummy$glottodata[,"var001"] <- c("Y", NA, "Y", "N", "N", "N")
  dummy$glottodata[,"var002"] <- c("a", "b", "a", "b", "c", "a")
  dummy$glottodata[,"var003"] <- c("N", "Y", "Y", "N", "Y", "N")

  dummy$structure[,"type"] <- c("symm", "factor", "symm")

  dummy
}

glottocreate_dummysubdata <- function(){
  dummy <- glottocreate_subdata(glottocodes = c("yucu1253", "tani1257"), variables = 3, groups = c("a", "b"), n = 5)
  dummy[[1]][,"var001"] <- sample(c("Y", "N", NA), size = 10, replace = TRUE)
  dummy[[1]][,"var002"] <- sample(c("a", "b", NA), size = 10, replace = TRUE)
  dummy[[1]][,"var003"] <- sample(c("Y", "N", NA), size = 10, replace = TRUE)

  dummy[[2]][,"var001"] <- sample(c("Y", "N", NA), size = 10, replace = TRUE)
  dummy[[2]][,"var002"] <- sample(c("a", "b", NA), size = 10, replace = TRUE)
  dummy[[2]][,"var003"] <- sample(c("Y", "N", NA), size = 10, replace = TRUE)

  dummy$structure[,"type"] <- c("symm", "factor", "symm")

  dummy
}

#' Add a table to glottodata
#'
#' @param glottodata A glottodata table, or a list of glottodata tables
#' @param table A table to be added
#' @param name A name for the table
#'
#' @return
#' @export
#'
#' @examples
#' structuretable <- glottocreate_structuretable()
#' glottodata_addtable(glottodata, table = structuretable, name = "structure")
glottodata_addtable <- function(glottodata, table, name){
  if(is_list(table) & length(table) != 1){stop("Please provide either a data.frame or a list of 1 data.frame")}

  if(!is_list(table)){
    table <- list(table)
    }
  names(table) <- name

  if(!is_list(glottodata)){
    glottodata <- list("glottodata" = glottodata)
  }

  c(glottodata, table)



}
