#' Generate empty glottodata or glottosubdata for a set of glottocodes.
#'
#' Creates glottodata/glottosubdata and optionally save it as excel file.
#'
#' By default, glottodata will be created. In case a groups argument is provided, glottosubdata will be created.
#'
#' glottodata has one table for all languages (and a number of metatables if meta = TRUE), with one row per glottocode.
#' glottosubdata has one table for each language (and a number of metatables if meta = TRUE), with one row per glottosubcode.
#'
#' Run glottoget("demodata") or glottoget("demosubdata") to see examples.
#'
#' In case you already have your own dataset and want to convert it into glottodata, use: glottoconvert().
#'
#' @param glottocodes Character vector of glottocodes
#' @param variables Either a vector with variable names, or a single number indicating the total number of variable columns to be generated
#' @param meta Should metatables be created?
#' @param filename  Optional name of excel file where to store glottodata
#' @param simplify By default, if a glottodata table is created without metadata, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @param groups Character vector of group names (only for glottosubdata)
#' @param n Optional, number of records to be assigned to each group (only for glottosubdata)
#' @param maintainer Name of the person/organization maintaining the data (optional, added to readme tab)
#' @param email Email address of maintainer/contact person (optional, added to readme tab)
#' @param citation How to cite the data (optional, added to readme tab)
#' @param url Link to a webpage (optional, added to readme tab).
#' @param levels Optional character vector with levels across all variables
#' @param check Should glottocodes be checked? Default is FALSE because takes much time to run.
#'
#' @return A glottodata or glottosubdata object (either with or without metadata). The output can be a list or a data.frame.
#' @export
#'
#' @family <glottoget><glottocreate>
#'
#' @examples
#' # Creates glottodata table without metadata tables
#' glottocreate(glottocodes = c("yucu1253", "tani1257"),
#' variables = 3, meta = FALSE)
#'
#' # Creates glottodata table with metadata tables (stored in a list):
#' glottocreate(glottocodes = c("yucu1253", "tani1257"), variables = 3)
#'
#'
#' # Creates glottosubdata table (stored in a list)
#' glottocreate(glottocodes = c("yucu1253", "tani1257"),
#' variables = 3, groups = c("a", "b") )
#'
#' # Create glottodata table and add some information to the readme table:
#' glottocreate(glottocodes = c("yucu1253", "tani1257"), variables = 3,
#' maintainer = "Your name", email = "yourname@domain.com")
#'
#'
glottocreate <- function(glottocodes, variables,
                         meta = TRUE, filename = NULL,
                         simplify = TRUE,
                         groups = NULL, n = NULL,
                         levels = NULL,
                         check = FALSE,
                         maintainer = NULL, email = NULL, citation = NULL, url = NULL){

  if(is.null(groups)){
    glottocreate_data(glottocodes = glottocodes, variables = variables,
                      filename = filename, meta = meta,
                      simplify = simplify,
                      levels = levels,
                      check = check,
                      maintainer = maintainer, email = email, citation = citation, url = url)
  } else {
    glottocreate_subdata(glottocodes = glottocodes, variables = variables,
                         filename = filename, meta = meta,
                         simplify = simplify,
                         groups = groups, n = n,
                         levels = levels,
                         check = check,
                         maintainer = maintainer, email = email, citation = citation, url = url)
  }

}


#' Create empty glottodata for specified glottocodes and variables.
#'
#' Output is a list with one data.frame for glottodata (and a number of metatables if meta = TRUE). Output can  also be saved as an excel file.
#'
#' @param variables Either a vector with variable names, or a single number indicating the total number of variable columns to be generated
#' @param filename Optional name of excel file where to store glottodata
#' @param glottocodes Character vector of glottocodes
#' @param meta By default, meta tables are created. Use meta=FALSE to exclude them.
#' @param simplify By default, if only one table is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#' @param maintainer Name of the person/organization maintaining the data (optional)
#' @param email Email address of maintainer/contact person (optional)
#' @param citation How to cite the data (optional)
#' @param url Optional url linking to a webpage.
#' @param check Should glottocodes be checked? Default is FALSE because takes much time to run.
#' @param levels Optional character vector with levels across all variables
#' @noRd
#' @family <glottocreate><glottodata>
#' @examples
#' glottocreate_data(glottocodes = c("yucu1253", "tani1257"),
#'                    variables = 3)
#' glottocreate_data(glottocodes = c("yucu1253", "tani1257"),
#'      variables = 3, meta = FALSE)
#'
glottocreate_data <- function(glottocodes, variables, filename = NULL, meta = TRUE, check = FALSE, simplify = TRUE, levels = NULL, maintainer = NULL, email = NULL, citation = NULL, url = NULL){
 if(check){
  if(!all(glottocode_exists(glottocodes)) ){stop("Not all glottocodes are valid. Use glottocode_exists() to check which ones. ")}
 }
  if(is.numeric(variables) & length(variables) == 1){
    varnames <- paste0("var", sprintf("%03d", seq(1,variables)))
  } else {
    varnames <- variables
  }

  glottodata <- glottocreate_glottotable(glottocodes = glottocodes, varnames = varnames)
  if(meta == TRUE){
    metatables <- glottocreate_metatables(glottocodes = glottocodes, varnames = varnames,
                                         levels = levels,
                                         maintainer = maintainer, email = email, citation = citation, url = url)
    glottodata <- list("glottodata" = glottodata)
    tablelist <- c(glottodata, metatables)
  } else {
    tablelist <- list("glottodata" = glottodata)
  }

  if(simplify == TRUE & length(tablelist) == 1 & inherits(tablelist, what = "list" ) ){
    tablelist <- tablelist[[1]]
  }

  if(!is.null(filename)){
    glottosave(glottodata = tablelist, filename = filename)
  }


  tablelist <- add_class(object = tablelist, class = "glottodata")
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
#' @param glottocodes Character vector of glottocodes
#' @param groups Character vector of group names
#' @param n Number of records to be assigned to each group
#' @param meta Should metatables be created?
#' @param maintainer Name of the person/organization maintaining the data (optional)
#' @param email Email address of maintainer/contact person (optional)
#' @param citation How to cite the data (optional)
#' @param url Optional url linking to a webpage.
#' @param check Should glottocodes be checked? Default is FALSE because takes much time to run.
#' @param levels Optional character vector with levels across all variables
#' @param simplify By default, if only one table is loaded, the data will be returned as a data.frame (instead of placing the data inside a list of length 1)
#'
#' @noRd
#'
#' @return A table or list of tables
#'
#' @family <glottocreate>
#'
#' @examples
#' glottocreate_subdata(glottocodes = c("yucu1253", "tani1257"),
#' variables = 3, groups = c("a", "b"), n = 5)
glottocreate_subdata <- function(glottocodes, variables, groups, filename = NULL, check = FALSE, levels = NULL, n = NULL, meta = TRUE, maintainer = NULL, email = NULL, citation = NULL, url = NULL, simplify = TRUE){
  if(check){
    if(!all(glottocode_exists(glottocodes)) ){stop("Not all glottocodes are valid. Use glottocode_exists() to check which ones. ")}
  }

  if(is.numeric(variables) & length(variables) == 1){
    varnames <- paste0("var", sprintf("%03d", seq(1,variables)))
  } else {
    varnames <- variables
  }

  if(is.null(n)){n <- 1}

  glottosublist <- vector(mode='list', length= length(glottocodes))

  for(i in seq(glottocodes)){
  glottosubcodes <- glottocreate_glottosubcodes(glottocode = glottocodes[i], groups = groups, n = n)
  glottosubdata <- glottocreate_glottotable(glottocodes = glottosubcodes, varnames = varnames)
  colnames(glottosubdata)[1] <- "glottosubcode"
  glottosublist[[i]] <- glottosubdata
  names(glottosublist)[[i]] <- glottocodes[i]
  }

  if(meta == TRUE){
  metatables <- glottocreate_metatables(glottocodes = glottocodes, glottosubcodes = glottosubcodes, varnames = varnames,
                                       levels = levels,
                                       maintainer = maintainer, email = email, citation = citation, url = url)

  glottosubtables <- c(glottosublist, metatables)
  } else {
    glottosubtables <- glottosublist
  }

  if(simplify == TRUE & length(glottosubtables) == 1 & inherits(glottosubtables, what = "list" ) ){
    glottosubtables <- glottosubtables[[1]]
  }

  if(!is.null(filename)){
    glottosave(glottodata = glottosubtables, filename = filename)
  }

  glottosubtables <- add_class(object = glottosubtables, class = "glottosubdata")
  glottosubtables
}


#' Create metatables for glottodata or glottosubdata
#'
#' @param glottocodes Character vector of glottocodes
#' @param varnames Character vector of variable names
#' @param maintainer Name of the person/organization maintaining the data (optional)
#' @param email Email address of maintainer/contact person (optional)
#' @param citation How to cite the data (optional)
#' @param url Optional url linking to a webpage.
#' @param levels Optional character vector with levels across all variables
#' @return a list of metatables
#' @export
#' @keywords internal
#'
glottocreate_metatables <- function(glottocodes = NULL, glottosubcodes = NULL, varnames = NULL, levels = NULL, maintainer = NULL, email = NULL, citation = NULL, url = NULL){
  if(is.null(glottosubcodes)){
    glottocreate_metadatatables(glottocodes = glottocodes,
                                varnames = varnames, levels = levels,
                                maintainer = maintainer, email = email, citation = citation, url = url)
  } else {
    glottocreate_metasubdatatables(glottocodes = glottocodes, glottosubcodes = glottosubcodes,
                                   varnames = varnames, levels = levels,
                                   maintainer = maintainer, email = email, citation = citation, url = url)
  }
}



#' Create metatables for glottodata
#'
#' @param glottocodes Character vector of glottocodes
#' @param varnames Character vector of variable names
#' @param maintainer Name of the person/organization maintaining the data (optional)
#' @param email Email address of maintainer/contact person (optional)
#' @param citation How to cite the data (optional)
#' @param url Optional url linking to a webpage.
#' @param levels Optional character vector with levels across all variables
#' @return a list of metatables
#' @noRd
#'
glottocreate_metadatatables <- function(glottocodes = NULL, varnames = NULL, levels = NULL, maintainer = NULL, email = NULL, citation = NULL, url = NULL){
  structure <- glottocreate_structuretable(varnames = varnames)
  description <- glottocreate_descriptiontable(varnames = varnames, levels = levels)
  references <- glottocreate_reftable(glottocodes = glottocodes, varnames = varnames)
  remarks <- glottocreate_remarkstable(glottocodes = glottocodes, varnames = varnames)
  contributors <- glottocreate_contributorstable(glottocodes = glottocodes, varnames = varnames)
  sample <- glottocreate_sampletable(glottocodes = glottocodes)

  readme <- glottocreate_readmetable(maintainer = maintainer, email = email, citation = citation, url = url)

  lookup <- glottocreate_lookuptable()

  tablelist <- list("structure" = structure,
                    "description" = description,
                    "references" = references,
                    "remarks" = remarks,
                    "contributors" = contributors,
                    "sample" = sample,
                    "readme" = readme,
                    "lookup" = lookup)
  tablelist
}

#' Create metatables for glottosubdata
#'
#' @param glottocodes Character vector of glottocodes
#' @param glottosubcodes Character vector of glottosubcodes
#' @param varnames Character vector of variable names
#' @param maintainer Name of the person/organization maintaining the data (optional)
#' @param email Email address of maintainer/contact person (optional)
#' @param citation How to cite the data (optional)
#' @param url Optional url linking to a webpage.
#' @param levels Optional character vector with levels across all variables
#'
#' @return a list of metatables
#' @noRd
glottocreate_metasubdatatables <- function(glottocodes = NULL, glottosubcodes = NULL, varnames = NULL, levels = NULL, maintainer = NULL, email = NULL, citation = NULL, url = NULL){
  structure <- glottocreate_structuretable(varnames = varnames)
  description <- glottocreate_descriptiontable(varnames = varnames, levels = levels)
  references <- glottocreate_refsubtable(glottosubcodes = glottosubcodes, varnames = varnames)
  remarks <- glottocreate_remarkssubtable(glottosubcodes = glottosubcodes, varnames = varnames)
  contributors <- glottocreate_contributorssubtable(glottosubcodes = glottosubcodes, varnames = varnames)
  sample <- glottocreate_sampletable(glottocodes = glottocodes)

  readme <- glottocreate_readmetable(maintainer = maintainer, email = email, citation = citation, url = url)

  lookup <- glottocreate_lookuptable()

  tablelist <- list("structure" = structure,
                    "description" = description,
                    "references" = references,
                    "remarks" = remarks,
                    "contributors" = contributors,
                    "sample" = sample,
                    "readme" = readme,
                    "lookup" = lookup)
  tablelist
}

#' Create glottotable for glottodata
#'
#' @param glottocodes Character vector of glottocodes
#' @param varnames Character vector of variable names
#'
#' @noRd
glottocreate_glottotable <- function(glottocodes, varnames){
  colnames <- c("glottocode", varnames)
  glottodata <- data.frame(matrix(nrow=length(glottocodes),ncol=length(colnames))) # alternative: tbl <- colnames %>% purrr::map_dfc(setNames, object = list(character()))
  colnames(glottodata) <- colnames
  glottodata[,"glottocode"] <- glottocodes
  glottodata
}

#' Add structure table to glottodata or glottosubdata
#'
#' @param glottodata glottodata or glottosubdata
#'
#' @return glottodata/glottosubdata with a structure table
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata")
#' glottocreate_addstructure(glottodata)
glottocreate_addstructure <- function(glottodata){
  if(!any(glottocheck_isglottodata(glottodata) | glottocheck_isglottosubdata(glottodata)) ){
    stop("Input should be glottodata or glottosubdata")}
  if(glottocheck_hasstructure(glottodata) ){stop("Glottodata already contains a structure table")}

  structuretable <- glottocreate_structuretable(varnames = colnames(glottodata)[-1])
  glottodata <- glottocreate_addtable(glottodata = glottodata, table = structuretable, name = "structure")
  return(glottodata)
}

#' Add sample table to glottodata or glottosubdata
#'
#' @param glottodata glottodata or glottosubdata
#'
#' @return glottodata/glottosubdata with a sample table
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata")
#' glottocreate_addsample(glottodata)
glottocreate_addsample <- function(glottodata){
  if(glottocheck_isglottodata(glottodata)){
    glottotmp <- glottosimplify(glottodata)
    glottocodes <- glottotmp$glottocode
  } else if (glottocheck_isglottosubdata(glottodata)){
    glottotmp <- glottosimplify(glottodata)
    glottocodes <- glottoconvert_subcodes(glottotmp$glottosubcode)
    glottocodes <- unique(glottocodes)
  } else{
    stop("Input should be glottodata or glottosubdata")
  }

  if(glottocheck_hassample(glottodata) ){stop("Glottodata already contains a sample table")}

  sampletable <- glottocreate_sampletable(glottocodes = glottocodes)
  glottodata <- glottocreate_addtable(glottodata = glottodata, table = sampletable, name = "sample")
  return(glottodata)
}

#' Create structure table for glottodata
#'
#' This function creates a new structure table, it can be added to glottodata using glottocreate_addtable()
#'
#' @param varnames Character vector of variable names
#' @keywords internal
#' @return A structure table
#' @export
#' @examples
#' glottocreate_structuretable(varnames = c("var001", "var002", "var003"))
glottocreate_structuretable <- function(varnames = NULL){
structure <- data.frame(matrix(nrow = length(varnames), ncol = 6) )
colnames(structure) <- c("varname", "type", "levels", "weight", "group", "subgroup")
if(!is.null(varnames)){
structure[,"varname"] <- varnames
structure[,"weight"] <- 1
}
structure
}

#' create descriptiontable for glottodata
#'
#' @param varnames Character vector of variable names
#'
#' @noRd
glottocreate_descriptiontable <- function(varnames = NULL, levels = NULL){
  if(is.null(levels)){levels <- c("Y", "N", NA, "A", "B", "C", "D")}
  description <- data.frame(matrix(nrow = length(varnames), ncol = 4 + length(levels)) )
  colnames(description) <- c("varname", "description", "reference", "remarks", paste0("lev_", levels) )
  if(!is.null(varnames)){
  description[,"varname"] <- varnames
  }
  description
}

#' create reference table for glottodata
#'
#' @param glottocodes Character vector of glottocodes
#' @param varnames Character vector of variable names
#'
#' @noRd
glottocreate_reftable <- function(glottocodes = NULL, varnames = NULL){
  if(is.null(glottocodes)){glottocodes <- NA}
  references <- data.frame(matrix(nrow = length(glottocodes), ncol = (length(varnames)*2)+3 ) )
  if(!is.null(varnames)){
  colnames(references) <- c("glottocode", "reference", "page", paste(rep(varnames, each = 2) , c("ref", "page"), sep = "_") )
  } else {
  colnames(references) <- c("glottocode", "reference", "page"  )
  }
  references[,"glottocode"] <- glottocodes
  references
}

#' create reference table for glottosubdata
#'
#' @param glottosubcodes Character vector of glottocodes
#' @param varnames Character vector of variable names
#'
#' @noRd
glottocreate_refsubtable <- function(glottosubcodes = NULL, varnames = NULL){
  if(is.null(glottosubcodes)){glottosubcodes <- NA}
  references <- data.frame(matrix(nrow = length(glottosubcodes), ncol = (length(varnames)*2)+3 ) )
  if(!is.null(varnames)){
    colnames(references) <- c("glottosubcode", "reference", "page", paste(rep(varnames, each = 2) , c("ref", "page"), sep = "_") )
  } else {
    colnames(references) <- c("glottosubcode", "reference", "page"  )
  }
  references[,"glottosubcode"] <- glottosubcodes
  references
}

#' create remarks table for glottodata
#'
#' @param glottocodes Character vector of glottocodes
#' @param varnames Character vector of variable names
#'
#' @noRd
glottocreate_remarkstable <- function(glottocodes = NULL, varnames = NULL){
  if(is.null(glottocodes)){glottocodes <- NA}
  remarks <- data.frame(matrix(nrow = length(glottocodes), ncol = length(varnames)+2  ) )
  if(!is.null(varnames)){
    colnames(remarks) <- c("glottocode", "remark", paste(varnames, c("remark"), sep = "_") )
  } else {
    colnames(remarks) <- c("glottocode", "remark")
  }

  remarks[,"glottocode"] <- glottocodes
  remarks
}

#' create remarks table for glottosubdata
#'
#' @param glottosubcodes Character vector of glottocodes
#' @param varnames Character vector of variable names
#'
#' @noRd
glottocreate_remarkssubtable <- function(glottosubcodes = NULL, varnames = NULL){
  if(is.null(glottosubcodes)){glottosubcodes <- NA}
  remarks <- data.frame(matrix(nrow = length(glottosubcodes), ncol = length(varnames)+2  ) )
  if(!is.null(varnames)){
    colnames(remarks) <- c("glottosubcode", "remark", paste(varnames, c("remark"), sep = "_") )
  } else {
    colnames(remarks) <- c("glottosubcode", "remark")
  }

  remarks[,"glottosubcode"] <- glottosubcodes
  remarks
}

#' create contributors table for glottodata
#'
#' @param glottocodes Character vector of glottocodes
#' @param varnames Character vector of variable names
#'
#' @noRd
glottocreate_contributorstable <- function(glottocodes = NULL, varnames = NULL){
  if(is.null(glottocodes)){glottocodes <- NA}
  contributors <- data.frame(matrix(nrow = length(glottocodes), ncol = length(varnames)+2  ) )

  if(!is.null(varnames)){
    colnames(contributors) <- c("glottocode", "contributor", paste(varnames, c("contributor"), sep = "_") )
  } else {
    colnames(contributors) <- c("glottocode", "contributor")
  }
  contributors[,"glottocode"] <- glottocodes
  contributors
}

#' create contributors table for glottodata
#'
#' @param glottosubcodes Character vector of glottocodes
#' @param varnames Character vector of variable names
#'
#' @noRd
glottocreate_contributorssubtable <- function(glottosubcodes = NULL, varnames = NULL){
  if(is.null(glottosubcodes)){glottosubcodes <- NA}
  contributors <- data.frame(matrix(nrow = length(glottosubcodes), ncol = length(varnames)+2  ) )

  if(!is.null(varnames)){
    colnames(contributors) <- c("glottosubcode", "contributor", paste(varnames, c("contributor"), sep = "_") )
  } else {
    colnames(contributors) <- c("glottosubcode", "contributor")
  }
  contributors[,"glottosubcode"] <- glottosubcodes
  contributors
}

#' create sample table for glottodata
#'
#' @param glottocodes Character vector of glottocodes
#' @keywords internal
#' @return A sample table
#' @export
#' @examples
#' glottocreate_sampletable(glottocodes = c("yucu1253", "tani1257"))
glottocreate_sampletable <- function(glottocodes = NULL){
  if(is.null(glottocodes)){glottocodes <- NA}
  sample <- data.frame(matrix(nrow = length(glottocodes), ncol = 3  ) )
  colnames(sample) <- c("glottocode", "group", "subgroup")
  sample[,"glottocode"] <- glottocodes
  sample
}

#' Create readme table for glottodata
#'
#' @param maintainer maintainer of the data
#' @param email email address of maintainer
#' @param citation how to cite the data
#' @param url url with background information on the data
#'
#' @noRd
glottocreate_readmetable <- function(maintainer = NULL, email = NULL, citation = NULL, url = NULL ){
  readme <- data.frame(matrix(nrow = 5, ncol = 2) )
  readme[,1] <- c("maintainer", "email", "citation", "url", "This database was created using the glottospace R package")
  readme[,2] <- c(ifelse(is.null(maintainer), NA, maintainer),
                  ifelse(purrr::is_empty(email), NA, email),
                  ifelse(is.null(citation), NA, citation),
                  ifelse(is.null(url), NA, url),
                  paste0("version " , utils::packageVersion("glottospace") ) )
  colnames(readme) <- c("info", "value")
  readme
}

#' Create lookup table for glottodata
#'
#' @noRd
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
#'
#' @noRd
#' @examples
#' glottocreate_glottosubcodes(glottocode = "yucu1253",
#' groups = c("a", "b"), n = 5)
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

#' Create demodata
#'
#'
#' @noRd
glottocreate_demodata <- function(meta = TRUE){
  demo <- glottocreate_data(glottocodes = c("yucu1253", "tani1257", "ticu1245", "orej1242", "nade1244", "mara1409"), variables = 3, meta = meta)
  if(meta == TRUE){
    demodata <- demo$glottodata
    demo$structure[,"type"] <- c("symm", "factor", "symm")
    demo$sample[,"group"] <- c("A","A", "A", "B", "B", "B")
    demo$sample[,"subgroup"] <- c("A","A", "B", "B", "C", "D")
  } else{
    demodata <- demo
  }

  demodata[,"var001"] <- c("Y", NA, "Y", "N", "N", "N")
  demodata[,"var002"] <- c("a", "b", "a", "b", "c", "a")
  demodata[,"var003"] <- c("N", "Y", "Y", "N", "Y", "N")

  if(meta == TRUE){
    demo$glottodata <- demodata
  } else{
    demo <- demodata
  }

  demo
}

#' Create demosubdata
#'
#'
#' @noRd
glottocreate_demosubdata <- function(meta = TRUE){
  demo <- glottocreate_subdata(glottocodes = c("yucu1253", "tani1257"), variables = 3, groups = c("a", "b"), n = 5, meta = meta)

  if(meta == TRUE){
    demo$structure[,"type"] <- c("symm", "factor", "symm")
    demo$sample[,"group"] <- c("A", "B")
  }

  demo[[1]][,"var001"] <- rep(c("Y", "N"), 5)
  demo[[1]][,"var002"] <- rep(c("a", "b"), 5)
  demo[[1]][,"var003"] <- rep(c("Y", "N", "N", "Y", NA), 2)

  demo[[2]][,"var001"] <- rep(c("Y", NA, "Y", "Y", "N"), 2)
  demo[[2]][,"var002"] <- rep(c("a", "a", "a", "b", "b"), 2)
  demo[[2]][,"var003"] <- rep(c("N", "Y"), 5)

  demo
}

#' Create demosubdata
#'
#'
#' @noRd
glottocreate_cnstn_toy <- function(){
  glottosubdata_cnstn_toy <- glottocreate(glottocodes = c("tani1257", "yucu1253"), variables = 7, meta = T, groups = "")

  glottosubdata_cnstn_toy$tani1257[1:3, ] <- NA
  glottosubdata_cnstn_toy$tani1257$glottosubcode <- c("tani1257_0001", "tani1257_0002", "tani1257_0003")

  glottosubdata_cnstn_toy$tani1257[1, 2:8] <- c(0, 1, 0, 0, 1, 1, 1)
  glottosubdata_cnstn_toy$tani1257[2, 2:8] <- c(0, 0, 1, 0, 0, 0, 1)
  glottosubdata_cnstn_toy$tani1257[3, 2:8] <- c(0, 0, 1, 0, 1, 1, 0)

  glottosubdata_cnstn_toy$yucu1253[1:2, ] <- NA
  glottosubdata_cnstn_toy$yucu1253$glottosubcode <- c("yucu1253_0001", "yucu1253_0002")

  glottosubdata_cnstn_toy$yucu1253[1, 2:8] <- c(0, 1, 0, 0, 1, 1, 1)
  glottosubdata_cnstn_toy$yucu1253[2, 2:8] <- c(0, 0, 1, 0, 1, 0, 1)

  glottosubdata_cnstn_toy$structure$type <- "symm"
  return(glottosubdata_cnstn_toy)
}

#' Add a table to glottodata
#'
#' @param glottodata A glottodata table, or a list of glottodata tables
#' @param table A table to be added
#' @param name A name for the table
#' @keywords internal
#' @return a glottodata object with structure table added to it.
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = FALSE)
#' structuretable <- glottocreate_structuretable(varnames = colnames(glottodata)[-1])
#' glottodata <- glottocreate_addtable(glottodata, table = structuretable, name = "structure")
glottocreate_addtable <- function(glottodata, table, name){
  if(is_list(table) & length(table) != 1){stop("Please provide either a data.frame or a list of 1 data.frame")}

  if(!is_list(table)){
    table <- list(table)
    }
  names(table) <- name

  if(!is_list(glottodata) & glottocheck_isglottodata(glottodata)){
    glottodata <- list("glottodata" = glottodata)
  }

  if(!is_list(glottodata) & glottocheck_isglottosubdata(glottodata)){
    glottodata <- list("glottosubdata" = glottodata)
  }

  c(glottodata, table)

}



#' Create empty distance matrix
#'
#' @param names
#'
#' @noRd
#'
glottocreate_emptydistmat <- function(names){
  outputmat <- matrix(data = NA,
                      nrow = length(names),
                      ncol = length(names),
                      dimnames = list(names, names))
  return(outputmat)
}
