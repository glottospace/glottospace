
#' Convert a linguistic dataset into glottodata or glottosubdata
#'
#' This function is mainly intended for 'messy' datasets that are not in glottodata/glottosubdata structure.
#'
#' @param data A dataset that should be converted into glottodata/glottosubdata. This will generally be an excel file loaded with glottoget().
#'
#' The dataset will be converted into glottodata if:
#' \itemize{
#'  \item all data are stored in a single table, or
#'  \item the dataset contains several tables of which one is called 'glottodata', or
#'  \item a table argument is provided.
#'  }
#'
#' Otherwise, glottospace will attempt to convert the dataset into glottosubdata. This works if:
#' \itemize{
#'  \item table names are glottocodes, and
#'  \item an argument is provided to glottocodes, or the dataset contains a sample table from which glottocodes can be obtained.
#' }
#' @param glottocodes Optional character vector of glottocodes. If no glottocodes are supplied, glottospace will search for them in the sample table.
#' @param table In case dataset consists of multiple tables, indicate which table contains the data that should be converted.
#' @param var Character string that distinguishes those columns which contain variable names.
#' @param ref Character string that distinguishes those columns which contain references.
#' @param page Character string that distinguishes those columns which contain page numbers.
#' @param remark Character string that distinguishes those columns which contain remarks.
#' @param contributor Character string that distinguishes those columns which contain contributors.
#' @param glottocolumn column name or column id with glottocodes (optional, provide if glottocodes are not stored in a column called 'glottocode')
#' @param glottosubcolumn Column name or column id with glottosubcodes (optional, provide if glottosubcodes are not stored in a column called 'glottosubcode')
#' @param varnamecol In case the dataset contains a structure table, but the varnamecol is not called 'varname', its name should be specified.
#' @examples
#' # Create a messy dataset:
#' glottodata <- glottoget("demodata")
#' glottodata <- cbind(glottodata, data.frame("redundant" = c(1:6)))
#'
#' # In this messy dataset there's no way to determine which columns contain the relevant variables...
#' # Therefore we manually add a character string to distinguish the relevant columns:
#' colnames(glottodata)[2:3] <- paste0("var_", colnames(glottodata)[2:3] )
#'
#' glottoconverted <- glottoconvert(glottodata, var = "var_")
#' @export
#' @return A glottodata or glottosubdata object (either a list or data.frame)
glottoconvert <- function(data, var = NULL, glottocodes = NULL, table = NULL, glottocolumn = NULL, glottosubcolumn = NULL, ref = NULL, page = NULL, remark = NULL, contributor = NULL, varnamecol = NULL){

  if(is.null(var)){
    var <- ""
    message("The 'var' argument is empty. Assuming that all columns except glottocolumn are variables. If this is not the case, please indicate how variable columns are distinguished from other columns.")
  }

  if(!is_list(data) & is.null(glottosubcolumn)){
    glottodata <- glottoconvert_table(table = data, glottocolumn = glottocolumn, var = var, ref = ref, page = page, remark = remark, contributor = contributor)
  } else if(is_list(data) & length(data) == 1){
    glottodata <- glottoconvert_table(table = data[[1]], glottocolumn = glottocolumn, var = var, ref = ref, page = page, remark = remark, contributor = contributor)
  } else if(is_list(data) & "glottodata" %in% names(data)){
    glottodata <- glottoconvert_data(data = data, table = "glottodata", glottocodes = glottocodes, glottocolumn = glottocolumn, var = var, ref = ref, page = page, remark = remark, contributor = contributor)
  } else if(is_list(data) & !is.null(table) ){
    glottodata <- glottoconvert_data(data = data, table = table, glottocolumn = glottocolumn, glottocodes = glottocodes, var = var, ref = ref, page = page, remark = remark, contributor = contributor)
  } else if(!is_list(data) & !is.null(glottosubcolumn)){
    glottodata <- glottoconvert_subtable(table = data, glottosubcolumn = glottosubcolumn, var = var)
  } else {
    glottodata <- glottoconvert_subdata(data = data, var = var, glottocodes = glottocodes, glottosubcolumn = glottosubcolumn)
  }

  message("I don't condemn, I DO convert, but Love is My Religion: https://youtu.be/r-eXYJnV3V4 \n \n your data has been converted into glottodata! \n (don't forget to assign it to a new object)")
  glottodata

}

#' Convert a dataset into glottodata
#'
#' @param table In case dataset consists of multiple tables, indicate which table contains the data that should be converted.
#' @param var Character string that distinguishes those columns which contain variable names.
#' @param ref Character string that distinguishes those columns which contain references.
#' @param page Character string that distinguishes those columns which contain page numbers.
#' @param remark Character string that distinguishes those columns which contain remarks.
#' @param contributor Character string that distinguishes those columns which contain contributors.
#' @param glottocolumn column name or column id with glottocodes (optional, provide if glottocodes are not stored in a column called 'glottocode')
#' @param data Dataset to be converted
#' @param varnamecol In case the dataset contains a structure table, but the varnamecol is not called 'varname', its name should be specified.
#' @param glottocodes Optional character vector of glottocodes. If no glottocodes are supplied, glottospace will search for them in the sample table. If no sample data is provided, all glottocodes from the glottodata table will be used.
#'
#' @return A glottodata object
#' @noRd
glottoconvert_data <- function(data, var, table = NULL, glottocolumn = NULL, glottocodes = NULL, ref = NULL, page = NULL, remark = NULL, contributor = NULL, varnamecol = NULL){

  if(is.null(glottocolumn)){glottocolumn <- "glottocode"}
  if("sample" %in% names(data) ){
    sample <- unique(data$sample[["glottocode"]])
  } else {
    sample <- NULL
  }
  if(!is.null(glottocodes) & !purrr::is_empty(sample) ){
    glottosample <- glottocodes
    message("Using the glottocodes you specified to transform the data. Please note that the data you provided also contains a 'sample' table with glottocodes. If you would rather want to use those, you should leave the glottocodes argument empty.")
  } else if(!is.null(glottocodes) & purrr::is_empty(sample)){
    glottosample <- glottocodes
    message("Using the glottocodes you specified to transform the data.")
  } else if(is.null(glottocodes) & !purrr::is_empty(sample) ){
    glottosample <- sample
    message("Using the glottocodes in the sample table to transform the data.")
  } else {
    glottosample <- NULL
  }

  if(is.null(table)){table <- "glottodata"}
  if(!is.null(glottosample)){
  glottosample <- data[[table]][, glottocolumn, drop = TRUE] %in% glottosample
  data[[table]] <- data[[table]][glottosample, ]
  }

  glottodata <- glottoconvert_table(table = data[[table]], glottocolumn = glottocolumn, var = var, ref = ref, page = page, remark = remark, contributor = contributor)

  glottometanames <- names(data) %in% names(glottocreate_metatables())
  if(sum(glottometanames) == 0){
    message("Unable to find meta tables in your dataset.
            In case you would like to add meta tables, use glottocreate_metatables() and add them to glottosubdata using glottojoin(). ")
  } else {
    glottometatables <- data[glottometanames]
    if("structure" %in% names(glottometatables)){
      if("varname" %nin% colnames(glottometatables[["structure"]]) & is.null(varnamecol)){
        stop("The structure table does not contain a column named 'varname'. Either add it to your dataset, or specify varnamecol argument to indicate which column in the structure table contains varnames.")
      } else if("varname" %nin% colnames(glottometatables[["structure"]]) & !is.null(varnamecol)){
        colnames(glottometatables[["structure"]])[colnames(glottometatables[["structure"]]) == varnamecol] <- "varname"
      }
      oldvarids <- grep(pattern = var, x = glottometatables[["structure"]][, "varname", drop = TRUE], ignore.case = TRUE, value = FALSE)
      newvarnames <- gsub(pattern = var, x = glottometatables[["structure"]][oldvarids, "varname", drop = TRUE], replacement = "")
      glottometatables[["structure"]][oldvarids, "varname", drop = TRUE] <- newvarnames
      glottodata[["structure"]] <- glottometatables[["structure"]]
      glottometatables[["structure"]] <- NULL
    }
    if("sample" %in% names(glottometatables)){
      glottodata[["sample"]] <- glottometatables[["sample"]]
      glottometatables[["sample"]] <- NULL
    }
    glottodata <- c(glottodata, glottometatables)
  }

  ignored <- names(data)[names(data) %nin% c(table, glottometanames)]
  if(length(ignored) != 0){
    message(paste("The following tables were ignored: ", paste(ignored, collapse = ", ") ) )
  }
  glottodata
}

#' Convert a linguistic dataset into glottosubdata
#'
#' @param data Any dataset that should be converted into glottosubdata. This will generally be an excel file loaded with glottoget() of which the sheetnames are glottocodes.
#' @param glottocodes Optional character vector of glottocodes. If no glottocodes are supplied, glottospace will search for them in the sample table.
#' @param var Character string that distinguishes those columns which contain variable names.
#' @param varnamecol In case the dataset contains a structure table, but the varnamecol is not called 'varname', its name should be specified.
#'
#' @noRd
glottoconvert_subdata <- function(data, var, glottocodes = NULL, varnamecol = NULL, glottosubcolumn = NULL){

  if("sample" %in% names(data) ){
    sample <- unique(data$sample[["glottocode"]])
  } else {
    sample <- NULL
  }
  if(!is.null(glottocodes) & !purrr::is_empty(sample) ){
    glottosample <- glottocodes
    message("Using the glottocodes you specified to transform the data. Please note that the data you provided also contains a 'sample' table with glottocodes. If you would rather want to use those, you should leave the glottocodes argument empty.")
  } else if(!is.null(glottocodes) & purrr::is_empty(sample)){
    glottosample <- glottocodes
    message("Using the glottocodes you specified to transform the data.")
  } else if(is.null(glottocodes) & !purrr::is_empty(sample) ){
    glottosample <- sample
    message("Using the glottocodes in the sample table to transform the data.")
  } else {
    stop("Please provide glottocodes, or add a sample table to your dataset with glottocodes.")
  }

  glottodatanames <- names(data) %in% glottosample
  if(sum(glottodatanames) == 0){
    stop("Unable to find tables in your dataset that match the specified glottocodes.
         Please check whether your table names are actually glottocodes (and only glottocodes),
         and not something like 'abcd1234_something'. ")
  } else {
  glottodatatables <- lapply(X = data[glottodatanames], FUN = glottoconvert_subtable, glottosubcolumn = glottosubcolumn, var = var)
  }

  glottometanames <- names(data) %in% names(glottocreate_metatables())
  if(sum(glottometanames) == 0){
    message("Unable to find meta tables in your dataset.
            In case you would like to add meta tables, use glottocreate_metatables() and add them to glottosubdata using glottojoin(). ")
    glottosubdata <- glottodatatables
  } else {
    glottometatables <- data[glottometanames]
    if("structure" %in% names(glottometatables)){
      if("varname" %nin% colnames(glottometatables[["structure"]]) & is.null(varnamecol)){
        stop("The structure table does not contain a column named 'varname'. Either add it to your dataset, or specify varnamecol argument to indicate which column in the structure table contains varnames.")
      } else if("varname" %nin% colnames(glottometatables[["structure"]]) & !is.null(varnamecol)){
        colnames(glottometatables[["structure"]])[colnames(glottometatables[["structure"]]) == varnamecol] <- "varname"
      }
      oldvarids <- grep(pattern = var, x = glottometatables[["structure"]][, "varname", drop = TRUE], ignore.case = TRUE, value = FALSE)
      newvarnames <- gsub(pattern = var, x = glottometatables[["structure"]][oldvarids, "varname", drop = TRUE], replacement = "")
      glottometatables[["structure"]][oldvarids, "varname", drop = TRUE] <- newvarnames
    }
    # if("sample" %in% names(glottometatables)){
    #   glottodata[["sample"]] <- glottometatables[["sample"]]
    #   glottometatables[["sample"]] <- NULL
    # }
    glottosubdata <- c(glottodatatables, glottometatables)
  }

  ignored <- names(data)[!(glottodatanames | glottometanames)]
  if(length(ignored) != 0){
  message(paste("The following tables were ignored: ", paste(ignored, collapse = ", ") ) )
  }

  glottosubdata
}

#' Transform a table into a glottodatatable and move info to metatables
#'
#' @param glottocolumn column name or column id with glottocodes
#' @param var Character string that distinguishes those columns which contain variable names.
#' @param ref Character string that distinguishes those columns which contain references.
#' @param table In case dataset consists of multiple tables, indicate which table contains the data that should be converted.
#' @param page Character string that distinguishes those columns which contain page numbers.
#' @param remark Character string that distinguishes those columns which contain remarks.
#' @param contributor Character string that distinguishes those columns which contain contributors.
#'
#' @noRd
#' @examples
#' \dontrun{
#' data <- glottoget("userdata.xlsx")
#' glottodata <- glottoconvert_table(data = data, var = "var_", ref = "ref_",
#'               page = "pag_", remark = "com_", contributor = "Coder")
#' glottocheck(glottodata)
#' glottosave(glottodata)
#' }
glottoconvert_table <- function(table, glottocolumn = NULL, var, ref = NULL, page = NULL, remark = NULL, contributor = NULL){

  totcol <- length(colnames(table))

# Create new glottodata structure based on glottocodes and variable names
    if(is.null(glottocolumn)){glottocolumn <- "glottocode"}
    glottocol <- grep(pattern = glottocolumn, x = colnames(table), ignore.case = TRUE, value = TRUE)
    if(length(glottocol) > 1){stop(paste0("Column ", glottocolumn, " is duplicated. Please rename one of the columns."))}
    if(length(glottocol) == 0){stop(paste0("Column ", glottocolumn, " not found. Please add a column with glottocodes."))}
    glottocodes <- table[[glottocol]]

    oldvarnames <- grep(pattern = var, x = colnames(table), ignore.case = TRUE, value = TRUE)
    oldvarnames <- oldvarnames[oldvarnames %nin% glottocol] # added to accomodate var = ""
    if(length(oldvarnames) == 0){stop(paste0("No columns found with ", var, " in the name."))}
    newvarnames <- gsub(pattern = var, x = oldvarnames, replacement = "")
    glottodata <- glottocreate(glottocodes = glottocodes, variables = newvarnames)

    # Drop glottocol from old table
    table <- table[,colnames(table) %nin% glottocol]  # added to accomodate var = ""

# Add data for variables:
  sdata <- glottoconvert_colname(data = table, oldfix = var,
                                 newfix = "", newname = "")
  varcols <- colnames(sdata)
  nvar <- ncol(sdata)
  glottodata[["glottodata"]][,-1] <- sdata

# Add references
  if(!is.null(ref)){
    sdata <- glottoconvert_colname(data = table, oldfix = ref,
                                   newfix = "_ref", newname = "reference")
    nref <- ncol(sdata)
    glottodata[["references"]][,colnames(sdata)] <- sdata
  } else {
    nref <- 0
  }

# Add page numbers
  if(!is.null(page)){
    sdata <- glottoconvert_colname(data = table, oldfix = page,
                                   newfix = "_page", newname = "page")
    pagecols <- colnames(sdata)
    npage <- ncol(sdata)
    glottodata[["references"]][,colnames(sdata)] <- sdata
  } else {
    npage <- 0
  }

# Add remarks
  if(!is.null(remark)){
    sdata <- glottoconvert_colname(data = table, oldfix = remark,
                                   newfix = "_remark", newname = "remark")
    remarkcols <- colnames(sdata)
    nremark <- ncol(sdata)
    glottodata[["remarks"]][,colnames(sdata)] <- sdata
  } else {
    nremark <- 0
  }

# Add contributors
  if(!is.null(contributor)){
    sdata <- glottoconvert_colname(data = table, oldfix = contributor,
                                   newfix = "_con", newname = "contributor")
    contrcols <- colnames(sdata)
    ncontr <- ncol(sdata)
    glottodata[["contributors"]][,colnames(sdata)] <- sdata
  } else {
    ncontr <- 0
  }

  message("Overview of the conversion process: ")
  overview <- tibble::tribble(
    ~column, ~count,
    "total", totcol,
    "variables", nvar,
    "references", nref,
    "page numbers", npage,
    "remarks", nremark,
    "contributors", ncontr,
    "glottocol", 1,
    "omitted", totcol-nvar-nref-npage-nremark-ncontr-1 # minus 1 for glottocode
  )

  printmessage(overview)

  omitted <- colnames(table)[!grepl(pattern = paste(c(glottocolumn, var, ref, page, remark, contributor), collapse = "|"), x = colnames(table), ignore.case = TRUE)]
  message(paste0("\n The following columns were omitted: \n", paste(omitted, collapse = ", "), "\n" ))

invisible(glottodata)
}


#' Transform a table into a glottosubdata table
#'
#' @param var Character string that distinguishes those columns which contain variable names.
#' @param glottosubcolumn Column name with glottosubcodes
#' @param table In case dataset consists of multiple tables, indicate which table contains the data that should be converted.
#'
#' @noRd
#'
glottoconvert_subtable <- function(table, glottosubcolumn = NULL, var){

  totcol <- length(colnames(table))

  # Create new glottotable structure based on glottocodes and variable names
  if(is.null(glottosubcolumn)){glottosubcolumn <- "glottosubcode"}
  glottosubcol <- grep(pattern = glottosubcolumn, x = colnames(table), ignore.case = TRUE, value = TRUE)
  if(length(glottosubcol) > 1){stop(paste0("Column ", glottosubcolumn, " is duplicated. Please rename one of the columns."))}
  if(length(glottosubcol) == 0){stop(paste0("Column ", glottosubcolumn, " not found. Please make sure each (!) table has a column with glottosubcodes."))}
  glottosubcodes <- table[[glottosubcol]]

  oldvarnames <- grep(pattern = var, x = colnames(table), ignore.case = TRUE, value = TRUE)
  oldvarnames <- oldvarnames[oldvarnames %nin% glottosubcol] # added to accomodate var = ""
  if(length(oldvarnames) == 0){stop(paste0("No columns found with ", var, " in the name."))}
  newvarnames <- gsub(pattern = var, x = oldvarnames, replacement = "")
  glottotable <- glottocreate(glottocodes = glottosubcodes, variables = newvarnames, meta = FALSE)
  colnames(glottotable)[1] <- "glottosubcode"

  # Drop glottocol from old table
  table <- table[,colnames(table) %nin% glottosubcol]  # added to accomodate var = ""

  # Add data for variables:
  sdata <- glottoconvert_colname(data = table, oldfix = var,
                                 newfix = "", newname = "")
  varcols <- colnames(sdata)
  nvar <- ncol(sdata)
  glottotable[,-1] <- sdata

  invisible(glottotable)
}

#' Convert colnames and subset
#'
#' @param data A data.frame
#' @param oldfix Either a prefix or suffix such as "_fix", or "fix_"
#' @param newfix New pre- or suffix.
#' @noRd
#'
#'
glottoconvert_colname <- function(data, oldfix, newfix = NULL, newname = NULL){
  cols <- grepl(pattern = oldfix, x = colnames(data), ignore.case = TRUE)
  data <- data[cols]
  if(ncol(data) != 1){
    newnames <- paste0(gsub(pattern = oldfix, x = colnames(data), replacement = ""), newfix)
    colnames(data) <- newnames
  } else {
    colnames(data) <- newname
  }
  data
}

#' Get glottocodes from glottosubcodes
#'
#' @param glottosubcodes Character vector of glottosubcodes
#' @param check Logical, should glottosubcodes be checked first?
#'
#' @return A character vector of glottocodes with the same length as glottosubcodes.
#' @noRd
#'
glottoconvert_subcodes <- function(glottosubcodes, check = FALSE){
  if(check == TRUE){glottosubcode_valid(glottosubcodes)}
  glottosubcodes_splitted <- strsplit(glottosubcodes, split = "_")
  sapply(glottosubcodes_splitted, `[[`, 1)
}

#' Conditionally transforms dist object to distance matrix
#'
#' If dist object is not a distance matrix it will be converted.
#'
#' @param dist dist object
#'
#' @return distance matrix
#' @noRd
#'
contransform_distmat <- function(dist){
  if(inherits(dist, what = "dist")){
    distmat <- as.matrix(dist)
  } else if (inherits(dist, what = "matrix")){
    distmat <- dist
  }
  distmat
}
