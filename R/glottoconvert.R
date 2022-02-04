
#' Convert a linguistic dataset into glottosubdata
#'
#' @param subdata Any dataset that should be converted into glottosubdata. This will generally an excel file loaded with glottoget() of which the sheetnames are glottocodes.
#' @param glottocodes Optional character vector of glottocodes. If no glottocodes are supplied, glottospace will search for them in the sample table.
#'
#' @export
#'
glottoconvert_subdata <- function(data, glottocodes = NULL){
  if("sample" %in% names(data) ){
    sample <- data$sample[["glottocode"]]
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
  glottodatatables <- data[glottodatanames]
  }

  glottometanames <- names(data) %in% names(glottocreate_metatables())
  if(sum(glottometanames) == 0){
    message("Unable to find meta tables in your dataset.
            In case you would like to add meta tables, use glottocreate_metatables() and add them to glottosubdata using glottojoin(). ")
    glottosubdata <- glottodatatables
  } else {
    glottometatables <- data[glottometanames]
    glottosubdata <- c(glottodatatables, glottometatables)
  }

  ignored <- paste(names(data)[!(glottodatanames | glottometanames)], collapse = ", ")
  message(paste("The following tables were ignored: ", ignored ) )

  glottosubdata
}

#' Transform a linguistic dataset into glottodata
#'
#' @param data Dataset to be transformed to glottodata
#' @param glottocode column name or column id with glottocodes
#' @param var Character string that distinguishes those columns which contain variable names.
#' @param ref Character string that distinguishes those columns which contain references.
#' @param table In case dataset consists of multiple tables, indicate which table contains the data that should be converted.
#' @param page Character string that distinguishes those columns which contain page numbers.
#' @param remark Character string that distinguishes those columns which contain remarks.
#' @param contributor Character string that distinguishes those columns which contain contributors.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- glottoget("userdata.xlsx")
#' glottodata <- glottoconvert(data = data, var = "var_", ref = "ref_",
#'               page = "pag_", remark = "com_", contributor = "Coder")
#' glottocheck(glottodata)
#' glottosave(glottodata)
#' }
glottoconvert <- function(data, glottocode = NULL, var, table = NULL, ref = NULL, page = NULL, remark = NULL, contributor = NULL){
  if(is_list(data) & is.null(table)){
    message(paste0("Dataset contains multiple tables, assuming data to be converted is in the first table: ", names(data)[[1]]), "\n" )
    data <- data[[1]]
  } else if(is_list(data) & !is.null(table) ){
    data <- data[[table]]
  }

  totcol <- length(colnames(data))

# Create new glottodata structure based on glottocodes and variable names
    if(is.null(glottocode)){glottocode <- "glottocode"}
    glottocol <- grep(pattern = glottocode, x = colnames(data), ignore.case = TRUE, value = TRUE)
    if(length(glottocol) > 1){stop(paste0("Column ", glottocode, " is duplicated. Please rename one of the columns."))}
    if(length(glottocol) == 0){stop(paste0("Column ", glottocode, " not found. Please add a column with glottocodes."))}
    glottocodes <- data[[glottocol]]

    oldvarnames <- grep(pattern = var, x = colnames(data), ignore.case = TRUE, value = TRUE)
    if(length(oldvarnames) == 0){stop(paste0("No columns found with ", var, " in the name."))}
    newvarnames <- gsub(pattern = var, x = oldvarnames, replacement = "")
    glottodata <- glottocreate(glottocodes = glottocodes, variables = newvarnames)

# Add data for variables:
  sdata <- glottoconvert_colname(data = data, oldfix = var,
                                 newfix = "", newname = "")
  varcols <- colnames(sdata)
  nvar <- ncol(sdata)
  glottodata[["glottodata"]][,-1] <- sdata

# Add references
  if(!is.null(ref)){
    sdata <- glottoconvert_colname(data = data, oldfix = ref,
                                   newfix = "_ref", newname = "reference")
    nref <- ncol(sdata)
    glottodata[["references"]][,colnames(sdata)] <- sdata
  }

# Add page numbers
  if(!is.null(page)){
    sdata <- glottoconvert_colname(data = data, oldfix = page,
                                   newfix = "_page", newname = "page")
    pagecols <- colnames(sdata)
    npage <- ncol(sdata)
    glottodata[["references"]][,colnames(sdata)] <- sdata
  }

# Add remarks
  if(!is.null(remark)){
    sdata <- glottoconvert_colname(data = data, oldfix = remark,
                                   newfix = "_remark", newname = "remark")
    remarkcols <- colnames(sdata)
    nremark <- ncol(sdata)
    glottodata[["remarks"]][,colnames(sdata)] <- sdata
  }

# Add contributors
  if(!is.null(contributor)){
    sdata <- glottoconvert_colname(data = data, oldfix = contributor,
                                   newfix = "_con", newname = "contributor")
    contrcols <- colnames(sdata)
    ncontr <- ncol(sdata)
    glottodata[["contributors"]][,colnames(sdata)] <- sdata
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
    "omitted", totcol-nvar-nref-npage-nremark-ncontr-1 # minus 1 for glottocode
  )

  print(overview)

  omitted <- colnames(data)[!grepl(pattern = paste(c(glottocode, var, ref, page, remark, contributor), collapse = "|"), x = colnames(data), ignore.case = TRUE)]
  message(paste0("\n The following columns were omitted: \n", paste(omitted, collapse = ", "), "\n" ))

  message("I don't condemn, I DO convert, but Love is My Religion: https://youtu.be/r-eXYJnV3V4 \n \n your data has been converted into glottodata! \n (don't forget to assign it to a new object)")

invisible(glottodata)
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

