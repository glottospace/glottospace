
#' glottoconvert
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
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' data <- glottoget("userdata.xlsx")
#' glottodata <- glottoconvert(data = data, var = "var_", ref = "ref_", page = "pag_", remark = "com_", contributor = "Coder")
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
    "omitted", totcol-nvars-nrefs-npage-nremark-ncont-1 # minus 1 for glottocode
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
#' @return
#' @export
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

