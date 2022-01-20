
#' glottoconvert
#'
#' @param data Dataset to be transformed to glottodata
#' @param glottocode column name or column id with glottocodes
#' @param var Character string that distinguishes those columns which contain variable names.
#' @param ref Character string that distinguishes those columns which contain references.
#' @param table In case dataset consists of multiple tables, indicate which table contains the data that should be converted.
#' @param page Character string that distinguishes those columns which contain page numbers.
#' @param position Optional position of distinguishing character string, either "start" or "end"
#' @param com Character string that distinguishes those columns which contain comments.
#'
#' @return
#' @export
#'
#' @examples
#' # Load data from path:
#' data <- glottoget("C:/Users/sjnor/surfdrive/Projecten en schrijfsels/Papers in progress/Yucuna-Tanimuca/data/testdata linguistics/2022_01_16_basic_data.xlsx")
glottoconvert <- function(data, glottocode = NULL, var, table = NULL, ref = NULL, page = NULL, com = NULL, position = NULL){
  if(!is_list(data) ){
    udata <- data
  } else if(is_list(data) & is.null(table)){
    udata <- data[[1]]
    message(paste0("Dataset contains multiple tables, assuming data to be converted is in the first table: ", names(data)[[1]]) )
  } else if(is_list(data) & !is.null(table) ){
    udata <- data[[table]]
  }

  ncol <- length(colnames(udata))

# Create new glottodata structure based on glottocodes and variable names
    if(is.null(glottocode)){glottocode <- "glottocode"}
    glottocol <- grep(pattern = glottocode, x = colnames(udata), ignore.case = TRUE, value = TRUE)
    if(length(glottocol) > 1){stop(paste0("Column ", glottocode, " is duplicated. Please rename one of the columns."))}
    if(length(glottocol) == 0){stop(paste0("Column ", glottocode, " not found. Please add a column with glottocodes."))}
    glottocodes <- udata[[glottocol]]

    varcols <- grepl(pattern = contrans_stringpos(string = var, pos = position), x = colnames(udata), ignore.case = TRUE)
    oldvarnames <- colnames(udata)[varcols]
    newvarnames <- gsub(pattern = var, x = oldvarnames, replacement = "")
    colnames(udata)[colnames(udata) %in% oldvarnames] <- newvarnames

  glottodata <- glottocreate(glottocodes = glottocodes, variables = newvarnames)
  glottodata[["glottodata"]][,-1] <- udata[,newvarnames]

  # References per variable
  if(!is.null(ref)){
    udata <- glottoconvert_colname(data = udata, oldfix = ref, oldpos = position, newfix = "_ref")
    refcols <- paste0(newvarnames, "_ref")[paste0(newvarnames, "_ref") %in% colnames(udata)]
    glottodata[["references"]][,paste0(newvarnames, "_ref")] <- udata[,refcols]
  }

  # Page numbers per variable
  if(!is.null(page)){
    udata <- glottoconvert_colname(data = udata, oldfix = page, oldpos = position, newfix = "_page")
    pagecols <- paste0(newvarnames, "_page")[paste0(newvarnames, "_page") %in% colnames(udata)]
    glottodata[["references"]][,paste0(newvarnames, "_page")] <- udata[,pagecols]
  }

  # Comments per variable
  if(!is.null(com)){
    udata <- glottoconvert_colname(data = udata, oldfix = com, oldpos = position, newfix = "_com")
    comcols <- paste0(newvarnames, "_com")[paste0(newvarnames, "_com") %in% colnames(udata)]
    glottodata[["comments"]][,comcols] <- udata[,comcols]
  }

  # if(!identical(varcols, refcols, pagecols) ){message("Number of columns does not match!")}



  # Copy values to glottodata





}

glottoconvert_colname <- function(data, oldfix, oldpos = NULL, newfix = NULL){
  if(is.null(newfix)){newfix <- ""}
  cols <- grepl(pattern = contrans_stringpos(string = oldfix, pos = oldpos), x = colnames(data), ignore.case = TRUE)
  oldnames <- colnames(data)[cols]
  newnames <- paste0(gsub(pattern = ref, x = oldrefnames, replacement = ""), newfix)
  colnames(data)[colnames(data) %in% oldnames] <- newnames
  data
}

