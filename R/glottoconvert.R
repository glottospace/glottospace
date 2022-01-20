
#' glottoconvert
#'
#' @param data Dataset to be transformed to glottodata
#' @param glottocode column name or column id with glottocodes
#' @param var Character string that distinguishes those columns which contain variable names.
#' @param ref Character string that distinguishes those columns which contain references.
#' @param table In case dataset consists of multiple tables, indicate which table contains the data that should be converted.
#' @param page Character string that distinguishes those columns which contain page numbers.
#' @param position Optional position of distinguishing character string, either "start" or "end"
#'
#' @return
#' @export
#'
#' @examples
#' # Load data from path:
#' # data <- glottoget("C:/Users/sjnor/surfdrive/Projecten en schrijfsels/Papers in progress/Yucuna-Tanimuca/data/testdata linguistics/2022_01_16_basic_data.xlsx")
glottoconvert <- function(data, glottocode = NULL, table = NULL, var = NULL, ref = NULL, page = NULL, position = NULL){
  if(!is_list(data) ){
    data <- data
  } else if(is_list(data) & is.null(table)){
    data <- data[[1]]
    message(paste0("Dataset contains multiple tables, assuming data to be converted is in the first table: ", names(data)[[1]]) )
  } else if(is_list(data) & !is.null(table) ){
    data <- data[[table]]
  }

  ncol <- length(colnames(data))

  # Glottocode
  if(is.null(glottocode)){glottocode <- "glottocode"}
  glottocol <- grep(pattern = glottocode, x = colnames(data), ignore.case = TRUE, value = TRUE)
  if(length(glottocol) > 1){stop(paste0("Column ", glottocode, " is duplicated. Please rename one of the columns."))}
  if(length(glottocol) == 0){stop(paste0("Column ", glottocode, " not found. Please add a column with glottocodes."))}
  glottocodes <- data[[glottocol]]

  if(!is.null(var)){
    varcols <- grepl(pattern = contrans_stringpos(string = var, pos = position), x = colnames(data), ignore.case = TRUE)
    varnames <- gsub(pattern = contrans_stringpos(string = var, pos = position), x = colnames(data)[varcols], replacement = "")
    nvarcols <- sum(varcols)
    message(paste0("Number of variable columns: ", nvarcols) )
  }
  if(!is.null(ref)){
    refcols <- grepl(pattern = contrans_stringpos(string = ref, pos = position), x = colnames(data), ignore.case = TRUE)
    nrefcols <- sum(refcols)
    message(paste0("Number of reference columns: ", nrefcols) )
  }
  if(!is.null(page)){
    pagecols <- grepl(pattern = contrans_stringpos(string = page, pos = position), x = colnames(data), ignore.case = TRUE)
    npagecols <- sum(pagecols)
    message(paste0("Number of reference columns: ", npagecols) )
  }

  if(!identical(varcols, refcols, pagecols) ){message("Number of columns does not match!")}


  glottodata <- glottocreate(glottocodes = glottocodes[1:3], variables = varnames[1:5])


}
