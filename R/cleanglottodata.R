# cleanglottodata/glottodataclean
# recode: N/A, NA, NA? --> NA
# "Y" --> TRUE
# etc.


gs_langdatacleaner <- function(data = NULL, rm = NULL, sel = NULL, id = NULL, structure = NULL, rmtypes = c("id", "meta", "bk", "fk")){

  structure <- suppressMessages(dplyr::left_join(data.frame("colnames" = colnames(data)), structure))
  types <- structure$type
  cbinary <- which(types == "asymm" | types == "symm")
  if(is.null(id)){id <- structure$colnames[(tolower(types) == "id")]}

  # reclass
  data[data == "#N/A" | data == "<NA>" | data == "NA" | data == "?"  | data == "" | data == " "] <- NA
  if(!is.null(cbinary)){
    bindat <- data[, cbinary]
    bindat[bindat == "Y" | bindat == "y" | bindat == 1] <- TRUE
    bindat[bindat == "N" | bindat == "n" | bindat == 0] <- FALSE
    data[, cbinary] <- bindat
    cat("Values in binary columns (symm/asymm) reclassified to TRUE/FALSE \n")
  }

  if(!purrr::is_empty(id)){

    idmissing <- nrow(data[is.na(data[,id]),] )
    if(idmissing > 0){
      data <- data[!is.na(data[,id]),]
      message(paste(idmissing, ' rows with missing ID removed'))
    }

    # Check whether ids are unique
    freqtab <- data.frame(table(data[,id]))
    colnames(freqtab)[1] <- "id"
    colnames(freqtab)[2] <- "n"

    if(any(freqtab$n > 1)){
      duplicate <- freqtab[freqtab$n > 1, ]
      message('IDs are not unique. The following ids have duplicates:')
      print(duplicate)
      message('Rownames not set because of duplicate ids. Please use the following naming convention: glottocode_dataname_001.')
    }

    if(all(freqtab$n == 1)){
      # set rownames
      data <- as.data.frame(data)
      rownames(data) <- data[,id]
    }
  }

  # select colnames to remove/select (not by index, because types argument uses index!)
  if(!is.null(rm) & is.numeric(rm)){rm <- colnames(data)[rm]}
  if(!is.null(sel) & is.numeric(sel)){sel <- colnames(data)[sel]}

  # remove columns based on types argument (by index of types!)
  if(length(rmtypes) > 0 | length(is.na(types)) > 0){
    rmcol <- which(tolower(types) %in% rmtypes)
    rmna <- which(is.na(types))
    rmcol <- c(rmcol, rmna)
    if(!is_empty(rmcol)){data <- data[, -rmcol]}
  }

  # remove columns based on sel/rm argument, done after index-based removal
  rm <- rm[rm %in% colnames(data)]
  if(purrr::is_empty(rm)){rm <- NULL}
  if(!is.null(rm) & is.null(sel)){data <- select(data, !all_of(rm))}

  sel <- sel[sel %in% colnames(data)]
  if(purrr::is_empty(sel)){sel <- NULL}
  if(is.null(rm) & !is.null(sel)){data <- dplyr::select(data, all_of(sel))}

  return(data)

}
