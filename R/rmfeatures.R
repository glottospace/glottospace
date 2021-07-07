glotto_tidy <- function(){

}

rowdrop_dialects <- function(glottodata){
  glottodata <- glottodata %>% dplyr::filter(level != "dialect")
}

rowdrop_families <- function(glottodata){
  glottodata <- glottodata %>% dplyr::filter(level != "family")
}

rowdrop_bookkeeping <- function(glottodata){
  glottodata$bookkeeping <- as.logical(glottodata$bookkeeping)
  glottodata <- glottodata %>% dplyr::filter(bookkeeping == FALSE)
}

rowdrop_bookkeeping <- function(glottodata){
  glotto_filter(family_name = "sign")


  glottodata <- glottodata %>% dplyr::filter(bookkeeping == FALSE)
}
