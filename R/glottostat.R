#' Permanova across all groups (overall or pairwise)
#'
#' @param glottodata glottodata or glottosubdata
#' @param sample sample table (optional). By default, searches for sample table in glottodata/glottosubdata.
#' @param comparison Either "overall" or "pairwise"
#' @param permutations Number of permutations (default is 999)
#'
#' @keywords invisible
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottostat_permanova(glottodata, comparison = "pairwise")
#'
#' glottodata[["sample"]][,2] <- glottodata[["sample"]][,3]
#' glottostat_permanova(glottodata, comparison = "pairwise")
#'
#' glottosubdata <- glottoget("demosubdata", meta = TRUE)
#' glottostat_permanova(glottodata = glottosubdata, comparison = "pairwise")
glottostat_permanova <- function(glottodata, comparison = NULL, sample = NULL, permutations = NULL){

  if(is.null(permutations)){permutations <- 999}
  if(is.null(comparison)){comparison <- "overall"}

  if(glottocheck_hassample(glottodata) & is.null(sample)){
    glottosample <- glottodata[["sample"]]
  } else if(glottocheck_hassample(glottodata) & !is.null(sample)){
    glottosample <- sample
  } else if(!glottocheck_hassample(glottodata) & !is.null(sample)){
    glottosample <- sample
  } else if(!glottocheck_hassample(glottodata) & is.null(sample)){
    stop("Please provide a sample table")
  }

  if("group" %nin% colnames(glottosample)){stop("There is no group column in the sample table. Use glottocreate_sampletable()")}
  if(all(is.na(glottosample$group))){stop("Please add groups to the sample table. Use glottocreate_sampletable()")}

  id <- glottocheck_id(glottodata)

  glottodist <- glottodist(glottodata)
  metadist <- glottojoin_dist(glottodata = glottodata, glottodist = glottodist, rm.na = TRUE)
  if(id == "glottosubcode"){
    metadist$glottocode <- glottoconvert_subcodes(metadist$glottosubcode)
    metadist <- glottojoin_data(glottodata = metadist, with = glottosample, type = "left", id = "glottocode")
  } else{
    metadist <- glottojoin(glottodata = metadist, with = glottosample, id = "glottocode")
  }

  if(comparison == "overall"){
    message("Running overall permanova")
    resultsdf <- glottostat_permanovall(metadist = metadist, id = id, permutations = permutations)
  } else if(comparison == "pairwise"){
    message("Running pairwise permanova")
    resultsdf <- glottostat_permanovapairs(metadist = metadist, id = id, permutations = permutations)
  } else{stop("Please specify the type of comparison ('overall' or 'pairwise') ")}
return(resultsdf)
}


#' Permanova across all groups (overall)
#'
#' @param metadist glottodata/glottsubdata joined with glottodist
#' @param id Either 'glottocode' or 'glottosubcode'
#' @param permutations Number of permutations (default is 999)
#'
#' @noRd
#'
glottostat_permanovall <- function(metadist, id, permutations){

condist <- metadist %>%
  dplyr::select(dplyr::all_of(metadist[,id]))

full <- vegan::adonis2(condist ~ group, data = metadist, permutations = permutations)
p <- round(full[["Pr(>F)"]][1],4)

groupnames <- unique(metadist$group)
resultsdf <- data.frame(matrix(nrow = 1, ncol = length(groupnames)+2) )
colnames(resultsdf) <- c(paste0("group", 1:length(groupnames)), "p-value", "significance")
resultsdf[1,1:length(groupnames)] <- groupnames
resultsdf[1,"p-value"] <- p
resultsdf[1,"significance"] <- pvalstars(p)

resultsdf

}



#' Permanova across all groups (pairwise)
#'
#' @param metadist glottodata/glottsubdata joined with glottodist
#' @param id Either 'glottocode' or 'glottosubcode'
#' @param permutations Number of permutations (default is 999)
#'
#' @noRd
#'
glottostat_permanovapairs <- function(metadist, id, permutations){

   # Create empty data.frame to store results
  groupnames <- unique(metadist$group)
  resultsdf <- data.frame(matrix(nrow = choose(length(groupnames), 2), ncol = 4) )
  colnames(resultsdf) <- c("group1", "group2", "p-value", "significance")
  resultsdf[,c("group1", "group2")] <- t(utils::combn(groupnames, 2))

  for(i in 1:nrow(resultsdf)){
    group1 <- resultsdf[i, "group1"]
    group2 <- resultsdf[i, "group2"]

  metadist12 <- metadist %>%
    dplyr::filter(.data$group == group1 | .data$group == group2) #  alternative: metadist12 <- metadist[metadist$group %in% c(group1, group2),]

  # Select distance matrix
  condist12 <- metadist12 %>%
    dplyr::select(dplyr::all_of(.[[id]])) %>% as.dist()

  pair <- vegan::adonis2(condist12 ~ group, data = metadist12, permutations = permutations)
  p <- round(pair[["Pr(>F)"]][1], 5)

  resultsdf[i, "p-value"] <- p
  resultsdf[i, "significance"] <- pvalstars(p)

  }

  # # p values adjusted.
  resultsdf[, "p-value (adj)"] <- round(stats::p.adjust(resultsdf[, "p-value"], method = "BH"), 5)
  resultsdf[, "sign (adj)"] <- sapply(X = resultsdf[, "p-value (adj)"], FUN = pvalstars)

  resultsdf
}

pvalstars <- function(pval){
  if(is.na(pval)){
    pstar <- NA
  } else if(pval > 0.05  ){
    pstar <- "n.s."
  } else if(pval <= 0.001){
    pstar <- "***"
  } else if(pval <= 0.01 ){
    pstar <- "**"
  } else if(pval <= 0.05 ){
    pstar <- "*"
  }

pstar
}
