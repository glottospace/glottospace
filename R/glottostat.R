

#' Permanova across all groups (overall)
#'
#' @param glottodata glottodata or glottosubdata
#' @param sample sample table
#'
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottostat_permanovall(glottodata)
glottostat_permanovall <- function(glottodata, sample = NULL, permutations = NULL){

if(is.null(permutations)){permutations <- 1000}

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
metadist <- glottojoin_dist(glottodata = glottodata, dist = glottodist, rm.na = TRUE)
if(id == "glottosubcode"){
  metadist$glottocode <- glottoconvert_subcodes(metadist$glottosubcode)
  metadist <- glottojoin_data(glottodata = metadist, with = glottosample, type = "left", id = "glottocode")
} else{
  metadist <- glottojoin(glottodata = metadist, with = glottosample, id = "glottocode")
}

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
#' @param glottodata glottodata
#' @param sample sample table
#'
#' @noRd
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottostat_permanovapairs(glottodata)
glottostat_permanovapairs <- function(glottodata, sample = NULL, permutations = NULL){

  if(is.null(permutations)){permutations <- 1000}

  if(glottocheck_hassample(glottodata) & is.null(sample)){
    glottosample <- glottodata[["sample"]]
  } else if(glottocheck_hassample(glottodata) & !is.null(sample)){
    glottosample <- sample
  } else if(!glottocheck_hassample(glottodata) & !is.null(sample)){
    glottosample <- sample
  } else if(!glottocheck_hassample(glottodata) & is.null(sample)){
    stop("Please provide a sample table")
  }

  id <- glottocheck_id(glottodata)

  glottodist <- glottodist(glottodata)
  metadist <- glottojoin(glottodata = glottodata, with = glottodist)
  metadist <- glottojoin(glottodata = metadist, with = glottosample)

  # Create empty data.frame to store results
  groupnames <- unique(metadist$group)
  resultsdf <- data.frame(matrix(nrow = choose(length(groupnames), 2), ncol = 4) )
  colnames(resultsdf) <- c("group1", "group2", "p-value", "significance")
  resultsdf[,c("group1", "group2")] <- t(utils::combn(groupnames, 2))

  for(i in 1:nrow(resultsdf)){
    group1 <- resultsdf[i, "group1"]
    group2 <- resultsdf[i, "group2"]

  metadist12 <- metadist %>%
    dplyr::filter(group == group1 | group == group2)

  condist12 <- metadist12 %>%
    dplyr::select(all_of(.[[id]]))

  pair <- vegan::adonis2(condist12 ~ group, data = metadist12, permutations = permutations)
  p <- round(pair[["Pr(>F)"]][1], 4)

  resultsdf[i, "p-value"] <- p
  resultsdf[i, "significance"] <- pvalstars(p)

  }

  # # p values adjusted.
  resultsdf[, "p-value (adj)"] <- stats::p.adjust(resultsdf[, "p-value"], method = "BH")
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
