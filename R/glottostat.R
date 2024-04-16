#' Permanova across all groups (overall or pairwise)
#'
#' This function takes a glottodata or glottosubdata object and performs a
#' Permutational Multivariate Analysis of Variance (PERMANOVA). It can be used
#' to test whether two or more groups are significantly different from each
#' other (by specifying the 'comparison' argument with either 'overall' or
#' 'pairwise'). The function uses the 'group' column in the sample table to do
#' the comparisons. Before running the analysis, a distance matrix is
#' constructed from the glotto(sub)data object using glottodist(). The function
#' calls vegan::adonis2(), type ?adonis2 for more details.
#'
#' @param glottodata glottodata or glottosubdata
#' @param sample sample table (optional). By default, searches for sample table in glottodata/glottosubdata.
#' @param comparison Either "overall" or "pairwise"
#' @param permutations Number of permutations (default is 999)
#' @param metric Either "gower" or "anderberg"
#'
#' @keywords invisible
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottostat_permanova(glottodata, comparison = "pairwise")
#'
#' # Use subgroup (or another column in the structure table) as group
#' glottodata[["sample"]][,"group"] <- NULL # delete old 'group' column
#' glottodata[["sample"]][,"group"] <- glottodata[["sample"]][,"subgroup"]
#' glottostat_permanova(glottodata, comparison = "pairwise")
#'
#' glottosubdata <- glottoget("demosubdata", meta = TRUE)
#' glottostat_permanova(glottodata = glottosubdata, comparison = "pairwise")
glottostat_permanova <- function(glottodata, comparison = NULL, sample = NULL, permutations = NULL, metric = "gower"){

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

  glottodist <- glottodist(glottodata, metric = metric)
  metadist <- glottojoin_dist(glottodata = glottodata, glottodist = glottodist, na.rm = TRUE)
  if(id == "glottosubcode"){
    metadist$glottocode <- glottoconvert_subcodes(metadist$glottosubcode)
    metadist <- glottojoin_data(glottodata = metadist, with = glottosample, type = "left", id = "glottocode")
  } else{
    metadist <- glottojoin(glottodata = metadist, with = glottosample, id = "glottocode")
  }

  if(comparison == "overall"){
    message("Running overall permanova")
    resultsdf <- glottostat_permanovall(metadist = metadist, id = id, permutations = permutations, by = "group")
  } else if(comparison == "pairwise"){
    message("Running pairwise permanova")
    resultsdf <- glottostat_permanovapairs(metadist = metadist, id = id, permutations = permutations, by = "group")
  } else{stop("Please specify the type of comparison ('overall' or 'pairwise') ")}
return(resultsdf)
}



#' Permanova across all groups (overall or pairwise)
#'
#' This function takes a dist object and performs a
#' Permutational Multivariate Analysis of Variance (PERMANOVA). It can be used
#' to test whether two or more groups are significantly different from each
#' other (by specifying the \code{comparison} argument with either 'overall' or
#' 'pairwise').
#'
#' The argument \code{by} is the name of a column in the sample table,
#' which can be either provided by a "sample" sheet in \code{glottodata} or given by the argument \code{sample}.
#' The default value of \code{by} is "group".
#' The function uses \code{by} to do the comparisons.
#' The function calls \code{vegan::adonis2()}, type \code{?adonis2} for more details.
#'
#' @param glottodist a dist object
#' @param glottodata glottodata contains sample
#' @param comparison Either "overall" or "pairwise"
#' @param sample sample table (optional). By default, searches for sample table in glottodata/glottosubdata.
#' @param permutations Number of permutations (default is 999)
#' @param by the column name of "sample", over which to compute the permanova.
#'
#' @export
#'
#' @examples
#' glottodata <- glottoget("demodata", meta = TRUE)
#' glottodist <- glottodist(glottodata, metric = "gower")
#' glottostat_dist_permanova(glottodist = glottodist, glottodata = glottodata, comparison = "pairwise")
#'
#'
glottostat_dist_permanova <- function(glottodist = NULL, glottodata = NULL, comparison = NULL, sample = NULL, permutations = NULL, by = NULL){

  if(is.null(permutations)){permutations <- 999}
  if(is.null(comparison)){comparison <- "overall"}
  if(is.null(by)){by <- "group"}

  if(glottocheck_hassample(glottodata) & is.null(sample)){
    glottosample <- glottodata[["sample"]]
  } else if(glottocheck_hassample(glottodata) & !is.null(sample)){
    message("The glottodata has a sample sheet, but the Permanova is based on the given argument \"sample\".")
    glottosample <- sample
  } else if(!glottocheck_hassample(glottodata) & !is.null(sample)){
    message("The glottodata has no sample sheet, so the Permanova is based on the given argument \"sample\".")
    glottosample <- sample
  } else if(!glottocheck_hassample(glottodata) & is.null(sample)){
    stop("Please provide a sample table.")
  }

  if("group" %nin% colnames(glottosample)){stop("There is no group column in the sample table. Use glottocreate_sampletable()")}
  if(all(is.na(glottosample$group))){stop("Please add groups to the sample table. Use glottocreate_sampletable()")}

  id <- glottocheck_id(glottodata)

  if (id == "glottosubcode" & all(sapply(names(glottodist), nchar) == 8)){ # in the case of glottodist_subdata() or RI...
    metadist <- glottojoin_dist(glottodata = glottosample, glottodist = glottodist, na.rm = TRUE)
    id <- "glottocode"
  } else if (id == "glottosubcode" & !all(sapply(names(glottodist), nchar) == 8)){ # in the case of glottodist() for glottosubcode...
    metadist <- glottojoin_dist(glottodata = glottodata, glottodist = glottodist, na.rm = TRUE)
    metadist$glottocode <- glottoconvert_subcodes(metadist$glottosubcode)
    metadist <- glottojoin_data(glottodata = metadist, with = glottosample, type = "left", id = "glottocode")
  } else{
    metadist <- glottojoin_dist(glottodata = glottodata, glottodist = glottodist, na.rm = TRUE)
    metadist <- glottojoin(glottodata = metadist, with = glottosample, id = "glottocode")
  }

  if(comparison == "overall"){
    message("Running overall permanova")
    resultsdf <- glottostat_permanovall(metadist = metadist, id = id, permutations = permutations, by = by)
  } else if(comparison == "pairwise"){
    message("Running pairwise permanova")
    resultsdf <- glottostat_permanovapairs(metadist = metadist, id = id, permutations = permutations, by = by)
  } else{stop("Please specify the type of comparison ('overall' or 'pairwise') ")}
  return(resultsdf)
}


#' A temporary version of glottostat_dist_permanova
#'
#' @param glottodist a dist object
#' @param glottodata a glottodata
#' @param comparison comparision
#' @param sample sample
#' @param permutations permutations
#' @param by by
#'
#' @export
#'
glottostat_dist_permanova_mci <- function(glottodist = NULL, glottodata = NULL, comparison = NULL, sample = NULL, permutations = NULL, by = NULL){

  if(is.null(permutations)){permutations <- 999}
  if(is.null(comparison)){comparison <- "overall"}
  if(is.null(by)){by <- "group"}

  if(glottocheck_hassample(glottodata) & is.null(sample)){
    glottosample <- glottodata[["sample"]]
  } else if(glottocheck_hassample(glottodata) & !is.null(sample)){
    message("The glottodata has a sample sheet, but the Permanova is based on the given argument \"sample\".")
    glottosample <- sample
  } else if(!glottocheck_hassample(glottodata) & !is.null(sample)){
    message("The glottodata has no sample sheet, so the Permanova is based on the given argument \"sample\".")
    glottosample <- sample
  } else if(!glottocheck_hassample(glottodata) & is.null(sample)){
    stop("Please provide a sample table.")
  }

  if("group" %nin% colnames(glottosample)){stop("There is no group column in the sample table. Use glottocreate_sampletable()")}
  if(all(is.na(glottosample$group))){stop("Please add groups to the sample table. Use glottocreate_sampletable()")}

  id <- "glottocode"

  metadist <- glottojoin_dist(glottodata = glottosample, glottodist = glottodist, na.rm = TRUE)


  if(comparison == "overall"){
    message("Running overall permanova")
    resultsdf <- glottostat_permanovall(metadist = metadist, id = id, permutations = permutations, by = by)
  } else if(comparison == "pairwise"){
    message("Running pairwise permanova")
    resultsdf <- glottostat_permanovapairs(metadist = metadist, id = id, permutations = permutations, by = by)
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
glottostat_permanovall <- function(metadist, id, permutations, by = NULL){
  if (is.null(by)){
    by <- "group"
  }

condist <- metadist %>%
  dplyr::select(dplyr::all_of(metadist[,id]))

full <- vegan::adonis2(stats::as.formula(paste("condist", by, sep = " ~ ")),
                       data = metadist, permutations = permutations)
p <- round(full[["Pr(>F)"]][1],4)

groupnames <- unique(metadist[, by])
resultsdf <- data.frame(matrix(nrow = 1, ncol = length(groupnames)+2) )
colnames(resultsdf) <- c(paste0(by, 1:length(groupnames)), "p-value", "significance")
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
#' @param adj P-value adjustment method. Default is 'bonferroni'. see ?stats::p.adjust for alternatives.
#'
#' @noRd
#'
glottostat_permanovapairs <- function(metadist, id, permutations, adj = NULL, by = NULL){

  if(is.null(adj)){adj <- "bonferroni"}
   # Create empty data.frame to store results
  if(is.null(by)){by <- "group"}
  groupnames <- unique(metadist[, by])
  groupnames <- groupnames[!is.na(groupnames)]
  resultsdf <- data.frame(matrix(nrow = choose(length(groupnames), 2), ncol = 4) )
  colnames(resultsdf) <- c(paste0(by, 1:2), "p-value", "significance")
  resultsdf[ ,paste0(by, 1:2)] <- t(utils::combn(groupnames, 2))

  for(i in 1:nrow(resultsdf)){
    group1 <- resultsdf[i, paste0(by, 1)]
    group2 <- resultsdf[i, paste0(by, 2)]

  metadist12 <- metadist %>%
    dplyr::filter(metadist[, by] == group1 | metadist[, by] == group2) #  alternative: metadist12 <- metadist[metadist$group %in% c(group1, group2),]

  # Select distance matrix
  condist12 <- metadist12 %>%
    # dplyr::select(dplyr::all_of(.[[id]])) # don't replace dot (.) with .data!!!!!
    dplyr::select(dplyr::all_of(metadist12[, id])) # don't replace dot (.) with .data!!!!!

  # permutations <- permute::how(nperm = permutations)
  # permute::setBlocks(permutations) <- with(metadist12, glottocode)

  pair <- vegan::adonis2(stats::as.formula(paste("condist12", by, sep = " ~ ")), data = metadist12, permutations = permutations)
  p <- round(pair[["Pr(>F)"]][1],3)

  resultsdf[i, "p-value"] <- p
  resultsdf[i, "significance"] <- pvalstars(p)

  }

  # # p values adjusted.
  resultsdf[, "p-value (adj)"] <- round(stats::p.adjust(resultsdf[, "p-value"], method = adj), 3)
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

pvalvecstars <- function(pvalvec){
  pvalvec <- ifelse(is.na(pvalvec), NA, pvalvec)
  pvalvec <- ifelse(pvalvec <= 0.001, "***", pvalvec)
  pvalvec <- ifelse(pvalvec > 0.001 & pvalvec <= 0.01, "**", pvalvec)
  pvalvec <- ifelse(pvalvec > 0.01 & pvalvec <= 0.05, "*", pvalvec)
  pvalvec <- ifelse(pvalvec > 0.05 & pvalvec <= 0.1, ".", pvalvec)
  pvalvec <- ifelse(pvalvec > 0.1, "", pvalvec)

  pvalvec
}

# glottostat_dist_permanova <- function(glottodist = NULL, glottodata = NULL, comparison = NULL, sample = NULL, permutations = NULL, by = NULL){
#
#   if(is.null(permutations)){permutations <- 999}
#   if(is.null(comparison)){comparison <- "overall"}
#   if(is.null(by)){by <- "group"}
#
#   if(glottocheck_hassample(glottodata) & is.null(sample)){
#     glottosample <- glottodata[["sample"]]
#   } else if(glottocheck_hassample(glottodata) & !is.null(sample)){
#     message("The glottodata has a sample sheet, but the Permanova is based on the given argument \"sample\".")
#     glottosample <- sample
#   } else if(!glottocheck_hassample(glottodata) & !is.null(sample)){
#     message("The glottodata has no sample sheet, so the Permanova is based on the given argument \"sample\".")
#     glottosample <- sample
#   } else if(!glottocheck_hassample(glottodata) & is.null(sample)){
#     stop("Please provide a sample table.")
#   }
#
#   if("group" %nin% colnames(glottosample)){stop("There is no group column in the sample table. Use glottocreate_sampletable()")}
#   if(all(is.na(glottosample$group))){stop("Please add groups to the sample table. Use glottocreate_sampletable()")}
#
#   id <- glottocheck_id(glottodata)
#
#   metadist <- glottojoin_dist(glottodata = glottodata, glottodist = glottodist, na.rm = TRUE)
#
#   if(id == "glottosubcode"){
#     metadist$glottocode <- glottoconvert_subcodes(metadist$glottosubcode)
#     metadist <- glottojoin_data(glottodata = metadist, with = glottosample, type = "left", id = "glottocode")
#   } else{
#     metadist <- glottojoin(glottodata = metadist, with = glottosample, id = "glottocode")
#   }
#
#   if(comparison == "overall"){
#     message("Running overall permanova")
#     resultsdf <- glottostat_permanovall(metadist = metadist, id = id, permutations = permutations, by = by)
#   } else if(comparison == "pairwise"){
#     message("Running pairwise permanova")
#     resultsdf <- glottostat_permanovapairs(metadist = metadist, id = id, permutations = permutations, by = by)
#   } else{stop("Please specify the type of comparison ('overall' or 'pairwise') ")}
#   return(resultsdf)
# }
