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
