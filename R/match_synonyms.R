#' Function to solve issues in taxonomy concepts between dfs
#' 
#' @description 
#' For all species names this functions tries to find a match between taxon names given in `df` and `BirdFuncDat`. First, all scientific names are checked in the sequence `taxon.iucn`, `taxon.hbw` and `taxon.malavi`. If no match was found, common english species names are compared. .
#' 
#' @param df data frame
#' @param BirdFuncDat data frame of Elton´s niche traits
#' @return character of matching names. NA indicate that no match was found
#' 
#' @export
#' 
match_synonyms <- function(df = NULL, BirdFuncDat = NULL) {
  
  out <- pbapply::pblapply(1:nrow(df), function(x) {
    if (df[["taxon.iucn"]][x] %in% BirdFuncDat[["Scientific"]]) {
      return(df[["taxon.iucn"]][x])
    } else if (df[["taxon.hbw"]][x] %in% BirdFuncDat[["Scientific"]]) {
      return(df[["taxon.hbw"]][x])
    } else if (df[["taxon.malavi"]][x] %in% BirdFuncDat[["Scientific"]]) {
      return(df[["taxon.malavi"]][x])
    } else if (df[["taxon.english"]][x] %in% BirdFuncDat[["English"]]) {
      return(BirdFuncDat[["Scientific"]][BirdFuncDat[["English"]] %in% df[["taxon.english"]][x]])
    } else {
      return(NA)
    }
  }) %>% 
  unlist()
  return(out)
}