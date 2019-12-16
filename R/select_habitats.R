#' select habitats for each species
#' 
#' @description 
#' Selects habitat description from the IUCN output, focussing on important habitats
#' during the breeding season. 
#' 
#' @param name 
#' scientific name
#' @param habitats 
#' list of data frames. Each object is named by the species name
#' 
#' @param out 
#' Output formats are either a `'matrix'`` where the importance of all habitat types (columns) are coded 
#' as 1 (=important) or 0 (=not important). Alternatively, a vector of habitat `'names'`` for the focal species is returned.
#' 
#' @param major.importance 
#' logical 
#' 
#' @export
#' 
select_habitats <- function(name = NULL, habitats = habitats, out = c("matrix", "names"),
                            major.importance = TRUE) {
  
  out <- match.arg(out)
  ## get all natural habitats
  natural.habitats <- lapply(habitats, function(x) x[["habitat"]]) %>% 
    unlist() %>% 
    as.factor() %>% 
    summary() %>% 
    names() %>% 
    .[stringr::str_detect(., "Artificial", negate = T)] %>% 
    .[stringr::str_detect(., "Introduced vegetation", negate = T)]
  
  ## subset and filter habitats dataframe
  hab <- dplyr::filter(habitats[[name]], 
                       habitat %in% natural.habitats,
                       suitability != "Marginal")
  
  ## distinguish between breeding season and resident
  if (any(hab[["season"]] == "Breeding Season")) {
    hab <- dplyr::filter(hab, season == "Breeding Season")
  } else if (any(hab[["season"]] == "Resident")) {
    hab <- dplyr::filter(hab, season == "Resident")
  }
  ## now check for major importance
  if (isTRUE(major.importance)) {
  if (any(hab[["majorimportance"]] == "Yes")) {
    hab <- dplyr::filter(hab, majorimportance == "Yes")
  }
  }
    
  if (out == "matrix") {
    ## create matrix to capture output
    output <- matrix(0, nrow = 1, ncol = length(natural.habitats)) %>% 
      set_colnames(., natural.habitats)
    
    output[1,hab[["habitat"]]] <- 1
    
  } else if (out == "names") {
    output <-   paste0(hab[["habitat"]], collapse = ";")
  }
  return(output)
}
