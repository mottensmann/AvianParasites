#' Query the IUCN database via package rredlist
#' 
#' @description 
#' The [IUCN Redlist API](http://apiv3.iucnredlist.org/api/v3/docs) is queried using the `rredlist` package. Using this service requires a token. Duplicated entries are only queried once.
#' 
#' @param taxon character vector of scientific names
#' @param key token to query the IUCN API
#' @param .update Only download new taxons. keep previous
#' @param path path for output 
#'
#' @export
#' 
iucn_query <- function(taxon = NULL, key = NULL, .update = TRUE, path = "data") {
  
  if (isTRUE(.update)) {
    load("data/redlist.RData")
    load("data/habitats.RData")
    load("data/common.names.RData")
    
    ## Check which records are not needed

    taxon.not.asked <- character()
    if (any(which(!common.names[["name"]] %in% taxon))) {
      taxon.not.asked <- common.names[["name"]][which(!common.names[["name"]] %in% taxon)]
      common.names <- common.names[-which(common.names[["name"]] %in% taxon.not.asked),]
      redlist <- redlist[-which(redlist[["scientific_name"]] %in% taxon.not.asked),]
      habitats[taxon.not.asked] <- NULL
    } 
    
     taxon.present <- character()
     if (any(which(taxon %in% common.names[["name"]]))) {
       taxon.present <- taxon[which(taxon %in% common.names[["name"]])]
     }
     
     # which records are needed
    taxon.missing <- character()
    if (any(which(!taxon %in% common.names[["name"]]))) {
      taxon.missing <- taxon[which(!taxon %in% common.names[["name"]])]
      taxon <- taxon.missing
    }
  }
  
  ## avoid duplicates
  if (any(duplicated(taxon))) { 
    cat("Read", length(taxon), "names.")
    taxon <- unique(taxon)
    cat("Keep", length(taxon), "unique ones.")  
  }
  if (any(duplicated(common.names[["name"]]))) {
    common.names <- unique.data.frame(common.names)
    redlist <- unique.data.frame(redlist)
    habitats[which(duplicated(names(habitats)))] <- NULL
  }
  
  ## retrieve common names
  common.names.query <- pbapply::pblapply(taxon, function(x) {
    query <- rredlist::rl_common_names(x, key = RedLIstAPI, parse = T)
    if (length(query[["result"]]) > 0) {
      df <- data.frame(name = query[["name"]],
                       query[["result"]]) %>% 
        dplyr::filter(., primary == "TRUE")
      return(df[1,c("name", "taxonname")])
    } else {
      df <- data.frame(name = query[["name"]], taxonname = "Unknown")
      return(df)
    }
  }) %>% 
    do.call("rbind",.) %>% 
    ## correct all scientific names before continouing!
    dplyr::filter(., taxonname != "Unknown")
  
  ## unknown names
  unknown.names <- taxon[-which(taxon %in% common.names$name)]
  
  # ## retrieve habitats
  habitats.query <- pbapply::pblapply(common.names.query[["name"]], function(x) {
    query <- rredlist::rl_habitats(as.character(x), key = RedLIstAPI, parse = T)
    return(query[["result"]])
    
  }) %>% set_names(., value =  common.names.query$name)
  
  ## retrieve main accounts
  redlist.query <- pbapply::pblapply(common.names.query[["name"]], function(x) {
    query <- rredlist::rl_search(as.character(x), key = RedLIstAPI, parse = T)
    return(query[["result"]])
  }) %>%
    do.call("rbind",.)
  
  ## Append to existing data
  if (isTRUE(.update)) {
    if (nrow(common.names.query) > 0) {
      common.names <- rbind(common.names, common.names.query)
      redlist <- rbind(redlist.query, redlist)
      habitats <- append(habitats, habitats.query)
    } 
  }
  
  save(redlist, file =  "data/redlist.RData")
  save(habitats, file =  "data/habitats.RData")
  save(common.names, file = "data/common.names.RData")
  save(unknown.names, file = "data/unknown.names.RData")  
}

