# In this script we fetch species records from the IUCN Redlist API using 
# functions provided within the R-package 'rredlist'. In order to conduct queries
# a specific token is required (see http://apiv3.iucnredlist.org/api/v3/docs)

library(magrittr)

## set up API to IUCN RedList
library("rredlist")
## load the token. The token is not provided here!
load("RedListAPI.RData")

## taxon list
taxon <- lapply(list.files("data/Species_split/", full.names = T), read.csv) %>% 
  do.call("rbind",.)
taxon <- taxon[["Species"]] %>% as.character()

## retrieve common names
common.names <- pbapply::pblapply(taxon, function(x) {
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

## retrieve habitats
habitats <- pbapply::pblapply(common.names[["name"]], function(x) {
  query <- rredlist::rl_habitats(as.character(x), key = RedLIstAPI, parse = T)
  return(query[["result"]])
  
}) %>% set_names(., value =  common.names$name)

## retrieve main accounts
redlist <- pbapply::pblapply(common.names[["name"]], function(x) {
  query <- rredlist::rl_search(as.character(x), key = RedLIstAPI, parse = T)
  return(query[["result"]])
}) %>% 
  do.call("rbind",.)

save(redlist, file =  "data/redlist.RData")
save(habitats, file =  "data/habitats.RData")
save(common.names, file = "data/common.names.RData")
save(unknown.names, file = "data/unknown.names.RData")
