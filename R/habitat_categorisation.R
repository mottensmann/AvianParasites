#' Categorisation of habitat types in coarse groups
#' 
#' @param habitat.matrx matrix
#' @param path path to habitat schemes
#' @import magrittr
#' ' 
#' @export
#'
habiat_categorisation <- function(habitat.matrix = NULL, path = "data/simplified_habitat_scheme.xlsx") {
  metafile <- readxl::read_xlsx(path = path)
  ## all to lower case
  
  metafile <- apply(metafile, 2, tolower) %>% 
    as.data.frame()
  metafile$Subcategory <- as.character(metafile$Subcategory)

  x <- (tolower(colnames(habitat.matrix)) %in% metafile$Subcategory)
  colnames(habitat.matrix)[!x]
  
  #nrow(habitat.matrix)
  out <- lapply(1:nrow(habitat.matrix), function(x) {
    ## get all ticked habitats and map metadata to it
    habitat.matrix.sub <- data.frame(
      Subcategory = names(habitat.matrix[x,][habitat.matrix[x,] != 0]) %>% 
        tolower()) 
    habitat.matrix.sub$Subcategory <- as.character(habitat.matrix.sub$Subcategory)
    habitat.matrix.sub <-  dplyr::left_join(habitat.matrix.sub, metafile, by = "Subcategory")
    
    ## paste unique levels together
    
    out <- data.frame(
      taxon.iucn = rownames(habitat.matrix)[x],
      habitat =
        habitat.matrix.sub[["Habitat"]] %>% 
        unique() %>% 
        as.character() %>% 
        sort() %>% 
        lapply(., stringr::str_to_title) %>% 
        unlist() %>% 
      paste0(collapse = "")
# Thu Dec 05 17:06:31 2019 ------------------------------
# Zone not working because of NAs
            # ,
      # zone = habitat.matrix.sub[["Zone"]] %>% 
      #   unique() %>% 
      #   na.omit() %>% 
      #   as.character() %>% 
      #   sort() %>% 
      #   lapply(., stringr::str_to_title) %>% 
      #   unlist() %>% 
      # paste0()
    )
    return(out)
  }) %>% 
    do.call("rbind",.)
  
}
# ## take on row
# x <- 422
# path = "data/simplified_habitat_scheme.xlsx"
