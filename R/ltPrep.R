#' Preparing life-tables for simulation
#' @param lt_male Life table for males. Should include Age for Age group and nMx for group specific mortality
#' @param lt_female Life table for females. Should include Age for Age group and nMx for group specific mortality
#' @param lt_out The rds file in which to write the resulting life table.
#' 
#' @return A life table to be 
#' @export prep_life_tables
#' @importFrom data.table melt
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr %>%
prep_life_tables <- function(lt_male, lt_female, lt_out){
  ltM <- lt_male %>% select(Age, nMx) %>% mutate(mrate=nMx)
  ltF <- lt_female %>% select(Age, nMx) %>% mutate(mrate=nMx)
  lt <- merge(ltM, ltF, by = c("Age")) %>% 
    select(Age, mrate.x, mrate.y) %>%
    melt(id.vars = "Age")
  
  lt<- lt %>% mutate(male = ifelse(variable == "mrate.x", 1, 0)) %>%
    select(-variable) %>%
    rename(mrate = value)
  
  saveRDS(lt, file = lt_out)
  return(lt)
}