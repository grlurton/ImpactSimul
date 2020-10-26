#' Load extracts from UNAIDS' website to usable estimates
#' @param extracted_data adress of the extracted datga
#' @param output adress where the result of the function will be written
#' @return a rds file of the usable parameters
#' @importFrom stringr str_extract
#' @export 
load_estimates <- function(extracted_data, output){
  estUN <- read.csv(extracted_data, header = T)
  pattern <- regex("[<]?(\\d{1,3})[ .]?(\\d+)[ ]\\[[<]?(\\d{1})[ .]?(\\d+)[ ]?(\\d+)?[ ][-][ ][<]?(\\d{1})[ .]?(\\d+)[ ]?(\\d+)?\\]")
  values <- str_extract(estUN$Estimates, pattern)
  index <- !is.na(values)
  split_pattern <- regex("\\[|[-]|\\]")
  estDF <- matrix(unlist(str_split(values[index], pattern = split_pattern)), nrow = sum(index), byrow = T)
  
  estDF <- data.frame(estDF) %>% rename(Mean = X1, Lo = X2, Hi = X3) %>%
    select(-X4)
  estDF <- estDF %>%
    mutate(Mean = gsub(pattern = " ", replacement = "", x = Mean),
           Lo = gsub(pattern = " ", replacement = "", x = Lo),
           Hi = gsub(pattern = " ", replacement = "", x = Hi))
  estDF <- estDF %>%
    mutate(MeanNum = as.numeric(as.character(Mean)),
           LoNum = as.numeric(as.character(Lo)),
           HiNum = as.numeric(as.character(Hi)))
  
  estDF$indic <- matrix(unlist(str_split(as.character(estUN$Estimates[index]), pattern = regex("[<]?(\\d{1,3})[ .]?(\\d+)[ ]\\["))), nrow = sum(index), byrow = T)[,1]

  saveRDS(estDF, file = output)
}

#' Extract data from a given pays from the UNAIDS database
#' @param python_script path to unaids_scrap.py
#' @param country country of interest
#' @param url url to unaids website
#' @param outpath path to the file where the extraction will be written
#' @importFrom reticulate source_python
#' @importFrom reticulate use_condaenv
#' @export
extract_unaids <- function(conda_env, python_script, country, path_to_chromedriver, outpath, url = 'https://aidsinfo.unaids.org/'){
  ImpactSimulEnv <- use_condaenv(conda_env)
  source_python(python_script, envir = globalenv() )
  extract_unaids_py(country, url, path_to_chromedriver, outpath)
}