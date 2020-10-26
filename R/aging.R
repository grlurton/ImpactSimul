#' Module for aging over time
#'
#' @param dat object with patients' data
#' 
#' @return 
#' This function returns \code{dat} after updating ' \code{age}. 
aging <- function(dat, at) {
  
  ## Parameters
  time.unit <- dat$param$time.unit
  
  ## Attributes
  age <- dat$attr$age
  
  ## Updates
  age <- age + time.unit/365
  
  ## Save out
  dat$attr$age <- age
  
  return(dat)
}