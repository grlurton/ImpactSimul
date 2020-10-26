#' @title Prevalence module
#' 
#'
#' @description Module function for keeping track of epidemiological quantities of interest
#'              including the number of virally suppressed agents `vlSupp`, 
#'              adherence over time.
#'
#' @param dat 
#' @param at 
#' @return
#' This function returns the \code{dat} object with updated epidemiological quantities for each
#' time step.
#' @seealso This function is used by:
#'   \code{\link{run_abm}}
#'   
prevalence <- function(dat, at) {
  
  status <- dat$attr$status
  male <- dat$attr$male
  age <- dat$attr$age
  
  nsteps <- dat$nsteps
  rNA <- rep(NA, nsteps)
  
  # Initialize vectors
  if (at == 1) {
    dat$epi$i.num <- rNA
    dat$epi$num <- rNA
    
    dat$epi$i.num.male <- rNA
    dat$epi$i.num.feml <- rNA
    dat$epi$i.prev.male <- rNA
    dat$epi$i.prev.feml <- rNA
    
    dat$epi$num.male <- rNA
    dat$epi$num.feml <- rNA
    dat$epi$meanAge <- rNA
    dat$epi$propMale <- rNA
    dat$epi$onTrt <- rNA
    dat$epi$Lfu <- rNA
    dat$epi$VlSupp <- rNA
    dat$epi$si.flow <- rNA
    dat$epi$dtrt.flow <- dat$epi$dlfu.flow <- rNA
    dat$epi$ds.flow <- dat$epi$di.flow <- rNA
  }
  
  dat$epi$i.num[at] <- sum(status == 1, na.rm = TRUE)
  dat$epi$num[at] <- length(status)
  
  dat$epi$i.num.male[at] <- sum(status == 1 & male == 1, na.rm = TRUE)
  dat$epi$i.num.feml[at] <- sum(status == 1 & male == 0, na.rm = TRUE)
  dat$epi$i.prev.male[at] <- sum(status == 1 & male == 1, na.rm = TRUE) /
    sum(male == 1, na.rm = TRUE)
  dat$epi$i.prev.feml[at] <- sum(status == 1 & male == 0, na.rm = TRUE) /
    sum(male == 0, na.rm = TRUE)
  
  dat$epi$num.male[at] <- sum(male == 1, na.rm = TRUE)
  dat$epi$num.feml[at] <- sum(male == 0, na.rm = TRUE)
  dat$epi$meanAge[at] <- mean(age, na.rm = TRUE)
  dat$epi$propMale[at] <- mean(male, na.rm = TRUE)
  dat$epi$VlSupp[at] <- length(whichVlSupp(dat$attr, dat$param))
  dat$epi$AidsonART[at] <- length(whichAidsonART(dat$attr, dat$param))
  dat$epi$AidsoffART[at] <- length(whichAidsoffART(dat$attr, dat$param))
  
  return(dat)
}


whichVlSupp <- function(attr, param) {
  which(attr$status == 1 &
          attr$vlLevel <= log10(50) &
          (attr$age - attr$ageInf) * (365 / param$time.unit) >
          (param$vl.acute.topeak + param$vl.acute.toset))
}

whichAidsonART <- function(attr, param) {
  which(attr$txStat == 1 &
          attr$cd4Count <= param$di.cd4.aids)
}

whichAidsoffART <- function(attr, param) {
  which(attr$txStat == 0 &
          attr$cd4Count <= param$di.cd4.aids)
}