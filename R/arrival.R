#' arrival records the newly-infected individuals at the previous time period, to add new individuals to the cohort
#' @param dat the current data on the cohort
#' @param at the current date
#' @param scenario scenario's name
#' @return  This function returns the \code{dat} object with updated treatment status for each individual, and the cohort summary
#' in \code{dat$epi} with the number of lost to follow-up
#' \code{lfu}, and the number of patients treated \code{onTrt} at each time unit.
arrival <- function(dat, at, scenario){
  nNew <- dat$epi$si.flow[at-1]
  ## Update Attribute
  if (nNew > 0) {
    dat <- setNewAttr(dat, at, nNew, scenario)
  }
  return(dat)
}


#'Assign attributes to the newly "created" individuals in the cohort
#' @param dat the current cohort
#' @param at the current date
#' @param nNew the number of new patients
#' @param scenario the scenario's name
#' @importFrom stats rnbinom
#' @return  This function returns the \code{dat} object with updated treatment status for each individual, and the cohort summary
#' in \code{dat$epi} with the number of lost to follow-up
#' \code{lfu}, and the number of patients treated \code{onTrt} at each time unit.
setNewAttr <- function(dat, at, nNew, scenario) {
  
  age <- dat$param$age
  # Set attributes for new infections to NA
  dat$attr <- lapply(dat$attr, function(x) c(x, rep(NA, nNew)))
  newIds <- which(is.na(dat$attr$active))
  
  # Network Status
  dat$attr$active[newIds] <- rep(1, nNew)
  dat$attr$entTime[newIds] <- rep(at, nNew)
  
  # Demography
  prop_male <- dat$param$prop_male
  dat$attr$male[newIds] <- rbinom(nNew, 1, prop_male)
  
  dat$attr$age[newIds] <- sample(age, nNew)
  
  # Epi/Clinical
  dat$attr$facility[newIds] <- sample(1:12,1)
  dat$attr$status[newIds] <- rep(1, nNew)
  dat$attr$infTime[newIds] <- at
  dat$attr$ageInf[newIds] <- dat$attr$age[newIds]
  dat$attr$dxStat[newIds] <- 0
  dat$attr$vlLevel[newIds] <- 0
  dat$attr$txCD4min[newIds] <-
    pmin(rnbinom(nNew,
                 size = nbsdtosize(dat$param[[scenario]]$tx.init.cd4.mean,
                                   dat$param[[scenario]]$tx.init.cd4.sd),
                 mu = dat$param[[scenario]]$tx.init.cd4.mean),
         dat$param[[scenario]]$tx.cd4.detect)
  
  if (length(unique(sapply(dat$attr, length))) != 1) {
    sapply(dat$attr, length)
    stop("Attribute dimensions not unique")
  }
  
  return(dat)
}