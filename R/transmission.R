#' @title transmission module
#'
#' @description Calculate the number of new infections at each time step based on biological parameters
#'              (the baseline probability of transmission per act \code{base.tprob}, the number of act per time unit), 
#'              and the viral load level of each individual 
#' @param dat 
#' @param at 
#' @return
#' This function returns the epidemiological module \code{dat$epi} after calculating the number of new infection
#' \code{si.flow} at each time step. 
#' @seealso This function is used by:
#'   \code{\link{run_abm}}

transmission <- function(dat, at) {

    ## Transmission
    nInf <- 0
    # Base transmission probability
    status <- dat$attr$status
    vlLevel <- dat$attr$vlLevel
    male <- dat$attr$male
    cd4Count <- dat$attr$cd4Count
    time.unit <- dat$param$time.unit
    base.tprob <- dat$param$base.tprob
    act.rate.early <- dat$param$act.rate.early
    act.rate.late <- dat$param$act.rate.late
    act.rate.cd4 <- dat$param$act.rate.cd4
    k <- 2.45
    act <- as.numeric(cd4Count < act.rate.cd4)*act.rate.late + (1-as.numeric(cd4Count < act.rate.cd4))*act.rate.early
    
    trans.prob <- rep(NA, length(status))
    
    trans.prob[which(status == 1 & male == 1)] <- 1-(1-base.tprob[1]*k^(vlLevel[which(status == 1 & male == 1)]-1))^act[which(status == 1 & male == 1)]
    trans.prob[which(status == 1 & male == 0)] <- 1-(1-base.tprob[2]*k^(vlLevel[which(status == 1 & male == 0)]-1))^act[which(status == 1 & male == 0)]
    
    nInf <- sum(trans.prob[which(status == 1)], na.rm = T)
    
  ## Incidence vector
  dat$epi$si.flow[at] <- round(nInf)
  
  return(dat)
}