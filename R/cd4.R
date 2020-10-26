#' @title cd4 module
#'
#' @description Module for assigning to newly HIV-infected individuals their baseline CD4 count (i.e. CD4 count at time of infection) 
#'              and updating individuals cd4 count at each time step, depending on individuals' treatment status, age and sex
#' @param dat data
#' @param at parameter
#' @return
#' This function returns \code{dat} after updating individuals'
#' \code{cd4Count}. 
##

cd4 <- function(dat, at) {
  
  status <- dat$attr$status
  time.unit <- dat$param$time.unit
  
  if (is.null(dat$attr$cd4Count)) {
    dat$attr$cd4Count <- rep(NA, length(status))
  }
  cd4Count <- dat$attr$cd4Count
  
  
  # Assign CD4 for newly infected -------------------------------------------
  idsAsn <- which(status == 1 & is.na(cd4Count))
  if (length(idsAsn) > 0) {
    cd4Count[idsAsn] <- expected_cd4(dat = dat, method = "assign",
                                     male = dat$attr$male[idsAsn],
                                     age = dat$attr$age[idsAsn],
                                     ageInf = dat$attr$ageInf[idsAsn],
                                     time.unit = time.unit)
  }
  
  
  # CD4 natural decline -----------------------------------------------------
  txStartTime <- dat$attr$txStartTime
  infTime <- dat$attr$infTime
  
  idsUpd <- which(status == 1 & infTime < at & is.na(txStartTime))
  idsUpd <- setdiff(idsUpd, idsAsn) # only if you have been infected at a previous time
  
  if (length(idsUpd) > 0) {
    cd4Count[idsUpd] <- expected_cd4(dat = dat, method = "update",
                                     cd4Count1 = cd4Count[idsUpd],
                                     male = dat$attr$male[idsUpd],
                                     age = dat$attr$age[idsUpd],
                                     ageInf = dat$attr$ageInf[idsUpd],
                                     time.unit = time.unit)
  }
  
  # CD4 increase with ART ---------------------------------------------------
  male <- dat$attr$male
  txStat <- dat$attr$txStat
  
  tx.cd4.recrat.feml <- dat$param$tx.cd4.recrat.feml
  tx.cd4.recrat.male <- dat$param$tx.cd4.recrat.male
  
  idsTxFeml <- which(status == 1 & male == 0 & txStat == 1)
  idsTxMale <- which(status == 1 & male == 1 & txStat == 1)
  
  if (length(idsTxFeml) > 0) {
    cd4Cap <- expected_cd4(dat = dat, method = "assign", male = 0, age = 25, ageInf = 25)
    cd4Count[idsTxFeml] <- pmin(cd4Count[idsTxFeml] + tx.cd4.recrat.feml, cd4Cap)
  }
  if (length(idsTxMale) > 0) {
    cd4Cap <- expected_cd4(dat = dat, method = "assign", male = 1, age = 25, ageInf = 25)
    cd4Count[idsTxMale] <- pmin(cd4Count[idsTxMale] + tx.cd4.recrat.male, cd4Cap)
  }
  
  
  # CD4 decline post ART ----------------------------------------------------
  tx.cd4.decrat.feml <- dat$param$tx.cd4.decrat.feml
  tx.cd4.decrat.male <- dat$param$tx.cd4.decrat.male
  
  idsNoTxFeml <- which(status == 1 & male == 0 &
                         !is.na(txStartTime) & txStat == 0)
  idsNoTxMale <- which(status == 1 & male == 1 &
                         !is.na(txStartTime) & txStat == 0)
  if (length(idsNoTxFeml) > 0) {
    cd4Count[idsNoTxFeml] <- pmax(cd4Count[idsNoTxFeml] - tx.cd4.decrat.feml, 0)
  }
  if (length(idsNoTxMale) > 0) {
    cd4Count[idsNoTxMale] <- pmax(cd4Count[idsNoTxMale] - tx.cd4.decrat.male, 0)
  }
  
  if (any(is.na(cd4Count[status == 1]))) {
    stop("NA in cd4Count among infected")
  }
  
  dat$attr$cd4Count <- cd4Count
  
  return(dat)
}


expected_cd4 <- function(dat, method, cd4Count1, cd4Count2,
                         male, age, ageInf,
                         at, time.unit = 7) {
  
  base.cd4 <- dat$param$base.cd4
  
  ## Variables
  timeInf <- (age - ageInf) * (365 / time.unit)
  catAI <- cut(ageInf, breaks = c(0, 30, 40, 50, Inf),
               labels = FALSE, right = FALSE)
  
  ## Model parameters
  base.male <- sqrt(base.cd4)
  base.feml <- base.male + 1.11
  bases <- c(base.feml, base.male)
  ind.bases <- bases[male + 1]
  
  # Yearly slopes
  slope1 <- -1.49 + 0.34
  slope2 <- slope1 - 0.10
  slope3 <- slope1 - 0.34
  slope4 <- slope1 - 0.63
  slopes <- c(slope1, slope2, slope3, slope4)
  ind.slopes <- slopes[catAI] * (time.unit / 365)
  
  if (method == "timeto") {
    tt1 <- (sqrt(cd4Count1) - ind.bases)/ind.slopes
    if (!missing(cd4Count2)) {
      tt2 <- (sqrt(cd4Count2) - ind.bases)/ind.slopes
      return(tt2 - tt1)
    } else {
      return(tt1)
    }
  } else {
    if (method == "assign") {
      cd4CountSqrt <- ind.bases + (ind.slopes * timeInf)
      cd4CountSqrt <- pmax(1, cd4CountSqrt)
    }
    if (method == "update") {
      cd4CountSqrt <- sqrt(cd4Count1) + ind.slopes
      cd4CountSqrt[cd4CountSqrt < 1] <- 0
    }
    cd4Count <- cd4CountSqrt ^ 2
    return(cd4Count)
  }
  
}
