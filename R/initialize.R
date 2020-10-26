#' @title initialize module
#' 
#' @description These scripts contain the functions that set the initial conditions for the models.
##              (1) Initial parametrization of the models including the maximum infection time, and the size of the cohort
##              (2) Create the initial cohort based on the initial parameters, the proportion of male, and the number of time units (steps)
##              (3) Define individuals initial infection status based on the prevalence in the simulated cohort
##              (4) Define individuals infection time based on the maximum infection time provided by the user
##              (5) Define individuals initial diagnostic status
##              (6) Define individuals initial treatment status, and stochastically determined individuals' CD4 count at treatment initiation
#' @param i.prev.male prevalence of the infection in males
#' [default = 1]
#' @param i.prev.feml prevalence of the infection in females
#' [default = 1]
#' @param max.inf.time oldest infection considered in the simulation
#' @param n number of patients in the simulated cohort
#' @return: This function returns
#'          (1) the \code{p} object with additional parameters
#'          (2) the \code{dat} object with initial \code{male}, \code{active},\code{facility}, \code{age}, and \code{entTime} attributes
#'          (3) the \code{dat} object with initial \code{status}
#'          (4) the \code{dat} object with initial \code{ageInf}
#'          (5) the \code{dat} object with initial \code{dxStatus} and \code{dxTime}
#'          (6) the \code{dat} object with initial \code{txCD4min}, \code{txCD4start}, and \code{txType}
#' @export init
init <- function(i.prev.male = 1,
                  i.prev.feml = 1,
                  max.inf.time = 5 * 365,
                 n = 1000,
                     ...) {
  
  ## Process parameters
  p <- list()
  formal.args <- formals(sys.function())
  formal.args[["..."]] <- NULL
  for (arg in names(formal.args)) {
    p[arg] <- list(get(arg))
  }
  dot.args <- list(...)
  names.dot.args <- names(dot.args)
  if (length(dot.args) > 0) {
    for (i in 1:length(dot.args)) {
      p[[names.dot.args[i]]] <- dot.args[[i]]
    }
  }
  
  class(p) <- "init"
  return(p)
}

# 2. Create the initial cohort based on the initial parameters, the proportion of male, and the number of time units (steps)

initialize <- function(init, param, scenario, prop.male, nsteps) {
  
  dat <- list()
  dat$temp <- list()
  dat$attr <- list()
  dat$init <- init
  n <- dat$init$n
  id <- 1:n
  # k <- nb.facilities
  facility <- cut(1:n, breaks = c(0, 25, 50, 75, 125, 175, 250, 325, 425, 525, 650, 800, Inf),
                     labels = FALSE, right = FALSE)
  dat$attr$facility <- facility
  dat$param <- param
  
  dat$nsteps <- nsteps
  
  dat$attr$male <- rbinom(n = n, size = 1, prob = prop.male)
  
  dat$attr$active <- rep(1, n)
  dat$attr$entTime <- rep(1, n)
  
  dat <- initStatus(dat)

  age <- dat$param$age
  dat$attr$age <- sample(age, n, TRUE)
  
  dat <- initInfTime(dat)
  dat <- initDx(dat)
  dat <- initTx(dat, scenario = scenario)

}

# 3. Define individuals initial infection status based on the prevalence in the simulated cohort
## here, the focus is a cohort of HIV-infected individuals; therefore, the prevalence is 1, everyone is infected
## but later application, interested in looking at HIV-uninfected individuals could use a prevalence lower than one

initStatus <- function(dat) {
  
  ## Variables
  i.prev.male <- dat$init$i.prev.male
  i.prev.feml <- dat$init$i.prev.feml
  
  male <- dat$attr$male
  idsMale <- which(male == 1)
  idsFeml <- which(male == 0)
  nMale <- length(idsMale)
  nFeml <- length(idsFeml)
  n <- nMale + nFeml
  
  ## Process
  status <- rep(0, n)
  status[sample(idsMale, round(i.prev.male * nMale))] <- 1
  status[sample(idsFeml, round(i.prev.feml * nFeml))] <- 1
  
  dat$attr$status <- status
  
  return(dat)
}

# 4. Define individuals infection time based on the maximum infection time provided by the user; we use a geometric distribution to stochastically 
## determine the date of infection; the geometric distribution makes earlier infections more likely than later infections 
#' @importFrom stats rgeom
initInfTime <- function(dat) {
  
  status <- dat$attr$status
  n <- length(status)
  
  infecteds <- which(status == 1)
  infTime <- rep(NA, n)
  
  
  total.d.rate <- 1/dat$init$max.inf.time
  infTime[infecteds] <- -rgeom(length(infecteds), total.d.rate)
  
  dat$attr$infTime <- infTime
  
  timeInf <- 1 - infTime
  dat$attr$ageInf <- pmax(0, dat$attr$age - round(timeInf * 1/ 365))
  
  stopifnot(all(dat$attr$ageInf[infecteds] <= dat$attr$age[infecteds]),
            all(dat$attr$ageInf[infecteds] >= 0))
  
  return(dat)
}

# 5. Define individuals initial diagnostic status
## Here, we wanted to look at a naive population, i.e. no one has been diagnosed with HIV at the initiation of the simulation
## but later applications could change this assumption to have some individuals diagnosed 

initDx <- function(dat) {
  
  n <- sum(dat$attr$active == 1)
  status <- dat$attr$status
  
  dxStat <- rep(NA, n)
  dxStat[status == 1] <- 0
  
  dxTime <- rep(NA, n)
  
  dat$attr$dxStat <- dxStat
  dat$attr$dxTime <- dxTime
  
  return(dat)
}

# 6. Define individuals initial treatment status, and stochastically determined individuals' CD4 count at treatment initiation
## NB: some patients may actually never initiate treatment during the intervention, but we supposed that this quantity exists anyway
## and that if we could track patients infinitely this is the CD4 count they would have at treatment initiation, but treatment initiation
## can be a censored event, i.e. not observed because the study ended or the patient died before initiating treatment
## Here, we wanted to look at a naive population, i.e. no one has been treated for HIV at the initiation of the simulation
## but later applications could change this assumption to have some individuals on treatment

initTx <- function(dat, scenario) {
  
  ## Variables
  status <- dat$attr$status
  n <- sum(dat$attr$active == 1)
  nInf <- sum(status == 1)
  
  tx.init.cd4.mean <- dat$param[[scenario]]$tx.init.cd4.mean
  tx.init.cd4.sd <- dat$param[[scenario]]$tx.init.cd4.sd
  tx.cd4.detect <- dat$param[[scenario]]$tx.cd4.detect
  
  ## Process
  dat$attr$txStat <- rep(NA, n)
  dat$attr$txStartTime <- rep(NA, n)
  dat$attr$txStops <- rep(NA, n)
  dat$attr$Lfu <- rep(NA, n)
  dat$attr$txTimeOn <- rep(NA, n)
  dat$attr$txTimeOff <- rep(NA, n)

  txCD4min <- rep(NA, n)
  txCD4min[status == 1] <- pmin(rnbinom(nInf,
                                        size = nbsdtosize(tx.init.cd4.mean,
                                                          tx.init.cd4.sd),
                                        mu = tx.init.cd4.mean), tx.cd4.detect)
  dat$attr$txCD4min <- txCD4min
  dat$attr$txCD4start <- rep(NA, n)
  dat$attr$txType <- rep(NA, n)
  
  return(dat)
}
