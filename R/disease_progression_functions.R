# rate of decline of CD4 per quarter as a function of viral load in the absence of treatment
rateCD4decline <- function(vl){
  cd4 <- rep(NA, length(vl))
  cd4[which(vl <= 2.7)] <- rnorm(n = length(which(vl <= 2.7)), mean = 5.1, sd = (7.8-5.1)/2)
  cd4[which(vl > 2.7 & vl <= 3.3)] <- rnorm(n = length(which(vl > 2.7 & vl <= 3.3)), mean = 9.9, sd = (12.3-9.9)/2)
  cd4[which(vl > 3.3 & vl <= 4.0)] <- rnorm(n = length(which(vl > 3.3 & vl <= 4.0)), mean = 12.0, sd = (13.8-12)/2)
  cd4[which(vl > 4.0 & vl <= 4.6)] <- rnorm(n = length(which(vl > 4.0 & vl <= 4.6)), mean = 14.1, sd = (16.2-14.1)/2)
  cd4[which(vl > 4.6)] <- rnorm(n = length(which(vl > 4.6)), mean = 19.5, sd = (21.9-19.5)/2)
  return(cd4)
}


# off-art probability of an opportunistic infection per quarter
#' @importFrom stats rbinom
#' @importFrom stats rnorm
OI <- function(N){
  OI <- rbinom(n=N, size=1, prob = pmax(rnorm(n=N, mean = (.353-.003)/2, sd = .5*(.353-(.353-.003)/2)),.001))
}

# off-art probability of death per quarter as a function of cd4 count
mortalityOFFart <- function(cd4){
  offARTDeath <- rep(NA, length(cd4))
  offARTDeath[which(cd4 < 50)] <- rbinom(n = length(which(cd4 < 50)), size = 1, prob = .049)
  offARTDeath[which(cd4 >= 50 & cd4 < 200)] <- rbinom(n = length(which(cd4 >= 50 & cd4 < 200)), size = 1, prob = .00767)
  offARTDeath[which(cd4 >= 200 & cd4 <350)] <- rbinom(n = length(which(cd4 >= 200 & cd4 <350)), size = 1, prob = .00145)
  offARTDeath[which(cd4 >= 350 & cd4 < 500)] <- rbinom(n = length(which(cd4 >= 350 & cd4 < 500)), size = 1, prob = .0008)
  offARTDeath[which(cd4 >= 500 & cd4 < 650)] <- rbinom(n = length(which(cd4 >= 500 & cd4 < 650)), size = 1, prob = .0005)
  offARTDeath[which(cd4 >= 650)] <- rbinom(n = length(which(cd4 >= 650)), size = 1, prob = .00043)
  return(offARTDeath)
}
