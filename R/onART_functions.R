
# Quarterly probability of viral load suppression after treatment initiation as a function of CD4 at treatment initiation
VLsupp <- function(cd4init){
  VLsupp <- rep(NA, length(cd4init))
  VLsupp[which(cd4init < 50)] <- rbinom(n = length(which(cd4init < 50)), size = 1, prob = .525) # we get this from the 6-month probability by translating to rate and getting the 3-month probability
  probaVLsupp[which(cd4init >= 50 & cd4init < 200)] <- rbinom(n = length(which(cd4init >= 50 & cd4init < 200)), size = 1, prob = .542)
  probaVLsupp[which(cd4init >= 200)] <- rbinom(n = length(which(cd4init >= 200)), size = 1, prob = .6)
  return(VLsupp)
}

# probability of treatment failure 1 year after treatment initiation as a function of cd4init cont at ART initiation
treatmentFailure <- function(cd4init){
  treatmentFailure <- rep(NA, length(cd4init))
  treatmentFailure[which(cd4init < 100)] <- rbinom(n = length(which(cd4init < 100)), size = 1, prob = .05)
  treatmentFailure[which(cd4init >= 100 & cd4init < 200)] <- rbinom(n = length(which(cd4init >= 100 & cd4init < 200)), size = 1, prob = .0245)
  treatmentFailure[which(cd4init >= 200 & cd4init < 300)] <- rbinom(n = length(which(cd4init >= 200 & cd4init < 300)), size = 1, prob = .0183)
  treatmentFailure[which(cd4init >= 300 & cd4init < 400)] <- rbinom(n = length(which(cd4init >= 300 & cd4init < 400)), size = 1, prob = .0146)
  treatmentFailure[which(cd4init >= 400)] <- rbinom(n = length(which(cd4init >= 400)), size = 1, prob = .0145)
  return(treatmentFailure)
}

# probability of treatment failure 1 year after initiation of 2nd line
treatmentFailure2 <- function(cd4init){
  treatmentFailure2 <- rep(NA, length(cd4init))
  treatmentFailure2[which(cd4init < 100)] <- rbinom(n = length(which(cd4init < 100)), size = 1, prob = 1.18*.05)
  treatmentFailure2[which(cd4init >= 100 & cd4init < 200)] <- rbinom(n = length(which(cd4init >= 100 & cd4init < 200)), size = 1, prob = 1.18*.0245)
  treatmentFailure2[which(cd4init >= 200 & cd4init < 300)] <- rbinom(n = length(which(cd4init >= 200 & cd4init < 300)), size = 1, prob = 1.18*.0183)
  treatmentFailure2[which(cd4init >= 300 & cd4init < 400)] <- rbinom(n = length(which(cd4init >= 300 & cd4init < 400)), size = 1, prob = 1.18*.0146)
  treatmentFailure2[which(cd4init >= 400)] <- rbinom(n = length(which(cd4init >= 400)), size = 1, prob = 1.18*.0145)
  return(treatmentFailure2)
}


# Quarterly increase in CD4 count in quarters following HIV viral load suppression
increaseCD4 <- function(Q,cd4){
  increasedCD4 <- cd4
  increasedCD4[which(Qvlsupp < Q & Q <= Qvlsupp + 2)] <- cd4[which(Qvlsupp < Q & Q <= Qvlsupp + 2)] + 68
  increasedCD4[which(Qvlsupp + 2 < Q & Q <= Qvlsupp + 12)] <- cd4[which(Qvlsupp + 2 < Q & Q <= Qvlsupp + 12)] + 40
  return(increasedCD4)
}


# on-art probability of death per quarter as a function of cd4 count, age and presence of AIDS symptoms 
mortalityOnart <- function(cd4, age, OI){
  # OI == 1
  onARTDeath <- rep(NA, length(cd4))
  onARTDeath[which(cd4 < 25 & age >= 50 & OI == 1)] <- rbinom(n = length(which(cd4 < 25 & age >= 50 & OI == 1)), size = 1, prob = .0177)
  onARTDeath[which(cd4 >= 350 & age >= 50 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 50 & OI == 1)), size = 1, prob = .0064)
  onARTDeath[which(cd4 >= 25 & cd4 < 350 & age >= 50 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 50 & OI == 1)), size = 1, prob = .012)
  
  onARTDeath[which(cd4 < 25 & age >= 40 & age < 50 & OI == 1)] <- rbinom(n = length(which(cd4 < 25 & age >= 40 & age < 50 & OI == 1)), size = 1, prob = .0093)
  onARTDeath[which(cd4 >= 350 & age >= 40 & age < 50 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 40 & age < 50 & OI == 1)), size = 1, prob = .0032)
  onARTDeath[which(cd4 >= 25 & cd4 < 350 & age >= 40 & age < 50 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 40 & age < 50 & OI == 1)), size = 1, prob = .0062)
  
  onARTDeath[which(cd4 < 25 & age >= 30 & age < 40 & OI == 1)] <- rbinom(n = length(which(cd4 < 25 & age >= 30 & age < 40 & OI == 1)), size = 1, prob = .0069)
  onARTDeath[which(cd4 >= 350 & age >= 30 & age < 40 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 30 & age < 40 & OI == 1)), size = 1, prob = .0025)
  onARTDeath[which(cd4 >= 25 & cd4 < 350 & age >= 30 & age < 40 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 30 & age < 40 & OI == 1)), size = 1, prob = .0047)
  
  onARTDeath[which(cd4 < 25 & age < 30 & OI == 1)] <- rbinom(n = length(which(cd4 < 25 & age >= 20 & age < 30 & OI == 1)), size = 1, prob = .0053)
  onARTDeath[which(cd4 >= 350 &  age < 30 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 20 & age < 30 & OI == 1)), size = 1, prob = .0019)
  onARTDeath[which(cd4 >= 25 & cd4 < 350 & age < 30 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 20 & age < 30 & OI == 1)), size = 1, prob = .0036)
  
  # OI == 0
  
  onARTDeath[which(cd4 < 25 & age >= 50 & OI == 1)] <- rbinom(n = length(which(cd4 < 25 & age >= 50 & OI == 1)), size = 1, prob = .0081)
  onARTDeath[which(cd4 >= 350 & age >= 50 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 50 & OI == 1)), size = 1, prob = .0029)
  onARTDeath[which(cd4 >= 25 & cd4 < 350 & age >= 50 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 50 & OI == 1)), size = 1, prob = .0055)
  
  onARTDeath[which(cd4 < 25 & age >= 40 & age < 50 & OI == 1)] <- rbinom(n = length(which(cd4 < 25 & age >= 40 & age < 50 & OI == 1)), size = 1, prob = .0043)
  onARTDeath[which(cd4 >= 350 & age >= 40 & age < 50 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 40 & age < 50 & OI == 1)), size = 1, prob = .0015)
  onARTDeath[which(cd4 >= 25 & cd4 < 350 & age >= 40 & age < 50 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 40 & age < 50 & OI == 1)), size = 1, prob = .0029)
  
  onARTDeath[which(cd4 < 25 & age >= 30 & age < 40 & OI == 1)] <- rbinom(n = length(which(cd4 < 25 & age >= 30 & age < 40 & OI == 1)), size = 1, prob = .0032)
  onARTDeath[which(cd4 >= 350 & age >= 30 & age < 40 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 30 & age < 40 & OI == 1)), size = 1, prob = .0012)
  onARTDeath[which(cd4 >= 25 & cd4 < 350 & age >= 30 & age < 40 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 30 & age < 40 & OI == 1)), size = 1, prob = .0022)
  
  onARTDeath[which(cd4 < 25 & age < 30 & OI == 1)] <- rbinom(n = length(which(cd4 < 25 & age >= 20 & age < 30 & OI == 1)), size = 1, prob = .0026)
  onARTDeath[which(cd4 >= 350 &  age < 30 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 20 & age < 30 & OI == 1)), size = 1, prob = .0017)
  onARTDeath[which(cd4 >= 25 & cd4 < 350 & age < 30 & OI == 1)] <- rbinom(n = length(which(cd4 >= 350 & age >= 20 & age < 30 & OI == 1)), size = 1, prob = .0009)
  
  return(onARTDeath)
}

