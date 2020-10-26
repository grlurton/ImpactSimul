#' Delete the attribute of an individual
#' \code{deleteAttr} delete the attribute of an individual that exits the simulation.
#'
#' @details
#' In the default version of the simulation,
#' individuals exit the simulation if `active == 0`, i.e. if they die.
#'
#' @param attrList list of attributes of individuals in the simulation
#' @param ids Numeric vector identifying the individuals to delete from the simulation
#'
#' @return attrList, list of attributes of individuals remaining in the simulation
deleteAttr <- function(attrList, ids) {
  
  if (class(attrList) != "list") {
    stop("attrList must be a list", call. = FALSE)
  }
  if (length(unique(sapply(attrList, length))) != 1) {
    stop("attrList must be rectangular (same number of obs per element)")
  }
  
  if (length(ids) > 0) {
    attrList <- lapply(attrList, function(x) x[-ids])
  }
  return(attrList)
}

nbsdtosize <- function(mu, sd) {
  mu ^ 2 / (sd ^ 2 - mu)
}

#' \code{proba_to_rate} transforms annual probability of a given event
#' into a rate of occurrence of this event for the specified time unit \code{t}
#'
#' @details
#' This function assumes a constant rate of occurrence of an event over a given time period
#'
#' @param p the annual probability (must be between 0 and 1)
#' @param t the unit of time in which the probability was expressed (by default, a year)
#'
#' @return `1-exp(-r)` the rate of occurence of the event per \code{time.unit}
proba_to_rate <- function(p, t = 365.25){
  r <- -log(1-p)/t
  return(1-exp(-r))
}
