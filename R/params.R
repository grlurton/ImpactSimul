#' @title Parameters for agent-based model simulation
#'
#' @description Sets the epidemic parameters for stochastic agent-based model
#'              simulated with \code{\link{run_abm}}.
#'
#' @details: Load the different parameters used later on to simulate the different components of the agent-based model
##              (1) Load the yaml files containing the biological parameters and the parameters that depend on the intervention
##              (2) Parse the biological and intervention parameters, along with the life table
#' @param size_active_line size observed active line of HIV patients - used to deduce the size of the catchment area
#' @section Biological parameters:
#' Parameters that are supposed to remain the same across the different scenarios, i.e. parameters
#' over which the intervention has no impact. Parameters' values were found in the literature.
#' They can be modified based on new evidence, or measurements.
#' @param age.mean mean age cohort patients
#' @param age.median median age cohort patients
#' @param base.cd4 cd4 count at infection
#' @param di.cd4.aids cd4 count AIDS
#' @param di.cd4.rate daily death rate AIDS
#' @param base.tprob.male baseline probability transmission male
#' @param base.tprob.feml baseline probability transmission female
#' @param act.rate.early daily rate sexual act in early phases of infection
#' @param act.rate.late daily rate sexual act in later stages of infection
#' @param act.rate.cd4 threshold cd4 count value at which patients 
#' switch from early to later stages
#' @param tx.cd4.recrat.feml
#' daily increase in cd4 count after treatment initiation for females"
#' @param tx.cd4.recrat.male daily increase in cd4 count after treatment initiation for males
#' @param tx.cd4.decrat.feml daily decrease in cd4 count after treatment interruption for females
#' @param tx.cd4.decrat.male daily decrease in cd4 count after treatment interruption for males
#' @param tx.vlsupp.time time in weeks from treatment initiation to viral suppression
#' @param tx.vlsupp.level 
#' @param vl.acute.topeak time in weeks to peak viremia during acute infection
#' @param vl.acute.toset time in weeks to viral set point following peak viremia
#' @param vl.acute.peak Log 10 viral load at acute peak
#' @param vl.acute.setpoint Log 10 viral load at set point
#' @param vl.aidsmax Maximum log 10 viral load during AIDS
#' 
#' @section Intervention parameters:
#' Parameters that are impacted by the simulation, and whose value changes from one scenario
#' to the other.
#' @param hiv.coverage National annual testing coverage
#' @param tx.cd4.detect Minimum cd4 count after which patients seek care at health facilities
#' @param tx.init.cd4.mean Mean cd4 count at ART initiation
#' @param tx.init.cd4.sd Standard deviation cd4 count at ART initiation
#' @param tx.coverage ART coverage 
#' @param tx.adhere.full proportion of fully adherent patients
#' @param tx.adhere.part Proportion of patients, among those that are not fully adherent,
#'  that take their treatment from time to time
#' @param lfu.prop Annual proportion of patients being lost to follow-up
#' 
#' @seealso Use @param time.unit any time step [default = 7], and @param list_scenario
#'          specified in a `yaml`. Run the parameterized model with \code{\link{run_abm}}

#' @export create_scenario_list
create_scenario_list <- function(scenarios_folder="params/scenarios/"){
  list_scenarios_files <- list.files(scenarios_folder)
  scenarios <- list()
  for(scenario in list_scenarios_files){
    scenario_name <- gsub(".yaml", "", scenario)
    scenarios[[scenario_name]] <- yaml.load_file(paste0(scenarios_folder,scenario))
  }
  return(scenarios)
  }
#


#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom stats rlnorm
#' @export params
params <- function(time.unit = 7,
                  scenarios = list_scenarios, ...) {
  
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
  
  # Parse biological parameters
  p$age.median <- parameters_bio$params_bio$age.median$value
  p$age.mean <- parameters_bio$params_bio$age.mean$value
  age <- rlnorm(10000, log(p$age.median-15), log((p$age.mean-p$age.median)/2))+15
  p$age <- age[age<55]
  
  p$base.cd4 <- parameters_bio$params_bio$base.cd4$value
  
  p$base.tprob <- c(parameters_bio$params_bio$base.tprob.male$value,parameters_bio$params_bio$base.tprob.feml$value)
  p$act.rate.early <- parameters_bio$params_bio$act.rate.early$value
  p$act.rate.late <- parameters_bio$params_bio$act.rate.early$value
  p$act.rate.cd4 <- parameters_bio$params_bio$act.rate.cd4$value
  
  p$tx.cd4.recrat.feml <- parameters_bio$params_bio$tx.cd4.recrat.feml$value
  p$tx.cd4.recrat.male <- parameters_bio$params_bio$tx.cd4.recrat.male$value
  p$tx.cd4.decrat.feml <- parameters_bio$params_bio$tx.cd4.decrat.feml$value
  p$tx.cd4.decrat.male <- parameters_bio$params_bio$tx.cd4.decrat.male$value
  
  p$tx.vlsupp.time <-parameters_bio$params_bio$tx.vlsupp.time$value
  p$tx.vlsupp.level <-parameters_bio$params_bio$tx.vlsupp.level$value
  
  p$vl.acute.topeak <- parameters_bio$params_bio$vl.acute.topeak$value
  p$vl.acute.toset <- parameters_bio$params_bio$vl.acute.toset$value
  p$vl.acute.peak <- parameters_bio$params_bio$vl.acute.peak$value
  p$vl.acute.setpoint <- parameters_bio$params_bio$vl.acute.setpoint$value
  p$vl.aidsmax <- parameters_bio$params_bio$vl.aidsmax$value
  
  p$di.cd4.aids <- parameters_bio$params_bio$di.cd4.aids$value 
  p$di.cd4.rate <- parameters_bio$params_bio$di.cd4.rate$value
  
  # Get the disability weights
  p$disability.w.hiv <- parameters_bio$Disability_weights$hiv$value
  p$disability.w.aids <- parameters_bio$Disability_weights$aids_no_art
  p$disability.w.aids.art <- parameters_bio$Disability_weights$aids_art
  # Parse intervention parameters
  for(scen_name in names(scenarios)){
    scenario <- scenarios[[scen_name]]

    p[[scen_name]]$hiv.coverage <- scenario$params_impact$hiv.coverage$value
    p[[scen_name]]$hiv.test.rate <- proba_to_rate(scenario$params_impact$hiv.coverage$value) * time.unit
    
    p[[scen_name]]$tx.cd4.detect <- scenario$params_impact$tx.cd4.detect$value
    p[[scen_name]]$tx.init.cd4.mean <- scenario$params_impact$tx.init.cd4.mean$value
    p[[scen_name]]$tx.init.cd4.sd <- scenario$params_impact$tx.init.cd4.sd$value
    
    p[[scen_name]]$tx.coverage <- scenario$params_impact$tx.coverage$value
    
    p[[scen_name]]$tx.adhere.full <- scenario$params_impact$tx.adhere.full$value
    p[[scen_name]]$tx.adhere.part <- scenario$params_impact$tx.adhere.part$value
    
    p[[scen_name]]$lfu.prop <- scenario$params_impact$lfu.prop$value
    p[[scen_name]]$tx.lfu <- proba_to_rate(scenario$params_impact$lfu.prop$value) * time.unit
  }
  
  ## Calculate catchment area size from size of active line
  name_baseline <- names(scenarios)[1]
  scen_baseline_N <- scenarios[[name_baseline]]
  p$N <- scen_baseline_N$size_active_line*(1/p[[name_baseline]]$hiv.coverage)*(1/p[[name_baseline]]$tx.coverage)
  
  ## Calculate age and sex-specific death rates for a given country-specific life table
  ds.rates <- lt
  ds.rates <- mutate(ds.rates,
                     reps = lead(Age)-Age)
  ds.rates$mrate <- ds.rates$mrate / 365


  ds.rates$reps[ds.rates$Age == 100] <- 1
  mrateM <- rep(ds.rates$mrate[ds.rates$male == 1], ds.rates$reps[ds.rates$male == 1])
  mrateF <- rep(ds.rates$mrate[ds.rates$male == 0], ds.rates$reps[ds.rates$male == 0])
  mrate <- c(mrateF, mrateM)
  male <- c(rep(1, length(mrateM)),rep(0, length(mrateF)))
  age <- rep(0:100,2)
  ds.rates <- data.frame(male = male, age, mrate = mrate)
  p$ds.rates <- ds.rates
  
  ## Time unit scaling
  if (time.unit > 1) {
    
    ## Rates multiplied by time unit
    p$act.rate.early <- p$act.rate.early * time.unit
    p$act.rate.late <- p$act.rate.late * time.unit
    p$ds.rates$mrate <- ifelse(p$ds.rates$mrate < 1,
                               p$ds.rates$mrate * time.unit,
                               p$ds.rates$mrate)
    

    p$tx.cd4.recrat.feml <- p$tx.cd4.recrat.feml * time.unit
    p$tx.cd4.recrat.male <- p$tx.cd4.recrat.male * time.unit
    p$tx.cd4.decrat.feml <- p$tx.cd4.decrat.feml * time.unit
    p$tx.cd4.decrat.male <- p$tx.cd4.decrat.male * time.unit

    
    ## Intervals divided by time unit
    p$vl.acute.topeak <- p$vl.acute.topeak / time.unit
    p$vl.acute.toset <- p$vl.acute.toset / time.unit
    
    p$tx.vlsupp.time <- p$tx.vlsupp.time / time.unit
    
  }
  
  
  class(p) <- "param"
  return(p)
}

