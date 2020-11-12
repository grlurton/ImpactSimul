
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ImpactSimul

<!-- badges: start -->

<!-- badges: end -->

ImpactSimul is an Agent Based Model aimed at helping estimate the impact
of an intervention in an HIV program, using a simple and tractable
approach. It was first developed to estimate the impact to be expected
from Solthis’ Empower II program. It is written in R.

## Installation

### Install the R package

You can install ImpactSimul from [GitHub](https://github.com/) just by
running:

``` r
# install.packages("devtools")
devtools::install_github("grlurton/ImpactSimul")
```

### Install Python

To obtain epidemiological data on a country, ImpactSim uses a Python
script that queries the UNAIDS database and gets standardized data. To
get this running, we need to set-up a python environment and download
*chromedriver*. You shouldn’t have to write any line of Python and will
use the script `unaids_scrap.py` which you can find under `/utils` in in
this repo which you can put into the `utils` directory in your project’s
folder..

We suggest installing Python using Anaconda. 1. Go to
<https://www.anaconda.com/products/individual> 2. Download the
appropriate installer for your operating system. 3.Just double-click the
downloaded file. In most cases, you can just keep all default options in
the installer.

### Load the conda environment

To install the Python libraries you’ll need for the extraction, you just
need to install the conda environment `ImpactSimulEnv.yml` which you can
find under `/utils` in this repo.

Once you have downloaded it, just open a terminal in the directory you
have downloaded it in, and run the following code.

``` bash
conda env create -f ImpactSimulEnv.yml -n ImpactSimul
```

### Install ChromeDriver

1.  Install the Chrome web browser, available on
    <https://www.google.com/chrome/>
2.  Download ChromeDriver from
    <https://chromedriver.chromium.org/downloads> . Make sure you pick
    the driver corresponding to your version of the Chrome browser.
3.  Unzip ChromeDriver, and move the file into the `utils` directory in
    your project’s folder

## Example

``` r
library(ImpactSimul)
library("readxl")
library(dplyr)
library(yaml)
library(reticulate)

download_data <- TRUE
run_life_table <- TRUE
n_sim <- 1000
time.unit <- 7
```

The last block here represents important parameters for the simulation.
`download_data`determines whether to download contextual data from
UNAIDS, `run_life_table` indicates if the script should format the life
table to include in the population simulation. `n_sim`indidcates the
number of simulations to run, `time.unit` indicates the number of days
in a step of simulation (ie: 7 corresponds to a week).

The next bit of codes then calls on the data to be formated and
extracted if the parameters have been set to do so.

``` r
###############################
##### Set up the life table ###
###############################

if(run_life_table){
  lt_male <- read_excel("data/lifeTables_SierraLeone.xlsx", sheet = "Male")
  lt_female <- read_excel("data/lifeTables_SierraLeone.xlsx", sheet = "Male")
  lt <- prep_life_tables(lt_male, lt_female, "data/ltSierraLeone.rds")
}

###############################
##### Download UNAIDS data ####
###############################

if(download_data){
  extract_unaids("ImpactSimul", "utils/unaids_scrap.py", "Sierra Leone", "utils/chromedriver", "data/unaids_estimates/")
  }
```

It is then time to load the biological parameters and the parameters
defining the different scenarios to simulate and compare, as well as the
epidemiologic data downloaded from UNAIDS.

``` r
####################################
### Loading different parameters ###
####################################

parameters_bio <- yaml.load_file("params/parameters_bio.yaml")

list_scenarios <- create_scenario_list()

#############################
### Loading external data ###
#############################

# estimates UNAIDS
data <- readRDS("data/unaids_estimates/SierraLeone/UNAIDS_estimates_SierraLeone.rds")
prop_male <- data$MeanNum[grepl(pattern = "Men aged 15 and over newly infected with HIV ", 
                                x = data$indic)]/data$MeanNum[grepl(pattern = "Adults aged 15 and over newly infected with HIV", 
                                                                    x = data$indic)]
```

All parameters are loaded and formated : TIME TO RUN A SIMULATION \!\!\!

``` r
##############################
### Running the simulation ###
##############################

param <- params(time.unit = time.unit)

init <- init(i.prev.male = 1,
             i.prev.feml = 1,
             max.inf.time = 15 * 365,
             n=param$N)


## Here, this loop takes all the scenarios found in the folder with the scenarios yaml, and runs them one ## by one, keeping the results in separate objects.
for(scenario_simulation in names(list_scenarios)){
  print(scenario_simulation)
  assign(paste0("result_", scenario_simulation), run_simulations(init, param, scenario_simulation, intervention_start = 0, prop_male, nsteps = 52 * 5, nsim = n_sim))
}
```

The results objects are finally post-processed and put into summary
objects. The results can also be translated into DALYs.

``` r
# Simulation Results for the different scenarios
res_0 <- summary_outcomes(result_parameters_baseline)
res_1 <- summary_outcomes(result_parameters_intervention)

# Combined
res <- bind_rows(
  tibble(
    Death = res_0$death$deaths,
    `New infection` = res_0$newInf$newInf,
    `Lost to follow-up` = res_0$LTFU$LTFU,
    `On treatment` = res_0$onTrt$onTrt,
    `Virally suppressed` = res_0$VlSupp$VlSupp,
    `AIDS on treatment` = res_0$AidsonART$AidsonART,
    `AIDS off treatment` = res_0$AidsoffART$AidsoffART,
    time = res_0$death$time,
    scenario = "baseline"
  ),
  tibble(
    Death = res_1$death$deaths,
    `New infection` = res_1$newInf$newInf,
    `Lost to follow-up` = res_1$LTFU$LTFU,
    `On treatment` = res_1$onTrt$onTrt,
    `Virally suppressed` = res_1$VlSupp$VlSupp,
    `AIDS on treatment` = res_1$AidsonART$AidsonART,
    `AIDS off treatment` = res_1$AidsoffART$AidsoffART,
    time = res_1$death$time,
    scenario = "intervention"
  )
)

##DALYs Estimations for the different scenarios

DALYs_0 <- calculate_DALYs(res_0, parameters_bio, param)
DALYs_1 <- calculate_DALYs(res_1, parameters_bio, param)

# Combined
daly <- bind_rows(
  tibble(
    dalys = DALYs_0$DALYs,
    scenario = "baseline"
  ),
  tibble(
    dalys = DALYs_1$DALYs,
    scenario = "intervention"
  )
)
```

Finally, making visualizations for better analysis of the results

``` r
make_pyramid(res)


result_comparison_plot(res)


DALY_comparison(daly)

## mean effect and 95% empirical confidence interval
c(mean(DALYs_0$DALYs - DALYs_1$DALYs),
  quantile(DALYs_0$DALYs - DALYs_1$DALYs, .025),
  quantile(DALYs_0$DALYs - DALYs_1$DALYs, .975)
)
```

## Indications on the method and approach

`ImpactSimul` uses Agent Based Simulation (ABM) in which individual
patients are being created, and their evolution is simulated in time
through multiple steps. This approach is flexibe, and allows to estimate
the impact of the modification of some hypothesis underlying the
parameters used to make the population evolve in time, such as, for
example, the average mortality rate or other characteristics.

![Drag Racing](img/Schema_model.png)

`ImpactSimul` uses a model in which patients can enter at any period
during the time of the simulation. He then undergoes a series of
transitions going through acute, asymptomatic and symptomatic HIV
infection. At each period, he has an occasion to enter the care system,
and to evolve through it.

Depending on the hypothesis that will be made for each of these
transitions, we will have different outcomes at the end of the program.
This is why a key aspect of running ImpactSimul and to analyze its
results lies in the hypothesis that we will be making on each
transition.

The set of parameters are supposed to be indicative of different domains
of HIV programs that can be modified by a program. This set is as
follows:

  - The *proportion of population tested each year* represents the
    ability of the health system to deploy testing at an important scale
    in the population, and to reach and get people tested.
      - *Numerator*: N patients tested each year
      - *Denominator*: Country Population
  - The *mean CD4 at ART start* represents the ability of the health
    system to identify HIV positive patients early enough in the course
    of their infection to ensure the best chance of survival.
  - The *ART starting rate* represents the ability of the health system
    to get HIV positive patients on treatment. This is a result of a
    variety of factor (availability of drugs, HR training, retention of
    HIV positive people right after test …) and can be affected by a
    variety of interventions.
      - *Numerator*: N patients starting ART on a given year
      - *Denominator*: N patients tested HIV+ on a given year
  - The *proportion of fully adherent patients* is an indication of the
    quality of care and of the ability of health professionals to
    properly convey the importance of adherence to patients, and to
    foster trust and adherence among them. It is of course a strong
    predictor of viral suppression.
      - *Numerator*: N patients with adherence over 90%
      - *Denominator*: N patients for whom adherence has been assessed
  - Finally, the *proportion of ART patients LTFU each year* is an
    indicator of the ability of the HIV care system to retain patients
    in care in the long run. This is essential to ensure long term
    success of HIV programs.
      - *Numerator*: N patients LTFU on a given year
      - *Denominator*: N patients on treatment on a given year

The different parameters can be specified in yaml files that can be
stored under `params/scenarios` in your project (or another directory to
be stipulated). An example file is provided [on this
repo](/params/scenario).
