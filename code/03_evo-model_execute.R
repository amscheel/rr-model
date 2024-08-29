################################################################################
####                                                                        ####
#### Script 3: Script to run the evolutionary simulation model              ####
####                                                                        ####
#### ---------------------------------------------------------------------- ####
#### Content:                                                               ####
####  * 1: Setup                                                            ####
####  * 2: Set model parameters                                             ####
####  * 3: Run the simulation                                               ####
####  * 4: Code to reproduce the specific simulations in the main paper     #### 
####                                                                        ####
#### Note:                                                                  ####
####   This script relies on functions from the scripts                     ####
####   "01_evo-model_main-function.R" and "02_evo-model_helper-functions.R" ####
####   Depending on the used settings and your computing power, the         ####
####   simulations can take a very long time to run.                        ####
####                                                                        ####
################################################################################

##============================================================================##
## 1. Setup
##============================================================================##

##----------------------------------------------------------------------------##
# This script calls functions from the script "01_evo-model_main-function.R". 
# We use the package "here" to navigate to the directory storing the script.

# Install "here" unless it is already installed:
if(!require(data.table)){install.packages("data.table")}

# load model functions
source(here::here("code", "01_evo-model_main-function.R"))
##----------------------------------------------------------------------------##


##============================================================================##
## 2. Set model parameters
##============================================================================##

##----------------------------------------------------------------------------##
## 2.1 Basic model parameters: Population size, generations, prior distribution

pop_size <- 500     # population size (default: 500)
generations <- 250  # number of generations the model will loop through (default = 250)
sim_runs <-  50     # number of times each model will be run (default = 50)
mutation_sd <- 0.01 # amount of noise added by mutations during evolution (default= 0.01)
prior_dist <- "uniform" # distribution type: "uniform" or "normal" (default = "uniform")
prior_dist_m <- 0.2     # mean of prior distribution IF prior_dist = "normal" 
                        # (must be between 0 and 1)
prior_dist_sd <- 0.2    # SD of prior distribution IF prior_dist = "normal"
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## 2.2 Varying model parameters: these are the variables of interest in the model

# 2.2.1 Research rounds in each generation (m):
generation_duration <- 1 # Number of research rounds in each generation 
                         # (default = 1)

# 2.2.3 Payoffs (b): 
payoff_SR_neg <- 0  # payoff for a negative result in a standard report 
                    # (b_SR-; default = 0, can also be a vector)
payoff_SR_pos <- 1  # payoff for a positive result in a standard report 
                    # (b_SR+; default = 1, can also be a vector)
payoff_RR <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9) # payoff for a Registered Report 
                                                   # (b_RR; default = c(.1, .2, .3, 
                                                   # .4, .5, .6, .7, .8, .9), can 
                                                   # also be a single value)

# 2.2.3 Fitness function (epsilon):
epsilon <- c(0.2, 1, 5) # exponent applied to accumulated payoffs to translate 
                        # them into fitness, determining the shape of the fitness 
                        # function (values between 0 and 1 yield diminishing 
                        # returns, values above 1 yield increasing returns; 
                        # default = c(.2, 1, 5))

# 2.2.4 Survival threshold (delta):
survival_threshold <- 0  # threshold below which accumulated payoffs are translated 
                         # to 0 fitness (default = 0, i.e. no survival threshold)

# 2.2.5 Competition (gamma): 
relative_top_n <- 1  # this number determines the size of the bottleneck: 
                     # when relative_top_n < 1, only those researchers with 
                     # fitness values in the relative_top_n portion of the 
                     # population will be considered for reproduction. E.g., 
                     # when relative_top_n = 0.1, only researchers in the 
                     # top 10% can reproduce (default = 1, i.e. no competition).
##----------------------------------------------------------------------------##


##============================================================================##
## 3. Run the simulation
##    !!!CAUTION!!! 
##    Depending on settings & computing power, the simulation can 
##    take VERY long to run. Run time is affected by the number of parameter  
##    combinations that are tested. Because the simulation function is
##    looped (sorry!), the most important factors are sim_runs, generations,
##    and generation_duration. E.g., running the simulation with the default 
##    settings specified above takes about 3 minutes on a Macbook Air 2019. 
##    Each additional combination of parameter values multiplies this number, 
##    so some settings can take many hours to run.
##    It's best to start with a small number of parameter values and sim runs 
##    to get a sense of how long it takes on your machine.
##============================================================================##

##----------------------------------------------------------------------------##
# 3.1 Initialise data frame with model settings
settings_df <- settings(generation_duration = generation_duration, 
                        payoff_SR_neg = payoff_SR_neg, 
                        payoff_SR_pos = payoff_SR_pos, 
                        payoff_RR = payoff_RR,
                        epsilon = epsilon, 
                        survival_threshold = survival_threshold, 
                        relative_top_n = relative_top_n, 
                        sim_runs = sim_runs)

# 3.2 Run the simulation & store the data
#     !!CAUTION!! This can take very long!
simdata_evo <- simulate.research(pop_size = pop_size, 
                                 generations = generations, 
                                 mutation_sd = mutation_sd, 
                                 prior_dist = prior_dist,
                                 prior_dist_m = prior_dist_m,  # only if prior_dist!=uniform
                                 prior_dist_sd = prior_dist_sd, # only if prior_dist!=uniform
                                 settings = settings_df)
saveRDS(simdata_evo, "simdata_evo.RData")
##----------------------------------------------------------------------------##


##============================================================================##
## 4. Code to reproduce the specific simulations in the main paper
##    The code below reproduces the data shown in Figures 4-7 of the main 
##    paper. All of these data _could_ be produced in one go, but that would
##    take an extremely long time. We therefore break it down by Figure.
##============================================================================##

##----------------------------------------------------------------------------##
## 4.1 Basic model parameters: These are the same across all simulations

pop_size <- 500     # population size
generations <- 250  # number of generations the model will loop through
sim_runs <-  50     # number of times each model will be run 
mutation_sd <- 0.01 # amount of noise added by mutations during evolution
prior_dist <- "uniform" # distribution type: "uniform" or "normal"
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## 4.2 Non-linear fitness functions (epsilon, Fig. 4) 

# 4.2.1 Settings for the simulation shown in Fig. 4:
settings_df_epsilon <- settings(generation_duration = 1, 
                                payoff_SR_neg = 0, 
                                payoff_SR_pos = 1, 
                                payoff_RR = c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                                epsilon = c(0.2, 0.5, 1, 2, 5), 
                                survival_threshold = 0, 
                                relative_top_n = 1, 
                                sim_runs = sim_runs)

# 4.2.2 Run model to produce the data underlying Fig. 4:
simdata_evo_epsilon <- simulate.research(pop_size = pop_size, 
                                         generations = generations, 
                                         mutation_sd = mutation_sd, 
                                         prior_dist = prior_dist,
                                         settings = settings_df_epsilon)

# 4.2.3 Store the data
saveRDS(simdata_evo_epsilon, "simdata_evo_epsilon.RData")
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## 4.3 Research cycles per generation (m, Fig. 5) 

# 4.3.1 Settings for the simulation shown in Fig. 5:
settings_df_m <- settings(generation_duration = c(1, 2, 4, 8, 16, 32), 
                          payoff_SR_neg = 0, 
                          payoff_SR_pos = 1, 
                          payoff_RR = c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                          epsilon = c(0.2, 1, 5), 
                          survival_threshold = 0, 
                          relative_top_n = 1, 
                          sim_runs = sim_runs)

# 4.3.2 Run model to produce the data underlying Fig. 5:
simdata_evo_m <- simulate.research(pop_size = pop_size, 
                                   generations = generations, 
                                   mutation_sd = mutation_sd, 
                                   prior_dist = prior_dist,
                                   settings = settings_df_m)

# 4.3.3 Store the data
saveRDS(simdata_evo_m, "simdata_evo_m.RData")
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## 4.4 Survival thresholds (delta, Fig. 6) 

# 4.4.1 Settings for the simulation shown in Fig. 6:
settings_df_delta <- settings(generation_duration = c(1, 2, 4, 8, 16, 32), 
                              payoff_SR_neg = 0, 
                              payoff_SR_pos = 1, 
                              payoff_RR = c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                              epsilon = c(0.2, 1, 5), 
                              survival_threshold = c(0, .25, .5, .75), 
                              relative_top_n = 1, 
                              sim_runs = sim_runs)

# 4.4.2 Run model to produce the data underlying Fig. 6:
simdata_evo_delta <- simulate.research(pop_size = pop_size, 
                                       generations = generations, 
                                       mutation_sd = mutation_sd, 
                                       prior_dist = prior_dist,
                                       settings = settings_df_delta)

# 4.4.3 Store the data
saveRDS(simdata_evo_delta, "simdata_evo_delta.RData")
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## 4.5 Competition (gamma, Fig. 7) 

# 4.5.1 Settings for the simulation shown in Fig. 7:
settings_df_gamma <- settings(generation_duration = c(1, 2, 4, 8, 16, 32), 
                          payoff_SR_neg = 0, 
                          payoff_SR_pos = 1, 
                          payoff_RR = c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                          epsilon = c(0.2, 1, 5), 
                          survival_threshold = 0, 
                          relative_top_n = c(1, .9, .5, .1, .05, .01), 
                          sim_runs = sim_runs)

# 4.5.2 Run model to produce the data underlying Fig. 7:
simdata_evo_gamma <- simulate.research(pop_size = pop_size, 
                                   generations = generations, 
                                   mutation_sd = mutation_sd, 
                                   prior_dist = prior_dist,
                                   settings = settings_df_gamma)

# 4.5.3 Store the data
saveRDS(simdata_evo_gamma, "simdata_evo_gamma.RData")
##----------------------------------------------------------------------------##

