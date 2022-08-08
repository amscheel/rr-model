##============================================================================##
## This script runs the simulation that only focusses on evolved submission
## thresholds as the outcome
##============================================================================##

##----------------------------------------------------------------------------##
## Explanation of model parameters
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## 1. Basic model parameters: Population size & number of generations

# pop_size:     population size (default: 500)
# generations:  number of generations the model will loop through (default = 250)
# sim_runs:     number of times each model will be run (default = 50)
# mutation_sd:  amount of noise added by mutations during evolution (default= 0.01)

##----------------------------------------------------------------------------##

## 2. Varying model parameters: these are the variables of interest

## 2.1 Parameters of the prior distribution:
# prior_dist:     distribution type: "uniform" or "normal" (default = "uniform")
# prior_dist_m:   mean of prior distribution IF prior_dist = "normal" 
#                 (must be between 0 and 1; default = 0.2)
# prior_dist_sd:  SD of prior distribution IF prior_dist = "normal" (default = 0.2)

# 2.2 Research rounds in each generation (m):
# generation_duration: Number of research rounds in each generation (default = 1)

# 2.3 Payoffs (b): 
# payoff_SR_neg:  payoff for a negative result in a standard report 
#                 (b_SR-; default = 0)
# payoff_SR_pos:  payoff for a positive result in a standard report 
#                 (b_SR+; default = 1)
# payoff_RR:      payoff for a Registered Report (b_RR; 
#                 default = c(.1, .2, .3, .4, .5, .6, .7, .8, .9))

# 2.4 Fitness function (epsilon):
# epsilon:        exponent applied to accumulated payoffs to translate them into
#                 fitness, determining the shape of the fitness function (values
#                 between 0 and 1 yield diminishing returns, values above 1 yield
#                 increasing returns; default = c(.2, .5, 1, 2, 5))

# 2.5 Survival threshold (delta):
# survival_threshold: threshold below which accumulated payoffs are translated 
#                     to 0 fitness (default = 0, i.e. no survival threshold)

# 2.6 Competition (gamma): 
# relative_top_n: this number determines the size of the bottleneck: 
#                 when relative_top_n < 1, only those researchers with fitness
#                 values in the relative_top_n portion of the population will be
#                 considered for reproduction. E.g., when relative_top_n = 0.1,
#                 only researchers in the top 10% can reproduce (default = 1, i.e.
#                 no competition).
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Prepare the simulation: 

# Function to generate a data frame with all combinations of model parameters:
settings <- function(generation_duration = 1, 
                     payoff_SR_neg = 0, 
                     payoff_SR_pos = 1, 
                     payoff_RR = c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                     epsilon = c(.2, .5, 1, 2, 5), 
                     survival_threshold = 0, 
                     relative_top_n = 1, 
                     sim_runs = 50){
  
  # Create a data frame with all combinations of the varying model 
  # parameters specified above:
  settings_df <- expand.grid(
    generation_duration = generation_duration,
    payoff_SR_neg = payoff_SR_neg, 
    payoff_SR_pos = payoff_SR_pos, 
    payoff_RR = payoff_RR, 
    epsilon = epsilon, 
    survival_threshold = survival_threshold, 
    relative_top_n = relative_top_n, 
    run_id = c(1:sim_runs))
  
  return(settings_df) # output the data frame
}
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Here comes the simulation function:

simulate.research <- function(pop_size = 500, 
                              generations = 250, 
                              mutation_sd = 0.01, 
                              prior_dist = "uniform",
                              prior_dist_m = 0.2,
                              prior_dist_sd = 0.2,
                              settings){
  
  # Model setup: initialise a list in with the simulation results will be stored
  sim <- list()
  
  for (i in 1:nrow(settings)) {
    
    # initialise a list in which the results of each run will be stored  
    # (this has to be emptied before each new combination of settings is run)
    run.output <- list() 
    
    ## Turn each parameter setting from the current combination of settings
    ## (back) into a variable:
    
    # Set generation duration
    generation_duration <- settings$generation_duration[i]
    
    # Set the payoffs for RRs and standard reports:
    payoff_SR_neg <- settings$payoff_SR_neg[i] # payoff for neg. result in standard report
    payoff_SR_pos <- settings$payoff_SR_pos[i]  # payoff for pos. result in standard report
    payoff_RR <- settings$payoff_RR[i] # payoff for Registered Report
    
    # Set parameters of the fitness function:
    epsilon <- settings$epsilon[i]  # exponent of x to influence the shape
    survival_threshold <- settings$survival_threshold[i] # threshold below which all payoffs have 0 fitness
    
    # Set competition level:
    relative_top_n <- pop_size * settings$relative_top_n[i]
    
    # Each run starts with a random allocation of submission thresholds
    submission_threshold <- runif(pop_size, 0,1)
    
    for (k in 1:generations) {
      
      # initialise the payoff vector: before conducting any research, 
      # everybody starts with 0 payoff
      payoff <- rep(0, pop_size)
      
      ## 1. Research phase:
      for (l in 1:generation_duration) {
        
        # 1.1 We use research.fun to "do the research":
        #     Assign priors, compare with submission thresholds, 
        #     decide who submits RRs and who doesn't, 
        #     and calculate the resulting payoffs
        payoff <- payoff + research.fun.unif(n = pop_size,
                                             submission.threshold = 
                                               submission_threshold,
                                             payoff.SR.neg = payoff_SR_neg,
                                             payoff.SR.pos = payoff_SR_pos,
                                             payoff.RR = payoff_RR)
      }
      
      ## 2. Evolution phase:
      
      # 2.1 Use fitness.fun to calculate researchers' fitness from their payoffs
      fitness <- fitness.fun(x = payoff, epsilon = epsilon,
                             survival.threshold = survival_threshold)
      
      # 2.2 Selection: 
      #     We use selection.fun to calculate whose traits are passed on to the
      #     next generation
      selection <- selection.fun(n = pop_size, top.n = relative_top_n,
                                 fitness = fitness, 
                                 submission.threshold.parent =
                                   submission_threshold)
      
      # 2.3 Mutation: 
      #     We use mutation.fun to generate the evolved submission thresholds
      #     of the new generation
      submission_threshold <- mutation.fun(n = pop_size,
                                           submission.threshold.selected = selection,
                                           mutation.sd = mutation_sd)
    }
    
    # Store the results of this run in the run.output list (i.e., the index 
    # of the run and the submission thresholds of the final generation)
    
    # Store the results of all sim_runs of the current combination of parameter
    # settings in the sim list
    sim[[i]] <- list(generation_duration = generation_duration,
                     payoff_SR_neg = payoff_SR_neg, 
                     payoff_SR_pos = settings$payoff_SR_pos[i],
                     payoff_RR = settings$payoff_RR[i], 
                     epsilon = epsilon, 
                     survival_threshold = 
                       settings$survival_threshold[i],
                     relative_top_n = settings$relative_top_n[i],
                     run_id = settings$run_id[i],
                     submission_threshold = submission_threshold)
  }
  
  # turn the model list into a data frame in long format:
  return(data.table::rbindlist(sim)) # unlist "model" with a remaining nested column (output)
}
##----------------------------------------------------------------------------##


## Run the simulation:

# load model functions
source("model_functions_outcomeST.R") 

# initialise settings data frame
settings_df <- settings()

time1 <- Sys.time() # create time stamp to calculate run time for model
# Simulate!
simdata <- simulate.research(settings = settings_df)
time2 <- Sys.time() # create time stamp to calculate run time for model
time2-time1 # calculate model run time

# Run delta = 8
settings_df_delta8 <- settings(generation_duration = c(8, 16, 32), 
                        epsilon = c(0.2, 1, 5), 
                        survival_threshold = 8)
time1 <- Sys.time() # create time stamp to calculate run time for model
# Simulate!
simdata_delta8 <- simulate.research(settings = settings_df_delta8)
time2 <- Sys.time() # create time stamp to calculate run time for model
time2-time1 # calculate model run time

# Run delta = 4
settings_df_delta4 <- settings(generation_duration = c(4, 8, 16, 32), 
                               epsilon = c(0.2, 1, 5), 
                               survival_threshold = 4)
time1 <- Sys.time() # create time stamp to calculate run time for model
# Simulate!
simdata_delta4 <- simulate.research(settings = settings_df_delta4)
time2 <- Sys.time() # create time stamp to calculate run time for model
time2-time1 # calculate model run time

# Run delta = 2
settings_df_delta2 <- settings(generation_duration = c(2, 4, 8, 16, 32), 
                               epsilon = c(0.2, 1, 5), 
                               survival_threshold = 2)
time1 <- Sys.time() # create time stamp to calculate run time for model
# Simulate!
simdata_delta2 <- simulate.research(settings = settings_df_delta2)
time2 <- Sys.time() # create time stamp to calculate run time for model
time2-time1 # calculate model run time

##----------------------------------------------------------------------------##
# saveRDS(simdata, "")
# saveRDS(settings_df, "")
##----------------------------------------------------------------------------##
