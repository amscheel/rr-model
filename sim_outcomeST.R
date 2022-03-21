##============================================================================##
## This script runs the simulation that only focusses on evolved submission
## thresholds as the outcome
##============================================================================##

##----------------------------------------------------------------------------##
## Basic setup: load model functions and required packages
source("model_functions_outcomeST.R") # load model functions
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Fixed model parameters: These are kept constant for all simulations

# Population size & number of generations
pop_size <-500 # population size
generations <- 250 # set number of generations the model will loop through
runs <- 50 # number of times each model will be run
mutation_sd <- .01 # amount of noise added by mutations during evolution

# Parameters of the prior distribution:
prior_dist <- "uniform" # distribution type: uniform or normal
prior_dist_m <- 0.2 # mean of the prior distribution IF prior_dist = "normal"
prior_dist_sd <- 0.2 # sd of the prior distribution IF prior_dist = "normal"

##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Varying model parameters: these are the variables of interest

# 1. Number of research rounds in each generation:
generation_duration <- 1 

# 2. Payoffs: 
#   We define the payoff researchers get for their studies as 
#   payoff = reward - cost.
# Set the payoffs for RRs and standard reports:
payoff_SR_neg <- 0  # payoff for a negative result in a standard report
relative_payoff_SR_pos <- 1  # payoff for a positive result in a standard report, relative to negative payoff (i.e., this determines the variance): payoff_SR_pos = payoff_SR_neg + relative_payoff_SR_pos
relative_payoff_RR <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9) # payoff for a Registered Report, relative to SR payoffs (payoff_SR_pos + payoff_SR_neg) * relative_payoff_RR

# 3. Parameters of the fitness function:
e <- c(.2, .5, 1, 2, 5)  # exponent of x to influence the shape
relative_survival_threshold <- 0  # threshold below which all payoffs have 0 fitness, relative to SR payoff and generation duration:
# survival_threshold = relative_survival_threshold * generation_duration * (payoff_SR_neg + payoff_SR_pos) 
# I.e., 0 means no surivial threshold, values below 0.5 mean the theshold is less than half of the expected payoff from SRs (i.e., the average SR payoff) per round, and values above 1 mean the threshold is more than half of the expected payoff from RRs.

# 4. Competition: 
relative_top_n <- 1 # this number determines the size of the bottleneck: when top_n < pop_size, only those researchers with the n top fitness values will be selected for reproduction.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Prepare the simulation: 

# Create a dataframe with all combinations of the varying model 
# parameters specified above:
settings_df <- expand.grid(generation_duration = generation_duration,
                           payoff_SR_neg = payoff_SR_neg, 
                           relative_payoff_SR_pos = relative_payoff_SR_pos, 
                           relative_payoff_RR = relative_payoff_RR, 
                           e = e, relative_survival_threshold = 
                             relative_survival_threshold, 
                           relative_top_n = relative_top_n, run = c(1:runs))

# Model setup: initialise a list in with the simulation results will be stored
sim <- list()
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Here comes the simulation:
time1 <- Sys.time()
for (i in 1:nrow(settings_df)) {
  
  # initialise a list in which the results of each run will be stored  
  # (this has to be emptied before each new combination of settings is run)
  run.output <- list() 
  
  ## Turn each parameter setting from the current combination of settings
  ## (back) into a variable:
  
  # Set generation duration
  generation_duration <- settings_df$generation_duration[i]
  
  # Set the payoffs for RRs and standard reports:
  payoff_SR_neg <- settings_df$payoff_SR_neg[i] # payoff for neg. result in standard report
  payoff_SR_pos <- payoff_SR_neg + settings_df$relative_payoff_SR_pos[i]  # payoff for pos. result in standard report
  payoff_RR <- settings_df$relative_payoff_RR[i] *
    (payoff_SR_pos + payoff_SR_neg) # payoff for Registered Report
  # payoff_RR <- settings_df$relative_payoff_RR[i]
  
  # Set parameters of the fitness function:
  e <- settings_df$e[i]  # exponent of x to influence the shape
  survival_threshold <- settings_df$relative_survival_threshold[i] * 
    generation_duration * (payoff_SR_neg + payoff_SR_pos) # threshold below which all payoffs have 0 fitness
  
  # Set competition level:
  top_n <- pop_size * settings_df$relative_top_n[i]
  
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
      fitness <- fitness.fun(x = payoff, e = e,
                             survival.threshold = survival_threshold)
      
      # 2.2 Selection: 
      #     We use selection.fun to calculate whose traits are passed on to the
      #     next generation
      selection <- selection.fun(n = pop_size, top.n = top_n,
                                 fitness = fitness, 
                                 submission.threshold.parent =
                                   submission_threshold)
      
      # 2.3 Mutation: 
      #     We use mutation.fun to generate the evolved submission thresholds
      #     of the new generation
      submission_threshold <- mutation.fun(n = pop_size,
                                                 submission.threshold.selected =
                                                   selection,
                                                 mutation.sd = mutation_sd)
    }
    
    # Store the results of this run in the run.output list (i.e., the index 
    # of the run and the submission thresholds of the final generation)
  
  # Store the results of all runs of the current combination of parameter
  # settings in the sim list
  sim[[i]] <- list(generation_duration = generation_duration,
                   payoff_SR_neg = payoff_SR_neg, 
                   relative_payoff_SR_pos = settings_df$relative_payoff_SR_pos[i],
                   relative_payoff_RR = settings_df$relative_payoff_RR[i], 
                   e = e, 
                   relative_survival_threshold = 
                     settings_df$relative_survival_threshold[i],
                   relative_top_n = settings_df$relative_top_n[i],
                   run = settings_df$run[i],
                   submission_threshold = submission_threshold)
}

# turn the model list into a data frame in long format:
simdata <- data.table::rbindlist(sim) # unlist "model" with a remaining nested column (output)
time2 <- Sys.time()
time2-time1


##----------------------------------------------------------------------------##
# saveRDS(simdata, "")
# saveRDS(settings_df, "")
##----------------------------------------------------------------------------##
