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
pop_size <-200 # population size
generations <- 500 # set number of generations the model will loop through
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
generation_duration <- c(1,2) 

# 2. Payoffs: 
#   We define the payoff researchers get for their studies as 
#   payoff = reward - cost.
# Set the payoffs for RRs and standard reports:
payoff_SR_neg <- 0  # payoff for a negative result in a standard report
relative_payoff_SR_pos <- 1  # payoff for a positive result in a standard report, relative to negative payoff (i.e., this determines the variance): payoff_SR_pos = payoff_SR_neg + relative_payoff_SR_pos
relative_payoff_RR <- c(.5, .8) # payoff for a Registered Report, relative to SR payoffs (payoff_SR_pos + payoff_SR_neg) * relative_payoff_RR

# 3. Parameters of the fitness function:
e <- c(1, 2)  # exponent of x to influence the shape
relative_survival_threshold <- 0  # threshold below which all payoffs have 0 fitness, relative to SR payoff and generation duration:
# survival_threshold = relative_survival_threshold * generation_duration * (payoff_SR_neg + payoff_SR_pos) 
# I.e., 0 means no surivial threshold, values below 0.5 mean the theshold is less than half of the expected payoff from SRs (i.e., the average SR payoff) per round, and values above 1 mean the threshold is more than half of the expected payoff from RRs.

# 4. Competition: 
relative_top_n <- c(0.1, 0.9) # this number determines the size of the bottleneck: when top_n < pop_size, only those researchers with the n top fitness values will be selected for reproduction.
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
                           relative_top_n = relative_top_n)

# Model setup: initialise a list in with the simulation results will be stored

#------------------------------------------------------------------------------#
df_loop <- as.data.frame(matrix(ncol = ncol(settings_df) + runs, 
                                nrow = pop_size))
names(df_loop) <- c(colnames(settings_df), as.character(1:runs))
sim_df <- df_loop[1,]
#------------------------------------------------------------------------------#
## Here comes the simulation:

for (i in 1:nrow(settings_df)) {
  
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
  
  
  run_df <- df_loop
  run_df$generation_duration <- generation_duration
  run_df$payoff_SR_neg <- payoff_SR_neg
  run_df$relative_payoff_SR_pos <- settings_df$relative_payoff_SR_pos[i]
  run_df$relative_payoff_RR <- settings_df$relative_payoff_RR[i]
  run_df$e <- e
  run_df$relative_survival_threshold <- settings_df$relative_survival_threshold[i]
  run_df$relative_top_n <- settings_df$relative_top_n[i]
  
  # Start simulating:
  for (j in 1:runs) {
    
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
        new_payoff <- research.fun(n = pop_size, prior_dist = prior_dist,
                                   prior_dist_m = prior_dist_m,
                                   prior_dist_sd = prior_dist_sd,
                                   submission_threshold = submission_threshold,
                                   payoff_SR_neg = payoff_SR_neg,
                                   payoff_SR_pos = payoff_SR_pos,
                                   payoff_RR = payoff_RR)
        
        # 1.2   Update payoff: 
        #       add the payoffs of this round to the previous amount
        payoff <- payoff + new_payoff
      }
      
      ## 2. Evolution phase:
      
      # 2.1 Use fitness.fun to calculate researchers' fitness from their payoffs
      fitness <- fitness.fun(x = payoff, e = e,
                             survival_threshold = survival_threshold)
      
      # 2.2 Selection: 
      #     We use selection.fun to calculate whose traits are passed on to the
      #     next generation
      selection <- selection.fun(n = pop_size, top_n = top_n,
                                 fitness = fitness, 
                                 submission_threshold_parent =
                                   submission_threshold)
      
      # Check: Stop the loop if any of the selected submission threshold is NA
      stopifnot(all(!is.na(selection)))
      
      # 2.3 Mutation: 
      #     We use mutation.fun to generate the evolved submission thresholds
      #     of the new generation
      submission_threshold <- mutation.fun(n = pop_size,
                                           submission_threshold_selected =
                                             selection,
                                           mutation_sd = mutation_sd)
      
    }
    
    run_df[,ncol(settings_df)+j] <- submission_threshold
    # Store the results of this run in the run.output list (i.e., the index 
    # of the run and the submission thresholds of the final generation)
  }
  
  sim_df <- rbind(sim_df, run_df)
  # Store the results of all runs of the current combination of parameter
  # settings in the sim list
}

sim_df <- sim_df[-1,] # remove helper row at the top
simdata <- data.table::data.table(sim_df)
simdata <- data.table::melt.data.table(simdata, measure.vars = 
                                        c((ncol(settings_df)+1):ncol(simdata)), 
                                      variable.name = "run", 
                                      value.name = "submission_threshold")



##----------------------------------------------------------------------------##

model_plot$sd <- model_prep[, sd(submission_threshold), 
                            by = .(run, generation_duration,
                                   payoff_SR_neg, relative_payoff_SR_pos, 
                                   relative_payoff_RR,
                                   a, e, relative_survival_threshold)]$V1
model_plot$min <- model_prep[, min(submission_threshold), 
                             by = .(run, generation_duration,
                                    payoff_SR_neg, relative_payoff_SR_pos, 
                                    relative_payoff_RR,
                                    a, e, relative_survival_threshold)]$V1
model_plot$max <- model_prep[, max(submission_threshold), 
                             by = .(run, generation_duration,
                                    payoff_SR_neg, relative_payoff_SR_pos, 
                                    relative_payoff_RR,
                                    a, e, relative_survival_threshold)]$V1
model_plot$mean <- model_prep[, median(submission_threshold), 
                              by = .(run, generation_duration,
                                     payoff_SR_neg, relative_payoff_SR_pos, 
                                     relative_payoff_RR,
                                     a, e, relative_survival_threshold)]$V1

