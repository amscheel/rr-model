################################################################################
####                                                                        ####
#### Script 6: Simulation functions for alternative model based on          ####
####           expected fitness                                             ####
#### ---------------------------------------------------------------------- ####
#### Content:                                                               ####
####  * 1: Setup                                                            ####
####  * 2: Calculate expected fitness for a given publication strategy      ####
####  * 3: Find publication strategy that maximises expected fitness        ####
####                                                                        ####
#### Note:                                                                  ####
####   This script only contains the functions for the simulation itself.   ####
####   To run the simulation, use the script "07_EF-model_execute.R".       ####
####                                                                        ####
################################################################################

##============================================================================##
## 1. Setup
##============================================================================##

##----------------------------------------------------------------------------##
# This script requires the package data.table.

# Install data.table unless it is already installed:
if(!require(data.table)){install.packages("data.table")}
##----------------------------------------------------------------------------##


##============================================================================##
## 2. Function to calculate expected fitness for a given publication strategy
##============================================================================##

##----------------------------------------------------------------------------##
expected.fitness.fun <- function(m = 1, # number of research cycles before fitness is calculated
                                 s, # publication strategy (value between 0 and 1)
                                 b.SR.neg = 0, # value of a negative standard report
                                 b.SR.pos = 1, # value of a positive standard report
                                 b.RR, # value of a Registered Report
                                 epsilon = 1, # exponent of the fitness function
                                 survival.threshold = 0, # threshold below which fitness = 0
                                 survival.threshold.rel = T, # whether survival threshold is calculated as a proportion of m (maximum payoff if b.SR.pos = 1)
                                 competition = 1){ # if fitness is outside of top X proportion, it is set to 0
  
  prior.SR <- (s+1)/2 
  prob.RR <- s
  prob.SR.neg <- (1-s)*(1-prior.SR)
  prob.SR.pos <- (1-s)*prior.SR 
  
  # Create a df with event counts for all possible outcome combinations (ignoring order)
  combinationmat <- expand.grid(0:m, 0:m, 0:m)
  combinationmat <- combinationmat[rowSums(combinationmat)==m,]
  
  colnames(combinationmat) <- c("SR.neg", "SR.pos", "RR")
  # Calculate the probability of each combination
  combinationmat$prob <- apply(combinationmat, 1, function(f) dmultinom(
    x = c(f[1], f[2], f[3]), 
    size = m, 
    prob = c(prob.SR.neg, prob.SR.pos, prob.RR)))
  
  # Calculate survival threshold, depending on whether it's relative or absolute
  survival.threshold <- ifelse(survival.threshold.rel == T, survival.threshold*m, survival.threshold)
  
  # calculate total payoffs for each outcome combination
  combinationmat$payoff <- combinationmat$SR.neg * b.SR.neg +
    combinationmat$SR.pos * b.SR.pos +
    combinationmat$RR * b.RR
  combinationmat$fitness <- ifelse(combinationmat$payoff < survival.threshold,
                                   0, combinationmat$payoff^epsilon)
  
  # OLD: calculate total payoffs for each outcome combination, with option of
  # adding a small amount of noise
  # combinationmat$payoff <- combinationmat$SR.neg * 
  #   (b.SR.neg + rnorm(nrow(combinationmat), m = 0, sd = noise)) +
  #   combinationmat$SR.pos * 
  #   (b.SR.pos + rnorm(nrow(combinationmat), m = 0, sd = noise)) + 
  #   combinationmat$RR * 
  #   (b.RR + rnorm(nrow(combinationmat), m = 0, sd = noise))
  # combinationmat$fitness <- ifelse(combinationmat$payoff < survival.threshold,
  #                                  0, combinationmat$payoff^epsilon)
  
  combinationmat$fitness <- ifelse(combinationmat$fitness < quantile(combinationmat$fitness, probs = 1-competition), 0, combinationmat$fitness)
  
  # output expected fitness (probability-weighted average)
  return(weighted.mean(combinationmat$fitness, combinationmat$prob))
}
##----------------------------------------------------------------------------##


##============================================================================##
## 3. Function to identify the publication strategy with maximum fitness 
##    (via simulation)
##============================================================================##

##----------------------------------------------------------------------------##
# Function to generate a data frame with all combinations of model parameters:

max.EF.sim <- function(m = c(1, 2, 4, 8, 16, 32),
                       s = seq(from = 0.01, to = .99, length.out = 100),
                       b_SR_neg = 0,
                       b_SR_pos = 1,
                       b_RR = seq(0.1, 0.9, 0.1),
                       epsilon = c(0.2, 1, 5),
                       survival_threshold.rel = T,
                       survival_threshold = 0,
                       competition = 1){
  
  # Generate a data frame with all combinations of model parameters:
  parameters <- data.table::as.data.table(expand.grid(m = m,
                                          s = s,
                                          b_SR_neg = b_SR_neg,
                                          b_SR_pos = b_SR_pos,
                                          b_RR = b_RR,
                                          epsilon = epsilon,
                                          survival_threshold.rel = 
                                            survival_threshold.rel,
                                          survival_threshold = survival_threshold,
                                          competition = competition))
  
  
  # Calculate expected fitness for a range of publication strategies (s) 
  # in each condition 
  parameters$expected_fitness <- apply(parameters, 1, 
                                       function(x) expected.fitness.fun(
                                         m = x[1], s = x[2], b.RR = x[5], 
                                         epsilon = x[6], 
                                         survival.threshold.rel = x[7], 
                                         survival.threshold = x[8], 
                                         competition = x[9]))
  
  # Find the publication strategy (s) that maximises expected fitness in 
  # each condition
  s_max_EF <- parameters[parameters[, .I[expected_fitness == max(expected_fitness)], 
                                    by = .(m, epsilon, b_RR, survival_threshold.rel,
                                           survival_threshold, competition)]$V1]
  
  colnames(s_max_EF)[2] <- "s_max_EF" # rename s to avoid confusion
  
  # Output the publication strategy (s) that maximises expected fitness in 
  # each condition
  return(s_max_EF)
  
}

#------------------------------------------------------------------------------#



