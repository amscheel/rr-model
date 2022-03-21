##----------------------------------------------------------------------------##
## THIS SCRIPT HAS THE FUNCTIONS
##----------------------------------------------------------------------------##
## Function setup

## i. Research function: This function "does the research"
#   i.i   It generates a set of priors for the researcher population
#   i.ii  It compares the priors to the submission thresholds of the 
#         population and calculates who submits an RR and who a normal study
#   i.iii It calculates everyone's study result based on their prior
#   i.iv  It calculates and outputs everyone's payoff, based on their
#         chosen submission format, study result, and rewards and costs for
#         RRs and normal studies

research.fun <- function(n, 
                         prior_dist, prior_dist_m, prior_dist_sd,
                         submission_threshold,
                         payoff_SR_neg, payoff_SR_pos, payoff_RR){
  # 1.  Each researcher is assigned a hypothesis with a random prior 
  #     probability, either uniformly distributed between 0 and 1, or
  #     drawn from a normal distribution centred on prior_dist_m with
  #     sd = prior_dist_sd and truncated to lie between 0 and 1
  if (prior_dist == "uniform") {
    prior <- runif(n, 0, 1)
  } else if (prior_dist == "normal") {
    prior <- pmin(pmax(rnorm(n, mean = prior_dist_m, sd = prior_dist_sd), 0), 1)
  }
  
  # 2.  Researchers decide to run an RR or a normal study by comparing their
  #     current prior with their current submission threshold
  submit_as_RR <-  ifelse(prior < submission_threshold, 1, 0)
  
  # 3.  Researchers test their hypotheses and get either a positive or a 
  #     negative result (based on their prior)
  result <- rbinom(n, 1, prior)
  
  # 4.  Researchers get a payoff depending on their chosen submission format
  #     and the result of their hypothesis test
  new_payoff <- ifelse(submit_as_RR == 1, payoff_RR,
                       ifelse(result == 1, payoff_SR_pos, payoff_SR_neg))
  
  # generate output
  invisible(list(prior_dist = prior_dist, 
                 payoff_SR_pos = payoff_SR_pos, payoff_SR_neg = payoff_SR_neg,
                 payoff_RR = payoff_RR,
                 submission_threshold = submission_threshold,
                 new_payoff = new_payoff))
}

## ii. Fitness function:
#     This function translates payoffs into fitness or fitness. In the present
#     model, fitness equals fitness because it is the probability with which
#     a researcher's submission threshold will be passed on to the next
#     generation.
#     ii.i  The value of exponent e determines the shape of the function:
#             e = 1: linear returns
#             0 < e < 1: diminishing returns 
#             e > 1: increasing returns
#     ii.ii Threshold/budget rule:
#           In addition, we can specify a "survival threshold". 
#           Payoffs below the survival threshold give 0 fitness, which 
#           means that these individuals won't pass on their submission 
#           thresholds to the next generation (they "starve"). 
#           Payoffs at or above the threshold are translated into fitness
#           as usual (according to the chosen exponent).
fitness.fun <- function(x, e, survival_threshold){
  ifelse(x < survival_threshold, 0, x^e)
}

## iii. Selection function:
#       This function uses researchers' acquired fitness and the level of
#       competition to decides whose traits are passed on to the next generation.
selection.fun <- function(n, top_n, fitness, submission_threshold_parent){

  # Competition: 
  # When top_n < n, only those researchers with fitness values in the 
  # top n are selected for reproduction.
  # Select only those researchers' submission thresholds whose fitness
  # is in the top n:
  submission_threshold_parent_top <- submission_threshold_parent[order(fitness, decreasing = TRUE)][1:top_n]
  
  # Create a vector that only contains the fitness values of the top n
  # to make the following step easier
  fitness_top <- sort(fitness, decreasing = TRUE)[1:top_n]
  
  # Produce a new generation based on the top n researchers of the previous
  # generation, weighted by fitness
  submission_threshold_selected <- sample(submission_threshold_parent_top, 
                                          size = n, 
                                          replace = TRUE, prob = fitness_top)
  # generate output
  invisible(list(generation = i, 
                 submission_threshold_selected = submission_threshold_selected))
}

## iv. Mutation function:
#     This function adds random noise (mutation) to the submission 
#     thresholdss of the new generation that was produced in the previous step
#     and truncates the mutated values so that they lie between 0 and 1.
mutation.fun <- function(n, submission_threshold_selected, mutation_sd){
  
  # Mutation:
  # New submission thresholds mutate (sd changes the size of mutations)
  submission_threshold_mutated <- submission_threshold_selected + 
    rnorm(n, 0, mutation_sd)
  
  # Truncate the mutated numbers so that they're between 0 and 1
  submission_threshold_child <- pmin(pmax(submission_threshold_mutated, 0), 1)
  
  # generate output
  invisible(list(submission_threshold_child = submission_threshold_child))
}

