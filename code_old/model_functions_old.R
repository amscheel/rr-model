##----------------------------------------------------------------------------##
## MODEL FUNCTIONS                                                            ##
##----------------------------------------------------------------------------##

## i. Research function: This function "does the research"
#   i.i   It generates a set of priors for the researcher population
#   i.ii  It compares the priors to the submission thresholds of the 
#         population and calculates who submits an RR and who a normal study
#   i.iii It calculates everyone's study result based on their prior
#   i.iv  It calculates and outputs everyone's payoff, based on their
#         chosen submission format, study result, and rewards and costs for
#         RRs and normal studies
research.fun <- function(n = length(researcher_ID), 
                         prior.dist = "uniform", prior.dist.m, prior.dist.sd,
                         submission_threshold,
                         reward_RR, reward_normal_pos, reward_normal_neg,
                         cost_RR, cost_normal){
  # 1.  Each researcher is assigned a hypothesis with a random prior 
  #     probability, either uniformly distributed between 0 and 1, or
  #     drawn from a normal distribution centred on prior.dist.m with
  #     sd = prior.dist.sd and truncated to lie between 0 and 1
  if (prior.dist == "uniform") {
    prior <- runif(n, 0, 1)
  } else if (prior.dist == "normal") {
    prior <- pmin(pmax(rnorm(n, mean = prior.dist.m, sd = prior.dist.sd), 0), 1)
  }
  
  # 2.  Researchers decide to run an RR or a normal study by comparing their
  #     current prior with their current submission threshold
  submit_as_RR <-  ifelse(prior < submission_threshold, 1, 0)
  
  # 3.  Researchers test their hypotheses and get either a positive or a 
  #     negative result (based on their prior)
  result <- rbinom(n, 1, prior)
  
  # 4.  Researchers get a payoff depending on their chosen submission format
  #     and the result of their hypothesis test
  new_payoff <- ifelse(submit_as_RR == 1, reward_RR - cost_RR,
                       ifelse(result == 1,
                              reward_normal_pos-cost_normal, reward_normal_neg))
  
  # generate output
  invisible(list(generation = i, round = j,
                 prior = prior, submission_threshold = submission_threshold,
                 submit_as_RR = submit_as_RR, result = result, 
                 new_payoff = new_payoff))
}

## ii. Utility function:
#     This function translates payoffs into utility or fitness. In the present
#     model, utility equals fitness because it is the probability with which
#     a researcher's submission threshold will be passed on to the next
#     generation.
#     ii.i  Three different function shapes are available:
#           Linear:              f(x) = a * x
#           Diminishing returns: f(x) = a * x^(1/e)
#           Increasing returns:  f(x) = a * x^e
#     ii.ii Threshold/budget rule:
#           In addition, we can specify a "survival threshold". 
#           Payoffs below the survival threshold give 0 utility, which 
#           means that these individuals won't pass on their submission 
#           thresholds to the next generation (they "starve"). 
#           Payoffs at or above the threshold are translated into utility
#           as usual (according to the chosen function).
utility.fun <- function(x, e = 2, a = 1, 
                        survival_threshold = 0, shape = "linear"){
  if (shape == "linear") {
    ifelse(x < survival_threshold, 0, a * x) # linear
  } else if (shape == "diminishing") {
    ifelse(x < survival_threshold, 0, a * x^(1/e)) # diminishing returns
  } else if (shape == "increasing") {
    ifelse(x < survival_threshold, 0, a * x^e)} # increasing returns
}

## iii. Selection function:
#       This function translates the acquired utility of all researchers into
#       fitness and decides who passes their traits on to the next generation.
selection.fun <- function(n, utility, submission_threshold_parent){
  # Calculate fitness as a standardised score between 0 and 1
  # The ifelse() construction makes sure that fitness doesn't turn to NA 
  # when sum(utility) == 0
  fitness <- ifelse(utility == 0, 0, utility/sum(utility))
  
  # Selection: 
  # Select whose submission threshold trait makes it into 
  # the next generation, based on fitness
  submission_threshold_selected <- sample(submission_threshold_parent, 
                                          size = n, 
                                          replace = TRUE, prob = fitness)
  # generate output
  invisible(list(generation = i, utility = utility, fitness = fitness,
                 submission_threshold_parent = submission_threshold_parent,
                 submission_threshold_selected = 
                   submission_threshold_selected))
}

## iv. Mutation function:
#     This function takes the submission threshold of the parent generation
#     that were selected to reproduce and generates the submission thresholds
#     of the child generation.
#     iv.i  It adds random noise (mutation) to the selected parent's submission
#           thresholds
#     iv.ii It truncates the mutated values so that they lie between 0 and 1.
mutation.fun <- function(n, submission_threshold_selected, mutation_sd){
  # Mutation:
  # New submission thresholds mutate (sd changes the size of mutations)
  submission_threshold_mutated<- submission_threshold_selected + 
    rnorm(n, 0, mutation_sd)
  
  # Truncate the mutated numbers so that they're between 0 and 1
  submission_threshold_child <- pmin(pmax(submission_threshold_mutated, 0), 1)
  
  # generate output
  invisible(list(generation = i, mutation_sd = mutation_sd,
                 submission_threshold_selected = submission_threshold_selected,
                 submission_threshold_mutated = submission_threshold_mutated,
                 submission_threshold_child = submission_threshold_child))
}
