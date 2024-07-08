##============================================================================##
## THIS SCRIPT HAS THE FUNCTIONS FOR THE SIMULATION THAT ONLY FOCUSSES ON
## EVOLVED SUBMISSION THRESHOLD AS THE OUTCOME
##
## This means that each functions only produces one output that is relevant
## for the evolution of the submission threshold and does not store/pass on  
## any extra information about the current settings of the model.
##============================================================================##

##----------------------------------------------------------------------------##
## i. Research function: This function "does the research"
#   i.i   It generates a set of priors for the researcher population
#   i.ii  It compares the priors to the submission thresholds of the 
#         population and calculates who submits an RR and who a normal study
#   i.iii It calculates everyone's study result based on their respective prior
#   i.iv  It outputs everyone's payoff, based on their chosen submission format, 
#         study result, and payoffs for RRs and normal studies

research.fun <- function(n, 
                         prior.dist, prior.dist.m, prior.dist.sd,
                         submission.threshold,
                         payoff.SR.neg, payoff.SR.pos, payoff.RR){
  
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
  submit.as.RR <-  ifelse(prior < submission.threshold, 1, 0)
  
  # 3.  Researchers test their hypotheses and get either a positive or a 
  #     negative result (based on their prior)
  result <- rbinom(n, 1, prior)
  
  # 4.  Researchers get a payoff depending on their chosen submission format
  #     and the result of their hypothesis test. This is the functions's output
  ifelse(submit.as.RR == 1, payoff.RR,
         ifelse(result == 1, payoff.SR.pos, payoff.SR.neg))
}
##----------------------------------------------------------------------------##
## ALTERNATIVE VERSION of research.fun that is faster when only uniform priors
## are used (it does not work with other prior distributions):

research.fun.unif <- function(n, submission.threshold,
                         payoff.SR.neg, payoff.SR.pos, payoff.RR, epsilon){
  
  # 1.  Each researcher is assigned a hypothesis with a random prior 
  #     probability, uniformly distributed between 0 and 1
    prior <- runif(n, 0, 1)

  # 2.  Researchers decide to run an RR or a normal study by comparing their
  #     current prior with their current submission threshold
  submit.as.RR <-  ifelse(prior < submission.threshold, 1, 0)
  
  # 3.  Researchers test their hypotheses and get either a positive or a 
  #     negative result (based on their prior)
  result <- rbinom(n, 1, prior)
  
  # 4.  Researchers get a payoff depending on their chosen submission format
  #     and the result of their hypothesis test. This is the function's output
  ifelse(submit.as.RR == 1, payoff.RR^epsilon,
         ifelse(result == 1, payoff.SR.pos^epsilon, payoff.SR.neg^epsilon))
}


##----------------------------------------------------------------------------##
## ii. Fitness function:
#     This function translates payoffs into fitness or fitness. In the present
#     model, fitness equals fitness because it is the probability with which
#     a researcher's submission threshold will be passed on to the next
#     generation.
#     ii.i  The value of exponent epsilon determines the shape of the function:
#             epsilon = 1: linear returns
#             0 < epsilon < 1: diminishing returns 
#             epsilon > 1: increasing returns
#     ii.ii Threshold/budget rule:
#           In addition, we can specify a "survival threshold". 
#           Payoffs below the survival threshold give 0 fitness, which 
#           means that these individuals won't pass on their submission 
#           thresholds to the next generation (they "starve"). 
#           Payoffs at or above the threshold are translated into fitness
#           as usual (according to the chosen exponent).
fitness.fun <- function(x, survival.threshold){
  ifelse(x < survival.threshold, 0, x)
}
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## iii. Selection function:
#       This function uses researchers' acquired fitness and the level of
#       competition to decides whose traits are passed on to the next generation.
selection.fun <- function(n, top.n, fitness, submission.threshold.parent){
  
  # When all researchers have 0 fitness, the population would die out
  # and the model would stop. To prevent this, we'll initialise a new
  # generation that's sampled randomly with equal weights from the old
  # generation. In other words, when nobody has any chance to spread 
  # their trait to the next generation, everyone's chances will instead
  # be set to the same positive value.
  if(sum(fitness)==0){
    sample(submission.threshold.parent, 
           size = n, 
           replace = TRUE) 
  } else {
  # Competition: 
  # When top.n < n, only those researchers with fitness values in the 
  # top n are selected for reproduction.
  # Here, we sample only those researchers' submission thresholds whose fitness
  # is in the top n (submission.threshold.parent[order(fitness, 
  # decreasing = TRUE)][1:top.n]) and weight them by their respective 
  # fitness (sort(fitness, decreasing = TRUE)[1:top.n])

  # Produce a new generation based on the top n researchers of the previous
  # generation, weighted by fitness. This is the function's output:
  sample(submission.threshold.parent[order(fitness, 
                                           decreasing = TRUE)][1:top.n], 
         size = n, 
         replace = TRUE, prob = sort(fitness, decreasing = TRUE)[1:top.n])
  }
}
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## iv. Mutation function:
#     This function adds random noise (mutation) to the submission 
#     thresholdss of the new generation that was produced in the previous step
#     and truncates the mutated values so that they lie between 0 and 1.
mutation.fun <- function(n, submission.threshold.selected, mutation.sd){
  
  # Mutation:
  # Mutate the new submission thresholds by adding random noise
  # (submission.threshold.selected + rnorm(n, 0, mutation.sd; 
  # sd changes the size of mutations) and
  # truncate them so that they're  between 0 and 1. 
  # This is the function's output:
  pmin(pmax(submission.threshold.selected + rnorm(n, 0, mutation.sd), 0), 1)
}
##----------------------------------------------------------------------------##
