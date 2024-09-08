################################################################################
####                                                                        ####
#### Script 2: Helper functions for main simulation function (evo model)    ####
####                                                                        ####
#### ---------------------------------------------------------------------- ####
#### Content:                                                               ####
####  * 1: Research function                                                ####
####  * 2: Fitness function                                                 ####
####  * 3: Selection function                                               ####
####  * 4: Mutation function                                                ####
####  * 5: Alternative version of research.fun (NOT USED)                   ####
####                                                                        ####
#### Note:                                                                  ####
####   This script only contains helper function for the main simulation.   ####
####   To run the simulation, use the script "03_evo-model_execute.R". It   ####
####   automatically loads this script and the main simulation function.    ####
####   This script here requires no specific packages.                      ####
####                                                                        ####
################################################################################

##============================================================================##
## 1. Research function
##============================================================================##

##----------------------------------------------------------------------------##
## Research function: This function "does the research"
#   1.a  It generates a set of priors for the researcher population
#   1.b  It compares the priors to the publication strategies of the 
#         population and calculates who submits an RR and who a normal study
#   1.c  It calculates everyone's study results based on their respective prior
#   1.d  It outputs everyone's payoffs, based on their chosen submission format, 
#         study result, and payoffs for RRs and normal studies

research.fun <- function(n, 
                         prior.dist, prior.dist.m, prior.dist.sd,
                         publication.strategy,
                         payoff.SR.neg, payoff.SR.pos, payoff.RR){
  
  # 1.1 Each researcher is assigned a hypothesis with a random prior 
  #     probability, either uniformly distributed between 0 and 1, or
  #     drawn from a normal distribution centred on prior.dist.m with
  #     sd = prior.dist.sd and truncated to lie between 0 and 1
  if (prior.dist == "uniform") {
    prior <- runif(n, 0, 1)
  } else if (prior.dist == "normal") {
    prior <- pmin(pmax(rnorm(n, mean = prior.dist.m, sd = prior.dist.sd), 0), 1)
  }
  
  # 1.2 Researchers decide to run an RR or a normal study by comparing their
  #     current prior with their current publication strategy
  submit.as.RR <-  ifelse(prior < publication.strategy, 1, 0)
  
  # 1.3 Researchers test their hypotheses and get either a positive or a 
  #     negative result (based on their prior)
  result <- rbinom(n, 1, prior)
  
  # 1.4 Researchers get a payoff depending on their chosen submission format
  #     and the result of their hypothesis test. This is the functions's output
  ifelse(submit.as.RR == 1, payoff.RR,
         ifelse(result == 1, payoff.SR.pos, payoff.SR.neg))
}


##============================================================================##
## 2. Fitness function
##============================================================================##

##----------------------------------------------------------------------------##
## Fitness function:
#     This function translates payoffs into fitness. In the present model,
#     fitness is the probability with which a researcher's submission
#     strategy will be passed on to the next generation.
#     This function also includes the option of a survival threshold. 
# 
#     2.a  The value of exponent epsilon determines the shape of the function:
#           epsilon = 1: linear returns
#           0 < epsilon < 1: diminishing returns 
#           epsilon > 1: increasing returns
#     2.b  Threshold/budget rule:
#           In addition, we can specify a "survival threshold". 
#           Payoffs below the survival threshold give 0 fitness, which 
#           means that these individuals won't pass on their submission 
#           strategies to the next generation (they "starve"). 
#           Payoffs at or above the threshold are translated into fitness
#           as usual (according to the chosen exponent).

fitness.fun <- function(x, epsilon, survival.threshold){
  ifelse(x < survival.threshold, 0, x^epsilon)
}
##----------------------------------------------------------------------------##


##============================================================================##
## 3. Selection function
##============================================================================##

##----------------------------------------------------------------------------##
## Selection function:
#     This function uses researchers' acquired fitness and the level of
#     competition to decide whose traits are passed on to the next generation.
selection.fun <- function(n, top.n, fitness, publication.strategy.parent){
  
  # When all researchers have 0 fitness, the population would die out
  # and the model would stop. To prevent this, we'll initialise a new
  # generation that's sampled randomly with equal weights from the old
  # generation. In other words, when nobody has any chance to spread 
  # their trait to the next generation, everyone's chances will instead
  # be set to the same positive value.
  if(sum(fitness)==0){
    sample(publication.strategy.parent, 
           size = n, 
           replace = TRUE) 
  } else {
    
  # Competition: 
  # When top.n < n, only those researchers with fitness values in the 
  # top n are selected for reproduction.
  # Here, we sample only those researchers' publication strategies whose fitness
  # is in the top n (publication.strategy.parent[order(fitness, 
  # decreasing = TRUE)][1:top.n]) and weight them by their respective 
  # fitness (sort(fitness, decreasing = TRUE)[1:top.n])

  # Produce a new generation based on the top n researchers of the previous
  # generation, weighted by fitness. This is the function's output:
  sample(publication.strategy.parent[order(fitness, 
                                           decreasing = TRUE)][1:top.n], 
         size = n, 
         replace = TRUE, prob = sort(fitness, decreasing = TRUE)[1:top.n])
  }
}
##----------------------------------------------------------------------------##


##============================================================================##
## 4. Mutation function
##============================================================================##

##----------------------------------------------------------------------------##
## Mutation function:
#    This function adds random noise to the publication strategies
#    of the new generation that was produced in the previous step
#    and truncates the mutated values so that they lie between 0 and 1.
mutation.fun <- function(n, publication.strategy.selected, mutation.sd){
  
  # Mutation:
  # Mutate the new publication strategies by adding random noise
  # (publication.strategy.selected + rnorm(n, 0, mutation.sd; 
  # sd changes the size of mutations) and
  # truncate them so that they're  between 0 and 1. 
  # This is the function's output:
  pmin(pmax(publication.strategy.selected + rnorm(n, 0, mutation.sd), 0), 1)
}
##----------------------------------------------------------------------------##


##============================================================================##
## 5. Alternative version of research.fun (NOT USED)
##============================================================================##

##----------------------------------------------------------------------------##
## ALTERNATIVE VERSION of research.fun that is faster when only uniform priors
## are used (it does not work with other prior distributions):

research.fun.unif <- function(n, publication.strategy,
                              payoff.SR.neg, payoff.SR.pos, payoff.RR){
  
  # 5.1 Each researcher is assigned a hypothesis with a random prior 
  #     probability, uniformly distributed between 0 and 1
  prior <- runif(n, 0, 1)
  
  # 5.2 Researchers decide to run an RR or a normal study by comparing their
  #     current prior with their current publication strategy
  submit.as.RR <-  ifelse(prior < publication.strategy, 1, 0)
  
  # 5.3 Researchers test their hypotheses and get either a positive or a 
  #     negative result (based on their prior)
  result <- rbinom(n, 1, prior)
  
  # 5.4 Researchers get a payoff depending on their chosen submission format
  #     and the result of their hypothesis test. This is the functions's output
  ifelse(submit.as.RR == 1, payoff.RR,
         ifelse(result == 1, payoff.SR.pos, payoff.SR.neg))
}

