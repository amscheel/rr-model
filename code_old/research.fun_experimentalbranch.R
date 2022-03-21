research.fun <- function(n = length(researcher_ID), 
                         rounds = generation.duration,
                         prior.dist = "uniform", prior.dist.m, prior.dist.sd,
                         submission_threshold,
                         starting_budget,
                         reward_RR, reward_normal_pos, reward_normal_neg,
                         cost_RR, cost_normal){
  
  research <- list()
  # initialise the payoff vector: before conducting any research, 
  # everybody starts with 0 payoff
  payoff <- rep(starting_budget, n)
  
  ## 1. Research phase:
  for (j in 1:generation_duration) {
    
    # 1.  Each researcher is assigned a hypothesis with a random prior 
    #     probability, either uniformly distributed between 0 and 1, or
    #     drawn from a normal distribution centred on prior.dist.m with
    #     sd = prior.dist.sd and truncated to lie between 0 and 1
    if (prior.dist == "uniform") {
      prior <- runif(n, 0, 1)
    } else if (prior.dist == "normal") {
      prior <- pmin(pmax(rnorm(n, mean = prior.dist.m, sd = prior.dist.sd), 
                         0), 1)
    }
    
    # Budget rule: only researchers with sufficient funds to pay the startup
    # cost can participate and do research
    play_round <- ifelse(payoff < startup_cost, 0, 1)
    
    # Those who participate in this round pay the startup cost
    # (subtract starup cost from budget)
    payoff <- ifelse(play_round == 1, payoff-startup_cost, payoff)
    
    # 2.  Researchers decide to run an RR or a normal study by comparing their
    #     current prior with their current submission threshold.
    #     Researchers with insufficient budget to do research in this round
    #     get NA
    # submit_as_RR <- ifelse(play_round == 1, 
    #                       ifelse(prior < submission_threshold, 1, 0), NA)
    submit_as_RR <-  ifelse(prior < submission_threshold, 1, 0)
    submit_as_RR[which(play_round == 0)] <- NA
    
    # 3.  Researchers test their hypotheses and get either a positive or a 
    #     negative result (based on their prior)
    result <- rbinom(n, 1, prior)
    
    # 4.  Researchers get a payoff depending on their chosen submission format
    #     and the result of their hypothesis test
    new_payoff <- ifelse(submit_as_RR == 1, reward_RR - cost_RR,
                         ifelse(result == 1,
                                reward_normal_pos-cost_normal, 
                                reward_normal_neg))
    
    # Researchers who did not do research in this round get 0 payoff:
    new_payoff[which(play_round == 0)] <- 0
    
    # Set result of researchers who did not do research in this round to NA:
    result[which(play_round == 0)] <- NA
    
    # Check:  Stop the loop if something went wrong with submission decisions
    stopifnot(sum(prior < submission_threshold, na.rm = TRUE) == 
                sum(submit_as_RR, na.rm = TRUE))
    # Check:  Payoffs should be the same for all RR authors, hence sd = 0.
    #         Stop the loop if this is false:
    #         (Caution: this check fails when less than 2 researchers
    #         submitted RRs.)
    stopifnot(sd(new_payoff[which(submit_as_RR == 1)], na.rm = TRUE) == 0)
    
    # 1.2   Update payoff: 
    #       add the payoffs of this round to the previous amount
    payoff <- payoff + new_payoff
    
    # store output
    research <- c(research, list(round = j,
                                 prior = prior, play_round = play_round,
                                 submit_as_RR = submit_as_RR, result = result, 
                                 new_payoff = new_payoff))
    ### CAUTION THIS IS UNFINISHED BUSINESS!!!
  }
  
}
##----------------------------------------------------------------------------##

