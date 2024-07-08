##============================================================================##
## This is the improved code for running the model without the df_loop dataframe
##============================================================================##
## Model setup: initialise lists and the submission_threshold variable

researchlist <- list()
researchoutput <- list()
model <- list()
submission_threshold <- runif(length(researcher_ID), 0,1)
#------------------------------------------------------------------------------#
## Here comes the loop:

for (i in 1:generations) {
  
  # initialise the payoff vector: before conducting any research, 
  # everybody starts with 0 payoff
  payoff <- rep(0, length(researcher_ID))
  
  ## 1. Research phase:
  for (j in 1:generation_duration) {
    
    # 1.1 We use research.fun to "do the research":
    #     Assign priors, compare with submission thresholds, decide who submits
    #     RRs and who doesn't, calculate results, and calculate payoffs
    research <- research.fun(prior.dist = prior.dist,
                             prior.dist.m = prior.dist.m,
                             prior.dist.sd = prior.dist.sd,
                             submission_threshold = submission_threshold,
                             reward_RR = reward_RR,
                             reward_normal_pos = reward_normal_pos,
                             reward_normal_neg = reward_normal_neg,
                             cost_RR = cost_RR,
                             cost_normal = cost_normal)
    
    # Check:  Stop the loop if something went wrong with submission decisions
    stopifnot(sum(research$prior < research$submission_threshold) == 
                sum(research$submit_as_RR))
    # Check:  Payoffs should be the same for all RR authors, hence sd = 0.
    #         Stop the loop if this is false:
    #         (Caution: this check fails when less than 2 researchers
    #         submitted RRs.)
    # stopifnot(sd(research$new_payoff[which(research$submit_as_RR == 1)]) == 0)
    
    # 1.2   Update payoff: 
    #       add the payoffs of this round to the previous amount
    payoff <- payoff + research$new_payoff
    
    research$researcher_ID <- researcher_ID
    research$total_payoff <- payoff # add total payoff to the "research" list
    researchlist[[j]] <- research # store data from this research cycle in the output list
  }
  
  ## 2. Evolution phase:
  
  # 2.1 We use utility.fun to calculate researchers' fitness from their payoffs
  utility <- utility.fun(x = payoff, e = e, a = a, 
                         survival_threshold = survival_threshold,
                         shape = utility_shape)
  
  # 2.2 Selection: 
  #     We use selection.fun to calculate whose traits are passed on to the
  #     next generation
  selection <- selection.fun(n = length(researcher_ID), utility = utility,
                             submission_threshold_parent = 
                               research$submission_threshold)
  
  # Check: Stop the loop if any of the selected submission threshold is NA
  stopifnot(all(!is.na(selection$submission_threshold_selected)))
  
  # 2.3 Mutation: 
  #     We use mutation.fun to generate the evolved submission thresholds
  #     of the new generation
  evolution <- mutation.fun(n = length(researcher_ID),
                            submission_threshold_selected = 
                              selection$submission_threshold_selected,
                            mutation_sd = mutation_sd)
  # researchoutput[[i]]$
  # Put the new generations' evolved submission thresholds into the data frame:
  submission_threshold <- evolution$submission_threshold_child
  
  selection$submission_threshold_child <- evolution$submission_threshold_child
  selection$researcher_ID <- researcher_ID
  model[[i]] <- selection
  researchoutput[[i]] <- data.table::rbindlist(researchlist)
  # model$generation[[i]]$evolution <- selection
}

model <- data.table::rbindlist(model)
researchoutput <- data.table::rbindlist(researchoutput)