##============================================================================##
## Experimental branch: here I tried a model where researchers have a starting
## budget that get depleted by a cost to be paid per study unless sufficient
## rewards offset the cost. Researchers whose budget is depleted (budget < cost)
## cannot perform any new study.
##
## IMPORTANT: I also changed the functions quite a bit and figured out how
##            to use facets in the plots.
##============================================================================##

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

research.fun <- function(n = pop_size, 
                         prior_dist = "uniform", prior_dist_m, prior_dist_sd,
                         submission_threshold,
                         budget, cost,
                         reward_RR, reward_SR_pos, reward_SR_neg){
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
  #     and the result of their hypothesis test. Payoff = reward - cost
  payoff <- ifelse(submit_as_RR == 1, reward_RR,
                   ifelse(result == 1, reward_SR_pos, reward_SR_neg)) - cost
  
  # 5. Researchs who had sufficient funds for running a study get the payoff 
  budget_updated <- ifelse(budget < cost, budget, budget + payoff)
  
  # generate output
  invisible(list(budget_updated = budget_updated))
}

## ii. Utility function:
#     This function translates payoffs into utility or fitness. In the present
#     model, utility equals fitness because it is the probability with which
#     a researcher's submission threshold will be passed on to the next
#     generation.
#     ii.i  The value of exponent e determines the shape of the function:
#             e = 1: linear returns
#             e < 1: diminishing returns 
#             e > 1: increasing returns
#     ii.ii Threshold/budget rule:
#           In addition, we can specify a "survival threshold". 
#           Payoffs below the survival threshold give 0 utility, which 
#           means that these individuals won't pass on their submission 
#           thresholds to the next generation (they "starve"). 
#           Payoffs at or above the threshold are translated into utility
#           as usual (according to the chosen exponent).
utility.fun <- function(x, e = 1, survival_threshold = 0){
  ifelse(x < survival_threshold, 0, x^e)
}

## iii. Selection function:
#       This function translates the acquired utility of all researchers into
#       fitness and decides who passes their traits on to the next generation.
selection.fun <- function(n = pop_size, utility, submission_threshold_parent){
  # Calculate fitness as a standardised score between 0 and 1
  # The ifelse() construction makes sure that fitness doesn't turn to NA 
  # when sum(utility) == 0
  #fitness <- ifelse(utility == 0, 0, utility/sum(utility))
  
  # Selection: 
  # Select whose submission threshold trait makes it into 
  # the next generation, based on fitness
  submission_threshold_selected <- sample(submission_threshold_parent, 
                                          size = n, 
                                          replace = TRUE, prob = utility)
  # generate output
  invisible(list(generation = i, 
                 submission_threshold_selected = submission_threshold_selected))
}

## iv. Mutation function:
#     This function takes the submission threshold of the parent generation
#     that were selected to reproduce and generates the submission thresholds
#     of the child generation.
#     iv.i  It adds random noise (mutation) to the selected parent's submission
#           thresholds
#     iv.ii It truncates the mutated values so that they lie between 0 and 1.
mutation.fun <- function(n = pop_size, submission_threshold_selected, mutation_sd){
  # Mutation:
  # New submission thresholds mutate (sd changes the size of mutations)
  submission_threshold_mutated <- submission_threshold_selected + 
    rnorm(n, 0, mutation_sd)
  
  # Truncate the mutated numbers so that they're between 0 and 1
  submission_threshold_child <- pmin(pmax(submission_threshold_mutated, 0), 1)
  
  # generate output
  invisible(list(submission_threshold_child = submission_threshold_child))
}


##----------------------------------------------------------------------------##
## Fixed model parameters

# Population size & number of generations
pop_size <-5e2 # population size
generations <- 500 # set number of generations the model will loop through
runs <- 50 # number of times each model will be run
mutation_sd <- .01 # amount of noise added by mutations during evolution

# Parameters of the prior distribution:
prior_dist <- "uniform" # distribution type: uniform or normal
prior_dist_m <- 0.2 # mean of the prior distribution IF prior_dist = "normal"
prior_dist_sd <- 0.2 # sd of the prior distribution IF prior_dist = "normal"

##----------------------------------------------------------------------------##
## Varying model parameters

generation_duration <- 5 # number of research rounds in each generation

# Starting budget, costs, & rewards of conducting research: 

starting_budget_relative <- c(.2, .5, 1) # starting budget for each researcher, relative to generation duration (starting_budget = starting_budget_relative * generation_duration)

cost <- 1 # cost paid for each study, subtracted from budget

# Set the rewardsfor RRs and standard reports:
reward_SR_neg <- 0  # reward for a negative result in a standard report, added to budget
reward_SR_pos <- c(1.2, 1.8, 3)  # reward for a positive result in a standard report, added to budget
rel_reward_RR <- 0.5 # reward for a Registered Report, added to budget

# Parameters of the utility/fitness function:
survival_threshold <- 1
e <- c(1, 2)  # exponent of x to influence the shape


settings.df <- expand.grid(generation_duration = generation_duration,
                           starting_budget_relative = starting_budget_relative,
                           cost = cost,
                           reward_SR_neg = reward_SR_neg, 
                           reward_SR_pos = reward_SR_pos, 
                           rel_reward_RR = rel_reward_RR, 
                           e = e, survival_threshold = survival_threshold)
#------------------------------------------------------------------------------#
## Model setup: initialise lists and the submission_threshold variable

model <- list()
#------------------------------------------------------------------------------#
## Here comes the loop:

for (i in 1:nrow(settings.df)) {
  run.output <- list()
  
  generation_duration <- settings.df$generation_duration[i]
  
  # Set starting budget
  starting_budget <- settings.df$starting_budget_relative[i] * 
    generation_duration
  
  # Set the costs and rewards for RRs and standard reports:
  cost <- settings.df$cost[i]
  reward_SR_neg <- settings.df$reward_SR_neg[i]  # reward for neg. result in standard report
  reward_SR_pos <- settings.df$reward_SR_pos[i]  # reward for pos. result in standard report
  reward_RR <- settings.df$rel_reward_RR[i] * 
    (reward_SR_neg + reward_SR_pos) # reward for Registered Report
  
  # Parameters of the utility/fitness function:
  e <- settings.df$e[i]  # exponent of x to influence the shape
  survival_threshold <- settings.df$survival_threshold[i]
  
  
  for (j in 1:runs) {
    submission_threshold <- runif(pop_size, 0,1)
    
    for (k in 1:generations) {
      
      # initialise the payoff vector: before conducting any research, 
      # everybody starts with 0 payoff
      budget <- rep(starting_budget, pop_size)
      
      ## 1. Research phase:
      for (l in 1:generation_duration) {
        
        # 1.1 We use research.fun to "do the research":
        #     Assign priors, compare with submission thresholds, decide who submits
        #     RRs and who doesn't, calculate results, and calculate payoffs
        research <- research.fun(prior_dist = prior_dist,
                                 prior_dist_m = prior_dist_m,
                                 prior_dist_sd = prior_dist_sd,
                                 budget = budget, cost = cost,
                                 submission_threshold = submission_threshold,
                                 reward_RR = reward_RR,
                                 reward_SR_pos = reward_SR_pos,
                                 reward_SR_neg = reward_SR_neg)
        
        # 1.2   Update payoff: 
        #       add the payoffs of this round to the previous amount
        budget <- research$budget_updated
      }
      
      ## 2. Evolution phase:
      
      # 2.1 We use utility.fun to calculate researchers' fitness from their payoffs
      utility <- utility.fun(x = budget, 
                             survival_threshold = survival_threshold, 
                             e = e)
      
      # 2.2 Selection: 
      #     We use selection.fun to calculate whose traits are passed on to the
      #     next generation
      selection <- selection.fun(n = pop_size, utility = utility,
                                 submission_threshold_parent = submission_threshold)
      
      # Check: Stop the loop if any of the selected submission threshold is NA
      stopifnot(all(!is.na(selection$submission_threshold_selected)))
      
      # 2.3 Mutation: 
      #     We use mutation.fun to generate the evolved submission thresholds
      #     of the new generation
      evolution <- mutation.fun(n = pop_size,
                                submission_threshold_selected = 
                                  selection$submission_threshold_selected,
                                mutation_sd = mutation_sd)
      
      # Put the new generations' evolved submission thresholds into the data frame:
      submission_threshold <- evolution$submission_threshold_child
      
      selection$submission_threshold_child <- evolution$submission_threshold_child
      
    }
    
    run.output[[j]] <- list(run = j, submission_threshold = submission_threshold)
  }
  
  model[[i]] <- list(generation_duration = generation_duration,
                     starting_budget_relative = 
                       settings.df$starting_budget_relative[i],
                     cost = cost,
                     reward_SR_neg = reward_SR_neg, 
                     reward_SR_pos = reward_SR_pos,
                     rel_reward_RR = rel_reward_RR, 
                     e = e, survival_threshold = survival_threshold,
                     output = run.output)
}

# saveRDS(model, "model_settings_3.RData")
# saveRDS(settings.df, "settings_3.RData")
# simdata_settings2 <- readRDS("model_settings_2.RData")

# turn the model list into a data frame in long format:
model <- data.table::rbindlist(model) # unlist "model" with a remaining nested column (output)
model$id <- seq.int(nrow(model)) # create an id column to help merge the dfs later
output <- data.table::rbindlist(model$output, idcol = "id") # unlist the output column separately
model <- dplyr::left_join(model, output, by = "id") # merge the two dfs
model <- model[, c("generation_duration", "starting_budget_relative",
                   "reward_SR_neg", "reward_SR_pos",
                   "rel_reward_RR", "cost", "e", "survival_threshold",
                   "run", "submission_threshold")] # get rid of old nested column and id helper column

# alternative way of getting rid of old nested column & id helper column but takes much more time if model is large
# model$output <- NULL
# model$id <- NULL


##----------------------------------------------------------------------------##
## Plot the submission thresholds
model_prep <- model
# model_prep$relative_survival_threshold <- as.factor(as.character(model_prep$relative_survival_threshold))
model_prep$starting_budget_relative <- as.factor(as.character(model_prep$starting_budget_relative))
model_prep$e <- as.factor(as.character(model_prep$e))
model_prep$run <- as.factor(as.character(model_prep$run))
model_prep$generation_duration <- as.factor(as.character(model_prep$generation_duration))



model_plot <- model_prep[, median(submission_threshold), 
                         by = .(run, generation_duration, 
                                starting_budget_relative,
                                reward_SR_neg, reward_SR_pos, 
                                rel_reward_RR, cost, 
                                e, survival_threshold)]
colnames(model_plot)[colnames(model_plot) == "V1"] <- "median"

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

##----------------------------------------------------------------------------##

# Basic plot setup:
basic_plot <- ggplot(
  model_plot,
  aes(x = starting_budget_relative,
      y = median,
      colour = e,
      group = run
  )) +
  # scale_x_continuous(breaks = seq(0, generations, generations / 10),
  #                    name = "generation") +
  scale_y_continuous(lim = c(0, 1),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold") +
  scale_colour_viridis_d() +
  theme_bw() +
  ggtitle(paste(pop_size, " researchers", ", ", generations, " generations", ", ",
                runs, " runs", sep = "")) + 
  geom_point(aes(group = e), 
             position = position_dodge(width = .3),
             size = .5,
             alpha = .5) +
  stat_summary(aes(x = as.numeric(starting_budget_relative)-0.05, 
                   group = e),
               geom = "line",
               fun = "median", 
               position = position_dodge(width = .3),
               size = .2) +
  stat_summary(aes(x = as.numeric(starting_budget_relative)-0.05, 
                   group = e),
               fun.data = "median_hilow", 
               position = position_dodge(width = .3),
               size = .2)

basic_plot + facet_grid(. ~ reward_SR_pos)

basic_plot + facet_grid(reward_SR_pos ~ generation_duration)

basic_plot + facet_grid(reward_SR_pos ~ generation_duration)


model_survthres0 <- model
model_survthres0_prep <- model_prep
model_survthres0_plot <- model_plot
# when reward_SR_pos = 1, i.e. just as high as cost
plot_survthres0 <- basic_plot + facet_grid(reward_SR_pos ~ generation_duration)



##----------------------------------------------------------------------------##