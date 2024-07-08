## Setup: load packages
library(reshape2)
library(ggplot2)

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


##----------------------------------------------------------------------------##
## Model setup: initial parameters

# Population size & number of generations
researcher_ID <-  seq(1:1e3) # initialise researcher population & set pop size
generations <- 5e2 # set number of generations the model will loop through
generation_duration <- 10 # number of research rounds in each generation
mutation_sd <- .01 # amount of noise added by mutations during evolution

# Rewards and costs: 
#   We define the payoff researchers get for their studies as 
#   payoff = reward - cost.
# Here we set the rewards and costs for RRs and normal studies:
reward_normal_pos <- 1  # reward for a positive result in a normal study
reward_normal_neg <- 0.5  # reward for a negative result in a normal study
scaling_factor <- .5
reward_RR <- scaling_factor * (reward_normal_pos - reward_normal_neg) +
  reward_normal_neg # reward for a Registered Report
cost_RR <- 0 # cost of conducting a RR (subtracted from reward)
cost_normal <- 0 # cost of publishing a normal paper (subtracted from reward)

# Parameters of the prior distribution:
prior.dist <- "uniform" # distribution type: uniform or normal
prior.dist.m <- 0.3 # mean of the prior distribution IF prior.dist = "normal"
prior.dist.sd <- 0.15 # sd of the prior distribution IF prior.dist = "normal"

# Parameters of the utility/fitness function:
utility_shape <- "linear" # shape of the utility function
a <- 1  # multiplier of x to influence the shape
e <- 10  # exponent of x to influence the shape
survival_threshold <- 0*generation_duration  # threshold below which all payoffs give 0 utility



##============================================================================##
## Model setup: create dataframe

# Create an empty data frame w/ 1 column for researcher ID, one for the
# submission threshold of generation 0 and one for each subsequent 
# generation's submission threshold (hence "2 + generations")
df_loop <- as.data.frame(matrix(ncol = 2 + generations, 
                                nrow = length(researcher_ID)))

# Rename the submission threshold variables to 0 - 100
names(df_loop) <- c("researcher_ID", as.character(0:generations))

df_loop$researcher_ID <- researcher_ID # fill in the researcher ID column
df_loop$`0` <- runif(length(researcher_ID), 0,1) # initialise submission threshold of generation 0

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
                             submission_threshold = df_loop[,1+i],
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
    stopifnot(sd(research$new_payoff[which(research$submit_as_RR == 1)]) == 0)
    
    # 1.2   Update payoff: 
    #       add the payoffs of this round to the previous amount
    payoff <- payoff + research$new_payoff
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
                             submission_threshold_parent = df_loop[,1+i])
  
  # Check: Stop the loop if any of the selected submission threshold is NA
  stopifnot(all(!is.na(selection$submission_threshold_selected)))
  
  # 2.3 Mutation: 
  #     We use mutation.fun to generate the evolved submission thresholds
  #     of the new generation
  evolution <- mutation.fun(n = length(researcher_ID),
                            submission_threshold_selected = 
                              selection$submission_threshold_selected,
                            mutation_sd = mutation_sd)
  
  # Put the new generations' evolved submission thresholds into the data frame:
  df_loop[,1+i+1] <- evolution$submission_threshold_child
}

##============================================================================##
## Prepare the plot:
# Turn the data frame into long format with numbers 0-100 for generation 
# and one variable for evolving submission thresholds
df_plot <- melt(df_loop,
                id.vars = "researcher_ID",
                variable.name = "generation",
                value.name = "submission_threshold")
df_plot$generation <- as.numeric(as.character(df_plot$generation))

##----------------------------------------------------------------------------##
## Plot the submission thresholds
# Basic plot setup:
basic_plot <- ggplot(
  df_plot,
  aes(x = generation,
      y = submission_threshold,
      # group = generation
  )) +
  scale_x_continuous(breaks = seq(0, generations, generations / 10),
                     name = "generation") +
  scale_y_continuous(lim = c(0, 1),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold") +
  theme_bw() +
  ggtitle(paste(generations, "generations", sep = " "),
          subtitle = paste(length(researcher_ID), " researchers, ",
                           generation_duration, 
                           ifelse(generation_duration ==1, 
                                  " round", " rounds"), "/generation, ",
                           "mutation size = ", mutation_sd, " SD", "\n",
                           "rewards: RR = ", reward_RR, 
                           ", normal pos. = ", reward_normal_pos,
                           ", normal neg. = ", reward_normal_neg, "\n",
                           "costs: RR = ", cost_RR, 
                           ", normal = ", cost_normal, "\n",
                           "prior distribution: ", 
                           ifelse(prior.dist == "uniform", 
                                  prior.dist, paste(prior.dist, ", m = ", 
                                                    prior.dist.m, ", sd = ",
                                                    prior.dist.sd, sep = "")),
                           "\n",
                           "fitness function: ", utility_shape, ", e = ", e, 
                           ", a = ", a, "; threshold = ", survival_threshold,
                           sep = ""))

# Output a plot with median (dashed blue line), mean (solid red-ish line),
# and 95% coverage (from quantile .025 to quantile .975, shaded light blue)
basic_plot +
  stat_summary(geom = "ribbon", 
               fun.data = median_hilow, fill = "lightskyblue1") +
  stat_summary(geom = "line", fun = median, 
               linetype="dashed", colour = "dodgerblue4")+
  # stat_summary(geom = "ribbon", fun.data = mean_cl_boot, fill = "salmon") +
  stat_summary(geom = "line", fun = mean, colour="salmon4")








##============================================================================##
## OLD STUFF BELOW: 
## Old visualisation plotting all data from every 10th generation in a dot plot
eleven_generations <- seq(0, generations, generations/10)

df_plot_subset <- df_plot[df_plot$generation == eleven_generations[1] |
                            df_plot$generation == eleven_generations[2] |
                            df_plot$generation == eleven_generations[3] |
                            df_plot$generation == eleven_generations[4] |
                            df_plot$generation == eleven_generations[5] |
                            df_plot$generation == eleven_generations[6] |
                            df_plot$generation == eleven_generations[7] |
                            df_plot$generation == eleven_generations[8] |
                            df_plot$generation == eleven_generations[9] |
                            df_plot$generation == eleven_generations[10] |
                            df_plot$generation == eleven_generations[11], ]

# Basic plot setup:
basic_plot <- ggplot(
  df_plot_subset,
  aes(x = generation,
      y = submission_threshold,
      # group = generation
  )) +
  scale_x_continuous(breaks = seq(0, generations, generations / 10),
                     name = "generation") +
  scale_y_continuous(lim = c(0, 1),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold") +
  theme_bw() +
  ggtitle(paste(generations, "generations", sep = " "),
          subtitle = paste("population size: ", length(researcher_ID), "\n", 
                           "generation length: ", generation_duration, 
                           ifelse(generation_duration ==1, " round", " rounds"), "\n",
                           "mutation size: ", mutation_sd, " SD", "\n",
                           "fitness function: ", utility_shape, ", e = ", e, 
                           ", a = ", a, "; threshold = ", survival_threshold,
                           sep = ""))

# Show plot with jitter and regression line:
basic_plot +
  geom_jitter(alpha = 0.5, width = generations*.03, height = .001) +
  geom_smooth(method = "lm", se = TRUE)






