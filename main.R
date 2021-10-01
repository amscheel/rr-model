## Setup: load packages
library(reshape2)
library(ggplot2)

##---------------------------------------------------------------------##
## Model setup: initial parameters

# Payoffs:
payoff_RR <- 0.5  #payoff for a Registered Report (fixed)
payoff_normal_pos <- 1  #payoff for a positive result published as a normal paper
payoff_normal_neg <- 0  #payoff for a negative result published as a normal paper
cost_RR <- 0 #cost of conducting a RR (subtracted from payoff)
cost_normal <- 0 #cost of publishing a normal paper (subtracted from payoff)

# Error rates for hypothesis tests
# alpha_error <- 0  #alpha error, perhaps for later
# beta_error <- 0  #beta error, perhaps for later

# Population size & number of generations
researcher_ID <-  seq(1:1e3) # initialise researcher population & set pop size
generations <- 1e3 # set number of generations the model will loop through
generation_duration <- 1
mutation_sd <- .01

#-----------------------------------------------------------------------#
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

#-----------------------------------------------------------------------#
## Here comes the loop:
for (i in 1:generations) {
  payoffs <- rep(0, length(researcher_ID))
  
  for (j in 1:generation_duration) {
  ## 1. Research phase
  
  # 1.1 Each researcher is assigned a hypothesis with a random prior probability 
  #     I made two versions: the first is a uniform distribution between 0 and 1,
  #     the second is a normal distribution with mean = .2, sd = .15, truncated
  #     to 0-1. Comment/un-comment depending on preference
  prior <- runif(length(researcher_ID), 0,1)
  # prior <- pmin(pmax(rnorm(length(researcher_ID), mean = .25, sd = 0.15), 0),1)
    # prior <- rep(1, length(researcher_ID))
  
  # 1.2 Researchers decide to run an RR or a normal study by comparing their
  #     current prior with their current submission threshold
  submit_as_RR <- ifelse(prior < df_loop[,1+i], 1, 0) 
  # mean(result) - sum(prior)X
  
  # 1.3 Researcheres test their hypotheses and get either a positive or a 
  #     negative result (based on their prior)
  result <- rbinom(length(prior), 1, prior)
  
  # 1.4 Researchers get a payoff depending on their chosen submission format
  #     and the result of their hypothesis test
  # payoffs = result
  # payoffs[submit_as_RR == 1] <- payoff_RR
  # 
  payoffs <- payoffs + ifelse(submit_as_RR == 1, payoff_RR-cost_RR,
                            ifelse(result == 1,
                                   payoff_normal_pos-cost_normal, payoff_normal_neg))

  }
  
  # 1.5 Researchers' fitness is calculated based on their payoff,
  #     relative to the population
  fitness <- payoffs/sum(payoffs)
  
  
  ## 2. Evolution phase
  
  # 2.1 Select whose submission threshold trait makes it into 
  #     the next generation, based on fitness
  evolved_submission_threshold <- sample(df_loop[,1+i], 
                                         size = length(researcher_ID), 
                                         replace = TRUE, prob = fitness)

  # 2.2 New submission thresholds mutate
  #     (change sd to change the size of mutations)
  evolved_submission_threshold <- evolved_submission_threshold + 
                                          rnorm(length(researcher_ID), 
                                                0, mutation_sd)
  
  # Truncate the mutated numbers so that they're between 0 and 1
  evolved_submission_threshold <- pmin(pmax(evolved_submission_threshold, 0), 1)
  
  # Put the new generations' evolved submission thresholds into the data frame:
  df_loop[,1+i+1] <- evolved_submission_threshold
}

#-----------------------------------------------------------------------#
# Turn the data frame into long format with numbers 0-100 for generation 
# and one variable for evolving submission thresholds
df_plot <- melt(df_loop,
                id.vars = "researcher_ID",
                variable.name = "generation",
                value.name = "submission_threshold")
df_plot$generation <- as.numeric(as.character(df_plot$generation))

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
##---------------------------------------------------------------------##
## Plot the submission thresholds of every 10th generation
# Basic plot setup:
basic_plot <- ggplot(df_plot_subset,
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
                           "mutation size: ", mutation_sd, " SD",
                           sep = ""))

# Show plot with jitter and regression line:
basic_plot +
  geom_jitter(alpha = 0.5, width = generations*.03, height = .001) +
  geom_smooth(method = "lm", se = TRUE)




