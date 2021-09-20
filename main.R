## Setup: load packages
library(reshape2)
library(ggplot2)

##---------------------------------------------------------------------##
## Model setup: initial parameters

# Payoffs:
payoff_RR <- 0.5  #payoff for a Registered Report (fixed)
payoff_normal_pos <- 1  #payoff for a positive result published as a normal paper
payoff_normal_neg <- 0  #payoff for a negative result published as a normal paper

# Error rates for hypothesis tests
# alpha_error <- 0  #alpha error, perhaps for later
# beta_error <- 0  #beta error, perhaps for later

# Population size & number of generations
researcher_ID <-  seq(1:1e4) # initialise researcher population & set pop size
generations <- 100 # set number of generations the model will loop through

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
  
  ## 1. Research phase
  
  # 1.1 Each researcher is assigned a hypothesis with a random prior probability 
  prior <- runif(length(researcher_ID), 0,1) 
  
  # 1.2 Researchers decide to run an RR or a normal study by comparing their
  #     current prior with their current submission threshold
  submit_as_RR <- ifelse(prior < df_loop[,1+i], 1, 0) 
  
  # 1.3 Researcheres test their hypotheses and get either a positive or a 
  #     negative result (based on their prior)
  result <- rbinom(length(prior), 1, prior)
  
  # 1.4 Researchers get a payoff depending on their chosen submission format
  #     and the result of their hypothesis test
  payoffs <- ifelse(submit_as_RR == 1, payoff_RR,
                            ifelse(result == 1, 
                                   payoff_normal_pos, payoff_normal_neg))
  
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
  evolved_submission_threshold <- round(evolved_submission_threshold + 
                                          rnorm(length(researcher_ID), 
                                                0, 0.02), 10) 
  # Truncate the mutated numbers so that they're between 0 and 1
  evolved_submission_threshold <- pmin(pmax(evolved_submission_threshold, 0), 1)
  
  # Put the new generations' evolved submission thresholds into the data frame:
  df_loop[,1+i+1] <- evolved_submission_threshold
}

#-----------------------------------------------------------------------#
# Turn the data frame into long format with numbers 0-100 for generation 
# and one variable for evolving submission thresholds
df_loop <- melt(df_loop,
                id.vars = "researcher_ID",
                variable.name = "generation",
                value.name = "submission_threshold")

#-----------------------------------------------------------------------#
# Plot the submission thresholds of every 10th generation:
ggplot(
  df_loop[as.numeric(as.character(df_loop$generation)) == seq(0,generations,10),],
  # df_loop,
  aes(x = as.numeric(as.character(generation)), y = submission_threshold)) +
  geom_jitter(alpha = 0.5, width = 3, height = .001)+
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(
    breaks = seq(0,generations,10),
    name = "generation") +
  scale_y_continuous(lim=c(0,1), 
                     breaks = seq(0,1,.1),
                     name = "submission threshold")+
  theme_bw(base_size = 14)





###===================================================================###
### Below here is old code that we first wrote to set up the model    ###
### without the loop (i.e. just two generations)                      ###
###-------------------------------------------------------------------###

researcher_ID <-  seq(1:1e4) #initialise first researcher population
prior_hyp <- runif(length(researcher_ID), 0,1) #initialise priors for each researcher's hypothesis, uniformly distributed between 0 and 1
submission_threshold <- runif(length(researcher_ID), 0, 1) #initialise 
payoffs <- rep(NA, length(researcher_ID))
# alpha <- rep(alpha_error, length(researcher_ID))
# beta <-  rep(beta_error, length(researcher_ID))

df <- data.frame(researcher_ID = researcher_ID,
                 prior = prior_hyp,
                 submission_threshold = submission_threshold, 
                 payoffs = payoffs)

#-----------------------------------------------------------------------#
### potential extension for later, for now commented out: calculating
### positive vs negative result based not only on the prior, but also
### the alpha and beta error
### IMPORTANT:  if used, df$result has to be changed to 
###             rbinom(length(df$pos_result_prob), 1, df$pos_result_prob)
##df$pos_result_prob <- (df$alpha*(1-df$prior)) + ((1-df$beta)*df$prior)

#will_i_submit_as_normal_study <- df$prior < df$submission_threshold

### SETTING SUBMISSION THRESHOLD: Connecting priors with publication format
### CAUTION:  it matters which way round this is programmed (whether
###           people submit RRs when their priors is BELOW or ABOVE
###           the submission threshold)
df$submit_as_RR <- ifelse(df$prior < df$submission_threshold, 1, 0)

# #payoffs as expected value
# df$payoffs <- ifelse(df$submit_as_RR == 1, payoff_RR,
#                      df$prior * payoff_normal_pos + (1-df$prior) * payoff_normal_neg)

#payoffs as either positive result OR negative result
df$result <- rbinom(length(df$prior), 1, df$prior)  # gives researchers a positive result (1) with probability prior, otherwise 0
df$payoffs <- ifelse(df$submit_as_RR == 1, payoff_RR,
                     ifelse(df$result == 1, 
                            payoff_normal_pos, payoff_normal_neg))

# calculate fitness and manage reproduction
df$fitness <- df$payoffs/sum(df$payoffs)

#might wanna call this soemthiung else for clairty
submission_threshold2 <- sample(df$submission_threshold, size = length(researcher_ID), replace = TRUE, prob = df$fitness)

#change sd to change size of mutations
submission_threshold2 <- round(submission_threshold2 + rnorm(length(researcher_ID), 0, 0.02)) 
submission_threshold2 <- pmin(pmax(submission_threshold2, 0), 1)

#let's see what selection favors
hist(df$submission_threshold - submission_threshold2)




  
  
  
