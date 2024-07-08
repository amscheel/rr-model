##============================================================================#
## This script runs and plots the outcomes of a simulation that stores the 
## evolved submission thresholds of every generation
##============================================================================##
##----------------------------------------------------------------------------##
## Basic setup: load model functions and required packages
source("model_functions_outcomeST.R") # load model functions
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Fixed model parameters: These are kept constant for all simulations

# Population size & number of generations
pop_size <-500 # population size
generations <- 250 # set number of generations the model will loop through
runs <- 10 # number of times each model will be run
mutation_sd <- .01 # amount of noise added by mutations during evolution

# Parameters of the prior distribution:
prior_dist <- "uniform" # distribution type: uniform or normal
prior_dist_m <- 0.2 # mean of the prior distribution IF prior_dist = "normal"
prior_dist_sd <- 0.2 # sd of the prior distribution IF prior_dist = "normal"

##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Varying model parameters: these are the variables of interest

# 1. Number of research rounds in each generation:
generation_duration <- 1

# 2. Payoffs: 
#   We define the payoff researchers get for their studies as 
#   payoff = reward - cost.
# Set the payoffs for RRs and standard reports:
payoff_SR_neg <- 0  # payoff for a negative result in a standard report
payoff_SR_pos <- 1  # payoff for a positive result in a standard report, relative to negative payoff (i.e., this determines the variance): payoff_SR_pos = payoff_SR_neg + relative_payoff_SR_pos
payoff_RR <- c(.2, .5, .8) # payoff for a Registered Report, relative to SR payoffs (payoff_SR_pos + payoff_SR_neg) * relative_payoff_RR

# 3. Parameters of the fitness function:
e <- 1  # exponent of x to influence the shape
survival_threshold <- 0  # threshold below which all payoffs have 0 fitness, relative to SR payoff and generation duration:
# survival_threshold = relative_survival_threshold * generation_duration * (payoff_SR_neg + payoff_SR_pos) 
# I.e., 0 means no surivial threshold, values below 0.5 mean the theshold is less than half of the expected payoff from SRs (i.e., the average SR payoff) per round, and values above 1 mean the threshold is more than half of the expected payoff from RRs.

# 4. Competition: 
top_n <- pop_size # this number determines the size of the bottleneck: when top_n < pop_size, only those researchers with the n top fitness values will be selected for reproduction.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Prepare the simulation: 

# Create a dataframe with all combinations of the varying model 
# parameters specified above:
# generation's submission threshold (hence "2 + generations")


# Rename the submission threshold variables to 0 - 100

x <- expand.grid(payoff_RR = payoff_RR, e = e, run = runs)
df_loop <- as.data.frame(matrix(ncol = ncol(x) + generations + 1, 
                                nrow = pop_size))
names(df_loop) <- c(colnames(x), as.character(0:generations))


# Model setup: initialise a list in with the simulation results will be stored
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Here comes the simulation:
sim_evo <- df_loop[1,]

for (i in 1:length(payoff_RR)) {
  
  b_RR <- payoff_RR[i]
  
  for (j in 1:runs) {
    
    # Each run starts with a random allocation of submission thresholds
    df <- df_loop
    df$payoff_RR <- b_RR
    df$run <- j
    df$`0` <-  runif(pop_size, 0,1)
    
    for (k in 1:generations) {
      # gen_output_new <- data.frame(df_evo[i,], run = rep(j, pop_size), 
      #                              gen = rep(k, pop_size), 
      #                              submission_threshold = rep(NA, pop_size))
      # 
        # 1.1 We use research.fun to "do the research":
        #     Assign priors, compare with submission thresholds, 
        #     decide who submits RRs and who doesn't, 
        #     and calculate the resulting payoffs
        payoff <- research.fun(n = pop_size, prior.dist = prior_dist,
                                   prior.dist.m = prior_dist_m,
                                   prior.dist.sd = prior_dist_sd,
                                   submission.threshold = 
                                 df[,3+k],
                                   payoff.SR.neg = payoff_SR_neg,
                                   payoff.SR.pos = payoff_SR_pos,
                                   payoff.RR = b_RR)
      
      ## 2. Evolution phase:
      
      # 2.1 Use fitness.fun to calculate researchers' fitness from their payoffs
      fitness <- fitness.fun(x = payoff, e = e,
                             survival.threshold = survival_threshold)
      
      # 2.2 Selection: 
      #     We use selection.fun to calculate whose traits are passed on to the
      #     next generation
      selection <- selection.fun(n = pop_size, top.n = top_n,
                                 fitness = fitness, 
                                 submission.threshold.parent =
                                   df[,3+k])
      
      # Check: Stop the loop if any of the selected submission threshold is NA
      stopifnot(all(!is.na(selection)))
      
      # 2.3 Mutation: 
      #     We use mutation.fun to generate the evolved submission thresholds
      #     of the new generation
      submission_threshold <- mutation.fun(n = pop_size,
                                           submission.threshold.selected =
                                             selection,
                                           mutation.sd = mutation_sd)
      
      df[,3+k+1] <- submission_threshold
      
      # gen_output_new$submission_threshold <- submission_threshold
      # gen_output <- rbind(gen_output, gen_output_new)
    }
    sim_evo <- rbind(sim_evo, df)
  }
}

x <- sim_evo[-1,]
x <- data.table::data.table(x)
x <- data.table::melt.data.table(x, measure.vars = c(4:generations+4), variable.name = "gen", value.name = "submission_threshold")


evo_summary <- x[, median(submission_threshold), by = .(payoff_RR, run, gen)]
colnames(evo_summary)[4] <- "median"
evo_summary$IQR <- x[, IQR(submission_threshold), by = .(payoff_RR, run, gen)]$V1
evo_summary$IQR_lower <- ifelse(evo_summary$median-evo_summary$IQR/2>0,
                              evo_summary$median-evo_summary$IQR/2, 0)
evo_summary$IQR_upper <- ifelse(evo_summary$median+evo_summary$IQR/2<1,
                              evo_summary$median+evo_summary$IQR/2, 1)

# save summary data for easier loading to reproduce the plot later
saveRDS(evo_summary, "data_evo_summary_forplotting.RData")

# load evo data
evo_summary <- readRDS(here::here("data", 
                                  "data_evo_summary_forplotting.RData"))

# make a custom colour palette
evocolours3 <- wesanderson::wes_palette("GrandBudapest1", 7,
                                     type = "continuous")[c(1,2,4)]
evocolours3[1] <- "#EFC16A"
#evocolours3 <- viridisLite::rocket(3, begin = 0.6, end = 0.85, direction = -1)

# plot
plot_evo <- ggplot(evo_summary,
  aes(x = as.numeric(gen), y = median)) +
  scale_x_continuous(breaks = seq(0, 250, 50),
                     name = "generation", expand = c(0,0)) +
  scale_y_continuous(lim = c(0, 1),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold (s)", expand = c(0,0)) +
  scale_colour_manual(values = evocolours3, name = expression(italic(b)[RR]),
                      guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = evocolours3, guide = "none") +
  theme_light() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin=grid::unit(c(3,0,1,0), "mm")) +
  coord_fixed(ratio = 200/1) +
  geom_line(aes(group = interaction(run, payoff_RR), 
                colour = factor(payoff_RR)),
            linewidth = .5, alpha = .5)+
  geom_ribbon(aes(ymin = IQR_lower, ymax = IQR_upper, 
                  group = interaction(run, payoff_RR), 
                  fill = factor(payoff_RR)), alpha = 0.11) +
  stat_summary(aes(group = payoff_RR, colour = factor(payoff_RR)), 
               geom = "line", fun = median, linewidth = 1.5) 

ggsave(here::here("plots", "plot_evo.png"), plot_evo, width = 12.8, height = 9, units = "cm")
#ggsave("plot_evo.7.png", plot_evo, scale = .7)

