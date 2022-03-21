##============================================================================##
## This is the improved code for running the model without the df_loop dataframe
##============================================================================##
source("model_functions.R")
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

generation_duration <- c(1, 5, 30) # number of research rounds in each generation

# Payoffs: 
#   We define the payoff researchers get for their studies as 
#   payoff = reward - cost.
# Set the payoffs for RRs and standard reports:
payoff_SR_neg <- c(0, 0.1)  # payoff for a negative result in a standard report
relative_payoff_SR_pos <- c(1, 50)  # payoff for a positive result in a standard report, relative to negative payoff (i.e., this determines the variance): payoff_SR_pos = payoff_SR_neg + relative_payoff_SR_pos
relative_payoff_RR <- c(0.8, 1, 1.2) # payoff for a Registered Report, relative to *average* SR payoff ((payoff_SR_pos + payoff_SR_neg)/2 * relative_payoff_RR)

# Parameters of the utility/fitness function:
a <- 1  # multiplier of x to influence the shape
e <- c(0.2, 1, 5)  # exponent of x to influence the shape
relative_survival_threshold <- c(0, 0.9, 1.1)  # threshold below which all payoffs have 0 utility, relative to average SR payoff over one generation:
# survival_threshold = relative_survival_threshold * generation_duration * ((payoff_SR_neg + payoff_SR_pos)/2) 

settings.df <- expand.grid(generation_duration = generation_duration,
                           payoff_SR_neg = payoff_SR_neg, 
                           relative_payoff_SR_pos = relative_payoff_SR_pos, 
                           relative_payoff_RR = relative_payoff_RR, 
                           a = a, e = e, 
                           relative_survival_threshold = relative_survival_threshold)
#------------------------------------------------------------------------------#
## Model setup: initialise lists and the submission_threshold variable

model <- list()
#------------------------------------------------------------------------------#
## Here comes the loop:

for (i in 1:nrow(settings.df)) {
  run.output <- list()
  
  generation_duration <- settings.df$generation_duration[i]
  
  # Set the payoffs for RRs and standard reports:
  payoff_SR_neg <- settings.df$payoff_SR_neg[i]  # payoff for neg. result in standard report
  payoff_SR_pos <- payoff_SR_neg + settings.df$relative_payoff_SR_pos[i]  # payoff for pos. result in standard report
  payoff_RR <- settings.df$relative_payoff_RR[i] * 
    ((payoff_SR_pos + payoff_SR_neg)/2) # payoff for Registered Report
  
  # Parameters of the utility/fitness function:
  a <- settings.df$a[i]  # multiplier of x to influence the shape
  e <- settings.df$e[i]  # exponent of x to influence the shape
  survival_threshold <- settings.df$relative_survival_threshold[i] * 
    generation_duration * ((payoff_SR_pos + payoff_SR_neg)/2)
    # threshold below which all payoffs have 0 utility
  
  for (j in 1:runs) {
    submission_threshold <- runif(pop_size, 0,1)
    
    for (k in 1:generations) {
      
      # initialise the payoff vector: before conducting any research, 
      # everybody starts with 0 payoff
      payoff <- rep(0, pop_size)
      
      ## 1. Research phase:
      for (l in 1:generation_duration) {
        
        # 1.1 We use research.fun to "do the research":
        #     Assign priors, compare with submission thresholds, decide who submits
        #     RRs and who doesn't, calculate results, and calculate payoffs
        research <- research.fun(prior_dist = prior_dist,
                                 prior_dist_m = prior_dist_m,
                                 prior_dist_sd = prior_dist_sd,
                                 submission_threshold = submission_threshold,
                                 payoff_RR = payoff_RR,
                                 payoff_SR_pos = payoff_SR_pos,
                                 payoff_SR_neg = payoff_SR_neg)
        
        # 1.2   Update payoff: 
        #       add the payoffs of this round to the previous amount
        payoff <- payoff + research$new_payoff
      }
      
      ## 2. Evolution phase:
      
      # 2.1 We use utility.fun to calculate researchers' fitness from their payoffs
      utility <- utility.fun(x = payoff, e = e, a = a, 
                             survival_threshold = survival_threshold)
      
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
                     payoff_SR_neg = payoff_SR_neg, 
                     relative_payoff_SR_pos = settings.df$relative_payoff_SR_pos[i],
                     relative_payoff_RR = settings.df$relative_payoff_RR[i], 
                     a = a, e = e, 
                     relative_survival_threshold = 
                       settings.df$relative_survival_threshold[i],
                     output = run.output)
}

saveRDS(model, "model_settings_2.RData")
saveRDS(settings.df, "settings_2.RData")
simdata_settings2 <- readRDS("model_settings_2.RData")

model <- simdata_settings2
# turn the model list into a data frame in long format:
model <- data.table::rbindlist(model) # unlist "model" with a remaining nested column (output)
model$id <- seq.int(nrow(model)) # create an id column to help merge the dfs later
output <- data.table::rbindlist(model$output, idcol = "id") # unlist the output column separately
model <- dplyr::left_join(model, output, by = "id") # merge the two dfs

model <- model[, c("generation_duration", 
                   "payoff_SR_neg", "relative_payoff_SR_pos",
                   "relative_payoff_RR",
                   "a", "e", "relative_survival_threshold",
                   "run", "submission_threshold")] # get rid of old nested column and id helper column

saveRDS(model, "model_settings_2.RData")


##----------------------------------------------------------------------------##
## Plot the submission thresholds
model_prep <- model
model_prep$relative_survival_threshold <- as.factor(as.character(model_prep$relative_survival_threshold))
model_prep$e <- as.factor(as.character(model_prep$e))
model_prep$run <- as.factor(as.character(model_prep$run))
model_prep$generation_duration <- as.factor(as.character(model_prep$generation_duration))
model_prep$relative_payoff_RR <- as.factor(as.character(model_prep$relative_payoff_RR))


model_plot <- model_prep[, median(submission_threshold), 
                         by = .(run, generation_duration, 
                                payoff_SR_neg, relative_payoff_SR_pos, 
                                relative_payoff_RR,
                                a, e, relative_survival_threshold)]
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
# plot_long <- data.table::melt(model_plot, id.vars = c("run", 
#                                                       "payoff_SR_neg", 
#                                                       "payoff_SR_pos", 
#                                                       "payoff_RR",
#                                                       "a", "e", 
#                                                       "survival_threshold"),
#                               variable.name = "stat",
#                               value.name = "submission_threshold")

#--- subsets of model with settings 1 to isolate gen duration conditions ---#
model_settings1_gen30 <- model_plot[model_plot$generation_duration==30]
model_settings1_gen30_payoff1 <- model_settings1_gen30[model_settings1_gen30$relative_payoff_SR_pos == 1]
model_settings1_gen30_payoff50 <- model_settings1_gen30[model_settings1_gen30$relative_payoff_SR_pos == 50]

model_settings1_gen1 <- model_plot[model_plot$generation_duration==1]
model_settings1_gen1_payoff1 <- model_settings1_gen1[model_settings1_gen1$relative_payoff_SR_pos == 1]
model_settings1_gen1_payoff50 <- model_settings1_gen1[model_settings1_gen1$relative_payoff_SR_pos == 50]

model_settings1_gen5 <- model_plot[model_plot$generation_duration==5]
model_settings1_gen5_payoff1 <- model_settings1_gen5[model_settings1_gen5$relative_payoff_SR_pos == 1]
model_settings1_gen5_payoff50 <- model_settings1_gen5[model_settings1_gen5$relative_payoff_SR_pos == 50]

#------------- previous model output with 1 round/generation ------------------#
payoff_range_1 <- model_plot[model_plot$relative_payoff_SR_pos == 1,]
payoff_range_10 <- model_plot[model_plot$relative_payoff_SR_pos == 10,]
payoff_range_100 <- model_plot[model_plot$relative_payoff_SR_pos == 100,]
##----------------------------------------------------------------------------##

#------------- previous model output with payoff range = 1---------------------#
gen_duration_1 <- model_plot[model_plot$generation_duration == 1,]
gen_duration_5 <- model_plot[model_plot$generation_duration == 5,]
##----------------------------------------------------------------------------##

# Basic plot setup:
basic_plot <- ggplot(
  model_settings1_gen30_payoff50,
  aes(x = e,
      y = median,
      colour = relative_survival_threshold,
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
                runs, " runs", sep = ""),
          subtitle = paste(generation_duration, ifelse(generation_duration ==1, 
                                                       " round", " rounds"), 
                           " per generation", "\n",
                           "prior odds distribution: ", 
                           ifelse(prior_dist == "uniform", "uniform [0, 1]",
                                  "normal with m = .2 and sd = .2"), "\n",
                           "payoff negative SR = ", payoff_SR_neg, "\n",
                           "payoff positive SR = 100.1", "\n",
                           #"payoff positive SR = ", payoff_SR_pos, "\n",
                           "payoff RR = ", payoff_RR, sep = "")) 

basic_plot + 
  geom_point(aes(group = relative_survival_threshold), 
             position = position_dodge(width = .3),
             size = .5,
             alpha = .5) +
  stat_summary(aes(x = as.numeric(e)-0.05, 
                   group = relative_survival_threshold),
               geom = "line",
               fun = "median", 
               position = position_dodge(width = .3),
               size = .2) +
  stat_summary(aes(x = as.numeric(e)-0.05, 
                   group = relative_survival_threshold),
               fun.data = "median_hilow", 
               position = position_dodge(width = .3),
               size = .2)


# Output a plot with median (dashed blue line), mean (solid red-ish line),
# and 95% coverage (from quantile .025 to quantile .975, shaded light blue)
basic_plot +
  # stat_summary(geom = "ribbon", 
  #              fun.data = median_hilow, 
  #              #fill = "lightskyblue1", 
  #              alpha = .2) +
  # stat_summary(aes(x = as.numeric(factor(e)) -.1),
  #              geom = "line", fun = median, colour = "grey", alpha = .5)+
  stat_summary(mapping = aes(x = as.numeric(e) -.1
                             , group = run, 
                             #colour = survival_threshold
                             ),
               geom = "point", fun = median, 
               #colour = "dodgerblue4", 
               size = .5)+
  stat_summary(mapping = aes(x = as.numeric(e) -.1
                             , y = model_plot[model_plot$survival_threshold == 0.8,]
                             , group = run, 
                             #colour = survival_threshold
  ),
  geom = "point", fun = median, 
  #colour = "dodgerblue4", 
  size = .5)+
  # stat_summary(geom = "ribbon", fun.data = mean_cl_boot, fill = "salmon") +
  # stat_summary(geom = "line", fun = mean, colour="salmon4") +
  stat_summary(fun.data = "median_hilow", 
               mapping = aes(x = as.numeric(e)
                             , group = e
               #               , group = survival_threshold
               )
               # ,  position = position_dodge(width = .1)
               ) 

##----------------------------------------------------------------------------##