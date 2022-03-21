##============================================================================##
runs <- 50
reward_RR_settings <- c(.2, .5, .8)
#------------------------------------------------------------------------------#

rewardoutput <- list()


for (l in length(reward_RR_settings)) {
  
  reward_RR <- reward_RR_settings[l]
  output <- list()
  
  for (k in 1:runs) {
    
    # Model setup
    submission_threshold <- runif(length(researcher_ID), 0,1)
    finalgen <- list()
    
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
        
        research$total_payoff <- payoff # add total payoff to the "research" list
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
    }
    
    finalgen$reward_RR <- reward_RR
    finalgen$run <- k
    finalgen$researcher_ID <- researcher_ID
    finalgen$submission_threshold <- research$submission_threshold
    finalgen$prior <- research$prior
    finalgen$submit_as_RR <- research$submit_as_RR
    finalgen$payoff <- research$total_payoff
    finalgen$fitness <- selection$fitness
    
    output[[k]] <- finalgen
  }
  rewardoutput[[l]] <- data.table::rbindlist(output)
}

rewardoutput <- data.table::rbindlist(rewardoutput)
rewardoutput$utility_shape <- utility_shape
rewardoutput$survival_threshold <- survival_threshold
rewardoutput$generation_duration <- generation_duration
rewardoutput$prior_dist <- prior.dist


masterdf <- rbind(masterdf, rewardoutput)


##============================================================================##
# Plot how mean and median priors of RRs vs normal studies develop over time


masterdf$reward_RR <- as.factor(masterdf$reward_RR)
masterdf$submit_as_RR <- as.factor(masterdf$submit_as_RR)

save(masterdf, file = "masterdf.RData", compress = F)
saveRDS(masterdf, file = "masterdf.Rds", compress = F)

plot_threshold <- 1.6
plot_generation <- 4
plot_priordist <- "normal" 
plot_utility <- "diminishing"

rewarddf <- masterdf[which(masterdf$survival_threshold == plot_threshold & 
                             masterdf$generation_duration == plot_generation &
                             masterdf$prior_dist == plot_priordist),]

reward_plot <- ggplot(
  rewarddf,
  aes(x = reward_RR,
      y = submission_threshold,
      group = utility_shape,
      colour = utility_shape,
  )) +
  scale_x_discrete(name = "reward_RR") +
  scale_y_continuous(lim = c(0, 1),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold") +
  scale_color_viridis_d() +
  theme_bw() +
  ggtitle(paste("prior: ", plot_priordist, " distribution",
                ifelse(plot_priordist == "uniform", " ", 
                       paste(", m = ", prior.dist.m, ", sd = ",  prior.dist.sd,
                            sep = "")), sep = ""),
          subtitle = paste(plot_generation, 
                           ifelse(plot_generation ==1, 
                                  " round", " rounds"), "/generation", "\n",
                           "survival threshold = ", plot_threshold, "\n",
                           "rewards: normal pos. = ", reward_normal_pos,
                           ", normal neg. = ", reward_normal_neg, "\n",
                           "costs: RR = ", cost_RR, 
                           ", normal = ", cost_normal, sep = ""))

reward_plot +
  stat_summary(fun.data = median_hilow
               # , colour = "dodgerblue4"
               , position = position_dodge(width = .2)
               # , position = position_nudge(x = .01)
               ) +
  stat_summary(geom = "line", fun = median
               # , linetype="dashed"
               , position = position_dodge(width = .2)) 
# +
#   stat_summary(geom = "ribbon", fun.data = mean_cl_boot, fill = "salmon") +
#   stat_summary(geom = "line", fun = mean)

# Show plot with jitter and regression line:
reward_plot +
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE)



priordf <- masterdf[which(masterdf$survival_threshold == plot_threshold & 
                             masterdf$generation_duration == plot_generation &
                      masterdf$utility_shape == plot_utility &
                        masterdf$prior_dist == plot_priordist),]


prior_plot <- ggplot(
  priordf,
  aes(x = reward_RR,
      y = prior,
      group = submit_as_RR,
      colour = submit_as_RR,
  )) +
  scale_x_discrete(name = "reward_RR") +
  scale_y_continuous(lim = c(0, 1),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold") +
  scale_color_viridis_d() +
  theme_bw() +
  ggtitle(paste("prior: ", plot_priordist, " distribution",
                ifelse(plot_priordist == "uniform", " ", 
                       paste(", m = ", prior.dist.m, ", sd = ",  prior.dist.sd,
                             sep = "")), sep = ""),
          subtitle = paste(plot_generation, 
                           ifelse(plot_generation ==1, 
                                  " round", " rounds"), "/generation", "\n",
                           "survival threshold = ", plot_threshold, "\n",
                           "shape of utility function: ", plot_utility, "\n",
                           "rewards: normal pos. = ", reward_normal_pos,
                           ", normal neg. = ", reward_normal_neg, "\n",
                           "costs: RR = ", cost_RR, 
                           ", normal = ", cost_normal, sep = ""))

prior_plot +
  stat_summary(fun.data = median_hilow
               # , colour = "dodgerblue4"
               , position = position_dodge(width = .1)
               # , position = position_nudge(x = .01)
  ) +
  stat_summary(geom = "line", fun = median
               # , linetype="dashed"
               , position = position_dodge(width = .1)) 
