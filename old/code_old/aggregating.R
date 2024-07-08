testdf <- data.frame(run = c(rep(1, 10), rep(2, 10), rep(3, 10)),
                     id = rep(c(1:10), 3),
                     factor1 = "a",
                     factor2 = "c",
                     outcome1 = c(runif(10, 0, 1),
                                  runif(10, 0, 1),
                                  runif(10, 0, 1))
                     )

testdf <- rbind(testdf, data.frame(run = c(rep(1, 10), rep(2, 10), rep(3, 10)),
                         id = rep(c(1:10), 3),
                         factor1 = "b",
                         factor2 = "c",
                         outcome1 = c(runif(10, 0, 1),
                                      runif(10, 0, 1),
                                      runif(10, 0, 1))
))

reshape2::dcast(testdf, run ~ factor1, mean)
testdf <- reshape2::dcast(masterdf, run ~ prior_dist, mean)

#------------------------------------------------------------------------------#
# testdf <- masterdf[c(sample(1:nrow(masterdf), 500000)),]
testdf <- masterdf
testdf$utility_shape <- as.factor(testdf$utility_shape)
testdf$prior_dist <- as.factor(testdf$prior_dist)
testdf$survival_threshold <- as.factor(testdf$survival_threshold)
testdf$generation_duration <- as.factor(testdf$generation_duration)

testrecast <- reshape2::recast(testdf, run + reward_RR + prior_dist + 
                                 generation_duration +  utility_shape + 
                                 survival_threshold ~ variable, mean,
                               measure.var = "submission_threshold")
names(testrecast)[7] <- "submission_threshold_mean"

testrecast$submission_threshold_median <- reshape2::recast(
  testdf, run + reward_RR + prior_dist + generation_duration + utility_shape + 
    survival_threshold ~ variable, median,
  measure.var = "submission_threshold")$submission_threshold

testrecast$submission_threshold_var <- reshape2::recast(
  testdf, run + reward_RR + prior_dist + generation_duration + utility_shape + 
    survival_threshold ~ variable, var,
  measure.var = "submission_threshold")$submission_threshold

testrecast$submission_threshold_sd <- reshape2::recast(
  testdf, run + reward_RR + prior_dist + generation_duration + utility_shape + 
    survival_threshold ~ variable, sd,
  measure.var = "submission_threshold")$submission_threshold

head(testrecast)

#------------------------------------------------------------------------------#
rerecast <- reshape2::recast(testrecast, reward_RR + prior_dist + 
                               generation_duration +  utility_shape + 
                               survival_threshold ~ variable, mean,
                             measure.var = "submission_threshold_mean")
head(rerecast)
#------------------------------------------------------------------------------#

plot_threshold <- 2.4
plot_generation <- 4
plot_priordist <- "normal" 
plot_utility <- "linear"

rewarddf <- testrecast[which(testrecast$survival_threshold == plot_threshold & 
                             testrecast$generation_duration == plot_generation &
                             testrecast$prior_dist == plot_priordist),]

reward_plot <- ggplot(
  rewarddf,
  aes(x = reward_RR,
      y = submission_threshold_mean,
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
  # geom_jitter(alpha = 0.3, width = .01, height = .005) +
  geom_point(alpha = 0.2, size = .5,
             position = position_jitterdodge(jitter.width = .01,
                                                jitter.height = .005, 
                                                dodge.width = .2)) +
  stat_summary(fun.data = median_hilow
               # , colour = "dodgerblue4"
               # , geom = "crossbar"
               # , width = .15
               # , colour = "black"
               # , fill = "white"
               # , fill = "black"
               , position = position_dodge(width = .2)
               # , position = position_nudge(x = .01)
  ) +
  stat_summary(geom = "line", fun = median
               # , linetype="dashed"
               , position = position_dodge(width = .2)) 

#------------------------------------------------------------------------------#

reward_sd_plot <- ggplot(
  rewarddf,
  aes(x = reward_RR,
      y = submission_threshold_sd,
      group = utility_shape,
      colour = utility_shape,
  )) +
  scale_x_discrete(name = "reward_RR") +
  scale_y_continuous(lim = c(0, .25),
                     breaks = seq(0, .25, .02),
                     name = "SD of submission threshold") +
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

reward_sd_plot +
  geom_point(alpha = 0.2, size = .5,
             position = position_jitterdodge(jitter.width = .01,
                                             jitter.height = .0001, 
                                             dodge.width = .2)) +
  stat_summary(aes(x = reward_RR, y = submission_threshold_var), 
               geom = "point", fun = function(x) sqrt(mean(x)),
               size = 2,
               position = position_dodge(width = .2)) +
  stat_summary(aes(x = reward_RR, y = submission_threshold_var), 
               geom = "line", fun = function(x) sqrt(mean(x)),
               position = position_dodge(width = .2))

