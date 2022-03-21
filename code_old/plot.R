## Setup: load packages
library(reshape2)
library(ggplot2)

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

