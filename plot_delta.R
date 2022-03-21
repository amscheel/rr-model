library(ggplot2)
library(here)

data_delta <- readRDS(here("data", "data_delta_bRR.5.RData"))
data_delta <- readRDS(here("data", "data_delta_bRR.5_m_allplus32.RData"))

y <- data_delta_summary
data_delta <- x

data_delta_summary <- data_delta[, median(submission_threshold), 
                           by = .(run, generation_duration, 
                                  payoff_SR_neg, relative_payoff_SR_pos, 
                                  relative_payoff_RR,
                                  e, relative_survival_threshold,
                                  relative_top_n)]
colnames(data_delta_summary)[colnames(data_delta_summary) == "V1"] <- "median"


# Basic plot setup:
plot_delta <- ggplot(
  data_delta_summary,
  aes(x = factor(relative_survival_threshold),
      y = median,
      colour = factor(e),
      #linetype = relative_survival_threshold,
      group = run)) +
  scale_y_continuous(lim = c(0, 1), expand = c(0,0),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold") +
  scale_colour_viridis_d(name = "epsilon") +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  geom_hline(yintercept = .5, colour = "darkgrey") +
  geom_point(aes(group = factor(e)), 
             position = position_dodge(width = .3),
             size = .5,
             alpha = .5) +
  stat_summary(aes(x = as.numeric(factor(relative_survival_threshold))-0.05, 
                   group = factor(e)),
               geom = "line",
               fun = "median", 
               position = position_dodge(width = .3),
               size = .2) +
  stat_summary(aes(x = as.numeric(factor(relative_survival_threshold))-0.05, 
                   group = factor(e)),
               fun.data = "median_hilow", 
               position = position_dodge(width = .3),
               size = .2)

plot_delta + facet_grid(.~ e)

plot_delta + facet_wrap(~ generation_duration, ncol = 3)


