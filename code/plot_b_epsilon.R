library(ggplot2)
library(here)
library(data.table)

# load b/e data
data_b_epsilon <- readRDS(here("data", "data_b_epsilon.RData"))
colnames(data_b_epsilon) <- c("generation_duration", "payoff_SR_neg",
                              "payoff_SR_pos", "payoff_RR",
                              "epsilon", "survival_threshold",
                              "relative_top_n", "run_id",
                              "publication_strategy")

# calculate median publication strategy of every run
data_b_epsilon_summary <- data_b_epsilon[, median(publication_strategy), 
                                 by = .(run_id, generation_duration, 
                                        payoff_SR_neg, payoff_SR_pos, 
                                        payoff_RR,
                                        epsilon, survival_threshold,
                                        relative_top_n)]
colnames(data_b_epsilon_summary)[colnames(data_b_epsilon_summary) == "V1"] <- "median"

# save this summary dataset for easier loading/reproduction later:
# saveRDS(data_b_epsilon_summary, "data_b_epsilon_summary_forplotting.RData")

# define a colour scheme 
colorbrewer5 <- c("#2166ac", "#67a9cf", "grey", "#ef8a62", "#b2182b")

# plot
plot_b_epsilon <- ggplot(data_b_epsilon_summary,
  aes(x = factor(payoff_RR),
      y = median,
      colour = factor(epsilon),
      group = run_id)) +
  scale_x_discrete(name = expression(italic(b)[R])) +
  scale_y_continuous(lim = c(0, 1), expand = c(0,0),
                     breaks = seq(0, 1, .1),
                     name = "publication strategy (s)") +
  scale_colour_manual(values = colorbrewer5, name = expression(epsilon)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #text = element_text(size = 12),
        plot.margin=grid::unit(c(4,0,1,0), "mm"),
        legend.title = element_text(hjust = 0.5))+
  guides(colour = guide_legend(override.aes = list(size = .5)))+
  coord_fixed(ratio = 9/1)+ 
  geom_hline(yintercept = .5, colour = "darkgrey") +
  geom_point(aes(group = factor(epsilon)), 
             position = position_dodge(width = .3), size = .4, alpha = .2) +
  stat_summary(aes(x = as.numeric(factor(payoff_RR))-0.1, 
                   group = factor(epsilon)),
               geom = "line",
               fun = "median", 
               position = position_dodge(width = .3), linewidth = .2) +
  stat_summary(aes(x = as.numeric(factor(payoff_RR))-0.1, 
                   group = factor(epsilon)),
               fun.data = "median_hilow", 
               position = position_dodge(width = .3),  size = .2)

ggsave(here::here("plots", "plot_epsilon_evo.png"), plot_b_epsilon, width = 10.5, height = 8.5, units = "cm")
