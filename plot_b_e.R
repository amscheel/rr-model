library(ggplot2)
library(here)
library(data.table)

# load b/e data
data_b_e <- readRDS(here("data", "data_b_e.RData"))

# calculate median submission threshold of every run
data_b_e_summary <- data_b_e[, median(submission_threshold), 
                                 by = .(run, generation_duration, 
                                        payoff_SR_neg, relative_payoff_SR_pos, 
                                        relative_payoff_RR,
                                        e, relative_survival_threshold,
                                        relative_top_n)]
colnames(data_b_e_summary)[colnames(data_b_e_summary) == "V1"] <- "median"

# save this summary dataset for easier loading/reproduction later
saveRDS(data_b_e_summary, "data_b_e_summary_forplotting.RData")

# define a colour scheme 
colorbrewer5 <- c("#2166ac", "#67a9cf", "grey", "#ef8a62", "#b2182b")

# plot
plot_b_e <- ggplot(data_b_e_summary,
  aes(x = factor(relative_payoff_RR),
      y = median,
      colour = factor(e),
      group = run)) +
  scale_x_discrete(name = expression(italic(b)[RR])) +
  scale_y_continuous(lim = c(0, 1), expand = c(0,0),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold (s)") +
  scale_colour_manual(values = colorbrewer5, name = expression(epsilon)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #text = element_text(size = 12),
        plot.margin=grid::unit(c(4,0,1,0), "mm"),
        legend.title.align=0.5)+
  guides(colour = guide_legend(override.aes = list(size = .5)))+
  coord_fixed(ratio = 9/1)+ 
  geom_hline(yintercept = .5, colour = "darkgrey") +
  #geom_vline(xintercept = 5, colour = "lightgrey") +
  geom_point(aes(group = factor(e)), 
             position = position_dodge(width = .3), size = .4, alpha = .2) +
  stat_summary(aes(x = as.numeric(factor(relative_payoff_RR))-0.1, 
                   group = factor(e)),
               geom = "line",
               fun = "median", 
               position = position_dodge(width = .3), size = .2) +
  stat_summary(aes(x = as.numeric(factor(relative_payoff_RR))-0.1, 
                   group = factor(e)),
               fun.data = "median_hilow", 
               position = position_dodge(width = .3),  size = .2)

ggsave("plot_b_e.png", plot_b_e, width = 10.5, height = 8.5, units = "cm")
