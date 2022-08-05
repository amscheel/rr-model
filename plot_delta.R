library(ggplot2)
library(here)
library(data.table)

# load delta data
data_delta <- readRDS(here("data", "data_delta.RData"))

# calculate median submission threshold of every run
data_delta_summary <- data_delta[, median(submission_threshold), 
                           by = .(run, generation_duration, 
                                  payoff_SR_neg, relative_payoff_SR_pos, 
                                  relative_payoff_RR,
                                  epsilon, survival_threshold,
                                  relative_top_n)]
colnames(data_delta_summary)[colnames(data_delta_summary) == "V1"] <- "median"

# turn variable generation_duration into a factor, reorder the factor levels
# (they get mixed up), and rename the levels to change the facet labels 
# in the plot
data_delta_summary$generation_duration <- as.factor(as.character(
  data_delta_summary$generation_duration))
data_delta_summary$generation_duration <- factor(
  data_delta_summary$generation_duration, 
  levels = c("1", "2", "4", "8", "16", "32"))
levels(data_delta_summary$generation_duration) <- c("m = 1", "m = 2", "m = 4",
                                                "m = 8", "m = 16", "m = 32")

# save this summary dataset for easier loading/reproduction later
saveRDS(data_delta_summary, "data_delta_summary_forplotting.RData")

# Create a subset of the data with only half of the m conditions, 
# m = c(1, 4, 16)
data_delta_subset <- data_delta_summary[
  data_delta_summary$generation_duration== "m = 1" | 
    data_delta_summary$generation_duration == "m = 4" | 
    data_delta_summary$generation_duration== "m = 16",]

# define a colour scheme 
colorbrewer3 <- c("#2166ac", "grey", "#b2182b")

# plot
plot_delta <- ggplot(data_delta_subset,
  aes(x = factor(survival_threshold),
      y = median,
      colour = factor(epsilon),
      group = run)) +
  scale_x_discrete(name = expression(paste("survival threshold (", 
                                delta, ") as proportion of ", 
                                italic(m), sep = ""))) +
  scale_y_continuous(lim = c(0, 1), expand = c(0,0),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold (s)") +
  scale_colour_manual(values = colorbrewer3, name = expression(epsilon)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin=grid::unit(c(0,0,0,1), "mm"),
        legend.title.align=0.5)+
  coord_fixed(ratio = 3/1)+ 
  geom_hline(yintercept = .5, colour = "darkgrey") +
  geom_point(aes(group = factor(epsilon)), 
             position = position_dodge(width = .3), size = .5, alpha = .2) +
  stat_summary(aes(x = as.numeric(factor(survival_threshold))-0.09, 
                   group = factor(epsilon)),
               geom = "line",
               fun = "median", 
               position = position_dodge(width = .3), size = .2) +
  stat_summary(aes(x = as.numeric(factor(survival_threshold))-0.09, 
                   group = factor(epsilon)),
               fun.data = "median_hilow", 
               position = position_dodge(width = .3), size = .2) + 
  facet_wrap(~ generation_duration, ncol = 3)

ggsave("plot_delta.png", plot_delta, bg = "white",
       width = 16, height = 5.7, units = "cm")




