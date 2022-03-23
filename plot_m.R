library(ggplot2)
library(here)
library(data.table)


# load m data
data_m <- readRDS(here("data", "data_m.RData"))

# calculate median submission threshold of every run
data_m_summary <- data_m[, median(submission_threshold), 
                         by = .(run, generation_duration, 
                                payoff_SR_neg, relative_payoff_SR_pos, 
                                relative_payoff_RR,
                                e, relative_survival_threshold,
                                relative_top_n)]

# calculate median of every run median per condition for the tile plot
data_m_tile <- data_m_summary[, median(V1), 
                              by = .(generation_duration, 
                                     payoff_SR_neg, 
                                     relative_payoff_SR_pos, 
                                     relative_payoff_RR,
                                     e, relative_survival_threshold,
                                     relative_top_n)]
colnames(data_m_tile)[colnames(data_m_tile) == "V1"] <- "median"

# turn variable e into a factor and rename the factor levels in order to
# change the facet labels in the plot
data_m_tile$e <- as.factor(as.character(data_m_tile$e))
levels(data_m_tile$e) <- c("epsilon = 0.2", "epsilon = 1", "epsilon = 5")

# turn variable generation_duration into a factor to facilitate plotting
# (on a numeric scale, the levels would have gaps between them) and  
# reorder the factor levels (they get mixed up)
data_m_tile$generation_duration <- as.factor(as.character(
  data_m_tile$generation_duration))
data_m_tile$generation_duration <- factor(data_m_tile$generation_duration,
                                          levels = c("1", "2", "4", 
                                                     "8", "16", "32"))

#saveRDS(data_m_tile, "data_m_tile_forplotting.RData")

plot_m <- ggplot(data_m_tile, 
                 aes(x = relative_payoff_RR,
                     y = generation_duration,
                     fill = median)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[RR]),
                     breaks = seq(.1, .9, .1),
                     labels = c(".1", ".2", ".3", ".4", ".5", 
                                ".6", ".7", ".8", ".9"))+
  scale_y_discrete(expand = c(0,0), 
                   name = "decision events before evaluation (m)")+
  coord_fixed(ratio = 1/10) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,0), "mm"),
        legend.title.align=0.2) +
  geom_tile() +
  scale_fill_viridis_c(name = "s", limits = c(0,1), option = "magma")+ 
  facet_grid(e ~ .)

ggsave("plot_m.png", plot_m, bg = "white",
       width = 10, height = 13.5, units = "cm")

