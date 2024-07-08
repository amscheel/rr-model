library(ggplot2)
library(here)
library(data.table)

# load m data
data_m <- readRDS(here("data", "data_m.RData"))

# calculate median submission threshold of every run
data_m_summary <- data_m[, median(submission_threshold), 
                         by = .(run_id, generation_duration, 
                                payoff_SR_neg, payoff_SR_pos, 
                                payoff_RR,
                                epsilon, survival_threshold,
                                relative_top_n)]

# calculate median of every run median per condition for the tile plot
data_m_tile <- data_m_summary[, median(V1), 
                              by = .(generation_duration, 
                                     payoff_SR_neg, 
                                     payoff_SR_pos, 
                                     payoff_RR,
                                     epsilon, survival_threshold,
                                     relative_top_n)]
colnames(data_m_tile)[colnames(data_m_tile) == "V1"] <- "median"

# turn variable generation_duration into a factor to facilitate plotting
# (on a numeric scale, the levels would have gaps between them) 
data_m_tile$generation_duration <- as.factor(data_m_tile$generation_duration)

# save this summary dataset for easier loading/reproduction later:
#saveRDS(data_m_tile, "data_m_tile_forplotting.RData")

viridis_option <- "rocket"
plot_m <- ggplot(data_m_tile, 
                 aes(x = payoff_RR,
                     y = generation_duration,
                     fill = median)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[RR]),
                     breaks = seq(.1, .9, .1),
                     labels = c(".1", ".2", ".3", ".4", ".5", 
                                ".6", ".7", ".8", ".9"))+
  scale_y_discrete(expand = c(0,0), 
                   name = "research cycles\nbefore evaluation (m)")+
  coord_fixed(ratio = 1/10) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,0), "mm"),
        legend.title = element_text(hjust = 0.2)) +
  geom_tile() +
  scale_fill_viridis_c(name = "s", limits = c(0,1), option = viridis_option)+ 
  facet_grid(. ~ epsilon,
             labeller = labeller(epsilon = c(
                 `0.2` = "decreasing returns\n(epsilon = 0.2)",
                 `1` = "linear\n(epsilon = 1)",
                 `5` = "increasing returns\n(epsilon = 5)")))

ggsave(paste("plot_m_evo_", viridis_option, ".png", sep = ""), 
       plot_m, bg = "white",
       width = 18, height = 6, units = "cm")

