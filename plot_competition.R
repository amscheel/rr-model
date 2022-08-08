library(ggplot2)
library(here)
library(data.table)


# load competition data
data_comp <- readRDS(here("data", "data_competition.RData"))

# calculate median submission threshold of every run
data_comp_summary <- data_comp[, median(submission_threshold), 
                               by = .(run_id, generation_duration, 
                                      payoff_SR_neg, payoff_SR_pos, 
                                      payoff_RR,
                                      epsilon, survival_threshold,
                                      relative_top_n)]
data_comp_summary <- z_summary
# calculate median of every run median per condition for the tile plot
data_comp_tile <- data_comp_summary[, median(V1), 
                               by = .(generation_duration, 
                                      payoff_SR_neg, 
                                      payoff_SR_pos, 
                                      payoff_RR,
                                      epsilon, survival_threshold,
                                      relative_top_n)]
colnames(data_comp_tile)[colnames(data_comp_tile) == "V1"] <- "median"

# turn variable epsilon into a factor and rename the factor levels in order to
# change the facet labels in the plot
data_comp_tile$epsilon <- as.factor(as.character(data_comp_tile$epsilon))
levels(data_comp_tile$epsilon) <- c("epsilon = 0.2", "epsilon = 1", "epsilon = 5")

# turn variable generation_duration into a factor, reorder the factor levels
# (they get mixed up), and rename the levels to change the facet labels 
# in the plot
data_comp_tile$generation_duration <- as.factor(as.character(
  data_comp_tile$generation_duration))
data_comp_tile$generation_duration <- factor(data_comp_tile$generation_duration,
                                             levels = c("1", "2", "4", 
                                                        "8", "16"))
levels(data_comp_tile$generation_duration) <- c("m = 1", "m = 2", "m = 4",
                                                "m = 8", "m = 16")

# save this summary dataset for easier loading/reproduction later
saveRDS(data_comp_tile, "data_comp_tile_forplotting.RData")

#data_comp_tile <- readRDS(here::here("data", 
#                                     "data_comp_tile_forplotting.RData"))

plot_comp <- ggplot(data_comp_tile, aes(x = payoff_RR,
                                   y = relative_top_n,
                                   fill = median)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[RR]),
                     breaks = seq(.1, .9, .1),
                     labels = c(".1", "", ".3", "", ".5", "", ".7", "", ".9"))+
  scale_y_reverse(expand = c(0,0), name = 
                    expression(paste("competition (", gamma, ")", "")),
                  breaks = seq(.1, 1, .1),
                  labels = c(".1", "", ".3", "", ".5", 
                             "", ".7", "", ".9", ""))+
  coord_fixed(ratio = 9/10) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,1), "mm"),
        legend.title.align=0.2) +
  geom_tile() +
  scale_fill_viridis_c(name = "s", limits = c(0,1), option = "magma")+ 
  facet_grid(e ~ generation_duration)

ggsave("plot_comp.png", plot_comp, bg = "white",
       width = 17.8, height = 10, units = "cm")
