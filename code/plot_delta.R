library(ggplot2)
library(here)
library(data.table)

# load delta data
data_delta <- readRDS(here("data", "data_delta.RData"))
       
# calculate median submission threshold of every run
data_delta_summary <- data_delta[, median(submission_threshold), 
                           by = .(run_id, generation_duration, 
                                  payoff_SR_neg, payoff_SR_pos, 
                                  payoff_RR,
                                  epsilon, survival_threshold,
                                  relative_top_n)]
colnames(data_delta_summary)[colnames(data_delta_summary) == "V1"] <- "median"


# calculate median of every run median per condition for the tile plot
data_delta_tile <- data_delta_summary[, median(median), 
                              by = .(generation_duration, 
                                     payoff_SR_neg, 
                                     payoff_SR_pos, 
                                     payoff_RR,
                                     epsilon, survival_threshold,
                                     relative_top_n)]
colnames(data_delta_tile)[colnames(data_delta_tile) == "V1"] <- "median"

## turn variable epsilon into a factor and rename the factor levels in order to
## change the facet labels in the plot
## EDIT: THIS STEP IS NOT NECESSARY -- facet labels are now added within 
## facet_grid in the plot
# data_delta_tile$epsilon <- as.factor(data_delta_tile$epsilon)
# levels(data_comp_tile$epsilon) <- c("decreasing returns\n(epsilon = 0.2)",
#                                     "linear\n(epsilon = 1)",
#                                     "increasing returns\n(epsilon = 5)")


## turn variable generation_duration into a factor to facilitate plotting
## (on a numeric scale, the levels would have gaps between them) 
data_delta_tile$generation_duration <- as.factor(data_delta_tile$generation_duration)


## turn variable survival_threshold into a factor and rename the factor levels 
## in order to change the facet labels in the plot (necessary to do this here 
## because we have to reverse the levels of the facet in facet_grid so that 
## delta = 0 is at the bottom, and for some reason doing this is incompatible with
## also renaming the facet labels within facet_grid -- also, no, reordering the
## factor labels here first doesn't work)
data_delta_tile$survival_threshold_numeric <- data_delta_tile$survival_threshold
data_delta_tile$survival_threshold <- as.factor(data_delta_tile$survival_threshold)
levels(data_delta_tile$survival_threshold) <- c("no threshold\n(delta = 0)", 
                                                "25% threshold\n(delta = .25)", 
                                                "50% threshold\n(delta = .5)",
                                                "75% threshold\n(delta = .75)")


viridis_option <- "mako" # set colour scheme

plot_delta_tile <- ggplot(data_delta_tile, 
                          aes(x = payoff_RR,
                              y = generation_duration,
                              fill = median)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[RR]),
                     breaks = seq(.1, .9, .1),
                     labels = c(".1", ".2", ".3", ".4", ".5", 
                                ".6", ".7", ".8", ".9"))+
  scale_y_discrete(expand = c(0,0), 
                   name = "research cycles before evaluation (m)")+
  coord_fixed(ratio = 1/10) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,0), "mm"),
        legend.title = element_text(hjust = 0.2)) +
  geom_tile() +
  geom_segment(x = data_delta_tile$survival_threshold_numeric - .05, 
               xend = data_delta_tile$survival_threshold_numeric - .05,
               y = 0, yend = max(as.numeric(data_delta_tile$generation_duration))+1,
               colour = "#FFEA00") +
  scale_fill_viridis_c(name = "s", limits = c(0,1), option = viridis_option)+ 
  facet_grid(factor(survival_threshold,
                    levels = rev(levels(survival_threshold))) ~ epsilon,
             labeller = labeller(epsilon = c(
                        `0.2` = "decreasing returns\n(epsilon = 0.2)",
                        `1` = "linear\n(epsilon = 1)",
                        `5` = "increasing returns\n(epsilon = 5)")))
#facet_grid(epsilon ~ survival_threshold) # to plot facets the other way around

ggsave(here("plots", paste("plot_delta_tile_evo_epsilon_", viridis_option, ".png", sep = ""), plot_delta_tile), bg = "white",
       #width = 21, height = 10, # setting when plotting facets the other way round
       width = 18, height = 15, 
       units = "cm")

