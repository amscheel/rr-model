################################################################################
####                                                                        ####
#### Script 5d: Script to produce Figure 6 (effects of survival thresholds) ####
####                                                                        ####
#### ---------------------------------------------------------------------- ####
#### Content:                                                               ####
####  * 1: Setup                                                            ####
####  * 2: Prepare data                                                     ####
####  * 3: Generate plot                                                    ####
####                                                                        ####
################################################################################

##============================================================================##
## 1. Setup
##============================================================================##

##----------------------------------------------------------------------------##
# This script requires the packages "here", "data.table" and "ggplot2".

# Install "here" unless it is already installed:
if(!require(here)){install.packages("here")}

# Install "data.table" unless it is already installed:
if(!require(data.table)){install.packages("data.table")}

# Install "ggplot2" unless it is already installed:
if(!require(ggplot2)){install.packages("ggplot2")}
##----------------------------------------------------------------------------##


##============================================================================##
## 2. Prepare data
##============================================================================##

##----------------------------------------------------------------------------##
## 2.1 Load data
data_delta <- readRDS(here::here("..", "rr-model", "data", "simdata_delta_evo.RData"))
colnames(data_delta) <- c("generation_duration", "payoff_SR_neg",
                              "payoff_SR_pos", "payoff_RR",
                              "epsilon", "survival_threshold",
                              "relative_top_n", "run_id",
                              "publication_strategy")

## 2.2 Calculate the median publication strategy (s) of every run
data_delta_summary <- data_delta[, median(publication_strategy), 
                           by = .(run_id, generation_duration, 
                                  payoff_SR_neg, payoff_SR_pos, 
                                  payoff_RR,
                                  epsilon, survival_threshold,
                                  relative_top_n)]
colnames(data_delta_summary)[colnames(data_delta_summary) == "V1"] <- "median"

## 2.3 Calculate the median of every run median per condition for the tile plot
data_delta_tile <- data_delta_summary[, median(median), 
                              by = .(generation_duration, 
                                     payoff_SR_neg, 
                                     payoff_SR_pos, 
                                     payoff_RR,
                                     epsilon, survival_threshold,
                                     relative_top_n)]
colnames(data_delta_tile)[colnames(data_delta_tile) == "V1"] <- "median"

## 2.4 Data wrangling

# 2.4.1 Turn variable generation_duration into a factor to facilitate plotting
#       (on a numeric scale, the levels would have gaps between them) 
data_delta_tile$generation_duration <- as.factor(data_delta_tile$generation_duration)


# 2.4.2 Turn variable survival_threshold into a factor and rename the factor levels 
#       in order to change the facet labels in the plot (necessary to do this here 
#       because we have to reverse the levels of the facet in facet_grid so that 
#       delta = 0 is at the bottom, and for some reason doing this is incompatible with
#       also renaming the facet labels within facet_grid -- also, no, reordering the
#       factor labels here first doesn't work)
data_delta_tile$survival_threshold_numeric <- data_delta_tile$survival_threshold
data_delta_tile$survival_threshold <- as.factor(data_delta_tile$survival_threshold)
levels(data_delta_tile$survival_threshold) <- c("no threshold\n(\u03b4 = 0)", 
                                                "25% threshold\n(\u03b4 = .25)", 
                                                "50% threshold\n(\u03b4 = .5)",
                                                "75% threshold\n(\u03b4 = .75)")
##----------------------------------------------------------------------------##



##============================================================================##
## 3. Generate plot
##============================================================================##

##----------------------------------------------------------------------------##
## 3.1 Set colour scheme
viridis_option <- "mako" # set colour scheme

## 3.2 Generate the plot
library(ggplot2)
plot_delta_tile_evo <- ggplot(data_delta_tile, 
                              aes(x = payoff_RR,
                                  y = generation_duration,
                                  fill = median)) +
  scale_x_continuous(expand = c(0,0), 
                     name = expression("payoff for Registered Reports (" * italic(b)[R] * ")"),
                     breaks = seq(.1, .9, .1),
                     labels = c(".1", ".2", ".3", ".4", ".5", 
                                ".6", ".7", ".8", ".9"))+
  scale_y_discrete(expand = c(0,0), 
                   name = expression("research cycles before evaluation ("* italic(m) * ")"))+
  coord_fixed(ratio = 1/10) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,0), "mm"),
        legend.title = element_text(hjust = 0.2),
        legend.position = "bottom") +
  geom_tile() +
  scale_fill_viridis_c(name = expression("evolved publication strategy (" * italic(s) * ")"), limits = c(0,1), option = viridis_option)+ 
  geom_segment(x = data_delta_tile$survival_threshold_numeric - .05, 
               xend = data_delta_tile$survival_threshold_numeric - .05,
               y = 0, yend = max(as.numeric(data_delta_tile$generation_duration))+1,
               colour = "#FFEA00") +
  facet_grid(factor(survival_threshold,
                    levels = rev(levels(survival_threshold))) ~ epsilon,
             labeller = labeller(epsilon = c(
               `0.2` = "decreasing returns\n(\u03b5 = 0.2)",
               `1` = "linear\n(\u03b5 = 1)",
               `5` = "increasing returns\n(\u03b5 = 5)")))+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = grid::unit(50, "mm"),
                                barheight = grid::unit(3, "mm")))
#facet_grid(epsilon ~ survival_threshold) # to plot facets the other way around

## 3.3 Save the plot
ggsave(here::here("..", "rr-model", "plots", "plot_delta_tile_evo.png"), plot_delta_tile_evo, bg = "white",
       #width = 21, height = 10, # setting when plotting facets the other way round
       width = 15.5, height = 16.5, 
       units = "cm")
##----------------------------------------------------------------------------##
