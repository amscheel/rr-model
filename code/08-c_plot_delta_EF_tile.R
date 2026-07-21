################################################################################
####                                                                        ####
#### Script 8c: Script to produce Figure A3 (effects of survival thresholds ####
####            using the expected fitness model)                           ####
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
data_delta_EF <- readRDS(here::here("..", "rr-model", "data", "simdata_delta_EF.RData"))

## 2.2 Data wrangling

# 2.2.1 Turn variable m into a factor to facilitate plotting
#       (on a numeric scale, the levels would have gaps between them) 
data_delta_EF$m <- as.factor(data_delta_EF$m)

# 2.2.2 Turn variable survival_threshold into a factor and rename the factor levels 
#       in order to change the facet labels in the plot (necessary to do this here 
#       because we have to reverse the levels of the facet in facet_grid so that 
#       delta = 0 is at the bottom, and for some reason doing this is incompatible with
#       also renaming the facet labels within facet_grid -- also, no, reordering the
#       factor labels here first doesn't work)
data_delta_EF$survival_threshold_numeric <- data_delta_EF$survival_threshold
data_delta_EF$survival_threshold <- as.factor(data_delta_EF$survival_threshold)
levels(data_delta_EF$survival_threshold) <- c("no threshold\n(\u03b4 = 0)", 
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
plot_delta_tile_EF <- ggplot(data_delta_EF, 
                              aes(x = b_RR,
                                  y = m,
                                  fill = s_max_EF)) +
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
  geom_segment(x = data_delta_EF$survival_threshold_numeric - .05, 
               xend = data_delta_EF$survival_threshold_numeric - .05,
               y = 0, yend = max(as.numeric(data_delta_EF$m))+1,
               colour = "#FFEA00") +
  #scale_fill_viridis_c(name = "s", limits = c(0,1), option = viridis_option)+ 
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
ggsave(here::here("..", "rr-model", "plots", "plot_delta_tile_EF.png"), plot_delta_tile_EF, bg = "white",
       #width = 21, height = 10, # setting when plotting facets the other way round
       width = 15.5, height = 16.5, 
       units = "cm")
##----------------------------------------------------------------------------##

