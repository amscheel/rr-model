################################################################################
####                                                                        ####
#### Script 8b: Script to produce Figure A2 (effects of empirical pace      ####
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
data_m_EF <- readRDS(here::here("data", "simdata_m_EF.RData"))

## 2.2 Data wrangling
#      Turn variable m into a factor to facilitate plotting
#      (on a numeric scale, the levels would have gaps between them) 
data_m_EF$m <- as.factor(data_m_EF$m)
##----------------------------------------------------------------------------##

##============================================================================##
## 3. Generate plot
##============================================================================##

##----------------------------------------------------------------------------##
## 3.1 Set colour scheme
viridis_option <- "rocket"

## 3.2 Generate plot
library(ggplot2)
plot_m_EF <- ggplot(data_m_EF, 
                     aes(x = b_RR,
                         y = m,
                         fill = s_max_EF)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[R]),
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


## 3.3 Save plot
ggsave(here::here("plots", "plot_m_tile_EF.png"), plot_m_EF, bg = "white",
       width = 18, height = 6, units = "cm")
##----------------------------------------------------------------------------##

