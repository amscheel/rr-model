################################################################################
####                                                                        ####
#### Script 8a: Script to produce Figure A1 (effects of fitness functions   ####
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
# Load data
data_epsilon_EF <- readRDS(here::here("data", "simdata_epsilon_EF.RData"))
##----------------------------------------------------------------------------##


##============================================================================##
## 3. Generate plot
##============================================================================##

##----------------------------------------------------------------------------##
## 3.1 Define colour scheme 
colorbrewer5 <- c("#2166ac", "#67a9cf", "grey", "#ef8a62", "#b2182b")

## 3.2 Generate plot
library(ggplot2)
plot_epsilon_EF <- ggplot(data_epsilon_EF,
                          aes(x = factor(b_RR),
                              y = s_max_EF,
                              colour = factor(epsilon))) +
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
             size = 1.5) +
  geom_line(aes(group = factor(epsilon)), 
             linewidth = .4)


## 3.3 Save plot
ggsave(here::here("plots", "plot_epsilon_EF.png"), plot_epsilon_EF, width = 10.5, height = 8.5, units = "cm")
##----------------------------------------------------------------------------##
