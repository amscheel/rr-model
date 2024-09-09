################################################################################
####                                                                        ####
#### Script 9: Script to produce Figure A4 (effects of competition in a     ####
####           line plot)                                                   ####
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
data_gamma <- readRDS(here::here("data", "simdata_gamma_evo.RData"))
colnames(data_gamma) <- c("generation_duration", "payoff_SR_neg",
                          "payoff_SR_pos", "payoff_RR",
                          "epsilon", "survival_threshold",
                          "relative_top_n", "run_id",
                          "publication_strategy")

## 2.2 Calculate the median publication strategy (s) of every run
data_gamma_summary <- data_gamma[, median(publication_strategy), 
                                 by = .(run_id, generation_duration, 
                                        payoff_SR_neg, payoff_SR_pos, 
                                        payoff_RR,
                                        epsilon, survival_threshold,
                                        relative_top_n)]
colnames(data_gamma_summary)[colnames(data_gamma_summary) == "V1"] <- "median"

## 2.3 Data wrangling
#      Select values of competition to plot (relative_top_n):
#     Only keep gamma = 1, .9, .5, .1, .05, and .01
data_gamma_summary <- data_gamma_summary[which(data_gamma_summary$relative_top_n == 0.01 |
                                                 data_gamma_summary$relative_top_n == 0.05 |
                                                 data_gamma_summary$relative_top_n == 0.1 |
                                                 data_gamma_summary$relative_top_n == 0.5 |
                                                 data_gamma_summary$relative_top_n == 0.9 |
                                                 data_gamma_summary$relative_top_n == 1),]
##----------------------------------------------------------------------------##


##============================================================================##
## 3. Generate plot
##============================================================================##

##----------------------------------------------------------------------------##
## 3.1 Define a colour scheme 
colorbrewer3 <- c("#2166ac", "grey", "#b2182b")

## 3.2 Generate plot
library(ggplot2)
plot_gamma_line_evo <- ggplot(data_gamma_summary,
                              aes(x = factor(payoff_RR),
                                  y = median,
                                  colour = factor(epsilon),
                                  group = run_id)) +
  scale_x_discrete(name = expression(italic(b)[R]),
                   breaks = c(0.1, "", 0.3, "", 0.5, "", 0.7, "", 0.9)) +
  scale_y_continuous(lim = c(0, 1), expand = c(0,0),
                     breaks = seq(0, 1, .2),
                     name = "publication strategy (s)") +
  scale_colour_manual(values = colorbrewer3, name = expression(epsilon)) +
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
             position = position_dodge(width = .3), size = .4, alpha = .2) +
  stat_summary(aes(x = as.numeric(factor(payoff_RR))-0.1, 
                   group = factor(epsilon)),
               geom = "line",
               fun = "median", 
               position = position_dodge(width = .3), linewidth = .2) +
  stat_summary(aes(x = as.numeric(factor(payoff_RR))-0.1, 
                   group = factor(epsilon)),
               fun.data = "median_hilow", 
               position = position_dodge(width = .3),  size = .2) +
  facet_grid(relative_top_n ~ generation_duration,
             labeller = labeller(
               relative_top_n = c(
                 `0.01` = "top 1%\n(gamma = .01)",
                 `0.05` = "top 5%\n(gamma = .05)",
                 `0.1` = "top 10%\n(gamma = .1)",
                 `0.5` = "top 50%\n(gamma = .5)",
                 `0.9` = "top 90%\n(gamma = .9)",
                 `1` = "no competition\n(gamma = 1)"),
               generation_duration = c(
                 `1` = "m = 1",
                 `2` = "m = 2",
                 `4` = "m = 4",
                 `8` = "m = 8",
                 `16` = "m = 16",
                 `32` = "m = 32")))

## 3.3 Save plot
ggsave(here::here("plots", "plot_gamma_line_evo.png"), plot_gamma_line_evo, width = 23.5, height = 21, units = "cm")
##----------------------------------------------------------------------------##

