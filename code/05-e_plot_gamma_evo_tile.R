################################################################################
####                                                                        ####
#### Script 5e: Script to produce Figure 7 (effects of competition)         ####
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

## 2.3 Calculate the median of every run median per condition for the tile plot
data_gamma_tile <- data_gamma_summary[, median(V1), 
                               by = .(generation_duration, 
                                      payoff_SR_neg, 
                                      payoff_SR_pos, 
                                      payoff_RR,
                                      epsilon, survival_threshold,
                                      relative_top_n)]
colnames(data_gamma_tile)[colnames(data_gamma_tile) == "V1"] <- "median"

## 2.4 Data wrangling

# 2.4.1 Select values of competition to plot (relative_top_n):
#       Only keep gamma = 1, .9, .5, .1, .05, and .01
data_gamma_tile <- data_gamma_tile[which(data_gamma_tile$relative_top_n == 0.01 |
                                         data_gamma_tile$relative_top_n == 0.05 |
                                         data_gamma_tile$relative_top_n == 0.1 |
                                         data_gamma_tile$relative_top_n == 0.5 |
                                         data_gamma_tile$relative_top_n == 0.9 |
                                         data_gamma_tile$relative_top_n == 1),]

# 2.4.2 Turn variable generation_duration into a factor to facilitate plotting
#       (on a numeric scale, the levels would have gaps between them) 
data_gamma_tile$generation_duration <- as.factor(data_gamma_tile$generation_duration)

# save this summary dataset for easier loading/reproduction later:
# saveRDS(data_gamma_tile, "data_gamma_tile_forplotting.RData")
##----------------------------------------------------------------------------##



##============================================================================##
## 3. Generate plot
##============================================================================##

##----------------------------------------------------------------------------##
## 3.1 Set colour scheme
viridis_option <- "inferno"

## 3.2 Generate plot
library(ggplot2)
plot_gamma_evo <- ggplot(data_gamma_tile, 
                         aes(x = payoff_RR,
                             y = generation_duration,
                             fill = median)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[R]),
                     breaks = seq(.1, .9, .1),
                     labels = c(".1", ".2", ".3", ".4", ".5", 
                                ".6", ".7", ".8", ".9"))+
  scale_y_discrete(expand = c(0,0), 
                   name = "research cycles before evaluation (m)")+
  coord_fixed(ratio = 1/10) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,1), "mm"),
        legend.title = element_text(hjust = 0.2)) +
  geom_tile() +
  scale_fill_viridis_c(name = "s", limits = c(0,1), option = viridis_option)+ 
  facet_grid(relative_top_n ~ epsilon,
             labeller = labeller(
               relative_top_n = c(
                 `0.01` = "top 1%\n(gamma = .01)",
                 `0.05` = "top 5%\n(gamma = .05)",
                 `0.1` = "top 10%\n(gamma = .1)",
                 `0.5` = "top 50%\n(gamma = .5)",
                 `0.9` = "top 90%\n(gamma = .9)",
                 `1` = "no competition\n(gamma = 1)"),
               epsilon = c(
                 `0.2` = "decreasing returns\n(epsilon = 0.2)",
                 `1` = "linear\n(epsilon = 1)",
                 `5` = "increasing returns\n(epsilon = 5)")))

## 3.3. Save plot
ggsave(here::here("plots", "plot_gamma_tile_evo.png"), plot_gamma_evo, 
       bg = "white", width = 18, height = 21, units = "cm")
##----------------------------------------------------------------------------##