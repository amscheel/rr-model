################################################################################
####                                                                        ####
#### Script 5c: Script to produce Figure 5 (effects of empirical pace)      ####
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
data_m <- readRDS(here::here("data", "simdata_m_evo.RData"))
colnames(data_m) <- c("generation_duration", "payoff_SR_neg",
                      "payoff_SR_pos", "payoff_RR",
                      "epsilon", "survival_threshold",
                      "relative_top_n", "run_id",
                      "publication_strategy")

## 2.2 Calculate the median publication strategy (s) of every run
data_m_summary <- data_m[, median(publication_strategy), 
                         by = .(run_id, generation_duration, 
                                payoff_SR_neg, payoff_SR_pos, 
                                payoff_RR,
                                epsilon, survival_threshold,
                                relative_top_n)]

## 2.3 Calculate the median of every run median per condition for the tile plot
data_m_tile <- data_m_summary[, median(V1), 
                              by = .(generation_duration, 
                                     payoff_SR_neg, 
                                     payoff_SR_pos, 
                                     payoff_RR,
                                     epsilon, survival_threshold,
                                     relative_top_n)]
colnames(data_m_tile)[colnames(data_m_tile) == "V1"] <- "median"

## 2.4 Data wrangling
# turn variable generation_duration into a factor to facilitate plotting
# (on a numeric scale, the levels would have gaps between them) 
data_m_tile$generation_duration <- as.factor(data_m_tile$generation_duration)

# save this summary dataset for easier loading/reproduction later:
#saveRDS(data_m_tile, "simdata_m_evo_tile_forplotting.RData")
##----------------------------------------------------------------------------##


##============================================================================##
## 3. Generate plot
##============================================================================##

##----------------------------------------------------------------------------##
## 3.1 Set colour scheme
viridis_option <- "rocket"

## 3.2 Generate plot
library(ggplot2)
plot_m_evo <- ggplot(data_m_tile, 
                     aes(x = payoff_RR,
                         y = generation_duration,
                         fill = median)) +
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
ggsave(here::here("plots", "plot_m_tile_evo.png"), plot_m_evo, bg = "white",
       width = 18, height = 6, units = "cm")
##----------------------------------------------------------------------------##
