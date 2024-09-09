################################################################################
####                                                                        ####
#### Script 5b: Script to produce Figure 4 (effects of fitness functions)   ####
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
## 2.1 Load epsilon data
data_epsilon <- readRDS(here::here("data", "simdata_epsilon_evo.RData"))
colnames(data_epsilon) <- c("generation_duration", "payoff_SR_neg",
                            "payoff_SR_pos", "payoff_RR",
                            "epsilon", "survival_threshold",
                            "relative_top_n", "run_id",
                            "publication_strategy")

## 2.2 Calculate median publication strategy (s) of every run
data_epsilon_summary <- data_epsilon[, median(publication_strategy), 
                                     by = .(run_id, generation_duration, 
                                            payoff_SR_neg, payoff_SR_pos, 
                                            payoff_RR,
                                            epsilon, survival_threshold,
                                            relative_top_n)]

colnames(data_epsilon_summary)[colnames(data_epsilon_summary) == "V1"] <- "median"

# save this summary dataset for easier loading/reproduction later:
# saveRDS(data_epsilon_summary, "simdata_epsilon_evo_summary_forplotting.RData")
##----------------------------------------------------------------------------##


##============================================================================##
## 3. Generate plot
##============================================================================##

##----------------------------------------------------------------------------##
## 3.1 Define a colour scheme 
colorbrewer5 <- c("#2166ac", "#67a9cf", "grey", "#ef8a62", "#b2182b")

## 3.2 Generate plot
library(ggplot2)
plot_epsilon_evo <- ggplot(data_epsilon_summary,
                                    aes(x = factor(payoff_RR),
                                        y = median,
                                        colour = factor(epsilon),
                                        group = run_id)) +
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
             position = position_dodge(width = .3), size = .4, alpha = .2) +
  stat_summary(aes(x = as.numeric(factor(payoff_RR))-0.1, 
                   group = factor(epsilon)),
               geom = "line",
               fun = "median", 
               position = position_dodge(width = .3), linewidth = .2) +
  stat_summary(aes(x = as.numeric(factor(payoff_RR))-0.1, 
                   group = factor(epsilon)),
               fun.data = "median_hilow", 
               position = position_dodge(width = .3),  size = .2)


## 3.3 Save plot
ggsave(here::here("plots", "plot_epsilon_evo.png"), plot_epsilon_evo, width = 10.5, height = 8.5, units = "cm")
##----------------------------------------------------------------------------##

