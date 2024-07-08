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


##############################################################################
## OLD STUFF incl alternative plot
##############################################################################

for (i in 1:length(unique(data_delta_summary$generation_duration))) {
  levels(data_delta_summary$generation_duration)[i] <- 
    paste("m = ", unique(data_delta_summary$generation_duration)[i])
}


# save this summary dataset for easier loading/reproduction later
saveRDS(data_delta_summary, "data_delta_summary_forplotting.RData")

# Create a subset of the data with only half of the m conditions, 
# m = c(1, 4, 16)
data_delta_subset <- data_delta_summary[
  data_delta_summary$generation_duration== "m = 1" | 
    data_delta_summary$generation_duration == "m = 4" | 
    data_delta_summary$generation_duration== "m = 16",]

# define a colour scheme 
colorbrewer3 <- c("#2166ac", "grey", "#b2182b")

# plot
plot_delta4_test <- ggplot(data_delta_summary,
                           aes(x = factor(payoff_RR),
                               y = median,
                               colour = factor(epsilon),
                               group = run)) +
  scale_x_discrete(name = expression(italic(b)[RR])) +
  scale_y_continuous(lim = c(0, 1), expand = c(0,0),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold (s)") +
  scale_colour_manual(values = colorbrewer3, name = expression(epsilon)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin=grid::unit(c(0,0,0,1), "mm"),
        legend.title.align=0.5)+
  coord_fixed(ratio = 9/1)+ 
  geom_hline(yintercept = .5, colour = "darkgrey") +
  geom_point(aes(group = factor(epsilon)), 
             position = position_dodge(width = .3), size = .5, alpha = .2) +
  stat_summary(aes(x = as.numeric(factor(payoff_RR))-0.1, 
                   group = factor(epsilon)),
               geom = "line",
               fun = "median", 
               position = position_dodge(width = .3), size = .2) +
  stat_summary(aes(x = as.numeric(factor(payoff_RR))-0.1, 
                   group = factor(epsilon)),
               fun.data = "median_hilow", 
               position = position_dodge(width = .3), size = .2) + 
  facet_wrap(survival_threshold ~ generation_duration, ncol = 3)

ggsave("plot_delta.png", plot_delta, bg = "white",
       width = 16, height = 5.7, units = "cm")
