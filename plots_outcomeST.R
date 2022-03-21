library(ggplot2)
library(here)

x <- readRDS(here("data", "settings1.RData"))

simplot <- x
simplot <- simdata
simplot_summary <- simplot[, median(submission_threshold), 
                           by = .(run, generation_duration, 
                                  payoff_SR_neg, relative_payoff_SR_pos, 
                                  relative_payoff_RR,
                                  e, relative_survival_threshold,
                                  relative_top_n)]
colnames(simplot_summary)[colnames(simplot_summary) == "V1"] <- "median"


# Basic plot setup:
basic_plot <- ggplot(
  simplot_summary,
  aes(x = factor(relative_survival_threshold),
      y = median,
      colour = factor(e),
      #linetype = relative_survival_threshold,
      group = run
  )) +
  scale_y_continuous(lim = c(0, 1), expand = c(0,0),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold") +
  scale_colour_viridis_d(name = "competition"
                         #, option = "plasma"
                         ) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())+
  ggtitle(paste(pop_size, " researchers", ", ", 
                generations, " generations", ", ",
                runs, " runs", sep = "")) + 
  geom_point(aes(group = factor(e)), 
             position = position_dodge(width = .3),
             size = .5,
             alpha = .5) +
  stat_summary(aes(x = as.numeric(factor(relative_survival_threshold))-0.05, 
                   group = factor(e)),
               geom = "line",
               fun = "median", 
               position = position_dodge(width = .3),
               size = .2) +
  stat_summary(aes(x = as.numeric(factor(relative_survival_threshold))-0.05, 
                   group = factor(e)),
               fun.data = "median_hilow", 
               position = position_dodge(width = .3),
               size = .2)

basic_plot + facet_grid(relative_payoff_RR ~ generation_duration)
basic_plot + facet_wrap(~ generation_duration, ncol = 3)

# make a heatmap/tile plot
# calculate the median of all run medians

tile <- readRDS(here("data", "simdata_m-competition_e1.RData"))

data_competition_m1 <- readRDS(here("data", "simdata_settings3.RData"))
data_competition_m248 <- readRDS(here("data", "simdata_settings3_3.RData"))
data_competition_m16 <- readRDS(here("data", "simdata_settings3_4.RData"))

# data_competition_m248 contains the conditions m == 3 and m == 6, which
# we don't need any more (focussing only on 1, 2, 4, 8, 16). 
# Delete rows with m == 3 and m == 6:
data_competition_m248 <- data_competition_m248[data_competition_m248$generation_duration!=3 & data_competition_m248$generation_duration!=6,]

# Now bind the dataframes with m = 1, m = c(2, 4, 8) and m = 16 together
data_competition <- rbind(data_competition_m1, data_competition_m248)
data_competition <- rbind(data_competition, data_competition_m16)

# Save the new data file as the main data file for competition
saveRDS(data_competition, "data_competition.RData")

# data_competition also contains all the information we'll use to study
# the effect of m alone (i.e., with zero competition --> relative_top_n == 1)
# We'll create a new df with only those results and save it

data_m <- data_competition[data_competition$relative_top_n==1,]
summary(data_m)
saveRDS(data_m, "data_m.RData")

tile_summary2 <- tile_summary
tile <- simdata
tile_summary <- tile[, median(submission_threshold), 
                        by = .(run, generation_duration, 
                               payoff_SR_neg, relative_payoff_SR_pos, 
                               relative_payoff_RR,
                               e, relative_survival_threshold,
                               relative_top_n)]

tile_summary <- tile_summary[, median(V1), 
                         by = .(generation_duration, 
                                payoff_SR_neg, relative_payoff_SR_pos, 
                                relative_payoff_RR,
                                e, relative_survival_threshold,
                                relative_top_n)]
colnames(tile_summary)[colnames(tile_summary) == "V1"] <- "median"


tileplot <- ggplot(x,
                   aes(x = relative_payoff_RR,
                       y = relative_top_n,
                       fill = median)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_reverse(expand = c(0,0))+
  coord_fixed(ratio = 9/10) +
  geom_tile() +
  scale_fill_viridis_c(name = "s", 
                       option = "magma",
                       limits = c(0,1))
tileplot + facet_grid(e ~ generation_duration)
