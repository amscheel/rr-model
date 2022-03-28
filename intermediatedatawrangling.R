### A script for intermediate data wrangling steps I did to clean up/create the
# eventual data sets


## DATA FOR COMPETITION

data_competition_m1 <- readRDS(here("data", "simdata_settings3.RData"))
data_competition_m23468 <- readRDS(here("data", "simdata_settings3_3.RData"))
data_competition_m16 <- readRDS(here("data", "simdata_settings3_4.RData"))

# data_competition_m248 contains the conditions m == 3 and m == 6, which
# we don't need any more (focussing only on 1, 2, 4, 8, 16). 
# Delete rows with m == 3 and m == 6:
data_competition_m248 <- data_competition_m23468[data_competition_m23468$generation_duration!=3 & data_competition_m23468$generation_duration!=6,]

# Now bind the dataframes with m = 1, m = c(2, 4, 8) and m = 16 together
data_competition <- rbind(data_competition_m1, data_competition_m248)
data_competition <- rbind(data_competition, data_competition_m16)

# Save the new data file as the main data file for competition
saveRDS(data_competition, "data_competition.RData")

# data_competition also contains all the information we'll use to study
# the effect of m alone (i.e., with zero competition --> relative_top_n == 1)
# We'll create a new df with only those results and save it
data_comp <- readRDS(here("data", "data_competition.RData"))
data_m <- data_comp[data_comp$relative_top_n==1,]
saveRDS(data_m, "data_m.RData")

##--------------------------------------------------------------------------##

# load competition data
data_comp <- readRDS(here("data", "data_competition.RData"))
data_comp_extra <- readRDS(here("data", "data_comp_extrahighcomp.RData"))

x <- data_comp

data_comp <- rbind(data_comp, data_comp_extra)

y <- data_comp[data_comp$generation_duration>4 & data_comp$relative_top_n < .3, ]
z <- data_comp[data_comp$relative_top_n < .3, ]
z_summary <- z[, median(submission_threshold), 
               by = .(run, generation_duration, 
                      payoff_SR_neg, relative_payoff_SR_pos, 
                      relative_payoff_RR,
                      e, relative_survival_threshold,
                      relative_top_n)]
colnames(z_summary)[colnames(z_summary) == "V1"] <- "median"

y_summary_8 <- y_summary[y_summary$generation_duration == 8,]
y_summary_16 <- y_summary[y_summary$generation_duration == 16, ]


colorbrewer3 <- c("#2166ac", "grey", "#b2182b")

ggplot(z_summary, aes(x = factor(relative_payoff_RR),
           y = median,
           colour = factor(e),
           group = run)) +
  scale_x_discrete(name = "run") +
  scale_y_continuous(lim = c(0, 1), expand = c(0,0),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold (s)") +
  scale_colour_manual(values = colorbrewer3, name = expression(epsilon)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #text = element_text(size = 12),
        plot.margin=grid::unit(c(4,0,1,0), "mm"),
        legend.title.align=0.5)+
  guides(colour = guide_legend(override.aes = list(size = .5)))+
  coord_fixed(ratio = 9/1)+ 
  geom_hline(yintercept = .5, colour = "darkgrey") +
  #geom_vline(xintercept = 5, colour = "lightgrey") +
  geom_point(aes(group = factor(e)), 
             position = position_dodge(width = .3), size = .4, alpha = .2) +
  stat_summary(aes(x = as.numeric(factor(relative_payoff_RR))-0.1, 
                   group = factor(e)),
               geom = "line",
               fun = "median", 
               position = position_dodge(width = .3), size = .2) +
  stat_summary(aes(x = as.numeric(factor(relative_payoff_RR))-0.1, 
                   group = factor(e)),
               fun.data = "median_hilow", 
               position = position_dodge(width = .3),  size = .2) +
  facet_grid(generation_duration ~ relative_top_n)


