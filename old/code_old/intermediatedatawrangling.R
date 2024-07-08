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
data_comp_new_uneven <- readRDS(here("data", "data_comp_new_uneven.RData"))
data_comp_new_even <- readRDS(here("data", "data_comp_new_even.RData"))

data_comp_new_uneven_32 <- readRDS(here("data", "data_comp_new_uneven_32.RData"))
data_comp_new_even_32 <- readRDS(here("data", "data_comp_new_even_32.RData"))

comp_old_summary <- data_comp[, median(submission_threshold), 
                              by = .(run, generation_duration, 
                                     payoff_SR_neg, relative_payoff_SR_pos, 
                                     relative_payoff_RR,
                                     e, relative_survival_threshold,
                                     relative_top_n)]
colnames(comp_old_summary)[colnames(comp_old_summary) == "V1"] <- "median"

z_summary <- z[, median(submission_threshold), 
               by = .(run, generation_duration, 
                      payoff_SR_neg, relative_payoff_SR_pos, 
                      relative_payoff_RR,
                      e, relative_survival_threshold,
                      relative_top_n)]
colnames(z_summary)[colnames(z_summary) == "V1"] <- "median"

old <- comp_old_summary[comp_old_summary$relative_top_n==1 | 
                          comp_old_summary$relative_top_n==0.9 |
                          comp_old_summary$relative_top_n==0.8 | 
                          comp_old_summary$relative_top_n==0.5 | 
                          comp_old_summary$relative_top_n==0.2 | 
                          comp_old_summary$relative_top_n==0.1,]
new <- z_summary[z_summary$relative_top_n==1 | z_summary$relative_top_n==0.9 |
                   z_summary$relative_top_n==0.8 | z_summary$relative_top_n==0.5 | 
                   z_summary$relative_top_n==0.2 | z_summary$relative_top_n==0.1,]
new <- new[new$generation_duration != 32, ]

colorbrewer3 <- c("#2166ac", "grey", "#b2182b")

ggplot(new, aes(x = factor(relative_payoff_RR),
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
  facet_grid(generation_duration ~ factor(relative_top_n, levels = c("1", "0.9",
                                                                     "0.8", "0.5",
                                                                     "0.2", "0.1")))


##--------------------------------------------------------------------------##
### tile plot
z_tile <- z_summary[, median(median), 
               by = .(run, generation_duration, 
                      payoff_SR_neg, relative_payoff_SR_pos, 
                      relative_payoff_RR,
                      e, relative_survival_threshold,
                      relative_top_n)]
colnames(z_tile)[colnames(z_tile) == "V1"] <- "median"

z_tile$relative_top_n <- as.factor(as.character(z_tile$relative_top_n))
# levels(z_tile$relative_top_n) <- c("m = 1", "m = 2", "m = 4",
#                                                 "m = 8", "m = 16")

x_tile <- x[, median(median), 
                    by = .(run, generation_duration, 
                           payoff_SR_neg, relative_payoff_SR_pos, 
                           relative_payoff_RR,
                           e, relative_survival_threshold,
                           relative_top_n)]
colnames(x_tile)[colnames(x_tile) == "V1"] <- "median"


x_tile$relative_top_n <- as.factor(as.character(x_tile$relative_top_n))

# tile plot with gamma on y axis and m in panels
ggplot(x_tile, aes(x = relative_payoff_RR,
                   y = relative_top_n,
                   fill = median)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[RR]),
                     breaks = seq(.1, .9, .1),
                     labels = c(".1", "", ".3", "", ".5", "", ".7", "", ".9"))+
  scale_y_discrete(expand = c(0,0), name = 
                     expression(paste("competition (", gamma, ")", ""),),
                   limits = rev
                   # , breaks = seq(.1, 1, .1),
                   # labels = c(".1", "", ".3", "", ".5", 
                   #            "", ".7", "", ".9", "")
  )+
  coord_fixed(ratio = 1/10) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,1), "mm"),
        legend.title.align=0.2) +
  geom_tile() +
  scale_fill_viridis_c(name = "s", limits = c(0,1), option = "magma")+ 
  facet_grid(e ~ generation_duration)
##--------------------------------------------------------------------------##


data_comp <- readRDS(here("data", "data_competition.RData"))

old_tile <- old[, median(median), 
                by = .(run, generation_duration, 
                       payoff_SR_neg, relative_payoff_SR_pos, 
                       relative_payoff_RR,
                       e, relative_survival_threshold,
                       relative_top_n)]
new_tile <- new[, median(median), 
                by = .(generation_duration, 
                       payoff_SR_neg, 
                       relative_payoff_SR_pos, 
                       relative_payoff_RR,
                       e, relative_survival_threshold,
                       relative_top_n)]
colnames(old_tile)[colnames(old_tile) == "V1"] <- "median"
colnames(new_tile)[colnames(new_tile) == "V1"] <- "median"


y_tile <- comp_old[comp_old$relative_top_n==1 | comp_old$relative_top_n==0.9|
                     comp_old$relative_top_n==0.8 | comp_old$relative_top_n==0.5 |
                     comp_old$relative_top_n==0.2 | comp_old$relative_top_n==0.1,]

# tile plot with m on y axis and gamma in tiles
ggplot(x2_tile, aes(x = relative_payoff_RR,
                   y = factor(generation_duration),
                   fill = median)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[RR]),
                     breaks = seq(.1, .9, .1),
                     labels = c(".1", "", ".3", "", ".5", "", ".7", "", ".9"))+
  scale_y_discrete(expand = c(0,0), name = "m"
                   #, limits = rev
                   # , breaks = seq(.1, 1, .1),
                   # labels = c(".1", "", ".3", "", ".5", 
                   #            "", ".7", "", ".9", "")
  )+
  coord_fixed(ratio = 1/10) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,1), "mm"),
        legend.title.align=0.2) +
  geom_tile() +
  scale_fill_viridis_c(name = "s", limits = c(0,1), option = "magma")+ 
  facet_grid(e ~ relative_top_n)
  facet_grid(e ~ factor(relative_top_n, levels = c("1", "0.9", "0.8", "0.5", "0.2",
                                                   "0.1","0.05", "0.01")))


##--------------------------------------------------------------------------##
## delta

delta_new <- readRDS(here("data", "data_delta_new.RData"))
delta_new_32 <- readRDS(here("data", "data_delta_new_32.RData"))

delta_new <- rbind(delta_new, delta_new_32)

# calculate median submission threshold of every run
delta_summary <- delta_new[, median(submission_threshold), 
                                 by = .(run, generation_duration, 
                                        payoff_SR_neg, relative_payoff_SR_pos, 
                                        relative_payoff_RR,
                                        e, relative_survival_threshold,
                                        relative_top_n)]
colnames(delta_summary)[colnames(delta_summary) == "V1"] <- "median"

delta_summary.25 <- delta_summary[delta_summary$relative_payoff_RR == 0.25, ]

ggplot(delta_summary.25, aes(x = relative_survival_threshold,
                      y = median,
                      colour = factor(e),
                      group = run)) +
  scale_x_continuous(lim = c(0, 1), expand = c(0,0),
                     name = "survival threshold") +
  scale_y_continuous(lim = c(0, 1), expand = c(0,0),
                     breaks = seq(0, 1, .2),
                     name = "submission threshold (s)") +
  scale_colour_manual(values = colorbrewer3, name = expression(epsilon)) +
  theme_light() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #text = element_text(size = 12),
        plot.margin=grid::unit(c(4,0,1,0), "mm"),
        legend.title.align=0.5)+
  guides(colour = guide_legend(override.aes = list(size = .5)))+
  coord_fixed(ratio = 3/3)+ 
  geom_hline(yintercept = .5, colour = "darkgrey") +
  #geom_vline(xintercept = 5, colour = "lightgrey") +
  geom_point(aes(group = factor(e)), 
            # position = position_dodge(width = .3), 
             size = .4, alpha = .2) +
  stat_summary(aes(x = relative_survival_threshold-0.01, 
                   group = factor(e)),
               geom = "line",
               fun = "median", 
              # position = position_dodge(width = .3), 
               size = .2) +
  stat_summary(aes(x = relative_survival_threshold-0.1, 
                   group = factor(e)),
               fun.data = "median_hilow", 
              # position = position_dodge(width = .3),  
               size = .2) +
  facet_grid(e ~ generation_duration)



# make tile plot
delta_tile <- delta_summary[, median(median), 
            by = .(run, generation_duration, 
                   payoff_SR_neg, relative_payoff_SR_pos, 
                   relative_payoff_RR,
                   e, relative_survival_threshold,
                   relative_top_n)]
colnames(delta_tile)[colnames(delta_tile) == "V1"] <- "median"

delta_tile$generation_duration <- as.factor(as.character(delta_tile$generation_duration))
delta_tile$generation_duration <- factor(delta_tile$generation_duration,
                                          levels = c("1", "2", "4", 
                                                     "8", "16", "32"))


ggplot(delta_tile, aes(x = factor(relative_survival_threshold),
                   y = generation_duration,
                   fill = median)) +
  scale_x_discrete(expand = c(0,0), name = "delta"
                     # , breaks = seq(.1, .9, .1),
                     # labels = c(".1", "", ".3", "", ".5", "", ".7", "", ".9")
                     )+
  scale_y_discrete(expand = c(0,0), name = 
                     expression(paste("competition (", gamma, ")", ""),)
                   # , breaks = seq(.1, 1, .1),
                   # labels = c(".1", "", ".3", "", ".5", 
                   #            "", ".7", "", ".9", "")
  )+
  coord_fixed(ratio = 1/1) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,1), "mm"),
        legend.title.align=0.2) +
  geom_tile() +
  scale_fill_viridis_c(name = "s", limits = c(0,1))+ 
  facet_grid(e ~ relative_payoff_RR)
