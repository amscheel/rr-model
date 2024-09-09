################################################################################
####                                                                        ####
#### Script 7: Script to run the expected fitness (EF) simulation model     ####
####                                                                        ####
#### ---------------------------------------------------------------------- ####
#### Content:                                                               ####
####  * 1: Setup                                                            ####
####  * 2: Set model parameters                                             ####
####  * 3: Run the simulation                                               ####
####  * 4: Code to reproduce the specific simulations in the appendix       #### 
####                                                                        ####
#### Note:                                                                  ####
####   This script relies on functions from the script                      ####
####   "06_EF-model_function.R".                                            ####
####   Depending on the used settings and your computing power, the         ####
####   simulations can take a while to run (but they are much faster than   ####
####   the evolutionary simulation).                                        ####
####                                                                        ####
################################################################################

##============================================================================##
## 1. Setup
##============================================================================##

##----------------------------------------------------------------------------##
# This script calls functions from the script "06_EF-model_function.R". 
# We use the package "here" to navigate to the directory storing the script.

# Install "here" unless it is already installed:
if(!require(here)){install.packages("here")}

# load model functions
source(here::here("code", "06_EF-model_function.R"))
##----------------------------------------------------------------------------##



##============================================================================##
## 2. Set model parameters
##============================================================================##

##----------------------------------------------------------------------------##
m <- 1 # Number of research rounds (default = 1)
s <- seq(from = 0.01, to = .99, length.out = 100) # publication strategies for
                                                  # which expected fitness is 
                                                  # calculated. The more fine-
                                                  # grained this range, the more
                                                  # accurate the result, but more
                                                  # values increase run time.
b_SR_neg <- 0 # payoff for a negative result in a standard report 
              # (b_neg; default = 0, can also be a vector)
b_SR_pos <- 1 # payoff for a positive result in a standard report 
              # (b_pos; default = 1, can also be a vector)
b_RR <- seq(0.1, 0.9, 0.1) # payoff for a Registered Report (b_RR; 
                           # default = c(.1, .2, .3, .4, .5, .6, .7, .8, .9), can 
                           # also be a single value)
epsilon <- c(0.2, 1, 5) # exponent applied to accumulated payoffs to translate 
                        # them into fitness, determining the shape of the fitness 
                        # function (values between 0 and 1 yield diminishing 
                        # returns, values above 1 yield increasing returns; 
                        # default = c(.2, 1, 5))
survival_threshold.rel <- T # whether the survival threshold is calculated as
                            # a proportion of the maximum accumulated payoffs (T) 
                            # or as an absolute number (F). Default = T
survival_threshold <- 0 # threshold below which accumulated payoffs are translated 
                        # to 0 fitness (default = 0, i.e. no survival threshold)
competition <- 1  # this number determines the size of the bottleneck: 
                  # when competition < 1, only those researchers with 
                  # fitness values in the competition portion of the 
                  # population will be considered for reproduction. E.g., 
                  # when competition = 0.1, only researchers in the 
                  # top 10% can reproduce (default = 1, i.e. no competition).
##----------------------------------------------------------------------------##



##============================================================================##
## 3. Run the simulation
##    Depending on settings & computing power, the simulation can take a while
##    to run. Run time is affected by the number of parameter combinations 
##    that are tested. 
##    It's best to start with a small number of parameter values to get a
##    sense of how long it takes on your machine.
##============================================================================##

##----------------------------------------------------------------------------##
# Run the simulation & store the data
simdata_EF <- max.EF.sim(m = m,
                         s = s,
                         b_SR_neg = b_SR_neg,
                         b_SR_pos = b_SR_pos,
                         b_RR = b_RR,
                         epsilon = epsilon,
                         survival_threshold.rel = survival_threshold.rel,
                         survival_threshold = survival_threshold,
                         competition = competition)

saveRDS(simdata_EF, here::here("data", "simdata_EF.RData"))
##----------------------------------------------------------------------------##



##============================================================================##
## 4. Code to reproduce the specific simulations in the appendix
##    The code below reproduces the data shown in Figures 4-7 in the appendix.
##    All of these data _could_ be produced in one go (though that may take
##    quite a while). For clarity, we break it down by figure.
##============================================================================##

##----------------------------------------------------------------------------##
## 4.1 Non-linear fitness functions (epsilon, Fig. A1) 

simdata_EF_epsilon <- max.EF.sim(m = 1,
                                 s = seq(from = 0.01, to = .99, length.out = 100),
                                 b_SR_neg = 0,
                                 b_SR_pos = 1,
                                 b_RR = seq(0.1, 0.9, 0.1),
                                 epsilon = c(.2, .5, 1, 2, 5),
                                 survival_threshold.rel = T,
                                 survival_threshold = 0,
                                 competition = 1)

# Store the data
saveRDS(simdata_EF_epsilon, here::here("data", "simdata_epsilon_EF.RData"))
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## 4.2 Empirical pace (m, Fig. A2) 

simdata_EF_m <- max.EF.sim(m = c(1, 2, 4, 8, 16, 32),
                                 s = seq(from = 0.01, to = .99, length.out = 100),
                                 b_SR_neg = 0,
                                 b_SR_pos = 1,
                                 b_RR = seq(0.1, 0.9, 0.1),
                                 epsilon = c(.2, 1, 5),
                                 survival_threshold.rel = T,
                                 survival_threshold = 0,
                                 competition = 1)

# Store the data
saveRDS(simdata_EF_m, here::here("data", "simdata_m_EF.RData"))
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## 4.3 Survival thresholds (delta, Fig. A3) 

simdata_EF_delta <- max.EF.sim(m = c(1, 2, 4, 8, 16, 32),
                               s = seq(from = 0.01, to = .99, length.out = 100),
                               b_SR_neg = 0,
                               b_SR_pos = 1,
                               b_RR = seq(0.1, 0.9, 0.1),
                               epsilon = c(.2, 1, 5),
                               survival_threshold.rel = T,
                               survival_threshold = c(0, .25, .5, .75),
                               competition = 1)

# Store the data
saveRDS(simdata_EF_delta, here::here("data", "simdata_delta_EF.RData"))
##----------------------------------------------------------------------------##



# tile plot
df_tile <- s_max_Efitness
df_tile$m <- as.factor(df_tile$m)
#levels(df_tile$m) <- c("1", "2", "4", "8", "16", "32")

#df_tile$epsilon <- as.factor(df_tile$epsilon)
#levels(df_tile$epsilon) <- c("risk averse", "neutral", "risk prone")


### Plotting for delta plot
df_tile$survival_threshold_numeric <- df_tile$survival_threshold
df_tile$survival_threshold <- as.factor(df_tile$survival_threshold)
levels(df_tile$survival_threshold) <- c("no threshold", "25% threshold", 
                                        "50% threshold", "75% threshold")

delta_tile_quarts <- ggplot(df_tile, 
                     aes(x = b_RR,
                         y = m,
                         fill = s)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[RR]),
                     breaks = seq(0, 1, .1),
                     labels = c("0",".1", ".2", ".3", ".4", ".5", 
                                ".6", ".7", ".8", ".9", "1"))+
  scale_y_discrete(expand = c(0,0), 
                   name = "research cycles before evaluation (m)")+
  coord_fixed(ratio = 1/10) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,0), "mm"),
        legend.title.align=0.2) +
  geom_tile() +
  geom_segment(x = df_tile$survival_threshold_numeric - .05, 
               xend = df_tile$survival_threshold_numeric - .05,
               y = 0, yend = max(as.numeric(df_tile$m))+1,
               colour = "#FFEA00") +
  scale_fill_viridis_c(name = "s", limits = c(0,1), option = "mako")+ 
  facet_grid(survival_threshold ~ epsilon)

ggsave("plot_delta_tile_quarts.png", delta_tile_quarts, bg = "white",
       width = 17, height = 13, units = "cm")

# ggsave("plot_delta_tiles.png", delta_tile, bg = "white",
#        width = 22, height = 10, units = "cm")


colorbrewer3 <- c("#2166ac", "grey", "#b2182b")

regular_plot <- ggplot(df_tile, aes(x = b_RR, y = s, 
                                       colour = as.factor(epsilon))) + 
  scale_x_continuous(name = expression(italic(b)[RR]),
                     limits = c(0, 1),
                     breaks = seq(0.1, 0.9, .2),
                     expand = c(0, 0)) +
  scale_y_continuous(name="s",
                     breaks = seq(0, 1, .2),
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  scale_colour_manual(values = colorbrewer3, name = expression(epsilon)) +
  theme_minimal() +
  coord_fixed(ratio = 9/10) +
  geom_point()+
  geom_path() +
  geom_segment(x = df_tile$survival_threshold_numeric, 
               xend = df_tile$survival_threshold_numeric,
               y = 0, yend = max(as.numeric(df_tile$m))+1,
               colour = "#59abff", 
               #colour = "#f78200",
               linetype = "dotted") +
  facet_grid(survival_threshold ~ m)
# facet_wrap(~ m)

ggsave("plot_delta_quarts.png", regular_plot, bg = "white",
       width = 24, height = 13, units = "cm")

### Plotting for gamma plot

ggplot(df_tile, 
       aes(x = b_RR,
           y = m,
           fill = s)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[RR]),
                     breaks = seq(0, 1, .1),
                     labels = c("0",".1", ".2", ".3", ".4", ".5", 
                                ".6", ".7", ".8", ".9", "1"))+
  scale_y_discrete(expand = c(0,0), 
                   name = "research cycles before evaluation (m)")+
  coord_fixed(ratio = 1/10) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,0), "mm"),
        legend.title.align=0.2) +
  geom_tile() +
  scale_fill_viridis_c(name = "s", limits = c(0,1), option = "cividis")+ 
  facet_grid(competition ~ epsilon)



ggsave("plot_delta_tile_quarts.png", delta_tile_quarts, bg = "white",
       width = 17, height = 13, units = "cm")