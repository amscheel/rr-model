library(data.table)

permutation.fun.new <- function(m = 1, # number of research cycles before fitness is calculated
                                s, # submission strategy (value between 0 and 1)
                                b.SR.neg = 0, # value of a negative standard report
                                b.SR.pos = 1, # value of a positive standard report
                                b.RR, # value of a Registered Report
                                epsilon = 1, # exponent of the fitness function
                                survival.threshold = 0, # threshold below which fitness = 0
                                survival.threshold.rel = F, # whether survival threshold is calculated as a proportion of m (maximum payoff if b.SR.pos = 1)
                                competition = 1, # if fitness is outside of top X proportion, it is set to 0
                                noise = 0){
  
  prior.SR <- (s+1)/2 
  prob.RR <- s
  prob.SR.neg <- (1-s)*(1-prior.SR)
  prob.SR.pos <- (1-s)*prior.SR 
  
  # Create a df with event counts for all possible outcome combinations (ignoring order)
  combinationmat <- expand.grid(0:m, 0:m, 0:m)
  combinationmat <- combinationmat[rowSums(combinationmat)==m,]
  
  colnames(combinationmat) <- c("SR.neg", "SR.pos", "RR")
  # Calculate the probability of each combination
  combinationmat$prob <- apply(combinationmat, 1, function(f) dmultinom(
    x = c(f[1], f[2], f[3]), 
    size = m, 
    prob = c(prob.SR.neg, prob.SR.pos, prob.RR)))
  
  # combinationmat$prob <- apply(combinationmat, 1, function(f) dmultinom(x = c(f[1], f[2], f[3]), size = m, prob = c(1/3, 1/3, 1/3)))
  # combinationmat$freq <- combinationmat$prob * 3^m
  
  # Calculate survival threshold, depending on whether it's relative or absolute
  survival.threshold <- ifelse(survival.threshold.rel == T, survival.threshold*m, survival.threshold)
  
  # calculate total payoffs for each outcome combination, with option of
  # adding a small amount of noise
  combinationmat$payoff <- combinationmat$SR.neg * 
    (b.SR.neg + rnorm(nrow(combinationmat), m = 0, sd = noise)) +
    combinationmat$SR.pos * 
    (b.SR.pos + rnorm(nrow(combinationmat), m = 0, sd = noise)) + 
    combinationmat$RR * 
    (b.RR + rnorm(nrow(combinationmat), m = 0, sd = noise))
  combinationmat$fitness <- ifelse(combinationmat$payoff < survival.threshold,
                                   0, combinationmat$payoff^epsilon)
  
  combinationmat$fitness <- ifelse(combinationmat$fitness < quantile(combinationmat$fitness, probs = 1-competition), 0, combinationmat$fitness)
  
  # output expected fitness (probability-weighted average)
  return(weighted.mean(combinationmat$fitness, combinationmat$prob))
}

# Note: Noise of sd = 0.002 or .005 doesn't seem to do much

# Simulate expected fitness for survival thresholds of 0, .25, .5, and .75
params_delta <- as.data.table(expand.grid(m = c(1, 2, 4, 8, 16, 32),
                                          s = seq(from = 0.01, to = .99,
                                                  length.out = 100),
                                          b_SR_neg = 0,
                                          b_SR_pos = 1,
                                          b_RR = seq(0.1, 0.9, 0.1),
                                          epsilon = c(0.2, 1, 5),
                                          survival_threshold.rel = T,
                                          survival_threshold = c(0, .25, 0.5, 0.75),
                                          competition = 1,
                                          noise = 0))

params_delta$expected_fitness <- apply(params_delta, 1, 
                                       function(x) permutation.fun.new(
                                         m = x[1], s = x[2], b.RR = x[5], 
                                         epsilon = x[6], 
                                         survival.threshold.rel = x[7], 
                                         survival.threshold = x[8], noise = x[9]))

# Simulate expected fitness for several levels of competition
params_gamma <- as.data.table(expand.grid(m = c(1, 2, 4, 8, 16, 32),
                                          s = seq(from = 0.01, to = .99,
                                                  length.out = 100),
                                          b_SR_neg = 0,
                                          b_SR_pos = 1,
                                          b_RR = seq(0.1, 0.9, 0.1),
                                          epsilon = c(0.2, 1, 5),
                                          survival_threshold.rel = T,
                                          survival_threshold = 0,
                                          competition = c(.01, .05, .1, .5, .9, 1),
                                          noise = 0))

time1 <- Sys.time() # create time stamp to calculate run time for model
# Simulate!
params_gamma$expected_fitness <- apply(params_gamma, 1, 
                                       function(x) permutation.fun.new(
                                         m = x[1], s = x[2], b.RR = x[5], 
                                         epsilon = x[6], 
                                         survival.threshold.rel = x[7], 
                                         survival.threshold = x[8], 
                                         competition = x[9], noise = x[10]))
time2 <- Sys.time() # create time stamp to calculate run time for model
time2-time1 # calculate model run time
saveRDS(params_gamma, "data_gamma_fun.RData")

params_gamma <- readRDS(here::here("data", "data_gamma_fun.RData"))

s_max_Efitness <- params_gamma[params_gamma[, .I[expected_fitness == max(expected_fitness)], by = .(m, epsilon, b_RR, survival_threshold.rel, survival_threshold, competition, noise)]$V1]


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