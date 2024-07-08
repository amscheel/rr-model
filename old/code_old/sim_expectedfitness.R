permutation.fun.new <- function(m = 1, s, b.SR.neg = 0, b.SR.pos = 1, b.RR, 
                                epsilon = 1, survival.threshold = 0,
                                relative.survival.threshold = FALSE,
                                output.fitness.dist = FALSE){
  
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
  
  # calculate total payoffs for each outcome combination
  combinationmat$payoff <- combinationmat$SR.neg * b.SR.neg +
    combinationmat$SR.pos * b.SR.pos + combinationmat$RR * b.RR
  
  # calculate fitness
  survival.threshold <- ifelse(relative.survival.threshold == TRUE, 
                               survival.threshold*m,
                               survival.threshold)
  combinationmat$fitness <- ifelse(combinationmat$payoff < survival.threshold,
                                   0, combinationmat$payoff^epsilon)
  
  if (output.fitness.dist == FALSE) {
    # output expected fitness (probability-weighted average)
    return(weighted.mean(combinationmat$fitness, combinationmat$prob))
  } else if (output.fitness.dist == TRUE) {
    # Create a table containing only unique values of fitness with their respective summed probabilities
    uniquefitness <- data.table::as.data.table(combinationmat)[, sum(prob), by = fitness]
    return(list(expected_fitness = weighted.mean(combinationmat$fitness, 
                                                 combinationmat$prob),
                unique_fitnesses = uniquefitness))
  }
}

payoff.increments.fun <- function(m = 1, b.SR.neg = 0, b.SR.pos = 1, b.RR, 
                                epsilon = 1, survival.threshold = 0,
                                relative.survival.threshold = FALSE){
  
  # Create a df with event counts for all possible outcome combinations (ignoring order)
  combinationmat <- expand.grid(0:m, 0:m, 0:m)
  combinationmat <- combinationmat[rowSums(combinationmat)==m,]
  colnames(combinationmat) <- c("SR.neg", "SR.pos", "RR")
  
  # calculate total payoffs for each unique outcome combination
  payoff <- sort(unique(combinationmat$SR.neg * b.SR.neg +
    combinationmat$SR.pos * b.SR.pos + combinationmat$RR * b.RR))
  
  df <- data.frame(payoff = payoff,
                   fitness = rep(NA, length(payoff)))
  
  # calculate fitness for each outcome combination
  survival.threshold <- ifelse(relative.survival.threshold == TRUE, 
                               survival.threshold*m,
                               survival.threshold)
  df$fitness <- ifelse(df$payoff < survival.threshold, 0, df$payoff^epsilon)
  
  # output payoff increments and fitness increments
  return(df)
}

##############
## Variation: Implement what Chris said and apply epsilon after each decision event, 
##            not just at the time of the selection event
permutation.fun.new.2 <- function(m = 1, s, b.SR.neg = 0, b.SR.pos = 1, b.RR, 
                                epsilon = 1, survival.threshold = 0,
                                relative.survival.threshold = FALSE,
                                output.fitness.dist = FALSE){
  
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
  
  # calculate total payoffs for each outcome combination
  combinationmat$payoff <- (combinationmat$SR.neg * b.SR.neg^epsilon +
    combinationmat$SR.pos * b.SR.pos^epsilon + combinationmat$RR * b.RR^epsilon)
  
  # calculate fitness
  survival.threshold <- ifelse(relative.survival.threshold == TRUE, 
                               survival.threshold*m,
                               survival.threshold)
  combinationmat$fitness <- ifelse(combinationmat$payoff < survival.threshold,
                                   0, combinationmat$payoff)
  
  if (output.fitness.dist == FALSE) {
    # output expected fitness (probability-weighted average)
    return(weighted.mean(combinationmat$fitness, combinationmat$prob))
  } else if (output.fitness.dist == TRUE) {
    # Create a table containing only unique values of fitness with their respective summed probabilities
    uniquefitness <- data.table::as.data.table(combinationmat)[, sum(prob), by = fitness]
    return(list(expected_fitness = weighted.mean(combinationmat$fitness, 
                                                 combinationmat$prob),
                unique_fitnesses = uniquefitness))
  }
}
###########



x <- permutation.fun.new(m = 16, s = .9, b.SR.neg = 0, b.SR.pos = 1, b.RR = 0.5,
                    epsilon = 1, survival.threshold = 2.5, output.fitness.dist = TRUE)
df <- x$unique_fitnesses[order(x$unique_fitnesses$fitness),]
plot(df$fitness, df$V1, "b") 
abline(v = x$expected_fitness)

x <- payoff.increments.fun(m = 8, b.RR = .5, epsilon = 1/5, 
                           survival.threshold = 0.7, relative.survival.threshold = TRUE)
plot(x$payoff, x$fitness)

params <- data.table::as.data.table(expand.grid(m = 16,
                                                s = seq(from = 0.01, to = .99,
                                                        length.out = 100),
                                                b_SR_neg = 0,
                                                b_SR_pos = 1,
                                                b_RR = c(.3, .5, .7),
                                                #b_RR = 0.1,
                                                epsilon = c(.2, 1, 5),
                                                survival_threshold = c(.1,.3,.5,.7,.9),
                                                relative.survival.threshold = TRUE))

params$expected_fitness <- apply(params, 1, function(x) permutation.fun.new(m = x[1], s = x[2], b.RR = x[5], epsilon = x[6], survival.threshold = x[7], relative.survival.threshold = x[8]))

s_max_Efitness <- params[params[, .I[expected_fitness == max(expected_fitness)], by = .(m, epsilon, b_RR, survival_threshold)]$V1]

hist(params$expected_fitness)

saveRDS(params, "data_nonevo_relative-delta_cont.RData")
saveRDS(s_max_Efitness, "data_nonevo_relative-delta-cont_maxEfitness.RData")


# 8th March 2023: Re-ran all the above again for the first time since Sept.
# Save params:
params.a <- params
# Save data with params.a:
s_max_Efitness.a <- s_max_Efitness

colorbrewer5 <- c("#2166ac", "#67a9cf", "grey", "#ef8a62", "#b2182b")
colorbrewer3 <- c("#2166ac", "grey", "#b2182b")
library(ggplot2)

# traditional plot with b_RR on x-axis
ggplot(s_max_Efitness, aes(x = survival_threshold, y = s, colour = as.factor(epsilon))) + 
  scale_x_continuous(name="b_RR",
                     #limits = c(0, 1),
                     breaks = seq(0, 1, .1),
                     labels = c("", ".1", "", ".3", "", ".5", 
                                "", ".7", "", ".9", ""),
                     expand = c(0, 0)) +
  scale_y_continuous(name="s",
                     breaks = seq(0, 1, .1),
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  #coord_fixed(ratio = 1/1) +
  scale_colour_manual(values = colorbrewer3, name = expression(epsilon)) +
  theme_bw() +
  geom_path() +
  theme(panel.grid.minor.x = element_blank()) +
  facet_grid(b_RR ~ m)

# new plot split by  b_RR with delta on x-axis
ggplot(s_max_Efitness, aes(x = m, y = s, colour = as.factor(epsilon))) + 
  scale_x_continuous(name="m",
                     #limits = c(0, 1),
                     #breaks = seq(0, 1, .1),
                     # labels = c("", ".1", "", ".3", "", ".5", 
                     #            "", ".7", "", ".9", ""),
                     expand = c(0, 0)) +
  scale_y_continuous(name="s",
                     breaks = seq(0, 1, .1),
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  #coord_fixed(ratio = 1/1) +
  scale_colour_manual(values = colorbrewer3, name = expression(epsilon)) +
  theme_bw() +
  geom_path() +
  theme(panel.grid.minor.x = element_blank()) +
  facet_grid(b_RR ~ survival_threshold)



# new plot with one fixed b_RR and delta on x-axis
deltaplot_new <- ggplot(s_max_Efitness, aes(x = survival_threshold, y = s, colour = as.factor(epsilon))) + 
  scale_x_continuous(name="survival threshold (delta)",
                     #limits = c(0, 1),
                     breaks = seq(0, 1, .1),
                     labels = c("", ".1", "", ".3", "", ".5", 
                                "", ".7", "", ".9", ""),
                     expand = c(0, 0)) +
  scale_y_continuous(name="s",
                     breaks = seq(0, 1, .1),
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  coord_fixed(ratio = 1/1) +
  scale_colour_manual(values = colorbrewer3, name = expression(epsilon)) +
  theme_bw() +
  #theme(panel.grid.minor.x = element_blank())+
  geom_path() +
  facet_grid(b_RR ~ m)


s_max_Efitness_m16bRR.1 <- s_max_Efitness[s_max_Efitness$m == 16 & s_max_Efitness$b_RR == 0.1,]
m16bRR.1 <- payoff.increments.fun(m = 16, b.RR = .1, epsilon = 1, 
                           survival.threshold = 0, relative.survival.threshold = TRUE)$payoff

ggplot(s_max_Efitness_m16bRR.1, aes(x = survival_threshold, y = s, colour = as.factor(epsilon))) + 
  scale_x_continuous(name="survival threshold (delta)",
                     #limits = c(0, 1),
                     breaks = m16bRR.1/16,
                     # labels = c("", ".1", "", ".3", "", ".5", 
                     #            "", ".7", "", ".9", ""),
                     expand = c(0, 0)) +
  scale_y_continuous(name="s",
                     breaks = seq(0, 1, .1),
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  coord_fixed(ratio = 1/1) +
  scale_colour_manual(values = colorbrewer3, name = expression(epsilon)) +
  theme_bw() +
  #theme(panel.grid.minor.x = element_blank())+
  geom_path() 


# tile plot
df_tile <- df_test2_max_fitness
df_tile$epsilon <- as.factor(as.character(df_tile$epsilon))
levels(df_tile$epsilon) <- c("epsilon = 0.2", "epsilon = 1", "epsilon = 5")

df_tile$m <- as.factor(as.character(df_tile$m))
df_tile$m <- factor(df_tile$m,levels = c("1", "2", "4", "8", "16", "32"))

ggplot(df_tile, 
       aes(x = b_RR,
           y = m,
           fill = s)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[RR]),
                     breaks = seq(0, 1, .1),
                     labels = c("0",".1", ".2", ".3", ".4", ".5", 
                                ".6", ".7", ".8", ".9", "1"))+
  scale_y_discrete(expand = c(0,0), 
                   name = "decision events before evaluation (m)")+
  coord_fixed(ratio = 1/12) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,0), "mm"),
        legend.title.align=0.2) +
  geom_tile() +
  scale_fill_viridis_c(name = "s", limits = c(0,1), option = "magma")+ 
  facet_grid(epsilon ~ .)


ggplot(df[df$epsilon==.2,], aes(x = b_RR, y = s, fill = expected_fitness)) +
  scale_x_continuous(expand = c(0,0), name = "b_RR",
                     breaks = seq(0, 1, .1),
                     labels = c("0",".1", ".2", ".3", ".4", ".5", 
                                ".6", ".7", ".8", ".9", "1"))+
  scale_y_continuous(expand = c(0,0), 
                     name = "submission strategy (s)",
                     breaks = seq(0, 1, .1),
                     labels = c("0",".1", ".2", ".3", ".4", ".5", 
                                ".6", ".7", ".8", ".9", "1"))+
  coord_fixed(ratio = 1/1) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,0), "mm"),
        legend.title.align=0.2) +
  geom_tile() +
  scale_fill_viridis_c(name = "expected fitness", 
                       option = "magma")
+ facet_grid(epsilon ~ .)




ggplot(df_max_fitness, aes(b_RR, expected_fitness, colour = as.factor(s))) +
  # scale_x_continuous(name="submission strategy", limits=c(0, 1),
  #                    expand = c(0,0)) +
  # scale_y_continuous(name="expected fitness", 
  #                    limits=c(0, 1),
  #                    expand = c(0,0)) +
  # scale_color_manual(values = colorbrewer5) +
  theme_bw() +
  geom_path() + 
  facet_wrap(~ epsilon, ncol = 3)