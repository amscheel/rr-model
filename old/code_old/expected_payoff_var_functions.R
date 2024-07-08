library(ggplot2)

# function calculating expected payoff (when epsilon = 1) or expected fitness (when epsilon!=1)
b.expected <- function(b_SR_neg = 0, 
                       relative_b_SR_pos = 2, 
                       relative_b_RR = .5, s, epsilon = 1) {
  b_SR_pos <- b_SR_neg + relative_b_SR_pos
  b_RR <- relative_b_RR * (b_SR_neg + b_SR_pos)
  p_SR <- (s + 1)/2
  s*b_RR^epsilon + (1-s)*((1-p_SR)*b_SR_neg^epsilon + p_SR*b_SR_pos^epsilon)
  #(s*b_RR + (1-s)*((1-p_SR)*b_SR_neg + p_SR*b_SR_pos))^epsilon
  # x <- s*b_RR + (1-s)*((1-p_SR)*b_SR_neg + p_SR*b_SR_pos)
  # ifelse(x < survival_threshold, 0, x^epsilon)
}

var.expected <- function(b_SR_neg = 0, 
                         relative_b_SR_pos = 2, 
                         relative_b_RR = .5, s, epsilon = 1) {
  b_SR_pos <- b_SR_neg + relative_b_SR_pos
  b_RR <- relative_b_RR * (b_SR_neg + b_SR_pos)
  p_SR <- (s + 1)/2
  (1-s) * (p_SR * b_SR_pos^epsilon - (1-p_SR) * b_SR_neg^epsilon)
}

df <- expand.grid(payoff_SR_neg = 0,
                  relative_payoff_SR_pos = 2,
                  relative_payoff_RR = .5,
                  s = seq(0, 1, .01),
                  epsilon = c(.5, 1, 2))
df$b_expected <- b.expected(df$payoff_SR_neg, 
                            df$relative_payoff_SR_pos,
                            df$relative_payoff_RR, df$s, df$epsilon)
df$var_expected <- var.expected(df$payoff_SR_neg, 
                                df$relative_payoff_SR_pos,
                                df$relative_payoff_RR, df$s, df$epsilon)

ggplot(df, aes(s, var_expected, colour = factor(epsilon))) +
  scale_x_continuous(name="submission strategy", limits=c(0, 1)) +
  scale_y_continuous(name="expected payoff", limits=c(0, 3)) +
  scale_color_viridis_d() +
  theme_bw() +
  geom_path() +
  geom_path(aes(y = var_expected), linetype = "dotted")

ggplot(data.frame(s=c(0,1)), aes(x = s)) + 
  scale_x_continuous(name="submission strategy", limits=c(0, 1)) +
  scale_y_continuous(name="expected payoff", limits=c(0, 3)) +
  stat_function(fun=b_expected, 
                args = list(b_RR = .5,
                            b_SR_neg = 0,
                            b_SR_pos = 1,
                            epsilon = 1/2),
                geom="line", color = viridis::viridis(3)[1]) + 
  stat_function(fun=b_expected, 
                args = list(b_RR = .5,
                            b_SR_neg = 0,
                            b_SR_pos = 1,
                            epsilon = 1),
                geom="line", color = viridis::viridis(3)[2]) + 
  stat_function(fun=b_expected, 
                args = list(b_RR = .5,
                            b_SR_neg = 0,
                            b_SR_pos = 1,
                            epsilon = 2),
                geom="line", color = viridis::viridis(3)[3]) + 
  theme_bw()

#------------------------------------------------------------------#

# Expected payoff for SRs, arithmetic mean version:
expected.payoff.SR <- function(payoff_SR_neg = 0, 
                               payoff_SR_pos = 1, prior){
  prior*payoff_SR_pos + (1-prior)*payoff_SR_neg
}

# Expected payoff for SRs, geometric mean version:
expected.payoff.SR.geom <- function(payoff_SR_neg = 0, 
                                    payoff_SR_pos = 1, prior){
  payoff_SR_pos^prior * payoff_SR_neg^(1-prior)
}


# plot arithmetic vs geometric expected payoff
ggplot(data.frame(x=c(0,1)), aes(x)) + 
  scale_x_continuous(name="prior", limits = c(0, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(name="E[b_SR]", limits = c(0, 1), 
                     expand = c(0, 0)) +
  theme_bw() +
  coord_fixed(ratio = 1/1) +
  stat_function(fun=expected.payoff.SR, 
                args = list(payoff_SR_neg = .01,
                            payoff_SR_pos = 1),
                geom="line", color = viridis::viridis(3)[2]) + 
  stat_function(fun=expected.payoff.SR.geom, 
                args = list(payoff_SR_neg = .01,
                            payoff_SR_pos = 1),
                geom="line", color = viridis::viridis(3)[1])

# arithmetic expected fitness of SRs alone
expected.fitness.SR <- function(b_SR_neg = 0, b_SR_pos = 1, 
                                prior, epsilon = 1) {
  prior*(b_SR_pos^epsilon) + (1-prior)*(b_SR_neg^epsilon)
}
# geometric expected fitness of SRs alone
expected.fitness.SR.geom <- function(b_SR_neg = 0, b_SR_pos = 1, 
                                prior, epsilon = 1) {
  (b_SR_pos^epsilon)^prior * (b_SR_neg^epsilon)^(1-prior)
}

# plot arithmetic vs geometric expected fitness of SRs by prior
ggplot(data.frame(x=c(0,1)), aes(x)) + 
  scale_x_continuous(name="prior", limits = c(0, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(name="E[u(b_SR)]", limits = c(0, 1), 
                     expand = c(0, 0)) +
  theme_bw() +
  coord_fixed(ratio = 1/1) +
  stat_function(fun=expected.fitness.SR, 
                args = list(b_SR_neg = .01,
                            b_SR_pos = 2,
                            epsilon = 1/2),
                geom="line", color = viridis::viridis(3)[2]) + 
  stat_function(fun=expected.fitness.SR.geom, 
                args = list(b_SR_neg = .01,
                            b_SR_pos = 2,
                            epsilon = 1/2),
                geom="line", color = viridis::viridis(3)[1])

#------------------------------------------------------------------#

# arithmetic expected fitness (general)
expected.fitness.arith <- function(s, epsilon, b_SR_neg, rel_b_SR_pos, rel_b_RR){
  b_SR_pos <- b_SR_neg + rel_b_SR_pos
  b_RR <- rel_b_RR * (b_SR_neg + b_SR_pos)
  p_SR <- (s + 1)/2
  s * b_RR^epsilon + (1-s)*(p_SR * b_SR_pos^epsilon + (1-p_SR) * b_SR_neg^epsilon)
}

# geometric expected fitness (general)
expected.fitness.geom <- function(s, epsilon, b_SR_neg, rel_b_SR_pos, rel_b_RR){
  b_SR_pos <- b_SR_neg + rel_b_SR_pos
  b_RR <- rel_b_RR * (b_SR_neg + b_SR_pos)
  p_SR <- (s + 1)/2
  (b_RR^epsilon)^s * ((b_SR_pos^epsilon)^p_SR * (b_SR_neg^epsilon)^(1-p_SR))^(1-s)
}


b_SR_neg <- 0.0000001
rel_b_SR_pos <- 1
rel_b_RR <- .5
epsilon = 1

# plot arithmetic vs geometric expected fitness for everything by strategy s
ggplot(data.frame(x=c(0,1)), aes(x)) + 
  scale_x_continuous(name="submission strategy", limits = c(0, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(name="E[u(b)]", 
                     limits = c(0, 1), 
                     expand = c(0, 0)) +
  theme_bw() +
  #coord_fixed(ratio = 1/1) +
  stat_function(fun=expected.fitness.arith, 
                args = list(b_SR_neg = b_SR_neg,
                            rel_b_SR_pos = rel_b_SR_pos,
                            rel_b_RR = rel_b_RR,
                            epsilon = epsilon),
                geom="line", color = viridis::viridis(3)[2]) + 
  stat_function(fun=expected.fitness.geom, 
                args = list(b_SR_neg = b_SR_neg,
                            rel_b_SR_pos = rel_b_SR_pos,
                            rel_b_RR = rel_b_RR,
                            epsilon = epsilon),
                geom="line", color = viridis::viridis(3)[1])


df <- expand.grid(b_SR_neg = 0.00001,
                  rel_b_SR_pos = 1,
                  rel_b_RR = .5,
                  s = seq(0, 1, .01),
                  epsilon = c(.2, .5, 1, 2, 5))

df$expected_u_arith <- expected.fitness.arith(df$s, df$epsilon, 
                                              df$b_SR_neg, 
                                              df$rel_b_SR_pos,
                                              df$rel_b_RR)
df$expected_u_geom <- expected.fitness.geom(df$s, df$epsilon, 
                                            df$b_SR_neg, 
                                            df$rel_b_SR_pos,
                                            df$rel_b_RR)

colorbrewer5 <- c("#2166ac", "#67a9cf", "grey", "#ef8a62", "#b2182b")

ggplot(df, aes(s, expected_u_arith, colour = factor(epsilon))) +
  scale_x_continuous(name="submission strategy", limits=c(0, 1),
                     expand = c(0,0)) +
  scale_y_continuous(name="expected fitness", 
                     limits=c(0, 1),
                     expand = c(0,0)) +
  scale_color_manual(values = colorbrewer5) +
  theme_bw() +
  geom_path() +
  geom_path(aes(y = expected_u_geom), linetype = "dotted")

ggplot(data.frame(x=c(0,10)), aes(x)) + 
  scale_x_continuous(name="x", 
                     #limits = c(0, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(name="prob", 
                     #limits = c(0, 1), 
                     expand = c(0, 0)) +
  theme_bw() +
  stat_function(fun=pbinom, 
                args = list(size = 4,
                            prob = .75),
                geom="line", color = viridis::viridis(3)[2])

#------------------------------------------------------------------#

# Functions to find the submission strategy (s) that (approximately!) maximises 
# a) arithmetic expected fitness
# b) geometric expected fitness

max.fitness.arith <- function(b.SR.neg = 0, b.SR.pos = 1, 
                              b.RR = c(.1, .2, .3, .4, .5, .6, .7, .8, .9),
                              m = 1, survival.threshold = 0, 
                              epsilon = c(.2, 1, 5)){
  df <- expand.grid(m = 8,
                    s = seq(0, 1, .1),
                    b.SR.neg = 0,
                    b.SR.pos = 1,
                    b.RR = seq(0, 1, .01),
                    epsilon = c(.2, .5, 1, 2, 5),
                    survival.threshold = 4)
  
  prior.SR <- (s+1)/2
  prob.RR <- s
  prob.SR.neg <- (1-s)*(1-prior.SR)
  prob.SR.pos <- (1-s)*prior.SR 
  
  df$expected_fitness <- apply(df, 1, function(x) permutation.fun(m = x[1], s = x[2], b.RR = x[5], epsilon = x[6], survival.threshold = x[7]))

}


expected.fitness.fun <- function(m = 1, survival.threshold = 0, epsilon = 1, 
                                 b.RR = .5, b.SR.neg = 0.000000001, b.SR.pos = 1){
  s <- seq(0,1,.01)

  prior.SR <- (s+1)/2
  
  prob.RR <- m*s
  prob.SR.neg <- m*(1-s)*(1-prior.SR)
  prob.SR.pos <- m*(1-s)*prior.SR 
  
  # calculate arithmetic expected fitness
  expected.payoff.arith <- b.RR*prob.RR + b.SR.neg*prob.SR.neg + b.SR.pos*prob.SR.pos
  expected.fitness.arith <- ifelse(expected.payoff.arith < survival.threshold, 0,
                                   expected.payoff.arith^epsilon)
  
  # s at which arithmetic expected fitness is maximal
  s[which(expected.fitness.arith==max(expected.fitness.arith))]
  
  # calculate geometric expected fitness
  expected.payoff.geom <- b.RR^prob.RR * b.SR.neg^prob.SR.neg * b.SR.pos^prob.SR.pos
  expected.fitness.geom <- ifelse(expected.payoff.geom < survival.threshold, 0,
                                  expected.payoff.geom^epsilon)
  
  # s at which geometric expected fitness is maximal
  s[which(expected.payoff.geom==max(expected.payoff.geom))]
  
  payoff.RR <- m * s * b.RR # expected RR payoff
  payoff.SR.pos <- m * (1-s) * prior.SR * b.SR.pos # expected pos SR payoff
  payoff.SR.neg <- m * (1-s) * (1-prior.SR) * b.SR.neg # expected neg SR payoff
  
  payoff.SR <- payoff.SR.neg + payoff.SR.pos
  total.payoff <- payoff.RR + payoff.SR.neg + payoff.SR.pos
  fitness <- ifelse(total.payoff < survival.threshold, 0, total.payoff^epsilon)
  # invisible(list(s = s,
  #                payoffs_RR = payoff.RR,
  #                payoffs_SR = payoff.SR,
  #                total_payoff = total.payoff,
  #                fitness = fitness))
  fitness
}




expected.fitness <- function(b.RR, b.SR.neg, b.SR.pos, s, generation.duration) {
  p.SR <- (s+1)/2
  dmultinom(x = c(b.RR, b.SR.neg, b.SR.pos), prob = c(s, (1-s)*(1-p.SR), (1-s)*p.SR))
}

b_RR <- .5
b_SR_neg <- 0
b_SR_pos <- 1
generation_duration <- 4

expected.fitness(b.RR = b_RR, b.SR.neg = b_SR_neg, b.SR.pos = b_SR_pos, 
                 generation.duration = generation_duration, s = c(0.4, .6))

s <- .9
p_SR <- (s+1)/2

dmultinom(x = c(c(0,0,4), c(1,2,1)), 
          prob = c(s, (1-s)*(1-p.SR), (1-s)*p.SR))


library(gtools) # need this to create a permutation matrix
library(arrangements)
library(microbenchmark)


permutation.fun.old <- function(m = 1, s, b.SR.neg = 0, b.SR.pos = 1, b.RR, 
                            epsilon = 1, survival.threshold = 0){
 
  prior.SR <- (s+1)/2
  prob.RR <- s
  prob.SR.neg <- (1-s)*(1-prior.SR)
  prob.SR.pos <- (1-s)*prior.SR 
  
   # Create a permutation matrix that contains all possible combinations of three outcomes for m
  permutationmat <- data.table::as.data.table(arrangements::permutations(
    k = m, n = 3, replace = TRUE))
  
  # create new colums with counts of the occurrences of each outcome
  permutationmat[, `:=`(sum1 = apply(.SD, 1, function(x) length(which(x==1))),
                        sum2 = apply(.SD, 1, function(x) length(which(x==2))),
                        sum3 = apply(.SD, 1, function(x) length(which(x==3))))]
  
  # Create a new table with only the unique combinations and their frequencies
  uniqueoutcomes <- permutationmat[, .N, by = c("sum1", "sum2", "sum3")]
  
  # calculate the probability of each outcome combination
  uniqueoutcomes$prob <- prob.SR.neg^uniqueoutcomes$sum1 * 
    prob.RR^uniqueoutcomes$sum2 * prob.SR.pos^uniqueoutcomes$sum3 *
    uniqueoutcomes$N
  
  # calculate total payoffs for each unique outcome combination
  uniqueoutcomes$payoff <- uniqueoutcomes$sum1 * b.SR.neg +
    uniqueoutcomes$sum2 * b.RR + uniqueoutcomes$sum3 * b.SR.pos
  uniqueoutcomes$fitness <- ifelse(uniqueoutcomes$payoff < survival.threshold,
                                   0, uniqueoutcomes$payoff^epsilon)

    return(weighted.mean(uniqueoutcomes$fitness, uniqueoutcomes$prob))
}

permutation.fun.new <- function(m = 1, s, b.SR.neg = 0, b.SR.pos = 1, b.RR, 
                                epsilon = 1, survival.threshold = 0,
                                survival.threshold.rel = F,
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
  
  # output expected fitness (probability-weighted average)
  return(weighted.mean(combinationmat$fitness, combinationmat$prob))
}


# This function calculates information entropy
entropy.fun <- function(m = 1, s, b.SR.neg = 0, b.SR.pos = 1, b.RR, 
                    epsilon = 1, survival.threshold = 0){
  
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
  
  # calculate total payoffs for each outcome combination
  combinationmat$payoff <- combinationmat$SR.neg * b.SR.neg +
    combinationmat$SR.pos * b.SR.pos + combinationmat$RR * b.RR
  combinationmat$fitness <- ifelse(combinationmat$payoff < survival.threshold,
                                   0, combinationmat$payoff^epsilon)
  
  # Create a table containing only unique values of fitness with their respective summed probabilities
  uniquefitness <- data.table::as.data.table(combinationmat)[, sum(prob), by = fitness]
  # Check that probabilities were summed correctly: 
  # sum(uniquefitness$V1) # should return 1
  
  # output information entropy for fitness
  
  return(-sum(uniquefitness$V1*log(uniquefitness$V1)))
}


permutation.fun.old(m = 1, s = .4, b.SR.neg = 0, b.SR.pos = 1, b.RR = 0.9,
                     epsilon = 2, survival.threshold = 1)
permutation.fun.new(m = 2, s = .4, b.SR.neg = 0, b.SR.pos = 1, b.RR = 0.5,
                    epsilon = 1, survival.threshold.rel = F, survival.threshold = 0.5)
entropy.fun(m = 1, s = 0, b.SR.neg = 0, b.SR.pos = 1, b.RR = 0.5,
            epsilon = 2, survival.threshold = 1)

# Note: Noise of sd = 0.002 or .005 doesn't seem to do much
params <- data.table::as.data.table(expand.grid(m = c(16, 32),
                                                s = seq(from = 0.01, to = .99,
                                                        length.out = 100),
                                                b_SR_neg = 0,
                                                b_SR_pos = 1,
                                                b_RR = seq(0.1, 0.9, 0.1),
                                                epsilon = c(0.2, 1, 5),
                                                survival_threshold.rel = T,
                                                survival_threshold = c(0, .2, 0.5, 0.8),
                                                noise = 0))

params$expected_fitness <- apply(params, 1, function(x) permutation.fun.new(m = x[1], s = x[2], b.RR = x[5], epsilon = x[6], survival.threshold.rel = x[7], survival.threshold = x[8], noise = x[9]))

params1 <- params
params2 <- rbind(params1, params)
params <- params2
s_max_Efitness <- params[params[, .I[expected_fitness == max(expected_fitness)], by = .(m, epsilon, b_RR, survival_threshold.rel, survival_threshold, noise)]$V1]

params$entropy <- apply(params, 1, function(x) entropy.fun(m = x[1], s = x[2], b.RR = x[5], epsilon = x[6], survival.threshold = x[7]))

s_max_entropy <- params[params[, .I[entropy == max(entropy, na.rm = TRUE)], by = .(m, epsilon, b_RR, survival_threshold)]$V1]

s_min_entropy <- params[params[, .I[entropy == min(entropy)], by = .(m, epsilon, b_RR, survival_threshold)]$V1]

mean_entropy <- params[, mean(entropy, na.rm = TRUE), by = .(m, epsilon, b_RR, survival_threshold)]

colorbrewer5 <- c("#2166ac", "#67a9cf", "grey", "#ef8a62", "#b2182b")
colorbrewer3 <- c("#2166ac", "grey", "#b2182b")


# Plotting
df_regular <- s_max_Efitness
df_regular$m <- factor(df_regular$m)
levels(df_regular$m) <- c("m = 1", "m = 2", "m = 4", "m = 8", "m = 16", "m = 32")

df_regular$epsilon <- as.factor(df_regular$epsilon)
levels(df_regular$epsilon) <- c("epsilon = 0.2", "epsilon = 1", "epsilon = 5")

df_regular$survival_threshold <- as.factor(df_regular$survival_threshold)
levels(df_regular$survival_threshold) <- c("no threshold", "20% threshold", 
                                        "50% threshold", "80% threshold")


regular_plot <- ggplot(df_regular, aes(x = b_RR, y = s, 
                           colour = as.factor(epsilon))) + 
  scale_x_continuous(name = expression(italic(b)[RR]),
                     limits = c(0, 1),
                     breaks = seq(0.1, 0.9, .2),
                     expand = c(0, 0)) +
  scale_y_continuous(name="s",
                     breaks = seq(0, 1, .2),
                     limits = c(0, 1),
                     expand = c(0, 0)) +
  # annotate("segment", x = survival_threshold, xend = survival_threshold,
  #          y = 0, yend = 1, 
  #          linetype = "dotted", colour = "black") +
  scale_colour_manual(values = colorbrewer3, name = expression(epsilon)) +
  theme_minimal() +
  geom_point()+
  geom_path() +
  facet_grid(survival_threshold ~ m)
  # facet_wrap(~ m)

  ggsave("plot_delta_new.png", regular_plot, bg = "white",
         width = 22, height = 16, units = "cm")
  
  
data_bRR.1.9_m2.32_delta0.8_epsilon.2.5 <- s_max_Efitness
plot1


# tile plot
df_tile <- s_max_Efitness
df_tile$m <- as.factor(as.character(df_tile$m))
levels(df_tile$m) <- c("1", "2", "4", "8", "16", "32")

df_tile$epsilon <- as.factor(df_tile$epsilon)
levels(df_tile$epsilon) <- c("risk averse", "neutral", "risk prone")

df_tile$survival_threshold <- as.factor(df_tile$survival_threshold)
levels(df_tile$survival_threshold) <- c("no threshold", "20% threshold", 
                                           "50% threshold", "80% threshold")

delta_tile <- ggplot(df_tile, 
       aes(x = b_RR,
           y = m,
           fill = s)) +
  scale_x_continuous(expand = c(0,0), name = expression(italic(b)[RR]),
                     breaks = seq(0, 1, .1),
                     labels = c("0",".1", ".2", ".3", ".4", ".5", 
                                ".6", ".7", ".8", ".9", "1"))+
  scale_y_discrete(expand = c(0,0), 
                   name = "research cycles before evaluation (m)")+
  coord_fixed(ratio = 1/12) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        plot.margin=grid::unit(c(1,0,1,0), "mm"),
        legend.title.align=0.2) +
  geom_tile() +
  scale_fill_viridis_c(name = "s", limits = c(0,1), option = "mako")+ 
  facet_grid(epsilon ~ survival_threshold)

ggsave("plot_delta_tile.png", delta_tile, bg = "white",
       width = 22, height = 10, units = "cm")


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

df <- data.frame(payoff = permutation.fun(m =8, s = 0, b.SR.neg = 0, 
                                          b.SR.pos = 1, b.RR = 0.5)$payoff,
                 s_0 = permutation.fun(m =8, s = 0, b.SR.neg = 0, 
                                       b.SR.pos = 1, b.RR = 0.5)$prob,
                 s_0.1 = permutation.fun(m =8, s = 0.1, b.SR.neg = 0, 
                                       b.SR.pos = 1, b.RR = 0.5)$prob,
                 s_0.2 = permutation.fun(m =8, s = 0.2, b.SR.neg = 0, 
                                         b.SR.pos = 1, b.RR = 0.5)$prob,
                 s_0.3 = permutation.fun(m =8, s = 0.3, b.SR.neg = 0, 
                                         b.SR.pos = 1, b.RR = 0.5)$prob,
                 s_0.4 = permutation.fun(m =8, s = 0.4, b.SR.neg = 0, 
                                         b.SR.pos = 1, b.RR = 0.5)$prob,
                 s_0.5 = permutation.fun(m =8, s = 0.5, b.SR.neg = 0, 
                                         b.SR.pos = 1, b.RR = 0.5)$prob,
                 s_0.6 = permutation.fun(m =8, s = 0.6, b.SR.neg = 0, 
                                         b.SR.pos = 1, b.RR = 0.5)$prob,
                 s_0.7 = permutation.fun(m =8, s = 0.7, b.SR.neg = 0, 
                                         b.SR.pos = 1, b.RR = 0.5)$prob,
                 s_0.8 = permutation.fun(m =8, s = 0.8, b.SR.neg = 0, 
                                         b.SR.pos = 1, b.RR = 0.5)$prob,
                 s_0.9 = permutation.fun(m =8, s = 0.9, b.SR.neg = 0, 
                                         b.SR.pos = 1, b.RR = 0.5)$prob,
                 s_1 = permutation.fun(m =8, s = 1, b.SR.neg = 0, 
                                         b.SR.pos = 1, b.RR = 0.5)$prob)

df_long <- data.table::melt.data.table(data.table::as.data.table(df), 
                                       id.vars = "payoff", 
                                       variable.name = "s", value.name = "prob")


ggplot(df_long, aes(payoff, prob, colour = factor(s))) +
  # scale_x_continuous(name="submission strategy", limits=c(0, 1),
  #                    expand = c(0,0)) +
  # scale_y_continuous(name="expected fitness", 
  #                    limits=c(0, 1),
  #                    expand = c(0,0)) +
  # scale_color_manual(values = colorbrewer5) +
  theme_bw() +
  geom_path() +
  geom_path(aes(y = expected_u_geom), linetype = "dotted")






# CAUTION: THIS IS PROBABLY WRONG 
# (gives weird results / probabilities don't sum to 1)
# calculate the multinomial probability of each combination of outcomes:
permutationmat$prob <- apply(permutationmat[, c("sum1", "sum2", "sum3")], 
                             1, function(y) dmultinom(
                               x = y, prob = c(s, (1-s)*(1-p.SR), (1-s)*p.SR)))

sum(permutationmat$prob)




## all possible outcomes of Multinom(N = 3, K = 3)
X <- t(as.matrix(expand.grid(0:3, 0:3)))
X <- X[, colSums(X) <= 3]
X <- rbind(X, 3:3 - colSums(X))
dimnames(X) <- list(letters[1:3], NULL)
X
round(apply(X, 2, function(x) dmultinom(x, prob = c(1,2,5))), 3)







#--------------------------------------------------------------------#


binomial.payoff.SR <- function(payoff_SR_neg = 0, payoff_SR_pos = 1, subm_strategy, m, k){
  prior.SR <- (subm_strategy + 1)/2
  choose(m, k) * prior.SR^k * (1-prior.SR)^(m-k)
}

plot(x = c(0:4), binomial.payoff.SR(subm_strategy = 0.5, m = 4, k = c(0:4)))
plot(x = c(0:4), dbinom(c(0:4), size = 4, prob = (0.5+1)/2))

# plot arithmetic vs geometric expected payoff
ggplot(data.frame(x=c(0,4)), aes(x)) + 
  scale_x_continuous(name="k", limits = c(0, 4),
                     expand = c(0, 0)) +
  scale_y_continuous(name="frequency", limits = c(0, 1), 
                     expand = c(0, 0)) +
  theme_bw() +
  coord_fixed(ratio = 1/1) +
  stat_function(fun=binomial.payoff.SR, 
                args = list(subm_strategy = 0,
                            m = 4,
                            k = x),
                geom="line", color = viridis::viridis(3)[2]) 
