library(ggplot2)

# function calculating expected payoff (when e = 1) or expected utility (when e!=1)
b.expected <- function(b_SR_neg = 0, 
                       relative_b_SR_pos = 2, 
                       relative_b_RR = .5, s, e = 1) {
  b_SR_pos <- b_SR_neg + relative_b_SR_pos
  b_RR <- relative_b_RR * (b_SR_neg + b_SR_pos)
  p_SR <- (s + 1)/2
  s*b_RR^e + (1-s)*((1-p_SR)*b_SR_neg^e + p_SR*b_SR_pos^e)
  #(s*b_RR + (1-s)*((1-p_SR)*b_SR_neg + p_SR*b_SR_pos))^e
  # x <- s*b_RR + (1-s)*((1-p_SR)*b_SR_neg + p_SR*b_SR_pos)
  # ifelse(x < survival_threshold, 0, x^e)
}

var.expected <- function(b_SR_neg = 0, 
                         relative_b_SR_pos = 2, 
                         relative_b_RR = .5, s, e = 1) {
  b_SR_pos <- b_SR_neg + relative_b_SR_pos
  b_RR <- relative_b_RR * (b_SR_neg + b_SR_pos)
  p_SR <- (s + 1)/2
  (1-s) * (p_SR * b_SR_pos^e - (1-p_SR) * b_SR_neg^e)
}

df <- expand.grid(payoff_SR_neg = 0,
                  relative_payoff_SR_pos = 2,
                  relative_payoff_RR = .5,
                  s = seq(0, 1, .01),
                  e = c(.5, 1, 2))
df$b_expected <- b.expected(df$payoff_SR_neg, 
                            df$relative_payoff_SR_pos,
                            df$relative_payoff_RR, df$s, df$e)
df$var_expected <- var.expected(df$payoff_SR_neg, 
                                df$relative_payoff_SR_pos,
                                df$relative_payoff_RR, df$s, df$e)

ggplot(df, aes(s, var_expected, colour = factor(e))) +
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
                            e = 1/2),
                geom="line", color = viridis::viridis(3)[1]) + 
  stat_function(fun=b_expected, 
                args = list(b_RR = .5,
                            b_SR_neg = 0,
                            b_SR_pos = 1,
                            e = 1),
                geom="line", color = viridis::viridis(3)[2]) + 
  stat_function(fun=b_expected, 
                args = list(b_RR = .5,
                            b_SR_neg = 0,
                            b_SR_pos = 1,
                            e = 2),
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

# arithmetic expected utility of SRs alone
expected.utility.SR <- function(b_SR_neg = 0, b_SR_pos = 1, 
                                prior, e = 1) {
  prior*(b_SR_pos^e) + (1-prior)*(b_SR_neg^e)
}
# geometric expected utility of SRs alone
expected.utility.SR.geom <- function(b_SR_neg = 0, b_SR_pos = 1, 
                                prior, e = 1) {
  (b_SR_pos^e)^prior * (b_SR_neg^e)^(1-prior)
}

# plot arithmetic vs geometric expected utility of SRs by prior
ggplot(data.frame(x=c(0,1)), aes(x)) + 
  scale_x_continuous(name="prior", limits = c(0, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(name="E[u(b_SR)]", limits = c(0, 1), 
                     expand = c(0, 0)) +
  theme_bw() +
  coord_fixed(ratio = 1/1) +
  stat_function(fun=expected.utility.SR, 
                args = list(b_SR_neg = .01,
                            b_SR_pos = 2,
                            e = 1/2),
                geom="line", color = viridis::viridis(3)[2]) + 
  stat_function(fun=expected.utility.SR.geom, 
                args = list(b_SR_neg = .01,
                            b_SR_pos = 2,
                            e = 1/2),
                geom="line", color = viridis::viridis(3)[1])

# arithmetic expected utility (general)
expected.utility.arith <- function(s, e, b_SR_neg, rel_b_SR_pos, rel_b_RR){
  b_SR_pos <- b_SR_neg + rel_b_SR_pos
  b_RR <- rel_b_RR * (b_SR_neg + b_SR_pos)
  p_SR <- (s + 1)/2
  s * b_RR^e + (1-s)*(p_SR * b_SR_pos^e + (1-p_SR) * b_SR_neg^e)
}

# geometric expected utility (general)
expected.utility.geom <- function(s, e, b_SR_neg, rel_b_SR_pos, rel_b_RR){
  b_SR_pos <- b_SR_neg + rel_b_SR_pos
  b_RR <- rel_b_RR * (b_SR_neg + b_SR_pos)
  p_SR <- (s + 1)/2
  (b_RR^e)^s * ((b_SR_pos^e)^p_SR * (b_SR_neg^e)^(1-p_SR))^(1-s)
}


b_SR_neg <- 0.001
rel_b_SR_pos <- 1
rel_b_RR <- .5
e = 1

# plot arithmetic vs geometric expected utility for everything by strategy s
ggplot(data.frame(x=c(0,1)), aes(x)) + 
  scale_x_continuous(name="submission strategy", limits = c(0, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(name="E[u(b)]", 
                     limits = c(0, 1), 
                     expand = c(0, 0)) +
  theme_bw() +
  #coord_fixed(ratio = 1/1) +
  stat_function(fun=expected.utility.arith, 
                args = list(b_SR_neg = b_SR_neg,
                            rel_b_SR_pos = rel_b_SR_pos,
                            rel_b_RR = rel_b_RR,
                            e = e),
                geom="line", color = viridis::viridis(3)[2]) + 
  stat_function(fun=expected.utility.geom, 
                args = list(b_SR_neg = b_SR_neg,
                            rel_b_SR_pos = rel_b_SR_pos,
                            rel_b_RR = rel_b_RR,
                            e = e),
                geom="line", color = viridis::viridis(3)[1])


df <- expand.grid(b_SR_neg = 0.00001,
                  rel_b_SR_pos = 1,
                  rel_b_RR = .5,
                  s = seq(0, 1, .01),
                  e = c(.2, .5, 1, 2, 5))

df$expected_u_arith <- expected.utility.arith(df$s, df$e, 
                                              df$b_SR_neg, 
                                              df$rel_b_SR_pos,
                                              df$rel_b_RR)
df$expected_u_geom <- expected.utility.geom(df$s, df$e, 
                                            df$b_SR_neg, 
                                            df$rel_b_SR_pos,
                                            df$rel_b_RR)

ggplot(df, aes(s, expected_u_arith, colour = factor(e))) +
  scale_x_continuous(name="submission strategy", limits=c(0, 1),
                     expand = c(0,0)) +
  scale_y_continuous(name="expected utility", 
                     limits=c(0, 1),
                     expand = c(0,0)) +
  scale_color_manual(values = becolours5) +
  theme_bw() +
  geom_path() +
  geom_path(aes(y = expected_u_geom), linetype = "dotted")