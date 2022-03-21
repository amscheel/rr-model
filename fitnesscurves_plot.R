library(ggplot2)

fitness.curve <- function(x, e = 2, survival_threshold = 0){
  ifelse(x < survival_threshold, 0, x^e)
}

payoff_SR_neg <- .4
payoff_SR_pos <- payoff_SR_neg + .4
payoff_RR <- (payoff_SR_neg + payoff_SR_pos)/2

e_low <- 1/4
e_high <- 4

survival_threshold <- 0

colorbrewer3 <- c("#2166ac", "grey", "#b2182b")
colour_low <- "#2166ac"
colour_neutral <- "darkgrey"
colour_high <- "#b2182b"

basic_plot <- ggplot(data.frame(x=c(0,1)), aes(x)) + 
  scale_x_continuous(name="payoff", limits = c(0, 1),
                     breaks = c(payoff_SR_neg, payoff_RR, payoff_SR_pos),
                     labels = c(expression(italic(b)["-"]), 
                                expression(italic(b)[safe]), 
                                expression(italic(b)["+"])),
                     expand = c(0, 0)) +
  scale_y_continuous(name="fitness", limits = c(0, 1), 
                     breaks = c(fitness.curve(x = c(payoff_SR_neg,
                                                    payoff_RR,
                                                    payoff_SR_pos), 
                                              e = e_low), 
                                fitness.curve(x = c(payoff_SR_neg,
                                                    payoff_RR,
                                                    payoff_SR_pos), 
                                              e = e_high)),
                     labels = c(), expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 14)
        ) +
  coord_fixed(ratio = 1/1)

annotations <-  basic_plot + 
  annotate("segment", x = payoff_SR_neg, xend = payoff_SR_neg, 
           y =  fitness.curve(x = 0, e = e_low), 
           yend = fitness.curve(x = payoff_SR_neg, e = e_low), 
           linetype = "dashed", colour = "grey") +
  annotate("segment", x = payoff_RR, xend = payoff_RR, 
           y = fitness.curve(x = 0, e = e_low), 
           yend = fitness.curve(x = payoff_RR, e = e_low), 
           linetype = "dotted", colour = "grey") +
  annotate("segment", x = payoff_SR_pos, xend = payoff_SR_pos, 
           y = fitness.curve(x = 0, e = e_low), 
           yend = fitness.curve(x = payoff_SR_pos, e = e_low), 
           linetype = "dashed", colour = "grey") + 
  annotate("segment", x = 0, xend = payoff_SR_neg, 
           y =  fitness.curve(x = payoff_SR_neg, e = e_high), 
           yend = fitness.curve(x = payoff_SR_neg, e = e_high), 
           linetype = "dashed", colour = colour_high) +
  annotate("segment", x = 0, xend = payoff_RR, 
           y = fitness.curve(x = payoff_RR, e = e_high), 
           yend = fitness.curve(x = payoff_RR, e = e_high), 
           linetype = "dotted", colour = colour_high) +
  annotate("segment", x = 0, xend = payoff_SR_pos, 
           y = fitness.curve(x = payoff_SR_pos, e = e_high), 
           yend = fitness.curve(x = payoff_SR_pos, e = e_high), 
           linetype = "dashed", colour = colour_high) +
  annotate("segment", x = 0, xend = payoff_SR_neg, 
           y =  fitness.curve(x = payoff_SR_neg, e = e_low), 
           yend = fitness.curve(x = payoff_SR_neg, e = e_low), 
           linetype = "dashed", colour = colour_low) +
  annotate("segment", x = 0, xend = payoff_RR, 
           y = fitness.curve(x = payoff_RR, e = e_low), 
           yend = fitness.curve(x = payoff_RR, e = e_low), 
           linetype = "dotted", colour = colour_low) +
  annotate("segment", x = 0, xend = payoff_SR_pos, 
           y = fitness.curve(x = payoff_SR_pos, e = e_low), 
           yend = fitness.curve(x = payoff_SR_pos, e = e_low), 
           linetype = "dashed", colour = colour_low)

fitness_plot <- annotations +
  stat_function(fun=fitness.curve, 
                args = list(e = e_high,
                            survival_threshold = 0),
                geom="line", color = colour_high) + 
  stat_function(fun=fitness.curve, 
                args = list(e = 1,
                            survival_threshold = 0),
                geom="line", color = colour_neutral) +
  stat_function(fun=fitness.curve, 
                args = list(e = e_low,
                            survival_threshold = 0),
                geom="line", color = colour_low) 