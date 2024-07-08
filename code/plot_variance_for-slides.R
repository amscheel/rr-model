library(ggplot2)

text_size <- 26 # use 26 for presentation slides and 14 otherwise (paper)
plot_size <- 15 # use 15 for presentation slides and 12 otherwise (paper)
line_thickness <- 1.2 # use 1.2 for presentation slides, 1 otherwise (paper)

variance_plot <- ggplot(data.frame(x=c(0,1)), aes(x)) + 
  scale_x_continuous(name="payoff", limits = c(0, 1),
                     breaks = c(.33, .67),
                     labels = c("low threshold", "high threshold"),
                     expand = c(0, 0)) +
  scale_y_continuous(name="probability", breaks = c(), 
                     labels = c(), expand = c(0, 0)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = text_size)) +
  coord_fixed(ratio = 1/12)+ 
  annotate("segment", x = .33, xend = .33, y =  0, yend = 7, 
           linetype = "dashed", colour = "darkgrey",
           linewidth = line_thickness) +
  annotate("segment", x = .67, xend = .67, y =  0, yend = 7, 
           linetype = "dotted", colour = "darkgrey",
           linewidth = line_thickness) +
  stat_function(fun=dnorm, 
                args = list(mean = .5, sd = .1), geom="line",
                linewidth = line_thickness) + 
  stat_function(fun=dnorm, 
                args = list(mean = .5, sd = .04), geom="line",
                linewidth = line_thickness)



ggsave("plot_variance_for-slides.png", variance_plot, bg = "white",
       width = plot_size+12, height = plot_size, units = "cm")
