##============================================================================##
## Model setup: create dataframe

# Create an data frame w/ 1 column for researcher ID and one for the
# submission threshold of generation 0

df_loop <- data.frame("researcher_ID" = researcher_ID,
                      "submission_threshold" = runif(length(researcher_ID), 0,1))

df_loop <- as.data.frame(matrix(nrow = length(researcher_ID), ncol = 50))
colnames(df_loop) <- c(1:50)

researchlist <- list()
researchoutput <- list()
model <- list()
# model$generation <- list()
#------------------------------------------------------------------------------#
for (k in 1:50) {
  submission_threshold <- runif(length(researcher_ID), 0,1)
  
  for (i in 1:generations) {
    
    # initialise the payoff vector: before conducting any research, 
    # everybody starts with 0 payoff
    payoff <- rep(0, length(researcher_ID))
    
    ## 1. Research phase:
    for (j in 1:generation_duration) {
      
      # 1.1 We use research.fun to "do the research":
      #     Assign priors, compare with submission thresholds, decide who submits
      #     RRs and who doesn't, calculate results, and calculate payoffs
      research <- research.fun(prior.dist = prior.dist,
                               prior.dist.m = prior.dist.m,
                               prior.dist.sd = prior.dist.sd,
                               submission_threshold = submission_threshold,
                               reward_RR = reward_RR,
                               reward_normal_pos = reward_normal_pos,
                               reward_normal_neg = reward_normal_neg,
                               cost_RR = cost_RR,
                               cost_normal = cost_normal)
      
      # Check:  Stop the loop if something went wrong with submission decisions
      stopifnot(sum(research$prior < research$submission_threshold) == 
                  sum(research$submit_as_RR))
      # Check:  Payoffs should be the same for all RR authors, hence sd = 0.
      #         Stop the loop if this is false:
      #         (Caution: this check fails when less than 2 researchers
      #         submitted RRs.)
      # stopifnot(sd(research$new_payoff[which(research$submit_as_RR == 1)]) == 0)
      
      # 1.2   Update payoff: 
      #       add the payoffs of this round to the previous amount
      payoff <- payoff + research$new_payoff
      
      research$researcher_ID <- researcher_ID
      research$total_payoff <- payoff # add total payoff to the "research" list
      researchlist[[j]] <- research # store data from this research cycle in the output list
    }
    
    ## 2. Evolution phase:
    
    # 2.1 We use utility.fun to calculate researchers' fitness from their payoffs
    utility <- utility.fun(x = payoff, e = e, a = a, 
                           survival_threshold = survival_threshold,
                           shape = utility_shape)
    
    # 2.2 Selection: 
    #     We use selection.fun to calculate whose traits are passed on to the
    #     next generation
    selection <- selection.fun(n = length(researcher_ID), utility = utility,
                               submission_threshold_parent = submission_threshold)
    
    # Check: Stop the loop if any of the selected submission threshold is NA
    stopifnot(all(!is.na(selection$submission_threshold_selected)))
    
    # 2.3 Mutation: 
    #     We use mutation.fun to generate the evolved submission thresholds
    #     of the new generation
    evolution <- mutation.fun(n = length(researcher_ID),
                              submission_threshold_selected = 
                                selection$submission_threshold_selected,
                              mutation_sd = mutation_sd)
    # researchoutput[[i]]$
    # Put the new generations' evolved submission thresholds into the data frame:
    submission_threshold <- evolution$submission_threshold_child
    
    selection$submission_threshold_child <- evolution$submission_threshold_child
    selection$researcher_ID <- researcher_ID
    model[[i]] <- selection
    researchoutput[[i]] <- data.table::rbindlist(researchlist)
    model$generation[[i]]$evolution <- selection
  }
  df_loop[,k] <- submission_threshold
}


##============================================================================##
## Prepare the plot:
# Turn the data frame into long format with numbers 0-100 for generation 
# and one variable for evolving submission thresholds
df_plot <- melt(df_loop,
                variable.name = "run",
                value.name = "submission_threshold")
df_plot$run <- as.numeric(as.character(df_plot$run))
##----------------------------------------------------------------------------##
## Plot the submission thresholds
# Basic plot setup:
basic_plot <- ggplot(
  df_plot,
  aes(x = run,
      y = submission_threshold,
      # group = generation
  )) +
 # scale_x_continuous(breaks = seq(0, generations, generations / 10),
 #                    name = "generation") +
  scale_y_continuous(lim = c(0, 1),
                     breaks = seq(0, 1, .1),
                     name = "submission threshold") +
  theme_bw() +
  ggtitle(paste(generations, "generations", sep = " "),
          subtitle = paste(length(researcher_ID), " researchers, ",
                           generation_duration, 
                           ifelse(generation_duration ==1, 
                                  " round", " rounds"), "/generation, ",
                           "mutation size = ", mutation_sd, " SD", "\n",
                           "rewards: RR = ", reward_RR, 
                           ", normal pos. = ", reward_normal_pos,
                           ", normal neg. = ", reward_normal_neg, "\n",
                           "costs: RR = ", cost_RR, 
                           ", normal = ", cost_normal, "\n",
                           "prior distribution: ", 
                           ifelse(prior.dist == "uniform", 
                                  prior.dist, paste(prior.dist, ", m = ", 
                                                    prior.dist.m, ", sd = ",
                                                    prior.dist.sd, sep = "")),
                           "\n",
                           "fitness function: ", utility_shape, ", e = ", e, 
                           ", a = ", a, "; threshold = ", survival_threshold,
                           sep = ""))

# Output a plot with median (dashed blue line), mean (solid red-ish line),
# and 95% coverage (from quantile .025 to quantile .975, shaded light blue)
basic_plot +
  stat_summary(geom = "ribbon", 
               fun.data = median_hilow, fill = "lightskyblue1") +
  stat_summary(geom = "line", fun = median, 
               linetype="dashed", colour = "dodgerblue4")+
  # stat_summary(geom = "ribbon", fun.data = mean_cl_boot, fill = "salmon") +
  stat_summary(geom = "line", fun = mean, colour="salmon4")




mysummary <- function(x){
  funs <- c(mean,sd,median,min,max)
  lapply(funs,function(f) f(x,na.rm = T))
}

mysummary(df_loop)
summary(as.data.frame(summary(df_loop)))


