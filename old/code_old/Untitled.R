# Goal: build a function that outputs the distribution of payoffs for a given 
# submission threshold s in a given scenario (combination of epsilon, m, & b_RR)

payoff.dist.fun <- function(m = 1, s, b.SR.neg = 0, b.SR.pos = 1, b.RR, 
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
  
 
    uniquepayoff <- data.table::as.data.table(combinationmat)[, sum(prob), by = payoff]
uniquepayoff
}

x <- payoff.dist.fun(s = 0.3, m = 80, b.RR = 0.1)

plot(x$payoff, x$V1)


