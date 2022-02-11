phatfunc <- function(m = 10000, n, p) {
  trialvals <- sample(c(0, 1), m*n, replace = TRUE, prob = c(1-p, p))
  trialmat <- matrix(trialvals, ncol = n)
  
  NumSuccesses <- apply(trialmat, 1, sum)
  Phats <- NumSuccesses/n
  ans <- c(mean(Phats), sd(Phats))
  names(ans) <- c("MeanPhat", "StdDevPhat")
  hist(Phats, breaks = seq(from = min(Phats), to = max(Phats), by = 1/n), 
       col = "lightblue", freq = FALSE, xlab = expression(hat(p)), ylab = "", main = "Histogram of Simulated p-hats")
  curve(dnorm(x, p, sqrt(p*(1-p)/n)), add = TRUE, col = "blue", lwd = 2)
  return(ans)
}
phatfunc(n = 81, p = 0.6)
