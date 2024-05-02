#' Solving an Overbooking Problem
#'
#' @param N Number of seats on the plane
#' @param gamma Probability that a plane will be overbooked
#' @param p Probability that a passenger will show
#' @param ... Extra parameters if needed
#'
#' @return Two graphs that use discrete and continuous distributions, respectively.
#' @export
#'
#' @examples
#' myntickets(N = 400, gamma = 0.02, p = 0.95)
myntickets <- function(N = 200, gamma = 0.02, p = 0.95,...) {
  #Least amount of tickets to sell to avoid disaster using discrete
  numN = as.numeric(N)
  V = seq(from = numN, to = 40 + numN, by = 1)
  fn = 1 - gamma - pbinom(N,V,p)
  nBinomAnswer = V[which(fn > 0)[1]]

  #Continuous now
  V2 = seq(from = numN, to = numN + 40, by = 1)
  fn2 = 1 - gamma - pnorm(N + 0.5, V2 * p, sqrt(V2 * p * (1 - p)))
  nNormAnswer = V2[which(fn2 > 0)[1]]

  #Plotting
  title = paste0("Finding the most optimal number of tickets to sell, n\n", nBinomAnswer, " with gamma = ", gamma, " and N = ", N, ", discrete", sep = "")
  plot(V, fn, main = title, xlab = "n", ylab = "Target")
  abline(h = min(fn[fn > 0]), v = nBinomAnswer, col = "Green")

  title2 = paste0("Finding the most optimal number of tickets to sell, n\n", nNormAnswer, " with gamma = ", gamma, " and N = ", N, ", continuous", sep = "")
  plot(V2, fn2, main = title2, xlab = "n", ylab = "Target")
  lines(V2, fn2, col = "purple")
  abline(h = min(abs(fn2)), v = nNormAnswer, col = "Red")


  #List outputs
  list(nd = nBinomAnswer, nc = nNormAnswer, N = N, gamma = gamma, p = p)
}
