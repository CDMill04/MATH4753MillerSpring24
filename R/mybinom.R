#' Binomial Function
#'
#' @param iter number of iterations
#' @param n sample size
#' @param p probability
#'
#' @return A binomial and its barplot
#' @export
#'
#' @examples
#' mybin(iter = 500, n = 10, p = 0.7)
mybin = function(iter = 100, n = 10, p = 0.7) {
  sampMatrix = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  ## Num of success
  succ = c()

  # For loop
  for (i in 1:iter) {
    sampMatrix[, i]=sample(c(1 ,0),n , replace = TRUE, prob=c(p, 1-p))
    succ[i]=sum(sampMatrix[,i])
  }

  #Table for success, then barplot
  succ.tab=table(factor(succ,levels=0:n))

  iter.lab = paste0("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("n = ", n)
  lab = paste(iter.lab, n.lab, p.lab, sep = ", ")

  barplot(succ.tab/(iter), col = rainbow(n+1), main = "Binomial simulation", xlab = "Number of successes")
  succ.tab/iter
}
