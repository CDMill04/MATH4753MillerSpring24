#' Normal Lower-Tail (P(X<=a)) Probablility Distrubution Function
#'
#' @param mu The mean of the distribution
#' @param sigma The standard deviation of the distribution
#' @param a The probablility of the samples being below this value
#'
#' @return A graph of the distribution with the probablility of a shaded in, as well its value.
#' @export
#'
#' @examples
#' myncurve(5, 10, 3)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma), col = "orange2")
  xcurve = seq(mu - 3 * sigma, a, length = 1000)
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)
  polygon(c(mu - 3 * sigma, xcurve, a), c(0, ycurve, 0), col = "blue")

  prob = pnorm(q = a, mean = mu, sd = sigma)
  Area = round(prob, 4)
  list(mu = mu, sigma = sigma, Area = Area)
}
