#' Quadratic Plot
#'
#' @param x Quadratic line equation
#' @param y Beta Values
#'
#' @return A quadratic plot
#' @export
#'
#' @examples
#' myplot(x = myQuadraticLineModel, y = 2)
myplot = function(x, y) {
  x$coef[1] + x$coef[2] * y + x$coef[3] * y ^ 2
}
