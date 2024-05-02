#' BootStrap Function
#'
#' @param iter Number of Iterations
#' @param x The given random sample
#' @param fun The mean
#' @param alpha The % Confidence Intervals
#' @param cx Text Label Size
#' @param ... Extra Parameters
#'
#' @return A Histogram
#' @export
#'
#' @examples
#' \dontrun{
#' sam <- rnorm(20, mean = 10, sd = 4)
#' myboot(iter = 10000, x = sam, fun = "mean", alpha = 0.05, cx = 1.5)
#' }
myboot<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){
  n=length(x)

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2))

  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x, xstat = xstat))
}
#' @importFrom stats quantile
#' @importFrom graphics hist abline segments text
