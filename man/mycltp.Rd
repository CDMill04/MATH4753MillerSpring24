% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mycltp.R
\name{mycltp}
\alias{mycltp}
\title{Title Poisson Distribution Function for Central Limit Theroem}
\usage{
mycltp(n, iter, lambda = 10, ...)
}
\arguments{
\item{n}{Sample size}

\item{iter}{Number of iterations}

\item{lambda}{The lambda of the distribution}

\item{...}{Extra parameters if needed}
}
\value{
Three plots of the distribution: A histogram, a barplot, and a Probability Function plot
}
\description{
Title Poisson Distribution Function for Central Limit Theroem
}
\examples{
mycltp(n = 2, iter = 10000, lambda = 4)
}
