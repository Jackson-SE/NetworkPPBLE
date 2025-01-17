% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arg_min_eval_count.r
\name{arg_min_eval_count}
\alias{arg_min_eval_count}
\title{Number of function evaluations.}
\usage{
arg_min_eval_count(p)
}
\arguments{
\item{p}{precision}
}
\value{
number of required evaluations
}
\description{
Related to function \code{\link{arg_min}}, this function simply calculates the number of required evaluations of \code{f} to ensure the chosen precision \code{p}, where \code{p} in this case is taken to be a proportion of \code{u-l}.
}
\examples{
arg_min_eval_count( 0.0001 )
}
