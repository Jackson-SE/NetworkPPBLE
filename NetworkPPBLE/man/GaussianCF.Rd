% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GaussianCF.R
\name{GaussianCF}
\alias{GaussianCF}
\title{Gaussian Correlation Function}
\usage{
GaussianCF(X, Y = X, theta, delta = 0)
}
\arguments{
\item{X}{a vector, matrix or dataframe}

\item{Y}{a vector, matrix or dataframe}

\item{theta}{a vector of correlation length parameter values (one for each column of X).}

\item{delta}{an (optional) scalar nugget parameter.}
}
\value{
Gaussian correlation function value between the rows of X and Y,
given as a matrix of dimension \code{nrow(X)} by \code{nrow(Y)}.
}
\description{
Calculate the Gaussian correlation function between the points in (given by the rows of) two matrices.
}
\examples{
X <- matrix( rnorm( 10 ), ncol = 2 )
Y <- matrix( runif( 6 ), ncol = 2 )
theta <- c( 0.5, 0.8 )
GaussianCF( X, Y, theta )
GaussianCF( as.data.frame(X), Y, theta )
}
