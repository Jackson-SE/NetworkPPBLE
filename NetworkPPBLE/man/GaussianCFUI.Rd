% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GaussianCFUI.R
\name{GaussianCFUI}
\alias{GaussianCFUI}
\title{Gaussian Correlation Function with Uncertain Inputs}
\usage{
GaussianCFUI(EX, EY = EX, VarX = 0, VarY = 0, CovXY = 0, theta, delta = 0)
}
\arguments{
\item{EX}{a vector, matrix or dataframe of the expected values of X}

\item{EY}{a vector, matrix or dataframe of the expected values of Y}

\item{VarX}{a vector, matrix or dataframe of the variances of X}

\item{VarY}{a vector, matrix or dataframe of the variances of Y}

\item{CovXY}{a matrix or array of the covariances between the points X and Y.}

\item{theta}{a vector of correlation length parameter values (one for each column of EX).}

\item{delta}{an (optional) scalar nugget parameter.}
}
\value{
Gaussian correlation function value between X and Y,
given as a matrix of dimension \code{nrow(EX)} by \code{nrow(EY)}.
}
\description{
Gaussian Correlation Function with Uncertain Inputs
}
\examples{
EX <- matrix( rnorm( 10 ), ncol = 2 )
EY <- matrix( runif( 6 ), ncol = 2 )
VarX <- matrix ( rep( 0.01, 10 ), ncol = 2 )
VarY <- matrix ( rep( 0.01, 6 ), ncol = 2 )
theta <- c( 0.5, 0.8 )
GaussianCFUI( EX, EY,  VarX, VarY, theta = theta )
GaussianCFUI( as.data.frame( EX ), EY, VarX, VarY, theta = theta )
}
