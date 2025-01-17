% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Scale.R
\name{Scale}
\alias{Scale}
\title{Scale points}
\usage{
Scale(x, a = 0, b = 1, l = -1, u = 1)
}
\arguments{
\item{x}{vector, matrix or dataframe of points (given by the elements or rows respectively) of points to scale.}

\item{a}{vector of lower limits of the original space, one for each dimension (column of x)
If all the lower limits are the same, that scalar value can be given.}

\item{b}{vector of upper limits of the original space, one for each dimension (column of x)
If all the upper limits are the same, that scalar value can be given.}

\item{l}{vector of lower limits of the transformed space, one for each dimension (column of x)
If all the lower limits are the same, that scalar value can be given.}

\item{u}{vector of upper limits of the transformed space, one for each dimension (column of x)
If all the upper limits are the same, that scalar value can be given.}
}
\value{
a vector or matrix of the transformed points.
}
\description{
Scale point from one hypercuboid domain space to another.
}
\details{
Scales the vector of matrix of points \code{x} from the hypercuboid [a,b] to [l,u].
}
\examples{
X <- matrix( runif(15, 2, 4), ncol = 3 )
Scale( X, a = 2, b = 4 )
# Compare with:
X - 3
}
