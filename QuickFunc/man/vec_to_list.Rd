% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vec_to_list.r
\name{vec_to_list}
\alias{vec_to_list}
\title{Convert a vector of elements into a list of vectors of prescribed lengths.}
\usage{
vec_to_list(z, lop = rep(1, length(z)), names = NA)
}
\arguments{
\item{z}{a vector.}

\item{lop}{a vector indicating how many elements of z should go into each vector in the list.}

\item{names}{optional names parameter for the elements of the constructed list.  If specified, it must be the same length as lop.}
}
\value{
a list of vectors, each taking several elements of z as presribed by the elements of lop.
}
\description{
Convert a vector of elements into a list of vectors of prescribed lengths.
}
\examples{
z <- 1:10
vec_to_list( z )
vec_to_list( z, lop = c(2,3,5) )
vec_to_list( z, lop = c(2,3,5), names = c("dog", "cat", "rabbit") )
}
