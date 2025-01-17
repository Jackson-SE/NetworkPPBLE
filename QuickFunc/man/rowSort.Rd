% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rowSort.R
\name{rowSort}
\alias{rowSort}
\title{Sort Each Row of Matrix}
\usage{
rowSort(X, decreasing = FALSE, na.last = TRUE)
}
\arguments{
\item{X}{a matrix.}

\item{decreasing}{logical (defaulting to \code{FALSE}).  Should the sort order be increasing or decreasing?}

\item{na.last}{for controlling the treatment of \code{NA}s.
    If \code{TRUE}, missing values in the data are put last; if
    \code{FALSE}, they are put first; if \code{NA}, they are removed
    (see \sQuote{Note}.)}
}
\value{
a matrix with each row containing the sorted elements of the rows in X.
}
\description{
Sort Each Row of Matrix
}
\examples{
X <- matrix( sample( 50, 20 ), ncol = 4 )
rowSort( X )
rowSort( X, decreasing = TRUE )
}
\seealso{
\code{\link[base]{order}}
}
