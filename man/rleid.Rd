% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rleid}
\alias{rleid}
\title{Generate run-length type group id}
\usage{
rleid(x)
}
\arguments{
\item{x}{a vector}
}
\value{
An integer vector having the same length as \code{x}.
}
\description{
For a vector, \code{x}, \code{rleid} creates a unique group variable for
sequential idenitcal elements of \code{x}.
}
\examples{
x <- LETTERS[c(1,1,2,1,1,2,3,3)]
data.frame(id = x, rleid = rleid(x))

}
\seealso{
\code{\link{rle}}; \code{data.table::rleid}
}
