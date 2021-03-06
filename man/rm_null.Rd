% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zxx.R
\name{rm_null}
\alias{rm_null}
\title{Recursive \code{rm} for lists}
\usage{
rm_null(x, rm_list = TRUE)
}
\arguments{
\item{x}{a list}

\item{rm_list}{logical; if \code{FALSE}, lists with only the \code{NULL}
object will not be removed}
}
\description{
Remove \code{NULL} or \code{list(NULL)} objects recursively from a list.
}
\examples{
l <- list(list(NULL), list(1), list('a', NULL))

l
rm_null(l)
rm_null(l, FALSE)

}
\references{
\url{https://stackoverflow.com/q/26539441/2994949}
}
