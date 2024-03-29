% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils2.R
\name{countr}
\alias{countr}
\title{Count formatter}
\usage{
countr(
  x,
  n,
  lowcase = NA,
  frac = FALSE,
  digits = 0L,
  which = seq_along(x),
  conf = NULL,
  show_conf = TRUE
)
}
\arguments{
\item{x}{named vector of counts (a summary or table) or a long vector
of character strings or factors}

\item{n}{total number of observations; if not given, the length of
\code{x} is used}

\item{lowcase}{logical; names will be lowercase if \code{TRUE}, upper
case if \code{FALSE}, and unchanged for any other value}

\item{frac}{logical; if \code{TRUE}, counts are shown as fractions of
\code{n}}

\item{digits}{number of digits past the decimal point to keep}

\item{which}{optional integer or character vector to select or re-order
the output; note that this does not change the counts or percentages}

\item{conf, show_conf}{optional arguments controlling binomial confidence
intervals, passed to \code{\link{binconr}}}
}
\description{
Formats and prints a \emph{named} vector of counts with percents.
}
\examples{
x <- setNames(3:1, c('Gold', 'Silver', 'Bronze'))
countr(x)
countr(x, conf = 0.95)
countr(x, n = 10, frac = TRUE)
countr(x, n = 10, frac = TRUE, which = 2)

## using a character/factor vector, not counts
countr(names(x))
countr(names(x), which = 1)
countr(names(x), which = c(3, 1), frac = TRUE)
countr(names(x), which = 'Silver', lowcase = TRUE)

}
