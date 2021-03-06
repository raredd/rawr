% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{xtable}
\alias{xtable}
\title{Cross table}
\usage{
xtable(x, by, digits = 0L, total = TRUE, pct.sign = FALSE, test = TRUE, ...)
}
\arguments{
\item{x, by}{row and column variables, respectively; should be factor-like
and will be coerced; missing values in \code{by} will be removed with the
corresponding values in \code{x}}

\item{digits}{for percentages, number of digits past the decimal to keep}

\item{total}{logical; if \code{TRUE}, the row totals will be added as a
separate column}

\item{pct.sign}{logical; if \code{FALSE} (default), no percentage signs
will be shown}

\item{test}{logical; if \code{TRUE}, a test and p-value will be added as
  a separate column; see \code{rawr:::guess_test}

  alternatively, a user-defined test function which takes two arguments,
  \code{x} and \code{by}, and returns a numeric p-value with an optional
  attribute, \code{"name"}, which will be used as a test label; see
  examples}

\item{...}{additional arguments passed to
\code{\link[Gmisc]{getDescriptionStatsBy}}}
}
\description{
Create a contingency table with totals, percentages, and statistical test.
}
\examples{
x <- mtcars$gear
y <- mtcars$cyl
z <- mtcars$vs

table(x, y)
xtable(x, y)

xtable(ordered(x), z)
xtable(ordered(x), y)
xtable(ordered(x), ordered(y))


## user-defined test function
test <- function(x, by) {
  structure(chisq.test(table(x, by))$p.value, name = '<i>p-value</i>')
}
res <- xtable(x, y, test = test)
res

names(dimnames(res)) <- c('Gears', 'Cylinders')
htmlTable::htmlTable(
  res, n.cgroup = c(1, 3, 1), cgroup = c('', 'Cylinders', '')
)

}
\seealso{
\code{\link[Gmisc]{getDescriptionStatsBy}}; \code{\link{tabler_stat2}};
\code{rawr:::guess_test}
}
