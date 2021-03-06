% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zxx.R
\name{all_equal2}
\alias{all_equal2}
\title{Test if two or more objects are (nearly) equal}
\usage{
all_equal2(
  ...,
  tolerance = .Machine$double.eps^0.5,
  scale = NULL,
  check.attributes = TRUE,
  use.names = TRUE,
  all.names = TRUE,
  check.names = TRUE
)
}
\arguments{
\item{...}{any \code{R} objects}

\item{tolerance}{numeric >= 0; differences smaller than \code{tolerance}
are not reported (default value is close to 1.5e-8)}

\item{scale}{numeric scalar > 0 (or \code{NULL}), see details in
\code{\link{all.equal}}}

\item{check.attributes}{logical indicating if the \code{\link{attributes}}
should be compared}

\item{use.names}{logical indicating if \code{\link{list}} comparison should
report differing components by name (if matching) instead of integer index}

\item{all.names}{logical passed to \code{\link{ls}} indicating if "hidden"
objects should also be considered in the environments}

\item{check.names}{logical indicating if the \code{\link{names}}\code{(.)}
should be compared}
}
\value{
If all \code{...} are nearly equal, \code{TRUE} otherwise returns a list
with the objects that failed.
}
\description{
A generalization of \code{\link{all.equal}} that allows more than two
objects to be tested for near-equality.
}
\examples{
all_equal2(pi, 355/113, 22/7)

all_equal2(pi, 355/113, 22/7, tolerance = 0.01)

all_equal2(cars[1], cars[, 1, drop = FALSE], cars[, -2, drop = TRUE])

}
\seealso{
\code{\link{all.equal}}; \code{\link{identical2}}; \code{\link{identical}}
}
