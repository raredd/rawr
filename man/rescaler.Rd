\name{rescaler}
\alias{rescaler}
\title{Rescale numeric vector}
\usage{
rescaler(x, to = c(0, 1), from = range(x, na.rm = TRUE))
}
\arguments{
  \item{x}{numeric vector of values}

  \item{to}{output range (numeric vector of length two)}

  \item{from}{input range (numeric vector of length two);
  if not given, \code{from} is calculated from the range of
  \code{x}}
}
\description{
Rescale a numeric vector to have specified maximum and
minimum; shamelessly stolen from hadley's \code{scales}
package
}
\examples{
rescaler(1:100)
rescaler(runif(10))
rescaler(1)
}
\seealso{
\code{\link[scales]{rescale}};
\code{\link[scales]{zero_range}}
}

