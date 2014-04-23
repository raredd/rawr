\name{roundr}
\alias{roundr}
\title{Round a number to the specified number of decimal places (default is 1),
preserving trailing 0s}
\usage{
roundr(..., digits  = 1)
}
\arguments{
  \item{x}{numeric value or vector}

  \item{digits}{number of digits past the decimal point to
  keep}
}
\value{
A vector of character strings.
}
\description{
Round a number to the specified number of decimal places
(default is 1), preserving trailing 0s
}
\details{
Uses \code{\link[base]{sprintf}} to round numeric value,
retaining extra 0s.
}
\examples{
roundr(51.01, 3)
roundr(0.199, 2)

# useful for dropping the negative in case 1:
roundr(c(-0.0002, 0.0002, 0.5, -0.5, -0.002), digits = 3)
}
\seealso{
\code{\link[base]{round}}; \code{\link[base]{sprintf}}
}

