\name{ggcols}
\alias{ggcols}
\title{ggplot colours}
\usage{
ggcols(n, c = 100, l = 65)
}
\arguments{
  \item{n}{number of colours}

  \item{c}{the chroma of the colour; the upper bound for
  chroma depends on hue and luminance}

  \item{l}{a value in the range \code{[0, 100]} giving the
  luminance of the colour; for a given combination of hue
  and chroma, only a subset of this range is possible}
}
\description{
A function to replicate default \code{\link{ggplot}}
colours
}
\examples{
plot(rnorm(1000), col = ggcols(1000), pch = 19)
}
\seealso{
\code{\link{hcl}}
}

