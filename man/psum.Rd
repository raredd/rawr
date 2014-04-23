\name{psum}
\alias{psum}
\title{Pairwise sum}
\usage{
psum(..., na.rm = FALSE)
}
\arguments{
  \item{...}{string of vectors}

  \item{na.rm}{logical; should missing values (including
  \code{NaN}) be omitted from calculations}
}
\value{
A single vector of element-wise sums.
}
\description{
Compute the pairwise sum of two or more vectors
}
\details{
Each vector passed in \code{...} must be equal in length.
The function coerces the vectors into a matrix and performs
\code{\link{rowSums}} on the resulting rows.
}
\examples{
x <- c(-1, NA, 4, 5)
y <- c(NA, NA, 6, -1)
psum(x, y)
psum(x, y, na.rm = TRUE)
}
\seealso{
\code{\link{pmin}}, \code{\link{pmax}}
}

