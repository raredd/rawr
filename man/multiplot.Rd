\name{multiplot}
\alias{multiplot}
\title{Draw multiple plots in a single layout}
\usage{
multiplot(width = 8.5, height = 11, rows, cols, matrix = c())
}
\arguments{
  \item{width,height}{the width and height of the plotting
  window in inches; if \code{NA}, taken from the resources
  and if not specified there, defaults to \code{7} inches;
  see \code{\link{x11}} and "Resources"}

  \item{rows,cols}{number of rows and columns in
  \code{matrix}}

  \item{matrix}{a matrix object specifying the location of
  the next \code{n} figures on the output device; each
  value in the matrix must be \code{0} or a positive
  integer; if \code{n} is the largest positive integer in
  the matrix, then the integers \code{{1, ..., n-1}} must
  also appear at least once in the matrix; see
  \code{\link{layout}}}
}
\description{
Function for plotting multiple base \code{R} graphics in a
single layout
}
\examples{
library(descr)
library(psych)

multiplot(18, 8, 3, 3, c(1,1,1, 1,1,1, 2,3,4))
with(mtcars, histkdnc(mpg, breaks = 15, main = 'MPG'))
with(mtcars, histkdnc(cyl, breaks = 15, main = "Cyl"))
with(mtcars, histkdnc(hp, breaks = 15, main = 'HP'))
with(mtcars, histkdnc(drat, breaks = 15, main = 'Drat'))


multiplot(18, 10, 3, 4, c(1,1,1, 1,1,1, 2,2, 3, 2,2, 3))
with(mtcars, histkdnc(mpg, breaks = 15))
with(mtcars, boxplot(mpg ~ cyl, notch = FALSE, ylab = 'MPG',
  border = 'blue', xlab = 'Cyl', main = 'MPG boxplot with error bars'))
with(mtcars, plot.design(mpg ~ (as.factor(cyl)), fun = mean))
}

