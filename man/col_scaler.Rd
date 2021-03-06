% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_extra.R
\name{col_scaler}
\alias{col_scaler}
\title{Color scaling}
\usage{
col_scaler(
  x,
  colors,
  na.color = NA,
  breaks = NULL,
  alpha = 1,
  alpha.min = min(0.1, x[x >= 0], na.rm = TRUE),
  to = c(0, 1),
  from = range(x, na.rm = TRUE)
)
}
\arguments{
\item{x}{a numeric or integer vector}

\item{colors}{a vector of color names as character strings (or
  hexadecimal strings) or integers corresponding to colors in the current
  \code{\link{palette}}; or a function taking an integer argument that
  returns a vector of colors (e.g., \code{\link{colorRampPalette}} or
  \code{\link{rainbow}})

  if only one color is given, the scaled value of \code{x} will determine
  the amount of transparency; default is from 0 (transparent) to 1 (opaque)}

\item{na.color}{color used for \code{NA} values of \code{x}}

\item{breaks}{(optional) numeric vector to center interpolation; if
\code{NULL} (default), \code{colors} are uniformly spread over a
continuous \code{x}; useful if \code{colors} should be centered at a
specific value of \code{x}; see examples}

\item{alpha}{transparency applied to interpolated colors (i.e., if
\code{colors} is not a single color)}

\item{alpha.min}{if a single color name is given, sets the lower bound of
alpha; a value greater than 0 ensures that the color is visible even for
the smallest value of \code{x} after rescaling}

\item{to, from}{output and input range, respectively; see
\code{\link{rescaler}}}
}
\value{
A character vector having the same length as \code{x} of hexadecimal color
values.
}
\description{
Color scaling and interpolation. For a numeric vector and a single color,
gradations of transparency is applied corresponding to each numeric value.
For two or more, color interpolation is applied.
}
\examples{
## basic usage
col_scaler(mtcars$mpg, 'red')
col_scaler(mtcars$vs, c('red', 'black'))


set.seed(1)
x <- sort(runif(50, 0, 2))
# x <- replace(x, runif(length(x)) > 0.75, NA)
p <- function(y, c) {
  points(seq_along(c), rep_len(y, length(c)),
         col = c, pch = 16, cex = 5, xpd = NA)
}

plot.new()
plot.window(c(0, 50), c(-3, 3))
p( 4, col_scaler(x, 'red'))
p( 3, col_scaler(x, c('red', 'blue')))
p( 2, col_scaler(x, c('red', 'blue'), to = c(.4, .8)))
p( 1, col_scaler(round(x), c('red', 'blue'), alpha = 0.5))
p( 0, col_scaler(x, 1:10))
p(-1, col_scaler(round(x), 1:3))
p(-2, col_scaler(x, 'heat.colors'))
p(-3, col_scaler(x, rainbow, alpha = 0.1))
p(-4, col_scaler(x, colorRampPalette(c('tomato', 'white', 'blue4'))))


set.seed(1)
x <- runif(1000)
y <- c('red', 'black', 'red')
op <- par(mfrow = c(2, 2), mar = c(3, 3, 1, 1))
plot(x, col = col_scaler(x, y), pch = 16)
plot(x, col = col_scaler(x, y, breaks = 0.5), pch = 16)
plot(x, col = col_scaler(x, y, breaks = 0.9), pch = 16)
plot(x, col = col_scaler(x, c(y, 'blue'), breaks = c(0.25, 0.75)), pch = 16)
par(op)

}
