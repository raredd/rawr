% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_extra.R
\name{bp.test}
\alias{bp.test}
\alias{bp.test.formula}
\alias{bp.test.default}
\title{Box plot tests}
\usage{
bp.test(x, ...)

\method{bp.test}{formula}(
  formula,
  data,
  which = NULL,
  at = NULL,
  line = NULL,
  test = wilcox.test,
  ...
)

\method{bp.test}{default}(x, which = NULL, at = NULL, line = NULL, test = wilcox.test, ...)
}
\arguments{
\item{x}{a vector of text to be drawn above each pair of groups}

\item{...}{arguments passed to or from other methods or additional
graphical parameters passed to \code{link{par}}}

\item{formula}{a formula of the form \code{response ~ group} where
\code{response} is a numeric variable and \code{group} is a factor-like
variable with three or more unique values (groups)}

\item{data}{an optional matrix or data frame (or similar: see
\code{\link{model.frame}}) containing the variables in \code{formula}; by
default, the variables are taken from \code{environment(formula)}}

\item{which}{an index vector of the values to display}

\item{at}{numeric vector giving the x-axis locations of each group having
the same length as \code{x} or the number of pairs; alternatively, a list
of pairs of x-axis locations for each \code{x}; see examples}

\item{line}{lines at which to plot \code{test} results; if \code{NULL},
these will be calculated; if length 1, the calculated lines will be shifted
by \code{line}}

\item{test}{the test to use for pairwise comparisons}
}
\value{
A list of user coordinates where each \code{x} is drawn.
}
\description{
Add pairwise tests to box plots with three or more groups. Using
\code{\link{cuzick.test}} under-the-hood, performs \code{test} for all
pairs of groups and draws results on an existing plot.
}
\examples{
boxplot(mpg ~ gear, mtcars)
bp.test(mpg ~ gear, mtcars) -> x

boxplot(mpg ~ gear, mtcars)
bp.test(x$text[c(1, 3)], at = list(1:2, 2:3))
bp.test(x$text[c(1, 3)], at = list(1:2, 2:3), line = 0:1)


boxplot(mpg ~ gear, mtcars, at = c(1, 3, 4), ylim = c(10, 55))
bp.test(mpg ~ gear, mtcars, at = c(1, 3, 4), which = c(1, 3, 2), line = -5)


op <- par(mar = par('mar') + c(0, 0, 3, 0))
tplot(mpg ~ interaction(vs, am), mtcars, show.n = FALSE)
bp.test(mpg ~ interaction(vs, am), mtcars)

tplot(mpg ~ interaction(vs, am), mtcars, show.n = FALSE)
bp.test(mpg ~ interaction(vs, am), mtcars, which = c(1, 3, 5))

bp.test(mpg ~ interaction(vs, am), mtcars, which = 6, line = 4,
        col = 'red', fg = 'red', lty = 2, font = 2, test = t.test) -> at
points(at[1], at[2], pch = 1, cex = 5, col = 'red', xpd = NA)
par(op)


## also works for barplots
bp <- barplot(with(mtcars, tapply(mpg, gear, mean)), ylim = c(0, 30))
bp.test(mpg ~ gear, mtcars, at = bp, line = -1, test = t.test)


## use default method for more control
boxplot(mpg ~ am + vs, mtcars, axes = FALSE, ylim = c(10, 55))
axis(2, las = 1)
box(bty = 'l')
at <- bp.test(letters[1:6], at = 1:4, line = -7)
points(at, cex = 3, col = 1:6, xpd = NA)


## special cases
sp <- split(mtcars$mpg, interaction(mtcars$cyl, mtcars$am))
pv <- sapply(sp[-1], function(x) pvalr(t.test(sp[[1]], x)$p.value))
op <- par(mar = c(5, 5, 8, 2))
tplot(sp, show.n = FALSE)
bp.test(pv, 1:6)

pairs <- list(1:2, 3:4, 5:6)
pv <- sapply(pairs, function(ii)
  pvalr(t.test(sp[[ii[1]]], sp[[ii[2]]])$p.value))
tplot(sp, show.n = FALSE)
bp.test(pv, at = pairs)
par(op)

}
\seealso{
\code{\link{cuzick.test}}; \code{\link{boxplot}}; \code{\link{tplot}};
\code{rawr:::coords}
}