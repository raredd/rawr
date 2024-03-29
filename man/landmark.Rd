% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surv.R
\name{landmark}
\alias{landmark}
\title{Landmark}
\usage{
landmark(
  object,
  times = NULL,
  col = 2L,
  plot = TRUE,
  plot.main = plot,
  lr_test = TRUE,
  adjust_start = FALSE,
  single = FALSE,
  ...
)
}
\arguments{
\item{object}{an object of class \code{\link[survival]{survfit}}}

\item{times}{a vector of landmark times}

\item{col}{color for \code{times} annotations; use \code{col = 0} to
suppress labels}

\item{plot, plot.main}{logicals; if \code{TRUE}, landmark and \code{s} are
plotted, respectively}

\item{lr_test}{logical or numeric; if \code{TRUE}, a log-rank test will be
performed and the results added to the top-right corner of the plot; if
numeric, the value is passed as \code{rho} controlling the type of test
performed; see \code{\link{survdiff}}}

\item{adjust_start}{logical; if \code{TRUE}, each landmark plot will begin
at the y-axis}

\item{single}{logical; if \code{TRUE}, plots drawn on a single frame}

\item{...}{additional arguments passed to \code{\link{kmplot}}}
}
\value{
A list with the following elements:

\item{\code{$table}}{data frame with the sample size, chi-square
  statistic, degrees of freedom, and p-value for the test (the type of test
  can be controlled by using a numeric value for \code{lr_test}, passed as
  \code{rho} to \code{\link[survival]{survdiff}})}
\item{\code{$survfit}}{a list with each \code{\link[survival]{survfit}}
  object}
}
\description{
Fit survival curves for landmark time points.
}
\examples{
library('survival')
s <- survfit(Surv(time, status) ~ rx, colon)
l <- landmark(s, times = c(500, 1000, 2000), single = TRUE)
l$table

layout(matrix(c(1, 1, 2, 3), 2))
landmark(s, times = c(500, 2000), adjust_start = TRUE,
         hr_text = TRUE, col.surv = 4:6)

}
