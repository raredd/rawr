% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surv.R
\name{points.kmplot}
\alias{points.kmplot}
\title{\code{kmplot} points}
\usage{
\method{points}{kmplot}(
  x,
  xscale,
  xmax,
  fun,
  col = par("col"),
  pch = par("pch"),
  censor = TRUE,
  event = FALSE,
  bump = FALSE,
  plot = TRUE,
  ...
)
}
\arguments{
\item{x, xscale, xmax, fun, col, pch}{see \code{\link{points.survfit}}}

\item{censor, event}{logical; \code{TRUE} will draw points at censored
and event times, respectively}

\item{bump}{logical; \code{TRUE} will draw a bump mark rather than pch}

\item{plot}{logical; \code{FALSE} will not plot but return plotting data}

\item{...}{additional graphical parameters passed to \code{\link{par}}}
}
\description{
\code{survival:::points.survfit} with additional features.
}
