\name{kmplot}
\alias{kmplot}
\title{Survival curves in base graphics}
\usage{
kmplot(s,
       # basic plot options
       lty.surv = 1, lwd.surv = 1, col.surv = 1,
       mark = 3,

       # confidence options
       lty.ci = 0, lwd.ci = 1, col.ci = col.surv,
       col.band = col.surv,

       # at risk table options
       atrisk = TRUE, atrisk.lab = 'Number at risk',
       atrisk.lines = TRUE, strata.lab = NULL,
       strata.order = seq(length(s$n)),
       extra.margin = 5,

       # aesthetics
       xlim = c(0, max(s$time)), ylim = c(0, 1),
       xaxis.at = pretty(s$time), xaxis.lab = xaxis.at,
       yaxis.at = pretty(ylim), yaxis.lab = yaxis.at,
       xlab = 'Time', ylab = 'Survival probability',
       main = '', cex.axis = 1,
       legend = !is.null(s$strata), legend.pos = 'bottomleft',

       # other options
       grid = TRUE, lty.grid = 1, lwd.grid = 1,
       col.grid = grey(.9),
       dev = TRUE, add = FALSE, ...)
}
\arguments{
  \item{s}{object of class \code{\link[survival]{survfit}}
  or \code{survfit.cox}}

  \item{lty.surv}{line type for survival curve(s); see
  \code{\link{par}}}

  \item{lwd.surv}{line width for survival curve(s)}

  \item{col.surv}{line color for survival curve(s); either
  numeric or character string(s); hexadecimal format also
  works}

  \item{mark}{numeric plotting character
  (\code{\link{pch}}) or character string, i.e., \code{''},
  \code{'|'}, etc}

  \item{lty.ci}{line type for confidence interval(s); not
  plotted (\code{= 0}) by default}

  \item{lwd.ci}{line width for confidence interval(s)}

  \item{col.ci}{line color for confidence interval(s);
  either numeric or character string(s); hexadecimal format
  also works}

  \item{col.band}{line color for confidence band(s); either
  numeric or character string(s); hexadecimal format also
  works; note that this is not a true confidence band; see
  details}

  \item{atrisk}{logical; if \code{TRUE}, draws at risk
  table}

  \item{atrisk.lab}{heading for at risk table}

  \item{atrisk.lines}{logical; draw lines next to strata in
  at risk table}

  \item{strata.lab}{labels used in legend and at risk table
  for strata; if \code{NULL} (default), labels created in
  \code{survfit} are used; if only one strata is present,
  "All" is used by default}

  \item{strata.order}{order of strata in legend and at risk
  table}

  \item{extra.margin}{increase left margin when strata
  labels in at risk table are long}

  \item{xlim}{x-axis limits}

  \item{ylim}{y-axis limits}

  \item{xaxis.at}{positions for x-axis labels and ticks}

  \item{xaxis.lab}{x-axis labels}

  \item{yaxis.at}{positions for y-axis labels and ticks}

  \item{yaxis.lab}{y-axis labels}

  \item{xlab}{x-axis label}

  \item{ylab}{y-axis label}

  \item{main}{title}

  \item{cex.axis}{text size for axes labels, legend, at
  risk table}

  \item{legend}{logical; draw legend}

  \item{legend.pos}{legend position}

  \item{grid}{logical; draw grid in background}

  \item{lty.grid}{line type for grid}

  \item{lwd.grid}{line width for grid}

  \item{col.grid}{line color for grid}

  \item{dev}{logical; if \code{TRUE}, will plot on a
  platform-specific graphics device; see details}

  \item{add}{logical; if \code{TRUE}, \code{par} is not
  refreshed; allows for multiple panels, e.g., when using
  \code{par(mfrow = c(1, 2))}}

  \item{...}{additional parameters (\code{font},
  \code{mfrow}, \code{bty}, \code{tcl}, \code{cex.lab},
  \code{xaxs}, etc) passed to \code{par}}
}
\description{
Function to plot Kaplan-Meier or Cox proportional hazards
plots with optional at risk table in base graphics
}
\details{
Line specifications (\code{lty.surv}, \code{lwd.surv}, etc)
will be recycled if needed.

If \code{col.band != NULL}, a confidence band is plotted;
however, this is not a confidence band in the statistical
sense, i.e., a xx-percent chance of containing the entire
population of the survival curve which are wider than the
point-wise confidence limits. Rather, it refers to a band
of color plotted between the confidence limits calculated
in the survfit object. That is, the xx-percent confidence
interval (plotted when \code{lty.ci != 0}) and the
confidence bands are identical, just two ways of plotting
the same invervals.

When \code{dev = TRUE}, a platform-specific graphics device
is used to displace the plot. Defaults are
\code{\link{quartz}}, \code{\link{windows}}, and
\code{\link{x11}} for apple, windows, and unix platforms,
respectively. However, any device can be used by setting
\code{dev = FALSE} and opening a device before running
\code{kmplot}. If \code{dev} is \code{FALSE}, the plot will
open in the current device; see \code{\link{.Device}}.
Close the device pane or use \code{dev.off()} to turn off
the current device.

When saving plots, it is highly recommended to use
\code{\link{png}}, \code{\link{svg}}, \code{\link{pdf}},
etc instead of exporting directly from the \code{R}
graphics device. Doing so may cause the at risk table or
legend to be mis-aligned. Be sure to set \code{dev = FALSE}
when saving an image to another device.
}
\examples{
library(survival)
kmfit1 <- survfit(Surv(time, status) ~ sex, data = colon)
kmfit2 <- survfit(Surv(time, status) ~ rx + adhere, data = colon)

## simple example, draw in r window
kmplot(kmfit1, dev = FALSE)

## using mfrow options, use ADD = TRUE
## when saving to another device, use dev = FALSE (see details)
\dontrun{
png('./desktop/kmplot2.png', width = 750, height = 1200, pointsize = 14)
par(mfrow = c(2, 1))
kmplot(kmfit1, add = TRUE, dev = FALSE)
kmplot(kmfit2, add = TRUE, dev = FALSE, extra.margin = 8)
dev.off()
}

## more complex example
\dontrun{
pdf('./tmp.pdf', height = 8, width = 11, pointsize = 12)
kmplot(kmfit2,
       dev = FALSE,  # view in r window
       mark = '',    # no censor mark
       lty.ci = 2,   # dashed line for CIs
       xaxis.at = c(0, .5, 1:9) * 365,    # change days to years
       xaxis.lab = c(0, .5, 1:9),         # label years
       yaxis.lab = pretty(c(0, 1)) * 100, # change to percent
       xlab = 'Time (years)',
       ylab = 'Percent survival',
       col.surv = c('blue', 'red', 'green', 'black','purple','orange'),
       strata.lab = c('Obs ','Obs+ ','Lev ','Lev+ ','Lev5fu','Lev5fu+'),
       extra.margin = 6, # increase margin for long strata labels
       strata.order = c(5, 6, 3, 1, 4, 2),
       # col.band = NULL, # remove confidence bands
       font = 2,  # bold table text
       bty = 'l', # L box type around plot
       tcl = .5)  # change length/direction of ticks
title(main='Chemotherapy for stage B/C colon cancer',
      adj = .5, font.main = 1, line = 0.5, cex.main = 1)
dev.off()
}
}
\references{
\url{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}
}
\seealso{
\code{\link[rawr]{ggsurv}}; \code{survival:::plot.survfit}
}

