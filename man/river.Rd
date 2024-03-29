% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{river}
\alias{river}
\alias{river2}
\alias{check_river_format}
\title{River plots}
\usage{
river(
  data,
  bar_data,
  id = NULL,
  at = seq_along(id),
  legend = TRUE,
  args.legend = list(),
  xlim = NULL,
  ylim = NULL,
  rev = FALSE,
  stagger = TRUE,
  col = NULL,
  axes = TRUE,
  label = TRUE,
  bar.width = 0.25,
  bar.alpha = 0.5,
  col.seg = c(1L, 3L),
  col.arrows = 1L,
  pch.pt = c(16L, 4L, 4L),
  cex.pt = 1.5,
  col.pt = c(2L, 2L, 4L),
  panel.first = NULL,
  panel.last = NULL,
  ...
)

river2(
  data,
  bar_data,
  bar_data2,
  id,
  legend = TRUE,
  args.legend = list(),
  xlim = NULL,
  ylim = NULL,
  rev = FALSE,
  stagger = FALSE,
  split = FALSE,
  col = NULL,
  col2 = NULL,
  axes = TRUE,
  bar.width = 0.25,
  bar.alpha = 0.5,
  ...
)

check_river_format(data, bar_data)
}
\arguments{
\item{data, bar_data, bar_data2}{data frames; these should have a specific
format, see details, examples, or run \code{check_river_format()}}

\item{id, at}{optional parameters specifying individuals (rows) from
\code{data} to plot and their positions along the y-axis; if not given,
time lines are plotted sequentially}

\item{legend}{logical; if \code{TRUE}, a legend using the unique values of
\code{data$status} is drawn}

\item{args.legend}{a \emph{named} list of arguments passed to
\code{\link{legend}}}

\item{xlim, ylim}{x- and y-axis limits}

\item{rev}{logical; if \code{TRUE}, observations will be plotted from top
to bottom}

\item{stagger}{logical; if \code{FALSE}, start dates will be fixed at 0
rather than relative to the first start date if \code{TRUE} (default)}

\item{col, col2}{vectors of colors for responses (\code{river}) or toxicity
grades \code{river2}, respectively}

\item{axes}{logical; if \code{TRUE}, x-axes are drawn}

\item{label}{logical; if \code{TRUE}, IDs are shown for each line}

\item{bar.width, bar.alpha}{width and alpha transparency for bars}

\item{col.seg}{colors for timeline/on-treatment}

\item{col.arrows}{the color for on-going arrows or use \code{NA} to suppress}

\item{pch.pt, cex.pt, col.pt}{the plotting character, size, and color for
progression, death, and censoring points, respectively}

\item{panel.first, panel.last}{expression evaluated before and after plotting}

\item{...}{graphical parameters passed to \code{\link{par}}}

\item{split}{logical; if \code{TRUE}, rows of \code{bar_data2} will be
plotted individually}
}
\description{
Summarize a timeline for individuals over the course of a study.

\code{river} shows the trajectory of multiple subjects. Time on treatment
is represented by a green line; progression as a red dot; censored or
death as an "x" in blue or red, respectively; and responses colored with
bands. Subjects continuing are shown with an arrow.

\code{river2} shows the trajectory for a single patient with additional
toxicity information represented by another series of bands colored by
grade. Optionally, the trajectory for a single patient from \code{river}
may be shown rather than with an arrow; see examples.
}
\details{
\code{data}, \code{bar_data}, and \code{bar_data2} data frames need to have
a minimum number of variables in a specific order, some of which should be
\code{\link{Date}} formats (or can be coerced to dates, e.g., integers).

\code{check_river_format()} without any arguments will give a summary of
the required formats.

Note that date columns can also be given as integers to be coerced to dates
as seen in the examples. In this case, \code{check_river_format} will do
the coersion to date before plotting. However, these two methods are
interchangeable, but using integers rather than dates assume the data have
a common starting time.

\code{data} should have 10 columns (any additional will be ignored) in the
following order: ID; dates of registration, start of treatment, end of
treatment, progression, off treatment, and survival status; status
indicator; and dates of last contact and off study. That is, these should
be in approximate chronological order.

\code{bar_data} and \code{bar_data2} provide additional information to
supplement \code{data} and may contain multiple records per ID.

\code{bar_data} is intended to represent response assessments and therefore
should have three columns: ID, date of assessment, and assessment. Any
additional columns are ignored. Assessment data should be a factor variable
with proper level ordering or will be coerced.

\code{bar_data2} is intended to represent toxicity assessments and therefore
should have five columns: ID, start date, end date, grade, and description.
Any additional columns are ignored. Grade should be a factor variable with
proper level ordering or will be coerced. Descriptions should be relatively
short to avoid text extending outside of the plotting window.

Despite the assumptions above, any type of data may work if properly
formatted and ordered according to \code{check_river_format()}.
}
\examples{
## to print a summary of the required formats
check_river_format()


## data in river format:
dd <- data.frame(
  id       = 1:5,
  dt_reg   = 1:5,
  dt_txst  = 1:5 + 1,
  dt_txend = c(10:13, 8),
  dt_prog  = c(11, 16, NA, NA, 8),
  dt_offtx = c(12, 19, NA, NA, 8),
  dt_surv  = c(12, 19, 24, 25, NA),
  surv     = c(0, 1, 0, 0, NA),
  dt_last  = c(12, 19, 24, 25, NA),
  dt_off   = c(12, 19, NA, NA, 8)
)
bd <- data.frame(
  id        = c(3, 3, 3, 3, 4, 4),
  dt_assess = c(9, 13, 17, 21, 10, 15),
  resp = factor(c('MR', 'PR', 'PR', 'CR', 'PR', 'CR'),
                levels = c('PD', 'SD', 'MR', 'PR', 'CR'))
)

## basic usage
river(dd, bd)
river(
  dd, bd, stagger = FALSE, rev = TRUE, col = 2:6,
  args.legend = list(x = 'bottom', title = 'Response', horiz = TRUE)
)


## using NA will suppress data from being plotted
river(dd, within(bd, resp <- NA))
river(within(dd, dt_txst <- NA), within(bd, resp <- NA))
river(within(dd, dt_txst <- NA), within(bd, {resp <- NA; dt_assess <- NA}))


## same data with single observations per id
bd1 <- data.frame(
  id = 3:4, dt_assesss = 9:10,
  resp = factor(c('PR','CR'), levels = c('PD','SD','MR','PR','CR'))
)

river(dd, bd1)

## id and at parameters control the positions of the timelines
river(dd, bd1, id = c(1, 2, 5, 3, 4), at = c(1:2, 4, 6:7), legend = FALSE)


## additional data for river2
tt <- data.frame(
  id       = rep(c(1, 3), times = c(1, 5)),
  dt_start = c(3, 5, 5, 8, 9, 11),
  dt_end   = c(NA, 5, NA, 10, 10, NA),
  grade    = c(1, 4, 3, 4, 1, 2),
  desc     = paste('tox', c(1, 1:5))
)

river2(dd, bd, tt, id = 3)

## multiple records per id (ie, worsening toxicities)
tt2 <- data.frame(
  id       = rep(c(1, 3), times = c(1, 8)),
  dt_start = c(3, 5, 5, 7, 15, 8, 9, 11, 14),
  dt_end   = c(NA, 5, 7, 15, NA, 10, 10, 14, NA),
  grade    = c(1, 4, 1, 2, 3, 4, 1, 2, 3),
  desc     = paste('tox', c(1, 1, 2, 2, 2, 3, 4, 5, 5))
)
                  
river2(dd, bd, tt2, id = 3)


## bar_data can also be given in river2 without additional information
river2(bar_data = tt,  id = 3)
river2(bar_data = tt2, id = 3)


## use custom axis() to change time units
river2(bar_data = tt2, id = 3, axes = FALSE)
at <- seq(0, 2, 0.5)
axis(1L, at * 7, at)
title(xlab = 'Weeks from registration', line = 2.5)

}
