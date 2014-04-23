\name{click.shape}
\alias{click.shape}
\title{Add shapes interactively in base \code{R} graphics}
\usage{
click.shape(shape = 'line', col = 'black', border = col, trans = NULL,
            corners = NULL, lty = par('lty'), lwd = par('lwd'), length = 1,
            code = 2, ...)
}
\arguments{
  \item{shape}{type of shape: 'box', 'arrow', 'line',
  'poly', 'circle', 'cyl'}

  \item{col}{shape outline colour}

  \item{border}{border colour for shape; defaults to value
  for \code{col}}

  \item{trans}{color transparency; see \code{\link{tcol}}}

  \item{corners}{number of corners to draw for a polygon}

  \item{lty,lwd}{graphical parameters; see
  \code{\link{par}}}

  \item{density}{the density of shading lines in lines per
  inch; the default value of \code{NULL} means that no
  shading lines are drawm; a zero value of \code{density}
  means no shading or filling, whereas negative values and
  \code{NA} suppress shading (and so allow color filling)}

  \item{length}{length of the edges of the arrow head (in
  inches); see \code{\link{arrows}}}

  \item{code}{integer code for 'arrows' determining
  \emph{kind} of arrows to be drawn}

  \item{x11}{logical; if \code{TRUE}, starts a graphics
  device for the X Window System (version 11); this can
  only be done on machines/accounts that have access to an
  X server}

  \item{...}{other parameters (future use)}
}
\description{
Allows you to add shapes anywhere in a plot (including
margins) with mouse click(s)
}
\examples{
plot.new()
click.shape()
click.shape('arrow', col = 'blue', code = 2, lwd = 2, length = .15)
click.shape('box', border = 'purple', col = 'pink', lwd = 2)
click.shape('box', col = NULL, border = 'purple', lwd = 2)
click.shape('line', col = 'orange', lty = 3, lwd = 3)
click.shape('poly', corners = 5, border = 'green', col = 'orange',
  lty = 1, lwd = 3)
click.shape('poly', corners = 3, border = 'red', col = 'yellow', lty = 1,
  lwd = 2)
click.shape('cyl', col = 'orange')
click.shape('circle', col = 'orange', border = 'black', lty = 3, lwd = 3)
}
\seealso{
\code{\link{click.text}}
}

