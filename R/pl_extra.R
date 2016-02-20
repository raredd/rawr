### plot extra
# dodge, show_colors, show_pch, tcol, pretty_sci, oom, parse_sci, arrows2,
# carrows
###


#' Point dodge
#' 
#' Dodge and center overlapping points by group. Spreads scattered points
#' similar to \code{jitter} but symmetrically. Although the default method
#' can be used, it is recommended to use the formula method for ease of use
#' and to set useful defaults for \code{jit} and \code{dist}.
#' 
#' @param formula a \code{\link{formula}}, such as \code{y ~ group}, where
#' \code{y} is a numeric vector of data values to be split into groups
#' according to the grouping variable, \code{group}
#' @param data optional matrix or data frame containing the variables in 
#' \code{formula}; by default, the variables are taken from
#' \code{environment(formula)}
#' @param x grouping variables or, equivalently, positions along x-axis
#' @param y a numeric vector of data, y-values
#' @param jit,dist jittering parameters; \code{jit} describes the spread of
#' close points, and \code{dist} defines a range to consider points "close"
#' @param ... additional arguments passed to other methods
#' 
#' @seealso
#' \code{\link{jitter}}, \code{\link{tplot}}
#' 
#' @examples
#' ## these are equivalent ways to call dodge:
#' dodge(mpg ~ gear + vs, mtcars)
#' with(mtcars, dodge(list(gear, vs), mpg))
#' dodge(mtcars[, c('gear', 'vs')], mtcars$mpg)
#' 
#' 
#' ## compare to overlapping points and jittering
#' op <- par(no.readonly = TRUE)
#' sp <- split(mtcars$mpg, do.call(interaction, mtcars[, c('gear','vs')]))
#' plot.new()
#' par(mar = c(0,0,0,0), cex = 2)
#' plot.window(c(.5,6.5),c(10,35))
#' box()
#' for (ii in seq_along(sp))
#'   points(rep(ii, length(sp[[ii]])), sp[[ii]])
#' for (ii in seq_along(sp))
#'   points(jitter(rep(ii, length(sp[[ii]]))), sp[[ii]], col = 4, pch = 1)
#' points(dodge(mpg ~ gear + vs, mtcars), col = 2, pch = 4)
#' legend('topleft', pch = c(1,1,4), col = c(1,4,2), cex = .8,
#'        legend = c('overlapping','random jitter','dodging'))
#' par(op)
#' 
#' 
#' ## practical use
#' boxplot(mpg ~ vs + gear, data = mtcars)
#' points(dodge(mpg ~ vs + gear, data = mtcars), col = 2, pch = 19)
#'
#' @export

dodge <- function(x, ...) UseMethod('dodge')

#' @rdname dodge
#' @export
dodge.formula <- function(formula, data = NULL, ...) {
  if (missing(formula) || (length(formula) !=  3))
    stop("\'formula\' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  nmargs <- names(args)
  m$... <- NULL
  m[[1]] <- as.name('model.frame')
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, 'terms'), 'response')
  dodge(mf[, -response], mf[, response])
}

#' @rdname dodge
#' @export
dodge.default <- function(x, y, dist, jit, ...) {
  if (is.data.frame(y)) {
    x <- y[, 2]
    y <- y[, 1]
  }
  x <- if (!missing(x) && is.list(x))
    as.numeric(do.call('interaction', x)) else
      rep_len(if (missing(x)) 1 else x, length(x))
  if (missing(dist) || is.na(dist) || is.null(dist)) 
    dist <- diff(range(x)) / 100
  if (missing(jit) || is.na(jit) || is.null(jit)) 
    jit <- 0.1
  ## call dodge on each group
  cbind.data.frame(x_new = ave(seq_along(y), x, FUN = function(ii)
    dodge_(y[ii], x[ii], dist, jit)$x),
    y = y)
}

#' Show colors
#' 
#' In \code{R}, there are 657 named colors. This function shows these colors 
#' and their respective numbers. Find a color by number in the plot or by the
#' name of the color with \code{colors()[n]}.
#' 
#' @seealso \code{\link{show_pch}}
#' 
#' @examples
#' show_colors()
#' colors()[81]
#' # [1] "darkgreen"
#' 
#' @export

show_colors <- function() {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(1,1), mai=c(.4,.4,.4,.4), oma=c(.2,0,0,.2))
  x <- 22
  y <- 30
  plot(c(-1, x), c(-1, y), ann = FALSE, type = 'n', bty = 'n', axes = FALSE)
  sapply(1:x, function(i) {
    sapply(1:y, function(j) {
      k <- y * (i - 1) + j
      co <- colors()[k]
      rect(i - 1, j - 1, i, j, col = co, border = grey(.5))
    })
  })
  text(rep(-.5, y), (1:y) - .5, 1:y, cex = 1.2 - .016 * y)
  text((1:x) - .5, rep(-.5, x), y * (0:(x - 1)), cex = 1.2 - .022 * x)
  title('col = colors()[n]')
}

#' Show plotting characters
#' 
#' In \code{R}, there are 26 numeric plotting characters. This function shows 
#' these options and their respective numbers. Note that \code{col} specifies
#' both the border and fill color (if applicable) for \code{0:20}; \code{pch}s
#' \code{21:25} can be filled with \code{bg}.
#' 
#' @seealso \code{\link{show_colors}}
#' 
#' @examples
#' show_pch()
#' 
#' @export

show_pch <- function() {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(xpd = TRUE, mfrow = c(1, 1), mai = c(.4,.4,.4,.4), oma = c(.2,0,0,.2))
  x <- rep(1:5, 6)[1:26]
  y <- c(rep(5:1, each = 5)[1:25], 0)
  plot(x, y, pch = 0:25, axes = FALSE, bg = 'gray', cex = 2, col = 'red')
  text(x = x, y = y, labels = 0:25, pos = 4, cex = 1.5, offset = 1)
  text(x = 4, y = 0, labels = 'plotting characters 0:25', cex = 1.5)
}

#' Transparent colors
#' 
#' Add alpha transparency to colors.
#' 
#' This is a vectorized function to add transparency to colors. \code{color}
#' and \code{trans} (or \code{alpha}) must either be the same length or one
#' of the two must have length one. 
#' 
#' The function adds integers (in hexadecimal) between 0 (fully transparent)
#' and 255 (fully visible) to the color(s) given. \code{color} values are
#' converted to RGB with transparency.
#' 
#' @param color vector of color names (or hexadecimal) as character strings
#' or integers corresponding to colors in the current \code{\link{palette}}
#' @param trans alpha transparency defined as an integer in the range 
#' \code{[0, 255]} where \code{0} is fully transparent and \code{255} is fully
#' visible; see details
#' @param alpha the alpha transparency in \code{[0,1]}; \code{trans} is
#' ignored if \code{alpha} is given
#' 
#' @seealso \code{\link{as.hexmode}}, \code{\link{col2rgb}},
#' \code{\link{adjustcolor}}
#' 
#' @examples
#' cols <- c('red','green','blue')
#' 
#' ## a normal plot
#' plot(rnorm(100), col = tcol(cols), pch = 16, cex = 4)
#' 
#' ## more transparent
#' plot(x <- rnorm(100), col = tcol(cols, 100), pch = 16, cex = 4)
#' ## or equivalently using alpha
#' plot(x, col = tcol(cols, alpha = .4), pch = 16, cex = 4)
#' 
#' ## hexadecimal colors also work
#' cols <- c('#FF0000','#00FF00','#0000FF')
#' plot(rnorm(100), col = tcol(cols, c(50, 100, 255)), pch= 16, cex = 4)
#' 
#' @export

tcol <- function(color, trans = 255, alpha) {
  if (!missing(alpha)) {
    stopifnot(alpha %inside% c(0,1))
    trans <- round(rescaler(alpha, to = c(0,255), from = c(0,1)))
  }
  if (length(color) != length(trans) & 
      !any(c(length(color), length(trans)) == 1)) 
    stop('Vector lengths are not comformable')
  if (length(color) == 1 & length(trans) > 1) 
    color <- rep(color, length(trans))
  if (length(trans) == 1 & length(color) > 1) 
    trans <- rep(trans, length(color))
  
  res <- paste0('#', apply(apply(rbind(col2rgb(color)), 2, function(x)
    format(as.hexmode(x), 2)), 2, paste, collapse = ''))
  res <- unlist(unname(Map(paste0, res, as.character(as.hexmode(trans)))))
  res[is.na(color)] <- NA
  res[color %in% 'transparent'] <- 'transparent'
  
  res
}

#' Print scientific numbers
#' 
#' Functions to parse numeric vectors in scientific notation and return an
#' \code{\link{expression}} for a pretty display.
#' 
#' @param x a numeric vector
#' @param digits integer indicating the number of decimal places to be used
#' @param limit a numeric value whose order of magnitude will set a limit
#' beyond which values of \code{x} will be displayed in scientific notation;
#' default is to display numbers beyond a magnitude of 3; to display
#' scientific notation always, use a negative value
#' 
#' @seealso
#' \code{\link{roundr}}; \code{\link{format}}; \code{\link{sprintf}},
#' \code{\link{rawr_parse}}
#' 
#' @return
#' For \code{oom} a vector of order of magnitudes. For \code{parse_sci} an
#' expression of values in scientific notation. For \code{pretty_sci} an
#' expression of values in standard or scientific notation depending on the
#' value of \code{limit}.
#' 
#' @examples
#' x <- 10^(1:5) / 10
#' oom(x)
#' 
#' parse_sci(x)
#' 
#' par(xpd = NA, mar = c(6,4,4,2) + .1)
#' plot(1:5, type = 'n', axes = FALSE, ann = FALSE)
#' axis(2, at = 1:5, labels = pretty_sci(x), las = 1, lwd.ticks = 0)
#' 
#' text(1:5, 0, pretty_sci(1 / x ^ 10))
#' text(1:5, 1, pretty_sci(1 / x, digits = 3))
#' text(1:5, 2, pretty_sci(1 / x, digits = 2, limit = 1e2))
#' text(1:5, 3, x)
#' text(1:5, 4, pretty_sci(x, limit = 1e2))
#' text(1:5, 5, pretty_sci(x, digits = 1))
#' text(1:5, 6, pretty_sci(x ^ 10))
#' 
#' text(1:5, -1, pretty_sci(1 / x, limit = -1))
#' 
#' @export

pretty_sci <- function(x, digits = 0, limit = 1e3) {
  l <- as.list(x)
  limit <- if (limit < 0) -1 else oom(limit)
  om <- sapply(l, oom)
  sapply(seq_along(l), function(y)
    if (abs(om[y]) > limit)
      parse_sci(l[[y]], digits) else roundr(l[[y]], digits))
}

#' @rdname pretty_sci
#' @export
oom <- function(x) {
  x <- format(x, scientific = TRUE)
  as.numeric(gsub('.*e.', '', x))
}

#' @rdname pretty_sci
#' @export
parse_sci <- function(x, digits = 0) {
  stopifnot(is.numeric(x))
  x <- format(x, nsmall = digits, scientific = TRUE)
  x <- strsplit(x, 'e[+]?[0]?')
  xbg <- sapply(x, function(y) roundr(as.numeric(y[[1]]), digits))
  xsm <- sapply(x, '[[', 2)
  parse(text = do.call('sprintf', list(fmt = '%s%%*%%10^%s', xbg, xsm)))
}

#' Add filled arrows to a plot
#' 
#' Draw filled arrows between pairs of points.
#' 
#' @param x0,y0 coordinates of points \strong{from} which to draw
#' @param x1,y1 coordinates of points \strong{to} which to draw
#' @param size,width size parameters for arrows
#' @param curve a numeric value greater than 0 giving the curvature of the
#' arrows; default is \code{1} for staight lines, but values less than or
#' greater than 1 may be given for 
#' @param code integer determining \emph{kind} of arrows to be drawn; arrows
#' are drawn at \code{{x0[i], y0[i]}}, \code{{x1[i], y1[i]}}, or both for
#' \code{code = 1:3}, respectively, or no heads drawn if \code{code = 0}
#' @param col,lty,lwd color, line type, and line width of the segment
#' @param fill,border fill and border color of arrow
#' @param sadj optional vector of the form \code{{x0,y0,x1,y1}} for adjusting
#' the \code{\link{segments}} of the arrows
#' @param ... additional graphical parameters passed to \code{\link{segments}}
#' and further to \code{\link{par}}
#' 
#' @seealso
#' \code{arrows}; \code{\link{carrows}}; \url{https://github.com/cran/sfsmisc}
#' 
#' @author
#' Modifications: Robert Redd; original: Andreas Ruckstuhl, 19 May 1994;
#' Cosmetic: Martin Machler, June 1998
#' 
#' @examples
#' plot.new()
#' plot.window(0:1, 0:1, asp = 1)
#' arrows2(0,0,1,1, code = 3, border = 2, fill = 0)
#' arrows2(.2, 0, .2, 1, code = 1, fill = 4, col = 4)
#' arrows2(.4, 0, .4, 1, code = 2, fill = 2, size = .5, width = .5)
#' arrows2(.6, 0, .6, 1, code = 3, curve = 1.5)
#' arrows2(.8, 0, .8, 1, code = 3, curve = .5, width = .5, lwd = 10, col = 3)
#' 
#' @export

arrows2 <- function(x0, y0, x1 = x0, y1 = y0, size = 1,
                    width = (sqrt(5) - 1) / 8 / cin, curve = 1, code = 2,
                    col = par('fg'), lty = par('lty'), lwd = par('lwd'),
                    fill = col, border = fill, sadj = c(0,0,0,0), ...) {
  stopifnot(length(code) == 1L & code %in% 0:3)
  cin <- size * par('cin')[2]
  ## inches per usr unit
  uin <- if (is.R()) 1 / xyinch() else par('uin')
  
  ## create coordinates of a polygon for a unit arrow head
  # x <- sqrt(seq(0, cin ^ 2, length = floor(350 * cin) + 2))
  x <- sqrt(seq(0, cin ^ 2, length = 1000))
  delta <- 0.005 / 2.54
  x.arr <- c(-x, -rev(x))
  wx2 <- width * x ^ curve
  y.arr <- c(-wx2 - delta, rev(wx2) + delta)
  
  ## polar, NA to "break" long polygon
  pol <- c2p(x.arr, y.arr)
  ## rawr:::c2p
  deg.arr <- c(pol$theta, NA)
  r.arr <- c(pol$radius, NA)
  
  segments(x0 + sadj[1], y0 + sadj[2], x1 + sadj[3], y1 + sadj[4],
           col = col, lty = lty, lwd = lwd, lend = 1, ...)
  
  if (code == 0L)
    return(invisible())
  
  if (code %in% 2:3) {
    theta <- atan2((y1 - y0) * uin[2], (x1 - x0) * uin[1])
    lx <- length(x0)
    Rep <- rep.int(length(deg.arr), lx)
    xx <- rep.int(x1, Rep)
    yy <- rep.int(y1, Rep)
    theta <- rep.int(theta, Rep) + rep.int(deg.arr, lx)
    r.arr <- rep.int(r.arr, lx)
    polygon(xx + r.arr * cos(theta) / uin[1],
            yy + r.arr * sin(theta) / uin[2],
            col = fill, xpd = NA, border = border)
  }
  
  if (code %in% c(1L, 3L)) {
    arrows2(x1, y1, x0, y0, size, width, code = 2, curve,
            col = col, lty = 0, lwd = 0, fill = fill, border = border, ...)
  }
}

#' Curved arrows
#' 
#' Draw an arrow along the arc of a circle.
#' 
#' @param p1,p2 vectors of length two giving the \code{{x,y}} coordinates for
#' two points to draw a connecting arc
#' @param arc a vector of length two with the starting and ending positions
#' to draw the arc, in radians or degrees
#' @param degree logical; if \code{TRUE}, \code{arc} should be in degrees;
#' however, if either element of \code{arc} is greater than \code{2 * pi},
#' they are assumed to be in degrees and will be converted to radians; set to
#' \code{TRUE} to avoid conversion of small angles to radians
#' @param pad a vector of length two giving 1) padding between the tips of
#' the arrow/segment and the points and 2) additional padding between the
#' segment endpoints and tip of arrow--useful for thick lines which may
#' protrude from under the arrowhead
#' @param flip logical; if \code{TRUE}, the arrow will be rotated around the
#' circle 180 degrees
#' @param dir optional vector of directions for arrows; by default, arrows
#' will point to the nearest endpoint; \code{dir} should be a vector of
#' \code{1}s and \code{-1}s and will be recycled as necessary; other values
#' will be ignored and result in no arrows for those positions
#' @param col,lwd,lty color, line width, and line type passed to
#' \code{\link{lines}}
#' @param size,width,curve,fill,border additional parameters passed to
#' \code{\link{arrows2}}
#' 
#' @return
#' A list containing the arc endpoints and the center and radius of the
#' corresponding circle.
#' 
#' @seealso
#' \code{\link{arrows2}}; \code{\link{xspline}}
#' 
#' @examples
#' plot.new()
#' plot.window(c(-2,2), c(-2,2))
#' p <- matrix(c(rep(-1:1, 2), rep(-1:1, each = 2)), ncol = 2)
#' points(p)
#' carrows(p1 <- p[2, ], p2 <- p[1, ], pad = .3)
#' carrows(p1 <- p[4, ], p2 <- p[5, ], dir = c(0,1), col = 3)
#' carrows(p1 <- p[6, ], p2 <- p[3, ], lwd = 10, pad = c(.05, .1))
#' carrows(p1 <- p[1, ], p2 <- p[6, ], flip = TRUE)
#' carrows(p1 <- p[1, ], p2 <- p[5, ], dir = c(1,0))
#' 
#' @export

carrows <- function(p1, p2, arc, degree = FALSE, pad = 0.01 * 1:2,
                    flip = FALSE, dir = NULL,
                    ## lines
                    col = par('col'), lwd = par('lwd'), lty = par('lty'),
                    ## arrows2
                    size = 1, width = size / 2, curve = 1, fill = col,
                    border = NA) {
  
  code_ <- function(x) c(2,0,1)[match(x, -1:1)]
  pad_ <- function(x, pad) ht(x, -length(x) * (1 - pad))
  
  ## try to guess code for arrows2
  slope <- (p2[2] - p1[2]) / (p2[1] - p1[1])
  slope[!is.finite(slope) | slope == 0] <- 1
  code <- code_(sign(slope))
  
  ## calculate arc based on p1, p2
  radius <- sqrt(sum((p1 - p2) ** 2)) / 2
  centers <- (p1 + p2) / 2
  arc <- if (!missing(arc)) {
    if (degree | any(arc > 2 * pi))
      d2r(arc) else arc[1:2]
  } else sapply(list(p1, p2), function(x)
    p2r(x[1], x[2], centers[1], centers[2]))
  
  ## convert polar to cart and plot lines/arrows
  theta <- seq(arc[1], arc[2], length = 500) + if (flip) pi else 0
  pad <- rep_len(pad, 2)
  th <- pad_(theta, pad[1])
  xx <- centers[1] + radius * cos(th)
  yy <- centers[2] + radius * sin(th)
  lines(pad_(xx, pad[2]), pad_(yy, pad[2]), col = col, lwd = lwd, lty = lty)
  
  xx <- ht(xx, 4)
  yy <- ht(yy, 4)
  arrows2(xx[1], yy[1], xx[2], yy[2], size = size, width = width,
          curve = curve, code = dir[1] %||% code, col = col, lty = 0,
          lwd = 0, fill = fill, border = border)
  arrows2(xx[4], yy[4], xx[3], yy[3], size = size, width = width,
          curve = curve, code = dir[2] %||% code, col = col, lty = 0,
          lwd = 0, fill = fill, border = border)
  invisible(list(arc = arc, centers = centers, radius = radius))
}
