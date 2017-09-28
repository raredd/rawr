### plot misc, extra, random
# dodge, show_colors, show_pch, tcol, pretty_sci, oom, parse_sci, arrows2,
# carrows, laxis, coords, col_scaler
# 
# unexported:
# dodge.formula, dodge.default, to_sci_
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
  if (missing(formula) || (length(formula) != 3L))
    stop("\'formula\' missing or incorrect")
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m$`...` <- NULL
  m[[1L]] <- as.name('model.frame')
  
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, 'terms'), 'response')
  
  dodge(mf[, -response], mf[, response])
}

#' @rdname dodge
#' @export
dodge.default <- function(x, y, dist, jit, ...) {
  if (is.data.frame(y)) {
    x <- y[, 2L]
    y <- y[, 1L]
  }
  
  x <- if (!missing(x) && is.list(x))
    as.numeric(do.call('interaction', x)) else
      rep_len(if (missing(x)) 1L else x, length(x))
  
  if (missing(dist) || is.na(dist) || is.null(dist))
    dist <- diff(range(x)) / 100
  if (missing(jit) || is.na(jit) || is.null(jit))
    jit <- 0.1
  
  ## call dodge on each group
  cbind.data.frame(
    x_new = ave(seq_along(y), x, FUN = function(ii)
      dodge_(y[ii], x[ii], dist, jit)$x),
    y = y
  )
}

#' Show colors
#' 
#' @description
#' \code{R} includes 657 named \code{\link{colors}}. This is a convenience
#' function to locate specific ones quickly.
#' 
#' Find a color by index in the plot created with \code{show_colors()} by
#' first locating the desired color and summing the row and column indices
#' corresponding to its position.
#' 
#' Return a color name or index by giving the index or name, respectively,
#' optionally, plotting one or more. Search for all colors with a pattern,
#' such as \code{"red|orange"}, and return all matches, optionally plotting.
#' 
#' @param ... integer(s) in \code{1:657} corresponding to the built-in color
#' name index or color name string(s); if \code{?} is included as a string or
#' part of a string, color names will be searched for matches
#' @param plot logical; if \code{TRUE}, integers or color names in \code{dots}
#' will be plotted with corresponding number and name
#' 
#' @return
#' If \code{...} is missing, a plot will be drawn. If an integer is given, the
#' color name will be returned; if a color name string is given, the index
#' will be returned. For the latter two options, no plot is drawn by default
#' but will be if \code{plot = TRUE}. If one or more strings are given and
#' one contains a \code{"?"}, all color names matching the input will be
#' returned and optionally plotted.
#' 
#' @seealso
#' \code{\link{show_pch}}; \code{\link{colors}}; \code{\link{waffle}};
#' \code{\link{tcol}}
#' 
#' @examples
#' ## typical usage
#' show_colors()
#' show_colors(5, 6, 544)
#' show_colors('blue4', 'dodgerblue2')
#' 
#' 
#' ## search for color names or numbers
#' show_colors(grep('red|orange', colors()), plot = TRUE)
#' ## shorthand
#' show_colors('?red', 'orange')
#' show_colors('?red|orange', plot = TRUE)
#' 
#' 
#' ## this function is its own inverse
#' show_colors(81)
#' show_colors('darkgreen')
#' 
#' x <- show_colors(sample(657, 10))
#' identical(x, show_colors(show_colors(x)))
#' 
#' ## these plots are identical
#' show_colors(x, plot = TRUE)
#' show_colors(show_colors(x), plot = TRUE)
#' 
#' @export

show_colors <- function(..., plot = FALSE) {
  dots <- c(...)
  
  ## if ? is found, return all colors matching inputs
  if (any(grepl('\\?', dots))) {
    dots <- Filter(nzchar, gsub('\\?', '', tolower(dots)))
    dots <- grep(paste0(dots, collapse = '|'), colors())
  }
  
  ## guess if color names or indices were given to determine return value
  cols <- if (is.numeric(dots)) {
    stopifnot(dots %inside% c(1, 657))
    colors(FALSE)[as.integer(dots)]
  } else if (is.character(dots)) {
    dots <- gsub('[^a-z0-9]', '', tolower(dots))
    match(dots, colors(FALSE))
  } else if (length(dots))
    warning('... should be missing, %in% 1:657, or a color name')
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(1,1), mar = c(1,4,1,2), cex = 1)
  
  if (!is.null(dots)) {
    if (plot) {
      cc <- if (is.numeric(cols))   Recall(cols) else cols
      cn <- if (is.character(cols)) Recall(cols) else cols
      m <- array(NA, n2mfrow(length(cols)))
      x <- c(col(m)[, rev(seq.int(ncol(m)))])[seq_along(cols)]
      y <- c(row(m))[seq_along(cols)]
      plot(y, x, pch = 16, cex = 3, col = cc,
           axes = FALSE, ann = FALSE, xpd = NA)
      text(y, x, pos = 3, col = 1, xpd = NA, labels = cn)
      text(y, x, pos = 1, col = 1, xpd = NA, labels = cc)
    }
    return(cols)
  }
  
  ## default plot of all colors with indices
  par(mfrow = c(1,1), mar = c(2,3,4,3), cex = .7)
  suppressWarnings({
    cc <- matrix(colors(), 30L)
    cc[duplicated(c(cc))] <- NA
  })
  
  w <- waffle(cc, border = 0, xpad = 0, reset_par = FALSE)
  title(main = 'col = colors()[n]', line = 2)
  
  ## left/right axes: 1, 2, ..., 30
  text(unique(w$centers[, 'x']),  0, 0:21 * 30, xpd = NA, pos = 1)
  text(unique(w$centers[, 'x']), 30, 0:21 * 30, xpd = NA, pos = 3)
  
  ## top/bottom axes: 0, 30, ..., 630
  axis(2, 1:30 - 0.5, 1:30, lwd = 0, las = 1)
  axis(4, 1:30 - 0.5, 1:30, lwd = 0, las = 1)
  
  invisible(w)
}

#' Show plotting characters
#' 
#' In \code{R}, there are 26 numeric plotting characters. This function shows
#' these options and their respective numbers. Note that \code{col} specifies
#' both the border and fill color (if applicable) for \code{0:20}; \code{pch}s
#' \code{21:25} can be filled with \code{bg}.
#' 
#' @param ... ignored
#' 
#' @seealso
#' \code{\link{show_colors}}; \code{\link{pch}}
#' 
#' @examples
#' show_pch()
#' 
#' @export

show_pch <- function(...) {
  op <- par(xpd = NA, mar = c(1,1,1,2))
  on.exit(par(op))
  
  x <- rep(1:5, 6)[1:26]
  y <- c(rep(5:1, each = 5)[1:25], 0)
  
  plot(x, y, pch = 0:25, axes = FALSE, bg = 'gray', cex = 2, col = 'red')
  text(x = x, y = y, labels = 0:25, pos = 4, cex = 1.5, offset = 1)
  text(x = 4, y = 0, labels = 'plotting characters 0:25', cex = 1.5)
  
  invisible(NULL)
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
#' @param colors a vector of color names as character strings (or hexadecimal
#' strings) or integers corresponding to colors in the current
#' \code{\link{palette}}
#' @param trans alpha transparency defined as an integer in the range
#' \code{[0, 255]} where \code{0} is fully transparent and \code{255} is fully
#' visible; see details
#' @param alpha the alpha transparency in \code{[0,1]}; \code{trans} is
#' ignored if \code{alpha} is given
#' 
#' @seealso
#' \code{\link{as.hexmode}}, \code{\link{col2rgb}}, \code{\link{adjustcolor}}
#' 
#' @examples
#' cols <- c('red', 'green', 'blue')
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
#' plot(rnorm(100), col = tcol(cols, c(50, 100, 255)), pch = 16, cex = 4)
#' 
#' @export

tcol <- function(colors, trans = NULL, alpha = NULL) {
  trans <- trans %||% 255L
  stopifnot(
    trans %inside% c(0L, 255L) | is.na(trans)
  )
  
  ## convert alpha to trans
  if (!is.null(alpha)) {
    stopifnot(
      alpha %inside% c(0, 1) | is.na(alpha)
    )
    trans <- as.integer(rescaler(alpha, to = c(0, 255), from = c(0, 1)))
  }
  
  ## get color and trans to conformable lengths
  if (length(colors) != length(trans) & 
      !any(c(length(colors), length(trans)) == 1L))
    stop('Vector lengths are not conformable')
  if (length(colors) == 1L & length(trans) > 1L)
    colors <- rep_len(colors, length(trans))
  if (length(trans) == 1L & length(colors) > 1L)
    trans <- rep_len(trans, length(colors))
  
  ## if color == 0, returns NA
  if (length(nocol <- which(colors == 0))) {
    colors[nocol] <- 1
    trans[nocol] <- NA
  }
  
  res <- paste0('#', apply(apply(rbind(col2rgb(colors)), 2L, function(x)
    format(as.hexmode(x), width = 2L)), 2L, paste, collapse = ''))
  res <- Map(paste0, res, tryCatch(
    as.character(as.hexmode(trans)),
    error = function(e) '', warning = function(w) ''
  ))
  res <- unname(unlist(res))
  
  ## return NAs and/or set color to transparent
  res[is.na(colors) | is.na(trans)] <- NA
  res[colors %in% 'transparent'] <- 'transparent'
  
  res
}

#' Print scientific numbers
#' 
#' Functions to parse numeric vectors in scientific notation and return an
#' \code{\link{expression}} for a pretty display.
#' 
#' @param x a numeric vector
#' @param digits integer indicating the number of decimal places to be used
#' @param base a positive or complex number: the base with respect to which
#' logarithms are computed (default is 10)
#' @param limit a numeric value whose order of magnitude will set a limit
#' beyond which values of \code{x} will be displayed in scientific notation;
#' default is to display numbers beyond a magnitude of \code{base^3}; to
#' display scientific notation always, use a negative value
#' @param simplify logical; if \code{TRUE} (default), removes "1 x" from
#' scientific format
#' 
#' @seealso
#' \code{\link[sfsmisc]{pretty10exp}}; \code{\link{roundr}};
#' \code{\link{format}}; \code{\link{sprintf}}, \code{\link{rawr_parse}}
#' 
#' @return
#' For \code{oom} an integer vector of magnitudes. For \code{parse_sci} an
#' expression of values in scientific notation. For \code{pretty_sci} an
#' expression of values in standard or scientific notation or combination
#' depending on the value of \code{limit}.
#' 
#' @examples
#' x <- 10 ** (1:5) / 10
#' oom(x)
#' oom(1 / x)
#' 
#' parse_sci(x)
#' parse_sci(x, simplify = FALSE)
#' parse_sci(x, base = 100)
#' parse_sci(1.1 * 2 ** (1:5), 1, 2)
#' 
#' par(xpd = NA, mar = c(6,4,4,2) + .1)
#' plot(1:5, type = 'n', axes = FALSE, ann = FALSE)
#' axis(2, at = 1:5, labels = pretty_sci(x, simplify = FALSE), las = 1)
#' 
#' text(1:5, 0, pretty_sci(1 / x ** 10))
#' text(1:5, 1, pretty_sci(1 / x, digits = 3))
#' text(1:5, 2, pretty_sci(1 / x, digits = 2, limit = 1e2))
#' text(1:5, 3, x)
#' text(1:5, 4, pretty_sci(x, limit = 1e2))
#' text(1:5, 5, pretty_sci(x, digits = 1))
#' text(1:5, 6, pretty_sci(x ** 10))
#' 
#' text(1:5, -1, pretty_sci(1 / x, limit = -1, simplify = FALSE))
#' 
#' @export

pretty_sci <- function(x, digits = 0, base = 10,
                       limit = base ** 3, simplify = TRUE) {
  l <- as.list(x)
  limit <- if (limit < 0)
    -1 else oom(limit, base)
  om <- sapply(l, oom, base)
  
  sapply(seq_along(l), function(y)
    if (abs(om[y]) > limit)
      parse_sci(l[[y]], digits, base, simplify)
    else roundr(l[[y]], digits))
}

#' @rdname pretty_sci
#' @export
oom <- function(x, base = 10) {
  as.integer(ifelse(x == 0, 0L, floor(log(abs(x), base))))
}

#' @rdname pretty_sci
#' @export
parse_sci <- function(x, digits = 0, base = 10, simplify = TRUE) {
  stopifnot(is.numeric(x))
  
  x <- to_sci_(x, digits, base)
  x <- strsplit(x, 'e[+]?[0]?')
  
  xbg <- sapply(x, function(y) roundr(as.numeric(y[[1]]), digits))
  xsm <- sapply(x, '[[', 2)
  txt <- do.call('sprintf', list(fmt = '"%s"%%*%%%s^%s', xbg, base, xsm))
  
  parse(text = if (simplify)
    gsub('\\"1\\"%\\*%\\s*', '', txt) else txt)
}

to_sci_ <- function(x, digits, base) {
  ## generalized format(x, scientific = TRUE)'er
  # base <- 2; digits = 1; x <- 1.1 * base ** (1:5)
  # rawr:::to_sci_(x, 1, base)
  stopifnot(is.numeric(x))
  xbg <- roundr(x / base ** oom(x, base), digits)
  xsm <- formatC(oom(x, base), width = 2, flag = 0)
  sprintf('%se+%s', xbg, xsm)
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
#' Original: Andreas Ruckstuhl, 19 May 1994; Cosmetic: Martin Machler, June
#' 1998; Modifications: Robert Redd
#' 
#' @examples
#' plot.new()
#' plot.window(c(-pi, pi), c(-1,1), asp = 1)
#' curve(sin(x), -pi, pi, add = TRUE)
#' xx <- seq(-pi, pi, length.out = 5)
#' 
#' ## arrows point to locations
#' arrows2(xx, sin(xx), xx + 0, sin(xx + 0))
#' 
#' ## arrows "follow" along curve
#' arrows2(xx, sin(xx), xx + .1, sin(xx + .1), col = 5, border = 2)
#' 
#' arrows2(-3,-1,3,1, code = 3, border = 2, fill = 0)
#' arrows2(-2, -1, -2, 1, code = 1, fill = 4, col = 4)
#' arrows2(-1, -1, -1, 1, code = 2, fill = 2, size = .5, width = .5)
#' arrows2(0, -1, 0, 1, code = 3, curve = 1.5)
#' arrows2(1, -1, 1, 1, code = 3, curve = .5, width = .5, lwd = 10, col = 3)
#' arrows2(2, -1, 2, 1, code = 3, lwd = 0)
#' 
#' @export

arrows2 <- function(x0, y0, x1 = x0, y1 = y0, size = 1, width = 0.1 / cin,
                    curve = 1, code = 2, col = par('fg'), lty = par('lty'),
                    lwd = par('lwd'), fill = col, border = fill,
                    sadj = c(0,0,0,0), ...) {
  stopifnot(length(code) == 1L, code %in% 0:3)
  
  ## create coordinates of a polygon for a unit arrow head
  cin <- size * par('cin')[2L]
  uin <- 1 / xyinch()
  x <- sqrt(seq(0, cin ** 2, length.out = 1000L))
  delta <- 0.005 / 2.54
  wx2 <- width * x ** curve
  
  ## polar, NA to "break" long polygon
  pol <- c2p(c(-x, -rev(x)), c(-wx2 - delta, rev(wx2) + delta))
  ## rawr:::c2p
  deg <- c(pol$theta, NA)
  rad <- c(pol$radius, NA)
  
  segments(x0 + sadj[1L], y0 + sadj[2L], x1 + sadj[3L], y1 + sadj[4L],
           col = col, lty = lty, lwd = lwd, lend = 1, xpd = NA, ...)
  
  if (code == 0L)
    return(invisible(NULL))
  
  if (code %in% 2:3) {
    theta <- atan2((y1 - y0) * uin[2L], (x1 - x0) * uin[1L])
    lx  <- length(x0)
    Rep <- rep.int(length(deg), lx)
    xx  <- rep.int(x1, Rep)
    yy  <- rep.int(y1, Rep)
    theta <- rep.int(theta, Rep) + rep.int(deg, lx)
    rad <- rep.int(rad, lx)
    polygon(xx + rad * cos(theta) / uin[1L], yy + rad * sin(theta) / uin[2L],
            col = fill, xpd = NA, border = border)
  }
  
  if (code %in% c(1L, 3L)) {
    arrows2(x1, y1, x0, y0, size, width, code = 2, curve, col = col,
            lty = 0, lwd = 0, fill = fill, border = border, ...)
  }
  
  invisible(NULL)
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
#' 
#' carrows(p1 <- p[2, ], p2 <- p[1, ], pad = .3)
#' carrows(p1 <- p[4, ], p2 <- p[5, ], dir = c(0, 1), col = 3)
#' carrows(p1 <- p[6, ], p2 <- p[3, ], lwd = 10, pad = c(.05, .1))
#' carrows(p1 <- p[1, ], p2 <- p[6, ], flip = TRUE)
#' carrows(p1 <- p[1, ], p2 <- p[5, ], dir = c(1, 0))
#' 
#' @export

carrows <- function(p1, p2, arc, degree = FALSE, pad = 0.01 * 1:2,
                    flip = FALSE, dir = NULL,
                    ## lines
                    col = par('col'), lwd = par('lwd'), lty = par('lty'),
                    ## arrows2
                    size = 1, width = size / 2, curve = 1, fill = col,
                    border = NA) {
  code_ <- function(x) c(2L, 0L, 1L)[match(x, -1:1)]
  pad_  <- function(x, pad) rawr::ht(x, -length(x) * (1 - pad))
  
  ## try to guess code for arrows2
  slope <- (p2[2L] - p1[2L]) / (p2[1L] - p1[1L])
  slope[!is.finite(slope) | slope == 0] <- 1
  code  <- code_(sign(slope))
  
  ## calculate arc based on p1, p2
  radius  <- sqrt(sum((p1 - p2) ** 2)) / 2
  centers <- (p1 + p2) / 2
  
  arc <- if (!missing(arc)) {
    if (degree | any(arc > 2 * pi))
      d2r(arc) else arc[1:2]
  } else sapply(list(p1, p2), function(x)
    p2r(x[1L], x[2L], centers[1L], centers[2L]))
  
  ## convert polar to cart and plot lines/arrows
  theta <- seq(arc[1L], arc[2L], length.out = 500L) + if (flip) pi else 0
  pad <- rep_len(pad, 2L)
  th <- pad_(theta, pad[1L])
  xx <- centers[1L] + radius * cos(th)
  yy <- centers[2L] + radius * sin(th)
  lines(pad_(xx, pad[2L]), pad_(yy, pad[2L]), col = col, lwd = lwd, lty = lty)
  
  xx <- ht(xx, 4L)
  yy <- ht(yy, 4L)
  arrows2(xx[1L], yy[1L], xx[2L], yy[2L], size = size, width = width,
          curve = curve, code = dir[1L] %||% code, col = col, lty = 0,
          lwd = 0, fill = fill, border = border)
  arrows2(xx[4L], yy[4L], xx[3L], yy[3L], size = size, width = width,
          curve = curve, code = dir[2L] %||% code, col = col, lty = 0,
          lwd = 0, fill = fill, border = border)
  
  invisible(list(arc = arc, centers = centers, radius = radius))
}

#' Add a logarithmic axis
#' 
#' Draw minor ticks on a log scale axis.
#' 
#' @param side an integer specifying which side of the plot the axis is to be
#' drawn on: 1=below, 2=left, 3=above, 4=right
#' @param nticks number of minor ticks between each pair of major ticks
#' @param labels logical; if \code{TRUE}, major ticks will be labeled using
#' \code{\link{pretty_sci}}
#' @param digits,base,limit,simplify additional arguments passed to
#' \code{\link{pretty_sci}}
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @return
#' A list with elements \code{at.major} and \code{at.minor} giving the points
#' at which tick marks were drawn for the major and minor axes, respectively.
#' 
#' @seealso
#' \code{\link{pretty_sci}}; \code{\link[sfsmisc]{axTexpr}}; \code{\link{axis}}
#' 
#' @examples
#' x <- 1:10
#' y <- function(base) base ** x
#' op <- par(mar = c(3,5,3,5), las = 1)
#' plot(x, log(y(2), 2), ann = FALSE, axes = FALSE)
#' laxis(2, base = 2, limit = -1)
#' 
#' par(new = TRUE)
#' plot(x, y(10), log = 'y', axes = FALSE, ann = FALSE)
#' laxis(4, nticks = 10, tcl = .5, col.axis = 2)
#' 
#' par(new = TRUE)
#' plot(x, x, log = 'x', axes = FALSE, ann = FALSE, xpd = NA)
#' laxis(1, nticks = 10, tcl = -1, col.axis = 1, lwd = 2)
#' abline(v = x)
#' 
#' par(op)
#' 
#' @export

laxis <- function(side = 1L, nticks = 5, labels = TRUE, digits = 0, base = 10,
                  limit = base ** 3, simplify = TRUE, ...) {
  ap <- par(switch(side, 'xaxp', 'yaxp', 'xaxp', 'yaxp', stop('Invalid axis')))
  yl <- c(-1, 1) + if (base == 10) log10(ap[-3L]) else c(1, ap[2L])
  pp <- seq(yl[1L], yl[2L])
  
  at0 <- at1 <- base ** pp
  at2 <- c(sapply(pp, function(x)
    seq(1, base, length.out = nticks) * base ** x))
  if (base != 10) {
    at1 <- log(at1, base)
    at2 <- log(at2, base)
  }
  
  op <- par(..., no.readonly = TRUE)
  on.exit(par(op))
  
  axis(side, at1, lwd = par('lwd'), if (labels)
    pretty_sci(at0, digits, base, limit, simplify) else FALSE)
  axis(side, at2, FALSE, tcl = par('tcl') * 0.5, lwd = 0,
       lwd.ticks = par('lwd'))
  
  invisible(list(at.major = at1, at.minor = at2))
}

#' Plotting coordinates
#' 
#' Return the user plot, figure, inner, and device \emph{{x,y}} coordinates
#' for a vector of normalized (i.e., in \code{[0,1]}) coordinates. Or, if
#' \code{line} and \code{side} are given, the x (or y) user coordinates.
#' 
#' @param x,y normalized x- and y-coordinates in \code{[0,1]}, recycled as
#' needed
#' @param to character string giving the coordinate system to convert to
#' @param line,side the margin line starting at 0 counting outwards and side
#' of the plot (1=below, 2=left, 3=above, 4=right); see \code{\link{mtext}}
#' 
#' @seealso
#' \code{\link[=grconvertX]{convertXY}}; \code{\link{mtext}}
#' 
#' @examples
#' op <- par(oma = 1:4, mar = 1:4, xpd = NA, pch = 16, xpd = NA)
#' plot.new()
#' box('plot', col = 1)
#' box('figure', col = 2)
#' box('outer', col = 3)
#' # box('inner', col = 4)
#' 
#' xx <- c(1,2,1,2)
#' yy <- c(1,1,2,2)
#' 
#' co <- coords()
#' 
#' points(co$plot$x[xx], co$plot$y[yy], cex = 5, col = 1)
#' points(co$figure$x[xx], co$figure$y[yy], cex = 5, col = 2)
#' points(co$device$x[xx], co$device$y[yy], cex = 5, col = 3)
#' 
#' 
#' co <- coords(seq(0, 1, 0.1), 1)
#' 
#' points(co$plot$x, co$plot$y, cex = 2, col = 4)
#' points(co$figure$x, co$figure$y, cex = 2, col = 5)
#' points(co$device$x, co$device$y, cex = 2, col = 6)
#' 
#' 
#' ## use line/side for x or y coordinates depending on side
#' mtext('text', line = 1, side = 3, at = 0.5)
#' text(0.5, coords(line = 1, side = 3), 'text', col = 2)
#' 
#' mtext('text', line = -1:4, side = 4, at = 0.5)
#' text(coords(line = -1:4, side = 4), 0.5, 'text', col = 2, srt = 90)
#' 
#' par(op)
#' 
#' @export

coords <- function(x = 0:1, y = x, to = 'user', line, side) {
  xy <- cbind(x, y)
  x  <- xy[, 1L]
  y  <- xy[, 2L]
  
  if (!missing(line) | !missing(side)) {
    lh <- par('cin')[2L] * par('cex') * par('lheight')
    
    sapply(line, function(li) {
      li <- li + 0.5
      x  <- diff(grconvertX(x, 'in', 'user')) * lh * li
      y  <- diff(grconvertY(y, 'in', 'user')) * lh * li
      
      (par('usr')[c(3, 1, 4, 2)] + c(-y, -x, y, x))[match(side, 1:4)]
    })
  } else
    list(
      plot   = list(x = grconvertX(x, 'npc', to), y = grconvertY(y, 'npc', to)),
      figure = list(x = grconvertX(x, 'nfc', to), y = grconvertY(y, 'nfc', to)),
      inner  = list(x = grconvertX(x, 'nic', to), y = grconvertY(y, 'nic', to)),
      device = list(x = grconvertX(x, 'ndc', to), y = grconvertY(y, 'ndc', to))
    )
}

#' Color scaling
#' 
#' Color scaling and interpolation. For a numeric vector and a single color,
#' gradations of transparency is applied corresponding to each numeric value.
#' For two or more, color interpolation is applied.
#' 
#' @param x a numeric or integer vector
#' @param colors a vector of color names as character strings (or
#' hexadecimal strings) or integers corresponding to colors in the current
#' \code{\link{palette}}; or a function taking an integer argument that
#' returns a vector of colors (e.g., \code{\link{colorRampPalette}} or
#' \code{\link{rainbow}})
#' 
#' if only one color is given, the scaled value of \code{x} will determine
#' the amount of transparency (default is from 0, fully-transparent to 1-
#' fully opaque)
#' @param alpha transparency applied to interpolated colors (i.e., if
#' \code{colors} is not a single color)
#' @param alpha.min if a single color name is given, sets the lower bound of
#' alpha; a value greater than 0 ensures that the color is visible even for
#' the smallest value of \code{x} after rescaling
#' @param to,from output and input range, respectively; see
#' \code{\link{rescaler}}
#' 
#' @return
#' A character vector having the same length as \code{x} of hexadecimal color
#' values.
#' 
#' @examples
#' ## basic usage
#' col_scaler(mtcars$mpg, 'red')
#' col_scaler(mtcars$vs, c('red', 'black'))
#' 
#' 
#' set.seed(1)
#' x <- sort(runif(50, 0, 2))
#' p <- function(y, c) {
#'   points(seq_along(c), rep_len(y, length(c)),
#'          col = c, pch = 16, cex = 5, xpd = NA)
#' }
#' 
#' plot.new()
#' plot.window(c(0, 50), c(-3, 3))
#' p( 4, col_scaler(x, 'red'))
#' p( 3, col_scaler(x, c('red', 'blue')))
#' p( 2, col_scaler(x, c('red', 'blue'), to = c(.4, .8)))
#' p( 1, col_scaler(round(x), c('red', 'blue'), alpha = 0.5))
#' p( 0, col_scaler(x, 1:10))
#' p(-1, col_scaler(round(x), 1:3))
#' p(-2, col_scaler(x, 'heat.colors'))
#' p(-3, col_scaler(x, rainbow, alpha = 0.1))
#' p(-4, col_scaler(x, colorRampPalette(c('tomato', 'white', 'blue4'))))
#' 
#' @export

col_scaler <- function(x, colors, alpha = 1,
                       alpha.min = min(0.1, x[x >= 0], na.rm = TRUE),
                       to = c(0, 1), from = range(x, na.rm = TRUE)) {
  pals <- c('rainbow', paste0(c('heat', 'terrain', 'topo', 'cm'), '.colors'))
  colors <- if (is.numeric(colors))
    rep_len(palette(), max(colors, na.rm = TRUE))[as.integer(colors)]
  else if (inherits(colors, 'function'))
    colors
  else if (colors[1L] %in% pals)
    get(colors, mode = 'function')
  else as.character(colors)
  
  x <- if (is.factor(x) || is.character(x) || is.integer(x))
    as.integer(as.factor(x)) else as.numeric(x)
  
  ## add alpha
  if (is.character(colors) & length(colors) == 1L)
    return(tcol(colors, alpha = rescaler(x, c(alpha.min, to[2L]), from)))
  
  ## use interpolation
  n  <- 10000L
  to <- to * n
  x  <- rescaler(x, to, from)
  x  <- as.integer(x) + 1L
  
  colors <- if (inherits(colors, 'function'))
    colors(n + 1L)[x]
  else colorRampPalette(colors)(n + 1L)[x]
  
  if (!all(alpha == 1))
    tcol(colors, alpha = rep_len(alpha, length(colors)))
  else tolower(colors)
}
