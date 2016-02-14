### plot extra
# dodge, show_colors, show_pch, tcol, pretty_sci, oom, parse_sci
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
