### plot extra
# dodge, show_colors, show_pch, tcol
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
#' \code{formula}; by default, variables are taken from 
#' \code{environment(formula)}
#' @param x a numeric vector of data
#' @param at grouping variables or, equivalently, positions along x-axis
#' @param jit,dist jittering parameters; \code{jit} describes the spread of
#' close points, and \code{dist} defines a range of data to declare points
#' "close"
#' @param ... ignored
#' 
#' @seealso
#' \code{\link{jitter}}, \code{\link{tplot}}
#' 
#' @examples
#' ## these are equivalent ways to call dodge:
#' dodge(mpg ~ gear + vs, mtcars)
#' with(mtcars, dodge(mpg, list(gear, vs)))
#' dodge(mtcars$mpg, mtcars[, c('gear', 'vs')])
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
#'   points(jitter(rep(ii, length(sp[[ii]]))), sp[[ii]], col = 4, pch = 2)
#' points(dodge(mpg ~ gear + vs, mtcars), col = 2, pch = 4)
#' legend('topleft', pch = c(1,1,4), col = c(1,4,2), cex = .8,
#'        legend = c('overlapping','random jitter','dodging'))
#' par(op)
#' 
#' 
#' ## practical use
#' boxplot(mpg ~ vs + gear, data = mtcars)
#' points(dodge(mpg ~ vs + gear, data = mtcars), col = 'red', pch = 19)
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
  dodge(mf[, response], mf[, -response])
}

#' @rdname dodge
#' @export
dodge.data.frame <- function(x, ...) {
  dodge(x[, 2], x[, 1], ...)
}

#' @rdname dodge
#' @export
dodge.default <- function(x, at, dist, jit, ...) {
  at <- if (is.list(at)) as.numeric(do.call('interaction', at)) else
    rep_len(if (missing(at)) 1 else at, length(x))
  if (missing(dist) || is.na(dist) || is.null(dist)) 
    dist <- diff(range(x)) / 100
  if (missing(jit) || is.na(jit) || is.null(jit)) 
    jit <- 0.1
  ## call dodge on each group
  sp <- if (length(unique(at)) > 1) split(x, at) else list(x)
  l <- lapply(seq_along(sp), function(xx) {
    ss <- sp[xx]
    dodge_(ss[[1]], rep_len(as.numeric(names(ss)), length(ss[[1]])), dist, jit)
  })
  do.call('rbind', l)
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
