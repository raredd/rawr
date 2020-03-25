### plot misc, extra, random
# dodge, dodge.default, dodge.formula, dodge2, dodge2.default, dodge2.formula,
# show_colors, show_pch, tcol, col_scaler, bp.test, bp.test.default,
# bp.test.formula
# 
# S3 methods:
# dodge, dodge2, bp.test
# 
# unexported:
# col_scaler2
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
#' close points, and \code{dist} defines a range to consider points "close";
#' both may be specified for each group and recycled as needed
#' @param ... additional arguments passed to or from other methods
#' 
#' @seealso
#' \code{\link{jitter}}; \code{\link{tplot}}; \code{\link{dodge2}}
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

dodge <- function(x, ...) {
  UseMethod('dodge')
}

#' @rdname dodge
#' @export
dodge.default <- function(x, y, dist = NULL, jit = NULL, ...) {
  if (is.data.frame(y)) {
    x <- y[, 2L]
    y <- y[, 1L]
  }
  
  x <- if (!missing(x) && is.list(x))
    as.numeric(do.call('interaction', x)) else
      rep_len(if (missing(x)) 1L else x, length(x))
  
  ng <- length(unique(x))
  
  if (is.null(dist) || is.na(dist))
    dist <- diff(range(x, na.rm = TRUE)) / 100
  dist <- rep_len(dist, ng)[x]
  
  if (is.null(jit) || is.na(jit))
    jit <- 0.1
  jit <- rep_len(jit, ng)[x]
  
  ## call dodge on each group
  list(
    x = ave(seq_along(y), x, FUN = function(ii)
      dodge_(y[ii], x[ii], unique(dist[ii]), unique(jit[ii]))$x),
    y = y
  )
}

#' @rdname dodge
#' @export
dodge.formula <- function(formula, data = NULL, ...) {
  if (missing(formula) || (length(formula) != 3L))
    stop('\'formula\' missing or incorrect')
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame(1L))))
    m$data <- as.data.frame(data)
  m$`...` <- NULL
  m[[1L]] <- as.name('model.frame')
  
  mf <- eval(m, parent.frame(1L))
  response <- attr(attr(mf, 'terms'), 'response')
  
  dodge(mf[, -response], mf[, response], ...)
}

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
#' close points, and \code{dist} defines a range to consider points "close";
#' both may be specified for each group and recycled as needed
#' @param ... additional arguments passed to or from other methods
#' 
#' @seealso
#' \code{\link{jitter}}; \code{\link{tplot}}; \code{\link{dodge}}
#' 
#' @examples
#' ## these are equivalent ways to call dodge2:
#' dodge2(mpg ~ gear + vs, mtcars)
#' with(mtcars, dodge2(list(gear, vs), mpg))
#' dodge2(mtcars[, c('gear', 'vs')], mtcars$mpg)
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
#' points(dodge2(mpg ~ gear + vs, mtcars), col = 2, pch = 4)
#' legend('topleft', pch = c(1,1,4), col = c(1,4,2), cex = .8,
#'        legend = c('overlapping','random jitter','dodging'))
#' par(op)
#' 
#' 
#' ## practical use
#' boxplot(mpg ~ vs + gear, data = mtcars)
#' points(dodge2(mpg ~ vs + gear, data = mtcars), col = 2, pch = 19)
#'
#' @export

dodge2 <- function(x, ...) {
  UseMethod('dodge2')
}

#' @rdname dodge2
#' @export
dodge2.default <- function(x, y, jit = NULL, dist = NULL, ...) {
  if (is.data.frame(y)) {
    x <- y[, 2L]
    y <- y[, 1L]
  }
  
  x <- if (!missing(x) && is.list(x))
    as.numeric(do.call('interaction', x)) else
      rep_len(if (missing(x)) 1L else x, length(x))
  
  sp <- split(y, x)
  at <- seq_along(sp)
  ng <- length(at)
  
  if (is.null(dist))
    dist <- diff(range(y, na.rm = TRUE)) / 100
  dist <- rep_len(dist, ng)
  
  if (is.null(jit))
    jit <- 1 / max(lengths(sp))
  jit <- rep_len(jit, ng)
  
  # gr <- lapply(sp, grouping_, dif = dist)
  gr <- Map(grouping_, sp, dist)
  gr <- lapply(seq_along(gr), function(ii) {
    gi <- gr[[ii]]
    aa <- at[ii]
    gi$x <- rep(aa, nrow(gi)) + jit_(gi$g.si, gi$hmsf) * jit[ii]
    gi
  })
  
  list(
    x = unlist(lapply(gr, '[[', 'x')),
    y = unlist(sp)
  )
}

#' @rdname dodge2
#' @export
dodge2.formula <- function(formula, data = NULL, ...) {
  if (missing(formula) || (length(formula) != 3L))
    stop('\'formula\' missing or incorrect')
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame(1L))))
    m$data <- as.data.frame(data)
  m$`...` <- NULL
  m[[1L]] <- as.name('model.frame')
  
  mf <- eval(m, parent.frame(1L))
  response <- attr(attr(mf, 'terms'), 'response')
  
  dodge2(mf[, -response], mf[, response], ...)
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
      plot(y, x, pch = 16L, cex = 3, col = cc,
           axes = FALSE, ann = FALSE, xpd = NA)
      text(y, x, pos = 3L, col = 1, xpd = NA, labels = cn)
      text(y, x, pos = 1L, col = 1, xpd = NA, labels = cc)
    }
    return(cols)
  }
  
  ## default plot of all colors with indices
  par(mfrow = c(1,1), mar = c(2,3,4,3), cex = 0.7)
  suppressWarnings({
    cc <- matrix(colors(), 30L)
    cc[duplicated(c(cc))] <- NA
  })
  
  w <- waffle(cc, border = 0, xpad = 0, reset_par = FALSE)
  title(main = 'col = colors()[n]', line = 2)
  
  ## left/right axes: 1, 2, ..., 30
  text(unique(w$centers[, 'x']),  0, 0:21 * 30, xpd = NA, pos = 1L)
  text(unique(w$centers[, 'x']), 30, 0:21 * 30, xpd = NA, pos = 3L)
  
  ## top/bottom axes: 0, 30, ..., 630
  axis(2, 1:30 - 0.5, 1:30, lwd = 0, las = 1L)
  axis(4, 1:30 - 0.5, 1:30, lwd = 0, las = 1L)
  
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
  
  x <- rep(1:5, 6L)[1:26]
  y <- c(rep(5:1, each = 5L)[1:25], 0L)
  
  plot(
    x, y, pch = 0:25, axes = FALSE, ann = FALSE,
    bg = 'gray', cex = 2, col = 'red'
  )
  text(x, y, 0:25, pos = 4L, cex = 1.5, offset = 1)
  text(par('usr')[2L], 0, 'plotting characters 0:25', cex = 1.5, adj = 1)
  
  invisible(NULL)
}

#' Transparent colors
#' 
#' Add alpha transparency to colors.
#' 
#' @param col a vector of color names, hexadecimal strings, or integers which
#' correspond to the current \code{\link{palette}}
#' @param alpha the alpha transparency in \code{[0,1]}
#' 
#' @seealso
#' \code{\link{as.hexmode}}; \code{\link{col2rgb}}; \code{\link{adjustcolor}};
#' \code{\link{rgb}};
#' 
#' @examples
#' cols <- c('red', 'green', 'blue')
#' 
#' ## a normal plot
#' plot(rnorm(100), col = tcol(cols), pch = 16, cex = 4)
#' 
#' ## more transparent
#' plot(x <- rnorm(100), col = tcol(cols, 0.5), pch = 16, cex = 4)
#' 
#' ## hexadecimal strings also work
#' cols <- c('#FF0000', '#00FF00', '#0000FF')
#' plot(rnorm(100), col = tcol(cols, c(0.2, 0.4, 1)), pch = 16, cex = 4)
#' 
#' @export

tcol <- function(col, alpha = 1) {
  col <- replace(col, col %in% 0, NA)
  dat <- data.frame(col = col, alpha = alpha, stringsAsFactors = FALSE)
  nas <- !complete.cases(dat)
  dat$alpha[is.na(dat$alpha)] <- 0
  
  x <- t(col2rgb(dat$col))
  x <- rgb(x, alpha = dat$alpha * 255, maxColorValue = 255)
  
  x <- replace(x, nas, NA)
  tolower(replace(x, dat$col %in% 'transparent', 'transparent'))
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
#' @param na.color color used for \code{NA} values of \code{x}
#' @param breaks (optional) numeric vector to center interpolation; if
#' \code{NULL} (default), \code{colors} are uniformly spread over a continuous
#' \code{x}; useful if \code{colors} should be centered at a specific value
#' of \code{x}; see examples
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
#' # x <- replace(x, runif(length(x)) > 0.75, NA)
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
#' 
#' op <- par(no.readonly = TRUE)
#' set.seed(1)
#' x <- runif(1000)
#' y <- c('red', 'black', 'red')
#' par(mfrow = c(2, 2), mar = c(3,3,1,1))
#' plot(x, col = col_scaler(x, y), pch = 16)
#' plot(x, col = col_scaler(x, y, breaks = 0.5), pch = 16)
#' plot(x, col = col_scaler(x, y, breaks = 0.9), pch = 16)
#' plot(x, col = col_scaler(x, c(y, 'blue'), breaks = c(0.25, 0.75)), pch = 16)
#' par(op)
#' 
#' @export

col_scaler <- function(x, colors, na.color = NA, breaks = NULL, alpha = 1,
                       alpha.min = min(0.1, x[x >= 0], na.rm = TRUE),
                       to = c(0, 1), from = range(x, na.rm = TRUE)) {
  if (!is.null(breaks))
    return(
      col_scaler2(x, colors, breaks, na.color = na.color, alpha = alpha,
                  alpha.min = alpha.min)
    )
  
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
  na <- is.na(x)
  
  ## add alpha
  if (is.character(colors) & length(colors) == 1L) {
    res <- tcol(colors, alpha = rescaler(x, c(alpha.min, to[2L]), from))
    return(replace(res, na, na.color))
  }
  
  ## use interpolation
  n  <- 10000L
  to <- to * n
  x  <- rescaler(x, to, from)
  x  <- as.integer(x) + 1L
  
  colors <- if (inherits(colors, 'function'))
    colors(n + 1L)[x]
  else colorRampPalette(colors)(n + 1L)[x]
  
  res <- if (!all(alpha == 1))
    tcol(colors, alpha = rep_len(alpha, length(colors)))
  else tolower(colors)
  
  replace(res, na, na.color)
}

col_scaler2 <- function(x, colors, breaks = 0, ...) {
  stopifnot(
    length(colors) == length(breaks) + 2L,
    all(breaks %inside% range(x, na.rm = TRUE))
  )
  
  res <- character(length(x))
  idx <- cut(x, c(-Inf, breaks, Inf))
  udx <- seq_along(levels(idx))
  
  for (ui in udx) {
    ii <- as.integer(idx) %in% ui
    res[ii] <- col_scaler(x[ii], colors[ui + 0:1], breaks = NULL, ...)
  }
  
  res
}

#' Box plot tests
#' 
#' Add pairwise tests to box plots with three or more groups. Using
#' \code{\link{cuzick.test}} under-the-hood, performs \code{test} for all
#' pairs of groups and draws results on an existing plot.
#' 
#' @param x a vector of text to be drawn above each pair of groups
#' @param ... arguments passed to or from other methods or additional
#' graphical parameters passed to \code{link{par}}
#' @param formula a formula of the form \code{response ~ group} where
#' \code{response} is a numeric variable and \code{group} is a factor-like
#' variable with three or more unique values (groups)
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in \code{formula}; by
#' default, the variables are taken from \code{environment(formula)}
#' @param which an index vector of the values to display
#' @param at numeric vector giving the x-axis locations of each group having
#' the same length as \code{x} or the number of pairs
#' @param line lines at which to plot \code{test} results; if \code{NULL},
#' these will be calculated; if length 1, the calculated lines will be shifted
#' by \code{line}
#' @param test the test to use for pairwise comparisons
#' 
#' @return
#' A matrix of user coordinates where p-values are drawn.
#' 
#' @seealso
#' \code{\link{cuzick.test}}; \code{\link{boxplot}}; \code{\link{tplot}};
#' \code{rawr:::coords}
#' 
#' @examples
#' boxplot(mpg ~ gear, mtcars)
#' bp.test(mpg ~ gear, mtcars)
#' 
#' 
#' boxplot(mpg ~ gear, mtcars, at = c(1, 3, 4), ylim = c(10, 55))
#' bp.test(mpg ~ gear, mtcars, at = c(1, 3, 4), which = c(1, 3, 2), line = -5)
#' 
#' 
#' op <- par(mar = par('mar') + c(0, 0, 3, 0))
#' tplot(mpg ~ interaction(vs, am), mtcars, show.n = FALSE)
#' bp.test(mpg ~ interaction(vs, am), mtcars)
#' 
#' tplot(mpg ~ interaction(vs, am), mtcars, show.n = FALSE)
#' bp.test(mpg ~ interaction(vs, am), mtcars, which = c(1, 3, 5))
#' 
#' bp.test(mpg ~ interaction(vs, am), mtcars, which = 6, line = 4,
#'         col = 'red', fg = 'red', lty = 2, font = 2, test = t.test) -> at
#' points(at[1], at[2], pch = 1, cex = 5, col = 'red', xpd = NA)
#' par(op)
#' 
#' 
#' ## also works for barplots
#' bp <- barplot(with(mtcars, tapply(mpg, gear, mean)), ylim = c(0, 30))
#' bp.test(mpg ~ gear, mtcars, at = bp, line = -1, test = t.test)
#' 
#' 
#' ## use default method for more control
#' boxplot(mpg ~ am + vs, mtcars, axes = FALSE, ylim = c(10, 55))
#' axis(2, las = 1)
#' box(bty = 'l')
#' at <- bp.test(letters[1:6], at = 1:4, line = -7)
#' points(at, cex = 3, col = 1:6, xpd = NA)
#' 
#' @export

bp.test <- function(x, ...) {
  UseMethod('bp.test')
}

#' @rdname bp.test
#' @export
bp.test.formula <- function(formula, data, which = NULL, at = NULL, line = NULL,
                    test = wilcox.test, ...) {
  bp <- boxplot(formula, data, plot = FALSE)
  ng <- length(bp$n)
  if (ng == 1L) {
    message('only one group -- no test performed')
    return(invisible(NULL))
  }
  
  pv <- if (ng == 2L)
    test(formula, data)
  else cuzick.test(formula, data, details = test)$details$pairs
  pv <- pvalr(pv$p.value, show.p = TRUE)
  
  which <- if (is.null(which))
    seq_along(pv) else which[which %in% seq_along(pv)]
  at <- if (is.null(at))
    seq.int(ng) else at
  line <- if (is.null(line) || length(line) == 1L)
    1.25 * (seq_along(which) - 1) + line %||% 0 else line
  
  bp.test(pv, which, at, line, test, ...)
}

#' @rdname bp.test
#' @export
bp.test.default <- function(x, which = NULL, at = NULL, line = NULL,
                            test = wilcox.test, ...) {
  op <- par(..., no.readonly = TRUE)
  on.exit(par(op))
  
  ng <- length(x) + 1L
  if (ng == 1L) {
    message('only one group -- no test performed')
    return(invisible(NULL))
  }
  
  which <- if (is.null(which))
    seq_along(x) else which[which %in% seq_along(x)]
  at <- if (is.null(at))
    seq.int(ng) else at
  line <- if (is.null(line) || length(line) == 1L)
    1.25 * (seq_along(which) - 1) + line %||% 0 else line
  
  seg <- function(x1, y, x2) {
    pad <- diff(par('usr')[3:4]) / 100
    col <- par('fg')
    
    segments(x1, y, x2, y,       col = col, xpd = NA)
    segments(x1, y, x1, y - pad, col = col, xpd = NA)
    segments(x2, y, x2, y - pad, col = col, xpd = NA)
    
    c(x = x1 + (x2 - x1) / 2, y = y + pad * 3)
  }
  
  yat <- coords(line = line, side = 3L)
  cbn <- combn(at, 2L)
  
  res <- sapply(seq_along(which), function(ii) {
    xat <- cbn[, which[ii]]
    xat <- seg(xat[1L], yat[ii], xat[2L])
    text(xat[1L], xat[2L], x[which[ii]], xpd = NA)
    xat
  })
  
  invisible(t(res))
}
