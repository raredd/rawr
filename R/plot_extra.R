### plot misc, extra, random
# dodge, dodge.default, dodge.formula, dodge2, dodge2.default, dodge2.formula,
# show_colors, show_pch, tcol, col_scaler, bp.test, bp.test.default,
# bp.test.formula, imgpal, rawr_palettes, rawr_pal, show_pal
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

#' Image palettes
#' 
#' Extract unique and most commonly-used unique colors from an image file
#' (requires \href{https://imagemagick.org/index.php}{ImageMagick}).
#' 
#' @param path full file path to image
#' @param n maximum number of colors to extract, result will be <= \code{n},
#' and the calculated number of unique colors will also be provided
#' @param options a (optional) character string of additional options passed
#' to \href{https://www.imagemagick.org/script/command-line-options.php}{\code{convert}}
#' 
#' @return
#' A list of class \code{"imgpal"} with the following elements:
#' \item{filename}{the image file name}
#' \item{n_unique}{the calculated number of unique colors}
#' \item{col}{a vector of colors (does not return transparent or white colors)}
#' \item{counts}{frequency counts for each \code{col}}
#' \item{call}{the call made to \code{convert}}
#' \item{convert}{the result of \code{call}}
#' 
#' @seealso
#' \code{\link{show_pal}}
#' 
#' @examples
#' \dontrun{
#' go <- 'https://www.google.com/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png'
#' ip <- imgpal(go)
#' show_pal(ip)
#' 
#' ip <- imgpal(go, options = '-colorize 0,0,50')
#' show_pal(ip)
#' 
#' ## rawr::rawr_palettes
#' img <- system.file('fig', package = 'rawr')
#' img <- list.files(img, full.names = TRUE, pattern = 'g$')
#' op <- par(mfrow = n2mfrow(length(img)))
#' sapply(img, function(x) show_pal(imgpal(x), fullrange = TRUE))
#' par(op)
#' }
#' 
#' @export

imgpal <- function(path, n = 10L, options = '') {
  cmd <- sprintf(
    # https://www.imagemagick.org/script/command-line-options.php
    'convert %s +dither -colors %s -layers flatten %s \\
    -define histogram:unique-colors=true \\
    -format "%%f, n=%%k\n%%c\n" histogram:info:',
    path, n, options
  )
  co <- capture.output({
    res <- system(cmd, intern = TRUE)
  })
  if (!is.null(attr(res, 'status')))
    stop(res, call. = FALSE)
  
  res <- trimws(res[nzchar(res)])
  
  dat <- read.table(
    comment.char = '', stringsAsFactors = FALSE,
    text = gsub('\\s*(\\d+):.*(#\\S+).*', '\\1 \\2', res[-1L])
  )
  dat <- dat[order(dat[, 1L], decreasing = TRUE), ]
  
  ## remove fully transparent or white
  idx <- grepl('(?i)#(.{6}00|ffffff)', dat[, 2L])
  dat <- dat[!idx, ]
  
  res <- list(
    filename = gsub(', n.*', '', res[1L]),
    n_unique = type.convert(gsub('n=(\\d+)$|.', '\\1', res[1L])),
    col = gsub('(#.{6})|.', '\\1', dat[, 2L]), counts = dat[, 1L],
    call = cmd, convert = res
  )
  
  structure(res, class = 'imgpal')
}

#' rawr palettes
#' 
#' @param name the palette name to be used
#' @param n the first \code{n} colors from the palette to use
#' @param z for \code{type = 'continuous'}, the number of colors desired
#' @param type return a discrete or continuous (gradient) of colors
#' @param rev logical; if \code{TRUE}, the palette is reversed
#' @param x one of 1) a \code{rawr_palette} name; 2) a vector of two or more
#' colors; 3) an \code{\link{imgpal}} object
#' @param fullrange logical; for \code{\link{imgpal}} objects, if \code{TRUE},
#' the entire palette is shown; otherwise, only the calculated number of
#' unique colors
#' @param counts logical; for \code{\link{imgpal}} objects, if \code{TRUE},
#' the color frequencies are shown for each
#' 
#' @seealso
#' \code{\link{imgpal}}; \code{\link{palette}}; \code{\link{colorRampPalette}};
#' \code{wesanderson::wes_palettes}; \code{nord::nord_palettes}
#' 
#' 
#' @examples
#' ## some built-in palettes
#' rawr_palettes
#' 
#' ## use or generate new palettes from existing
#' p <- rawr_pal('dfci')
#' show_pal(p)
#' p <- rawr_pal('dfci', 4)
#' show_pal(p)
#' p <- rawr_pal('dfci', 4, 100, type = 'continuous')
#' show_pal(p)
#' 
#' ## view palettes from other sources
#' show_pal(nord::nord_palettes$afternoon_prarie)
#' show_pal(rainbow(8))
#' 
#' filled.contour(volcano, col = rawr_pal('dfci', 4, 21, type = 'c'))
#' filled.contour(volcano, col = rawr_pal('dfci', z = 21, type = 'c'))
#' filled.contour(volcano, col = rawr_pal('pokrie', 4, 21, type = 'c'))
#'
#' @export

# img <- system.file('fig', package = 'rawr')
# img <- list.files(img, full.names = TRUE, pattern = 'g$')
# pal <- sapply(img, function(x) imgpal(x)$col)
# names(pal) <- gsub('.*/|\\..*', '', names(pal))
# dput(pal)

rawr_palettes <- list(
  bidmc =
    c('#171F69', '#181661', '#D4D5E3', '#7B7CA7',
      '#A9AAC6', '#414380', '#5E6194'),
  dfci =
    c('#63666B', '#0F699B', '#3CC5F1', '#F39625',
      '#9FA2A6', '#D5D7D9', '#847F5F', '#40A0C9'),
  harvard =
    c('#C5112E', '#231F20', '#0B0808', '#CDABB1',
      '#E4D5D7', '#BA263E',  '#9F9F9F', '#79575C'),
  mgh =
    c('#007DA2', '#374249', '#B5C1C6',  '#6E8189',
      '#DAE2E5', '#1C8BAC', '#8FC6D6', '#3D9CB8'),
  pokrie =
    c('#612D13',  '#E1A863', '#245967', '#232324',
      '#C49E67', '#57503E', '#975A2E',  '#DFC597',
      '#257589', '#4F8B93')
)

#' @rdname rawr_palettes
#' @export
rawr_pal <- function(name, n, z = n, type = c('discrete', 'continuous'),
                     rev = FALSE) {
  type <- match.arg(type)
  pal <- rawr_palettes[[tolower(name)]]
  
  if (rev)
    pal <- rev(pal)
  
  if (missing(n))
    n <- length(pal)
  
  if (is.null(pal))
    stop(sprintf('palette %s not found', shQuote(name)), call. = FALSE)
  
  if (type == 'discrete' & n > length(pal)) {
    warning(sprintf('palette %s has max %s colors, try type = \'continuous\'',
                    shQuote(name), length(pal)))
    n <- length(pal)
  }
  
  res <- switch(
    type,
    continuous = grDevices::colorRampPalette(pal[seq.int(n)])(z),
    discrete = pal[seq.int(n)]
  )
  
  structure(res, class = 'rawr_pal', name = name)
}

#' @rdname rawr_palettes
#' @export
show_pal <- function(x, fullrange = FALSE, counts = TRUE) {
  imgpal <- inherits(x, 'imgpal')
  
  if (inherits(x, 'rawr_pal')) {
    name <- attr(x, 'name')
    pal <- x
  } else if (length(x) == 1L) {
    idx <- match(tolower(x), names(rawr_palettes), nomatch = 0L)
    if (idx == 0L)
      stop(sprintf('palette %s not found', shQuote(x)), call. = FALSE)
    pal <- rawr_palettes[[idx]]
    name <- x
  } else if (imgpal) {
    obj <- x
    pal <- obj$col
    len <- length(pal)
    name <- obj$filename
    pal <- pal[seq.int(if (fullrange) len else pmin(obj$n_unique, len))]
  } else {
    pal <- x
    name <- deparse(substitute(x))
  }
  
  n <- length(pal)
  
  op <- par(mar = rep_len(1, 4L))
  on.exit(par(op))
  image(seq.int(n), 1, matrix(seq.int(n)), col = pal, ann = FALSE, axes = FALSE)
  abline(v = seq.int(n) + 0.5, col = 'white')
  
  ## add bars of color frequencies
  if (imgpal && counts) {
    ht <- obj$counts[seq.int(n)]
    ht <- rescaler(ht, par('usr')[3:4], c(0, max(ht)))
    rect(seq.int(n) - 0.5, par('usr')[3L], seq.int(n) + 0.25, ht,
         col = 'white', density = 10, angle = 45)
    rect(seq.int(n) - 0.5, par('usr')[3L], seq.int(n) + 0.25, ht,
         col = 'white', density = 10, angle = -45)
  }
  
  rect(0, 0.9, n + 1, 1.1, col = adjustcolor('white', 0.8), border = NA)
  text((n + 1) / 2, 1, labels = name)
  
  pal
}
