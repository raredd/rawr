### plot functions
# tplot, tplot.default, tplot.formula, waffle, river, river2,
# check_river_format, plothc, waterfall, heatmap.3, bplot, tabler_bincon,
# pplot, pplot.default, pplot.formula
# 
# S3 methods:
# tplot, pplot
# 
# unexported:
# vioplot, bars_, binconr_
###


#' tplot
#' 
#' An alternative to \code{\link{boxplot}} with additional annotations,
#' hypothesis testing, and panel expressions. The individual data points can
#' be shown (either in the foreground or background) with point-dodging.
#' Violin plots with optional boxplots (and/or points) may also be shown.
#' 
#' @param x a numeric vector or a single list containing such vectors
#' @param g a vector or factor object giving the group for the corresponding
#'   elements of \code{x}, ignored with a warning if \code{x} is a list
#' @param ... for the \code{formula} method, named arguments to be passed to
#'   the default method
#' 
#'   for the default method, graphical parameters passed to \code{\link{par}}
#' @param formula a \code{\link{formula}}, such as \code{y ~ grp}, where y is
#'   a numeric vector of data values to be split into groups according to the
#'   grouping variable \code{grp} (usually a factor)
#' @param data a data frame (or list) from which the variables in
#'   \code{formula} should be taken
#' @param subset an optional vector specifying a subset of observations to be
#'   used for plotting
#' @param na.action a function which indicates what should happen when the data
#'   contain \code{\link{NA}}s; the default is to ignore missing values in either
#'   the response or the group
#' @param type type of plot (\code{"d"} for dot, \code{"db"} for dot-box,
#'   \code{"bd"} for box-dot, or \code{"b"} box; \code{"v"} may be used instead
#'   of \code{"b"} for a violin plot); see examples for all options
#' @param main,sub overall title and sub-title (below x-axis) for the plot
#' @param xlab,ylab x- and y-axis labels
#' @param xlim,ylim x- and y-axis limits
#' @param names group labels
#' @param axes logical; draw axes
#' @param frame.plot logical; draw box around \code{x-y} plot
#' @param at the x-axis group positions
#' @param horizontal logical; flip axes
#' @param jit,dist,dist.n jitter parameters for overlapping points (use
#' \code{0} for no jitter (i.e., points may overlap) and values > 0 for more
#'   distance between points); both can be length 1 (recycled as needed for
#'   groups) or equal to the number of groups (useful if one group has more
#'   points and needs more jittering than other groups)
#' 
#'   \code{jit} controls the amount of spread in a group of neighboring points,
#'   and \code{dist} controls the size of the interval to group neighboring
#'   points, i.e., a group of sequential points that are no more than
#'   \code{dist} apart are considered neighbors and will be jittered
#' 
#'   \code{dist.n} is the maximum number of points allowed in each group of
#'   neighboring points, useful for limiting the spread of points
#' @param args.beeswarm logical or a \emph{named} list of arguments passed to
#'   \code{\link[beeswarm]{beeswarm}}; if \code{NULL} (default) or \code{FALSE},
#'   \code{beeswarm} is not used; if \code{TRUE}, \code{beeswarm} is used with
#'   pre-set defaults (i.e., \code{method = 'center'} and \code{horizontal} to
#'   match the \code{tplot} setting); passing a list of arguments will add or
#'   override arguments except that \code{tplot} \emph{will not} adjust data
#'   values as \code{beeswarm} methods may
#' @param boxplot.pars additional list of graphical parameters for box plots
#'   (or violin plots)
#' @param quantiles for violin plots, probabilities for quantile lines (as an
#'   alternative to box plots); note \code{lwd}/\code{lty} may be passed to
#'   control the quantile lines
#' @param col plotting color
#' @param group.col logical; if \code{TRUE}, color by group; otherwise by order
#' @param boxcol,bordercol box fill and border colors
#' @param pch plotting character
#' @param group.pch logical; if \code{TRUE}, \code{pch} by group; otherwise
#'   by order
#' @param cex \strong{c}haracter \strong{ex}pansion value
#' @param group.cex logical; if \code{TRUE}, groups use the same \code{cex}
#'   value; otherwise, points have individual values, recycled if necessary
#' @param median.line,mean.line logical; draw median, mean lines
#' @param median.pars,mean.pars lists of graphical parameters for median and
#'   mean lines
#' @param show.n,show.na logical; show total and missing in each group
#' @param cex.n character expansion for \code{show.n} and \code{show.na}
#' @param text.na label for missing values (default is "missing")
#' @param n.at the y-coordinate (or x-coordinate if \code{horizontal = TRUE})
#'   to place the total and missing in each group
#' @param test logical or function; if \code{TRUE}, a rank-sum p-value is
#'   added to the plot (\code{\link{wilcox.test}} or \code{\link{kruskal.test}}
#'   based on the number of groups)
#' 
#'   Alternatively, a function (or function name as a character string) can be
#'   used, e.g., \code{test = cuzick.test} or \code{function(x, g)
#'   cuzick.test(x, g)}; note that if \code{test} is a function, it must have
#'   at least two arguments with the numeric data values and group
#' @param args.test an optional \emph{named} list of \code{\link{mtext}}
#'   arguments controlling the \code{test} text
#' @param format_pval logical; if \code{TRUE}, p-values are formatted with
#'   \code{\link{pvalr}}; if \code{FALSE}, no formatting is performed;
#'   alternatively, a function can be passed which should take a numeric value
#'   and return a character string (or a value to be coerced) for printing
#' @param ann logical; annotate plot
#' @param add logical; add to an existing plot
#' @param panel.first an expression to be evaluated after the plot axes are
#'   set up but before any plotting takes place; this can be useful for drawing
#'   background grids or scatterplot smooths; note that this works by lazy
#'   evaluation: passing this argument from other plot methods may well not work
#'   since it may be evaluated too early; see also \code{\link{plot.default}}
#' @param panel.last an expression to be evaluated after plotting has taken
#'   place but before the axes, title, and box are added; see the comments about
#'   \code{panel.first}
#' 
#' @return
#' A list with the following elements (see \code{\link{boxplot}}:
#' 
#' \item{\code{$stats}}{a matrix, each column contains the extreme of the lower
#' whisker, the lower hinge, the median, the upper hinge and the extreme of the
#' upper whisker for one group/plot. If all the inputs have the same class
#' attribute, so will this component}
#' \item{\code{$n}}{a vector with the number of observations in each group.}
#' \item{\code{$conf}}{a matrix where each column contains the lower and upper
#' extremes of the notch}
#' \item{\code{$out}}{the values of any data points which lie beyond the
#' extremes of the whiskers}
#' \item{\code{$group}}{a vector of the same length as \code{out} whose
#' elements indicate to which group the outlier belongs}
#' \item{\code{$names}}{a vector of names for the groups}
#' 
#' additionally, \code{tplot} returns the following:
#' \item{\code{$test}}{the object returned by the \code{test} function}
#' \item{\code{$coords}}{a list for each group of data frames containing the
#' x- and y-coordinates of the points}
#'
#' @aliases vioplot
#' 
#' @seealso
#' \href{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}{Tatsuki
#' \code{tplot}}; \href{http://data.vanderbilt.edu/~graywh/dotplot/}{web app
#' for Tatsuki \code{tplot}}; \code{\link{boxplot}}; \code{\link[plotr]{jmplot}}
#'
#' @examples
#' ## these are equivalent ways to call tplot
#' x <- mtcars$mpg
#' g <- interaction(mtcars$gear, mtcars$vs)
#' 
#' tplot(x, g)
#' tplot(split(x, g))
#' tplot(x ~ g)
#' tplot(mpg ~ gear + vs, mtcars)
#' 
#' 
#' ## use of point coordinates
#' co <- tplot(mpg ~ vs, mtcars)
#' sapply(co$coords, function(x)
#'   points(x, pch = 16L, col = findInterval(x$y, fivenum(x$y)) + 1L))
#' 
#' 
#' ## options for box, violin, dots
#' types <- c('d', 'db', 'bd', 'b', 'v', 'vd', 'dv', 'dbv', 'bv', 'n')
#' l <- lapply(types, function(...) mtcars$mpg)
#' tplot(l, type = types, names = types, xlab = 'tplot(x, type = ...)')
#' 
#' 
#' ## horizontal plots may cut off show.n/show.na text
#' tplot(x, g, horizontal = TRUE)
#' 
#' op <- par(mar = par('mar') + c(0, 0, 0, 2))
#' tplot(x, g, horizontal = TRUE)
#' 
#' ## and/or rotate labels
#' tplot(x, g, horizontal = TRUE, srt = 45)
#' par(op)
#' 
#' 
#' ## add rank-sum or custom test to plot
#' tplot(mpg ~ vs, mtcars, test = TRUE)   ## two groups - wilcox.test
#' tplot(mpg ~ gear, mtcars, test = TRUE) ## >=2 groups - kruskal.test
#' tplot(mpg ~ gear, mtcars, test = rawr::cuzick.test) ## trend test
#' 
#' ## custom test/text formatting
#' tplot(mtcars$mpg, 1:2, test = function(x, g)
#'   wilcox.test(x ~ g, data.frame(x, g), exact = FALSE, paired = TRUE),
#'   args.test = list(col = 2, at = 1.5, adj = 0.5, line = -3, cex = 2))
#' 
#' 
#' ## tplot has the same return value as boxplot with addition elements
#' ## for the test/coordinates if applicable
#' identical(
#'   within(tplot(mtcars$mpg), {test <- coords <- NULL}),
#'   within(boxplot(mtcars$mpg), {test <- coords <- NULL})
#' )
#' 
#' 
#' ## use panel.first/panel.last like in `plot` (unavailable in `boxplot`)
#' tplot(
#'   mpg ~ gear, data = mtcars, col = 1:3, type = 'd', show.na = FALSE,
#'   cex = c(1, 5)[(mtcars$mpg > 30) + 1L],
#'   panel.last = legend('topleft', legend = 3:5, col = 1:3, pch = 1),
#'     panel.first = {
#'       rect(1.5, par('usr')[3], 2.5, par('usr')[4], col = 'cyan', border = NA)
#'       abline(h = mean(mtcars$mpg))
#'       abline(h = 1:6 * 5 + 5, lty = 'dotted', col = 'grey70')
#'     }
#' )
#' 
#' 
#' ## beeswarm options
#' x <- rnorm(1000)
#' tplot(
#'   x, type = 'd',
#'   args.beeswarm = list(method = 'square', corral = 'gutter', corralWidth = 0.25)
#' )
#' 
#' ## compare (note that tplot **does not** change the original data values)
#' beeswarm::beeswarm(x, method = 'square', corral = 'gutter', corralWidth = 0.25)
#' 
#' 
#' ## example with missing data
#' set.seed(1)
#' dat <- data.frame(
#'   age   = replace(rnorm(80, rep(c(26, 36), c(70, 10)), 4), 1:5, NA),
#'   sex   = factor(sample(c('Female', 'Male'), 80, replace = TRUE)),
#'   group = paste0('Group ', sample(1:4, 80, prob = c(2, 5, 4, 1),
#'                                   replace = TRUE))
#' )
#' 
#' tplot(
#'   age ~ group, data = dat, las = 1, cex.axis = 1, bty = 'L',
#'   type = c('db', 'dv', 'dbv', 'bv'), names = LETTERS[1:4],
#'   quantiles = c(0.25, 0.5, 0.75), lwd = c(0.5, 2, 0.5),
#'   text.na = 'n/a', ## default is 'missing'
#'   group.pch = TRUE, pch = c(15, 17, 19, 8),
#'   group.col = FALSE, col = c('darkred', 'darkblue')[sex],
#'   boxcol = c('lightsteelblue1', 'lightyellow1', grey(0.9)),
#'   boxplot.pars = list(notch = TRUE, boxwex = 0.5)
#' )
#' legend(
#'   par('usr')[1], par('usr')[3], xpd = NA, bty = 'n',
#'   legend = levels(dat$sex), col = c('darkred', 'darkblue'), pch = 19
#' )
#'
#' @export

tplot <- function(x, ...) {
  UseMethod('tplot')
}

#' @rdname tplot
#' @export
tplot.formula <- function(formula, data = NULL, ...,
                          subset, na.action = NULL,
                          panel.first = NULL, panel.last = NULL) {
  
  if (missing(formula) || (length(formula) !=  3))
    stop("\'formula\' missing or incorrect")
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame(1L))))
    m$data <- as.data.frame(data)
  
  args <- lapply(m$..., eval, data, parent.frame(1L))
  nmargs <- names(args)
  
  form <- as.character(formula)
  args <- modifyList(list(xlab = form[3L], ylab = form[2L]), args)
  
  if ('main' %in% nmargs) args[['main']] <- enquote(args[['main']])
  if ('sub' %in% nmargs)  args[['sub']]  <- enquote(args[['sub']])
  if ('xlab' %in% nmargs) args[['xlab']] <- enquote(args[['xlab']])
  if ('ylab' %in% nmargs) args[['ylab']] <- enquote(args[['ylab']])
  
  ## hacky way to pass panel.first/panel.last to tplot.default
  args[['panel.first']] <- substitute(panel.first)
  args[['panel.last']]  <- substitute(panel.last)
  
  m$... <- m$panel.first <- m$panel.last <- NULL
  m$na.action <- na.pass
  
  m[[1L]] <- as.name('model.frame')
  mf <- eval(m, parent.frame(1L))
  n <- nrow(mf)
  response <- attr(attr(mf, 'terms'), 'response')
  
  ## special handling of col and pch for grouping
  group.col <- if ('group.col' %in% names(args)) args$group.col else FALSE
  group.pch <- if ('group.pch' %in% names(args)) args$group.pch else FALSE
  group.cex <- if ('group.cex' %in% names(args)) args$group.cex else FALSE
  
  ## reorder if necessary
  if ('col' %in% names(args) && !group.col)
    args$col <- unlist(split(rep_len(args$col, n), mf[-response]))
  if ('pch' %in% names(args) && !group.pch)
    args$pch <- unlist(split(rep_len(args$pch, n), mf[-response]))
  if ('cex' %in% names(args) && !group.cex)
    args$cex <- unlist(split(rep_len(args$cex, n), mf[-response]))
  
  do.call('tplot', c(list(split(mf[[response]], mf[-response])), args))
}

#' @rdname tplot
#' @export
tplot.default <- function(x, g, ..., type = 'db',
                          jit = NULL, dist = NULL, dist.n = Inf,
                          args.beeswarm = list(),
                          
                          ## labels/aesthetics
                          main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                          xlim = NULL, ylim = NULL, names,
                          col, group.col = TRUE, boxcol = 'grey90',
                          bordercol = par('fg'),
                          pch = par('pch'), group.pch = TRUE,
                          cex = par('cex'), group.cex = FALSE,
                          
                          ## additional aesthetics
                          median.line = FALSE, mean.line = FALSE,
                          median.pars = list(), mean.pars = list(),
                          boxplot.pars = list(), quantiles = NULL,
                          
                          ## n/missing for each group
                          show.n = TRUE, show.na = show.n, cex.n = cex,
                          text.na = 'missing', n.at = NULL,
                          
                          ## extra stuff
                          test = FALSE, args.test = list(), format_pval = TRUE,
                          ann = par('ann'), axes = TRUE, frame.plot = axes,
                          add = FALSE, at = NULL, horizontal = FALSE,
                          panel.first = NULL, panel.last = NULL) {
  ## helpers
  localAxis   <- function(..., bg, cex, log, lty, lwd,       pos      )
    axis(...)
  localBox    <- function(..., bg, cex, log, lty, lwd, tick, pos, padj)
    box(...)
  localMtext  <- function(..., bg, cex, log, lty, lwd, tick, pos, padj)
    mtext(..., cex = cex.n)
  localPoints <- function(...,          log, lty, lwd, tick, pos, padj)
    points(...)
  localText   <- function(..., bg, cex, log, lty, lwd, tick,      padj)
    text(..., cex = cex.n)
  localTitle  <- function(..., bg, cex, log, lty, lwd, tick, pos, padj)
    title(...)
  localVplot  <- function(..., outline)
    vioplot(...)
  localWindow <- function(..., bg, cex,      lty, lwd, tick, pos, padj)
    plot.window(...)
  
  if (!missing(g)) {
    if (is.null(xlab))
      xlab <- deparse(substitute(g))
    if (is.list(x))
      message('\'x\' is a list -- \'g\' will be ignored', domain = NA)
    else {
      if (is.null(ylab))
        ylab <- deparse(substitute(x))
      if (is.null(xlab))
        xlab <- deparse(substitute(g))
      if (is.numeric(g)) {
        g <- as.integer(g)
        g <- factor(g, seq(min(g), max(g)))
      }
      x <- split(x, g)
    }
  } else {
    if (is.null(xlab))
      xlab <- deparse(substitute(x))
  }
  
  args <- list(x, ...)
  namedargs <- if (!is.null(attributes(args)$names))
    attributes(args)$names !=  '' else logical(length(args))
  groups <- if (is.list(x))
    x else args[!namedargs]
  pars <- args[namedargs]
  
  if ((n <- length(groups)) == 0L)
    stop('invalid first argument')
  
  if (length(class(groups)))
    groups <- unclass(groups)
  
  if (!missing(names)) {
    if (isTRUE(names) | is.null(names))
      names <- attr(groups, 'names')
    if (!identical(names, FALSE))
      attr(groups, 'names') <- names
  } else {
    if (is.null(attr(groups, 'names')))
      attr(groups, 'names') <- seq.int(n)
    names <- attr(groups, 'names')
  }
  res <- do.call('boxplot', list(x = x, plot = FALSE, names = names))
  
  ## number and size of groups
  ng <- length(groups)
  lg <- lengths(groups)
  nv <- sum(lg)
  g  <- factor(rep(seq.int(ng), lg), seq.int(ng), names(groups))
  ## .x used when test = TRUE
  .x <- x
  
  opts <- c('d', 'db', 'bd', 'b', 'v', 'vd', 'dv', 'dbv', 'bv', 'n')
  type <- match.arg(type, opts, several.ok = TRUE)
  ## type of plot for each group
  type <- rep_len(type, ng)
  
  ## default colors
  ## 50% gray for box/dots in back, otherwise default color
  defcols <- c(bordercol, par('col'))
  
  if (missing(col)) {
    col <- defcols[2L - grepl('.d', type)]
    group.col <- TRUE
  }
  
  boxcol <- rep_len(boxcol, ng)
  boxborder <- rep_len(bordercol, ng)
  
  if (group.col) {
    ## colors by group
    g.col <- rep_len(col, ng)
    col   <- rep(g.col, lg)
  } else {
    ## colors by individual or global
    col   <- rep_len(col, nv)
    g.col <- rep_len(1L, ng)
  }
  pch <- if (group.pch) {
    ## plot characters by group
    rep(rep_len(pch, ng), lg)
  } else {
    ## plot characters by individual or global
    rep_len(pch, nv)
  }
  force(cex.n)
  cex <- if (group.cex) {
    ## plot characters by group
    rep(rep_len(cex, ng), lg)
  } else {
    ## plot characters by individual or global
    rep_len(cex, nv)
  }
  
  ## split colors and plot characters into groups
  col <- split(col, g)
  pch <- split(pch, g)
  cex <- split(cex, g)
  
  ## remove any NAs from the data and options
  nonas <- lapply(groups, function(x) !is.na(x))
  l2    <- sapply(groups, function(x) sum(is.na(x)))
  
  if (all(l2 == 0L) && missing(show.na))
    show.na <- FALSE
  groups <- Map('[', groups, nonas)
  col <- Map('[', col, nonas)
  pch <- Map('[', pch, nonas)
  cex <- Map('[', cex, nonas)
  
  ## mean and median line for each group
  mean.line   <- rep_len(mean.line, ng)
  median.line <- rep_len(median.line, ng)
  
  if (is.null(at))
    at <- seq.int(ng)
  if (length(at) !=  ng) {
    warning('\'at\' must have same length as the number of groups')
    at <- seq.int(ng)
  }
  
  ## scales
  xlim <- xlim %||% range(at, finite = TRUE) + c(-0.5, 0.5)
  ylim <- ylim %||%
    range(res$stats[is.finite(res$stats)],
          if (is.null(args$outline) || isTRUE(args$outline))
            res$out[is.finite(res$out)],
          if (isTRUE(args$notch))
            res$conf[is.finite(res$conf)])
  
  ## defaults for dist and jit for groups
  dist <- rep_len(if (is.null(dist) || is.na(dist))
    diff(range(ylim)) / pmax(500, nv) else dist, ng)
  jit  <- rep_len(if (is.null(jit) || is.na(jit))
    # 0.025 * ng else jit, ng)
    min(1 / lg) else jit, ng)
  
  
  dist.n <- rep_len(dist.n, ng)
  groups <- lapply(seq_along(groups), function(ii) {
    grouping_(groups[[ii]], dist[ii], dist.n[ii])
  })
  ## rawr:::grouping_; rawr:::jit_
  
  ## set up new plot unless adding to existing one
  if (!add) {
    plot.new()
    if (horizontal)
      do.call('localWindow', c(list(ylim, xlim), pars))
    else
      do.call('localWindow', c(list(xlim, ylim), pars))
  }
  
  panel.first
  
  coords <- setNames(vector('list', ng), names)
  Lme <- 0.2 * c(-1, 1)
  
  for (ii in seq.int(ng)) {
    p <- groups[[ii]]
    if (!nrow(p))
      next
    if (isTRUE(args.beeswarm) ||
        (is.list(args.beeswarm) & length(args.beeswarm) > 0L)) {
      args <- list(method = 'center', horizontal = horizontal)
      if (isTRUE(args.beeswarm))
        args.beeswarm <- args
      args <- modifyList(args, args.beeswarm)
      bs <- do.call(
        beeswarm::beeswarm,
        modifyList(args, list(do.plot = FALSE, x = p$vs, at = at[ii]))
      )
      x <- bs$x
      y <- bs$y.orig
    } else {
      y <- ave(p$vs, p$g.id, FUN = sym_sort)
      x <- rep_len(at[ii], nrow(p)) + jit_(p$g.si, p$hmsf) * jit[ii]
    }
    
    coords[[ii]] <- data.frame(x = x, y = y)
    
    boxFUN <- ifelse(grepl('v', type[ii]), 'localVplot', 'boxplot')
    
    if (grepl('v', type[ii])) {
      boxplot.pars <- modifyList(
        boxplot.pars,
        list(
          quantiles = quantiles,
          lwd = pars$lwd %||% 1,
          lty = pars$lty %||% 1L
        )
      )
      
      if (grepl('b', type[ii]))
        boxplot.pars <- modifyList(boxplot.pars, list(boxplot = TRUE))
      if (grepl('v', type[ii]))
        boxplot.pars <- modifyList(boxplot.pars, list(viocol = boxcol[ii]))
    }
    
    ## switch to points if not enough data for density estimation
    if (nrow(p) == 1L && grepl('v', type[ii])) {
      boxFUN <- 'boxplot'
      type[ii] <- gsub('v', ifelse(grepl('d', type[ii]), '', 'd'), type[ii])
    }
    
    ## dots behind
    if (type[ii] %in% c('bd', 'n', 'vd')) {
      bp <- do.call(
        boxFUN,
        c(list(x = y, at = at[ii], plot = FALSE, add = FALSE,
               axes = FALSE, col = boxcol[ii], border = boxborder[ii],
               outline = FALSE, horizontal = horizontal),
          boxplot.pars)
      )
      if (type[ii] == 'n')
        next
      
      notoplot <- (y <= bp$stats[5L, ]) & (y >= bp$stats[1L, ])
      
      if (sum(notoplot) > 0)
        col[[ii]][notoplot] <- '#bfbfbf'
      do.call(
        'localPoints',
        if (horizontal)
          c(list(x = y, y = x, pch = pch[[ii]],
                 col = col[[ii]], cex = cex[[ii]]),
            pars)
        else
          c(list(x = x, y = y, pch = pch[[ii]],
                 col = col[[ii]], cex = cex[[ii]]),
            pars)
      )
    }
    
    ## box in front
    if (type[ii] %in% c('bd', 'b', 'v', 'vd', 'bv')) {
      bp <- do.call(
        boxFUN,
        c(list(x = y, at = at[ii], add = TRUE, axes = FALSE,
               col = boxcol[ii], border = boxborder[ii],
               outline = FALSE, horizontal = horizontal),
          boxplot.pars)
      )
      
      toplot <- if (type[ii] %in% c('bd', 'v', 'vd', 'bv'))
        0 else (y > bp$stats[5L, ]) | (y < bp$stats[1L, ])
      
      if (sum(toplot) > 0)
        if (col[[ii]][toplot][1] == '#bfbfbf')
          col[[ii]][toplot] <- 1L
      do.call(
        'localPoints',
        if (horizontal)
          c(list(x = y[toplot], y = x[toplot], pch = pch[[ii]][toplot],
                 col = col[[ii]][toplot], cex = cex[[ii]][toplot]), pars)
        else
          c(list(x = x[toplot], y = y[toplot], pch = pch[[ii]][toplot],
                 col = col[[ii]][toplot], cex = cex[[ii]][toplot]), pars)
      )
    }
    
    ## box behind
    if (type[ii] %in% c('db', 'dv', 'dbv'))
      bp <- do.call(
        boxFUN,
        c(list(x = y, at = at[ii], add = TRUE, axes = FALSE,
               col = boxcol[ii], border = boxborder[ii],
               outline = FALSE, horizontal = horizontal),
          boxplot.pars)
      )
    
    ## dots in front
    if (type[ii] %in% c('db', 'd', 'dv', 'dbv')) {
      do.call(
        'localPoints',
        if (horizontal)
          c(list(x = y, y = x, pch = pch[[ii]],
                 col = col[[ii]], cex = cex[[ii]]), pars)
        else
          c(list(x = x, y = y, pch = pch[[ii]],
                 col = col[[ii]], cex = cex[[ii]]), pars)
      )
    }
    
    ## mean and median lines
    if (mean.line[ii])
      do.call(
        'lines',
        if (horizontal)
          c(list(rep(mean(y), 2L), at[ii] + Lme), mean.pars)
        else
          c(list(at[ii] + Lme, rep(mean(y), 2)), mean.pars)
      )
      
    if (median.line[ii])
      do.call(
        'lines',
        if (horizontal)
          c(list(rep_len(median(y), 2L), at[ii] + Lme), median.pars)
        else
          c(list(at[ii] + Lme, rep_len(median(y), 2L)), median.pars)
      )
  }
  
  ## p-value for wilcoxon/kw test in upper-right corner
  pv <- NULL
  if (!identical(test, FALSE)) {
    tFUN <- if (ng == 2L)
      function(x, g) wilcox.test(x ~ g, data.frame(x = x, g = g)) else
        function(x, g) kruskal.test(x ~ g, data.frame(x = x, g = g))
    if (is.function(test) || is.character(test))
      tFUN <- match.fun(test)
    
    # pv <- tFUN(unlist(lapply(groups, '[[', 'vs')), g)
    pv <- tryCatch(
      tFUN(unlist(.x), g),
      error = function(e) {
        message('Error in test -- no p-value computed:\n\t', e$message)
        list(p.value = NA)
      }
    )
    
    ## defaults passed to mtext
    targs <- list(
      text = if (isTRUE(format_pval))
        pvalr(pv$p.value, show.p = TRUE)
      else if (identical(format_pval, FALSE))
        pv$p.value
      else format_pval(pv$p.value),
      side = 3L, line = 0.5, cex = par('cex.main'),
      at = par('usr')[2L], font = 3L, adj = 1
    )
    
    if (!islist(args.test))
      args.test <- list()
    do.call('mtext', modifyList(targs, args.test))
  }
  
  panel.last
  
  if (axes) {
    do.call(
      'localAxis',
      c(list(side = 1L + horizontal, at = at, labels = names), pars)
    )
    do.call(
      'localAxis',
      c(list(side = 2L - horizontal), pars)
    )
  }
  
  ## frame and text, optional sample sizes
  if (show.n | show.na) {
    txt <- sprintf('%s%s\n%s%s', 
                   ifelse(rep_len(show.n, ng), 'n = ', ''),
                   ifelse(rep_len(show.n, ng), roundr(lg - l2, 0L), ''),
                   if (show.na)
                     ifelse(l2 > 0L, paste0(text.na, ' = '), '') else '',
                   if (show.na)
                     ifelse(l2 > 0L, roundr(l2, 0L), '') else ''
    )
    txt <- trimws(txt)
    
    log <- if (is.null(args$log))
      '' else args$log
    usr <- par('usr')
    usr <- if (is.na(idx <- match(log, c('x', 'y', 'xy', 'yx'))))
      par('usr')
    else list(
      x  = c(10, 10, 1, 1),
      y  = c(1, 1, 10, 10),
      xy = c(10, 10, 10, 10),
      yx = c(10, 10, 10, 10)
    )[[idx]] ^ usr
    
    if (is.null(n.at))
      n.at <- if (horizontal)
        (diff(ylim) * 0.08 * sign(ylim) + ylim)[2L] else usr[4L]
    
    do.call(
      'localText',
      if (horizontal)
        c(list(x = n.at, y = at, labels = txt, xpd = NA), pars)
      else
        c(list(x = at, y = n.at, labels = txt, xpd = NA,
               pos = if (is.null(pars$srt)) 3L else NULL), pars)
    )
  }
  
  if (frame.plot)
    do.call('localBox', pars)
  
  if (ann) {
    do.call(
      'localTitle',
      if (horizontal)
        c(list(main = main, sub = sub, xlab = ylab, ylab = xlab), pars)
      else
        c(list(main = main, sub = sub, xlab = xlab, ylab = ylab), pars)
    )
  }
  
  res <- within.list(res, {
    coords <- coords
    test <- pv
  })
  
  invisible(res)
}

#' waffle
#' 
#' A waffle chart.
#' 
#' @param mat a matrix of integers or character strings of color names; if
#'   \code{mat} is a matrix of integers, the colors used will correspond to
#'   the current \code{\link{palette}}
#' @param xpad,ypad padding between \code{\link{rect}}s, recycled as needed;
#'   note that these do not affect the center coordinates of the rectangles
#' @param heights,widths row heights and column widths, respectively, usually
#'   in \code{[0,1]}, recycled as needed; note \code{xpad} and/or \code{ypad}
#'   are ignored if \code{widths} and/or \code{heights} are used, respectively
#' @param colpad,rowpad amount of padding between columns and rows; note that
#'   these shift the \code{\link{rect}} center coordinates rather than the
#'   heights and widths directly
#' @param invert character string indicating about which axis the matrix
#'   should be inverted; possible values are \code{"x"}, \code{"y"}, or
#'   \code{"xy"}
#' @param ... additional graphical parameters passed to \code{\link{rect}}
#'   such as \code{border}, \code{density}, \code{lty}, etc.
#' @param reset_par logical; if \code{TRUE}, resets \code{\link{par}}
#'   settings to state before function call; setting \code{reset_par = FALSE}
#'   along with the return value are useful for adding to a \code{waffle} plot
#' @param add logical; use \code{TRUE} to add to an existing plot; otherwise,
#'   a new frame and window are initialized
#' 
#' @return
#' A list of four matrices:
#' 
#' \item{\code{$matrix}}{the input matrix as plotted including inversions}
#' \item{\code{$origin}}{coordinates of the bottom-left corner for each box}
#' \item{\code{$centers}}{coordinates for the centers of each box}
#' \item{\code{$rect}}{coordinates for each corner of each box}
#' 
#' @examples
#' waffle(matrix(1:9, 3))
#' waffle(matrix(1:9, 3), invert = 'x')
#' waffle(matrix(1:9, 3), heights = c(.25, .95, .5), border = NA)
#' waffle(matrix(1:9, 3), xpad = 0, colpad = c(0, .1, 0))
#' 
#' 
#' ## heatmap
#' ## convert the numeric data to color strings
#' cols <- c(cor(mtcars))
#' cols <- tcol(c('blue', 'red')[(cols > 0) + 1L], alpha = c(abs(cols)))
#' 
#' mat <- matrix(cols, 11)
#' waffle(mat, reset_par = FALSE, invert = 'x')
#' axis(3, at = 1:11 - .5, labels = names(mtcars), lwd = 0)
#' 
#' 
#' ## use colpad/rowpad to create sections
#' w <- waffle(mat, reset_par = FALSE, invert = 'x',
#'             colpad = rep(c(0, 0.5, 1, 0), c(5, 1, 1, 4)),
#'             rowpad = rep(c(0, 0.5, 0), c(5, 1, 5)))
#' axis(3, unique(w$centers[, 'x']), names(mtcars), lwd = 0)
#' 
#' 
#' ## adding to margins of another plot
#' set.seed(1)
#' n  <- 97
#' ng <- 3
#' cols <- c('beige', 'dodgerblue2', 'green', 'orange')
#' x <- sample(cols, n * ng, replace = TRUE, prob = c(.05,.31,.32,.32))
#' x <- kinda_sort(x, n = 20)
#' 
#' op <- par(fig = c(0, 1, 0.2, 0.9), mar = c(0, 5, 0, 1))
#' plot(cumsum(rnorm(n)), type = 'l', ann = FALSE, xaxt = 'n')
#' 
#' par(fig = c(0, 1, 0, 0.2), mar = c(1, 5, 0, 1), new = TRUE)
#' waffle(matrix(x, ng), heights = c(0.95, 0.5, 0.95), border = 'white',
#'        reset_par = FALSE) -> wf
#' text(0, rev(unique(wf$centers[, 'y'])), paste('Feature', 1:3),
#'      xpd = NA, pos = 2L)
#' 
#' par(fig = c(0, 1, 0.9, 1), mar = c(0.5, 5, 0.5, 1), new = TRUE)
#' waffle(matrix(x, ng)[1L, , drop = FALSE], heights = 0.5,
#'        border = 'white', reset_par = FALSE)
#' box()
#' box('outer')
#' par(op)
#' 
#' 
#' ## waffle conveniently returns the centers of the rects
#' ## be sure _not_ to reset pars for proper alignment
#' w <- waffle(matrix(1:8, 2), reset_par = FALSE, invert = 'xy')
#' text(w$c[, 'x'], w$c[, 'y'], labels = palette(), col = 'white')
#' 
#' 
#' ## this is similar to ?rawr::show_colors
#' col <- colors()[1:25 ^ 2]
#' w <- waffle(matrix(col, 25), reset_par = FALSE, invert = 'xy', border = NA)
#' text(w$c[, 'x'], w$c[, 'y'], labels = col, col = 'black', cex = .4)
#' 
#' @export

waffle <- function(mat, xpad = 0, ypad = 0,
                   heights = NULL, widths = NULL,
                   colpad = 0, rowpad = 0, invert = '',
                   ..., reset_par = TRUE, add = FALSE) {
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op))
  
  mat <- omat <- as.matrix(mat)
  mat <- mat[rev(seq.int(nrow(mat))), , drop = FALSE]
  o    <- cbind(c(row(mat)), c(col(mat))) - 1L
  
  if (grepl('x', invert)) {
    mat <- mat[rev(seq.int(nrow(mat))), , drop = FALSE]
    o[, 1L] <- rev(o[, 1L])
  }
  if (grepl('y', invert)) {
    mat <- mat[, rev(seq.int(ncol(mat))), drop = FALSE]
    o[, 2L] <- rev(o[, 2L])
  }
  
  nc <- ncol(mat)
  nr <- nrow(mat)
  nn <- length(mat)
  
  ## xpad/ypad intended to be by column/row
  if (nn > length(xpad) & length(xpad) == nc)
    xpad <- rep(xpad, each = nr)
  if (nn > length(ypad) & length(ypad) == nr)
    ypad <- rep(ypad, each = nc)
  
  if (!is.null(heights)) {
    if (nn > length(heights) & length(heights) == nc)
      heights <- rep(heights, each = nc)
    ypad <- 1 - (1 - rev(heights)) / 2
  }
  if (!is.null(widths)) {
    if (nn > length(widths) & length(widths) == nc)
      widths <- rep(widths, each = nr)
    xpad <- 1 - rep((1 - widths) / 2, each = nc)
  }
  
  ## add column/row sep values to origins
  colpad <- c(0, cumsum(rep_len(colpad, nc - 1L)))
  rowpad <- c(0, cumsum(rep_len(rowpad, nr - 1L)))
  
  o <- o + cbind(rev(rep(rowpad, nc)), rep(colpad, each = nr))
  
  if (!add) {
    plot.new()
    plot.window(
      xlim = c(0, max(o[, 2L]) + 1L), ylim = c(0, max(o[, 1L]) + 1L),
      xaxs = 'i', yaxs = 'i'
    )
  }
  
  rect(
    xl <- o[, 2L] + xpad, yb <- o[, 1L] + ypad,
    xr <- o[, 2L] + (1 - xpad), yt <- o[, 1L] + (1 - ypad),
    col = c(omat), ...
  )
  
  o <- o[, 2:1]
  colnames(o) <- c('x', 'y')
  
  res <- list(
    matrix  = mat,
    origin  = o,
    centers = cbind(x = psum(xl, xr) / 2, y = psum(yb, yt) / 2),
    rect    = cbind(xleft = xl, ybottom = yb, xright = xr, ytop = yt)
  )
  
  invisible(res)
}

#' River plots
#' 
#' @description
#' Summarize a timeline for individuals over the course of a study.
#' 
#' \code{river} shows the trajectory of multiple subjects. Time on treatment
#' is represented by a green line; progression as a red dot; censored or
#' death as an "x" in blue or red, respectively; and responses colored with
#' bands. Subjects continuing are shown with an arrow.
#' 
#' \code{river2} shows the trajectory for a single patient with additional
#' toxicity information represented by another series of bands colored by
#' grade. Optionally, the trajectory for a single patient from \code{river}
#' may be shown rather than with an arrow; see examples.
#' 
#' @details
#' \code{data}, \code{bar_data}, and \code{bar_data2} data frames need to have
#' a minimum number of variables in a specific order, some of which should be
#' \code{\link{Date}} formats (or can be coerced to dates, e.g., integers).
#' 
#' \code{check_river_format()} without any arguments will give a summary of
#' the required formats.
#' 
#' Note that date columns can also be given as integers to be coerced to dates
#' as seen in the examples. In this case, \code{check_river_format} will do
#' the coersion to date before plotting. However, these two methods are
#' interchangeable, but using integers rather than dates assume the data have
#' a common starting time.
#' 
#' \code{data} should have 10 columns (any additional will be ignored) in the
#' following order: ID; dates of registration, start of treatment, end of
#' treatment, progression, off treatment, and survival status; status
#' indicator; and dates of last contact and off study. That is, these should
#' be in approximate chronological order.
#' 
#' \code{bar_data} and \code{bar_data2} provide additional information to
#' supplement \code{data} and may contain multiple records per ID.
#' 
#' \code{bar_data} is intended to represent response assessments and therefore
#' should have three columns: ID, date of assessment, and assessment. Any
#' additional columns are ignored. Assessment data should be a factor variable
#' with proper level ordering or will be coerced.
#' 
#' \code{bar_data2} is intended to represent toxicity assessments and therefore
#' should have five columns: ID, start date, end date, grade, and description.
#' Any additional columns are ignored. Grade should be a factor variable with
#' proper level ordering or will be coerced. Descriptions should be relatively
#' short to avoid text extending outside of the plotting window.
#' 
#' Despite the assumptions above, any type of data may work if properly
#' formatted and ordered according to \code{check_river_format()}.
#' 
#' @param data,bar_data,bar_data2 data frames; these should have a specific
#'   format, see details, examples, or run \code{check_river_format()}
#' @param id,at optional parameters specifying individuals (rows) from
#'   \code{data} to plot and their positions along the y-axis; if not given,
#'   time lines are plotted sequentially
#' @param legend logical; if \code{TRUE}, a legend using the unique values of
#'   \code{data$status} is drawn
#' @param args.legend a \emph{named} list of arguments passed to
#'   \code{\link{legend}}
#' @param xlim,ylim x- and y-axis limits
#' @param rev logical; if \code{TRUE}, observations will be plotted from top
#'   to bottom
#' @param stagger logical; if \code{FALSE}, start dates will be fixed at 0
#'   rather than relative to the first start date if \code{TRUE} (default)
#' @param col,col2 vectors of colors for responses (\code{river}) or toxicity
#'   grades \code{river2}, respectively
#' @param axes logical; if \code{TRUE}, x-axes are drawn
#' @param split logical; if \code{TRUE}, rows of \code{bar_data2} will be
#'   plotted individually
#' 
#' @examples
#' ## to print a summary of the required formats
#' check_river_format()
#' 
#' 
#' ## data in river format:
#' dd <- data.frame(
#'   id       = 1:5,
#'   dt_reg   = 1:5,
#'   dt_txst  = 1:5 + 1,
#'   dt_txend = c(10:13, 8),
#'   dt_prog  = c(11, 16, NA, NA, 8),
#'   dt_offtx = c(12, 19, NA, NA, 8),
#'   dt_surv  = c(12, 19, 24, 25, NA),
#'   surv     = c(0, 1, 0, 0, NA),
#'   dt_last  = c(12, 19, 24, 25, NA),
#'   dt_off   = c(12, 19, NA, NA, 8)
#' )
#' bd <- data.frame(
#'   id        = c(3, 3, 3, 3, 4, 4),
#'   dt_assess = c(9, 13, 17, 21, 10, 15),
#'   resp = factor(c('MR', 'PR', 'PR', 'CR', 'PR', 'CR'),
#'                 levels = c('PD', 'SD', 'MR', 'PR', 'CR'))
#' )
#' 
#' ## basic usage
#' river(dd, bd)
#' river(
#'   dd, bd, stagger = FALSE, rev = TRUE, col = 2:6,
#'   args.legend = list(x = 'bottom', title = 'Response', horiz = TRUE)
#' )
#' 
#' 
#' ## using NA will suppress data from being plotted
#' river(dd, within(bd, resp <- NA))
#' river(within(dd, dt_txst <- NA), within(bd, resp <- NA))
#' river(within(dd, dt_txst <- NA), within(bd, {resp <- NA; dt_assess <- NA}))
#' 
#' 
#' ## same data with single observations per id
#' bd1 <- data.frame(
#'   id = 3:4, dt_assesss = 9:10,
#'   resp = factor(c('PR','CR'), levels = c('PD','SD','MR','PR','CR'))
#' )
#' 
#' river(dd, bd1)
#' 
#' ## id and at parameters control the positions of the timelines
#' river(dd, bd1, id = c(1, 2, 5, 3, 4), at = c(1:2, 4, 6:7), legend = FALSE)
#' 
#' 
#' ## additional data for river2
#' tt <- data.frame(
#'   id       = rep(c(1, 3), times = c(1, 5)),
#'   dt_start = c(3, 5, 5, 8, 9, 11),
#'   dt_end   = c(NA, 5, NA, 10, 10, NA),
#'   grade    = c(1, 4, 3, 4, 1, 2),
#'   desc     = paste('tox', c(1, 1:5))
#' )
#' 
#' river2(dd, bd, tt, id = 3)
#' 
#' ## multiple records per id (ie, worsening toxicities)
#' tt2 <- data.frame(
#'   id       = rep(c(1, 3), times = c(1, 8)),
#'   dt_start = c(3, 5, 5, 7, 15, 8, 9, 11, 14),
#'   dt_end   = c(NA, 5, 7, 15, NA, 10, 10, 14, NA),
#'   grade    = c(1, 4, 1, 2, 3, 4, 1, 2, 3),
#'   desc     = paste('tox', c(1, 1, 2, 2, 2, 3, 4, 5, 5))
#' )
#'                   
#' river2(dd, bd, tt2, id = 3)
#' 
#' 
#' ## bar_data can also be given in river2 without additional information
#' river2(bar_data = tt,  id = 3)
#' river2(bar_data = tt2, id = 3)
#' 
#' 
#' ## use custom axis() to change time units
#' river2(bar_data = tt2, id = 3, axes = FALSE)
#' at <- axTicks(1L)
#' axis(1L, at, at * 12)
#' title(xlab = 'Months from registration', line = 2.5)
#' 
#' @export

river <- function(data, bar_data, id, at,
                  legend = TRUE, args.legend = list(),
                  xlim = NULL, ylim = NULL, rev = FALSE,
                  stagger = TRUE, col = NULL, axes = TRUE) {
  ## error checks
  dd <- check_river_format(data)
  bd <- check_river_format(data, bar_data)
  nn <- as.character(unique(dd$id))
  
  if (missing(id))
    id <- nn
  if (missing(at))
    at <- seq_along(id)
  if (rev)
    at <- rev(at)
  
  stopifnot(length(at) == length(id))
  if (length(excl <- id[!id %in% nn]))
    stop('invalid id: ', toString(excl))
  
  dd <- merge(bd, dd, by = 'id', all = TRUE)
  
  ## colors for resp - PD:CR
  cols <- if (is.null(col))
    c('red', 'transparent', 'yellow', 'orange',
      'deepskyblue', 'dodgerblue3', 'blue4')
  else rep_len(col, nlevels(dd$assess))
  
  ## convert dates to days with origin at first id reg (ie, ref date == 0)
  dd_reg <- status <- dt_offstudy <- dd_prog <-
    dd_assess_start <- NULL ## binding note
  dts <- grep('^dt_', names(dd))
  mm  <- setNames(dd[, dts], gsub('dt_', 'dd_', names(dd)[dts]))
  
  rx <- if (stagger)
    range(unlist(mm), na.rm = TRUE) else
      range(unlist(mm[dd$id %in% id, , drop = FALSE]), na.rm = TRUE)
  
  mm[] <- lapply(mm, as.numeric)
  mm   <- t(apply(mm, 1, function(x)
    x - min(if (stagger) mm[, 'dd_reg'] else x['dd_reg'], na.rm = TRUE)))
  
  dd <- within(cbind(dd, mm), {
    end_day <- apply(mm, 1, max, na.rm = TRUE)
    end_day <- pmax(end_day, dd_reg)
    alive   <- is.na(status) | (grepl('(?i)[0v]', status) + 0L)
    censor  <- alive & !is.na(dt_offstudy)
    assess  <- as.factor(assess)
    col_assess <- cols[as.numeric(assess)]
    ## no red rects after progression date
    col_assess[col_assess == 'red' & dd_prog <= dd_assess_start] <- 'transparent'
  })
  
  plot.new()
  par(mar = c(4,1,1,0))
  plot.window(if (!is.null(xlim)) xlim else c(0, diff(rx)),
              ## set min ylim to c(0,5) for case: id < 5
              if (!is.null(ylim)) ylim else range(c(0, at, 5)))
  if (axes) {
    axis(1, tcl = 0.2, las = 1L)
    title(
      xlab = sprintf('Days from %sregistration',
                     c('', 'first ')[stagger + 1L]),
      line = 2.5
    )
  }
  
  if (isTRUE(legend) & !all(is.na(cols)) & !all(is.na(dd$assess))) {
    largs <- list(
      x = 'topleft', fill = cols, legend = levels(dd$assess),
      horiz = FALSE, cex = 0.8, bty = 'n'
    )
    do.call('legend', modifyList(largs, args.legend))
  }
  
  sp <- split(dd, dd$id, drop = FALSE)
  
  for (ii in id) {
    ## lines at specific points require new index
    jj <- at[which(id %in% ii)]
    
    # with(dd[ii, ], {
    with(sp[[as.character(ii)]], {
      ## label ids in black to left of rect
      text(dd_reg[1L], jj, labels = id[1L], pos = 2L, xpd = NA)
      
      ## lines - time alive, on tx
      do_seg_(jj, dd_reg, end_day, arrow = alive[1L] & !censor[1L],
              single = TRUE, col = 1L)
      ## thicker line and arrow for continuing
      do_seg_(jj, dd_txstart, dd_txend %|% end_day, arrow = FALSE,
              single = TRUE, lty = 1L, lwd = 4, col = 'green4')
      
      ## rects - assessments
      do_rect_(jj, dd_assess_start, dd_assess_end %|% end_day,
               col = tcol(col_assess, alpha = 0.5))
      ## pipe at each assessment time
      points(dd_assess_start, rep(jj, length(dd_assess_start)),
             pch = '|', col = 1L, cex = 0.5)
      
      ## points - prog (red circle), death, (red x), censor (blue x)
      points(dd_prog[1L], jj, pch = 16L, col = 2L, cex = 1.5)
      points(end_day[1L], jj, pch = c(4L, NA)[alive[1L] + 1L],
             col = 2L, lwd = 3, cex = 1.5)
      points(end_day[1L], jj, pch = c(NA, 4L)[(alive[1L] & censor[1L]) + 1L],
             col = 4L, lwd = 3, cex = 1.5)
    })
  }
  
  invisible(list(data = dd, bar_data = bd))
}

#' @rdname river
#' @export
river2 <- function(data, bar_data, bar_data2, id,
                   legend = TRUE, args.legend = list(),
                   xlim = NULL, ylim = NULL, rev = FALSE, stagger = FALSE,
                   split = FALSE, col = NULL, col2 = NULL, axes = TRUE) {
  ## error checks
  if (!missing(data)) {
    if (missing(bar_data2))
      return(
        river(data = data, bar_data = bar_data, id = id, at = 1, rev = rev,
              legend = legend, xlim = xlim, ylim = ylim, stagger = stagger,
              axes = axes)
      )
  } else {
    mmin <- function(x) min(x, na.rm = !all(is.na(x)))
    mmax <- function(x) max(x, na.rm = !all(is.na(x)))
    
    bar_data2 <- if (missing(bar_data2)) bar_data else bar_data2
    
    if (ncol(bar_data2) <= 3L)
      ## use bar_data2 format--check_river_format()
      bar_data2 <- bar_data[, c(1L, 2L, 2L, 3L, 3L)]
    
    data <- data.frame(
      id = bar_data[, 1L],
      dt_reg = ave(bar_data[, 2L], bar_data[, 1L], FUN = mmin),
      dt_txst  = NA, dt_txend = NA, dt_prog = NA,
      dt_offtx = NA, dt_surv  = NA, surv    = NA,
      dt_last = ave(unlist(bar_data[, 2:3]),
                    unlist(bar_data[, c(1L, 1L)]), FUN = mmax),
      dt_off = NA
    )
    
    data     <- data[!duplicated(data$id), ]
    bar_data <- data.frame(id = data$id, dt_assess = data$dt_reg, resp = NA)
  }
  
  if (!any(c(data[, 1L], bar_data[, 1L], bar_data2[, 1L]) %in% id))
    stop('invalid id: ', id)
  
  dd <- check_river_format(data)
  bd <- check_river_format(data, bar_data)
  td <- check_river_format(data, bar_data2)
  
  ## select bd for id, remove rows if NA start AND end, order by date
  dt_end <- NULL ## binding note
  td <- split(td, td$id, drop = FALSE)[[as.character(id)]]
  td <- td[!(is.na(td$dt_start) & is.na(td$dt_end)), ]
  td <- within(td, {
    no_start <- is.na(dt_start)
    dt_start[no_start] <- dt_end[no_start]
  })
  td <- td[do.call('order', as.list(td[, c('dt_start', 'desc')])), ]
  
  ## grade colors - 1:5
  cols <- if (is.null(col2))
    c('green', 'blue', 'orange', 'red', 'black')
  else rep_len(tcol(col2), lunique(td$grade))
  nn <- if (split)
    seq.int(nrow(td)) else seq_along(unique(td$desc))
  
  ## base plot of the id summary
  rv <- river(
    data = data, bar_data = bar_data, id = id, at = 1, col = col,
    legend = FALSE, rev = FALSE, stagger = stagger, xlim = xlim,
    ylim = if (is.null(ylim))
      c(0, max(5, (if (split) max(nn) else length(nn)) + 1)) else ylim,
    axes = axes
  )
  
  td <- within(td, {
    dt_reg    <- rv$data$dt_reg[id]
    end_day   <- rv$data$end_day[id]
    ong       <- is.na(dt_end)
    grade     <- as.factor(grade)
    desc      <- factor(desc, levels = unique(desc))
    col_grade <- cols[grade]
    num       <- ave(seq_along(desc), desc, FUN = seq_along)
  })
  
  if (isTRUE(legend)) {
    largs <- list(
      x = 'topleft', fill = tcol(cols, 0.5), legend = levels(td$grade),
      horiz = FALSE, cex = 0.8, bty = 'n'
    )
    do.call('legend', modifyList(largs, args.legend))
  }
  
  ## convert dates to days, fix origin at first reg date (ie, ref date == 0)
  dts <- grep('^dt_', names(td))
  mm <- setNames(
    lapply(td[, dts], function(x)
      as.numeric(x) - unique(as.numeric(rv$data[if (stagger) 1 else
        rv$data$id %in% id, 'dt_reg']))),
    gsub('dt_', 'dd_', names(td)[dts])
  )
  td <- cbind(td, do.call('cbind', mm))
  sp <- split(td, if (split) seq.int(nrow(td)) else td$desc)
  
  ## max end bar date
  dd_end2 <- rv$data[rv$data$id %in% id, c('dd_prog')]
  dd_end2 <- min(dd_end2, na.rm = !all(is.na(dd_end2)))
  
  for (ii in nn) {
    with(sp[[ii]], {
      ## rect for indiv td
      col  <- tcol(col_grade, alpha = 0.5)
      endx <- pmax(dd_end %|% dd_end2, dd_start, na.rm = TRUE)
      do_rect_(ii + 1L, dd_start, endx, col = col, border = col)
      
      ## td with end but no start date
      # if (no_start)
      if (FALSE)
        segments(endx, 1 + ii + c(0.15, -0.15), endx - 0.15,
                 1 + ii + c(0.15, -0.15), col = col, lwd = 2)
      
      ## add count of td to left in black if start date, color/italic if NA
      ## desc on right in color, italics if continuing; black otherwise
      text(dd_start[1L], ii + 1L, labels = ii, pos = 2L, xpd = NA,
           cex = 0.8, col = if (no_start[1L]) col else 1L,
           font = if (no_start[1L]) 3L else 1L)
      # text(tail(dd_end %|% end_day, 1), ii + 1, labels = tail(desc, 1),
      
      text(max(dd_start, dd_end %|% dd_end2, na.rm = TRUE), ii + 1L,
           labels = tail(desc, 1L), pos = 4L, xpd = NA, cex = 0.8,
           font = c(1L, 3L)[tail(ong, 1L) + 1L],
           col = c('black', tail(col, 1L))[tail(ong, 1) + 1L])
    })
  }
  
  invisible(list(data = rv$data, bar_data = bd, bar_data2 = td))
}

#' @rdname river
#' @export
check_river_format <- function(data, bar_data) {
  fmt1 <- c("'data.frame': n obs. of 10 variables:",
            " $ id            : any ",
            " $ dt_reg        :Class 'Date'",
            " $ dt_txstart    :Class 'Date'",
            " $ dt_txend      :Class 'Date'",
            " $ dt_prog       :Class 'Date'",
            " $ dt_offtx      :Class 'Date'",
            " $ dt_status     :Class 'Date'",
            " $ status        : any ",
            " $ dt_lastcontact:Class 'Date'",
            " $ dt_offstudy   :Class 'Date'")
  fmt2 <- c("'data.frame': n obs. of 3 variables:",
            " $ id            : any ",
            " $ dt_assess     :Class 'Date'",
            " $ assessment    : factor")
  fmt3 <- c("'data.frame': n obs. of 5 variables:",
            " $ id            : any ",
            " $ dt_tox_start  :Class 'Date'",
            " $ dt_tox_end    :Class 'Date'",
            " $ tox_grade     : factor ",
            " $ tox_desc      : any ")
  
  ## if no arguments, print the formats
  if (missing(data) && missing(bar_data)) {
    message('\'data\' should have the following format:\n', domain = NA)
    cat(fmt1, sep = '\n')
    
    message('\n\'bar_data\' objects should have the following format:\n',
            domain = NA)
    cat(fmt2, sep = '\n')
    
    message('\n\t\tOR\n')
    cat(fmt3, sep = '\n')
    
    return(invisible(NULL))
  }
  
  to_date <- function(x) {
    to_date_ <- function(x)
      if (inherits(x, 'Date')) x else as.Date.numeric(x, origin = '1970-01-01')
    dts <- grep('^dt_', names(x))
    x[, dts] <- lapply(x[, dts, drop = FALSE], to_date_)
    x
  }
  
  do_error <- function(fmt, wh) {
    message(sprintf('%s should have the following format:\n', shQuote(wh)))
    cat(fmt, sep = '\n')
    stop('Format error: see of ?rawr::river for more information',
         domain = NA, call. = FALSE)
  }
  
  dd <- tryCatch({
    dd <- setNames(
      data[, 1:10],
      c('id', 'dt_reg', 'dt_txstart', 'dt_txend', 'dt_prog', 'dt_offtx',
        'dt_status', 'status', 'dt_lastcontact', 'dt_offstudy')
    )
    to_date(dd)
  }, error = function(e) do_error(fmt1, 'data'))
  
  if (missing(bar_data))
    return(invisible(dd))
  
  dts <- grep('^dt_', names(bar_data))
  bd  <- tryCatch({
    if (one <- length(dts) == 1L) {
      dt_assess_start <- NULL ## binding note
      bd <- setNames(bar_data[, 1:3], c('id', 'dt_assess_start', 'assess'))
      bd <- within(bd, {
        id <- factor(id, levels = unique(dd$id))
        dt_assess_end <- ave(dt_assess_start, id, FUN = function(x)
          c(tail(x, -1L), NA))
      })[!is.na(bd$dt_assess_start), ]
      to_date(bd)
    } else {
      bd <- setNames(bar_data[, 1:5],
                     c('id', 'dt_start', 'dt_end', 'grade', 'desc'))
      bd <- within(bd, {
        id <- factor(id, levels = unique(dd$id))
      })
      to_date(bd)
    }
  }, error = function(e)
    do_error(if (one) fmt2 else fmt3,
             paste0('bar_data', c('', '2')[one + 1L])))
  
  invisible(bd)
}

#' Color \code{plot.hclust}
#' 
#' Plot an \code{\link{hclust}} object with colored labels.
#' 
#' @param hc an \code{\link{hclust}} object
#' @param labels a character vector of labels for the leaves of the tree
#' @param col a vector of valid colors for \code{labels}
#' @param hang the fraction of the plot height by which \code{labels} should
#'   hang below the rest of the plot; a negative value will cause \code{labels}
#'   to hang down from 0
#' @param ... additional arguments passed to \code{\link{plot.hclust}}
#' 
#' @author
#' Eva KF Chan, \url{https://github.com/ekfchan/evachan.org-Rscripts},
#' modifications by Robert Redd
#' 
#' @examples
#' hc <- hclust(dist(iris[, 1:4]))
#' plothc(hc)
#' plothc(hc, labels = iris$Species, col = as.numeric(iris$Species))
#' 
#' 
#' ## add features below leaves
#' plothc(hc, col = iris$Species, xlab = '', sub = '')
#' 
#' set.seed(1)
#' mat <- matrix(rep(as.numeric(iris$Species), 3), 3)
#' mat[sample(length(mat), length(mat) / 2)] <- NA
#' mat <- sort_matrix(mat)[3:1, ]
#' shift <- c(0.5, 6)
#' rect(col(mat) - shift[1], row(mat) - shift[2], col(mat) + shift[1],
#'      row(mat) - shift[2] + 0.5, col = mat, xpd = NA, border = 'white')
#' 
#' @export

plothc <- function(hc, labels = hc$labels, col = as.factor(labels),
                   hang = 0.1, ...) {
  stopifnot(inherits(hc, 'hclust'))
  
  o   <- hc$order
  ht  <- hc$height
  col <- rep_len(if (is.factor(col)) as.numeric(col) else col, length(o))
  labels <- as.character(if (is.null(labels)) seq_along(o) else labels)
  
  y <- rep(ht, 2)
  x <- c(hc$merge)
  
  y <- y[which(x < 0)]
  x <- x[which(x < 0)]
  
  x <- abs(x)
  
  y <- y[order(x)]
  x <- x[ox <- order(x)]
  y <- y[o] - (max(ht) * hang)
  
  plot(hc, labels = FALSE, hang = hang, ...)
  text(x, y, labels = labels[o], col = col[o],
       srt = 90, xpd = NA, adj = c(1, 0.5))
  
  invisible(list(x = x[ox], y = y))
}

#' Waterall plot
#' 
#' Draw two types of waterfall plots.
#' 
#' @param x a numeric vector
#' @param type type of waterfall plot; \code{type = 1} draws sorted bars for
#'   values of \code{x}; \code{type = 2} starts from 0 and draws bars according
#'   to the magnitude and direction of \code{x}
#' @param col a vector of colors having 1) the same length as \code{x}; length
#'   2 (for negative and positive values if \code{type = 2}) or otherwise to be
#'   used for color interpolation (\code{\link{colorRampPalette}})
#' @param ... additional arguments passed to \code{\link{barplot}}, to/from
#'   other methods, or to \code{\link{par}}
#' @param arrows logical; if \code{TRUE}, arrows are drawn in the direction
#'   of each bar
#' @param rev logical; if \code{TRUE}, the order along the x-axis is reversed
#' @param plot logical; if \code{FALSE}, nothing is plotted
#' @param panel.first an expression to be evaluated after the plot axes are
#'   set up but before any plotting takes place; this can be useful for drawing
#'   background grids or scatterplot smooths; note that this works by lazy
#'   evaluation: passing this argument from other plot methods may well not
#'   work since it may be evaluated too early; see also
#'   \code{\link{plot.default}}
#' @param panel.last an expression to be evaluated after plotting has taken
#'   place but before the axes, title, and box are added; see the comments
#'   about \code{panel.first}
#' 
#' @return
#' A matrix giving the coordinates of \emph{all} the bar midpoints drawn (see
#' \code{\link{barplot}}) and the \code{\link{order}} of \code{x}.
#' 
#' @examples
#' set.seed(1)
#' change <- runif(20, -1, 1) * 100
#' col <- c(PD = 'red', SD = 'blue', CR = 'chartreuse4')
#' 
#' ## interpolation
#' waterfall(change, col = col)
#' 
#' ## discrete breaks
#' waterfall(change, col = as.character(cut(change, c(-Inf, -50, 50, Inf), col)))
#' legend('top', names(col), fill = col, horiz = TRUE, bty = 'n', border = NA)
#' title(xlab = 'Patient', ylab = '% change', main = 'waterfall', line = 2)
#' 
#' ## use return value to add an existing plot
#' wf <- waterfall(change, plot = FALSE)
#' text(wf[, 1L], 0, labels = sprintf('%0.1f', change[wf[, 2L]]),
#'      pos = ifelse(change[wf[, 2L]] > 0, 1, 3), cex = 0.5)
#' 
#' 
#' ## type 2
#' waterfall(change, type = 2, col = col, panel.first = grid())
#' title(main = 'waterfall - type 2')
#' axis(2, las = 1)
#' 
#' @export

waterfall <- function(x, type = 1L, col = c('red','blue'), ...,
                      arrows = type == 2L, rev = FALSE, plot = TRUE,
                      panel.first = NULL, panel.last = NULL) {
  m  <- match.call(expand.dots = FALSE)$`...`
  # op <- par(no.readonly = TRUE)
  # on.exit(par(op))
  
  o   <- order(if (rev) -x else x, na.last = NA)
  bp  <- barplot(x[o], plot = FALSE)
  rx  <- range(x, na.rm = TRUE)
  res <- cbind(mp = c(bp), order = o)
  
  if (!plot)
    return(res)

  type <- if (!type[1L] %in% 1:2) {
    warning('\'type\' should be 1 or 2 - defaulting to 1\n', call. = FALSE)
    1
  } else type[1L]
  
  col <- if (type == 2L)
    rep_len(col, 2L) else if (length(col) != length(x))
      colorRampPalette(col)(1000L)[(x - rx[1L]) / diff(rx) * 999 + 1][o] else
        col[o]
  
  plot.new()
  plot.window(
    if (is.null(m$xlim)) range(bp) else eval(m$xlim),
    if (is.null(m$ylim))
      extendrange(if (type == 2L) cumsum(x) else x) else eval(m$ylim)
  )
  panel.first

  if (type == 2L) {
    dx <- diff(c(bp))[1L] * .4
    rect(bp - dx, y0 <- cumsum(c(0, x[-length(x)])), bp + dx, y1 <- cumsum(x),
         col = col[(x > 0) + 1L], border = NA, xpd = NA)
  } else {
    y0 <- 0
    y1 <- x[o]
    barplot(x[o], border = NA, col = col, ..., xpd = NA, add = TRUE)
  }
  
  if (arrows)
    arrows(bp, y0, bp, y1, lwd = 2, length = .1, xpd = NA, col = 0L)
  panel.last
  
  invisible(res)
}

#' Enhanced heat map
#' 
#' @description
#' A heat map is a false color image (basically \code{image(t(x))}) with a
#' \code{\link{dendrogram}} added to the left side and to the top. Typically,
#' reordering of the rows and columns according to some set of values (row or
#' column means) within the restrictions imposed by the dendrogram is carried
#' out.
#' 
#' Some improvements in this version over \code{\link{heatmap}} or
#' \code{\link[gplots]{heatmap.2}} include better defaults; more control of
#' the layout, spacing, and margins; built-in \code{1-correlation}
#' distances; automatic color interpolation used in the heat map; optional
#' multi-layered vertical and horizontal color bars; a smoother interpolation
#' in the color key; and other improvements.
#' 
#' @param x a numeric matrix of the values to be plotted
#' @param Rowv,Colv logical or a \code{\link{dendrogram}} controlling the
#'   order and dendrogram along the rows and columns, respectively; for other
#'   options, see \code{\link{heatmap}}
#' @param distfun a function used to compute the distance (dissimilarity)
#'   between both rows and columns (default is \code{\link{dist}} with Euclidean
#'   distance); alternatively, any of the \code{method}s can be passed as a
#'   character string; \code{"spearman"} or \code{"pearson"} may also be used
#'   which will calculate the distance as \code{1 - cor(x)}
#' @param hclustfun a function used to compute the hierarchical clustering
#'   when \code{Rowv} or \code{Colv} are \emph{not} dendrograms; default is
#'   \code{\link{hclust}}; should take as argument a result of \code{distfun}
#'   and return an object to which \code{\link{as.dendrogram}} can be applied
#' @param symm logical; if \code{TRUE} and \code{x} is a square matrix,
#'   \code{x} is treated \strong{symm}etrically
#' @param scale character indicating if \code{x} should be centered and
#'   scaled in either the row or column direction or neither; default is none
#' @param na.rm logical; if \code{TRUE}, \code{NA}s are removed before
#'   calculating dendrogram weights; see \code{\link{reorder.dendrogram}}
#' @param revC logical; if \code{TRUE}, the column order is reversed for
#'   plotting, e.g., for the symmetric case, the symmetry axis is as usual
#' @param add.expr an \code{\link{expression}} evaluated after the call to
#'   \code{\link{image}}, useful for adding components to the plot
#' @param breaks (optional) either a numeric vector indicating the splitting
#'   points for binning \code{x} into colors, or an integer number of break
#'   points to be used; for the latter, the break points will be spaced equally
#'   between \code{min(x)} and \code{max(x)}
#' @param symbreaks logical; if \code{TRUE}, breaks are made symmetric about
#'   0; default is \code{TRUE} if \code{x} contains negative values and
#'   \code{FALSE} otherwise
#' @param cols a vector of character strings of two or more colors used for
#'   interpolation and passed to \code{\link{image}}; alternatively a function
#'   such as \code{\link{heat.colors}} taking an integer argument and returning
#'   a vector of colors
#' @param colsep,rowsep,sepcolor,sepwidth (optional) vector of integers
#'   indicating which columns or rows should be separated from the preceding
#'   columns or rows by a narrow space of \code{sepcolor}; widths of space
#'   between is given by \code{sepwidth}
#' @param cellnote (optional) a matrix having the same dimensions as \code{x}
#'   with text to be plotted in each cell
#' @param notecex,notecol size and color for \code{cellnote}
#' @param na.color color used for \code{NA} values; defaults to background
#'   color \code{par('bg')}
#' @param trace character string indicating whether a solid "trace" line
#'   should be drawn across rows or down columns; the distance of the line
#'   from the center of each color-cell is proportional to the size of the
#'   measurement; one of \code{"row"}, \code{"column"}, \code{"both"}, or
#'   \code{"none"} (default)
#' @param tracecol color for \code{trace} line
#' @param hline,vline,linecol a vector of values within cells where a
#'   horizontal or vertical dotted line should be drawn; the color of the
#'   line is controlled by \code{linecol} (default is \code{tracecol})
#' 
#'   horizontal lines are only plotted if \code{trace} is "row" or "both";
#'   vertical lines are only drawn if \code{trace} "column" or "both"; both
#'   \code{hline} and \code{vline} default to the median of the \code{breaks}
#' @param margins numeric vector of length 2 controlling the margins for
#'   column and row names, respectively
#' @param dmargins numeric vector of length 2 controlling the margins for
#'   column and row dendrograms, respectively; useful for "squishing"
#' @param ColSideColors,RowSideColors (optional) character vector or matrix
#'   with color names for horizontal or vertical side bars useful for annotating
#'   columns and/or rows of \code{x}
#' @param ColSideColorsSize,RowSideColorsSize numeric value controlling the
#'   sizes of the horizontal and vertical side bars, respectively
#' @param side.height.fraction scaling factor for height and width of bars
#' @param labRow,labCol row and column labels; defaults to row and column
#'   names of \code{x}; \code{FALSE} suppresses labels
#' @param labRowCol,labColCol (optional) vectors of colors for row and column
#'   labels, recycled as needed
#' @param cexRow,cexCol size for row and column labels
#' @param key logical; if \code{TRUE}, a color key is drawn
#' @param key.cex numeric value controlling the size of the color key
#' @param key.title,key.sub main and sub titles for the color key
#' @param density.info character string indicating whether to supermipose a
#'   "histogram" or a "density" plot on the color key; default is "none"
#' @param denscol color for \code{density.info}; default is \code{tracecol}
#' @param symkey logical; if \code{TRUE}, color key will be symmetric about
#'   0; default is \code{TRUE} if \code{x} contains negative values and
#'   \code{FALSE} otherwise
#' @param densadj numeric scaling value for tuning the kernel width when a
#'   density plot is drawn on the color key; see the \code{adjust} parameter
#'   in \code{\link{density}}; default is 0.25
#' @param main,xlab,ylab main, x-, and y-axis labels
#' @param lmat,lhei,lwid (optional) arguments for controlling \strong{l}ayout
#' \strong{hei}ghts and \code{wid}ths, passed to \code{\link{layout}}
#' @param ... additional arguments passed to \code{\link{image}} or further
#'   to \code{\link{plot}}
#' 
#' @seealso
#' \code{\link{heatmap}}; \code{\link[gplots]{heatmap.2}}; \code{\link{image}};
#' \code{\link{dendrogram}}; \code{\link{dist}}; \code{\link{hclust}}
#' 
#' @examples
#' x <- scale(as.matrix(mtcars))
#' 
#' heatmap.3(x)
#' heatmap.3(x, cols = 'heat.colors')
#' heatmap.3(x, cols = c('blue4', 'grey95', 'tomato'))
#' 
#' heatmap.3(x, distfun = dist)        ## default ('euclidean')
#' heatmap.3(x, distfun = 'euclidean') ## passed to dist
#' heatmap.3(x, distfun = 'manhattan') ## same
#' heatmap.3(x, distfun = 'spearman')  ## calculated 1 - cor
#' heatmap.3(x, distfun = 'pearson')   ## same
#' 
#' 
#' ## example from ?stats::heatmap
#' x <- as.matrix(mtcars)
#' rc <- rainbow(nrow(x), start = 0, end = .3)
#' cc <- rainbow(ncol(x), start = 0, end = .3)
#' 
#' hm <- heatmap.3(
#'   x, cols = cm.colors(256), scale = 'column',
#'   RowSideColors = rc, ColSideColors = cc, margins = c(5, 10)
#' )
#' str(hm) ## the two re-ordering index vectors
#' 
#' 
#' ## multiple variables for row and/or column colors
#' rc <- cbind(gear = x[, 'gear'], am = x[, 'am'], vs = x[, 'vs'])
#' rc[] <- palette()[rc + 2L]
#' 
#' cc <- rbind(var1 = nchar(colnames(x)), var2 = nchar(sort(colnames(x))))
#' cc[] <- palette()[cc]
#' 
#' hm <- heatmap.3(
#'   x, scale = 'column',
#'   distfun = 'spearman', hclustfun = 'ward.D2',
#'   RowSideColors = rc, ColSideColors = cc,
#'   margins = c(5, 10), labRowCol = rc[, 3], labColCol = cc[1, ],
#'   colsep = c(2, 6), rowsep = c(9, 14, 21), sepwidth = c(5, 2)
#' )
#' 
#' 
#' ## layout control
#' hm$layout
#' 
#' heatmap.3(
#'   ## same as above
#'   x, scale = 'column',
#'   distfun = 'spearman', hclustfun = 'ward.D2',
#'   RowSideColors = rc, ColSideColors = cc,
#'   
#'   ## less "squish" of dendrograms
#'   dmargins = c(2, 2),
#'   
#'   ## more room for heat map in layout
#'   key = FALSE, margins = c(3, 8),
#'   
#'   ## fine control over layout heights and widths
#'   lhei = c(7, 2, 30),
#'   lwid = c(5, 1, 30),
#'   
#'   cellnote = x
#' )
#' 
#' @export

heatmap.3 <- function(x,
                      
                      ## dendrogram
                      Rowv = TRUE, Colv = if (symm) 'Rowv' else TRUE,
                      distfun = dist, hclustfun = hclust, symm = FALSE,
                      
                      ## data scaling
                      scale = c('none', 'row', 'column'), na.rm = TRUE,
                      
                      ## image plot
                      revC = identical(Colv, 'Rowv'), add.expr = NULL,
                      
                      ## mapping data to colors
                      breaks = NULL,
                      symbreaks = max(x < 0, na.rm = TRUE) || scale != 'none',
                      cols = c('blue', 'white', 'red'),
                      
                      ## block separation
                      colsep = NULL, rowsep = NULL,
                      sepcolor = par('bg'), sepwidth = c(1, 1),
                      
                      ## cell labeling
                      cellnote = NULL, notecex = 1,
                      notecol = 'cyan', na.color = par('bg'),
                      
                      ## level trace
                      trace = c('none', 'column', 'row', 'both'),
                      tracecol = 'cyan',
                      hline = median(breaks), vline = median(breaks),
                      linecol = tracecol,
                      
                      ## row/column labeling
                      margins = c(5, 5), dmargins = c(5, 5),
                      ColSideColors = NULL, RowSideColors = NULL,
                      ColSideColorsSize = 1, RowSideColorsSize = 1,
                      side.height.fraction = 0.3,
                      labRow = NULL, labCol = NULL,
                      labRowCol = NULL, labColCol = NULL,
                      cexRow = 0.2 + 1 / log10(nr),
                      cexCol = 0.2 + 1 / log10(nc),
                      
                      ## color key and density info
                      key = TRUE, key.cex = 1.5,
                      key.title = if (density.info %in% c('density', 'histogram'))
                        NULL else 'Color Key', key.sub = NULL,
                      density.info = c('none', 'histogram', 'density'),
                      denscol = tracecol,
                      symkey = max(x < 0, na.rm = TRUE) || symbreaks,
                      densadj = 0.25,
                      
                      ## plot labels
                      main = NULL, xlab = NULL, ylab = NULL,
                      
                      ## plot layout
                      lmat = NULL, lhei = NULL, lwid = NULL,
                      
                      ...) {
  invalid <- function (x) {
    if (missing(x) || is.null(x) || length(x) == 0L)
      return(TRUE)
    if (is.list(x))
      all(sapply(x, invalid))
    else if (is.vector(x))
      all(is.na(x))
    else FALSE
  }
  scale01 <- function(x, low = min(x), high = max(x))
    (x - low) / (high - low)
  plot.null <- function() {
    op <- par(mar = c(0,0,0,0))
    on.exit(par(op))
    try(plot.new())
  }
  
  if (is.character(distfun)) {
    dmethod <- match.arg(distfun, c('euclidean', 'maximum', 'manhattan',
                                    'canberra',  'binary',  'minkowski',
                                    'spearman',  'pearson'))
    if (dmethod %in% c('spearman', 'pearson'))
      distfun <- function(x)
        as.dist(1 - cor(t(x), method = dmethod))
    else {
      distfun <- dist
      formals(distfun)$method <- dmethod
    }
  }
  
  if (is.character(hclustfun)) {
    hmethod   <- hclustfun
    hclustfun <- hclust
    formals(hclustfun)$method <- hmethod
  }
  
  x <- as.matrix(x)
  scale <- if (symm && missing(scale))
    'none' else match.arg(scale)
  trace <- match.arg(trace)
  density.info <- match.arg(density.info)
  
  if (is.character(cols))
    cols <- if (length(cols) == 1L)
      get(cols, mode = 'function') else colorRampPalette(cols)
  if (!missing(breaks) && (scale != 'none'))
    warning('Using scale = \'row\' or scale = \'column\' when breaks are ',
            'specified can produce unpredictable results - use only one')
  
  if (is.null(Rowv) || is.na(Rowv))
    Rowv <- FALSE
  if (is.null(Colv) || is.na(Colv))
    Colv <- FALSE
  else if (identical(Colv, 'Rowv') && !isTRUE(Rowv))
    Colv <- FALSE
  if (length(dim(x)) != 2L || !is.numeric(x))
    stop('\'x\' must be a numeric matrix')
  
  nr <- nrow(x)
  nc <- ncol(x)
  if (nr <= 1L || nc <= 1L)
    stop('\'x\' must have at least 2 rows and 2 columns')
  if (!is.numeric(margins) || length(margins) != 2L)
    stop('\'margins\' must be a numeric vector of length 2')
  if (is.null(cellnote))
    cellnote <- matrix(NA, nrow(x), ncol(x))
  
  if (inherits(Rowv, 'dendrogram')) {
    ddr <- Rowv
    rowInd <- order.dendrogram(ddr)
  } else if (is.integer(Rowv)) {
    hcr <- hclustfun(distfun(x))
    ddr <- as.dendrogram(hcr)
    ddr <- reorder(ddr, Rowv)
    rowInd <- order.dendrogram(ddr)
    if (nr != length(rowInd))
      stop('Row dendrogram ordering gave index of wrong length')
  } else if (isTRUE(Rowv)) {
    Rowv <- rowMeans(x, na.rm = na.rm)
    hcr <- hclustfun(distfun(x))
    ddr <- as.dendrogram(hcr)
    ddr <- reorder(ddr, Rowv)
    Rowv <- TRUE
    rowInd <- order.dendrogram(ddr)
    if (nr != length(rowInd))
      stop('Row dendrogram ordering gave index of wrong length')
  } else rowInd <- rev(seq.int(nr))
  
  if (inherits(Colv, 'dendrogram')) {
    ddc <- Colv
    colInd <- order.dendrogram(ddc)
  } else if (identical(Colv, 'Rowv')) {
    if (nr != nc)
      stop('Colv == Rowv but nrow(x) != ncol(x)')
    if (exists('ddr')) {
      ddc <- ddr
      colInd <- order.dendrogram(ddc)
    } else colInd <- rowInd
  } else if (is.integer(Colv)) {
    hcc <- hclustfun(distfun(if (symm) x else t(x)))
    ddc <- as.dendrogram(hcc)
    ddc <- reorder(ddc, Colv)
    colInd <- order.dendrogram(ddc)
    if (nc != length(colInd))
      stop('Column dendrogram ordering gave index of wrong length')
  } else if (isTRUE(Colv)) {
    Colv <- colMeans(x, na.rm = na.rm)
    hcc <- hclustfun(distfun(if (symm) x else t(x)))
    ddc <- as.dendrogram(hcc)
    ddc <- reorder(ddc, Colv)
    Colv <- TRUE
    colInd <- order.dendrogram(ddc)
    if (nc != length(colInd))
      stop('column dendrogram ordering gave index of wrong length')
  } else colInd <- seq.int(nc)
  
  res <- list()
  res$call   <- match.call()
  res$rowInd <- rowInd
  res$colInd <- colInd
  x <- x[rowInd, colInd]
  x.unscaled <- x
  cellnote <- cellnote[rowInd, colInd]
  
  ## row/column labels
  label_rn <- !identical(labRow, FALSE)
  label_cn <- !identical(labCol, FALSE)
  labRow <- if (is.null(labRow)) {
    if (is.null(rownames(x)))
      seq.int(nr)[rowInd] else rownames(x)
  } else labRow[rowInd]
  labCol <- if (is.null(labCol)) {
    if (is.null(colnames(x)))
      seq.int(nc)[colInd] else colnames(x)
  } else labCol[colInd]
  
  labRowCol <- if (is.null(labRowCol))
    1L else rep_len(drop(labRowCol), nr)[rowInd]
  labColCol <- if (is.null(labColCol))
    1L else rep_len(drop(labColCol), nc)[colInd]
  
  ## scaling
  if (scale == 'row') {
    res$rowMeans <- rm <- rowMeans(x, na.rm = na.rm)
    x <- sweep(x, 1L, rm)
    res$rowSDs <- sx <- apply(x, 1L, sd, na.rm = na.rm)
    x <- sweep(x, 1L, sx, '/')
  } else if (scale == 'column') {
    res$colMeans <- rm <- colMeans(x, na.rm = na.rm)
    x <- sweep(x, 2L, rm)
    res$colSDs <- sx <- apply(x, 2L, sd, na.rm = na.rm)
    x <- sweep(x, 2L, sx, '/')
  }
  
  ## breaks
  if (is.null(breaks) || length(breaks) < 1L)
    breaks <- if (missing(cols) || is.function(cols))
      16L else length(cols) + 1L
  if (length(breaks) == 1L) {
    breaks <- if (!symbreaks)
      seq(min(x, na.rm = na.rm), max(x, na.rm = na.rm), length.out = breaks)
    else {
      extreme <- max(abs(x), na.rm = TRUE)
      seq(-extreme, extreme, length.out = breaks)
    }
  }
  
  nbr  <- length(breaks)
  ncol <- length(breaks) - 1L
  
  if (inherits(cols, 'function'))
    cols <- cols(ncol)
  min.breaks <- min(breaks)
  max.breaks <- max(breaks)
  
  x[x < min.breaks] <- min.breaks
  x[x > max.breaks] <- max.breaks
  
  if (identical(RowSideColors, FALSE))
    RowSideColors <- NULL
  RowSideColors <- if (!is.null(RowSideColors) & !is.matrix(RowSideColors))
    matrix(RowSideColors, ncol = nr)
  else if (is.null(RowSideColors)) NULL else t(RowSideColors)
  
  if (identical(ColSideColors, FALSE))
    ColSideColors <- NULL
  ColSideColors <- if (!is.null(ColSideColors) & !is.matrix(ColSideColors))
    matrix(ColSideColors, nrow = nc)
  else if (is.null(ColSideColors)) NULL else t(ColSideColors)
  
  if (is.null(lhei))
    lhei <- c(key.cex, side.height.fraction * ColSideColorsSize / 2, 4)
  if (is.null(lwid))
    lwid <- c(key.cex, side.height.fraction * RowSideColorsSize / 2, 4)
  
  cscm <- rscm <- TRUE
  if (is.null(lmat)) {
    # lmat <- matrix(c(6,0,4, 0,0,2, 5,3,1), 3L)
    lmat <- matrix(c(6,0,4, 0,0,1, 5,2,3), 3L)
    
    if (is.null(ColSideColors)) {
      cscm <- FALSE
      ColSideColors <- matrix(0L, nc, 1L)
      lhei[2L] <- 0.01
    } else if (nrow(ColSideColors) != nc)
      stop(sprintf('\'ColSideColors\' must be a %s x %s matrix', nr, nc))
    
    if (is.null(RowSideColors)) {
      rscm <- FALSE
      RowSideColors <- matrix(0L, 1L, nr)
      lwid[2L] <- 0.01
    } else if (ncol(RowSideColors) != nr)
      stop(sprintf('\'RowSideColors\' must be a %s x %s matrix', nr, nc))
  }
  
  stopifnot(
    length(lwid) == 3L,
    length(lhei) == 3L,
    dim(lmat) == c(3L, 3L)
  )
  
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.flush()
  })
  dev.hold()
  
  layout(lmat, widths = lwid, heights = lhei, respect = FALSE)
  
  ## 1 - matrix of colors on left
  if (!is.null(RowSideColors)) {
    rsc <- t(RowSideColors[, rowInd, drop = FALSE])
    rsc[] <- rf <- factor(rsc)
    class(rsc) <- 'integer'
    
    if (rscm) {
      par(mar = c(margins[1L], 0, 0, 0.5))
      image(t(rsc), col = levels(rf), axes = FALSE)
      
      if (length(rownames(RowSideColors)))
        axis(1L, 0:(ncol(rsc) - 1L) / max(1, (ncol(rsc) - 1L)),
             rownames(RowSideColors), las = 2L, tick = FALSE)
    } else plot.null()
  }
  
  
  ## 2 - matrix of colors on top
  if (!is.null(ColSideColors)) {
    csc <- ColSideColors[colInd, , drop = FALSE]
    csc[] <- cf <- factor(csc)
    class(csc) <- 'integer'
    
    if (cscm) {
      par(mar = c(0.5, 0, 0, margins[2L]))
      image(csc, col = levels(cf), axes = FALSE)
      
      if (length(colnames(ColSideColors)))
        axis(2L, 0:(ncol(csc) - 1L) / max(1, (ncol(csc) - 1L)),
             colnames(ColSideColors), las = 2L, tick = FALSE)
    } else plot.null()
  }
  
  
  ## 3 - main image, text, labels
  x <- t(x)
  cellnote <- t(cellnote)
  if (revC) {
    iy <- rev(seq.int(nr))
    if (exists('ddr'))
      ddr <- rev(ddr)
    x <- x[, iy]
    cellnote <- cellnote[, iy]
  } else iy <- seq.int(nr)
  
  par(mar = c(margins[1L], 0, 0, margins[2L]))
  image(seq.int(nc), seq.int(nr), x,
        xlim = 0.5 + c(0, nc), ylim = 0.5 + c(0, nr),
        axes = FALSE, xlab = '', ylab = '',
        col = cols, breaks = breaks, ...)
  res$carpet <- x
  
  if (exists('ddr'))
    res$rowDendrogram <- ddr
  if (exists('ddc'))
    res$colDendrogram <- ddc
  res$breaks <- breaks
  res$cols   <- cols
  
  if (!invalid(na.color) & any(is.na(x))) {
    mmat <- ifelse(is.na(x), 1, NA)
    image(seq.int(nc), seq.int(nr),
          mmat, axes = FALSE, xlab = '', ylab = '',
          col = na.color, add = TRUE)
  }
  
  
  ## rowsep and colsep on top of matrix and bars
  sepwidth <- rep_len(sepwidth, 2L)
  if (!is.null(colsep))
    abline(v = colsep + 0.5, col = sepcolor,
           lwd = sepwidth[1L], xpd = NA)
  if (!is.null(rowsep))
    abline(h = nr - rowsep + 0.5, col = sepcolor,
           lwd = sepwidth[2L], xpd = NA)
  
  
  ## labels
  if (label_cn)
    text(seq.int(nc), par('usr')[3L], labCol, col = labColCol,
         las = 2L, cex = cexCol, pos = 1L, xpd = NA)
  if (label_rn)
    text(par('usr')[2L], iy, labRow, col = labRowCol,
         las = 2L, cex = cexRow, pos = 4L, xpd = NA)
  
  mtext(side = 1L, xlab, line = margins[1L] - 1.25)
  mtext(side = 4L, ylab, line = margins[2L] - 1.25)
  
  
  eval(substitute(add.expr))
  
  
  min.scale <- min(breaks)
  max.scale <- max(breaks)
  x.scaled  <- scale01(t(x), min.scale, max.scale)
  
  if (trace %in% c('both', 'column')) {
    res$vline  <- vline
    vline.vals <- scale01(vline, min.scale, max.scale)
    
    for (i in colInd) {
      if (!is.null(vline))
        abline(v = i - 0.5 + vline.vals, col = linecol, lty = 2L)
      xv <- rep_len(i, nrow(x.scaled)) + x.scaled[, i] - 0.5
      xv <- c(xv[1L], xv)
      yv <- seq_along(xv) - 0.5
      lines(xv, yv, lwd = 1, col = tracecol, type = 's')
    }
  }
  
  if (trace %in% c('both', 'row')) {
    res$hline  <- hline
    hline.vals <- scale01(hline, min.scale, max.scale)
    
    for (i in rowInd) {
      if (!is.null(hline))
        abline(h = i + hline, col = linecol, lty = 2L)
      yv <- rep_len(i, ncol(x.scaled)) + x.scaled[i, ] - 0.5
      yv <- rev(c(yv[1L], yv))
      xv <- rev(seq_along(yv)) - 0.5
      lines(xv, yv, lwd = 1, col = tracecol, type = 's')
    }
  }
  
  if (!all(is.na(cellnote)))
    text(c(row(cellnote)), c(col(cellnote)), labels = c(cellnote),
         col = notecol, cex = notecex)
  
  
  ## 4 - row dendrogram
  if (Rowv) {
    par(mar = c(margins[1L], dmargins[1L], 0, 0))
    plot(ddr, horiz = TRUE, axes = FALSE, yaxs = 'i', leaflab = 'none')
  } else plot.null()
  
  
  ## 5 - column dendrogram
  if (Colv) {
    par(mar = c(0, 0, (if (!is.null(main)) 1 else 0) +
                  dmargins[2L], margins[2L]))
    plot(ddc, axes = FALSE, xaxs = 'i', leaflab = 'none')
  } else plot.null()
  
  if (!is.null(main))
    title(main, cex.main = 1.5 * op[['cex.main']])
  
  
  ## 6 - color key
  if (key) {
    par(mar = c(3, 2, 2, 1), cex = 0.75)
    tmpbreaks <- breaks
    
    if (symkey) {
      max.raw <- max(abs(c(x, breaks)), na.rm = TRUE)
      min.raw <- -max.raw
      tmpbreaks[1L] <- -max(abs(x), na.rm = TRUE)
      tmpbreaks[length(tmpbreaks)] <- max(abs(x), na.rm = TRUE)
    } else {
      min.raw <- min(x, na.rm = TRUE)
      max.raw <- max(x, na.rm = TRUE)
    }
    
    z <- seq(min.raw, max.raw, length.out = 1000L)
    image(z = matrix(z, ncol = 1L), col = col_scaler(z, cols),
          xaxt = 'n', yaxt = 'n')
    
    par(usr = c(0, 1, 0, 1))
    lv <- pretty(breaks)
    xv <- scale01(as.numeric(lv), min.raw, max.raw)
    axis(1L, at = xv, labels = lv)
    
    if (scale == 'row')
      mtext(side = 1L, key.sub %||% 'Row Z-Score', line = 2)
    else if (scale == 'column')
      mtext(side = 1L, key.sub %||% 'Column Z-Score', line = 2)
    else mtext(side = 1L, key.sub %||% 'Value', line = 2)
    
    if (density.info == 'density') {
      dens <- density(x, adjust = densadj, na.rm = TRUE)
      omit <- dens$x < min(breaks) | dens$x > max(breaks)
      dens$x <- dens$x[-omit]
      dens$y <- dens$y[-omit]
      dens$x <- scale01(dens$x, min.raw, max.raw)
      
      lines(dens$x, dens$y / max(dens$y) * 0.95, col = denscol, lwd = 1)
      axis(2L, pretty(dens$y) / max(dens$y) * 0.95, pretty(dens$y))
      title(key.title %||% 'Color Key\nand Density Plot')
      mtext(side = 2L, 'Density', line = 2, cex = 0.5)
      
    } else if (density.info == 'histogram') {
      h  <- hist(x, plot = FALSE, breaks = breaks)
      hx <- scale01(breaks, min.raw, max.raw)
      hy <- c(h$counts, h$counts[length(h$counts)])
      
      lines(hx, hy / max(hy) * 0.95, lwd = 1, type = 's', col = denscol)
      axis(2L, pretty(hy) / max(hy) * 0.95, pretty(hy))
      title(key.title %||% 'Color Key\nand Histogram')
      mtext(side = 2L, 'Count', line = 2, cex = 0.5)
    } else title(key.title %||% 'Color Key')
    
  } else plot.null()
  
  res$colorTable <- data.frame(
    low  = res$breaks[-length(res$breaks)],
    high = res$breaks[-1L], color = res$cols
  )
  attr(lmat, 'key') <-
    paste(1:6, c('row color', 'col color', 'matrix',
                 'row dend', 'col dend', 'key'),
          sep = ': ')
  res$layout <- list(lmat = lmat, lhei = lhei, lwid = lwid)
  
  invisible(res)
}

vioplot <- function(x, range = 1.5, xlim = NULL, ylim = NULL, names,
                    plot = TRUE, axes = TRUE, frame.plot = axes,
                    horizontal = FALSE, viocol = 'grey90', border = par('fg'),
                    lty = 1L, lwd = 1, boxcol = border, at, add = FALSE,
                    viowex = 1, boxwex = viowex / 4, boxplot = FALSE,
                    dFUN = c('density', 'sm.density'), quantiles = NULL, ...) {
  x <- ox <- if (inherits(x, 'list'))
    x else if (is.data.frame(x))
      as.list(x) else list(x)
  n <- length(x)
  at <- if (missing(at)) 
    seq_along(x) else at
  label <- if (missing(names))
    seq_along(x) else names
  dFUN <- match.arg(dFUN)
  
  res   <- boxplot(x, plot = FALSE, horizontal = horizontal, names = label)
  stats <- res$stats
  
  if (!plot)
    return(invisible(res))
  
  ylim <- if (is.null(ylim))
    range(c(c(stats), res$out)) else ylim
  xlim <- if (is.null(xlim))
    if (n == 1L)
      at + c(-0.5, 0.5) else range(at) + c(-0.5, 0.5)
  else xlim
  
  if (!add) {
    plot.new()
    op <- par(..., no.readonly = TRUE)
    on.exit(par(op))
    
    if (horizontal)
      plot.window(ylim, xlim) else plot.window(xlim, ylim)
    if (axes) {
      axis((!horizontal) + 1L)
      axis(horizontal + 1L, at, label)
    }
    if (frame.plot)
      box()
  }
  
  for (ii in seq_along(x)) {
    xl <- range(ox[[ii]], na.rm = TRUE)
    dn <- tryCatch(
      switch(
        dFUN,
        density    = stats::density(ox[[ii]], from = xl[1L], to = xl[2L]),
        sm.density = sm::sm.density(ox[[ii]], xlim = xl, display = 'none')
      ),
      error = function(e) {
        warning(e$message, call. = FALSE)
        list(NA, NA)
      }
    )
    
    x <- dn[[1L]]
    y <- dn[[2L]] / max(dn[[2L]]) * viowex / 5
    l <- list(x = c(at[ii] - y, rev(at[ii] + y)), y = c(x, rev(x)))
    
    if (horizontal)
      names(l) <- rev(names(l))
    
    if (!identical(x, NA))
      do.call('polygon', c(l, col = viocol, border = border))
    
    if (lunique(ox[[ii]]) > 1 && length(quantiles)) {
      f <- stats::approxfun(x, y)
      z <- quantile(ox[[ii]], probs = quantiles)
      segments(at + f(z), z, at - f(z), col = boxcol, lty = lty, lwd = lwd)
    }
    
    boxplot(
      ox[[ii]], at = at[ii], add = TRUE, axes = FALSE, plot = boxplot,
      border = border, boxcol = boxcol, outline = FALSE, boxwex = boxwex,
      horizontal = horizontal
    )
  }
  
  invisible(res)
}

#' Binomial confidence interval plot
#' 
#' Wrapper function to create table of binomial confidence intervals and draw
#' as a forest plot.
#' 
#' @param data a matrix or data frame with variables \code{varname} and
#'   \code{byvar}
#' @param varname,byvar one or more variables in \code{data} to calculate
#'   binomial confidence intervals by \code{byvar}; the rows variable(s) of
#'   the table;
#' 
#'   note that \code{varname} variables should be factor-like, and \code{byvar}
#'   should be binary
#' @param varname_label,byvar_label optional labels for each \code{varname}
#'   and \code{byvar}
#' @param main,xlab the x-axis and title labels
#' @param col a vector of colors for each \code{varname}; the first will be
#'   used for the overall (if \code{show_overall = TRUE})
#' @param conf confidence level; passed to \code{\link{binconr}}
#' @param digits the number of places past the decimal to keep
#' @param show_overall,show_missing logical; if \code{TRUE}, rows with the
#'   overall and missing value confidence intervals are shown, respectively
#' @param alpha_missing if \code{show_missing = TRUE}, the amount of alpha
#'   transparency added to missing value rows; passed to \code{\link{tcol}}
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @examples
#' dat <- mtcars[sample(nrow(mtcars), 100L, TRUE), ]
#' dat[1, 2] <- NA
#' vv  <- c('cyl', 'vs', 'gear', 'carb')
#' dat$gear <- factor(dat$gear, 3:6)
#' 
#' bplot(
#'   dat, vv, 'am',
#'   col = c(0, seq_along(vv)) + 1L,
#'   main = 'Manual rate', xlab = 'Proportion of manual'
#' )
#' 
#' bplot(
#'   dat, setNames(vv, case(vv)), 'am',
#'   col = c('red', 'grey50', 'blue', 'grey50', 'blue'),
#'   conf = 0.9, alpha_missing = 0.4
#' )
#' 
#' @export

bplot <- function(data, varname, byvar, varname_label = names(varname),
                  byvar_label = names(byvar), main = byvar_label,
                  xlab = byvar_label, col = c('dodgerblue4', 'dodgerblue2'),
                  conf = 0.95, digits = 2L, show_overall = TRUE,
                  show_missing = TRUE, alpha_missing = 1, ...) {
  varname_label <- varname_label %||% varname
  byvar_label <- byvar_label %||% byvar
  
  if (show_overall) {
    data$Overall <- 'Overall'
    varname <- c('Overall', varname)
    varname_label <- c('Overall', varname_label)
  }
  
  ff <- lapply(seq_along(varname), function(ii) {
    res <- binconr_(
      data[, byvar], data[, varname[ii]], lbl = varname_label[ii],
      conf = conf, digits = digits
    )
    do.call('rbind', res$num)
  })
  nn <- sapply(ff, nrow)
  ff <- do.call('rbind', ff)
  
  tt <- lapply(seq_along(varname), function(ii) {
    res <- binconr_(
      data[, byvar], data[, varname[ii]], lbl = varname_label[ii],
      conf = conf, digits = digits
    )
    do.call('rbind', res$txt)
  })
  tt <- do.call('rbind', tt)
  
  na <- is.nan(ff[, 1L])
  ff[na, ] <- NA
  tt <- replace(tt, na, '-')
  
  a <- rownames(ff) %in% 'Missing'
  if (!show_missing) {
    ff <- ff[!a, ]
    tt <- tt[!a, , drop = FALSE]
    a  <- rep_len(FALSE, nrow(tt))
  }
  
  a <- if (isTRUE(alpha_missing))
    c(1, 0.4)[a + 1L]
  else if (is.numeric(alpha_missing))
    c(1, alpha_missing)[a + 1L]
  else rep_len(1, length(a))
  
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.flush()
  })
  dev.hold()
  
  lo <- layout(t(c(2, 1, 3)), widths = c(3, 3.5, 3))
  par(mar = c(5, 0, 3, 0), oma = c(0, 1, 0, 1))
  par(...)
  
  col <- if (show_overall & length(col) == 2L)
    rep(col, c(1L, length(nn) - 1L)) else rep_len(col, length(nn))
  col <- rep(col, nn)
  
  y <- rev(seq.int(nrow(ff)))
  i <- if (show_overall)
    y == max(y) else rep_len(FALSE, length(y))
  
  plot(
    ff[, 1L], y, xlim = c(0, 1), bty = 'n', axes = FALSE, ann = FALSE,
    pch = ifelse(i, 18L, 16L), col = tcol(col, alpha = a), cex = i + 2L,
    panel.first = {
      bars_(y)
      abline(v = ff[1L, 1L], lwd = 2, lty = 2L, col = 'grey')
      arrows(ff[, 2L], y, ff[, 3L], code = 3L, angle = 90, length = 0.05,
             lwd = 1.5, col = tcol(c('grey50', 'black')[i + 1L], alpha = a))
    },
    panel.last = {
      axis(1L, cex.axis = 1.5)
      title(xlab = xlab, cex.lab = 1.5)
      title(main = main, cex.main = 1.5, line = 0.5)
    }
  )
  
  x <- rep_len(0, length(y))
  par(mar = c(5, 2, 3, 4), xpd = NA)
  plot(x, y, type = 'n', ann = FALSE, axes = FALSE)
  title(main = 'Subgroup', cex.main = 1.5, adj = 0, xpd = NA, line = 0.5)
  
  pad <- 0.8
  lag <- (is.na(ff[, 1L]) & !na) | y == max(y)
  text(x - ifelse(lag, 1, pad), y, rownames(ff), font = i + 1L,
       col = tcol('black', alpha = a), adj = 0, cex = 1.5)
  
  x <- rep_len(0, length(y))
  plot(x, y, type = 'n', ann = FALSE, axes = FALSE)
  title(main = sprintf('Point estimate (%s%% CI)', conf * 100),
        cex.main = 1.5, xpd = NA, line = 0.5)
  text(x, y, tt[, 1L], col = tcol('black', alpha = a),
       cex = 1.5, font = ifelse(i, 2L, 1L), xpd = NA)
  box('outer')
  
  invisible(list(ff, tt))
}

#' @rdname bplot
#' @export
tabler_bincon <- bplot

bars_ <- function(x, cols = c(grey(0.95), NA), horiz = TRUE, fullspan = TRUE) {
  ## forest:::bars
  p <- if (fullspan)
    c(grconvertX(c(0.025, 0.975), 'ndc'), grconvertY(c(0, 1), 'ndc'))
  else par('usr')
  
  cols <- rep_len(cols, length(x))
  x <- rev(x) + 0.5
  
  if (horiz)
    rect(p[1L], x - 1L, p[2L], x, border = NA, col = cols, xpd = NA)
  else rect(x - 1L, p[3L], x, p[4L], border = NA, col = rev(cols), xpd = NA)
  
  invisible(NULL)
}

binconr_ <- function(outcome, variable, addNA = TRUE, lbl = NULL,
                     conf = 0.95, digits = 2L) {
  # binconr_(mtcars$vs, mtcars$gear)
  pad <- if (all(variable == 'Overall')) {
    lbl <- NULL
    NULL
  } else NA
  na <- is.na(outcome)
  
  outcome  <- as.integer(as.factor(outcome))[!na]
  variable <- as.factor(variable)[!na]
  
  if (addNA) {
    variable <- addNA(variable, TRUE)
    levels(variable)[is.na(levels(variable))] <- 'Missing'
  }
  
  sp <- split(outcome, variable)
  
  txt <- lapply(sp, function(x)
    binconr(sum(x - 1L), length(x), frac = TRUE, conf = conf, digits = digits,
            show_conf = FALSE, est = TRUE, pct.sign = TRUE, percent = FALSE)
  )
  num <- lapply(sp, function(x)
    bincon(sum(x - 1L), length(x), alpha = 1 - conf)[3:5]
  )
  
  list(
    num = setNames(c(pad, num), c(lbl, names(num))),
    txt = setNames(c(pad, txt), c(lbl, names(num)))
  )
}

#' pplot
#' 
#' A paired plot for continuous data. Draws a dot plot with points jittered
#' to the left and right about the group position. Jittering is done with
#' \code{\link[beeswarm]{beeswarm}}; however, the original data values are
#' used instead of using the slight adjustments made by \code{beeswarm}.
#' Additionally, a two-sample \code{\link{wilcox.test}} test is performed
#' on each pair by default, but the test and assumptions may be controlled
#' using \code{test} and \code{args.test} arguments, respectively; see
#' examples.
#' 
#' @param formula a \code{\link{formula}}, such as \code{y ~ group1 + group2},
#'   where y is a numeric vector of data values to be split into n groups
#'   (\code{group1}), each group split into at most two groups (\code{group2})
#' @param data a data frame (or list) from which the variables in
#'   \code{formula} should be taken
#' @param x for the default method, a list of groups each having a list of
#'   length 2 of data values
#' @param at the x-axis group positions
#' @param pad the padding between \code{at} for each group and the points
#' @param test logical or function; if \code{TRUE}, a \code{\link{wilcox.test}}
#'   p-value is added for each group
#' 
#'   alternatively, a function (or function name as a character string) can be
#'   used, e.g., \code{test = t.test} or \code{function(x, y)
#'   t.test(x, y)}; note that if \code{test} is a function, it must have
#'   at least two arguments with the two groups of data; see examples
#' @param args.test a \emph{named} list of arguments passed to \code{test}
#' @param col,pch,cex color, plotting character, and character expansion
#'   vectors, each may be length 1, 2, or total number of groups to be plotted,
#'   recycled as needed
#' @param args.beeswarm a \emph{named} list of arguments passed to
#'   \code{\link[beeswarm]{beeswarm}} to override defaults
#' @param names group labels having the same length as \code{l}
#' @param quantiles optional probabilities passed to \code{\link{quantile}}
#'   to show for each group
#' @param pch.quantiles plotting character for each quantile
#' @param xlab,ylab x- and y-axis labels
#' @param legend.text group labels of length 2
#' @param ... for the \code{formula} method, named arguments to be passed to
#'   the default method
#' 
#'   for the default method, additional arguments passed to \code{\link{tplot}}
#'   or graphical parameters passed to \code{\link{par}}
#' 
#' @examples
#' ## basic usage
#' pplot(split(mtcars$mpg, mtcars$vs))
#' pplot(mpg ~ vs, mtcars)
#' 
#' 
#' set.seed(1)
#' x <- lapply(1:5, function(x) list(rnorm(20, 0, x), rnorm(20, 0, x)))
#' 
#' pplot(x)
#' pplot(x[1], ylim = c(-3, 3))
#' pplot(x, test = t.test, args.test = list(paired = TRUE))
#' pplot(x, col = rep(2:6, each = 2), pch = c(1, 16), legend.text = c('+', '-'))
#' pplot(x, quantiles = 0.5, legend.text = c('+', '-'))
#' 
#' 
#' ## pplot returns the object returned by test
#' pv <- pplot(
#'   x, quantiles = c(0.25, 0.5, 0.75), pch.quantiles = c(6, 18, 2),
#'   legend.text = c('+', '-'), pad = 0.1, pch = 16, bty = 'l',
#'   names = paste('group', 1:5), las = 1
#' )
#' sapply(pv, function(x) x$p.value)
#' 
#' 
#' ## use a custom test function
#' pplot(x, test = function(x, y) cor.test(x, y, method = 'spearman'))
#' 
#' moods <- function(x, y) rawr::moods.test(list(x, y))
#' pplot(x, test = moods)
#' sapply(x, function(y) do.call('moods', y)$p.value)
#' 
#' 
#' ## formula method
#' pplot(mpg ~ gear + am, mtcars)
#' pplot(mpg ~ gear + vs, mtcars, xlab = 'Gears', legend.text = c('V', 'S'))
#' 
#' @export

pplot <- function(x, ...) {
  UseMethod('pplot')
}

#' @rdname pplot
#' @export
pplot.formula <- function(formula, data, ...) {
  name <- deparse(substitute(data))
  vars <- all.vars(formula)
  data[vars[-1L]] <- lapply(data[vars[-1L]], as.factor)
  
  if (!length(vars) %in% 2:3)
    stop('invalid formula')
  if (nlevels(data[, tail(vars, 1L)]) != 2L)
    stop(sprintf('%s[, \'%s\'] must have two unique, nonmissing values',
                 name, tail(vars, 1L)))
  
  sp <- if (length(vars) == 3L) {
    sp <- split(data, data[, vars[2L]])
    lapply(sp, function(x) split(x[, vars[1L]], x[, vars[3L]]))
  } else list(split(data[, vars[1L]], data[, vars[2L]]))
  
  pplot(sp, ...)
}

#' @rdname pplot
#' @export
pplot.default <- function(x, at = seq_along(x), pad = 0.05,
                          test = wilcox.test, args.test = list(),
                          col = 1:2, pch = par('pch'), cex = par('cex'),
                          args.beeswarm = list(), names, quantiles = NULL,
                          pch.quantiles = rep_len(16L, length(quantiles)),
                          xlab = NULL, ylab = NULL, legend.text = NULL, ...) {
  if (length(x) == 2L)
    if (all(!sapply(x, islist)))
      x <- list(x)
  
  stopifnot(lengths(x) == 2L)
  
  ## base plot
  tplot(
    lapply(x, unlist), at = at, show.n = FALSE, ...,
    names = names, type = 'n', xlab = xlab %||% '', ylab = ylab
  )
  
  ul <- unlist(x, recursive = FALSE)
  ng <- length(x)
  at <- rep(at, each = 2L)
  gr <- at + c(-pad, pad)
  qq <- at + c(-pad, pad) / 3
  si <- rep(c(-1, 1), ng)
  
  col <- rep_len(col, length(ul))
  pch <- rep_len(pch, length(ul))
  cex <- rep_len(cex, length(ul))
  quantile <- length(quantiles) && !identical(quantiles, FALSE)
  
  ## each group * 2
  sapply(seq_along(ul), function(ii) {
    x <- ul[[ii]]
    if (!length(x))
      return(NULL)
    
    args <- list(side = si[ii], method = 'center', corral = 'wrap',
                 corralWidth = 1 / 2 - pad * 2)
    if (!islist(args.beeswarm))
      args.beeswarm <- list()
    
    tplot(
      x, add = TRUE, ann = FALSE, axes = FALSE, type = 'd', show.n = FALSE,
      at = gr[ii], col = col[ii], pch = pch[ii], cex = cex[ii],
      args.beeswarm = modifyList(args, args.beeswarm)
    )
    
    if (quantile) {
      xx <- rep(qq[ii], length(quantiles))
      yy <- quantile(x, quantiles, na.rm = TRUE)
      points(xx, yy, col = col[ii], pch = pch.quantiles)
    }
  })
  
  if (!identical(legend.text, FALSE)) {
    legend.text <- legend.text %||% base::names(x[[1L]])
    if (!is.null(legend.text)) {
      col <- if (col[1L] == col[2L])
        1L else col[1:2]
      legend(
        'topleft', legend = legend.text, col = col, pch = pch, bty = 'n'
      )
    }
  }
  
  if (quantile) {
    legend(
      'bottomleft', horiz = TRUE, legend = sprintf('%s%%', quantiles * 100),
      title = sprintf('quantile%s', ifelse(length(quantiles) == 1L, '', 's')),
      pch = pch.quantiles, col = 1L, bty = 'n'
    )
  }
  
  res <- NULL
  if (!identical(test, FALSE) && !is.null(test)) {
    test <- if (is.function(test) || is.character(test))
      match.fun(test)
    else if (isTRUE(test))
      wilcox.test
    else test
    
    res <- lapply(x, function(x) {
      y <- x[[2L]]
      x <- x[[1L]]
      args <- list(x = quote(x), y = quote(y))
      tryCatch(
        do.call('test', modifyList(args, args.test)),
        error = function(e) {
          message('Warning: ', e$message, ' - skipping test')
          list(p.value = NA)
        }
      )
    })
    
    mtext(sapply(res, function(x) pvalr(x$p.value, show.p = TRUE)),
          at = unique(at), side = 3L)
  }
  
  invisible(res)
}
