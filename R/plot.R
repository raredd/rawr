### plot functions
# jmplot, tplot, dsplot, waffle, river, river2, dose_esc, plothc, waterfall,
# heatmap.3
###


#' Joint-marginal plot
#' 
#' Joint and marginal distributions scatterplots with \code{\link{tplot}}s on
#' margins.
#' 
#' @param x,y x- and y-axis variables
#' @param z grouping variable
#' @param main,sub main- and sub-titles (below x-axis) for the plot
#' @param xlab,ylab x- and y-axis labels
#' @param names labels for \code{x} and \code{y} variables
#' @param xlim,ylim x- and y-axis limits
#' @param axes logical; draw axes
#' @param frame.plot logical; draw box around \code{x-y} plot
#' @param log \code{"x"}, \code{"y"}, or \code{"xy"} for logarithmic scale or
#' \code{""} for none (default); sets negative values to \code{\link{NA}} and
#' gives a warning; see \code{\link{xy.coords}}
#' @param xratio,yratio proportion of x- and y-axes allotted for scatterplots;
#' see \code{widths} and \code{heights} in \code{\link{layout}}
#' @param show.n,show.na logical; show total and missing in each group
#' @param cex.n size of \code{show.n} and \code{show.na} text
#' @param ann logical; annotate plot
#' @param asp numeric, giving the \strong{asp}ect ratio \emph{y/x}; see
#' \code{\link{plot.window}}
#' @param panel.first an "expression" to be evaluated after the plot axes are
#' set up but before any plotting takes place; this can be useful for drawing
#' background grids or scatterplot smooths; note that this works by lazy
#' evaluation: passing this argument from other plot methods may well not work
#' since it may be evaluated too early; see also \code{\link{plot.default}}
#' @param panel.last an expression to be evaluated after plotting has taken
#' place but before the axes, title, and box are added; see the comments about
#' \code{panel.first}
#' @param ... further arguments passed to \code{par} (\code{las}, \code{pch},
#' etc) and/or \code{\link{tplot}} (\code{group.col}, \code{group.pch}, etc)
#' 
#' @return
#' A list with components \code{x} and \code{y} corresponding to boxplots
#' on x- and y-axes, respectively. See \code{\link{boxplot}} or
#' \code{\link{tplot}} for a detailed description of each list.
#' 
#' @references
#' \href{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}{Tatsuki
#' \code{jmplot}}; \code{\link{tplot}}; \code{\link{boxplot}}
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(x = rnorm(100, 20, 5),
#'                   y = rexp(100),
#'                   z = c('M','F'),
#'                   zz = c(LETTERS[1:4]))
#' with(dat,
#'   jmplot(x, y, zz, type = 'db', jit = .02, col = 1:4, las = 1, cex.n = .5,
#'          group.col = TRUE, pch = 1:4, group.pch = TRUE, boxcol = grey(.9))
#' )
#'
#' @export

jmplot <- function(x, y, z,
                   ## labels/aesthetics
                   main = '', sub = '', xlab = NULL, ylab = NULL, names,
                   
                   ## additional aesthetics
                   xlim = NULL, ylim = NULL, axes = TRUE, frame.plot = axes,
                   log = '', xratio = .8, yratio = xratio,
                   
                   ## n/missing for each group
                   show.n = TRUE, show.na = show.n, cex.n = 1,
                   
                   ## extra stuff
                   ann = par('ann'), asp = NA,
                   panel.first = NULL, panel.last = NULL, ...) {
  
  ## helpers
  localTplot <- function(..., type = 'b', horizontal = FALSE)
    tplot(..., type = type, axes = FALSE, horizontal = horizontal)
  eliminateTplot <- function(func, ..., type, dist, jit, names, group.col,
                             boxcol, boxborder, group.pch, median.line,
                             mean.line, median.pars, mean.pars, boxplot.pars,
                             border.col, axes, frame.plot, add, horizontal) {
    func(...)
  }
  localPlot <- function(xy, ..., lwd)
    eliminateTplot(plot.xy, xy, 'p', ...)
  localAxis <- function(..., col, bg, pch, cex, lty, lwd)
    eliminateTplot(axis, ...)
  localBox <- function(..., col, bg, pch, cex, lty, lwd)
    eliminateTplot(box, ...)
  localWindow <- function(..., col, bg, pch, cex, lty, lwd)
    eliminateTplot(plot.window, ...)
  localTitle <- function(..., col, bg, pch, cex, lty, lwd)
    eliminateTplot(title, ...)
  
  ## calculate xlim, ylim
  lim <- function(z) {
    r <- range(z, na.rm = TRUE, finite = TRUE)
    # pm <- diff(r) / 20
    # r <- r + pm * c(-1,1)
    r
  }
  
  z  <- as.factor(z)
  xy <- xy.coords(x, y, deparse(substitute(x)), deparse(substitute(y)), log)
  
  ## defaults
  if (missing(names)) names <- levels(z)
  if (is.null(xlab))   xlab <- xy$xlab
  if (is.null(ylab))   ylab <- xy$ylab
  if (is.null(xlim))   xlim <- lim(xy$x)
  if (is.null(ylim))   ylim <- lim(xy$y)
  
  op <- par(no.readonly = TRUE)
  mar <- op$mar
  ## set the layout
  layout(matrix(c(1,3,0,2), 2L), widths = c(xratio, 1 - xratio),
         heights = c(1 - yratio, yratio))
  par(mar = c(0,0,0,0), oma = c(0, 0, mar[3L], mar[4L]) + op$oma)
  
  ## plot x distribution on top
  par(mar = c(0, mar[2L], 0, 0))
  X <- localTplot(x ~ z, ylim = xlim, horizontal = TRUE,
                  show.n = FALSE, show.na = FALSE, ...)
  if (axes) 
    localAxis(side = 2, at = 1:nlevels(z), labels = names, ...)
  
  ## plot y distribution on right
  par(mar = c(mar[1L], 0, 0, 0))
  Y <- localTplot(y ~ z, ylim = ylim, horizontal = FALSE,
                  show.n = show.n, show.na = show.na, cex.n = cex.n, ...)
  if (axes) 
    localAxis(side = 1L, at = seq.int(nlevels(z)), labels = names, ...)
  
  ## plot xy points
  par(mar = c(mar[1L], mar[2L], 0, 0))
  plot.new()
  localWindow(xlim, ylim, log, asp, ...)
  panel.first
  localPlot(xy, xlim = xlim, ylim = ylim, ...)
  panel.last
  
  ## plot options
  if (axes) {
    localAxis(side = 1L, ...)
    localAxis(side = 2L, ...)
  }
  if (frame.plot)
    localBox(...)
  if (ann) {
    localTitle(sub = sub, xlab = xlab, ylab = ylab, ...)
    localTitle(main = main, outer = TRUE, ...)
  }
  
  invisible(list(x = X, y = Y))
}

#' tplot
#' 
#' An alternative to \code{\link{boxplot}}. The individual data can be shown
#' (either in the foreground or background) with jittering if necessary.
#' 
#' @param x a numeric vector or a single list containing such vectors
#' @param g a vector or factor object giving the group for the corresponding
#' elements of \code{x}, ignored with a warning if \code{x} is a list
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' @param formula a \code{\link{formula}}, such as \code{y ~ grp}, where y is
#' a numeric vector of data values to be split into groups according to the
#' grouping variable \code{grp} (usually a factor)
#' @param data a data frame (or list) from which the variables in formula
#' should be taken
#' @param subset an optional vector specifying a subset of observations to be
#' used for plotting
#' @param na.action a function which indicates what should happen when the data
#' contain \code{\link{NA}}s; the default is to ignore missing values in either
#' the response or the group
#' @param type type of plot (dot, dot-box, box-dot, box)
#' @param main,sub overall title and sub-title for the plot (below x-axis)
#' @param xlab,ylab x- and y-axis labels
#' @param xlim,ylim x- and y-axis limits
#' @param names group labels
#' @param axes logical; draw axes
#' @param frame.plot logical; draw box around \code{x-y} plot
#' @param at group positions on axis
#' @param horizontal logical; flip axes
#' @param jit,dist x-axis jitter parameters for overlapping points (use
#' \code{0} for  overlap and higher values for more distance between points);
#' \code{dist} and \code{jit} define the interval width and spreading factors,
#' respectively
#' @param boxplot.pars additional list of graphical parameters for box plots
#' @param col plotting color
#' @param group.col logical; if \code{TRUE}, color by group; otherwise by order
#' @param boxcol,bordercol box fill and border colors
#' @param pch plotting character
#' @param group.pch logical; if \code{TRUE}, \code{pch} by group; o/w, by order
#' @param cex \strong{c}haracter \strong{ex}pansion value
#' @param group.cex logical; if \code{TRUE}, groups will use the same
#' \code{cex} value; otherwise, points will have individual values, recycled if
#' necessary
#' @param median.line,mean.line logical; draw median, mean lines
#' @param median.pars,mean.pars list of graphical parameters for median, mean
#' lines
#' @param show.n,show.na logical; show total and missing in each group
#' @param cex.n character expansion for \code{show.n} and \code{show.na}
#' @param text.na label for missing values (default is "missing")
#' @param test logical or function; if \code{TRUE}, a rank-sum p-value is
#' added to the plot (\code{\link{wilcox.test}} or \code{\link{kruskal.test}}
#' based on the number of groups)
#' 
#' Alternatively, a function (or function name as a character string) can be
#' used, e.g., \code{test = cuzick.test} or \code{function(x, g)
#' cuzick.test(x, g)}; note that if \code{test} is a function, it must have
#' at least two arguments with the numeric data values and group
#' @param ann logical; annotate plot
#' @param add logical; add to an existing plot
#' @param panel.first an "expression" to be evaluated after the plot axes are
#' set up but before any plotting takes place; this can be useful for drawing
#' background grids or scatterplot smooths; note that this works by lazy
#' evaluation: passing this argument from other plot methods may well not work
#' since it may be evaluated too early; see also \code{\link{plot.default}}
#' @param panel.last an expression to be evaluated after plotting has taken
#' place but before the axes, title, and box are added; see the comments about
#' \code{panel.first}
#' @param reset_par logical; if \code{TRUE}, resets \code{\link{par}} settings
#' to state before function call
#'
#' @return
#' A list with the following components (see \code{\link{boxplot}}:
#' \item{\code{$stats}}{a matrix, each column contains the extreme of the lower
#' whisker, the lower hinge, the median, the upper hinge and the extreme of the
#' upper whisker for one group/plot. If all the inputs have the same class
#' attribute, so will this component.}
#' \item{\code{$n}}{a vector with the number of observations in each group.}
#' \item{\code{$conf}}{a matrix where each column contains the lower and upper
#' extremes of the notch.}
#' \item{\code{$out}}{the values of any data points which lie beyond the
#' extremes of the whiskers.}
#' \item{\code{$group}}{a vector of the same length as \code{out} whose
#' elements indicate to which group the outlier belongs.}
#' \item{\code{$names}}{a vector of names for the groups.}
#'
#' @seealso
#' \href{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}{Tatsuki
#' \code{tplot}}; \href{http://data.vanderbilt.edu/~graywh/dotplot/}{web app
#' for Tatsuki \code{tplot}}; \code{\link{boxplot}}; \code{\link{jmplot}}
#'
#' @examples
#' ## these are equivalent ways to call tplot
#' ## formula method is a convenience function for the second (split) case
#' x <- mtcars$mpg
#' g <- interaction(mtcars$gear, mtcars$vs)
#' 
#' tplot(x, g)
#' tplot(split(x, g))
#' tplot(mpg ~ gear + vs, mtcars)
#' 
#' 
#' ## horizontal plots may cut off text
#' tplot(x, g, horizontal = TRUE)
#' 
#' op <- par(no.readonly = TRUE)
#' par(mar = c(5,5,5,5))
#' tplot(x, g, horizontal = TRUE)
#' par(op)
#' 
#' ## or rotate labels
#' tplot(x, g, horizontal = TRUE, srt = -90)
#' 
#' 
#' ## add rank-sum or custom test to plot
#' tplot(mpg ~ vs, mtcars, test = TRUE)   ## two groups - wilcox.test
#' tplot(mpg ~ gear, mtcars, test = TRUE) ## >=2 groups - kruskal.test
#' 
#' tplot(mpg ~ gear, mtcars, test = rawr::cuzick.test) ## trend test
#' tplot(mtcars$mpg, 1:2, test = function(x, g)        ## custom test
#'   wilcox.test(x ~ g, data.frame(x, g), exact = FALSE, paired = TRUE))
#' 
#' 
#' ## tplot has the same return value as boxplot
#' identical(tplot(mtcars$mpg), boxplot(mtcars$mpg))
#' 
#' 
#' ## use panel.first/panel.last like in `plot` (unavailable in `boxplot`)
#' tplot(mpg ~ gear, data = mtcars, col = 1:3, type = 'd', show.na = FALSE,
#'       cex = c(1,5)[(mtcars$mpg > 30) + 1L],
#'       panel.last = legend('topleft', legend = 3:5, col = 1:3, pch = 1),
#'       panel.first = {
#'         abline(h = mean(mtcars$mpg))
#'         abline(h = 1:6 * 5 + 5, lty = 'dotted', col = 'grey70')
#'       })
#' 
#' 
#' ## example with missing data
#' set.seed(1)
#' dat <- data.frame(age = rnorm(80, rep(c(26, 36), c(70, 10)), 4),
#'                   sex = factor(sample(c('Female', 'Male'), 80, replace = TRUE)),
#'                   group = paste0('Group ',
#'                                  sample(1:4, 40, prob = c(2, 5, 4, 1),
#'                                         replace = TRUE)))
#' dat[1:5, 'age'] <- NA
#' 
#' tplot(age ~ group, data = dat, las = 1, cex.axis = 1, bty = 'L',
#'       type = c('db', 'db', 'db', 'd'), names = LETTERS[1:4],
#'       text.na = 'n/a', ## default is 'missing'
#'       group.pch = TRUE, pch = c(15, 17, 19, 8),
#'       group.col = FALSE, col = c('darkred', 'darkblue')[sex],
#'       boxcol = c('lightsteelblue1', 'lightyellow1', grey(.9)),
#'       boxplot.pars = list(notch = TRUE, boxwex = .5))
#' legend(par('usr')[1], par('usr')[3], xpd = NA, bty = 'n',
#'        legend = levels(dat$sex), col = c('darkred', 'darkblue'), pch = 19)
#'
#' @export

tplot <- function(x, ...) UseMethod('tplot')

#' @rdname tplot
#' @export
tplot.default <- function(x, g, ..., type = 'db', jit = 0.1, dist = NULL,
                          
                          ## labels/aesthetics
                          main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                          xlim = NULL, ylim = NULL, names,
                          col, group.col = TRUE, boxcol = 'grey90',
                          bordercol = par('fg'),
                          pch = par('pch'), group.pch = TRUE,
                          cex = par('cex'), group.cex = FALSE,
                          
                          ## additional aesthetics
                          median.line = FALSE, mean.line = FALSE,
                          median.pars = list(col = par('col')),
                          mean.pars = median.pars, boxplot.pars,
                          
                          ## n/missing for each group
                          show.n = TRUE, show.na = show.n, cex.n = cex,
                          text.na = 'missing',
                          
                          ## extra stuff
                          test = FALSE,
                          ann = par('ann'), axes = TRUE, frame.plot = axes,
                          add = FALSE, at, horizontal = FALSE,
                          panel.first = NULL, panel.last = NULL,
                          reset_par = FALSE) {
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op))
  
  ## helpers
  localAxis   <- function(..., bg, cex, log, lty, lwd, pos)
    axis(...)
  localBox    <- function(..., bg, cex, log, lty, lwd, tick, pos, padj)
    box(...)
  localMtext  <- function(..., bg, cex, log, lty, lwd, tick, pos, padj)
    mtext(..., cex = cex.n)
  localText   <- function(..., bg, cex, log, lty, lwd, tick, padj)
    text(..., cex = cex.n)
  localPoints <- function(..., log, tick, pos, padj)
    points(...)
  localTitle  <- function(..., bg, cex, log, lty, lwd, tick, pos, padj)
    title(...)
  localWindow <- function(..., bg, cex, lty, lwd, tick, pos, padj)
    plot.window(...)
  
  if (!missing(g)) {
    if (is.null(xlab))
      xlab <- deparse(substitute(g))
    if (is.list(x))
      message('\'x\' is a list -- \'g\' will be ignored', domain = NA) else {
        if (is.null(ylab))
          ylab <- deparse(substitute(x))
        if (is.null(xlab))
          xlab <- deparse(substitute(g))
        x <- split(x, g)
      }
  } else {
    if (is.null(xlab))
      xlab <- deparse(substitute(x))
  }
  
  args <- list(x, ...)
  namedargs <- if (!is.null(attributes(args)$names))
    attributes(args)$names !=  '' else logical(length(args))
  groups <- if (is.list(x)) x else args[!namedargs]
  pars   <- args[namedargs]
  if ((n <- length(groups)) == 0L)
    stop('invalid first argument')
  if (length(class(groups)))
    groups <- unclass(groups)
  if (!missing(names))
    attr(groups, 'names') <- names else {
      if (is.null(attr(groups, 'names')))
        attr(groups, 'names') <- 1:n
      names <- attr(groups, 'names')
    }
  zzz <- do.call('boxplot', list(x = x, plot = FALSE, names = names))
  
  ## number and size of groups
  ng <- length(groups)
  lg <- lengths(groups)
  nv <- sum(lg)
  g  <- factor(rep(seq.int(ng), lg), seq.int(ng), names(groups))
  ## .x used when test = TRUE
  .x <- x
  
  if (missing(at))
    at <- seq.int(ng)
  if (length(at) !=  ng) {
    warning("\'at\' must have same length as the number of groups", domain = NA)
    at <- seq.int(ng)
  }
  
  ## scales
  if (is.null(ylim)) {
    r <- range(groups, na.rm = TRUE, finite = TRUE)
    pm <- diff(r) / 20
    ylim <- r + pm * c(-1, 1)
  }
  if (is.null(xlim))
    xlim <- c(0.5, if (missing(at)) ng else max(at) + 0.5)
  
  type <- match.arg(type, c('d', 'db', 'bd', 'b'), several.ok = TRUE)
  ## type of plot for each group
  type <- rep_len(type, ng)
  
  ## default colors
  ## 50% gray for box/dots in back, otherwise default color
  defcols <- c(bordercol, par('col'))
  
  if (missing(col)) {
    col <- defcols[2L - grepl('.d', type)]
    group.col <- TRUE
  }
  if (missing(boxplot.pars))
    boxplot.pars <- NULL
  
  boxcol <- rep_len(boxcol, ng)
  boxborder <- rep_len(bordercol, ng)
  
  if (group.col) {
    ## colors by group
    g.col <- rep_len(col, ng)
    col <- rep(g.col, lg)
  } else {
    ## colors by individual or global
    col   <- rep_len(col, nv)
    g.col <- rep_len(1, ng)
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
  l2 <- sapply(groups, function(x) sum(is.na(x)))
  
  if (all(l2 == 0L) && missing(show.na))
    show.na <- FALSE
  groups <- Map('[', groups, nonas)
  col <- Map('[', col, nonas)
  pch <- Map('[', pch, nonas)
  cex <- Map('[', cex, nonas)
  
  ## mean and median line for each group
  mean.line   <- rep(mean.line, length.out = ng)
  median.line <- rep(median.line, length.out = ng)
  
  ## defaults for dist and jit for groups
  if (missing(dist) || is.na(dist) || is.null(dist))
    dist <- diff(range(ylim)) / 100
  if (missing(jit) || is.na(jit) || is.null(jit))
    jit <- 0.025 * ng
  groups <- lapply(groups, grouping_, dif = dist)
  ## rawr:::grouping_; rawr:::jit_
  
  ## set up new plot unless adding to existing one
  op <- par(no.readonly = TRUE)
  if (!add) {
    plot.new()
    if (horizontal)
      do.call('localWindow', c(list(ylim, xlim), pars))
    else do.call('localWindow', c(list(xlim, ylim), pars))
  }
  if (show.n && horizontal)
    par(oma = par('oma') + c(0,0,0,2))
  panel.first
  
  out <- list()
  Lme <- 0.2 * c(-1, 1)
  
  for (i in 1:ng) {
    to.plot <- groups[[i]]
    nn <- names(groups)
    gs <- to.plot$g.si
    hms <- to.plot$hmsf
    x <- rep(at[i], nrow(to.plot)) + jit_(gs, hms) * jit
    y <- to.plot$vs
    
    ## dots behind
    if (type[i] == 'bd') {
      bp <- do.call('boxplot',
                    c(list(x = y, at = at[i], plot = FALSE, add = FALSE,
                           axes = FALSE, col = boxcol[i],
                           border = boxborder[i], outline = FALSE,
                           horizontal = horizontal), boxplot.pars))
      notoplot <- (y <= bp$stats[5, ]) & (y >= bp$stats[1, ])
      if (sum(notoplot) > 0)
        col[[i]][notoplot] <- '#bfbfbf'
      if (horizontal) {
        do.call('localPoints', c(list(x = y, y = x, pch = pch[[i]],
                                      col = col[[i]], cex = cex[[i]]),
                                 pars))
      } else do.call('localPoints', c(list(x = x, y = y, pch = pch[[i]],
                                           col = col[[i]], cex = cex[[i]]),
                                      pars))
    }
    ## box in front
    if (type[i] %in% c('bd', 'b')) {
      bp <- do.call('boxplot',
                    c(list(x = y, at = at[i], add = TRUE, axes = FALSE,
                           col = boxcol[i], border = boxborder[i],
                           outline = FALSE, horizontal = horizontal),
                      boxplot.pars))
      toplot <- (y > bp$stats[5, ]) | (y < bp$stats[1, ])
      if (sum(toplot) > 0)
        if (col[[i]][toplot][1] == '#bfbfbf')
          col[[i]][toplot] <- 1
      if (horizontal) {
        do.call('localPoints',
                c(list(x = y[toplot], y = x[toplot], pch = pch[[i]][toplot],
                       col = col[[i]][toplot], cex = cex[[i]][toplot]),
                  pars))
      } else
        do.call('localPoints',
                c(list(x = x[toplot], y = y[toplot], pch = pch[[i]][toplot],
                       col = col[[i]][toplot], cex = cex[[i]][toplot]), pars))
    }
    ## box behind
    if (type[i] == 'db')
      bp <- do.call('boxplot',
                    c(list(x = y, at = at[i], add = TRUE, axes = FALSE,
                           col = boxcol[i], border = boxborder[i],
                           outline = FALSE, horizontal = horizontal),
                      boxplot.pars))
    ## dots in front
    if (type[i] %in% c('db', 'd')) {
      if (horizontal)
        do.call('localPoints', c(list(x = y, y = x, pch = pch[[i]],
                                      col = col[[i]], cex = cex[[i]]), pars))
      else
        do.call('localPoints', c(list(x = x, y = y, pch = pch[[i]],
                                      col = col[[i]], cex = cex[[i]]), pars))
    }
    
    ## mean and median lines
    if (mean.line[i]) {
      if (horizontal)
        do.call('lines', c(list(rep(mean(y), 2), at[i] + Lme), mean.pars))
      else do.call('lines', c(list(at[i] + Lme, rep(mean(y), 2)), mean.pars))
    }
    if (median.line[i]) {
      if (horizontal)
        do.call('lines', c(list(rep(median(y), 2), at[i] + Lme), median.pars))
      else
        do.call('lines', c(list(at[i] + Lme, rep(median(y), 2)), median.pars))
    }
  }
  
  ## p-value for wilcoxon/kw test in upper-right corner
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
    
    mtext(pvalr(pv$p.value, show.p = TRUE), 3, line = 0.5, cex = 1.2,
          # at = if (ng %% 2 == 0) NA else par('usr')[2] * .95,
          at = par('usr')[2], font = 3L, adj = 1)
  }
  
  panel.last
  
  if (axes) {
    do.call('localAxis',
            c(list(side = 1 + horizontal, at = at, labels = names), pars))
    do.call('localAxis', c(list(side = 2 - horizontal), pars))
  }
  
  ## frame and text, optional sample sizes
  if (show.n | show.na) {
    txt <- sprintf('%s%s\n%s%s', 
                   ifelse(rep_len(show.n, ng), 'n = ', ''),
                   ifelse(rep_len(show.n, ng), lg - l2, ''),
                   if (show.na)
                     ifelse(l2 > 0L, paste0(text.na, ' = '), '') else '',
                   if (show.na)
                     ifelse(l2 > 0L, l2, '') else ''
    )
    if (horizontal)
      do.call('localText', c(list(
        x = (diff(ylim) * .08 * sign(ylim) + ylim)[2L], y = at,
        labels = txt, xaxt = 's', yaxt = 's', xpd = NA
      ), pars))
    else
      do.call('localText', c(list(
        x = at, y = par('usr')[4L], labels = txt,
        xaxt = 's', yaxt = 's', xpd = NA, pos = 3
      ), pars))
      # do.call('localMtext', c(list(
      #   text = txt, side = 3 + horizontal, at = at, xaxt = 's', yaxt = 's'),
      #   pars))
  }
  
  if (frame.plot)
    do.call('localBox', pars)
  if (ann) {
    if (horizontal)
      do.call('localTitle', c(list(main = main, sub = sub,
                                   xlab = ylab, ylab = xlab), pars))
    else
      do.call('localTitle', c(list(main = main, sub = sub,
                                   xlab = xlab, ylab = ylab), pars))
  }
  invisible(zzz)
}

#' @rdname tplot
#' @export
tplot.formula <- function(formula, data = NULL, ...,
                          subset, na.action = NULL,
                          panel.first = NULL, panel.last = NULL) {
  
  if (missing(formula) || (length(formula) !=  3))
    stop("\'formula\' missing or incorrect")
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  
  args <- lapply(m$..., eval, data, parent.frame())
  nmargs <- names(args)
  
  form <- as.character(formula)
  args <- modifyList(list(xlab = form[3], ylab = form[2]), args)
  
  if ('main' %in% nmargs) args[['main']] <- enquote(args[['main']])
  if ('sub' %in% nmargs)  args[['sub']]  <- enquote(args[['sub']])
  if ('xlab' %in% nmargs) args[['xlab']] <- enquote(args[['xlab']])
  if ('ylab' %in% nmargs) args[['ylab']] <- enquote(args[['ylab']])
  
  ## hacky way to pass panel.first/panel.last to tplot.default
  args[['panel.first']] <- substitute(panel.first)
  args[['panel.last']]  <- substitute(panel.last)
  
  m$... <- m$subset <- m$panel.first <- m$panel.last <- NULL
  m$na.action <- na.pass
  subset.expr <- m$subset
  
  m[[1]] <- as.name('model.frame')
  mf <- eval(m, parent.frame())
  n <- nrow(mf)
  response <- attr(attr(mf, 'terms'), 'response')
  
  ## special handling of col and pch for grouping
  group.col <- if ('group.col' %in% names(args)) args$group.col else FALSE
  group.pch <- if ('group.pch' %in% names(args)) args$group.pch else FALSE
  group.cex <- if ('group.cex' %in% names(args)) args$group.cex else FALSE
  
  ## reorder if necessary
  if ('col' %in% names(args) && !group.col)
    args$col <- unlist(split(rep(args$col, length.out = n), mf[-response]))
  if ('pch' %in% names(args) && !group.pch)
    args$pch <- unlist(split(rep(args$pch, length.out = n), mf[-response]))
  if ('cex' %in% names(args) && !group.cex)
    args$cex <- unlist(split(rep(args$cex, length.out = n), mf[-response]))
  
  if (!missing(subset)) {
    s <- eval(subset.expr, data, parent.frame())
    ## rawr:::do_sub_
    args <- lapply(args, do_sub_, x, n, s)
    mf <- mf[s, ]
  }
  
  do.call('tplot', c(list(split(mf[[response]], mf[-response])), args))
}

#' Discrete scatter plot
#' 
#' This creates a scatter plot (sort of) for discrete, bivariate data; an
#' alternative to sunflower plots for integer-valued variables.
#' 
#' @param formula a \code{\link{formula}}, such as \code{y ~ group}, where y
#' is a numeric vector of data values to be split into groups according to the
#' grouping variable \code{group} (usually a factor)
#' @param data a data frame (or list) from which the variables in formula
#' should be taken
#' @param subset an optional vector specifying a subset of observations to be
#' used for plotting
#' @param na.action a function which indicates what should happen when the data
#' contain \code{\link{NA}}s; the default is to ignore missing values in either
#' the response or the group
#' @param x,y x- and y-axis variables
#' @param ... for the \code{formula} method, named arguments to be passed to
#' the default method
#' 
#' for the default method, unnamed arguments are additional data vectors
#' (unless x is a list when they are ignored), and named arguments are
#' arguments and \code{\link{par}}s to be passed to \code{\link{plot}}
#' @param bg.col logical or color name to fill boxes with based on density; if
#' \code{FALSE}, gridlines are drawn to distinguish boxes; if \code{TRUE},
#' grayscale is used as the default color
#' @param col plotting color
#' @param xlab,ylab x- and y-axis labels
#' @param pch \strong{p}lotting \strong{ch}aracter
#' @param cex numerical value giving the amount by which plotting text and
#' symbols should be magnified relative to the default; this starts as 1
#' when a device is opened and is reset when the layout is changed, e.g.,
#' by setting \code{mfrow}
#' 
#' @return
#' A table (invisibly) corresponding to the plot cell counts.
#' 
#' @references
#' \href{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}{Tatsuki
#' \code{dsplot}}
#' 
#' @examples
#' set.seed(1)
#' x <- round(rnorm(400, 100, 4))
#' y <- round(rnorm(400, 200, 4))
#' sex <- sample(c('Female', 'Male'), 400, replace = TRUE)
#' 
#' dsplot(x, y, col = (sex %in% 'Female') + 1L, bg.col = FALSE)
#' 
#' dsplot(y ~ x, pch = 19, col = (sex %in% 'Female') + 1L,
#'        cex = .6, bty = 'l', bg.col = 'tomato',
#'        xlab = 'measurement 1', ylab = 'measurement 2')
#' legend('bottomright', pch = 19, col = 1:2, bty = 'n',
#'        legend = c('Male', 'Female'))
#'        
#' @export

dsplot <- function(x, ...) UseMethod('dsplot')

#' @rdname dsplot
#' @export
dsplot.default <- function(x, y, ..., bg.col = TRUE, col = par('col'),
                           xlab = NULL, ylab = NULL,
                           pch = par('pch'), cex = par('cex')) {
  # if (any(x != round(x), na.rm = TRUE) | any(y != round(y), na.rm = TRUE))
  #   stop('\'x\' must be integer values', '\n')
  
  ## helpers
  localAxis   <- function(..., bg, bty, cex, log, lty, lwd, pos)
    axis(...)
  
  square.coordinates <- function(box.size) {
    x.c <- y.c <- 1
    for (i in 2:box.size)
      x.c <- c(x.c, every.other.element.x(i))
    for (j in 2:box.size)
      y.c <- c(y.c, every.other.element.y(j))
    data.frame(x.c, y.c)
  }
  
  ## vector 1,n,2,n,3,n, ...,  n, n for x
  ## vector n,1,n,2,n,3, ..., n-1,n for y
  every.other.element.x <- function(n) c(rbind(1:n, rep(n, n)))[-(2 * n)]
  every.other.element.y <- function(n) c(rbind(rep(n, n), 1:n))[-(2 * n)]
  
  xy <- xy.coords(x, y, deparse(substitute(x)), deparse(substitute(y)))
  L <- length(x)
  cc <- complete.cases(x, y)
  
  pch <- rep_len(pch, L)
  col <- rep_len(col, L)
  cex <- rep_len(cex, L)
  
  x <- x[cc]
  y <- y[cc]
  X <- range(x) + c(0, 1)
  Y <- range(y) + c(0, 1)
  pch <- pch[cc]
  col <- col[cc]
  cex <- cex[cc]
  
  x.levels <- sort(unique(x))
  y.levels <- sort(unique(y))
  tab <- table(x, y)
  max.freq <- max(tab)
  box.size <- ceiling(sqrt(max.freq))
  
  # op <- par(no.readonly = TRUE)
  # on.exit(par(op))
  
  plot(X, Y, type = 'n', xaxs = 'i', yaxs = 'i',
       ann = FALSE, xaxt = 'n', yaxt = 'n', ...)
  title(xlab = xlab %||% xy$xlab, ylab = ylab %||% xy$ylab)
  
  localAxis(1L, pretty(x) + .5, pretty(x), ...)
  localAxis(2L, pretty(y) + .5, pretty(y), ...)
  
  if (identical(bg.col, FALSE)) {
    abline(h = y.levels, col = grey(.9))
    abline(v = x.levels, col = grey(.9))
  }
  
  sc <- square.coordinates(box.size)
  coord <- (1:box.size) / (box.size + 1)
  off.set <- coord[1L] / 4
  alpha <- seq(0.2, 0.9, length = max.freq)
  bg.col <- if (isTRUE(bg.col))
    'grey' else bg.col[1L]
  
  dat <- data.frame(id = seq.int(length(x)), x, y)
  dat <- dat[order(dat$x, dat$y), ]
  within <- c(t(tab))
  within <- within[within > 0L]
  idx <- hm <- NULL
  
  for (i in within) {
    ## index within category
    idx <- c(idx, 1:i)
    hm  <- c(hm, rep(i, i))
  }
  dat$idx <- idx
  
  ## local offset
  dat$ly <- (box.size - ceiling(sqrt(hm))) / (box.size + 1) / 2
  dat$lx <- dat$ly + ((ceiling(sqrt(hm - 1)) ** 2 == hm - 1) & (hm > 1)) /
    (box.size + 1) / 2
  dat <- dat[order(dat$id), ]
  dat$col <- col
  dat$pch <- pch
  
  if (!identical(bg.col, FALSE)) {
    for (i in x.levels) {
      for (j in y.levels) {
        n <- sum(x == i & y == j)
        if (n > 0) {
          col <- adjustcolor(bg.col, alpha[n])
          rect(i + off.set, j + off.set, i + 1 - off.set, j + 1 - off.set,
               border = col, col = col)
        }
      }
    }
  }
  
  points(dat$x + coord[sc[dat$idx, 1L]] + dat$lx,
         dat$y + coord[sc[dat$idx, 2L]] + dat$ly,
         pch = dat$pch, col = dat$col, cex = cex)
  
  invisible(table(factor(y, levels = rev(min(y):max(y))),
                  factor(x, levels = min(x):max(x))))
}

#' @rdname dsplot
#' @export
dsplot.formula <- function(formula, data = NULL, ...,
                           subset, na.action = NULL) {
  if (missing(formula) || (length(formula) != 3L))
    stop('\'formula\' missing or incorrect')
  
  enquote <- function(x) as.call(list(as.name('quote'), x))
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  args <- lapply(m$..., eval, data, parent.frame())
  nmargs <- names(args)
  
  if ('main' %in% nmargs) args[['main']] <- enquote(args[['main']])
  if ('sub' %in% nmargs)  args[['sub']]  <- enquote(args[['sub']])
  if ('xlab' %in% nmargs) args[['xlab']] <- enquote(args[['xlab']])
  if ('ylab' %in% nmargs) args[['ylab']] <- enquote(args[['ylab']])
  
  # m$na.action <- na.pass
  subset.expr <- m$subset
  m$... <- m$subset <- NULL
  
  m[[1L]] <- as.name('model.frame')
  m <- as.call(c(as.list(m), list(na.action = NULL)))
  mf <- eval(m, parent.frame())
  n <- nrow(mf)
  response <- attr(attr(mf, 'terms'), 'response')
  
  if (!missing(subset)) {
    s <- eval(subset.expr, data, parent.frame())
    args <- lapply(args, do_sub_, x, n, s)
    ## rawr:::do_sub_
    mf <- mf[s, ]
  }
  
  do.call('dsplot', c(list(mf[[-response]], mf[[response]]), args))
}

#' waffle chart
#' 
#' A waffle chart.
#' 
#' @param mat a matrix of integers or character strings of color names; if
#' \code{mat} is a matrix of integers, the colors used will correspond to
#' the current \code{\link{palette}}
#' @param xpad,ypad amount of padding between \code{rect}s along axes; note
#' that if if \code{xpad}/\code{ypad} have the same length as the number of
#' columns/rows, the padding will be applied to the column/row; otherwise,
#' the vector is recycled and each \code{rect} uses its own padding
#' @param heights,widths the row heights and column widths, respectively,
#' usually in \code{0,1}, and recycled as needed; note that \code{xpad} and
#' \code{ypad} are ignored if \code{widths} or \code{heights}, respectively,
#' are given; the same recycling that is done for \code{xpad}/\code{ypad} is
#' also repeated here (see the note in \code{xpad})
#' @param colpad,rowpad amount of padding between columns and rows; note that
#' changing these moves the \code{rect} center positions rather than
#' affecting the heights and widths
#' @param invert character string indicating about which axis the matrix
#' should be inverted; possible values are \code{"x"}, \code{"y"}, or
#' \code{"xy"}
#' @param ... additional graphical parameters passed to \code{\link{rect}}
#' such as \code{border}, \code{density}, \code{lty}, etc.
#' @param reset_par logical; if \code{TRUE}, resets \code{\link{par}}
#' settings to state before function call; setting \code{reset_par = FALSE}
#' along with the return value is useful for adding to a \code{waffle} plot
#' 
#' @return
#' A list of three matrices:
#' \item{\code{$matrix}}{the input matrix as plotted, including inversions}
#' \item{\code{$origin}}{coordinates of the bottom-left corner for each box}
#' \item{\code{$centers}}{the coordinates for the centers of each box
#' adjusted for \code{xpad}, \code{ypad} or \code{heights}, \code{widths}}
#' 
#' @examples
#' op <- par(no.readonly = TRUE)
#' waffle(matrix(1:9, 3))
#' waffle(matrix(1:9, 3), invert = 'x')
#' waffle(matrix(1:9, 3), heights = c(.25, .95, .5), border = NA)
#' waffle(matrix(1:9, 3), xpad = 0, colsep = c(0, .1, 0))
#' 
#' 
#' ## heatmap
#' ## convert the numeric data to color strings
#' cols <- c(cor(mtcars))
#' cols <- rawr::tcol(c('blue','red')[(cols > 0) + 1L], alpha = c(abs(cols)))
#' 
#' mat <- matrix(cols, 11)
#' waffle(mat, reset_par = FALSE, xpad = 0, invert = 'x')
#' axis(3, at = 1:11 - .5, labels = names(mtcars), lwd = 0)
#' 
#' 
#' ## use colpad/rowpad to create sections
#' w <- waffle(mat, reset_par = FALSE, xpad = 0, invert = 'x',
#'             colpad = rep(c(0, 0.5, 1, 0), c(5, 1, 1, 4)),
#'             rowpad = rep(c(0, 0.5, 0), c(5, 1, 5)))
#' axis(3, unique(w$centers[, 'x']), names(mtcars), lwd = 0)
#' 
#' 
#' ## adding to margins of another plot
#' set.seed(1)
#' n <- 97
#' ng <- 3
#' cols <- c('beige', 'dodgerblue2', 'green', 'orange')
#' x <- sample(cols, n * ng, replace = TRUE, prob = c(.05,.31,.32,.32))
#' x <- rawr::kinda_sort(x, n = 20)
#' 
#' par(fig = c(0,1,.2,.9), mar = c(0,5,0,1))
#' plot(cumsum(rnorm(n)), type = 'l', ann = FALSE, xaxt = 'n')
#' 
#' par(fig = c(0,1,0,.2), mar = c(1,5,0,1), new = TRUE)
#' waffle(matrix(x, ng), heights = c(0.95, 0.5, 0.95), border = 'white')
#' 
#' par(fig = c(0,1,.9,1), mar = c(.5,5,.5,1), new = TRUE)
#' waffle(matrix(x, ng)[1L, , drop = FALSE], heights = 0.5,
#'        border = 'white', reset_par = FALSE)
#' box()
#' box('outer')
#' 
#' 
#' ## waffle conveniently returns the centers of the rects
#' ## be sure /not/ to reset pars on exit for proper alignment
#' par(op)
#' (w <- waffle(matrix(1:8, 2), reset_par = FALSE, invert = 'xy'))
#' text(w$c[, 'x'], w$c[, 'y'], labels = palette(), col = 'white')
#' 
#' \dontrun{
#' ## this is similar to ?rawr::show_colors
#' col <- colors()[1:25 ^ 2]
#' w <- waffle(matrix(col, 25), reset_par = FALSE, invert = 'xy',
#'             xpad = 0, border = c(col))
#' text(w$c[, 'x'], w$c[, 'y'], labels = col, col = 'black', cex = .4)
#' }
#' 
#' @export

waffle <- function(mat, xpad = 0.05, ypad = xpad, heights, widths,
                   colpad = 0, rowpad = 0,
                   invert = '', ..., reset_par = TRUE) {
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op))
  
  omat <- mat <- as.matrix(mat)
  mat  <- mat[rev(seq.int(nrow(mat))), , drop = FALSE]
  o    <- cbind(c(row(mat)), c(col(mat))) - 1L
  
  if (grepl('x', invert)) {
    mat <- mat[rev(seq.int(nrow(mat))), , drop = FALSE]
    o[, 1L] <- rev(o[, 1L])
  }
  if (grepl('y', invert)) {
    mat <- mat[, rev(seq.int(ncol(mat))), drop = FALSE]
    o[, 2L] <- rev(o[, 2L])
  }
  
  ## xpad/ypad intended to be by column/row
  if (length(mat) > length(xpad) & length(xpad) == ncol(mat))
    xpad <- rep(xpad, each = nrow(mat))
  if (length(mat) > length(ypad) & length(ypad) == nrow(mat))
    ypad <- rep(ypad, each = ncol(mat))
  
  if (!missing(heights)) {
    if (length(mat) > length(heights) & length(heights) == ncol(mat))
      heights <- rep(heights, each = nrow(mat))
    ypad <- 1 - (1 - rev(heights)) / 2
  }
  if (!missing(widths)) {
    if (length(mat) > length(widths) & length(widths) == ncol(mat))
      widths <- rep(widths, each = nrow(mat))
    xpad <- 1 - rep((1 - widths) / 2, each = ncol(mat))
  }
  
  ## add column/row sep values to origins
  colpad <- c(0, cumsum(rep_len(colpad, ncol(mat) - 1L)))
  rowpad <- c(0, cumsum(rep_len(rowpad, nrow(mat) - 1L)))
  
  o <- o + cbind(rev(rep(rowpad, ncol(mat))), rep(colpad, each = nrow(mat)))
  
  plot.new()
  plot.window(xlim = c(0, max(o[, 2L]) + 1),
              ylim = c(0, max(o[, 1L]) + 1),
              xaxs = 'i', yaxs = 'i')
  rect(xl <- o[, 2L] + xpad, yb <- o[, 1L] + ypad,
       xr <- o[, 2L] + (1 - xpad), yt <- o[, 1L] + (1 - ypad),
       col = c(omat), ...)
  
  invisible(
    list(matrix = mat, origin = `colnames<-`(o[, 2:1], c('x', 'y')),
         centers = cbind(x = psum(xl, xr) / 2, y = psum(yb, yt) / 2))
  )
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
#' \code{\link{Date}} formats.
#' 
#' \code{check_river_format()} without any arguments will give a summary of
#' the required formats.
#' 
#' Note that date columns can also be given as integers to be coerced to dates
#' as seen in the examples. In this case, \code{check_river_format} will do
#' the coersion to date before plotting. However, these two methods are
#' interchangeable as long as all values are relative to a starting time.
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
#' format, see details, examples, or run \code{check_river_format()}
#' @param id,at optional parameters specifying individuals (rows) from
#' \code{data} to plot and their positions along the y-axis; if not given,
#' timelines are plotted sequentially
#' @param legend character string giving position (see \code{\link{legend}}),
#' \code{FALSE}, or a list of named arguments passed to \code{legend}
#' @param xlim,ylim x- and y-axis limits
#' @param rev logical; if \code{TRUE}, observations will be plotted from top
#' to bottom
#' @param stagger logical; if \code{FALSE}, start dates will be fixed at 0
#' rather than relative to the first start date if \code{TRUE} (default)
#' @param split logical; if \code{TRUE}, rows of \code{bar_data2} will be
#' plotted individually
#' @param col,col2 vectors of colors for responses (\code{river}) or toxicity
#' grades \code{river2}, respectively
#' 
#' @examples
#' ## to print a summary of the required formats
#' check_river_format()
#' 
#' 
#' ## data in river format:
#' dd <- data.frame(id       = c(1,2,3,4,5),
#'                  dt_reg   = c(1,2,3,4,5),
#'                  dt_txst  = c(1,2,3,4,5) + 1,
#'                  dt_txend = c(10,11,12,13,8),
#'                  dt_prog  = c(11,16,NA,NA,8),
#'                  dt_offtx = c(12,19,NA,NA,8),
#'                  dt_surv  = c(12,19,24,25,NA),
#'                  surv     = c(0,1,0,0,NA),
#'                  dt_last  = c(12,19,24,25,NA),
#'                  dt_off   = c(12,19,NA,NA,8))
#' bd <- data.frame(id        = c(3,3,3,3,4,4),
#'                  dt_assess = c(9,13,17,21,10,15),
#'                  resp = factor(c('MR','PR','PR','CR','PR','CR'),
#'                                levels = c('PD','SD','MR','PR','CR')))
#' 
#' river(dd, bd, stagger = FALSE, rev = TRUE, col = 2:6,
#'       legend = list(x = 'bottom', title = 'Response', horiz = TRUE))
#' 
#' ## same data with single observations per id
#' bd1 <- data.frame(
#'   id = 3:4, dt_assesss = 9:10,
#'   resp = factor(c('PR','CR'), levels = c('PD','SD','MR','PR','CR')))
#' 
#' river(dd, bd1)
#' 
#' ## id and at parameters control the positions of the timelines
#' river(dd, bd1, id = c(1,2,5,3,4), at = c(1:2, 4, 6:7), legend = FALSE)
#' 
#' 
#' ## additional data for river2
#' tt <- data.frame(id = rep(c(1, 3), times = c(1, 5)),
#'                  dt_start = c(3,5,5,8,9,11),
#'                  dt_end = c(NA,5,NA,10,10,NA),
#'                  grade = c(1,4,3,4,1,2),
#'                  desc = paste('tox', c(1,1:5)))
#' 
#' river2(dd, bd, tt, id = 3)
#' 
#' ## multiple records per id (ie, worsening toxicities)
#' tt2 <- data.frame(id = rep(c(1, 3), times = c(1, 8)),
#'                   dt_start = c(3,5,5,7,15,8,9,11,14),
#'                   dt_end = c(NA,5,7,15,NA,10,10,14,NA),
#'                   grade = c(1,4,1,2,3,4,1,2,3),
#'                   desc = paste('tox', c(1,1,2,2,2,3,4,5,5)))
#'                   
#' river2(dd, bd, tt2, id = 3)
#' 
#' 
#' ## bar_data can also be given in river2 without additional information
#' river2(bar_data = tt,  id = 3)
#' river2(bar_data = tt2, id = 3)
#' 
#' @export

river <- function(data, bar_data, id, at, legend = 'topleft',
                  xlim, ylim, rev = FALSE, stagger = TRUE, col) {
  ## error checks
  dd <- check_river_format(data)
  bd <- check_river_format(data, bar_data)
  nn <- as.character(unique(dd$id))
  
  if (missing(id))
    id <- nn
  if (missing(at))
    at <- seq_along(nn)
  if (rev)
    at <- rev(at)
  
  stopifnot(length(at) == length(id))
  if (!all(id %in% nn))
    stop('Invalid ID: ', toString(id))
  
  dd <- merge(bd, dd, by = 'id', all = TRUE)
  
  ## colors for resp - PD:CR
  cols <- if (missing(col))
    c('red', 'transparent', 'yellow', 'orange',
      'deepskyblue', 'dodgerblue3', 'blue4')
  else rep_len(col, nlevels(dd$assess))
  
  ## convert dates to days with origin at first id reg (ie, ref date == 0)
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
  plot.window(if (!missing(xlim)) xlim else c(0, diff(rx)),
              ## set min ylim to c(0,5) for case: id < 5
              if (!missing(ylim)) ylim else range(c(0, at, 5)))
  axis(1, tcl = .2, las = 1L)
  title(xlab = sprintf('Days from %sregistration',
                       c('', 'first ')[stagger + 1L]),
        line = 2.5)
  
  if (legend[[1L]] != FALSE)
    do.call('legend',
      modifyList(list(
        x = legend, fill = cols, legend = levels(dd$assess),
        horiz = FALSE, cex = .8, bty = 'n'),
        val = as.list(legend)
      )
    )
  
  sp <- split(dd, dd$id, drop = FALSE)
  
  for (ii in id) {
    ## lines at specific points require new index
    jj <- at[which(id %in% ii)]
    
    # with(dd[ii, ], {
    with(sp[[as.character(ii)]], {
      ## label ids in black to left of rect
      text(dd_reg[1L], jj, labels = id[1L], pos = 2, xpd = NA)
      
      ## lines - time alive, on tx
      do_seg_(jj, dd_reg, end_day, arrow = alive[1L] & !censor[1L],
              single = TRUE, col = 1)
      ## thicker line and arrow for continuing
      do_seg_(jj, dd_txstart, dd_txend %|% end_day, arrow = FALSE,
              single = TRUE, lty = 1, lwd = 4, col = 'green4')
      
      ## rects - assessments
      do_rect_(jj, dd_assess_start, dd_assess_end %|% end_day,
               col = tcol(col_assess, alpha = .5))
      ## pipe at each assessment time
      points(dd_assess_start, rep(jj, length(dd_assess_start)),
             pch = '|', col = 1)
      
      ## points - prog (red circle), death, (red x), censor (blue x)
      points(dd_prog[1L], jj, pch = 16, col = 2, cex = 1.5)
      points(end_day[1L], jj, pch = c(4, NA)[alive[1L] + 1L],
             col = 2, lwd = 3, cex = 1.5)
      points(end_day[1L], jj, pch = c(NA, 4)[(alive[1L] & censor[1L]) + 1L],
             col = 4, lwd = 3, cex = 1.5)
    })
  }
  
  invisible(list(data = dd, bar_data = bd))
}

#' @rdname river
#' @export
river2 <- function(data, bar_data, bar_data2, id, legend = 'topleft',
                   xlim, ylim, rev = FALSE, stagger = FALSE,
                   split = FALSE, col, col2) {
  ## error checks
  if (!missing(data)) {
    if (missing(bar_data2))
      return(
        river(data = data, bar_data = bar_data, id = id, at = 1, rev = rev,
              legend = legend, xlim = xlim, ylim = ylim, stagger = stagger)
      )
  } else {
    mmin <- function(x) min(x, na.rm = !all(is.na(x)))
    mmax <- function(x) max(x, na.rm = !all(is.na(x)))
    
    bar_data2 <- if (missing(bar_data2)) bar_data else bar_data2
    
    if (ncol(bar_data2) <= 3L)
      ## use bar_data2 format--check_river_format()
      bar_data2 <- bar_data[, c(1,2,2,3,3)]
    
    data <- data.frame(
      id = bar_data[, 1L],
      dt_reg = ave(bar_data[, 2L], bar_data[, 1L], FUN = mmin),
      dt_txst  = NA, dt_txend = NA, dt_prog = NA,
      dt_offtx = NA, dt_surv  = NA, surv    = NA,
      dt_last = ave(unlist(bar_data[, 2:3]),
                    unlist(bar_data[, c(1,1)]), FUN = mmax),
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
  td <- split(td, td$id, drop = FALSE)[[as.character(id)]]
  td <- td[!(is.na(td$dt_start) & is.na(td$dt_end)), ]
  td <- within(td, {
    no_start <- is.na(dt_start)
    dt_start[no_start] <- dt_end[no_start]
  })
  td <- td[do.call('order', as.list(td[, c('dt_start', 'desc')])), ]
  
  ## grade colors - 1:5
  cols <- if (missing(col2))
    c('green', 'blue', 'orange', 'red', 'black')
  else rep_len(tcol(col2), lunique(td$grade))
  nn <- if (split)
    seq.int(nrow(td)) else seq_along(unique(td$desc))
  
  ## base plot of the id summary
  rv <- river(
    data = data, bar_data = bar_data, id = id, at = 1, col = col,
    legend = FALSE, rev = FALSE, stagger = stagger, xlim = xlim,
    ylim = if (missing(ylim))
      c(0, max(5, (if (split) max(nn) else length(nn)) + 1)) else ylim
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
  
  if (nrow(td) && legend[[1L]] != FALSE)
    do.call('legend',
      modifyList(list(
        x = legend, fill = tcol(cols, alpha = 0.5), horiz = FALSE,
        cex = .8, legend = levels(td$grade), bty = 'n'),
        val = as.list(legend)
      )
    )
  
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
      col  <- tcol(col_grade, alpha = .5)
      endx <- pmax(dd_end %|% dd_end2, dd_start, na.rm = TRUE)
      do_rect_(ii + 1, dd_start, endx, col = col, border = col)
      
      ## td with end but no start date
      # if (no_start)
      if (FALSE)
        segments(endx, 1 + ii + c(.15, -.15), endx - .15,
                 1 + ii + c(.15, -.15), col = col, lwd = 2)
      
      ## add count of td to left in black if start date, color/italic if NA
      ## desc on right in color, italics if continuing; black otherwise
      text(dd_start[1L], ii + 1L, labels = ii, pos = 2, xpd = NA,
           cex = .8, col = if (no_start[1L]) col else 1L,
           font = if (no_start[1L]) 3L else 1L)
      # text(tail(dd_end %|% end_day, 1), ii + 1, labels = tail(desc, 1),
      
      text(max(dd_start, dd_end %|% dd_end2, na.rm = TRUE), ii + 1L,
           labels = tail(desc, 1), pos = 4, xpd = NA, cex = .8,
           font = c(1L, 3L)[tail(ong, 1L) + 1L],
           col = c('black', tail(col, 1))[tail(ong, 1) + 1L])
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
  bd <- tryCatch({
    if (one <- length(dts) == 1L) {
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

#' Plot dose escalation
#' 
#' Plot results of a dose-escalation study.
#' 
#' @param dose,col.dose vector of dose levels for each observation; colors
#' should correspond to DLT or similar
#' @param nstep number entered at each step, recycled as necessary; for 3+3
#' or 4+2 studies for example, the value should be \code{3} or \code{c(4,2)},
#' respectively
#' @param dose.exp,col.exp optional vectors for expansion cohort
#' @param xlab,ylab x- and y-axis label for each dose level
#' @param xlim,ylim x- and y-axis limits
#' @param squish numeric value describing a squishing factor; larger values
#' result in plot being compressed
#' 
#' @examples
#' ## 3 + 3
#' dose_esc(d33 <- c(1,1,1,2,2,2,3,3,3,3,3,3,4,4,4),
#'          c33 <- c(3,3,3,3,3,3,3,3,2,3,3,3,3,2,2))
#' legend(0, 4, col = 2:3, pch = 16, pt.cex = 4, xpd = NA, bty = 'n',
#'        legend = paste(c('','Non-'), 'DLT'), y.intersp = 1.5)
#' 
#' ## 3 + 3 with expansion
#' dose_esc(d33, c33, dose.exp = rep(4, 10), col.exp = rep(3, 10))
#' dose_esc(d33, c33, dose.exp = rep(3, 4), col.exp = rep(3, 4))
#' 
#' ## 4 + 4
#' dose_esc(c(1,1,1,1, 2,2,2,2, 2,2,2,2, 3,3,3), nstep = 4,
#'          col.dose = c(3,3,3,3, 3,3,2,3, 3,3,3,3, 3,2,2),
#'          ylab = parse(text = paste0(1:2, '~mg^2')))
#' 
#' ## 4 + 2
#' dose_esc(c(1,1,1,1,1,1, 2,2,2,2,2), nstep = c(4,2),
#'          col.dose = c(3,3,3,2,3,3, 2,3,3,3,2))
#'          
#' @export

dose_esc <- function(dose, col.dose, nstep = 3, dose.exp, col.exp,
                     xlab = 'Time', ylab = 'Dose', xlim = NULL, ylim = NULL,
                     squish = length(dose) %/% 3) {
  nlevel <- table(dose)
  xlab <- if (missing(xlab)) 'Time' else xlab
  ylab <- if (missing(ylab)) paste('Dose', seq_along(nlevel)) else ylab
  dose.exp <- if (missing(dose.exp)) NULL else c(0, dose.exp)
  col.exp  <- if (missing(col.exp)) {
    if (is.null(dose.exp))
      NULL else {
        warning('\'dose.exp\' given without \'exp.col\'', domain = NA)
        rep(NA, length(dose.exp) + 1L)
      }
  } else c(NA, col.exp)
  
  n <- sum(nlevel)
  N <- n + if (!is.null(dose.exp))
    length(dose.exp) else 0
  y <- c(dose, dose.exp)
  x <- seq.int(N) + c(0, cumsum(diff(y) * 0.1))
  col <- c(col.dose, col.exp)
  
  x <- x + cumsum(ave(y, y, FUN = seq_along) %in%
                    (cumsum(rep(nstep, length(y))) + 1)) * 0.1
  pls_idx <- which(!diff(y) > 0 & diff(x) > 1 | y[-length(y)] == 0L)
  pls_idx <- pls_idx[pls_idx <= n + 1]
  x <- rescaler(x, c(1, max(x) - squish)) - 1 / squish
  pls <- sapply(Map(`:`, pls_idx, pls_idx + 1L), function(ii)
    mean(x[ii]))
  arr_idx <- which(diff(y) > 0) + 1L
  arr <- roll_fun(x, 2L, mean)[arr_idx[arr_idx <= n]]
  
  plot.new()
  plot.window(xl <- xlim %||% c(0, max(x)),
              yl <- ylim %||% c(0, max(y) + 1))
  p <- par('usr')
  arrows2(c(0, 0), c(0, 0), c(0, max(xl)), c(max(yl), 0), lwd = 3,
          size = 0.5, width = 0.5)
  axis(2L, seq_along(ylab), ylab, las = 1L, lwd = 0, mgp = c(0, 0, 0))
  text(p[2L], 0, pos = 1L, xlab, xpd = NA, font = 2L)
  text(0, p[4L], pos = 2L, ylab, xpd = NA, font = 2L)
  
  points(x, y, pch = 16L, col = col, cex = 3.5, xpd = NA)
  points(pls, y[pls_idx + 1L], pch = '+', cex = 1.5, xpd = NA)
  arrows2(arr, seq_along(arr) + 0.2, arr, seq_along(arr) + 0.8,
          size = 0.5, width = 0.5, lwd = 4, curve = 1.2,
          sadj = c(0, 0, 0, -.1))
  
  if (!is.null(dose.exp))
    if (diff(y[wh <- which(y == 0L) + c(-1, 1)]) < 0) {
      carrows(c(x[wh[1L]], y[wh[1L]]), c(x[wh[1L]] * 1.01, y[wh[2L]]),
              col = 2L, size = .5, arc = pi / 3 * c(-1, 1), lwd = 4,
              width = 0.7, dir = c(1, 0), pad = c(.1, .3))
    } else arrows2(mean(x[wh[1:2]]), max(dose.exp), mean(x[wh[1:2]]),
                   size = 0.7, width = 1, lwd = 4, col = 3L, curve = 1.3)
  
  invisible(list(x, y, col, arr, pls))
}

#' Color \code{plot.hclust}
#' 
#' Plot an \code{\link{hclust}} object with colored labels.
#' 
#' @param hc an \code{\link{hclust}} object
#' @param labels a character vector of labels for the leaves of the tree
#' @param col a vector of valid colors for \code{labels}
#' @param hang the fraction of the plot height by which \code{labels} should
#' hang below the rest of the plot; a negative value will cause \code{labels}
#' to hang down from 0
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
#' shift <- c(.5, 6)
#' rect(col(mat) - shift[1], row(mat) - shift[2], col(mat) + shift[1],
#'      row(mat) - shift[2] + .5, col = mat, xpd = NA, border = 'white')
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
#' values of \code{x}; \code{type = 2} starts from 0 and draws bars according
#' to the magnitude and direction of \code{x}
#' @param col a vector of colors having 1) the same length as \code{x}; length
#' 2 (for negative and positive values if \code{type = 2}) or otherwise to be
#' used for color interpolation (\code{\link{colorRampPalette}})
#' @param ... additional arguments passed to \code{\link{barplot}}, to/from
#' other methods, or to \code{\link{par}}
#' @param arrows logical; if \code{TRUE}, arrows are drawn in the direction
#' of each bar
#' @param rev logical; if \code{TRUE}, the order along the x-axis is reversed
#' @param plot logical; if \code{FALSE}, nothing is plotted
#' @param panel.first an "expression" to be evaluated after the plot axes are
#' set up but before any plotting takes place; this can be useful for drawing
#' background grids or scatterplot smooths; note that this works by lazy
#' evaluation: passing this argument from other plot methods may well not work
#' since it may be evaluated too early; see also \code{\link{plot.default}}
#' @param panel.last an expression to be evaluated after plotting has taken
#' place but before the axes, title, and box are added; see the comments about
#' \code{panel.first}
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
#' order and dendrogram along the rows and columns, respectively; for other
#' options, see \code{\link{heatmap}}
#' @param distfun a function used to compute the distance (dissimilarity)
#' between both rows and columns (default is \code{\link{dist}} with Euclidean
#' distance); alternatively, any of the \code{method}s can be passed as a
#' character string; \code{"spearman"} or \code{"pearson"} may also be used
#' which will calculate the distance as \code{1 - cor(x)}
#' @param hclustfun a function used to compute the hierarchical clustering
#' when \code{Rowv} or \code{Colv} are \emph{not} dendrograms; default is
#' \code{\link{hclust}}; should take as argument a result of \code{distfun}
#' and return an object to which \code{\link{as.dendrogram}} can be applied
#' @param symm logical; if \code{TRUE} and \code{x} is a square matrix,
#' \code{x} is treated \strong{symm}etrically
#' @param scale character indicating if \code{x} should be centered and
#' scaled in either the row or column direction or neither; default is none
#' @param na.rm logical; if \code{TRUE}, \code{NA}s are removed
#' @param revC logical; if \code{TRUE}, the column order is reversed for
#' plotting, e.g., for the symmetric case, the symmetry axis is as usual
#' @param add.expr an \code{\link{expression}} evaluated after the call to
#' \code{\link{image}}, useful for adding components to the plot
#' @param breaks (optional) either a numeric vector indicating the splitting
#' points for binning \code{x} into colors, or an integer number of break
#' points to be used; for the latter, the break points will be spaced equally
#' between \code{min(x)} and \code{max(x)}
#' @param symbreaks logical; if \code{TRUE}, breaks are made symmetric about
#' 0; default is \code{TRUE} if \code{x} contains negative values and
#' \code{FALSE} otherwise
#' @param cols a vector of character strings of two or more colors used for
#' interpolation and passed to \code{\link{image}}; alternatively a function
#' such as \code{\link{heat.colors}} taking an integer argument and returning
#' a vector of colors
#' @param colsep,rowsep,sepcolor,sepwidth (optional) vector of integers
#' indicating which columns or rows should be separated from the preceding
#' columns or rows by a narrow space of \code{sepcolor}; widths of space
#' between is given by \code{sepwidth}
#' @param cellnote (optional) a matrix having the same dimensions as \code{x}
#' with text to be plotted in each cell
#' @param notecex,notecol size and color for \code{cellnote}
#' @param na.color color used for \code{NA} values; defaults to background
#' color \code{par('bg')}
#' @param trace character string indicating whether a solid "trace" line
#' should be drawn across rows or down columns; the distance of the line from
#' the center of each color-cell is proportional to the size of the
#' measurement; one of \code{"row"}, \code{"column"}, \code{"both"}, or
#' \code{"none"} (default)
#' @param tracecol color for \code{trace} line
#' @param hline,vline,linecol a vector of values within cells where a
#' horizontal or vertical dotted line should be drawn; the color of the line
#' is controlled by \code{linecol} (default is \code{tracecol})
#' 
#' horizontal lines are only plotted if \code{trace} is "row" or "both";
#' vertical lines are only drawn if \code{trace} "column" or "both"; both
#' \code{hline} and \code{vline} default to the median of the \code{breaks}
#' @param margins numeric vector of length 2 controlling the margins for
#' column and row names, respectively
#' @param dmargins numeric vector of length 2 controlling the margins for
#' column and row dendrograms, respectively; useful for "squishing"
#' @param ColSideColors,RowSideColors (optional) character vector or matrix
#' with color names for horizontal or vertical side bars useful for annotating
#' columns and/or rows of \code{x}
#' @param ColSideColorsSize,RowSideColorsSize numeric value controlling the
#' sizes of the horizontal and vertical side bars, respectively
#' @param side.height.fraction scaling factor for height and width of bars
#' @param labRow,labCol row and column labels; defaults to row and column
#' names of \code{x}
#' @param cexRow,cexCol size for row and column labels
#' @param key logical; if \code{TRUE}, a color key is drawn
#' @param key.cex numeric value controlling the size of the color key
#' @param key.title,key.sub main and sub titles for the color key
#' @param density.info character string indicating whether to supermipose a
#' "histogram" or a "density" plot on the color key; default is "none"
#' @param denscol color for \code{density.info}; default is \code{tracecol}
#' @param symkey logical; if \code{TRUE}, color key will be symmetric about
#' 0; default is \code{TRUE} if \code{x} contains negative values and
#' \code{FALSE} otherwise
#' @param densadj numeric scaling value for tuning the kernel width when a
#' density plot is drawn on the color key; see the \code{adjust} parameter
#' in \code{\link{density}}; default is 0.25
#' @param main,xlab,ylab main, x-, and y-axis labels
#' @param lmat,lhei,lwid (optional) arguments for controlling \strong{l}ayout
#' \strong{hei}ghts and \code{wid}ths, passed to \code{\link{layout}}
#' @param reset_par logical; if \code{TRUE}, resets \code{\link{par}} settings
#' to state before function call; setting \code{reset_par = FALSE} is useful
#' for adding to a heatmap
#' @param ... additional arguments passed to \code{\link{image}}
#' 
#' @seealso
#' \code{\link{heatmap}}; \code{\link[gplots]{heatmap.2}}; \code{\link{image}};
#' \code{\link{dendrogram}}; \code{\link{dist}}; \code{\link{hclust}}
#' 
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(1000), 50)
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
#'   RowSideColors = rc, ColSideColors = cc
#' )
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
#'   lwid = c(5, 1, 30)
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
                      cols = c('red', 'white', 'blue'),
                      
                      ## block separation
                      colsep = NULL, rowsep = NULL,
                      sepcolor = 'white', sepwidth = c(0.05, 0.05),
                      
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
                      
                      reset_par = TRUE, ...) {
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
    # op <- par(mar = c(0,0,0,0), oma = c(0,0,0,0), new = TRUE)
    op <- par(mar = c(0,0,0,0))
    on.exit(par(op))
    plot.new()
  }
  
  if (is.character(distfun)) {
    dmethod <- match.arg(distfun, c('euclidean', 'maximum', 'manhattan',
                                    'canberra',  'binary',  'minkowski',
                                    'spearman',  'pearson'))
    if (dmethod %in% c('spearman', 'pearson'))
      distfun <- function(x)
        # as.dist(1 - abs(cor(t(x), method = dmethod)))
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
  else if (Colv == 'Rowv' && !isTRUE(Rowv))
    Colv <- FALSE
  if (length(di <- dim(x)) != 2L || !is.numeric(x))
    stop('\'x\' must be a numeric matrix')
  
  nr <- di[1L]
  nc <- di[2L]
  if (nr <= 1L || nc <= 1L)
    stop('\'x\' must have at least 2 rows and 2 columns')
  if (!is.numeric(margins) || length(margins) != 2L)
    stop('\'margins\' must be a numeric vector of length 2')
  if (is.null(cellnote))
    cellnote <- matrix('', nrow(x), ncol(x))
  
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
  
  ## row/col labels
  labRow <- if (is.null(labRow)) {
    if (is.null(rownames(x)))
      seq.int(nr)[rowInd] else rownames(x)
  } else labRow[rowInd]
  labCol <- if (is.null(labCol)) {
    if (is.null(colnames(x)))
      seq.int(nc)[colInd] else colnames(x)
  } else labCol[colInd]
  
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
    lmat <- matrix(c(6,0,4, 0,0,2, 5,3,1), 3L)
    
    if (is.null(ColSideColors)) {
      cscm <- FALSE
      ColSideColors <- matrix(0, nc, 1L)
      lhei[2L] <- 0.01
    } else if (nrow(ColSideColors) != nc)
      stop(sprintf('\'ColSideColors\' must be an n x %s matrix', nc))
    
    if (is.null(RowSideColors)) {
      rscm <- FALSE
      RowSideColors <- matrix(0, 1L, nr)
      lwid[2L] <- 0.01
    } else if (ncol(RowSideColors) != nr)
        stop(sprintf('\'RowSideColors\' must be a %s x n matrix', nr))
  }
  
  stopifnot(
    length(lwid) == 3L,
    length(lhei) == 3L,
    dim(lmat) == c(3L, 3L)
  )
  
  dev.hold()
  on.exit(dev.flush())
  
  # op <- par(mar = c(0,0,0,0), no.readonly = TRUE)
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op), add = TRUE)
  
  layout(lmat, widths = lwid, heights = lhei, respect = FALSE)
  
  par(mar = c(margins[1L], 0, 0, margins[2L]))
  x <- t(x)
  cellnote <- t(cellnote)
  if (revC) {
    iy <- rev(seq.int(nr))
    if (exists('ddr'))
      ddr <- rev(ddr)
    x <- x[, iy]
    cellnote <- cellnote[, iy]
  } else iy <- seq.int(nr)
  
  ## 1 - main image
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
  axis(1L, seq.int(nc), labels = labCol, las = 2L,
       line = -0.5, tick = 0, cex.axis = cexCol)
  if (!is.null(xlab))
    mtext(side = 1L, xlab, line = margins[1L] - 1.25)
  axis(4L, iy, labels = labRow, las = 2L,
       line = -0.5, tick = 0, cex.axis = cexRow)
  if (!is.null(ylab))
    mtext(side = 4L, ylab, line = margins[2L] - 1.25)
  
  eval(substitute(add.expr))
  
  if (!is.null(colsep))
    for (csep in colsep)
      rect(csep + 0.5, rep(0, length(csep)),
           csep + 0.5 + sepwidth[1], rep(ncol(x) + 1L, csep),
           lty = 1L, lwd = 1, col = sepcolor, border = sepcolor)
  if (!is.null(rowsep))
    for (rsep in rowsep)
      rect(0, (ncol(x) + 1L - rsep) - 0.5,
           nrow(x) + 1L, (ncol(x) + 1L - rsep) - 0.5 - sepwidth[2L],
           lty = 1L, lwd = 1, col = sepcolor, border = sepcolor)
  
  min.scale <- min(breaks)
  max.scale <- max(breaks)
  x.scaled  <- scale01(t(x), min.scale, max.scale)
  
  if (trace %in% c('both', 'column')) {
    res$vline <- vline
    vline.vals <- scale01(vline, min.scale, max.scale)
    
    for (i in colInd) {
      if (!is.null(vline))
        abline(v = i - 0.5 + vline.vals, col = linecol, lty = 2L)
      xv <- rep(i, nrow(x.scaled)) + x.scaled[, i] - 0.5
      xv <- c(xv[1L], xv)
      yv <- seq.int(length(xv)) - 0.5
      lines(xv, yv, lwd = 1, col = tracecol, type = 's')
    }
  }
  
  if (trace %in% c('both', 'row')) {
    res$hline <- hline
    hline.vals <- scale01(hline, min.scale, max.scale)
    
    for (i in rowInd) {
      if (!is.null(hline))
        abline(h = i + hline, col = linecol, lty = 2L)
      yv <- rep(i, ncol(x.scaled)) + x.scaled[i, ] - 0.5
      yv <- rev(c(yv[1L], yv))
      xv <- rev(seq.int(length(yv))) - 0.5
      lines(xv, yv, lwd = 1, col = tracecol, type = 's')
    }
  }
  
  if (!is.null(cellnote))
    text(c(row(cellnote)), c(col(cellnote)), labels = c(cellnote),
         col = notecol, cex = notecex)
  
  ## 2 - matrix of colors on left
  if (!is.null(RowSideColors)) {
    rsc <- t(RowSideColors[, rowInd, drop = FALSE])
    rsc.colors <- matrix()
    rsc.names <- names(table(rsc))
    
    rsc.i <- 1L
    for (rsc.name in rsc.names) {
      rsc.colors[rsc.i] <- rsc.name
      rsc[rsc == rsc.name] <- rsc.i
      rsc.i <- rsc.i + 1L
    }
    
    rsc <- matrix(as.numeric(rsc), nrow(rsc))
    if (rscm) {
      par(mar = c(margins[1L], 0, 0, 0.5))
      image(t(rsc), col = as.vector(rsc.colors), axes = FALSE)
      
      if (length(rownames(RowSideColors)))
        axis(1L, 0:(ncol(rsc) - 1L) / max(1, (ncol(rsc) - 1L)),
             rownames(RowSideColors), las = 2L, tick = FALSE)
    } else plot.null()
  }
  
  ## 3 - matrix of colors on top
  if (!is.null(ColSideColors)) {
    csc <- ColSideColors[colInd, , drop = FALSE]
    csc.colors <- matrix()
    csc.names <- names(table(csc))
    
    csc.i <- 1L
    for (csc.name in csc.names) {
      csc.colors[csc.i] <- csc.name
      csc[csc == csc.name] <- csc.i
      csc.i <- csc.i + 1L
    }
    
    csc <- matrix(as.numeric(csc), nrow = nrow(csc))
    if (cscm) {
      par(mar = c(0.5, 0, 0, margins[2L]))
      image(csc, col = as.vector(csc.colors), axes = FALSE)
      
      if (length(colnames(ColSideColors)))
        axis(2L, 0:(ncol(csc) - 1L) / max(1, (ncol(csc) - 1L)),
             colnames(ColSideColors), las = 2L, tick = FALSE)
    } else plot.null()
  }
  
  ## 4 - row dendrogram
  if (Rowv) {
    par(mar = c(margins[1L], dmargins[1L], 0, 0))
    plot(ddr, horiz = TRUE, axes = FALSE, yaxs = 'i', leaflab = 'none')
  } else plot.null()
  
  ## 5 - col dendrogram
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
  res$layout <- list(lmat = lmat, lhei = lhei, lwid = lwid)
  
  invisible(res)
}
