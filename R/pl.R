### plot functions
# jmplot, tplot, dsplot, waffle, river, river2, dose_esc
###


#' Joint-marginal plot
#' 
#' Joint and marginal distributions scatterplots with \code{\link{tplot}}s on
#' margins.
#' 
#' @param x x-axis variable
#' @param y y-axis variable
#' @param z group variable
#' @param main overall title for the plot
#' @param sub sub-title for the plot (below x-axis)
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param names labels for \code{x} and \code{y} variables
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param axes logical; draw axes
#' @param frame.plot logical; draw box around \code{x-y} plot
#' @param log character, "x", "y", or "xy" for logarithmic scale; sets negative
#' values to \code{\link{NA}} and gives a warning; see \code{\link{xy.coords}}
#' @param xratio ratio of \code{x-y} plot to bar plots along x-axis; see 
#' \code{widths} in \code{\link{layout}}
#' @param yratio ratio of \code{x-y} plot to bar plots along x-axis; see
#' \code{heights} in \code{\link{layout}}
#' @param show.n logical; show number in each group
#' @param show.na logical; show number missing in each group
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
#' @references
#' \url{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}
#' 
#' @examples
#' set.seed(1618)
#' dat <- data.frame(x = rnorm(100, 20, 5),
#'                   y = rexp(100),
#'                   z = c('M','F'),
#'                   zz = c(LETTERS[1:4]))
#' with(dat, jmplot(x, y, zz, type = 'db', jit = .02, col = 1:4, las = 1,
#'      group.col = TRUE, pch = 1:4, group.pch = TRUE, boxcol = grey(.9), 
#'      cex.n = .5))
#'        
#' @export

jmplot <- function(x, y, z, 
                   
                   ## labels
                   main, sub, xlab, ylab, names, 
                   
                   ## axes stuff
                   xlim, ylim, axes = TRUE, frame.plot = axes, 
                   log = '', xratio = .8, yratio = xratio, 
                   
                   ## more options
                   show.n = TRUE, show.na = TRUE, cex.n, 
                   ann = par('ann'), asp = NA, 
                   panel.first = NULL, panel.last = NULL,
                   
                   ...) {
  
  localTplot <- function(..., type = 'b', horizontal = FALSE) 
    tplot(..., type = type, axes = FALSE, horizontal = horizontal)
  eliminateTplot <- function(func, ..., type, dist, jit, names, group.col, 
                             boxcol, boxborder, group.pch, median.line, 
                             mean.line, median.pars, mean.pars, boxplot.pars, 
                             my.gray, axes, frame.plot, add, horizontal) 
    func(...)
  
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
  
  z <- as.factor(z)
  xy <- xy.coords(x, y, deparse(substitute(x)), deparse(substitute(y)), log)
  
  ## defaults
  if (missing(names)) 
    names <- levels(z)
  if (missing(xlab)) 
    xlab <- xy$xlab
  if (missing(ylab)) 
    ylab <- xy$ylab
  if (missing(sub)) sub <- ''
  if (missing(main)) main <- ''
  
  op <- par(no.readonly = TRUE)
  mar <- op$mar
  ## set the layout
  layout(matrix(c(1, 3, 0, 2), 2), 
         widths = c(xratio, 1 - xratio), 
         heights = c(1 - yratio, yratio))
  par(mar = c(0, 0, 0, 0), 
      oma = c(0,0, mar[3], mar[4]) + op$oma)
  
  ## calculate xlim, ylim
  lim <- function(z) {
    r <- range(z, na.rm = TRUE, finite = TRUE)
    #     pm <- diff(r) / 20
    #     r + pm * c(-1,1)
  }
  if (missing(xlim)) 
    xlim <- lim(xy$x)
  if (missing(ylim)) 
    ylim <- lim(xy$y)
  if (missing(cex.n)) 
    cex.n <- 1
  
  ## plot x distribution on top
  par(mar = c(0, mar[2], 0, 0))
  localTplot(x ~ z, ylim = xlim, horizontal = TRUE, 
             show.n = FALSE, show.na = FALSE, ...)
  if (axes) 
    localAxis(side = 2, at = 1:nlevels(z), labels = names, ...)
  
  ## plot y distribution on right
  par(mar = c(mar[1], 0, 0, 0))
  localTplot(y ~ z, ylim = ylim, show.n = show.n, show.na = show.na, 
             cex.n = cex.n, ...)
  if (axes) 
    localAxis(side = 1, at = 1:nlevels(z), labels = names, ...)
  
  ## plot xy points
  par(mar = c(mar[1], mar[2], 0, 0))
  plot.new()
  localWindow(xlim, ylim, log, asp, ...)
  panel.first
  localPlot(xy, xlim = xlim, ylim = ylim, ...)
  panel.last
  
  ## add axes
  if (axes) {
    localAxis(side = 1, ...)
    localAxis(side = 2, ...)
  }
  if (frame.plot) 
    localBox(...)
  ## add titles
  if (ann) {
    localTitle(sub = sub, xlab = xlab, ylab = ylab, ...)
    localTitle(main = main, outer = TRUE, ...)
  }
}

#' tplot (boxplot)
#' 
#' An alternative to \code{\link{boxplot}}. The individual data can be shown 
#' (either in the foreground or background) with jittering if necessary.
#' 
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
#' @param x for specifying data from which the boxplots are to be produced; 
#' either a numeric vector or a single list containing such vectors; additional
#' unnamed arguments specify further data as separate vectors (each 
#' corresponding to a component boxplot); \code{NA}s are allowed in the data
#' @param ... for the \code{formula} method, named arguments to be passed to 
#' the default method
#' 
#' for the default method, unnamed arguments are additional data vectors
#' (unless x is a list when they are ignored), and named arguments are 
#' arguments and \code{\link{par}}s to be passed to \code{\link{bxp}}
#' @param type type of plot (dot, dot-box, box-dot, box)
#' @param main,sub overall title and sub-title for the plot (below x-axis)
#' @param xlab,ylab x- and y-axis labels
#' @param names group labels
#' @param xlim,ylim x- and y-axis limits
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
#' @param boxcol fill color
#' @param boxborder border color
#' @param pch plotting character
#' @param group.pch logical; if \code{TRUE}, \code{pch} by group; o/w, by order
#' @param cex \strong{c}haracter \strong{ex}pansion value
#' @param group.cex logical; if \code{TRUE}, groups will use the same
#' \code{cex} value; otherwise, points will have individual values, recycled if
#' necessary
#' @param median.line logical; draw median line
#' @param mean.line logical; draw mean line
#' @param median.pars list of graphical parameters for median line
#' @param mean.pars see above
#' @param show.n logical; show number in each group
#' @param show.na logical; show number missing in each group
#' @param cex.n character expansion for \code{show.n} and \code{show.na}
#' @param my.gray default border color
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
#' 
#' @return
#' A list with length equal to the number of groups
#' 
#' @seealso
#' \href{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}{Tatsuki
#' \code{tplot}}; \href{http://data.vanderbilt.edu/~graywh/dotplot/}{web app
#' for Tatsuki \code{tplot}}
#' 
#' @examples
#' ## equivalent ways to call tplot
#' ## the formula method is a convenience function for the first case
#' tplot(split(mtcars$mpg, interaction(mtcars$gear, mtcars$vs)))
#' tplot(mpg ~ gear + vs, mtcars)
#' 
#' ## tplot has the same return value as boxplot
#' identical(tplot(mtcars$mpg), boxplot(mtcars$mpg))
#' 
#' ## use panel.first/panel.last like in `plot` (unavailable in `boxplot`)
#' tplot(mpg ~ gear, data = mtcars, col = 1:3, type = 'd',
#'       cex = c(1,5)[(mtcars$mpg > 30) + 1L],
#'       panel.last = legend('topleft', legend = 3:5, col = 1:3, pch = 1),
#'       panel.first = {
#'         abline(h = mean(mtcars$mpg))
#'         abline(h = 1:6 * 5 + 5, lty = 'dotted', col = 'grey70')
#'       })
#' 
#' set.seed(1)
#' dat <- data.frame(age = rnorm(80, rep(c(26, 36), c(70, 10)), 4),
#'                   sex = sample(c('Female', 'Male'), 80, replace = TRUE),
#'                   group = paste0('Group ', 
#'                                  sample(1:4, 40, prob = c(2, 5, 4, 1), 
#'                                         replace = TRUE)))
#' dat[1:5, 'age'] <- NA
#' 
#' tplot(age ~ group, data = dat, las = 1, cex.n = .8, cex.axis = 1, bty = 'L',
#'       type = c('db', 'db', 'db', 'd'), names = LETTERS[1:4],
#'       group.pch = TRUE, pch = c(15, 17, 19, 8),
#'       group.col = FALSE, col = c('darkred', 'darkblue')[c(sex)],
#'       boxcol = c('lightsteelblue1', 'lightyellow1', grey(.9)),
#'       boxplot.pars = list(notch = TRUE, boxwex = .5))
#'
#' @export

tplot <- function(x, ...) UseMethod('tplot')

#' @rdname tplot
#' @export
tplot.default <- function(x, ...,
                          type = c('d','db','bd','b'), jit = 0.1, dist,
                          ## labels/aesthetics
                          main, sub, xlab, ylab, names, xlim, ylim,
                          col, group.col = TRUE, boxcol, boxborder,
                          pch = par('pch'), group.pch = TRUE,
                          cex = par('cex'), group.cex = FALSE,
                          
                          ## additional aesthetics
                          median.line = FALSE, mean.line = FALSE, 
                          median.pars = list(col = par('col')), 
                          mean.pars = median.pars, boxplot.pars,
                          
                          ## n/missing for each group
                          show.n = TRUE, show.na = TRUE, cex.n,
                          
                          ## extra stuff
                          my.gray = gray(0.75), ann = par('ann'),
                          axes = TRUE, frame.plot = axes,
                          add = FALSE, at, horizontal = FALSE,
                          
                          panel.first = NULL, panel.last = NULL) {
  
  op <- par(no.readonly = TRUE)
  
  ## helpers
  localPoints <- function(..., tick) points(...)
  localAxis   <- function(..., bg, cex, lty, lwd) axis(...)
  localBox    <- function(..., bg, cex, lty, lwd, tick) box(...)
  localWindow <- function(..., bg, cex, lty, lwd, tick) plot.window(...)
  localTitle  <- function(..., bg, cex, lty, lwd, tick) title(...)
  localMtext  <- function(..., bg, cex, lty, lwd, tick) mtext(..., cex = cex.n)
  
  args <- list(x, ...)
  namedargs <- if (!is.null(attributes(args)$names))
    attributes(args)$names !=  '' else logical(length(args))
  groups <- if (is.list(x)) x else args[!namedargs]
  pars <- args[namedargs]
  if ((n <- length(groups)) == 0)
    stop('invalid first argument')
  if (length(class(groups)))
    groups <- unclass(groups)
  if (!missing(names))
    attr(groups, 'names') <- names else {
      if (is.null(attr(groups, 'names')))
        attr(groups, 'names') <- 1:n
      names <- attr(groups, 'names')
    }
  
  ## number and size of groups
  ng <- length(groups)
  l <- sapply(groups, length)
  g <- factor(rep(1:ng, l), levels = 1:ng, labels = names(groups))
  nv <- sum(l)
  
  if (missing(at))
    at <- 1:ng
  if (length(at) !=  ng)
    stop("\'at\' must have same length as the number of groups")
  
  ## scales
  if (missing(ylim)) {
    r <- range(groups, na.rm = TRUE, finite = TRUE)
    pm <- diff(r) / 20
    ylim <- r + pm * c(-1, 1)
  }
  if (missing(xlim)) {
    if (missing(at))
      xlim <- c(0.5, ng + 0.5)
    else xlim <- c(0.5, max(at) + 0.5)
  }
  
  if (missing(xlab))   xlab <- ''
  if (missing(ylab))   ylab <- ''
  if (missing(main))   main <- ''
  if (missing(sub))    sub  <- ''
  if (missing(boxcol)) boxcol <- 'grey97'
  
  type <- match.arg(type, choices = c('d','db','bd','b'), several.ok = TRUE)
  ## type of plot for each group
  type <- rep(type, length.out = ng)
  
  ## default colors
  ## 50% gray for box/dots in back, otherwise default color
  defcols <- c(my.gray, par('col'))
  
  if (missing(boxborder))
    boxborder <- defcols[2 - grepl('.b', type)]
  if (missing(col)) {
    col <- defcols[2 - grepl('.d', type)]
    group.col <- TRUE
  }
  if (missing(boxplot.pars))
    boxplot.pars <- NULL
  
  boxborder <- rep(boxborder, length.out = ng)
  boxcol <- rep(boxcol, length.out = ng)
  
  if (group.col) {
    ## colors by group
    g.col <- rep(col, length.out = ng)
    col <- rep(g.col, l)
  } else {
    ## colors by individual or global
    col   <- rep(col, length.out = nv)
    g.col <- rep(1, length.out = ng)
  }
  pch <- if (group.pch) {
    ## plot characters by group
    rep(rep(pch, length.out = ng), l)
  } else {
    ## plot characters by individual or global
    rep(pch, length.out = nv)
  }
  cex <- if (group.cex) {
    ## plot characters by group
    rep(rep(cex, length.out = ng), l)
  } else {
    ## plot characters by individual or global
    rep(cex, length.out = nv)
  }
  
  ## split colors and plot characters into groups
  col <- split(col, g)
  pch <- split(pch, g)
  cex <- split(cex, g)
  
  ## remove any NAs from the data and options
  nonas <- lapply(groups, function(x) !is.na(x))
  l2 <- sapply(groups, function(x) sum(is.na(x)))
  groups <- mapply('[', groups, nonas, SIMPLIFY = FALSE)
  l <- sapply(groups, length)
  col <- mapply('[', col, nonas, SIMPLIFY = FALSE)
  pch <- mapply('[', pch, nonas, SIMPLIFY = FALSE)
  cex <- mapply('[', cex, nonas, SIMPLIFY = FALSE)
  
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
  if (!add) {
    plot.new()
    if (horizontal)
      do.call('localWindow', c(list(ylim, xlim), pars))
    else do.call('localWindow', c(list(xlim, ylim), pars))
  }
  panel.first
  
  out <- list()
  Lme <- 0.2 * c(-1, 1)
  
  for (i in 1:ng) {
    to.plot <- groups[[i]]
    gs <- to.plot$g.si
    hms <- to.plot$hmsf
    x <- rep(at[i], nrow(to.plot)) + jit_(gs, hms) * jit
    y <- to.plot$vs
    
    ## dots behind
    if (type[i] == 'bd') {
      boxplotout <- do.call('boxplot',
                            c(list(x = y, at = at[i], plot = FALSE, add = FALSE,
                                   axes = FALSE, col = boxcol[i], 
                                   border = boxborder[i], outline = FALSE, 
                                   horizontal = horizontal), boxplot.pars))
      notoplot <- (y <=  boxplotout$stats[5,]) & (y >=  boxplotout$stats[1,])
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
      boxplotout <- do.call('boxplot',
                            c(list(x = y, at = at[i], add = TRUE, 
                                   axes = FALSE, col = boxcol[i], 
                                   border = boxborder[i], outline = FALSE, 
                                   horizontal = horizontal), boxplot.pars))
      toplot <- (y > boxplotout$stats[5,]) | (y < boxplotout$stats[1,])
      if (sum(toplot) > 0) 
        if (col[[i]][toplot][1] == '#bfbfbf') 
          col[[i]][toplot] <- 1
      if (horizontal) {
        do.call('localPoints', 
                c(list(x = y[toplot], y = x[toplot], pch = pch[[i]][toplot],
                       col = col[[i]][toplot], cex = cex[[i]][toplot]),
                  pars))
      } else do.call('localPoints', c(list(x = x[toplot], y = y[toplot],
                                           pch = pch[[i]][toplot],
                                           col = col[[i]][toplot],
                                           cex = cex[[i]][toplot]),
                                      pars))
    }
    ## box behind
    if (type[i] == 'db')
      boxplotout <- do.call('boxplot',
                            c(list(x = y, at = at[i], add = TRUE, axes = FALSE,
                                   col = boxcol[i], border = boxborder[i],
                                   outline = FALSE, horizontal = horizontal),
                              boxplot.pars))
    ## dots in front
    if (type[i] %in% c('db', 'd')) {
      if (horizontal)
        do.call('localPoints', c(list(x = y, y = x, pch = pch[[i]],
                                      col = col[[i]], cex = cex[[i]]),
                                 pars))
      else do.call('localPoints', c(list(x = x, y = y, pch = pch[[i]],
                                         col = col[[i]], cex = cex[[i]]),
                                    pars))
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
      else do.call('lines', c(list(at[i] + Lme, rep(median(y), 2)), median.pars))
    }
    
    ## use return value of boxplot instead
    # out[[i]] <- data.frame(to.plot, col = col[[i]], pch = pch[[i]])
    out[[i]] <- if (type[i] == 'd')
      boxplot(x = y, at = at[i], plot = FALSE) else boxplotout
  }
  panel.last
  
  if (axes) {
    do.call('localAxis', c(list(side = 1 + horizontal, at = at,
                                labels = names), pars))
    do.call('localAxis', c(list(side = 2 - horizontal), pars))
  }
  
  ## frame and text, optional sample sizes
  if (show.n) {
    if (missing(cex.n))
      cex.n <- 1
    do.call('localMtext',
            if (show.na)
              c(list(sprintf('n = %s\nmissing = %s', l, l2),
                     side = 3 + horizontal, at = at),
                pars, list(xaxt = 's', yaxt = 's'))
            else c(list(paste0('n = ', l), side = 3 + horizontal, at = at),
                   pars, list(xaxt = 's', yaxt = 's')))
  }
  if (frame.plot)
    do.call('localBox', pars)
  if (ann) {
    if (horizontal)
      do.call('localTitle', c(list(main = main, sub = sub,
                                   xlab = ylab, ylab = xlab), pars))
    else do.call('localTitle', c(list(main = main, sub = sub,
                                      xlab = xlab, ylab = ylab), pars))
  }
  
  names(out) <- names(groups)
  invisible(if (length(out) == 1L) out[[1]] else Reduce(clist, out))
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
#' @param x x-axis variable
#' @param y y-axis variable
#' @param ... for the \code{formula} method, named arguments to be passed to 
#' the default method
#' 
#' for the default method, unnamed arguments are additional data vectors
#' (unless x is a list when they are ignored), and named arguments are 
#' arguments and \code{\link{par}}s to be passed to \code{\link{plot}}
#' @param bkgr logical; fill boxes with gray scale based on density
#' @param col plotting color
#' @param pch \strong{p}lotting \strong{ch}aracter
#' @param cex numerical value giving the amount by which plotting text and 
#' symbols should be magnified relative to the default; this starts as 1 when 
#' a device is opened and is reset when the layout is changed, e.g., by setting
#' \code{mfrow}
#' 
#' @return
#' An \code{\link{invisible}} table corresponding to the plot cell counts.
#' 
#' @references
#' \url{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}
#' 
#' @examples
#' set.seed(1)
#' x <- round(rnorm(400, 100, 4))
#' y <- round(rnorm(400, 200, 4))
#' sex <- sample(c('Female', 'Male'), 400, replace = TRUE)
#' dsplot(y ~ x, pch = 19, col = 1 + (sex %in% 'Female'), cex = .6,
#'        xlab = 'measurement 1', ylab = 'measurement 2', bty = 'L')
#' legend('bottomright', pch = 19, col = 1:2, 
#'        legend = c('Male', 'Female'), cex = .8)
#'        
#' @export

dsplot <- function(x, ...) UseMethod('dsplot')

#' @rdname dsplot
#' @export
dsplot.default <- function(x, y, ..., bkgr = TRUE, col = 1,
                           pch = 19, cex = 0.8) {
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  # if (any(x != round(x), na.rm = TRUE) | any(y != round(y), na.rm = TRUE))
  #   stop('\'x\' must be integer values', '\n')
  
  L <- length(x)
  cc <- complete.cases(x, y)
  
  if (length(pch) < L) pch <- rep(pch, length.out = L)
  if (length(col) < L) col <- rep(col, length.out = L)
  if (length(cex) < L) cex <- rep(cex, length.out = L)
  
  x <- x[cc]
  y <- y[cc]
  pch <- pch[cc]
  col <- col[cc]
  cex <- cex[cc]
  
  x.levels <- sort(unique(x))
  y.levels <- sort(unique(y))
  tab <- table(x, y)
  max.freq <- max(tab)
  box.size <- ceiling(sqrt(max.freq))
  X <- range(x) + c(0, 1)
  Y <- range(y) + c(0, 1)
  plot(X, Y, las = 1, type = 'n', xaxs = 'i', yaxs = 'i', bty = 'n', 
       xaxt = 'n', yaxt = 'n', ...)
  axis(1, at = pretty(x) + .5, labels = pretty(x), tick = FALSE, las = 1)
  axis(2, at = pretty(y) + .5, labels = pretty(y), tick = FALSE, las = 1)
  
  if (!bkgr) {
    for (i in y.levels)
      segments(min(x), i, max(x), i, col = grey(.9))
    for (i in x.levels)
      segments(i, min(y), i, max(x), col = grey(.9))
  }
  
  ## vector 1,n,2,n,3,n, ...,  n, n for x
  ## vector n,1,n,2,n,3, ..., n-1,n for y
  every.other.element.x <- function(n) c(rbind(1:n, rep(n, n)))[-(2 * n)]
  every.other.element.y <- function(n) c(rbind(rep(n, n), 1:n))[-(2 * n)]
  
  square.coordinates <- function(box.size) {
    x.c <- y.c <- 1
    for (i in 2:box.size)
      x.c <- c(x.c, every.other.element.x(i))
    for (j in 2:box.size)
      y.c <- c(y.c, every.other.element.y(j))
    data.frame(x.c, y.c)
  }
  
  sc <- square.coordinates(box.size)
  coord <- (1:box.size) / (box.size + 1)
  off.set <- coord[1] / 4
  grey.scale <- rev(seq(.65, .95, length = max.freq))
  
  dat <- data.frame(id = 1:length(x), x, y)
  dat <- dat[order(dat$x, dat$y), ]
  within <- c(t(tab))
  within <- within[within > 0]
  idx <- hm <- NULL
  
  for (i in within) {
    ## index within category
    idx <- c(idx, 1:i)
    hm <- c(hm, rep(i, i))
  }
  dat$idx <- idx
  ## local offset
  dat$ly <- (box.size - ceiling(sqrt(hm))) / (box.size + 1) / 2
  dat$lx <- dat$ly + ((ceiling(sqrt(hm - 1)) ** 2 == hm - 1) & (hm > 1)) /
    (box.size + 1) / 2
  dat <- dat[order(dat$id), ]
  dat$col <- col
  dat$pch <- pch
  
  if (bkgr) {
    for (i in x.levels) {
      for (j in y.levels) {
        n <- sum(x == i & y == j)
        if (n > 0) { 
          rect(i + off.set, j + off.set, i + 1 - off.set, j + 1 - off.set, 
               border = grey(grey.scale[n]), col = grey(grey.scale[n]))
        }
      }
    }
  }
  points(dat$x + coord[sc[dat$idx, 1]] + dat$lx, dat$y + 
           coord[sc[dat$idx, 2]] + dat$ly, pch = dat$pch, 
         col = dat$col, cex = cex)
  invisible(table(factor(y, levels = rev(min(y):max(y))), 
                  factor(x, levels = min(x):max(x))))
}

#' @rdname dsplot
#' @export
dsplot.formula <- function(formula, data = NULL, ...,
                           subset, na.action = NULL) {
  if (missing(formula) || (length(formula) != 3))
    stop("\'formula\' missing or incorrect")
  enquote <- function(x) as.call(list(as.name('quote'), x))
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  args <- lapply(m$..., eval, data, parent.frame())
  nmargs <- names(args)
  
  if ('main' %in% nmargs) args[['main']] <- enquote(args[['main']])
  if ('sub' %in% nmargs)  args[['sub']] <- enquote(args[['sub']])
  if ('xlab' %in% nmargs) args[['xlab']] <- enquote(args[['xlab']])
  if ('ylab' %in% nmargs) args[['ylab']] <- enquote(args[['ylab']])
  
  # m$na.action <- na.pass
  subset.expr <- m$subset
  m$... <- m$subset <- NULL
  
  m[[1]] <- as.name('model.frame')
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
  do.call('dsplot', c(list(mf[[response]], mf[[-response]]), args))
}

#' waffle chart
#' 
#' A waffle chart.
#' 
#' @param mat a matrix of integers or character strings of color names; if
#' \code{mat} is a matrix of integers, the colors used will correspond to
#' the current \code{\link{palette}}
#' @param xpad,ypad amount of padding between \code{rect}s along axes
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' @param reset_par logical; if \code{TRUE}, resets \code{par} to current
#' settings after running \code{waffle}
#' 
#' @return
#' A list of three matrices: \code{matrix}, the input matrix; \code{origin},
#' coordinates of the bottom-left corner for each box; and \code{centers},
#' the coordinates for the centers of each box adjusted for \code{xpad} and
#' \code{ypad}.
#' 
#' @examples
#' waffle(matrix(1:8, 2))
#' 
#' ## heatmap
#' cols <- c(cor(mtcars))
#' cols <- tcol(c('blue','red')[(cols > 0) + 1L], alpha = c(abs(cols)))
#' waffle(matrix(cols, 11)[11:1, ], reset_par = FALSE, ypad = 0)
#' axis(3, at = 1:11 - .5, labels = names(mtcars), cex.axis = .8, lwd = 0)
#' 
#' ## adding to margins of another plot
#' set.seed(1)
#' n <- 100
#' ng <- 3
#' cols <- c('beige','dodgerblue2','green','orange')
#' x <- sample(cols, n * ng, replace = TRUE, prob = c(.05,.31,.32,.32))
#' x <- rawr::kinda_sort(x, n = 20)
#' 
#' par(fig = c(0,1,.2,.9), mar = c(0,5,0,1))
#' plot(cumsum(rnorm(n)), type = 'l', ann = FALSE, xaxt = 'n')
#' par(fig = c(0,1,0,.2), mar = c(1,5,0,1), new = TRUE)
#' waffle(matrix(x, ng))
#' par(fig = c(0,1,.9,1), mar = c(.5,5,.5,1), new = TRUE)
#' waffle(matrix(x, ng)[1, , drop = FALSE], ypad = 0, reset_par = FALSE)
#' box()
#' 
#' ## waffle conveniently returns the centers of the rects
#' ## be sure /not/ to reset pars on exit for proper alignment
#' (w <- waffle(matrix(1:8, 2), reset_par = FALSE))
#' text(w$c[, 'x'], w$c[, 'y'], labels = palette(), col = 'white')
#' 
#' @export

waffle <- function(mat, xpad = 0, ypad = .05, ..., reset_par = TRUE) {
  op <- par(no.readonly = TRUE)
  if (reset_par)
    on.exit(par(op))
  plot.new()
  par(list(...))
  o <- cbind(c(row(mat)), c(col(mat))) - 1
  plot.window(xlim = c(0, max(o[, 2]) + 1), ylim = c(0, max(o[, 1]) + 1),
              xaxs = 'i', yaxs = 'i')
  rect(xl <- o[, 2], yb <- o[, 1], xr <- o[, 2] + (1 - xpad),
       yt <- o[, 1] + (1 - ypad), col = c(mat), border = NA)
  invisible(list(matrix = mat, origin = `colnames<-`(o[, 2:1], c('x','y')),
                 centers = cbind(x = psum(xl, xr) / 2, y = psum(yb, yt) / 2)))
}

#' River plots
#' 
#' Summarize a timeline for individuals over the course of a study.
#' 
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
#' river(dd, bd, stagger = FALSE, rev = TRUE,
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
#' river(dd, bd1, id = c(1,2,5,3,4), at = c(1:3, 5:6), legend = FALSE)
#' 
#' 
#' ## additional data for river2
#' tt <- data.frame(id = rep(1:2, times = c(1, 5)),
#'                  dt_start = c(3,5,5,8,9,11),
#'                  dt_end = c(NA,5,NA,10,10,NA),
#'                  grade = c(1,4,3,4,1,2),
#'                  desc = paste('tox', c(1,1:5)))
#' 
#' river2(dd, bd, tt, id = 2)
#' 
#' ## multiple records per id (ie, worsening toxicities)
#' tt2 <- data.frame(id = rep(1:2, times = c(1, 8)),
#'                   dt_start = c(3,5,5,7,15,8,9,11,14),
#'                   dt_end = c(NA,5,7,15,NA,10,10,14,NA),
#'                   grade = c(1,4,1,2,3,4,1,2,3),
#'                   desc = paste('tox', c(1,1,2,2,2,3,4,5,5)))
#'                   
#' river2(dd, bd, tt2, id = 2)
#' 
#' 
#' ## bar_data can also be given in river2 without additional information
#' river2(bar_data = tt, id = 2)
#' river2(bar_data = tt2, id = 2)
#' 
#' @export

river <- function(data, bar_data, id, at = id, legend = 'topleft',
                  xlim, ylim, rev = FALSE, stagger = TRUE) {
  ## error checks
  dd <- check_river_format(data)
  bd <- check_river_format(data, bar_data)
  
  dd <- merge(bd, dd, by = 'id', all = TRUE)
  nn <- seq_along(unique(dd$id))
  
  ## colors for resp - PD:CR
  cols <- c('red','transparent','yellow','orange',
            'deepskyblue','dodgerblue3','blue4')
  
  if (missing(id))
    id <- nn else stopifnot(all(id %in% nn & length(at) == length(id)))
  if (rev)
    at <- rev(at)
  
  ## convert dates to days with origin at first id reg (ie, ref date == 0)
  dts <- grep('^dt_', names(dd))
  mm <- setNames(dd[, dts], gsub('dt_', 'dd_', names(dd)[dts]))
  rx <- if (stagger)
    range(unlist(mm), na.rm = TRUE) else
      range(unlist(mm[dd$id %in% id, , drop = FALSE]), na.rm = TRUE)
  
  mm[] <- lapply(mm, as.numeric)
  mm <- t(apply(mm, 1, function(x)
    x - min(if (stagger) mm[, 'dd_reg'] else x['dd_reg'], na.rm = TRUE)))
  
  dd <- within(cbind(dd, mm), {
    end_day <- apply(mm, 1, max, na.rm = TRUE)
    end_day <- pmax(end_day, dd_reg)
    alive <- is.na(status) | (grepl('(?i)[0v]', status) + 0L)
    censor <- alive & !is.na(dt_offstudy)
    assess <- as.factor(assess)
    col_assess <- cols[as.numeric(assess)]
    ## no red rects after progression date
    col_assess[col_assess == 'red' & dd_prog <= dd_assess_start] <- 'transparent'
  })
  
  plot.new()
  par(mar = c(4,1,1,0))
  plot.window(if (!missing(xlim)) xlim else c(0, diff(rx)),
              ## set min ylim to c(0,5) for case: id < 5
              if (!missing(ylim)) ylim else range(c(0, at, 5)))
  axis(1, tcl = .2, las = 1)
  title(xlab = sprintf('Days from %sregistration', c('','first ')[stagger + 1L]),
        line = 2.5)
  
  if (legend[[1]] != FALSE)
    do.call('legend', modifyList(list(
      x = legend, fill = c('red', cols[-1]), legend = levels(dd$assess),
      horiz = FALSE, cex = .8, bty = 'n'), val = as.list(legend)))
  
  sp <- split(dd, dd$id, drop = FALSE)
  
  for (ii in id) {
    ## lines at specific points requires new index
    jj <- at[which(id %in% ii)]
    
    # with(dd[ii, ], {
    with(sp[[as.character(ii)]], {
      ## label ids in black to left of rect
      text(dd_reg[1], jj, labels = id[1], pos = 2, xpd = NA)
      
      ## lines - time alive, on tx
      do_seg_(jj, dd_reg, end_day, arrow = alive & !censor,
              single = TRUE, col = 1)
      do_seg_(jj, dd_txstart, dd_txend %|% end_day, arrow = FALSE,
              single = TRUE, lty = 1, lwd = 4, col = 'green4')
      
      ## rects - assessments
      do_rect_(jj, dd_assess_start, dd_assess_end %|% end_day,
               col = tcol(col_assess, alpha = .5))
      points(dd_assess_start, rep(jj, length(dd_assess_start)),
             pch = '|', col = 1)
      
      ## points - prog (red circle), death, (red x), censor (blue x)
      points(dd_prog[1], jj, pch = 16, col = 2, cex = 1.5)
      points(end_day[1], jj, pch = c(4, NA)[alive[1] + 1L],
             col = 2, lwd = 3, cex = 1.5)
      points(end_day[1], jj, pch = c(NA, 4)[(alive[1] & censor[1]) + 1L],
             col = 4, lwd = 3, cex = 1.5)
    })
  }
  invisible(list(data = dd, bar_data = bd))
}

#' @rdname river
#' @export
river2 <- function(data, bar_data, bar_data2, id = 1, legend = 'topleft',
                   xlim, ylim, rev = FALSE, stagger = FALSE, split = FALSE) {
  ## error checks
  if (!missing(data)) {
    if (missing(bar_data2))
      return(river(data = data, bar_data = bar_data, id = id, at = 1, rev = rev,
                   legend = legend, xlim = xlim, ylim = ylim, stagger = stagger))
  } else {
    mmin <- function(x) min(x, na.rm = !all(is.na(x)))
    mmax <- function(x) max(x, na.rm = !all(is.na(x)))
    bar_data2 <- if (missing(bar_data2)) bar_data else bar_data2
    if (ncol(bar_data2) <= 3)
      ## use bar_data2 format--check_river_format()
      bar_data2 <- bar_data[, c(1,2,2,3,3)]
    data <- data.frame(
      id = bar_data[, 1], dt_reg = ave(bar_data[, 2], bar_data[, 1], FUN = mmin),
      dt_txst = NA, dt_txend = NA, dt_prog = NA, dt_offtx = NA, dt_surv = NA,
      surv = NA, dt_last = ave(unlist(bar_data[, 2:3]),
                               unlist(bar_data[, c(1,1)]), FUN = mmax),
      dt_off = NA)
    data <- data[!duplicated(data$id), ]
    bar_data <- data.frame(id = data$id, dt_assess = data$dt_reg, resp = NA)
  }
  
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
  cols <- c('blue','green','orange','red','black')
  nn <- if (split) seq.int(nrow(td)) else seq_along(unique(td$desc))
  
  ## base plot of the id summary
  rv <- river(data = data, bar_data = bar_data, id = id, at = 1,
              legend = FALSE, rev = FALSE, stagger = stagger, xlim = xlim,
              ylim = if (missing(ylim))
                c(0, max(5, (if (split) max(nn) else length(nn)) + 1)) else ylim)
  
  td <- within(td, {
    dt_reg <- rv$data$dt_reg[id]
    end_day <- rv$data$end_day[id]
    ong <- is.na(dt_end)
    grade <- as.factor(grade)
    desc <- factor(desc, levels = unique(desc))
    col_grade <- cols[as.numeric(td$grade)]
    num <- ave(seq_along(desc), desc, FUN = seq_along)
  })
  
  if (legend[[1]] != FALSE)
    do.call('legend', modifyList(list(
      x = legend, fill = tcol(cols, alpha = .5), horiz = FALSE, cex = .8,
      legend = levels(td$grade), bty = 'n'), val = as.list(legend)))
  
  
  ## convert dates to days, fix origin at first reg date (ie, ref date == 0)
  dts <- grep('^dt_', names(td))
  mm <- setNames(lapply(td[, dts], function(x)
    as.numeric(x) - unique(as.numeric(rv$data[if (stagger) 1 else
      rv$data$id %in% id, 'dt_reg']))),
    gsub('dt_', 'dd_', names(td)[dts]))
  td <- cbind(td, do.call('cbind', mm))
  sp <- split(td, if (split) seq.int(nrow(td)) else td$desc)
  
  ## max end bar date
  dd_end2 <- rv$data[rv$data$id %in% id, c('dd_prog')]
  dd_end2 <- min(dd_end2, na.rm = !all(is.na(dd_end2)))
  
  for (ii in nn) {
    with(sp[[ii]], {
      ## rect for indiv td
      col <- tcol(col_grade, alpha = .5)
      endx <- pmax(dd_end %|% dd_end2, dd_start, na.rm = TRUE)
      do_rect_(ii + 1, dd_start, endx, col = col, border = col)
      ## td with end but no start date
      # if (no_start)
      if (FALSE)
        segments(endx, 1 + ii + c(.15, -.15),
                 endx - .15, 1 + ii + c(.15, -.15),
                 col = col, lwd = 2)
      
      ## add count of td to left in black if start date, color/italic if NA
      ## desc on right in color, italics if continuing; black otherwise
      text(dd_start[1], ii + 1, labels = ii, pos = 2, xpd = NA, cex = .8,
           col = if (no_start[1]) col else 1, font = if (no_start[1]) 3 else 1)
      # text(tail(dd_end %|% end_day, 1), ii + 1, labels = tail(desc, 1),
      
      text(max(dd_start, dd_end %|% dd_end2, na.rm = TRUE), ii + 1,
           labels = tail(desc, 1),
           pos = 4, xpd = NA, cex = .8, font = c(1,3)[tail(ong, 1) + 1L],
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
    return(invisible())
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
    dd <- setNames(data[, 1:10],
                   c('id','dt_reg','dt_txstart','dt_txend','dt_prog','dt_offtx',
                     'dt_status','status','dt_lastcontact','dt_offstudy'))
    to_date(dd)
  }, error = function(e) do_error(fmt1, 'data'))
  
  if (missing(bar_data))
    return(invisible(dd))
  
  dts <- grep('^dt_', names(bar_data))
  bd <- tryCatch({
    if (one <- length(dts) == 1L) {
      bd <- setNames(bar_data[, 1:3], c('id','dt_assess_start','assess'))
      bd <- within(bd, {
        id <- factor(id, levels = unique(dd$id))
        dt_assess_end <- ave(dt_assess_start, id, FUN = function(x)
          c(tail(x, -1), NA))
      })[!is.na(bd$dt_assess_start), ]
      to_date(bd)
    } else {
      bd <- setNames(bar_data[, 1:5],
                     c('id','dt_start','dt_end','grade','desc'))
      bd <- within(bd, {
        id <- factor(id, levels = unique(dd$id))
      })
      to_date(bd)
    }
  }, error = function(e)
    do_error(if (one) fmt2 else fmt3, paste0('bar_data', c('','2')[one + 1L])))
  
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
        rep(NA, length(dose.exp) + 1)
      }
  } else c(NA, col.exp)
  
  n <- sum(nlevel)
  N <- n + if (!is.null(dose.exp)) length(dose.exp) else 0
  y <- c(dose, dose.exp)
  x <- seq.int(N) + c(0, cumsum(diff(y) * 0.1))
  col <- c(col.dose, col.exp)
  
  x <- x + cumsum(ave(y, y, FUN = seq_along) %in%
                    (cumsum(rep(nstep, length(y))) + 1)) * 0.1
  pls_idx <- which(!diff(y) > 0 & diff(x) > 1 | y[-length(y)] == 0L)
  pls_idx <- pls_idx[pls_idx <= n + 1]
  x <- rawr::rescaler(x, c(1, max(x) - squish)) - 1 / squish
  pls <- sapply(Map(`:`, pls_idx, pls_idx + 1L),
                function(ii) mean(x[ii]))
  arr_idx <- which(diff(y) > 0) + 1L
  arr <- roll_fun(x, 2, mean)[arr_idx[arr_idx <= n]]
  
  plot.new()
  plot.window(xl <- xlim %||% c(0, max(x)),
              yl <- ylim %||% c(0, max(y) + 1))
  p <- par('usr')
  arrows2(c(0,0), c(0,0), c(0, max(xl)), c(max(yl), 0), lwd = 3,
          size = 0.5, width = 0.5)
  axis(2, seq_along(ylab), ylab, las = 1, lwd = 0, mgp = c(0,0,0))
  text(p[2], 0, pos = 1, xlab, xpd = NA, font = 2)
  text(0, p[4], pos = 2, ylab, xpd = NA, font = 2)
  
  points(x, y, pch = 16, col = col, cex = 3.5, xpd = NA)
  points(pls, y[pls_idx + 1L], pch = '+', cex = 1.5, xpd = NA)
  arrows2(arr, seq_along(arr) + 0.2, arr, seq_along(arr) + 0.8,
          size = 0.5, width = 0.5, lwd = 4, curve = 1.2, sadj = c(0,0,0,-.1))
  
  if (!is.null(dose.exp))
    if (diff(y[wh <- which(y == 0L) + c(-1, 1)]) < 0) {
      carrows(c(x[wh[1]], y[wh[1]]), c(x[wh[1]] * 1.01, y[wh[2]]), col = 2,
              size = .5, arc = pi / 3 * c(-1, 1), lwd = 4, width = 0.7,
              dir = c(1,0), pad = c(.1, .3))
    } else arrows2(mean(x[wh[1:2]]), max(dose.exp), mean(x[wh[1:2]]),
                   size = 0.7, width = 1, lwd = 4, col = 3, curve = 1.3)
  
  invisible(list(x, y, col, arr, pls))
}
