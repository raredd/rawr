### plot functions
# jmplot, tplot, dsplot, waffle, river, river2
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
#' @param main overall title for the plot
#' @param sub sub-title for the plot (below x-axis)
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param names group labels
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param axes logical; draw axes
#' @param frame.plot logical; draw box around \code{x-y} plot
#' @param at group positions on axis
#' @param horizontal logical; flip axes
#' @param jit numeric; x-axis jitter parameter for overlapping pts (\code{0} for 
#' overlap and higher values for more distance between points)
#' @param dist numeric; additional jitter parameter
#' @param boxplot.pars additional list of graphical parameters for box plots
#' @param col plotting color
#' @param group.col logical; if \code{TRUE}, color by group; o/w by order
#' @param boxcol fill color
#' @param boxborder border color
#' @param pch plotting character
#' @param group.pch logical; if \code{TRUE}, \code{pch} by group; o/w, by order
#' @param median.line logical; draw median line
#' @param mean.line logical; draw mean line
#' @param median.pars list of graphical parameters for median line
#' @param mean.pars see above
#' @param show.n show number in each group
#' @param show.na show number missing in each group
#' @param cex.n text size for \code{show.n} and \code{show.na}
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
#' @seealso
#' \href{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}{Tatsuki
#' \code{tplot}}; \href{http://data.vanderbilt.edu/~graywh/dotplot/}{web app
#' for Tatsuki \code{tplot}}
#' 
#' @examples
#' ## equivalent ways to call tplot
#' ## the formula method is a convenience function for the first case
#' tplot(split(mtcars$mpg, mtcars$gear))
#' tplot(mpg ~ gear, mtcars)
#' 
#' ## use panel.first/panel.last like in `plot` (unavailable in `boxplot`)
#' tplot(mpg ~ gear, data = mtcars, col = 1:3, type = 'd',
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
  # if ((length(type) > 1) && (length(type) !=  ng))
  #   warning("length of \'type\' does not match the number of groups")
  type <- rep(type, length.out = ng)
  
  ## default colors
  defcols <- c(my.gray, par('col'))
  ## 50% gray for box in back, otherwise default color
  if (missing(boxborder))
    boxborder <- defcols[2 - grepl('.b', type)]
  ## 50% gray for dots in back, otherwise default color
  if (missing(col)) {
    col <- defcols[2 - grepl('.d', type)]
    group.col <- TRUE
  }
  if (missing(boxplot.pars))
    boxplot.pars <- NULL
  
  # if (length(boxborder) !=  ng)
  #   warning('length of \'boxborder\' does not match the number of groups')
  boxborder <- rep(boxborder, length.out = ng)
  
  # if (!is.null(boxcol) && length(boxcol) !=  ng)
  #   warning('length of 'boxcol' does not match the number of groups')
  boxcol <- rep(boxcol, length.out = ng)
  
  ## colors by group
  if (group.col) {
    # if (length(col) !=  ng)
    #   warning("length of \'col\' does not match the number of groups")
    g.col <- rep(col, length.out = ng)
    col <- rep(g.col, l)
    ## colors by individual or global
  } else {
    # if ((length(col) > 1) && (length(col) !=  nv))
    #   warning("length of \'col\' does not match the number of data points")
    col   <- rep(col, length.out = nv)
    g.col <- rep(1, length.out = ng)
  }
  ## plot characters by group
  if (group.pch) {
    # if (length(pch) !=  ng)
    #   warning("length of \'pch\' does not match the number of groups")
    pch <- rep(rep(pch, length.out = ng), l)
    ## plot characters by individual or global
  } else {
    # if ((length(pch) > 1) && (length(pch) !=  nv))
    #   warning("length of \'pch\' does not match the number of data points")
    pch <- rep(pch, length.out = nv)
  }
  
  ## split colors and plot characters into groups
  col <- split(col, g)
  pch <- split(pch, g)
  ## remove any NAs from the data and options
  nonas <- lapply(groups, function(x) !is.na(x))
  l2 <- sapply(groups, function(x) sum(is.na(x)))
  groups <- mapply('[', groups, nonas, SIMPLIFY = FALSE)
  l <- sapply(groups, length)
  col <- mapply('[', col, nonas, SIMPLIFY = FALSE)
  pch <- mapply('[', pch, nonas, SIMPLIFY = FALSE)
  
  ## mean and median line for each group
  mean.line   <- rep(mean.line, length.out = ng)
  median.line <- rep(median.line, length.out = ng)
  
  ## defaults for dist and jit for groups
  if (missing(dist) || is.na(dist) || is.null(dist)) 
    dist <- diff(range(ylim)) / 100
  if (missing(jit) || is.na(jit) || is.null(jit)) 
    jit <- 0.025 * ng
  groups <- lapply(groups, grouping_, dif = dist)
  ## rawr:::grouping_; rawr:::hmsf_; rawr:::jit_
  
  ## set up new plot unless adding to existing one
  if (!add) {
    plot.new()
    if (horizontal)
      do.call('localWindow', c(list(ylim, xlim), pars))
    else
      do.call('localWindow', c(list(xlim, ylim), pars))
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
    
    ## type of plot
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
      if (horizontal)
        do.call('localPoints', c(list(x = y, y = x, pch = pch[[i]], 
                                      col = col[[i]]), pars))
      else do.call('localPoints', c(list(x = x, y = y, pch = pch[[i]],
                                         col = col[[i]]), pars))
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
      if (horizontal)
        do.call('localPoints', 
                c(list(x = y[toplot], y = x[toplot], pch = pch[[i]][toplot], 
                       col = col[[i]][toplot]), pars))
      else do.call('localPoints', c(list(x = x[toplot], y = y[toplot],
                                         pch = pch[[i]][toplot],
                                         col = col[[i]][toplot]), pars))
    }
    ## box behind
    if (type[i] == 'db')
      do.call('boxplot', c(list(x = y, at = at[i], add = TRUE, axes = FALSE,
                                col = boxcol[i], border = boxborder[i],
                                outline = FALSE, horizontal = horizontal),
                           boxplot.pars))
    ## dots in front
    if (type[i] %in% c('db', 'd')) {
      if (horizontal)
        do.call('localPoints', c(list(x = y, y = x, pch = pch[[i]],
                                      col = col[[i]]), pars))
      else do.call('localPoints', c(list(x = x, y = y, pch = pch[[i]],
                                         col = col[[i]]), pars))
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
    out[[i]] <- data.frame(to.plot, col = col[[i]], pch = pch[[i]])
  }
  panel.last
  
  if (axes) {
    do.call('localAxis', c(list(side = 1 + horizontal, at = at,
                                labels = names), pars))
    do.call('localAxis', c(list(side = 2 - horizontal), pars))
  }
  
  ## frame and text
  ## optional sample sizes
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
  invisible(out)
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
  if ('sub' %in% nmargs)  args[['sub']] <- enquote(args[['sub']])
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
  
  ## special handling of col and pch -- pick out these options
  group.col <- if ('group.col' %in% names(args)) args$group.col else FALSE
  group.pch <- if ('group.pch' %in% names(args)) args$group.pch else FALSE
  ## reorder if necessary
  if ('col' %in% names(args) && !group.col)
    args$col <- unlist(split(rep(args$col, length.out = n), mf[-response]))
  if ('pch' %in% names(args) && !group.pch)
    args$pch <- unlist(split(rep(args$pch, length.out = n), mf[-response]))
  
  if (!missing(subset)) {
    s <- eval(subset.expr, data, parent.frame())
    args <- lapply(args, do_sub_, x, n, s)
    ## rawr:::do_sub_
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
#' \code{data} and \code{tox} data frames need to have a minimum number of
#' variables in a specific order, some of which should be \code{\link{Date}}
#' formats. \code{check_river_format()} without any arguments will give a
#' summary of the formats.
#' 
#' \code{data} should have at least 12 columns (any additional will be
#' ignored) in the following order: ID; dates of registration, last treatment
#' start and end, progression, and response; type of response; dates off
#' treatment, last contact, off study, and status; and status.
#' 
#' \code{tox} should have at least 5 columns (any additional will be
#' ignored) in the following order: ID, dates of toxicity start and end,
#' grade, and description.
#' 
#' Non \code{Date} columns can be character, factor, or numeric. The response
#' and grade variables should be ordinal and therefore factors with proper
#' level order since the legend and colors will use this ordering. The
#' description variable should have relatively short descriptions to avoid
#' text extending outside the plotting window.
#' 
#' @param data,tox data frames; these should have a specific format, see
#' details, examples, or run \code{check_river_format()}
#' @param id,at optional parameters specifying individuals (rows) from
#' \code{data} to plot and their positions along the y-axis; if not given,
#' timelines are plotted sequentially
#' @param legend character string giving position (see \code{\link{legend}}
#' or \code{FALSE}
#' @param xlim,ylim x- and y-axis limits
#' 
#' @examples
#' ## generate outcome data for river
#' dd <- data.frame(id = 1:5, dt_reg = 1:5, dt_txst = 2:6, dt_txend = c(10:13,8),
#'                  dt_prog = c(11,16,NA,NA,8), dt_resp = c(NA,NA,9,13,NA),
#'                  resp = factor(c('PD','PD','PR','CR','SD'),
#'                                levels = c('PD','SD','MR','PR','CR')),
#'                  dt_offtx = c(12,19,NA,NA,8), dt_last = c(12,19,24,25,NA),
#'                  dt_off = c(12,19,NA,NA,8),
#'                  dt_surv = c(12,19,24,25,NA), surv = c(0,1,0,0,NA))
#' 
#' ## dates should be date formats
#' dts <- grep('dt_', names(dd))
#' dd[, dts] <- lapply(dd[, dts], as.Date.numeric, origin = '1970-01-01')
#' 
#' (river(dd, xlim = c(0,25)))
#' 
#' 
#' ## generate tox data for river2
#' tt <- data.frame(id = rep(1:2, times = c(1, 5)), dt_start = c(3,5,5,8,9,11),
#'                  dt_end = c(NA,5,NA,10,10,NA), grade = c(1,4,3,4,1,2),
#'                  desc = paste('tox', c(1,1:5)))
#' 
#' ## dates should be date formats
#' dts <- grep('dt_', names(tt))
#' tt[, dts] <- lapply(tt[, dts], as.Date.numeric, origin = '1970-01-01')
#' 
#' river2(dd, 2, tt)
#' 
#' @export

river <- function(data, id, at = id, legend = 'topleft', xlim, ylim) {
  ## error checks
  data <- setNames(data[, 1:12],
                   c('id','dt_reg','dt_txstart','dt_txend','dt_prog',
                     'dt_resp','resp','dt_offtx','dt_lastcontact',
                     'dt_offstudy','dt_status','status'))
  stopifnot(check_river_format(data))
  
  nn <- seq.int(nrow(data))
  # data <- data[rev(nn), ]
  
  if (missing(id)) {
    id <- nn
  } else stopifnot(all(id %in% nn & at %in% nn))
  
  ## colors for resp - SD:CR
  cols <- c('red','transparent','yellow','orange','blue','green4')
  
  ## convert dates to days with origin at first id reg (ie, ref date == 0)
  dts <- grep('^dt_', names(data))
  mm <- setNames(data[, dts], gsub('dt_', 'dd_', names(data)[dts]))
  rx <- range(unlist(mm[id, , drop = FALSE]), na.rm = TRUE)
  
  mm[] <- lapply(mm, as.numeric)
  mm <- as.matrix(mm) - min(mm, na.rm = TRUE)
  
  data <- within(cbind(data, mm), {
    end_day <- apply(mm, 1, max, na.rm = TRUE)
    end_day <- pmax(end_day, dd_reg)
    alive <- is.na(status) | (grepl('(?i)[0v]', status) + 0L)
    censor <- alive & !is.na(dt_offstudy)
    resp <- factor(resp)
    col_resp <- cols[as.numeric(resp)]
  })
  
  plot.new()
  par(mar = c(4,0,1,0))
  plot.window(if (!missing(xlim)) xlim else c(0, diff(rx)),
              if (!missing(ylim)) ylim else c(0, nrow(data)))
  axis(1, tcl = .2, las = 1)
  title(xlab = 'Days from study entry', line = 2.5)
  
  if (legend != FALSE)
    legend(legend, fill = cols, legend = levels(data$resp),
           horiz = FALSE, cex = .8, bty = 'n')
  
  for (ii in id) {
    ## lines at specific points requires new index
    jj <- at[which(id %in% ii)]
    
    with(data[ii, ], {
      ## label ids in black to left of rect
      text(dd_reg, jj, labels = id, pos = 2, xpd = NA)
      
      ## lines - time alive, on tx
      do_seg_(jj, dd_reg, end_day, arrow = alive & !censor, col = 1)
      do_seg_(jj, dd_txstart, dd_txend %|% end_day, arrow = FALSE, lty = 1,
             lwd = 4, col = 'green4')
      
      ## rects - resp
      do_rect_(jj, dd_resp, dd_prog %|% end_day,
              col = adjustcolor(col_resp, alpha.f = .5))
      
      ## points - prog (red circle), death, (red x), censor (blue x)
      points(dd_prog, jj, pch = 16, col = 2, cex = 1.5)
      points(end_day, jj, pch = c(4, NA)[alive + 1L],
             col = 2, lwd = 3, cex = 1.5)
      points(end_day, jj, pch = c(NA, 4)[(alive & censor) + 1L],
             col = 4, lwd = 3, cex = 1.5)
    })
  }
  invisible(list(data = data))
}

#' @rdname river
#' @export
river2 <- function(data, id = 1, tox, legend = 'topleft', xlim, ylim) {
  ## error checks
  data <- setNames(data[, 1:12],
                   c('id','dt_reg','dt_txstart','dt_txend','dt_prog',
                     'dt_resp','resp','dt_offstudy','dt_offtx',
                     'dt_lastcontact','dt_status','status'))
  tox <- setNames(tox[, 1:5], c('id','dt_start','dt_end','grade','desc'))
  stopifnot(check_river_format(data, tox))
  
  ## select the tox for id, remove NA rows, order by date
  tox <- split(tox, tox$id, drop = FALSE)[[id]]
  tox <- tox[!is.na(tox$dt_start), ]
  tox <- tox[order(tox$dt_start), ]
  nn <- seq.int(nrow(tox))
  
  ## grade colors - 1:5
  cols <- c('blue','green','orange','red','black')
  
  ## base plot of the id summary
  rv <- river(data, id = id, at = 1, legend = FALSE, xlim = xlim,
              ylim = if (missing(ylim)) c(0, nrow(tox) + 1) else ylim)
  
  tox <- within(tox, {
    dt_reg <- rv$data$dt_reg[id]
    end_day <- rv$data$end_day[id]
    ong <- is.na(dt_end)
    grade <- factor(grade)
    col_grade <- cols[as.numeric(tox$grade)]
    toxnum <- nn
  })
  
  ## convert dates to days, fix origin at first reg date (ie, ref date == 0)
  dts <- grep('^dt_', names(tox))
  mm <- setNames(lapply(tox[, dts], function(x)
    as.numeric(x) - min(as.numeric(rv$data$dt_reg), na.rm = TRUE)),
    gsub('dt_', 'dd_', names(tox)[dts]))
  tox <- cbind(tox, do.call('cbind', mm))
  
  if (legend != FALSE)
    legend(legend, fill = cols, legend = levels(tox$grade),
           horiz = FALSE, cex = .8, bty = 'n')
  
  for (ii in nn) {
    with(tox[ii, ], {
      ## rect for indiv tox
      col <- adjustcolor(col_grade, alpha.f = .5)
      do_rect_(ii + 1, dd_start, dd_end %|% end_day, col = col, border = col)
      
      ## add count of tox to left in black
      ## desc on right in color, italics if continuing; black otherwise
      text(dd_start, ii + 1, labels = toxnum, pos = 2, xpd = NA, cex = .8)
      text(dd_end %|% end_day, ii + 1, labels = desc, pos = 4, xpd = NA,
           cex = .8, col = c('black', col)[ong + 1L], font = c(1,3)[ong + 1L])
    })
  }
  invisible(list(data = rv$data, tox = tox))
}

#' @rdname river
#' @export
check_river_format <- function(data, tox) {
  fmt1 <- c("'data.frame':\tn obs. of  12 variables:", " $ id            : chr ",
            " $ dt_reg        :Class 'Date'", " $ dt_txstart    :Class 'Date'",
            " $ dt_txend      :Class 'Date'", " $ dt_prog       :Class 'Date'",
            " $ dt_resp       :Class 'Date'", " $ resp          : chr ",
            " $ dt_offtx      :Class 'Date'", " $ dt_lastcontact:Class 'Date'",
            " $ dt_offstudy   :Class 'Date'"," $ dt_status     :Class 'Date'",
            " $ status        : chr ")
  fmt2 <- c("'data.frame':\tn obs. of  5 variables:", " $ id           : chr ", 
            " $ dt_dtstarttox:Class 'Date'", " $ dt_dtendtox  :Class 'Date'", 
            " $ tox_grade    : chr ", " $ tox_desc     : chr ")
  
  if (missing(data) && missing(tox)) {
    message('\'data\' should have the following format:\n', domain = NA)
    cat(fmt1, sep = '\n')
    message('\n\n\'tox\' should have the following format:\n', domain = NA)
    cat(fmt2, sep = '\n')
    return(invisible())
  }
  
  do_error <- function(fmt, wh) {
    message(sprintf('%s should have the following format:\n', shQuote(wh)))
    cat(fmt, sep = '\n')
    stop('Format error: see of ?rawr::river for more information',
         domain = NA, call. = FALSE)
  }
  
  dts <- grep('^dt_', names(data))
  ok <- all(sapply(data[, dts], class) == 'Date')
  if (!ok || !length(dts))
    do_error(fmt1, 'data')
  if (!missing(tox)) {
    dts <- grep('^dt_', names(tox))
    ok <- all(sapply(tox[, dts], class) == 'Date')
    if (!ok || !length(dts))
      do_error(fmt2, 'tox')
  }
  TRUE
}
