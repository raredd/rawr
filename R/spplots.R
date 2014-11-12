### special plots
# jmplot, tplot, dsplot, color.bar, prettybars, prettybars2
###

#' Joint/marginal plot
#' 
#' Joint distribution and marginal distributions plot; requires 
#' \code{\link{tplot}}
#' 
#' @usage
#' jmplot(x, y, z, 
#'        
#'        # labels
#'        main, sub, xlab, ylab, names, 
#'        
#'        # axes stuff
#'        xlim, ylim, axes = TRUE, frame.plot = axes, 
#'        log = '', xratio = .8, yratio = xratio, 
#'        
#'        # more options
#'        show.n = TRUE, show.na = TRUE, cex.n, 
#'        ann = par('ann'), asp = NA, 
#'        panel.first = NULL, panel.last = NULL,
#'        
#'        ...)
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
#' @param log character, "x", "y", or both for logarithmic scale; sets negative
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
#' @references \url{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}
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
                   
                   # labels
                   main, sub, xlab, ylab, names, 
                   
                   # axes stuff
                   xlim, ylim, axes = TRUE, frame.plot = axes, 
                   log = '', xratio = .8, yratio = xratio, 
                   
                   # more options
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
  
  # defaults
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
  # set the layout
  layout(matrix(c(1, 3, 0, 2), 2), 
         widths = c(xratio, 1 - xratio), 
         heights = c(1 - yratio, yratio))
  par(mar = c(0, 0, 0, 0), 
      oma = c(0,0, mar[3], mar[4]) + op$oma)
  
  # calculate xlim, ylim
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
  
  # plot x distribution on top
  par(mar = c(0, mar[2], 0, 0))
  localTplot(x ~ z, ylim = xlim, horizontal = TRUE, 
             show.n = FALSE, show.na = FALSE, ...)
  if (axes) 
    localAxis(side = 2, at = 1:nlevels(z), labels = names, ...)
  
  # plot y distribution on right
  par(mar = c(mar[1], 0, 0, 0))
  localTplot(y ~ z, ylim = ylim, show.n = show.n, show.na = show.na, 
             cex.n = cex.n, ...)
  if (axes) 
    localAxis(side = 1, at = 1:nlevels(z), labels = names, ...)
  
  # plot xy points
  par(mar = c(mar[1], mar[2], 0, 0))
  plot.new()
  localWindow(xlim, ylim, log, asp, ...)
  panel.first
  localPlot(xy, xlim = xlim, ylim = ylim, ...)
  panel.last
  
  # add axes
  if (axes) {
    localAxis(side = 1, ...)
    localAxis(side = 2, ...)
  }
  if (frame.plot) 
    localBox(...)
  # add titles
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
#' @usage
#' tplot(x, ...)
#' 
#' ## S3 method for class 'formula':
#' tplot(formula, data = parent.frame(), ..., subset, na.action = NULL)
#' 
#' ## Default S3 method:
#' tplot(x, ..., type = c('d','db','bd','b'),
#'       
#'       # labels
#'       main, sub, xlab, ylab, names,
#'       
#'       # axes stuff
#'       xlim, ylim, axes = TRUE, frame.plot = axes, at, horizontal = FALSE,
#'       
#'       # aesthetics
#'       jit = 0.1, dist, boxplot.pars, col, group.col = TRUE,
#'       boxcol, boxborder, pch = par('pch'), group.pch = TRUE,
#'       median.line = FALSE, mean.line = FALSE, 
#'       median.pars = list(col = par('col')), mean.pars = median.pars, 
#'       show.n = TRUE, show.na = TRUE, cex.n = NULL, my.gray = gray(0.75),
#'       
#'       # more options
#'       ann = par('ann'), add = FALSE,
#'       panel.first = NULL, panel.last = NULL
#' )
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
#' @references \url{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}
#' 
#' @examples
#' tplot(mpg ~ gear, mtcars)
#' 
#' set.seed(1618)
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

#' @export
tplot.default <- function(x, ..., 
                          type = c('d','db','bd','b'),
                          jit = 0.1, 
                          dist,
                          main,
                          sub,
                          xlab,
                          ylab,
                          names,
                          xlim,
                          ylim,
                          col,
                          group.col = TRUE,
                          boxcol,
                          boxborder ,
                          pch = par('pch'), 
                          group.pch = TRUE,
                          median.line = FALSE, 
                          mean.line = FALSE, 
                          median.pars = list(col = par('col')), 
                          mean.pars = median.pars, 
                          boxplot.pars,
                          show.n = TRUE,
                          show.na = TRUE,
                          cex.n,
                          my.gray = gray(0.75),
                          ann = par('ann'),
                          axes = TRUE,
                          frame.plot = axes,
                          add = FALSE,
                          at,
                          horizontal = FALSE,
                          panel.first = NULL,
                          panel.last = NULL) {
  
  op <- par(no.readonly = TRUE)
  
  # helpers
  localPoints <- function(..., tick) points(...)
  localAxis <- function(..., bg, cex, lty, lwd) axis(...)
  localBox <- function(..., bg, cex, lty, lwd, tick) box(...)
  localWindow <- function(..., bg, cex, lty, lwd, tick) plot.window(...)
  localTitle <- function(..., bg, cex, lty, lwd, tick) title(...)
  localMtext <- function(..., bg, cex, lty, lwd, tick) mtext(..., cex = cex.n)
  
  args <- list(x, ...)
  namedargs <- if (!is.null(attributes(args)$names))
    attributes(args)$names !=  ''
  else logical(length(args))
  groups <- if (is.list(x)) x
  else args[!namedargs]
  pars <- args[namedargs]
  if ((n <- length(groups)) == 0)
    stop('invalid first argument')
  if (length(class(groups)))
    groups <- unclass(groups)
  if (!missing(names))
    attr(groups, 'names') <- names
  else {
    if (is.null(attr(groups, 'names')))
      attr(groups, 'names') <- 1:n
    names <- attr(groups, 'names')
  }
  
  ng <- length(groups) # number of groups
  l <- sapply(groups, length) # size of each group
  g <- factor(rep(1:ng, l), levels = 1:ng, labels = names(groups))
  nv <- sum(l) # total count
  
  if (missing(at)) at <- 1:ng
  if (length(at) !=  ng)
    stop("'at' must have same length as the number of groups")
  
  # set y scale
  if (missing(ylim)) {
    r <- range(groups, na.rm = TRUE, finite = TRUE)
    pm <- diff(r) / 20
    ylim <- r + pm * c(-1,1)
  }
  # set x scale
  if (missing(xlim)) {
    if (missing(at)) xlim <- c(0.5, ng+0.5)
    else xlim <- c(0.5, max(at)+0.5)
  }
  
  if (missing(xlab)) xlab <- ''
  if (missing(ylab)) ylab <- ''
  if (missing(main)) main <- ''
  if (missing(sub)) sub <- ''
  if (missing(boxcol)) boxcol <- 'grey97'
  
  type <- match.arg(type, choices = c('d','db','bd','b'), several.ok = TRUE)
  # type of plot for each group
  if ((length(type) > 1) && (length(type) !=  ng))
    warning("length of 'type' does not match the number of groups")
  type <- rep(type, length.out = ng)
  #type[l > 1000] <- 'b'
  
  # Handle default colors
  defcols <- c(my.gray, par('col'))
  # use 50% gray for box in back, otherwise default color
  if (missing(boxborder))
    boxborder <- defcols[2 - grepl('.b', type)]
  # use 50% gray for dots in back, otherwise default color
  if (missing(col)) {
    col <- defcols[2-grepl('.d', type)]
    group.col <- TRUE
  }
  if (missing(boxplot.pars))
    boxplot.pars <- NULL
  
  #if (length(boxborder) !=  ng)
  #    warning('length of 'boxborder' does not match the number of groups')
  boxborder <- rep(boxborder, length.out = ng)
  
  # if (!is.null(boxcol) && length(boxcol) !=  ng)
  #   warning('length of 'boxcol' does not match the number of groups')
  boxcol <- rep(boxcol, length.out = ng)
  
  # Use colors by group
  if (group.col) {
    if (length(col) !=  ng)
      warning("length of 'col' does not match the number of groups")
    g.col <- rep(col, length.out = ng)
    col <- rep(g.col, l)
    # Use colors by individual or global
  } else {
    if((length(col) > 1) && (length(col) !=  nv))
      warning("length of 'col' does not match the number of data points")
    col <- rep(col, length.out = nv)
    g.col <- rep(1, length.out = ng)
  }
  
  # Use plot characters by group
  if (group.pch) {
    if (length(pch) !=  ng)
      warning("length of 'pch' does not match the number of groups")
    pch <- rep(rep(pch, length.out = ng), l)
    # Use plot characters by individual or global
  } else {
    if((length(pch) > 1) && (length(pch) !=  nv))
      warning("length of 'pch' does not match the number of data points")
    pch <- rep(pch, length.out = nv)
  }
  
  # split colors and plot characters into groups
  col <- split(col, g)
  pch <- split(pch, g)
  # remove any NAs from the data and options
  nonas <- lapply(groups, function(x) !is.na(x))
  l2 <- sapply(groups, function(x) sum(is.na(x)))
  groups <- mapply('[', groups, nonas, SIMPLIFY = FALSE)
  l <- sapply(groups, length)
  col <- mapply('[', col, nonas, SIMPLIFY = FALSE)
  pch <- mapply('[', pch, nonas, SIMPLIFY = FALSE)
  
  # whether or not to display a mean and median line for each group
  mean.line <- rep(mean.line, length.out = ng)
  median.line <- rep(median.line, length.out = ng)
  
  # set defaults for dist and jit
  if (missing(dist) || is.na(dist) || is.null(dist)) 
    dist <- diff(range(ylim)) / 100
  if (missing(jit) || is.na(jit) || is.null(jit)) 
    jit <- 0.025 * ng

  how.many.so.far <- function(g) {
    out <- NULL
    u <- unique(g)
    for (i in 1:length(u)) {
      j <- g == u[i]
      out[which(j)] <- 1:sum(j)
    }
    out
  }
  # turns the values in each group into their plotting points
  grouping <- function(v, dif) {
    vs <- sort(v)
    together <- c(FALSE, diff(vs) <=  dif)
    g.id <- cumsum(!together)
    g.si <- rep(x <- as.vector(table(g.id)), x)
    vg <- cbind(vs, g.id, g.si)[rank(v), ]
    if (length(v) == 1) 
      vg <- as.data.frame(t(vg))
    hmsf <- how.many.so.far(vg[ , 2])
    data.frame(vg, hmsf)
  }
  groups <- lapply(groups, grouping, dif = dist)
  
  # set up new plot unless adding to existing one
  if (!add) {
    plot.new()
    if (horizontal)
      do.call('localWindow', c(list(ylim, xlim), pars))
    else
      do.call('localWindow', c(list(xlim, ylim), pars))
  }
  panel.first
  
  # function to compute the jittering
  jit.f2 <- function(g.si, hm.sf) hm.sf - (g.si + 1) / 2
  
  out <- list()
  
  Lme <- 0.2 * c(-1, 1)
  for (i in 1:ng) {
    to.plot <- groups[[i]]
    gs <- to.plot$g.si
    hms <- to.plot$hmsf
    x <- rep(at[i], nrow(to.plot)) + jit.f2(gs, hms) * jit
    y <- to.plot$vs
    
    if (type[i] == 'bd') { # dots behind
      boxplotout <- do.call('boxplot', 
                            c(list(x = y, at = at[i], plot = FALSE, add = FALSE,
                                   axes = FALSE, col = boxcol[i], 
                                   border = boxborder[i], outline = FALSE, 
                                   horizontal = horizontal), boxplot.pars))
      notoplot <- (y <=  boxplotout$stats[5,]) & (y >=  boxplotout$stats[1,])
      if (sum(notoplot) > 0)
        col[[i]][notoplot] <- '#BFBFBF'
      if (horizontal)
        do.call('localPoints', c(list(x = y, y = x, pch = pch[[i]], 
                                      col = col[[i]]), pars))
      else
        do.call('localPoints', c(list(x = x, y = y, pch = pch[[i]], 
                                      col = col[[i]]), pars))
    }
    if (type[i] %in% c('bd', 'b')) { # boxplot in front
      boxplotout <- do.call('boxplot', 
                            c(list(x = y, at = at[i], add = TRUE, 
                                   axes = FALSE, col = boxcol[i], 
                                   border = boxborder[i], outline = FALSE, 
                                   horizontal = horizontal), boxplot.pars))
      toplot <- (y > boxplotout$stats[5,]) | (y < boxplotout$stats[1,])
      if (sum(toplot) > 0) 
        if (col[[i]][toplot][1] == '#BFBFBF') 
          col[[i]][toplot] <- 1
      if (horizontal)
        do.call('localPoints', 
                c(list(x = y[toplot], y = x[toplot], pch = pch[[i]][toplot], 
                       col = col[[i]][toplot]), pars))
      else
        do.call('localPoints', 
                c(list(x = x[toplot], y = y[toplot], pch = pch[[i]][toplot], 
                       col = col[[i]][toplot]), pars))
    }
    if (type[i] == 'db') # boxplot behind
      do.call('boxplot', 
              c(list(x = y, at = at[i], add = TRUE, axes = FALSE, 
                     col = boxcol[i], border = boxborder[i], outline = FALSE, 
                     horizontal = horizontal), boxplot.pars))
    if (type[i] %in% c('db', 'd')) { # dots in front
      if (horizontal)
        do.call('localPoints', c(list(x = y, y = x, pch = pch[[i]], 
                                      col = col[[i]]), pars))
      else
        do.call('localPoints', c(list(x = x, y = y, pch = pch[[i]], 
                                      col = col[[i]]), pars))
    }
    if (mean.line[i]) { # mean line
      if (horizontal)
        do.call('lines', c(list(rep(mean(y), 2), at[i]+Lme), mean.pars))
      else
        do.call('lines', c(list(at[i]+Lme, rep(mean(y), 2)), mean.pars))
    }
    if (median.line[i]) { # median line
      if (horizontal)
        do.call('lines', c(list(rep(median(y), 2), at[i]+Lme), median.pars))
      else
        do.call('lines', c(list(at[i]+Lme, rep(median(y), 2)), median.pars))
    }
    
    out[[i]] <- data.frame(to.plot, col = col[[i]], pch = pch[[i]])
  }
  panel.last
  
  # add axes
  if (axes) {
    do.call('localAxis', c(list(side = 1 + horizontal, 
                                at = at, labels = names), pars))
    do.call('localAxis', c(list(side = 2 - horizontal), pars))
  }
  # optional sample sizes
  if (show.n) {
    if (missing(cex.n)) cex.n <- 1
    do.call('localMtext', 
            if (show.na) 
              c(list(paste0('n = ', l, '\nmissing = ', l2), 
                     side = 3 + horizontal, at = at), 
                pars, list(xaxt = 's', yaxt = 's'))
            else c(list(paste0('n = ', l), 
                        side = 3 + horizontal, at = at), 
                   pars, list(xaxt = 's', yaxt = 's')))
  }
  # add bounding box
  if (frame.plot)
    do.call('localBox', pars)
  # add titles
  if (ann) {
    if (horizontal)
      do.call('localTitle', c(list(main = main, sub = sub, 
                                   xlab = ylab, ylab = xlab), pars))
    else
      do.call('localTitle', c(list(main = main, sub = sub, 
                                   xlab = xlab, ylab = ylab), pars))
  }
  
  names(out) <- names(groups)
  invisible(out)
}

#' @export
tplot.formula <- function(formula, data = parent.frame(), ..., subset,
                          na.action = NULL) {
  
  if (missing(formula) || (length(formula) !=  3))
    stop("'formula' missing or incorrect")
  
  m <- match.call(expand.dots  =  FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  
  args <- lapply(m$..., eval, data, parent.frame())
  nmargs <- names(args)
  if ('main' %in% nmargs) args[['main']] <- enquote(args[['main']])
  if ('sub' %in% nmargs) args[['sub']] <- enquote(args[['sub']])
  if ('xlab' %in% nmargs) args[['xlab']] <- enquote(args[['xlab']])
  if ('ylab' %in% nmargs) args[['ylab']] <- enquote(args[['ylab']])
  
  m$... <- NULL
  m$na.action <- na.pass
  subset.expr <- m$subset
  m$subset <- NULL
  
  m[[1]] <- as.name('model.frame')
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, 'terms'), 'response')
  
  ## special handling of col and pch
  n <- nrow(mf)
  # pick out these options
  group.col <- if ('group.col' %in% names(args)) args$group.col else FALSE
  group.pch <- if ('group.pch' %in% names(args)) args$group.pch else FALSE
  # reorder if necessary
  if ('col' %in% names(args) && !group.col)
    args$col <- unlist(split(rep(args$col, length.out = n), mf[-response]))
  if ('pch' %in% names(args) && !group.pch)
    args$pch <- unlist(split(rep(args$pch, length.out = n), mf[-response]))
  
  if (!missing(subset)) {
    s <- eval(subset.expr, data, parent.frame())
    dosub <- function(x) { if (length(x) == n) x[s] else x }
    args <- lapply(args, dosub)
    mf <- mf[s,]
  }
  do.call('tplot', c(list(split(mf[[response]], mf[-response])), args))
}

#' Discrete scatter plot
#' 
#' This creates a scatter plot (sort of) for discrete, bivariate data; an
#' alternative to sunflower plots for integer-valued variables.
#' 
#' @usage
#' dsplot(x, y, ...)
#' 
#' ## S3 method for class 'formula':
#' dsplot(formula, data = parent.frame(), ..., subset, na.action = NULL)
#' 
#' ## Default S3 method:
#' dsplot(x, y, bkgr = TRUE, col = 1, pch = 19, cex = 0.8, ...)
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
#' @param x x-axis variable
#' @param y y-axis variable
#' @param bkgr logical; fill boxes with gray scale based on density
#' @param col plotting color
#' @param pch \strong{p}lotting \strong{ch}aracter
#' @param cex numerical value giving the amount by which plotting text and 
#' symbols should be magnified relative to the default; this starts as 1 when 
#' a device is opened and is reset when the layout is changed, e.g., by setting
#' \code{mfrow}
#' @param ... for the \code{formula} method, named arguments to be passed to 
#' the default method
#' 
#' for the default method, unnamed arguments are additional data vectors
#' (unless x is a list when they are ignored), and named arguments are 
#' arguments and \code{\link{par}}s to be passed to \code{\link{plot}}
#' 
#' @return An \code{\link{invisible}} table corresponding to the plot cell 
#' counts.
#' 
#' @references \url{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}
#' 
#' @examples
#' set.seed(1618)
#' x <- round(rnorm(400, 100, 4))
#' y <- round(rnorm(400, 200, 4))
#' sex <- sample(c('Female', 'Male'), 400, replace = TRUE)
#' dsplot(y ~ x, pch = 19, col = 1 + (sex %in% 'Female'), cex = .6,
#'        xlab = 'measurement 1', ylab = 'measurement 2', bty = 'L')
#' legend('bottomright', pch = 19, col = 1:2, 
#'        legend = c('Male', 'Female'), cex = .8)
#'        
#' @export

dsplot <- function(x, y, ...) UseMethod('dsplot')

#' @export
dsplot.default <- function(x, y, 
                           bkgr = TRUE,
                           col = 1, 
                           pch = 19, 
                           cex = 0.8,
                           ...) {
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  #   if (any(x != round(x), na.rm = TRUE) | any(y != round(y), na.rm = TRUE))
  #     stop('x must be integer values', '\n')
  
  L <- length(x)
  cc <- complete.cases(x, y)
  
  if (length(pch) < L)
    pch <- rep(pch, length.out = L)
  if (length(col) < L)
    col <- rep(col, length.out = L)
  if (length(cex) < L)
    cex <- rep(cex, length.out = L)
  
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
  
  every.other.element.x <- function(n) {
    # to make 1,n,2,n,3,n, ..., n,n vector
    c(rbind(1:n, rep(n, n)))[-(2*n)]
  }
  every.other.element.y <- function(n)
    c(rbind(rep(n, n), 1:n))[-(2 * n)]
  
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
  
  dat <- data.frame(id=1:length(x), x, y)
  dat <- dat[order(dat$x, dat$y), ]
  within <- c(t(tab))
  within <- within[within > 0]
  idx <- NULL
  hm <- NULL
  for (i in within) {
    idx <- c(idx, 1:i) # index within category
    hm <- c(hm, rep(i, i))
  }
  dat$idx <- idx
  dat$ly <- (box.size - ceiling(sqrt(hm))) / (box.size + 1) / 2 # local offset
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

#' @export
dsplot.formula <- function(formula, data = parent.frame(),... , subset,
                           na.action = NULL) {
  if (missing(formula) || (length(formula) != 3))
    stop("'formula' missing or incorrect")
  
  enquote <- function(x) as.call(list(as.name('quote'), x))
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  
  args <- lapply(m$..., eval, data, parent.frame())
  nmargs <- names(args)
  if ('main' %in% nmargs) 
    args[['main']] <- enquote(args[['main']])
  if ('sub' %in% nmargs) 
    args[['sub']] <- enquote(args[['sub']])
  if ('xlab' %in% nmargs) 
    args[['xlab']] <- enquote(args[['xlab']])
  if ('ylab' %in% nmargs) 
    args[['ylab']] <- enquote(args[['ylab']])
  
  m$... <- NULL
  #   m$na.action <- na.pass
  subset.expr <- m$subset
  m$subset <- NULL
  require(stats, quietly = TRUE) || stop("package 'stats' is missing")
  m[[1]] <- as.name('model.frame')
  m <- as.call(c(as.list(m), list(na.action = NULL)))
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, 'terms'), 'response')
  
  if (!missing(subset)) {
    s <- eval(subset.expr, data, parent.frame())
    n <- nrow(mf)
    dosub <- function(x) if (length(x) == n) x[s] else x
    args <- lapply(args, dosub)
    mf <- mf[s,]
  }
  do.call(dsplot, c(list(mf[[response]], mf[[-response]]), args))
}

#' Color legend
#' 
#' Continuous color bar legend
#' 
#' @usage
#' color.bar(cols, x = NULL, y = x, labels = NULL,
#'           at.x = par('usr')[2], at.y = par('usr')[3], 
#'           cex.x = 1, cex.y = 1, ...)
#' 
#' @param cols vector of color names (or hexadecimal) from low to high
#' @param x numeric vector of data
#' @param y optional numeric vector of data; to color by a variable other than
#' \code{x}, \code{y} must be specified for \code{labels} coordinates to work 
#' properly; see examples
#' @param labels optional labels for color bar
#' @param at.x x-coordinate of lower-left corner of bar
#' @param at.y y-coordinate of lower-left corner of bar; note that the bar can 
#' extend ouside the plotting region, so use \code{cex.x} and \code{cex.y} to
#' scale the bar down (or up)
#' @param cex.x,cex.y scaling parameters
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @return An \code{\link{invisible}} vector of colors (hexadecimal format)
#' used for the bar. Note that these colors may be mapped to \code{x} so that 
#' only one \code{color.bar} needs to be called in the \code{col} argument for
#' both the coloring of points and the generation of the legend; see examples.
#' 
#' @examples
#' plot.new()
#' color.bar(c('black','red','white','blue'), at.x = 0, at.y = 0)
#' text(x = 0, y = seq(0, par('usr')[4], length = 6), pos = 4, xpd = TRUE,
#'      labels = pretty(seq(0, par('usr')[4]), n = 6), cex = .8, offset = .75)
#' 
#' \dontrun{
#' ## calling color.bar once in the col argument will color x accordingly
#' # and plot the legend simultaneously
#' 
#' ## compare:
#' png('/users/rawr/desktop/tmp.png')
#' par(mfrow = c(2,1))
#' plot(mtcars$mpg, pch = 19, cex = 2, 
#'      col = colorRampPalette(c('yellow','red'))(1000)[rescaler(mtcars$mpg, c(1, 1000))])
#' plot(mtcars$mpg, pch = 19, cex = 2, bty = 'l',
#'      col = color.bar(c('yellow','red'), mtcars$mpg, labels = mtcars$mpg))
#' dev.off()
#'
#' ## plot a color.bar legend by a variable other than x
#' # use y argument to match the variable in the original plot call
#' # and x as the variable to color and label
#' 
#' ## compare:
#' png('/users/rawr/desktop/tmp.png')
#' par(mfrow = c(2,1))
#' with(mtcars,
#'      plot(mpg, pch = 19, cex = 2, main = 'color by weight (red = heavier)',
#'      col = colorRampPalette(c('yellow','red'))(1000)[rescaler(wt, c(1, 1000))]))
#' with(mtcars,
#'      plot(mpg, pch = 19, cex = 2, bty = 'l', main = 'color by weight (red = heavier)',
#'      col = color.bar(c('yellow','red'), x = wt, y = mpg, labels = wt)))
#' dev.off()
#'}
#'
#' @export

color.bar <- function(cols, 
                      x = NULL, 
                      y = x,
                      labels = NULL,
                      at.x = par('usr')[2], 
                      at.y = par('usr')[3], 
                      cex.x = 1, 
                      cex.y = 1,
                      ...) {
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(list(...))
  
  par(mar = c(5, 4, 4, 4) + .1, xpd = TRUE)
  bx <- par('usr')
  nc <- 1000
  cols <- colorRampPalette(cols)(nc)
  
  bx.y <- c(bx[3], bx[4])
  sapply(0:nc, function(x) {
    segments(at.x, 
             at.y + x * diff(bx.y) / nc * cex.y, 
             at.x + diff(bx[1:2]) / nc * 20 * cex.x, 
             at.y + x * diff(bx.y) / nc * cex.y, 
             col = cols[x], lwd = 1, xpd = TRUE)
  })
  if (!is.null(labels))
    text(x = at.x, y = pretty(y), labels = pretty(labels), 
         pos = 4, cex = .8, offset = .75)
  if (!is.null(x))
    invisible(cols[rescaler(x, c(1, nc))])
}

#' prettybars
#' 
#' A barplot
#' 
#' @usage
#' prettybars(x, y = names(x), emph = NULL, 
#'            
#'            ## aesthetics
#'            col.bar = grey(.9), col.emph = 'magenta1', col.y = 'black', 
#'            col.bg = 'lightblue', cex.y = 0.5,
#'            
#'            ## summary line(s)
#'            FUN = mean, ..., fun.lab = 'overall mean', digits = 2, 
#'            
#'            ## labels and text
#'            title = paste0('prettybar of ', m$x), sub = NULL, note = NULL, 
#'            col.note = col.emph, subnote = 'source: github.com/raredd/rawr',
#'            
#'            ## etc
#'            extra.margin = 0, pars = NULL)
#' 
#' @param x numeric vector; values of bar heights
#' @param y rownames of bars (or extracted from a named vector)
#' @param emph vector of bars to emphasize (must be in \code{y})
#' @param col.bar color of bars
#' @param col.emph color of emphasis bars
#' @param col.y color of y labels
#' @param col.bg background color bars
#' @param cex.y size of y labels
#' @param FUN summary function to apply to \code{x}
#' @param ... additional arguments passed to \code{FUN}
#' @param fun.lab text label for \code{FUN}
#' @param digits numeric; digits after decimal after applying \code{FUN}
#' @param title main title
#' @param sub subtitle
#' @param note optional note (top right)
#' @param col.note color of note
#' @param subnote optional subnote (bottom right)
#' @param extra.margin extra left spacing for long \code{y} labels
#' @param pars additional list of graphical parameters passed to 
#' \code{\link{par}}
#' 
#' @seealso \code{\link{prettybars2}}
#' 
#' @examples
#' set.seed(1618)
#' myrandomdata <- setNames(round(runif(10), 2) * 100, LETTERS[1:10])
#' prettybars(myrandomdata, FUN = NULL, fun.lab = NULL, cex.y = 1.2, 
#'            bg = 'white', emph = 'F', digits = 2)
#' 
#' x <- setNames(mtcars$mpg, rownames(mtcars))
#' prettybars(x, emph = rownames(mtcars)[mtcars$hp < 100], extra.margin = 1,
#'            FUN = median, fun.lab = 'overall median mpg',
#'            title = 'motor trend cars', sub = '   (miles per gallon)',
#'            note = 'vehicles with less than 100 hp in bold')
#'
#' prettybars(mtcars$mpg, y = rownames(mtcars), col.bg = 'snow',
#'            emph = rownames(mtcars)[grepl('Merc', rownames(mtcars))],
#'            extra.margin = 1, col.emph = 'cyan2',
#'            FUN = quantile, probs = c(.25, .5, .75), na.rm = TRUE, 
#'            fun.lab = c('lower quartile','median','upper quartile'),
#'            note = "if you buy a Mercedes,\nget ready to pay for lots of gas",
#'            title = 'motor trend cars', sub = '   (miles per gallon)') 
#' @export

prettybars <- function(x, y = names(x), emph = NULL, 
                       
                       # aesthetics
                       col.bar = grey(.9), col.emph = 'magenta1', col.y = 'black', 
                       col.bg = 'lightblue', cex.y = 0.5,
                       
                       # summary line(s)
                       FUN = mean, ..., fun.lab = 'overall mean', digits = 2, 
                       
                       # labels and text
                       title = paste0('prettybar of ', m$x), sub = NULL, note = NULL, 
                       col.note = col.emph, subnote = 'source: github.com/raredd/rawr',
                       
                       # etc
                       extra.margin = 0, pars = NULL) {
  
  m <- match.call()
  
  ## par settings
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mar = c(3, 4 + extra.margin, 4, 2) + .1, las = 1, bg = 'snow')
  par(pars)
  
  ## error checks
  if (length(x) != length(y))
    stop('variable lengths not equal')
  if (!is.null(emph) && all(emph %in% rownames(y)))
    stop('error: some emph not found in y')
  
  x <- x[order(x)]
  y <- y[order(x)]
  
  ## background bars
  breaks <- pretty(x)
  s <- seq_along(breaks)
  p0 <- barplot(x, horiz = TRUE, axes = FALSE, col = NA, border = NA, 
                xlim = range(breaks), names.arg = FALSE)
  
  rect(breaks[1], 0, breaks[length(breaks)], sum(range(p0)), 
       col = tcol(col.bg, 50), border = NA)
  rect(breaks, 0, breaks + rep(diff(breaks)[1], length(breaks)), sum(range(p0)), 
       col = c(tcol(col.bg, 80), NA), border = NA)
  
  ## data bars
  p1 <- barplot(x, names.arg = FALSE, horiz = TRUE, border = NA, axes = FALSE,
                xlim = range(breaks), ylim = c(0, length(x)), col = col.bar, 
                cex.names = 0.85, add = TRUE, xpd = FALSE)
  
  ## emphasized bars
  x2 <- x * (y %in% emph)
  barplot(x2, names.arg = FALSE, horiz = TRUE, border = NA,
          xlim = range(breaks), col = tcol(col.emph, 200), 
          cex.names = 0.85, axes = FALSE, add = TRUE, xpd = FALSE)
  
  ## FUN line
  if (!is.null(FUN)) {
    at.x <- round(FUN(x, ...), digits)
    #     tcl <- length(p0) / (max(p0) - min(p0)) / 4
    tcl <- min(p0) / mean(p0)
    arrows(x0 = at.x, y0 = 0, x1 = at.x, y1 = sum(range(p0)),
           lwd = 1.5, length = 0, xpd = TRUE, col = col.y, lty = 3)
    arrows(x0 = at.x, y0 = 0, x1 = at.x, y1 = -tcl,
           lwd = 3, length = 0, xpd = TRUE)
    arrows(x0 = at.x, y0 = sum(range(p0)), x1 = at.x, y1 = sum(range(p0)) + tcl,
           lwd = 3, length = 0, xpd = TRUE)
    #     text(x = at.x, y = sum(range(p0)) * 1.25, labels = fun.lab, pos = 2, 
    #          xpd = TRUE, cex = 0.65, font = 3)
    #     text(x = at.x, y = sum(range(p0)) * 1.25, labels = at.x, pos = 4, 
    #          xpd = TRUE, cex = 0.65, font = 4)
    mtext(text = fun.lab, side = 3, line = 0, adj = 0, at = at.x, 
          cex = 0.65, font = 3)
    mtext(text = at.x, side = 3, line = -.5, adj = 0, at = at.x, 
          cex = 0.65, font = 4)
  }
  
  ## axes text
  mtext(text = breaks, at = breaks, side = 1, line = 0, cex = 0.80)
  font <- 1 + (y %in% emph) * rep(1, length(x))
  text(x = breaks[1], y = p1, labels = y, pos = 2, col = col.y,
       xpd = TRUE, adj = 0, cex = cex.y, font = font)
  text(x = breaks[1], y = p1, labels = x, col = col.y,
       xpd = TRUE, adj = 0, cex = cex.y, font = font)
  
  ## plot text (titles, notes)
  mtext(text = title, side = 3, line = 2, adj = 0, cex = 1.2, font = 2)
  mtext(text = sub, side = 3, line = 1, adj = 0, cex = 0.9)
  mtext(text = note, side = 3, line = -.5, adj = 1, cex = 0.65, font = 3, 
        col = col.note)
  #   text(x = breaks[length(breaks)], y = max(p0), labels = note, pos = 3,
  #        xpd = TRUE, cex = 0.65, font = 3)
  mtext(text = subnote, side = 1, line = 1, adj = 1, cex = 0.65, font = 3)
}

#' prettybars2
#' 
#' A barplot
#' 
#' @usage
#' prettybars2(x, lab.y = colnames(x), n.y = ncol(x), lab.group = rownames(x), 
#'             n.group = nrow(x), col.group = tcol(1:n.group, 100),
#'             col.line = 'skyblue3', extra.margin = 0, col.bg = 'snow',
#'             cex.y = 0.7, title = paste0('prettybar of ', m$x), 
#'             sub = paste0('     n = ', sum(x)),  note = NULL, 
#'             subnote = 'source: github.com/raredd/rawr', legend = 1, 
#'             notext = FALSE, ...) 
#' 
#' @param x a table of data, typically from \code{\link{table}} or 
#' \code{\link{xtabs}}
#' @param lab.y y-axis labels
#' @param n.y number of y-groups (calculated from \code{x})
#' @param lab.group group labels
#' @param n.group number of groups (calculated from \code{x})
#' @param col.group color for each group level
#' @param col.line color of origin line
#' @param extra.margin extra left spacing for long \code{lab.y} labels
#' @param col.bg background color
#' @param cex.y size of \code{lab.y}
#' @param title main title
#' @param sub subtitle
#' @param note optional note (top right)
#' @param subnote optional subnote (bottom right)
#' @param legend numeric (0, 1, 2); \code{0} for no legend; \code{1} default
#' legend; or \code{2} for a default \code{\link{legend}}
#' @param notext logical; if \code{TRUE}, suppresses all plot text, labels, and
#' axes for user input
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @seealso \code{\link{prettybars}}
#' 
#' @examples
#' set.seed(1618)
#' f <- function(...) sample(1:5, 100, replace = TRUE, prob = c(...))
#' dat <- data.frame(q1 = f(.1, .2, .3, .3, .1),
#'                   q2 = f(.1, .4, .1, .3, .1),
#'                   q3 = f(.1, .2, .3, .3, .1),
#'                   q4 = f(.1, .1, .3, .3, .1),
#'                   q5 = f(.2, .1, .2, .3, .2),
#'                   q6 = f(.1, .3, .3, .2, .1),
#'                   q7 = f(.1, .4, .1, .1, .3))
#' dat <- stack(dat)
#' dat <- within(dat, {
#'   values <- factor(values, levels = 1:5, labels = c('NA','SA','A','D','SD'))})
#' 
#' mydata <- table(dat)
#' cols <- c(grey(.9), tcol(c('lightblue','lightblue','magenta1','magenta1'), 
#'                c(200, 100, 100, 200)))
#'                
#' ## compare:
#' barplot(mydata, horiz = TRUE, las = 1, col = cols, border = NA)
#' prettybars2(mydata, lab.y = paste('Question #', 1:7), extra.margin = 3, 
#'             col.group = cols)
#'
#' @export

prettybars2 <- function(x,
                        lab.y = colnames(x), 
                        n.y = ncol(x),
                        lab.group = rownames(x), 
                        n.group = nrow(x),
                        col.group = tcol(1:n.group, 100),
                        col.line = 'skyblue3',
                        extra.margin = 0,
                        col.bg = 'snow',
                        cex.y = 0.7,
                        title = paste0('prettybar of ', m$x), 
                        sub = paste0('     n = ', sum(x)), 
                        note = NULL, 
                        subnote = 'source: github.com/raredd/rawr',
                        legend = 1,
                        notext = FALSE,
                        ...) {
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  m <- match.call()
  
  if (!is.table(x))
    stop('x must be a table')
  if (nrow(x) != n.group || nrow(x) != length(lab.group))
    stop('check group labels')
  if (ncol(x) != n.y || ncol(x) != length(lab.y))
    stop('check y labels')
  if (length(col.group) != n.group)
    warning('colors will be recycled (length(col.group) != n.group)')
  
  par(mar = c(6, 4 + extra.margin, 4, 2) + .1, bg = 'snow',
      #       lheight = 1.5,
      las = 1)
  par(list(...))
  
  ## data bars
  p0 <- barplot(-rep(100, n.y), names.arg = lab.y, cex.names = cex.y, 
                horiz = TRUE, border = col.bg, xlim = c(-100, n.y * 10), 
                col = col.group[1], axes = FALSE)
  barplot(-(100 - x[1, ]), axisnames = FALSE, horiz = TRUE, border = col.bg, 
          xlim = c(-100, n.y * 10), col = col.bg, axes = FALSE, add = TRUE)
  barplot(-x[3:2, ], axisnames = FALSE, horiz = TRUE, border = NA, 
          xlim = c(-100, n.y * 10), col = col.group[3:2], axes = FALSE, 
          add = TRUE)
  barplot(x[4:5, ], axisnames = FALSE, horiz = TRUE, border = NA, 
          xlim = c(-100, n.y * 10), col = col.group[4:5], axes = FALSE, 
          add = TRUE)
  
  ## legend, axes
  arrows(x0 = 0, y0 = -0.1, x1 = 0, y1 = sum(range(p0)),
         lwd = 2.5, length = 0, xpd = TRUE, col = col.line)
  if (legend == 2) {
    legend(x = -110, y = -0.1, horiz = TRUE, fill = col.group, legend = lab.group, 
           border = col.group, pt.cex = 3, bty = 'n', xpd = TRUE, cex = .8)
  } else if (legend == 1) {
    # if fine-tune
    px <- c(-95,-90, -59, -53, -37)
    tx <- c(-100, -79, -65, -45, -25)
    lab.group <- c('N/A','Strongly agree','Agree','Disagree','Strongly disagree')
    points(x = px, y = rep(-1, n.group), pch = 15, cex = 3, 
           col = col.group, xpd = TRUE)
    text(x = tx, y = rep(-1, n.group), labels = lab.group, xpd = TRUE, font = 3, 
         cex = .8)
  } else if (legend == 0) TRUE
  else warning('legend should be 0, 1, or 2')
  
  if (!notext) {
    ## x-lab
    mtext(c(80, 60, 40, 20, 0, 20, 40, 60), at = c(-80,-60,-40,-20,0,20,40,60), 1,
          line = 0, cex = 0.95)
    
    ## plot text (titles, notes)
    mtext(text = title, side = 3, line = 2, adj = 0, cex = 1.2, font = 2)
    mtext(text = sub, side = 3, line = 1, adj = 0, cex = 0.9, font = 3)
    mtext(text = note, side = 3, line = -.5, adj = 1, cex = 0.65, font = 3)
    #   text(x = breaks[length(breaks)], y = max(p0), labels = note, pos = 3,
    #        xpd = TRUE, cex = 0.65, font = 3)
    mtext(text = subnote, side = 1, line = 1, adj = 1, cex = 0.65, font = 3)
    #   mtext(paste0('N = ', sum(x)), side = 3, line = 0, at = -70, 
    #         cex = 0.8, font = 3)
  }
}
