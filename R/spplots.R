### specific plots
# tplot, jmplot, dsplot
###

#' jmplot
#' 
#' Joint distribution and marginal distributions plot; requires 
#' \code{\link{tplot}}
#' 
#' @usage
#' jmplot(x, y, z, 
#'        
#'        # labels
#'        main = NULL, sub = NULL, xlab = NULL, ylab = NULL, names = NULL, 
#'        
#'        # axes stuff
#'        xlim = NULL, ylim = NULL, axes = TRUE, frame.plot = axes, 
#'        log = '', xratio = .8, yratio = xratio, 
#'        
#'        # more options
#'        show.n = FALSE, cex.n = NULL, ann = par('ann'), asp = NA, 
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
#' @param cex.n size of \code{show.n} text
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
#'      group.col = TRUE, pch = 1:4, group.pch = TRUE, boxcol = grey(.9)))
#'        
#' @export

jmplot <- function(x, y, z, 
                   log = '', 
                   main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
                   names = NULL, 
                   xlim = NULL, ylim = NULL, axes = TRUE, 
                   xratio = .8, yratio = xratio, 
                   show.n = FALSE, cex.n = NULL, 
                   ann = par('ann'), frame.plot = axes, 
                   panel.first = NULL, panel.last = NULL, asp = NA, 
                   ...
) {
  
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
  if (is.null(names)) 
    names <- levels(z)
  if (is.null(xlab)) 
    xlab <- xy$xlab
  if (is.null(ylab)) 
    ylab <- xy$ylab
  
  # save pars
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
  if (is.null(xlim)) 
    xlim <- lim(xy$x)
  if (is.null(ylim)) 
    ylim <- lim(xy$y)
  
  # plot x distribution on top
  par(mar = c(0, mar[2], 0, 0))
  localTplot(x ~ z, ylim = xlim, horizontal = TRUE, show.n = FALSE, ...)
  if (axes) 
    localAxis(side = 2, at = 1:nlevels(z), labels = names, ...)
  
  # plot y distribution on right
  par(mar = c(mar[1], 0, 0, 0))
  localTplot(y ~ z, ylim = ylim, show.n = show.n, cex.n = cex.n, ...)
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
  
  # reset par
  par(op)
}

#' tplot
#' 
#' An alternative to \code{\link{boxplot}}. The individual data can be shown 
#' (either in the foreground or background) with jittering if necessary.
#' 
#' @usage
#' tplot(x, ...)
#' 
#' ## S3 method for class 'formula':
#' tplot(formula, data = parent.frame(), ..., subset)
#' 
#' ## Default S3 method:
#' tplot(x, ..., type = c('d','db','bd','b'),
#'       
#'       # labels
#'       main = NULL, sub = NULL, xlab = NULL, ylab = NULL, names,
#'       
#'       # axes stuff
#'       xlim = NULL, ylim = NULL, axes = TRUE, frame.plot = axes, 
#'       at = NULL, horizontal = FALSE,
#'       
#'       # aesthetics
#'       jit = 0.1, dist = NULL, boxplot.pars = NULL,
#'       col = NULL, group.col = TRUE,
#'       boxcol = NULL, boxborder = NULL, 
#'       pch = par('pch'), group.pch = TRUE,
#'       median.line = FALSE, mean.line = FALSE, 
#'       median.pars = list(col = par('col')), mean.pars = median.pars, 
#'       show.n = FALSE, cex.n = NULL, my.gray = gray(0.75),
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
#' @param x-axis tick labels for groups
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
#' @param show.n show number per group
#' @param cex.n text size for \code{show.n}
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
#' set.seed(1618)
#' dat <- data.frame(age = rnorm(80, rep(c(26, 36), c(70, 10)), 4),
#'                   sex = sample(c('Female', 'Male'), 80, replace = TRUE),
#'                   group = paste0('Group ', 
#'                                  sample(1:4, 40, prob = c(2, 5, 4, 1), 
#'                                         replace = TRUE)))
#' 
#' tplot(age ~ group, data = dat, las = 1, cex = 1, cex.axis = 1, bty = 'L', 
#'       show.n = TRUE, dist = NULL, jit = .1, type = c('db', 'db', 'db', 'd'),
#'       group.pch = TRUE, pch = c(15, 17, 19, 8), 
#'       group.col = FALSE, names = LETTERS[1:4],
#'       col = c('darkred', 'darkblue')[c(sex)],
#'       boxcol = c('lightsteelblue1', 'lightyellow1', grey(.9)), 
#'       boxplot.pars = list(notch = TRUE, boxwex = .5))
#'
#' @export

tplot <- function(x, ...) UseMethod('tplot')

#' @export
tplot.default <- function(x, ..., 
                          type = c('d','db','bd','b'),
                          jit = 0.1, 
                          dist = NULL,
                          main = NULL, 
                          sub = NULL, 
                          xlab = NULL, 
                          ylab = NULL, 
                          names,
                          xlim = NULL, 
                          ylim = NULL, 
                          col = NULL, 
                          group.col = TRUE,
                          boxcol = NULL, 
                          boxborder = NULL, 
                          pch = par('pch'), 
                          group.pch = TRUE,
                          median.line = FALSE, 
                          mean.line = FALSE, 
                          median.pars = list(col = par('col')), 
                          mean.pars = median.pars, 
                          boxplot.pars = NULL, 
                          show.n = FALSE,
                          cex.n = NULL,
                          my.gray = gray(0.75),
                          ann = par('ann'),
                          axes = TRUE,
                          frame.plot = axes,
                          add = FALSE,
                          at = NULL,
                          horizontal = FALSE,
                          panel.first = NULL,
                          panel.last = NULL
) {
  
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
  groups <- if (is.list(x))
    x
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
  
  if (is.null(at)) at <- 1:ng
  if (length(at) !=  ng)
    stop("'at' must have same length as the number of groups")
  
  # set y scale
  if (is.null(ylim)) {
    r <- range(groups, na.rm = TRUE, finite = TRUE)
    pm <- diff(r) / 20
    ylim <- r + pm * c(-1,1)
  }
  # set x scale
  if (is.null(xlim)) {
    if (is.null(at)) xlim <- c(0.5, ng+0.5)
    else xlim <- c(0.5, max(at)+0.5)
  }
  
  if (is.null(xlab)) xlab <- ''
  if (is.null(ylab)) ylab <- ''
  if (is.null(main)) main <- ''
  if (is.null(sub)) sub <- ''
  
  type <- match.arg(type, choices = c('d','db','bd','b'), several.ok = TRUE)
  # type of plot for each group
  if ((length(type) > 1) && (length(type) !=  ng))
    warning("length of 'type' does not match the number of groups")
  type <- rep(type, length.out = ng)
  #type[l > 1000] <- 'b'
  
  # Handle default colors
  defcols <- c(my.gray, par('col'))
  # use 50% gray for box in back, otherwise default color
  if (is.null(boxborder))
    boxborder <- defcols[2 - grepl('.b', type)]
  # use 50% gray for dots in back, otherwise default color
  if (is.null(col)) {
    col <- defcols[2-grepl('.d', type)]
    group.col <- TRUE
  }
  
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
  groups <- mapply('[', groups, nonas, SIMPLIFY = FALSE)
  l <- sapply(groups, length)
  col <- mapply('[', col, nonas, SIMPLIFY = FALSE)
  pch <- mapply('[', pch, nonas, SIMPLIFY = FALSE)
  
  # whether or not to display a mean and median line for each group
  mean.line <- rep(mean.line, length.out = ng)
  median.line <- rep(median.line, length.out = ng)
  
  # set defaults for dist and jit
  if (is.null(dist) || is.na(dist)) dist <- diff(range(ylim)) / 100
  if (is.null(jit) || is.na(jit)) jit <- 0.025 * ng
  
  # 1 2 3 1 3 2 1 1 4 2
  # -------------------
  # 1 1 1 2 2 2 3 4 1 3
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
    hms <- to.plot$hm
    x <- rep(at[i], nrow(to.plot)) + jit.f2(gs, hms) * jit
    y <- to.plot$vs
    
    if (type[i] == 'bd') { # dots behind
      boxplotout <- do.call('boxplot', 
                            c(list(x = y, at = at[i], plot = FALSE, add = FALSE,
                                   axes = FALSE, col = boxcol[i], 
                                   border = boxborder[i], outline = FALSE, 
                                   horizontal = horizontal), boxplot.pars))
      notoplot <- (y <=  boxplotout$stats[5,]) & (y >=  boxplotout$stats[1,])
      if( sum(notoplot) > 0 ){ col[[i]][notoplot] <- '#BFBFBF' }
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
      if( sum(toplot) > 0 ){
        if( col[[i]][toplot][1] == '#BFBFBF' ) col[[i]][toplot] <- 1 }
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
  if (show.n){
    if(is.null(cex.n)) cex.n <- 1
    do.call('localMtext', c(list(paste('n = ', l, sep = ''), 
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
#' dsplot(formula, data = parent.frame(), ..., subset)
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
#' @return A table corresponding to the plot cell counts.
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

dsplot <- function(x, ...) UseMethod('dsplot')

#' @export
dsplot.default <- function(x, y, 
                           bkgr = TRUE,
                           col = 1, 
                           pch = 19, 
                           cex = 0.8, 
                           ...) {
  
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
  
  table(factor(y, levels = rev(min(y):max(y))), 
        factor(x, levels = min(x):max(x)))
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