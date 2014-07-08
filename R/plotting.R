### plot functions
# multiplot, ggmultiplot, click.text, click.shape, facet_adjust, 
# facet_adjust.print, ggcaterpillar, zoomin, ggheat, dodge
###


#' Draw multiple plots in a single layout
#' 
#' Function for plotting multiple base \code{R} graphics in a 
#' single layout
#' 
#' @usage multiplot(width = 8.5, height = 11, rows, cols, matrix = c(1, 1))
#' 
#' @param width,height the width and height of the plotting window in inches; 
#' if \code{NA}, taken from the resources and if not specified there, defaults 
#' to \code{7} inches; see \code{\link{x11}} and "Resources"
#' @param rows,cols number of rows and columns in \code{matrix}
#' @param matrix a matrix object specifying the location of the next \code{n} 
#' figures on the output device; each value in the matrix must be \code{0} or 
#' a positive integer;  if \code{n} is the largest positive integer in the 
#' matrix, then the integers \code{{1, ..., n-1}} must also appear at least 
#' once in the matrix; see \code{\link{layout}}
#' 
#' @examples
#' \dontrun{
#' library(descr)
#' 
#' multiplot(18, 8, 3, 3, c(1,1,1, 1,1,1, 2,3,4))
#' with(mtcars, histkdnc(mpg, breaks = 15, main = 'MPG'))
#' with(mtcars, histkdnc(cyl, breaks = 15, main = "Cyl"))
#' with(mtcars, histkdnc(hp, breaks = 15, main = 'HP'))
#' with(mtcars, histkdnc(drat, breaks = 15, main = 'Drat'))
#' }
#' @export

multiplot <- function(width = 8.5, height = 11, 
                      rows, cols, matrix = c(1, 1)) {
  x11(width, height)
  layout(matrix(c(matrix), rows, cols, byrow = TRUE))
}

#' Draw multiple ggplot objects in a single layout
#' 
#' Uses functions in \pkg{grid} to arrange one or more 
#' \code{\link[ggplot2]{ggplot}} objects into a single layout
#' 
#' @usage ggmultiplot(..., plotlist = NULL, cols = 1, layout = NULL)
#' 
#' @param ... ggplot objects
#' @param plotlist list of ggplot objects (optional)
#' @param cols number of columns in layout
#' @param layout matrix specifying the layout; if present, \code{cols} is 
#' ignored; if layout is \code{matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE)} 
#' for example, then plot 1 will go in the upper left, 2 will go in the upper 
#' right, and 3 will go all the way across the bottom.
#' 
#' @examples
#' data(mtcars)
#' library(ggplot2)
#' 
#' tmp <- ggplot(mtcars, aes(factor(1), fill = factor(cyl))) + 
#'   geom_bar(width = 1)
#' tmp1 <- tmp + coord_polar()
#' tmp2 <- tmp + coord_polar(theta = 'y')
#' tmp3 <- ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) + 
#'   geom_bar(width = 1) + coord_polar()
#' 
#' ggmultiplot(tmp1, tmp2, tmp3, cols = 2)
#' ggmultiplot(tmp1, tmp2, tmp3, 
#'   layout = matrix(c(1, 2, 3, 3, 3, 3), ncol = 2, byrow = TRUE))
#' @export

ggmultiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL) {
  
  require(grid)
  require(ggplot2)
  
  # make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # make the panel
    # ncol: number of columns of plots
    # nrow: number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # make each plot, in the correct location
    for (i in 1:numPlots) {
      # get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Add text interactively in base \code{R} graphics
#' 
#' Allows you to add text and expressions anywhere in a plot (including 
#' margins) with mouse click(s).
#' 
#' @usage 
#' click.text(express, col = 'black', cex = NULL, srt = 0, 
#'            trans = NULL, family = 'sans', dev = FALSE, ...)
#' 
#' @param express text or \code{\link{expression}}
#' @param col colour of text
#' @param cex numerical \strong{c}haracter \strong{ex}pansion factor; 
#' multiplied by \code{\link{par}} ("cex") yields the final character size; 
#' \code{NULL} and \code{NA} are equivalent to \code{1.0}
#' @param srt the string rotation in degrees; see \code{\link{par}}
#' @param trans color transparency; see \code{\link{tcol}}
#' @param family the name of a font or drawing text; see \code{\link{Hershey}}
#' @param dev logical; if \code{TRUE}, will plot on a platform-specific
#' graphics device; see details
#' @param ... further graphical parameters passed to \code{\link{text}} (or 
#' \code{\link{par}})
#'
#' @details
#' When \code{dev = TRUE}, a platform-specific graphics device is used to 
#' displace the plot. Defaults are \code{\link{quartz}}, for apple and 
#' \code{\link{x11}} for windows and unix platforms.
#' 
#' However, any device can be used by setting \code{dev = FALSE} and opening
#' a device before \code{click.text}. If \code{dev} is \code{FALSE}, the
#' plot will open in the current device; see \code{\link{.Device}}. Close the
#' device pane or use \code{dev.off()} to turn off the current device.
#' @seealso \code{\link{click.shape}}; \code{\link{plotmath}} for help with 
#' mathematical expressions
#' 
#' @examples
#' \dontrun{
#' plot.new()
#' click.text('hello', col = 'red', cex = .5)
#' click.text('goodbye', family = 'HersheyScript', cex = 3)
#' click.text(expression(sum(x ^ 2) == 5 ^ hat(x)), srt = 45)
#' }
#' @export

click.text <- function(express, col = 'black', cex = NULL, srt = 0, 
                       trans = NULL, family = 'sans', dev = FALSE, ...) {
  
  op <- par(no.readonly = TRUE) 
  on.exit(par(op))
  if (!is.null(trans))
    col <- tcol(col, trans)
  
  if (dev) {
    if (grepl('apple', sessionInfo()$platform))
      quartz()
    else 
      x11()
  } else par(xpd = NA)
  
  par(mar = rep(0, 4), xpd = NA)
  x <- locator(1)
  X <- format(x, digits = 3)
  text(x[1], x[2], express, 
       col = col, cex = cex, srt = srt, family = family, ...)
  noquote(paste(X[1], X[2], sep = ', '))
}

#' Add shapes interactively in base \code{R} graphics
#' 
#' Allows you to add shapes anywhere in a plot (including margins)
#' with mouse click(s)
#' 
#' @usage 
#' click.shape(shape = 'line', col = 'black', border = col, trans = NULL,
#'             corners = NULL, lty = par('lty'), lwd = par('lwd'), 
#'             density = NULL, length = 1, code = 2, dev = FALSE, ...)
#' @param shape type of shape: 'box', 'arrow', 'line', 'poly', 'circle', 'cyl'
#' @param col shape outline colour
#' @param border border colour for shape; defaults to value for \code{col}
#' @param trans color transparency; see \code{\link{tcol}}
#' @param corners number of corners to draw for a polygon
#' @param lty,lwd graphical parameters; see \code{\link{par}}
#' @param density the density of shading lines in lines per inch; the default
#' value of \code{NULL} means that no shading lines are drawm; a zero value of
#' \code{density} means no shading or filling, whereas negative values and 
#' \code{NA} suppress shading (and so allow color filling)
#' @param length length of the edges of the arrow head (in inches); see 
#' \code{\link{arrows}}
#' @param code integer code for 'arrows' determining \emph{kind} of arrows to 
#' be drawn
#' @param dev logical; if \code{TRUE}, will plot on a platform-specific
#' graphics device; see details
#' @param ... other parameters (future use)
#' 
#' @details
#' When \code{dev = TRUE}, a platform-specific graphics device is used to 
#' displace the plot. Defaults are \code{\link{quartz}}, for apple and 
#' \code{\link{x11}} for windows and unix platforms.
#' 
#' However, any device can be used by setting \code{dev = FALSE} and opening
#' a device before \code{click.shape}. If \code{dev} is \code{FALSE}, the
#' plot will open in the current device; see \code{\link{.Device}}. Close the
#' device pane or use \code{dev.off()} to turn off the current device.
#' @seealso \code{\link{click.text}}
#' 
#' @examples
#' \dontrun{
#' plot.new()
#' click.shape() # a line segment
#' click.shape('arrow', col = 'blue', code = 2, lwd = 2, length = .15)
#' click.shape('box', border = 'purple', col = 'pink', lwd = 2)
#' click.shape('box', col = NULL, border = 'purple', lwd = 2)
#' click.shape('line', col = 'orange', lty = 3, lwd = 3)
#' click.shape('poly', corners = 5, border = 'green', col = 'orange', 
#'   lty = 1, lwd = 3)
#' click.shape('poly', corners = 3, border = 'red', col = 'yellow', lty = 1, 
#'   lwd = 2)
#' click.shape('cyl', col = 'orange')
#' click.shape('circle', col = 'orange', border = 'black', lty = 3, lwd = 3)
#' }
#' @export

click.shape <- function(shape = 'line', col = 'black', border = col, trans = NULL,
                        corners = NULL, lty = par('lty'), lwd = par('lwd'), 
                        density = NULL, length = 1, code = 2, dev = FALSE, ...) {
  
  op <- par(no.readonly = TRUE) 
  on.exit(par(op))
  if (!is.null(trans))
    col <- tcol(col, trans)
  
  if (dev) {
    if (grepl('apple', sessionInfo()$platform))
      quartz()
    else 
      x11()
  } else par(xpd = NA)
  
  RECTANGLE <- function(...) {
    coords <- c(unlist(locator(1)), unlist(locator(1)))
    rect(coords[1], coords[2], coords[3], coords[4], col = col, 
         density = density, border = border, lty = lty, lwd = lwd, length = NULL)
  }
  ARROW <- function(...) {
    coords <- c(unlist(locator(1)), unlist(locator(1)))
    arrows(coords[1], coords[2], coords[3], coords[4], code = code, col = col, 
           border = NULL, lty = lty, lwd = lwd, length = length)
  }
  lineSEGMENT <- function(...){
    coords <- c(unlist(locator(1)), unlist(locator(1)))
    segments(coords[1], coords[2], coords[3], coords[4], col = col, 
             border = NULL, lty = lty, lwd = lwd, length = NULL)
  }
  POLYGON <- function(...) {
    locations <- locator(corners)
    polygon(locations, col = col, density = density,
            border = border, lty = lty, lwd = lwd, length = NULL)
  }
  CIRCLE <- function(...) {
    require(plotrix)
    coords <- c(unlist(locator(1)), unlist(locator(1)))
    rad <- sqrt(((coords[3] - coords[1]) ** 2) + ((coords[4] - coords[2]) ** 2))
    draw.circle(coords[1], coords[2], radius = rad, col = col,
                border = border, lty = lty, lwd = lwd)
  }
  CYLINDER <- function(...) {
    require(plotrix)
    coor <- unlist(locator(2))
    cylindrect(coor[1], coor[3], coor[2], coor[4], col = col, border = border)
  }
  
  suppressWarnings(
    switch(shape,
           box = RECTANGLE(col, border, lty, lwd, density),
           arrow = ARROW(col, border, lty, lwd, code, length),
           line = lineSEGMENT(col, border, lty, lwd),
           poly = POLYGON(col, border, lty, lwd, density, corners),
           circle = CIRCLE(col, border, lty, lwd),
           cyl = CYLINDER(col, border),
           stop('Invalid Argumets'))
  )
}

#' Facet labeling 
#' 
#' Adjusts labels on x-axes when using \code{\link[ggplot2]{facet_wrap}}
#'
#' @usage 
#' facet_adjust(x, pos = c('up', 'down'), newpage = is.null(vp), vp = NULL)
#'   
#' @param x \code{\link[ggplot2]{ggplot}} object
#' @param pos position of labels
#' @param newpage draw new (empty) page first; see 
#' \code{\link[ggplot2]{print.ggplot}}
#' @param vp viewport to draw plot in; see \code{\link[ggplot2]{print.ggplot}}
#' @return facet_adjust object that inherits \code{\link[gtable]{gtable}} class
#' 
#' @examples
#' library(ggplot2)
#' # missing some labels 
#' (tmp <- ggplot(diamonds[1:100, ], aes(carat, price, colour = clarity)) + 
#'   geom_point() + facet_wrap( ~ cut))
#' facet_adjust(tmp)
#' facet_adjust(tmp, pos = 'down')
#' @export

facet_adjust <- function(x, pos = c('up', 'down'), 
                         newpage = is.null(vp), vp = NULL) {
  
  require(grid)
  require(ggplot2)
  
  pos <- match.arg(pos)
  p <- ggplot_build(x)
  gtable <- ggplot_gtable(p)
#   dev.off() # this prevented plots from being rendered in knitr
  # finding dimensions
  dims <- apply(p$panel$layout[2:3], 2, max)
  nrow <- dims[1]
  ncol <- dims[2]
  # number of panels in plot
  panels <- sum(grepl('panel', names(gtable$grobs)))
  space <- ncol * nrow
  # missing panels
  n <- space - panels
  # check if modifications are needed
  if (panels != space) {
    # indices of panels to fix
    idx <- (space - ncol - n + 1):(space - ncol)
    # copying x-axis of the last existing panel to the chosen panels in the row above
    gtable$grobs[paste0('axis_b', idx)] <- list(gtable$grobs[[paste0('axis_b', panels)]])
    if (pos == 'down') {
      # if pos == 'down' shift labels down to same level as x-axis of last panel
      rows <- grep(paste0('axis_b\\-[', idx[1], '-', idx[n], ']'), 
                   gtable$layout$name)
      lastAxis <- grep(paste0('axis_b\\-', panels), gtable$layout$name)
      gtable$layout[rows, c('t','b')] <- gtable$layout[lastAxis, c('t')]
    }
  }
  class(gtable) <- c('facet_adjust','gtable','ggplot','gg')
  
  gtable
}

#' facet_adjust print method
#' 
#' @usage print.facet_adjust(x, newpage = is.null(vp), vp = NULL)
#' @param x object from \code{\link{facet_adjust}}
#' @param newpage draw new (empty) page first; see 
#' \code{\link[ggplot2]{print.ggplot}}
#' @param vp viewport to draw plot in; see \code{\link[ggplot2]{print.ggplot}}
#' 
#' @seealso \code{\link{facet_adjust}}

print.facet_adjust <- function(x, newpage = is.null(vp), vp = NULL) {
  
  require(grid)
  require(ggplot2)
  
  if (newpage)
    grid.newpage()
  if (is.null(vp)){
    grid.draw(x)
  } else {
    if (is.character(vp)) 
      seekViewport(vp)
    else pushViewport(vp)
    grid.draw(x)
    upViewport()
  } 
  invisible(x)
}

#' Caterpillar plot
#' 
#' Caterpillar plots for random effects models using ggplot
#' 
#' @usage ggcaterpillar(re, qq = TRUE, likeDotplot = TRUE)
#' 
#' @param re random effects from lmer object
#' @param qq if \code{TRUE}, returns normal q/q plot; else returns caterpillar 
#' dotplot
#' @param likeDotplot if \code{TRUE}, uses different scales for random effects,
#' i.e., \code{\link[ggplot2]{facet_wrap}}
#' @details Behaves like \code{\link[lattice]{qqmath}} and 
#' \code{\link[lattice]{dotplot}} from the lattice package; also handles models
#' with multiple correlated random effects
#' 
#' @examples
#' \donttest{
#' library(lme4)
#' fit <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' 
#' # this function
#' ggcaterpillar(ranef(fit, condVar = TRUE))
#' 
#' # for comparison (requires lattice package)
#' lattice::qqmath(ranef(fit, condVar = TRUE))
#' }
#' @export

ggcaterpillar <- function(re, qq  =  TRUE, likeDotplot  =  TRUE) {
  
  require(ggplot2)
  
  f <- function(x) {
    pv   <- attr(x, 'postVar')
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + 
      rep((0:(ncol(x) - 1)) * nrow(x), each = nrow(x))
    pDf  <- data.frame(y = unlist(x)[ord],
                       ci = 1.96*se[ord],
                       nQQ = rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID = factor(rep(rownames(x), ncol(x))[ord], 
                                   levels = rownames(x)[ord]),
                       ind = gl(ncol(x), nrow(x), labels = names(x)))
    
    if (qq) {  ## normal q/q plot
      p <- ggplot(pDf, aes(nQQ, y)) + 
        facet_wrap(~ ind, scales = 'free') +
        xlab('Standard normal quantiles') + 
        ylab('Random effect quantiles')
    } else {              # caterpillar dotplot
      p <- ggplot(pDf, aes(x = ID, y = y)) + coord_flip()
      if (likeDotplot) {  # imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap( ~ ind)
      } else {            # different scales for random effects
        p <- p + facet_grid(ind ~ ., scales = 'free_y')
      }
      p <- p + xlab('Levels') + ylab('Random effects')
    }
    
    p <- p + theme_bw() + 
      theme(legend.position = 'none') + 
      geom_hline(yintercept = 0) + 
      geom_errorbar(aes(ymin = y - ci, ymax = y + ci), 
                    width = 0, colour = 'black') + 
      geom_point(aes(size = 1.2), colour = 'blue') 
    
    return(p)
  }
  
  lapply(re, f)
}

#' Zoom for points in base \code{R} plot
#' 
#' Provides a summary statistic for sample points in a plot (in progress).
#' 
#' @usage zoomin(x, y, ...)
#' 
#' @param x x-coordinates
#' @param y y-coordinates
#' @param ... other options passed to \code{\link{identify}}
#' 
#' @examples
#' set.seed(1618)
#' x <- runif(10)
#' y <- rnorm(10, mean = 5)
#' 
#' par(mfrow = c(1, 2))
#' plot(x, y, xlab = 'mean', ylab = 'sd')
#' 
#' zoomin(x, y)
#' ## ESC to quit
#' @export

zoomin <- function(x, y, ...) {
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  ans <- identify(x, y, n = 1, plot = FALSE, ...)
  
  zoom <- function (x, y, xlim, ylim, xd, yd) {
    
    rxlim <- x + c(-1, 1) * (diff(range(xd)) / 20)
    rylim <- y + c(-1, 1) * (diff(range(yd)) / 20)
    
    par(mfrow = c(1, 2))
    plot(xd, yd, xlab = 'mean', ylab = 'sd')
    
    xext <- yext <- rxext <- ryext <- 0
    
    if (par('xaxs') == 'r') {
      xext <- diff(xlim) * 0.04
      rxext <- diff(rxlim) * 0.04
    }
    if (par('yaxs') == 'r') {
      yext <- diff(ylim) * 0.04
      ryext <- diff(rylim) * 0.04
    }
    
    rect(rxlim[1] - rxext, rylim[1] - ryext, rxlim[2] + rxext, rylim[2] + ryext)
    xylim <- par('usr')
    xypin <- par('pin')
    
    rxi0 <- xypin[1] * (xylim[2] - (rxlim[1] - rxext)) / diff(xylim[1:2])
    rxi1 <- xypin[1] * (xylim[2] - (rxlim[2] + rxext)) / diff(xylim[1:2])
    y01i <- xypin[2] * (xylim[4] - (rylim[2] + ryext)) / diff(xylim[3:4])
    y02i <- xypin[2] * ((rylim[1] - ryext) - xylim[3]) / diff(xylim[3:4])
    mu <- x
    
    curve(dnorm(x, mean = mu, sd = y), from = -4 * y + mu, to = 4 * y + mu, 
          xlab = paste('mean:', round(mu, 2), ', sd: ', round(y, 2)), ylab = '')
    
    xypin <- par('pin')
    par(xpd = NA)
    xylim <- par('usr')
    xymai <- par('mai')
    
    x0 <- xylim[1] - diff(xylim[1:2]) * (xymai[2] + xymai[4] + rxi0)/xypin[1]
    x1 <- xylim[1] - diff(xylim[1:2]) * (xymai[2] + xymai[4] + rxi1)/xypin[1]
    y01 <- xylim[4] - diff(xylim[3:4]) * y01i/xypin[2]
    y02 <- xylim[3] + diff(xylim[3:4]) * y02i/xypin[2]
    
    par(xpd = TRUE)
    
    xend <- xylim[1] - diff(xylim[1:2]) * xymai[2] / (2 * xypin[1])
    xprop0 <- (xylim[1] - xend) / (xylim[1] - x0)
    xprop1 <- (xylim[2] - xend) / (xylim[2] - x1)
    par(xpd = NA)
    segments(c(x0, x0, x1, x1), 
             c(y01, y02, y01, y02), 
             c(xend, xend, xend, xend), 
             c(xylim[4] - (xylim[4] - y01) * xprop0, 
               xylim[3] + (y02 - xylim[3]) * xprop0, 
               xylim[4] - (xylim[4] - y01) * xprop1, 
               xylim[3] + (y02 - xylim[3]) * xprop1))
    par(mfg = c(1, 1))
    
    plot(xd, yd, xlab = 'mean', ylab = 'sd')
  }
  
  if(length(ans)) {
    zoom(x[ans], y[ans], range(x), range(y), x, y)
    points(x[ans], y[ans], pch = 19)
    zoomin(x, y)
  }
}

#' ggHeatmap
#' 
#' Function to plot a heat map using \code{ggplot}
#' 
#' @usage 
#' ggheat(cors = NULL, data = NULL, limits = c(-1, 1),
#'        gradn = rev(heat.colors(10)),
#'        gradc = c('white', 'steelblue'))
#' 
#' @param cors matrix (or matrix-like object) of correlations
#' @param data data frame of data (in progress)
#' @param limits range of color scale in legend; see 
#' \code{\link[ggplot2]{discrete_scale}}
#' @param gradn vector of \code{n} colors to use, from low to high; see details
#' @param gradc vector of length two, low color and high color; see details
#' 
#' @details
#' \code{gradn} takes a vector of \code{n} colors: either a list of names, a 
#' list of hexadecimal values, an existing color palette (i.e., 
#' \code{\link{heat.colors}}, \code{\link{rainbow}}, etc); see also 
#' \code{\link{palette}}, \code{\link{colors}}, the \code{RColorBrewer} 
#' package, etc.
#' 
#' \code{gradc} takes a low color and a high color, respectively, and generates
#' a continuous scale between those colors; see 
#' \code{\link[ggplot2]{scale_fill_gradient}}
#' 
#' @examples
#' tmp <- rescaler(matrix(1:25, 5))
#' diag(tmp) <- 1
#' colnames(tmp) <- rownames(tmp) <- LETTERS[1:5]
#' ggheat(cors = tmp, limits = c(0, 1))
#' ggheat(cors = tmp, gradn = NULL, gradc = c('white','red'))
#' @export

ggheat <- function(cors = NULL, data = NULL, 
                   limits = c(-1, 1),
                   gradn = rev(heat.colors(10)),
                   gradc = c('white', 'steelblue')) {
  
  require(ggplot2)
  
  zzz <- as.data.frame(cors)
  zzz <- stack(zzz)
  zzz$ind1 <- rownames(cors)
  names(zzz) <- c('corr', 'x', 'y')
  zzz <- within(zzz, {
    x <- factor(x, levels = colnames(cors), ordered = TRUE)
    y <- factor(y, levels = rownames(cors), ordered = TRUE)
  })
  
  p <- ggplot(zzz, aes(x = x, y = rev(y), fill = corr)) + 
    geom_tile() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(labels = rev(unique(zzz$y)), expand = c(0, 0)) +
    coord_fixed() + 
    theme_bw() + 
    theme(legend.position = 'right',
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  if (!is.null(gradc)) {
    p <- p + scale_fill_gradient(limits = limits, 
                                 low = gradc[1], 
                                 high = gradc[2])
  }
  if (!is.null(gradn))
    p <- p + scale_fill_gradientn(limits = limits,
                                  colours = gradn)
  
  p
}

#' Point dodge
#' 
#' Dodge and center overlapping points (in progress; see details).
#' 
#' @usage dodge(formula, data = parent.frame(), z = .5, spread = FALSE)
#' 
#' @param formula an object of class \code{\link{formula}} (or an object which 
#' can be coerced to a formula) having the form \code{var1 ~ var2}; interactions
#' are not allowed and \code{> 1} variable per side are not allowed.
#' @param data optional matrix or data frame containing the variables in 
#' \code{formula}; by default, variables are taken from 
#' \code{environment(formula)}
#' @param z numeric scaling parameter for distance between points; \code{0} for
#' no point dodge and larger values for more spread; default is \code{0.5}
#' @param spread logical; if values are close but not identical and points still
#' overlap, if \code{TRUE}, decimal places will be ignored to creater larger 
#' neighborhoods from which the point offset will be calculated; see details
#' 
#' @details
#' Still in progress. First attempt uses \code{\link{floor}} to find points 
#' that are close, i.e., within 1 unit, and only spreads those within the range.
#' This is desirable if \code{x} values are close but not exact. For example, 
#' points \code{1.1} and \code{1.0} may overlap and will not be offset if 
#' \code{spread} is \code{FALSE}. If \code{TRUE}, both values will be treated 
#' as \code{1.0} and will be offset; see boxplot example below.
#' 
#' @seealso \code{\link{jitter}}
#' 
#' @examples
#' ## these are equivalent ways to call dodge:
#' dodge(mpg ~ cyl, mtcars)
#' with(mtcars, dodge(cyl ~ mpg))
#' dodge(mtcars[c('mpg', 'cyl')])
#' 
#' set.seed(1618)
#' dat <- data.frame(x = rpois(50, 1),
#'                   grp = 1:5)
#' op <- par(no.readonly = TRUE)
#' par(list(mfrow = c(2, 2),
#'          mar = c(3,1,2,2)))
#' with(dat, plot(grp, x, main = 'overlapping points'))
#' with(dat, plot(grp, x, cex = runif(100), main = 'still overlapping points'))
#' with(dat, plot(jitter(grp), x, main = 'adding random noise'))
#' with(dat, plot(grp + dodge(x ~ grp, dat), x, main = 'dodge points'))
#' par(op)
#' 
#' boxplot(mpg ~ gear, data = mtcars, xlab = 'gears', ylab = 'mpg')
#' with(dat <- mtcars[order(mtcars$gear, mtcars$mpg), ], 
#'      points(gear - 2 + dodge(mpg ~ gear, dat), mpg,
#'             pch = 19, col = rep(c('red','green','blue'), table(dat$gear))))
#' with(dat <- mtcars[order(mtcars$gear, mtcars$mpg), ],
#'      points(gear - 2 + dodge(mpg ~ gear, dat, spread = TRUE), mpg,
#'             col = 'black', pch = 4))
#' legend('topleft', pch = c(4, 19), 
#'        legend = c('TRUE','FALSE'), title = 'spread = ')
#' @export

dodge <- function(formula, data = parent.frame(), z = .5, spread = FALSE) {
  if (missing(formula) && missing(data)) 
    stop("must supply either 'formula' or 'data'")
  if (!missing(formula)) {
    formula <- as.formula(formula)
    if (!inherits(formula, 'formula')) 
      stop("'formula' missing or incorrect")
  }
  if (any(attr(terms(formula, data = data), "order") > 1)) 
    stop('interactions are not allowed')
  m <- match.call()
  if (is.matrix(eval(m$data, parent.frame()))) 
    m$data <- as.data.frame(data)
  m$z <- m$spread <- NULL
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame())
  if (length(formula) == 2L) {
    by <- mf
    y <- NULL
  } else {
    i <- attr(attr(mf, 'terms'), 'response')
    by <- mf[-i]
    y <- mf[i]
  }
  dat <- cbind(by, y)
  
  if (spread)
    dat[names(y)] <- floor(dat[names(y)])
  
  dat$offset <- ave(dat[[names(y)]], dat[ , 1:2], 
                    FUN = function(x) seq_along(x) * z / 10)
  dat$offset <- ave(dat$offset, dat[ , 1:2], FUN = function(x) x - mean(x))
  dat$offset
}