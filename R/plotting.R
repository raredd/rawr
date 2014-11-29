### plot functions
# multiplot, ggmultiplot, click.text, click.shape, facet_adjust, 
# facet_adjust.print, ggcaterpillar, ggheat, dodge, jmplot, tplot, dsplot,
# bpCI
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
  ## no visible binding note
  nQQ <- y <- ID <- ci <- NULL
  
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
  ## no visible binding note
  corr <- NULL
  
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

#' Barplot confidence intervals
#' 
#' Add confidence intervals (error bars) and group comparisons to barplots.
#' 
#' @usage
#' bpCI(bp, horiz = FALSE, ci = TRUE, ci.u, ci.l, ci.width = .5,
#'      sig = FALSE, pvals, ch = '*', ...)
#' 
#' @param bp the return value of \code{\link{barplot}}, i.e., a vector or
#' matrix (when \code{beside = TRUE}) of all bar (or group) midpoints
#' @param horiz logical; if \code{TRUE}, \code{bpCI} assumes horizontal bars
#' @param ci logical; draw error bars (must give \code{ci.u}, \code{ci.l})
#' @param ci.u,ci.l a numeric vector or matrix having the same dimensions as
#' \code{bp} giving the upper and lower intervals, respectively
#' @param ci.width width of the ends of the error bars, will depend on 
#' \code{range(bp)}
#' @param sig logical; if \code{TRUE}, draws group comparisons (must give
#' \code{pvals} to plot sig stars)
#' @param pvals p-values of group comparisons to be displayed as sig stars
#' @param ch plotting character to be used for significance; default is 
#' \code{*} and uses same significance codes as \code{\link{printCoefmat}}
#' @param ... additional parameters passed to \code{\link{par}}
#' 
#' @examples
#' ## generate data and p-values
#' hh <- t(VADeaths)[1:2, 5:1]
#' ci.l <- hh * 0.85
#' ci.u <- hh * 1.15
#' pvals <- pt(apply(hh, 2, diff), 1) / 5:1
#' 
#' bp <- barplot(hh, beside = TRUE, ylim = c(0,100))
#' bpCI(bp, ci.u = ci.u, ci.l = ci.l, sig = TRUE, pvals = pvals)
#' mtext("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
#'       side = 1, at = par('usr')[2], line = 2, adj = 1, cex = .8, font = 3)
#' 
#' 
#' bp <- barplot(hh <- cbind(x = c(465, 91) / 465 * 100,
#'                           y = c(200, 840) / 840 * 100,
#'                           z = c(37, 17) / 37 * 100),
#'               beside = TRUE, width = c(465, 840, 37),
#'               col = c(1, 2), ylim = c(0,130))
#' 
#' ci.l <- hh * 0.85
#' ci.u <- hh * 1.15
#' pvals <- pt(-abs(apply(hh, 2, diff)), 1)
#' 
#' bpCI(bp, ci.u = ci.u, ci.l = ci.l, sig = TRUE, pvals = pvals, ci.width = 100,
#'      col = 'red', lty = 'dashed', lwd = 2)
#' mtext("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1",
#'       side = 1, at = par('usr')[2], line = 2, adj = 1, cex = .8, font = 3)
#' 
#' @export

bpCI <- function(bp, horiz = FALSE, ci = TRUE, ci.u, ci.l, ci.width = .5,
                     sig = FALSE, pvals, ch = '*', ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(list(...))
  if (ci) {
    ci.width <- ci.width / 2
    if (horiz) {
      ci.l <- t(ci.l)
      ci.u <- t(ci.u)
      segments(ci.l, t(bp), ci.u, t(bp))
      segments(ci.u, t(bp - ci.width), ci.u, t(bp + ci.width))
      segments(ci.l, t(bp - ci.width), ci.l, t(bp + ci.width))
    } else {
      segments(bp, ci.l, bp, ci.u)
      segments(bp - ci.width, ci.u, bp + ci.width, ci.u)
      segments(bp - ci.width, ci.l, bp + ci.width, ci.l)
    }
    if (sig) {
      pstar <- function(pv, ch = '*') {
        symnum(pv, corr = FALSE, na = FALSE, 
               cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
               symbols = gsub('\\*', ch, c("***", "**", "*", ".", "NS")))
      }
      if (horiz)
        stop('sig is not supported when horiz = TRUE')
      if (nrow(bp) > 2)
        stop('sig is not supported for > 2 bars per group')
      yy <- rbind(c(ci.u[1, ] + 3), c(apply(ci.u, 2 , max) + 5),
                  c(apply(ci.u, 2, max) + 5), c(ci.u[2, ] + 3))
      xx <- apply(bp, 2, function(x) rep(x, each = nrow(bp)))
      sapply(1:ncol(bp), function(x) lines(xx[, x], yy[, x]))
      xt <- colMeans(bp)
      yt <- apply(ci.u, 2, max) + 7
      text(pstar(pvals, ch = '*'), x = xt, y = yt)
    }
  }
}
