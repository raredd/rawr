### plot functions
# ggmultiplot, click_text, click_shape, facet_adjust, print.facet_adjust,
# ggcaterpillar, ggheat, ggheat2, dodge, jmplot, tplot, dsplot, bpCI, inset,
# show_colors, show_pch, tcol, ggcols, grcols, ctext, cmtext, ctitle, waffle
# river, river2
###


#' Draw multiple ggplot objects in a single layout
#' 
#' Uses functions in \pkg{grid} to arrange one or more 
#' \code{\link[ggplot2]{ggplot}} objects into a single layout
#' 
#' @param ... ggplot objects
#' @param plotlist list of ggplot objects (optional)
#' @param cols number of columns in layout
#' @param layout matrix specifying the layout; if present, \code{cols} is 
#' ignored; 
#' 
#' if layout is \code{matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE)},
#' for example, then plot 1 will go in the upper left, 2 will go in the upper 
#' right, and 3 will go all the way across the bottom.
#' 
#' @examples
#' data(mtcars)
#' library('ggplot2')
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
#'   
#' @export

ggmultiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL) {
  
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
#' Add text and expressions anywhere in a plot (including margins) with mouse
#' click(s).
#' 
#' @param expr a character string of text or an \code{\link{expression}}
#' @param ... additional graphical parameters passed to \code{\link{text}} (or 
#' \code{\link{par}}) such as \code{col}, \code{srt}, \code{family}, etc
#'
#' @seealso
#' \code{\link{click_shape}}; \code{\link{plotmath}} for help with plotting
#' mathematical expressions; \code{link{tcol}}
#' 
#' @return
#' A vector of length two with the x- and y-coordinates of the text position.
#' 
#' @examples
#' \dontrun{
#' plot.new()
#' click_text('hello', col = 'red', cex = .5)
#' click_text('goodbye', family = 'HersheyScript', cex = 3)
#' click_text(expression(sum(x ^ 2) == 5 ^ hat(x)), srt = 45)
#' }
#' 
#' @export

click_text <- function(expr, ...) {
  op <- par(no.readonly = TRUE) 
  on.exit(par(op))
  par(mar = c(0,0,0,0), xpd = NA)
  co <- locator(1)
  text(co[[1]], co[[2]], labels = if (missing(expr)) '' else expr, ...)
  c(x = co[[1]], y = co[[2]])
}

#' Add shapes interactively in base \code{R} graphics
#' 
#' Add shapes anywhere in a plot (including margins) with mouse click(s).
#' 
#' @param shape type of shape; choices are \code{'box'}, \code{'arrow'},
#' \code{'line'}, \code{'poly'}, \code{'circle'}, and \code{'cyl'}
#' @param col shape outline color
#' @param border border colour for shape; defaults to value for \code{col}
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
#' @param ... ignored
#' 
#' @seealso \code{\link{click_text}}, \code{link{tcol}}, \code{\link{rect}},
#' \code{\link{arrows}}, \code{\link{rect}}, \code{\link{rect}},
#' \code{\link{segments}}, \code{\link{polygon}},
#' \code{\link[plotrix]{draw.circle}}, \code{\link[plotrix]{cylindrect}}
#' 
#' @examples
#' \dontrun{
#' plot.new()
#' click_shape() # a line segment
#' click_shape('arrow', col = 'blue', code = 2, lwd = 2, length = .15)
#' click_shape('box', border = 'purple', col = 'pink', lwd = 2)
#' click_shape('box', col = NULL, border = 'purple', lwd = 2)
#' click_shape('line', col = 'orange', lty = 3, lwd = 3)
#' click_shape('poly', corners = 5, border = 'green', col = 'orange')
#' click_shape('poly', corners = 3, border = 'red', col = 'yellow', lty = 1)
#' click_shape('cyl', col = 'orange')
#' click_shape('circle', col = 'orange', border = 'black', lty = 3, lwd = 3)
#' }
#' 
#' @export

click_shape <- function(shape = 'line', col = 'black', border = col,
                        corners = 5, lty = par('lty'), lwd = par('lwd'), 
                        density = NULL, length = 1, code = 2, ...) {
  
  op <- par(no.readonly = TRUE) 
  on.exit(par(op))
  par(xpd = NA)
  
  RECT <- function(...) {
    co <- c(unlist(locator(1)), unlist(locator(1)))
    rect(co[1], co[2], co[3], co[4], col = col, 
         density = density, border = border, lty = lty, lwd = lwd,
         length = NULL)
  }
  ARRO <- function(...) {
    co <- c(unlist(locator(1)), unlist(locator(1)))
    arrows(co[1], co[2], co[3], co[4], code = code,
           col = col, border = NULL, lty = lty, lwd = lwd, length = length)
  }
  LINE <- function(...){
    co <- c(unlist(locator(1)), unlist(locator(1)))
    segments(co[1], co[2], co[3], co[4], col = col, 
             border = NULL, lty = lty, lwd = lwd, length = NULL)
  }
  POLY <- function(...) {
    locations <- locator(corners)
    polygon(locations, col = col, density = density,
            border = border, lty = lty, lwd = lwd, length = NULL)
  }
  CIRC <- function(...) {
    co <- c(unlist(locator(1)), unlist(locator(1)))
    rad <- sqrt(((co[3] - co[1]) ** 2) + ((co[4] - co[2]) ** 2))
    draw.circle(co[1], co[2], radius = rad, col = col,
                border = border, lty = lty, lwd = lwd)
  }
  CYLI <- function(...) {
    coor <- unlist(locator(2))
    cylindrect(coor[1], coor[3], coor[2], coor[4], col = col, border = border)
  }
  
  suppressWarnings(
    switch(shape,
           box    = RECT(col, border, lty, lwd, density),
           arrow  = ARRO(col, border, lty, lwd, code, length),
           line   = LINE(col, border, lty, lwd),
           poly   = POLY(col, border, lty, lwd, density, corners),
           circle = CIRC(col, border, lty, lwd),
           cyl    = CYLI(col, border),
           stop('Invalid Argumets'))
  )
}

#' Facet labeling 
#' 
#' Adjusts labels on x-axes when using \code{\link[ggplot2]{facet_wrap}}.
#'   
#' @param x \code{\link[ggplot2]{ggplot}} object
#' @param pos position of labels
#' @param newpage draw new (empty) page first; see 
#' \code{\link[ggplot2]{print.ggplot}}
#' @param vp viewport to draw plot in; see \code{\link[ggplot2]{print.ggplot}}
#' 
#' @return
#' \code{facet_adjust} object that inherits \code{\link[gtable]{gtable}} class
#' 
#' @examples
#' library('ggplot2')
#' ## missing some labels 
#' (tmp <- ggplot(diamonds[1:100, ], aes(carat, price, colour = clarity)) + 
#'   geom_point() + facet_wrap( ~ cut))
#' facet_adjust(tmp)
#' facet_adjust(tmp, pos = 'down')
#' 
#' @export

facet_adjust <- function(x, pos = c('up', 'down'), 
                         newpage = is.null(vp), vp = NULL) {
  
  pos <- match.arg(pos)
  p <- ggplot_build(x)
  gtable <- ggplot_gtable(p)
  ## dev.off() ## this prevented plots from being rendered in knitr
  ## finding dimensions
  dims <- apply(p$panel$layout[2:3], 2, max)
  nrow <- dims[1]
  ncol <- dims[2]
  ## number of panels in plot
  panels <- sum(grepl('panel', names(gtable$grobs)))
  space <- ncol * nrow
  ## missing panels
  n <- space - panels
  ## check if modifications are needed
  if (panels != space) {
    ## indices of panels to fix
    idx <- (space - ncol - n + 1):(space - ncol)
    ## copy x-axis of last existing panel to the chosen panels in row above
    gtable$grobs[paste0('axis_b', idx)] <- list(
      gtable$grobs[[paste0('axis_b', panels)]])
    if (pos == 'down') {
      ## shift labels down to same level as x-axis of last panel
      rows <- grep(sprintf('axis_b\\-[%s-%s]', idx[1], idx[n]),
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
#' @param x object from \code{\link{facet_adjust}}
#' @param newpage draw new (empty) page first; see 
#' \code{\link[ggplot2]{print.ggplot}}
#' @param vp viewport to draw plot in; see \code{\link[ggplot2]{print.ggplot}}
#' 
#' @seealso \code{\link{facet_adjust}}

print.facet_adjust <- function(x, newpage = is.null(vp), vp = NULL) {
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
#' Caterpillar plots for random effects models using \code{\link{ggplot}}.
#' 
#' Behaves like \code{\link[lattice]{qqmath}} and 
#' \code{\link[lattice]{dotplot}} from the lattice package; also handles
#' models with multiple correlated random effects
#' 
#' @param re random effects from lmer object
#' @param qq if \code{TRUE}, returns normal q/q plot; else returns caterpillar
#' dotplot
#' @param likeDotplot if \code{TRUE}, uses different scales for random effects,
#' i.e., \code{\link[ggplot2]{facet_wrap}}
#' 
#' @examples
#' \donttest{
#' library('lme4')
#' fit <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#' 
#' ggcaterpillar(ranef(fit, condVar = TRUE))
#' 
#' ## compare (requires lattice package)
#' lattice::qqmath(ranef(fit, condVar = TRUE))
#' }
#' @export

ggcaterpillar <- function(re, qq  =  TRUE, likeDotplot  =  TRUE) {
  
  f <- function(x) {
    pv   <- attr(x, 'postVar')
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + 
      rep((0:(ncol(x) - 1)) * nrow(x), each = nrow(x))
    pDf  <- data.frame(y = unlist(x)[ord],
                       ci = 1.96 * se[ord],
                       nQQ = rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID = factor(rep(rownames(x), ncol(x))[ord], 
                                   levels = rownames(x)[ord]),
                       ind = gl(ncol(x), nrow(x), labels = names(x)))
    if (qq) {
      ## normal q/q plot
      p <- ggplot(pDf, aes(nQQ, y)) + 
        facet_wrap(~ ind, scales = 'free') +
        xlab('Standard normal quantiles') + 
        ylab('Random effect quantiles')
    } else {
      ## caterpillar dotplot
      p <- ggplot(pDf, aes(x = ID, y = y)) + coord_flip()
      if (likeDotplot) {
        ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap( ~ ind)
      } else {
        ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales = 'free_y')
      }
      p <- p + xlab('Levels') + ylab('Random effects')
    }
    
    p + theme_bw() + 
      theme(legend.position = 'none') + 
      geom_hline(yintercept = 0) + 
      geom_errorbar(aes(ymin = y - ci, ymax = y + ci), 
                    width = 0, colour = 'black') + 
      geom_point(aes(size = 1.2), colour = 'blue')
  }
  lapply(re, f)
}

#' ggheat
#' 
#' Function to plot a heat map using \code{\link{ggplot}}.
#' 
#' \code{gradn} takes a vector of \code{n} colors: either a list of names, a 
#' list of hexadecimal values, an existing color palette (i.e., 
#' \code{\link{heat.colors}}, \code{\link{rainbow}}, etc); see also 
#' \code{\link{palette}}, \code{\link{colors}}, the \code{RColorBrewer} 
#' package, etc.
#' 
#' @param cors matrix (or matrix-like object) of correlations
#' @param data data frame of data (in progress)
#' @param limits range of color scale in legend; see 
#' \code{\link[ggplot2]{discrete_scale}}
#' @param gradn vector of \code{n} colors to use, from low to high; see details
#' @param gradc vector of length two, low color and high color; see details
#' 
#' \code{gradc} takes a low color and a high color, respectively, and generates
#' a continuous scale between those colors; see 
#' \code{\link[ggplot2]{scale_fill_gradient}}
#' 
#' @seealso
#' \code{\link{cor}}, \code{\link{ggheat2}}, \code{\link[iplotr]{icorr}}
#' 
#' @examples
#' library('ggplot2')
#' tmp <- rescaler(matrix(1:25, 5))
#' diag(tmp) <- 1
#' colnames(tmp) <- rownames(tmp) <- LETTERS[1:5]
#' ggheat(cors = tmp, limits = c(0, 1))
#' ggheat(cors = tmp, gradn = NULL, gradc = c('white','red'))
#' 
#' @export

ggheat <- function(cors = NULL, data = NULL, limits = c(-1, 1),
                   gradn = rev(heat.colors(10)),
                   gradc = c('white', 'steelblue')) {
  
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
    p <- p + 
      scale_fill_gradient(limits = limits, low = gradc[1], high = gradc[2])
  }
  if (!is.null(gradn))
    p <- p + scale_fill_gradientn(limits = limits, colours = gradn)
  
  p
}

#' ggheat2
#'
#' Function to plot a heat map using \code{\link{ggplot}}.
#' 
#' Default cluster method is \code{stats::hclust(dist(x), method = 'average')}
#' which will return a list containing a named vector, "order", which is used
#' to reorder the variables.
#'
#' In order to pass a custom clustering function to \code{cluster}, the
#' function must take a single input (a correlation matrix) and return either
#' a vector or a list with a named vector, \code{"order"}.
#' 
#' @param data a data frame or matrix (observations x variables) of numeric
#' values
#' @param corr a correlation matrix
#' @param cluster logical or function; if \code{TRUE}, the variables will be
#' clustered and reordered; if \code{FALSE}, no reordering will be done;
#' otherwise, a custom clustering function may be given; see details
#' @param nbreaks number of breaks to categorize the correlations (default is
#' \code{NULL}, ie, a continuous scale)
#' @param palette for a continuous scale, a vector of length three giving the
#' low, mid, and high colors of the gradient (default is
#' \code{c('blue','white',''red)}, see \code{\link{scale_colour_gradient2}});
#' for a discrete scale, a character string of the palette name or an integer
#' giving the index of the palette, see \code{\link{scale_colour_brewer}}
#' @param legend_name the legend name; see \code{\link{discrete_scale}}
#' @param pch (optional) plotting character; if \code{missing},
#' \code{\link{geom_tile}} is used instead of plotting characters
#' @param cex size of \code{pch}; a vector of length one (all \code{pch} will
#' be the same size) or two (size will be proportional to the correlation);
#' default is c(2,6); see\code{\link{scale_size_identity}}
#' @param label logical; if \code{TRUE}, adds correlation coefficients on top
#' of each \code{pch}
#' @param label_alpha logical, if \code{TRUE}, adds alpha transparency when
#' \code{label} is \code{TRUE} (correlations closer to 0 will be less visible)
#' @param label_color color of correlations (default is \code{'black'})
#' @param label_digits number of digits in correlation labels
#' @param midpoint the midpoint value for continuous scaling of correlations
#' (default is \code{0})
#' @param clim vector of length two giving the limits of correlation
#' coefficients (default is \code{-1,1})
#' @param ... additional arguments passed to \code{\link{geom_text}} for the
#' diagonal labels
#' 
#' @seealso
#' \code{\link{cor}}, \code{\link{ggheat}}, \code{\link[iplotr]{icorr}},
#' \code{\link[arm]{corrplot}}, \url{https://github.com/briatte/ggcorr}
#' 
#' @examples
#' library('ggplot2')
#' ggheat2(mtcars)
#'
#' ggheat2(mtcars, label = TRUE, label_alpha = TRUE, cluster = FALSE,
#'         ## additional args passed to diagonal labels
#'         colour = 'red', angle = 45, size = 7)
#'
#' ggheat2(mtcars, pch = 19, nbreaks = 6, cex = c(2,10),
#'         palette = 'PuOr',         ## colorblind palette
#'         size = 5, hjust = 0.75) + ## passed to diag text
#'     labs(title = 'Correlation Matrix')
#'
#' ## custom clustering function
#' ggheat2(data = NULL, corr = cor(mtcars, use = 'pairwise'),
#'         nbreaks = 5, palette = 'Blues',
#'         cluster = function(...) sample(ncol(mtcars)))
#' 
#' @export

ggheat2 <- function(data, corr = cor(data, use = 'pairwise.complete'),
             cluster = TRUE, nbreaks = NULL,
             palette = if (is.null(nbreaks)) c('blue','white','red') else 1,
             legend_name = expression(rho), pch, cex = c(2, 6),
             label = FALSE, label_alpha = FALSE, label_color = 'black',
             label_digits = 2, midpoint = 0, clim = c(-1, 1), ...) {
  
  ## clustering
  stopifnot(class(cluster) %in% c('logical','function'))
  ord <- if (is.function(cluster)) cluster(corr) else
    if (cluster) stats::hclust(dist(corr), method = 'average')$order else
      seq.int(ncol(corr))
  
  ord <-  tryCatch(if (is.vector(ord, mode = 'integer')) ord else ord$order,
                   error = function(e) {
                     warning('Variables not reordered; see \'details\' ',
                             'section on the use of \'cluster\'', domain = NA)
                     seq.int(ncol(corr))
                   },
                   warning = function(w) {
                     warning('Variables not reordered; see \'details\' ',
                             'section on the use of \'cluster\'', domain = NA)
                     seq.int(ncol(corr))
                   })
  corr <- corr[ord, ord]
  
  if (label_alpha)
    label <- TRUE
  colnames(corr) <- rownames(corr) <- make.names(colnames(corr))
  cex <- c(cex, cex)
  dd <- round(corr, label_digits)
  dd <- as.data.frame(dd * lower.tri(dd))
  rn <- names(dd)
  dd <- data.frame(row = rn, dd)
  dd <- melt(dd, varying = list(2:ncol(dd)))
  
  corr <- as.data.frame(corr * lower.tri(corr))
  corr <- cbind.data.frame(row = rn, corr)
  corr <- melt(corr, varying = list(2:ncol(corr)))
  corr$value[corr$value == 0] <- NA
  
  if (!is.null(nbreaks)) {
    s <- seq(-1, 1, length.out = nbreaks + 1)
    if (!nbreaks %% 2)
      s <- unique(sort(c(s, 0)))
    corr <- within(corr, {
      value <- droplevels(cut(value, breaks = s, include.lowest = TRUE))
      value <- factor(value,
                      levels = unique(cut(s, breaks = s, include.lowest = TRUE)))
    })
  }
  
  if (is.null(midpoint)) {
    midpoint <- median(corr$value, na.rm = TRUE)
    message('Color gradient midpoint set to median of correlation, ',
            round(midpoint, 2), domain = NA)
  }
  
  corr <- within(corr, {
    row <- factor(row, levels = unique(as.character(variable)))
    num <- as.numeric(value)
    num <- as.numeric(factor(abs(num - median(unique(num), na.rm = TRUE))))
    num <- seq(cex[1], cex[2], length.out = length(na.omit(unique(num))))[num]
    variable <- factor(variable, levels = levels(row))
  })
  
  diag  <- corr[with(corr, row == variable), ]
  corr <- corr[complete.cases(corr), ]
  p <- ggplot(corr, aes(x = row, y = variable))
  
  ## if pch is given, use geom_point
  if (!missing(pch)) {
    p <- p + geom_point(aes(size = num, colour = value), shape = pch)
    if (is.null(nbreaks))
      p <- p + scale_size_continuous(range = c(cex[1], cex[2])) +
        scale_colour_gradient2(legend_name, limits = clim, low = palette[1],
                               mid = palette[2], high = palette[3],
                               midpoint = midpoint) +
        guides(size = FALSE)
    else
      p <- p + scale_size_identity(legend_name) +
        scale_colour_brewer(legend_name, palette = palette, drop = FALSE) +
        guides(colour = guide_legend(
          legend_name, override.aes = list(size = (cex[1] + cex[2]) / 2))
        )
    ## use geom_tile otherwise
  } else {
    p <- p + geom_tile(aes(fill = value), colour = 'white') +
      if (is.null(nbreaks))
        scale_fill_gradient2(legend_name, low = palette[1], mid = palette[2],
                             high = palette[3], midpoint = midpoint,
                             limits = clim)
    else 
      scale_fill_brewer(legend_name, palette = palette, drop = FALSE)
  }
  
  ## corr labels
  if (label) {
    dd <- dd[dd$value != 0, ]
    p <- p +
      if (label_alpha)
        geom_text(data = dd, aes(row, variable, label = value,
                                 alpha = abs(as.numeric(value))),
                  show_guide = FALSE, colour = label_color)
    else geom_text(data = dd, aes(row, variable, label = value),
                   colour = label_color)
  }
  
  ## diagonal text and options
  p <- p  +
    geom_text(data = diag, aes(label = variable), ...) +
    scale_x_discrete(breaks = NULL, limits = levels(corr$row)) +
    scale_y_discrete(breaks = NULL, limits = levels(corr$variable)) +
    labs(x = NULL, y = NULL) +
    coord_equal() +
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.key       = element_blank(),
          legend.title	   = element_text(size = 15),
          axis.text.x      = element_text(angle = -90))
  p
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
#' \code{formula}; by default, variables are taken from 
#' \code{environment(formula)}
#' @param x a numeric vector of data
#' @param at grouping variables or, equivalently, positions along x-axis
#' @param jit,dist jittering parameters; \code{jit} describes the spread of
#' close points, and \code{dist} defines a range of data to declare points
#' "close"
#' @param ... ignored
#' 
#' @seealso
#' \code{\link{jitter}}, \code{\link{tplot}}
#' 
#' @examples
#' ## these are equivalent ways to call dodge:
#' dodge(mpg ~ gear + vs, mtcars)
#' with(mtcars, dodge(mpg, list(gear, vs)))
#' dodge(mtcars$mpg, mtcars[, c('gear', 'vs')])
#' 
#' ## compare to overlapping points and jittering
#' sp <- split(mtcars$mpg, do.call(interaction, mtcars[, c('gear','vs')]))
#' plot.new()
#' plot.window(c(.5,6.5),c(10,35))
#' box()
#' invisible(sapply(seq_along(sp), function(x)
#'   points(rep(x, length(sp[[x]])), sp[[x]])))
#' invisible(sapply(seq_along(sp), function(x)
#'   points(jitter(rep(x, length(sp[[x]]))), sp[[x]], col = 4, pch = 2)))
#' points(dodge(mpg ~ gear + vs, data = mtcars), col = 2, pch = 4)
#' legend('topleft', pch = c(1,1,4), col = c(1,4,2),
#'        legend = c('overlapping','random jitter','dodging'))
#' 
#' ## practical use
#' boxplot(mpg ~ vs + gear, data = mtcars)
#' points(dodge(mpg ~ vs + gear, data = mtcars), col = 'red', pch = 19)
#'
#' @export

dodge <- function(x, ...) UseMethod('dodge')

#' @rdname dodge
#' @export
dodge.formula <- function(formula, data = NULL, ...) {
  if (missing(formula) || (length(formula) !=  3))
    stop("\'formula\' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  
  args <- lapply(m$..., eval, data, parent.frame())
  nmargs <- names(args)
  m$... <- NULL
  
  m[[1]] <- as.name('model.frame')
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, 'terms'), 'response')
  
  dodge(mf[, response], mf[, -response])
}

#' @rdname dodge
#' @export
dodge.default <- function(x, at, dist, jit, ...) {
  if (missing(at))
    at <- 1
  at <- do.call('interaction', list(at))
  if (missing(dist) || is.na(dist) || is.null(dist)) 
    dist <- diff(range(x)) / 100
  if (missing(jit) || is.na(jit) || is.null(jit)) 
    jit <- 0.1
  ## call dodge on each group
  sp <- if (length(unique(at)) > 1) split(x, at) else list(x)
  l <- lapply(seq_along(sp), function(xx)
    dodge_(sp[[xx]], seq_along(sp)[xx], dist, jit))
  do.call('rbind', l)
}

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
    args <- lapply(args, do_sub_)
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
    args <- lapply(args, do_sub_)
    ## rawr:::do_sub_
    mf <- mf[s, ]
  }
  do.call('dsplot', c(list(mf[[response]], mf[[-response]]), args))
}

#' Barplot confidence intervals
#' 
#' Add confidence intervals (error bars) and group comparisons to barplots.
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
#' @param pch plotting character to be used for significance; default is 
#' \code{*} and uses same significance codes as \code{\link{printCoefmat}}
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @examples
#' ## generate data and p-values
#' hh <- t(VADeaths)[1:2, 5:1]
#' ci.l <- hh * 0.85
#' ci.u <- hh * 1.15
#' pvals <- pt(apply(hh, 2, diff), 1) / 5:1
#' 
#' bp <- barplot(hh, beside = TRUE, ylim = c(0,100))
#' bpCI(bp, ci.u = ci.u, ci.l = ci.l, sig = TRUE, pvals = pvals, pch = "+")
#' mtext("Signif. codes:  0 '+++' 0.001 '++' 0.01 '+' 0.05 '.' 0.1 ' ' 1",
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
#' pv <- pt(-abs(apply(hh, 2, diff)), 1)
#' 
#' bpCI(bp, ci.u = ci.u, ci.l = ci.l, sig = TRUE, pvals = pv, ci.width = 100,
#'      col = 'red', lty = 'dashed', lwd = 2)
#' mtext("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
#'       side = 1, at = par('usr')[2], line = 2, adj = 1, cex = .8, font = 3)
#' 
#' @export

bpCI <- function(bp, horiz = FALSE, ci = TRUE, ci.u, ci.l, ci.width = .5,
                     sig = FALSE, pvals, pch, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(...)
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
      if (horiz)
        stop('\'sig\' is not supported when \'horiz = TRUE\'')
      if (nrow(bp) > 2)
        stop('\'sig\' is not supported for > 2 bars per group')
      if (missing(pch))
        pch <- '*'
      yy <- rbind(c(ci.u[1, ] + 3), c(apply(ci.u, 2 , max) + 5),
                  c(apply(ci.u, 2, max) + 5), c(ci.u[2, ] + 3))
      xx <- apply(bp, 2, function(x) rep(x, each = nrow(bp)))
      sapply(1:ncol(bp), function(x) lines(xx[, x], yy[, x]))
      xt <- colMeans(bp)
      yt <- apply(ci.u, 2, max) + 7
      text(pstar_(pvals, pch), x = xt, y = yt)
    }
  }
}

#' Inset plots
#'
#' Inset new plots in existing plot windows.
#' 
#' \code{x} should be of the form \code{x0, x1} and similar for \code{y} 
#' giving the starting and ending coordinates to draw the inset plot and must
#' be in the range defined in the current plotting area.
#' 
#' Alternatively, \code{x} can be a keyword ("bottomright", "bottom",
#' "bottomleft", "left", "topleft", "top", "topright" "right", or "center")
#' giving the approximate location of the inset plot. \code{pct} is used to
#' adjust the size.
#' 
#' @param x a keyword (see details) or a vector of length two giving the 
#' positions on the current plot at which to draw the inset plot along the
#' x-axis
#' @param y ignored if \code{x} is a keyword or a vector of length two giving
#' the positions on the current plot at which to draw the inset plot along the
#' y-axis
#' @param pct inset plot scaling (only used if \code{x} is a keyword)
#' @param ... additional graphical parameters passed to \code{\link{par}}
#' 
#' @examples
#' op <- par(no.readonly = TRUE)
#' plot(mpg ~ wt, data = mtcars, col = 'blue')
#' abline(lm(mpg ~ wt, data = mtcars), col = 'red')
#' inset('topright', pct = .4)
#' hist(mtcars$mpg, ann = FALSE, panel.last = box(),
#'      col = 'dodgerblue2', las = 1)
#' 
#' par(op)
#' plot(1:10)
#' op <- par(no.readonly = TRUE)
#' Map(function(x) {
#'  inset(x, las = 1, col = 'red', pct = 1/3)
#'  plot(rnorm(10), ann = FALSE, axes = FALSE, panel.last = box())
#'  par(op)
#'  Sys.sleep(.5)
#'  }, c("bottomright", "bottom", "bottomleft", "left",
#'       "topleft", "top", "topright", "right", "center"))
#' 
#' @export

inset <- function(x, y = NULL, pct = .25, ...) {
  m <- substitute(...())
  usr <- par('usr')
  plt <- par('plt')
  pctx <- pct * diff(plt[1:2])
  pcty <- pct * diff(plt[3:4])
  
  auto <- if (is.character(x))
    match.arg(x, c("bottomright", "bottom", "bottomleft", "left",
                   "topleft", "top", "topright", "right", "center")) else NA
  
  xx <- switch(auto, bottomright = c(plt[2] - pctx, plt[2]),
               bottom = mean(plt[1:2]) + c(-1, 1) * pctx / 2,
               bottomleft = c(plt[1], plt[1] + pctx),
               left = c(plt[1], plt[1] + pctx),
               topleft = c(plt[1], plt[1] + pctx),
               top = mean(plt[1:2]) + c(-1, 1) * pctx / 2,
               topright = c(plt[2] - pctx, plt[2]),
               right = c(plt[2] - pctx, plt[2]),
               center = mean(plt[1:2]) + c(-1, 1) * pctx / 2)
  
  yy <- switch(auto, bottomright = c(plt[3], plt[3] + pcty),
               bottom = c(plt[3], plt[3] + pcty),
               bottomleft = c(plt[3], plt[3] + pcty),
               left = mean(plt[3:4]) + c(-1, 1) * pcty / 2,
               topleft = c(plt[4] - pcty, plt[4]),
               top = c(plt[4] - pcty, plt[4]),
               topright = c(plt[4] - pcty, plt[4]),
               right = mean(plt[3:4]) + c(-1, 1) * pcty / 2,
               center = mean(plt[3:4]) + c(-1, 1) * pcty / 2)
  
  # xx <- rescaler(xx, plt[1:2], usr[1:2])
  # yy <- rescaler(yy, plt[3:4], usr[3:4])
  if (is.na(auto)) {
    xx <- grconvertX(x, 'user', 'ndc')
    yy <- grconvertY(y, 'user', 'ndc')
  }
  if ('mar' %ni% names(m))
    par(fig = c(xx, yy), new = TRUE, mar = c(0,0,0,0), ...)
  else par(fig = c(xx, yy), new = TRUE, ...)
  invisible(c(xx, yy))
}

#' Show colors
#' 
#' In \code{R}, there are 657 named colors. This function shows these colors 
#' and their respective numbers. Find a color by number in the plot or by the
#' name of the color with \code{colors()[n]}.
#' 
#' @seealso \code{\link{show_pch}}
#' 
#' @examples
#' show_colors()
#' colors()[81]
#' # [1] "darkgreen"
#' 
#' @export

show_colors <- function() {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(1,1), mai=c(.4,.4,.4,.4), oma=c(.2,0,0,.2))
  x <- 22
  y <- 30
  plot(c(-1, x), c(-1, y), ann = FALSE, type = 'n', bty = 'n', axes = FALSE)
  sapply(1:x, function(i) {
    sapply(1:y, function(j) {
      k <- y * (i - 1) + j
      co <- colors()[k]
      rect(i - 1, j - 1, i, j, col = co, border = grey(.5))
    })
  })
  text(rep(-.5, y), (1:y) - .5, 1:y, cex = 1.2 - .016 * y)
  text((1:x) - .5, rep(-.5, x), y * (0:(x - 1)), cex = 1.2 - .022 * x)
  title('col = colors()[n]')
}

#' Show plotting characters
#' 
#' In \code{R}, there are 26 numeric plotting characters. This function shows 
#' these options and their respective numbers. Note that \code{col} specifies
#' both the border and fill color (if applicable) for \code{0:20}; \code{pch}s
#' \code{21:25} can be filled with \code{bg}.
#' 
#' @seealso \code{\link{show_colors}}
#' 
#' @examples
#' show_pch()
#' 
#' @export

show_pch <- function() {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(xpd = TRUE, mfrow = c(1, 1), mai = c(.4,.4,.4,.4), oma = c(.2,0,0,.2))
  x <- rep(1:5, 6)[1:26]
  y <- c(rep(5:1, each = 5)[1:25], 0)
  plot(x, y, pch = 0:25, axes = FALSE, bg = 'gray', cex = 2, col = 'red')
  text(x = x, y = y, labels = 0:25, pos = 4, cex = 1.5, offset = 1)
  text(x = 4, y = 0, labels = 'plotting characters 0:25', cex = 1.5)
}

#' Transparent colors
#' 
#' Add alpha transparency to colors.
#' 
#' This is a vectorized function to add transparency to colors. \code{color}
#' and \code{trans} (or \code{alpha}) must either be the same length or one
#' of the two must have length one. 
#' 
#' The function adds integers (in hexadecimal) between 0 (fully transparent)
#' and 255 (fully visible) to the color(s) given. \code{color} values are
#' converted to RGB with transparency.
#' 
#' @param color vector of color names (or hexadecimal) as character strings
#' or integers corresponding to colors in the current \code{\link{palette}}
#' @param trans alpha transparency defined as an integer in the range 
#' \code{[0, 255]} where \code{0} is fully transparent and \code{255} is fully
#' visible; see details
#' @param alpha the alpha transparency in \code{[0,1]}; \code{trans} is
#' ignored if \code{alpha} is given
#' 
#' @seealso \code{\link{as.hexmode}}, \code{\link{col2rgb}},
#' \code{\link{adjustcolor}}
#' 
#' @examples
#' cols <- c('red','green','blue')
#' 
#' ## a normal plot
#' plot(rnorm(100), col = tcol(cols), pch = 16, cex = 4)
#' 
#' ## more transparent
#' plot(x <- rnorm(100), col = tcol(cols, 100), pch = 16, cex = 4)
#' ## or equivalently using alpha
#' plot(x, col = tcol(cols, alpha = .4), pch = 16, cex = 4)
#' 
#' ## hexadecimal colors also work
#' cols <- c('#FF0000','#00FF00','#0000FF')
#' plot(rnorm(100), col = tcol(cols, c(50, 100, 255)), pch= 16, cex = 4)
#' 
#' @export

tcol <- function(color, trans = 255, alpha) {
  if (!missing(alpha)) {
    stopifnot(alpha %inside% c(0,1))
    trans <- round(rescaler(alpha, to = c(0,255), from = c(0,1)))
  }
  if (length(color) != length(trans) & 
        !any(c(length(color), length(trans)) == 1)) 
    stop('Vector lengths are not comformable')
  if (length(color) == 1 & length(trans) > 1) 
    color <- rep(color, length(trans))
  if (length(trans) == 1 & length(color) > 1) 
    trans <- rep(trans, length(color))
  res <- paste0('#', apply(apply(rbind(col2rgb(color)), 2, function(x)
    format(as.hexmode(x), 2)), 2, paste, collapse = ''))
  res <- unlist(unname(Map(paste0, res, as.character(as.hexmode(trans)))))
  res[is.na(color)] <- NA
  res
}

#' ggplot colors
#' 
#' A function to replicate default \code{\link[ggplot2]{ggplot}} colors.
#' 
#' @param n number of colors
#' @param c the chroma of the color; the upper bound for chroma depends on hue
#' and luminance
#' @param l a value in the range \code{[0, 100]} giving the luminance of the 
#' color; for a given combination of hue and chroma, only a subset of this 
#' range is possible
#' @seealso \code{\link{hcl}}
#' 
#' @examples
#' plot(rnorm(1000), col = ggcols(1000), pch = 19)
#' 
#' @export

ggcols <- function(n, l = 65, c = 100) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = l, c = c)[1:n]
}

#' Choose n colors using the golden ratio
#'
#' This chooses \code{n} colour hues using a sequence generated by the Golden
#' Ratio.
#'
#' @param n number of colors
#' @param s,v numeric vectors of values in the range \code{[0, 1]} for 
#' "saturation" and "value," respectively, to be combined to form a vector of
#' colors; values in shorter arguments are recycled
#' @param alpha  numeric vector of values in the range \code{[0, 1]} for alpha 
#' transparency channel (0 is transparent and 1 is opaque)
#' @seealso \code{\link{hsv}}
#' 
#' @examples
#' plot(1:5, 1:5, col = grcols(5), pch = 20, cex = 3)
#' 
#' plot(c(1, 6), c(0, 1), type = 'n', axes = FALSE, 
#'      bty = 'n', xlab = '', ylab = '')
#' rect(1:5, 0, 2:6, 1, col = grcols(5), border = NA)
#' 
#' @export

grcols <- function(n, s = .5, v = 1, alpha = 1) {
  GR <- 2 / (1 + sqrt(5))
  hues <- (seq(0, n - 1) * GR) %% 1
  hsv(hues, s = s, v = v, alpha = alpha)
}

#' Color text
#' 
#' Add color to individual words in text functions. \code{ctext},
#' \code{cmtext}, and \code{ctitle} are analogous to \code{\link{text}},
#' \code{\link{mtext}}, and \code{\link{title}}, respectively. Note that
#' \code{title} accepts some graphical parameters specific to the label type,
#' e.g., \code{col.main}, but this is not implemented in \code{ctitle}--colors
#' will be recycled if more than one label type is given. Similarly, further
#' graphical parameters such as \code{cex} or \code{line} will be passed to
#' all label types; see examples.
#' 
#' @param text vector of text
#' @param cols vector of colors; should be the same lenght as \code{text} or
#' will me recycled with a warning
#' @param space logical; if \code{TRUE}, adds space between \code{text}
#' @param ... additional parameters passed to \code{text} or \code{mtext}
#' @param main,sub,xlab,ylab vector(s) of text for specific labels
#' 
#' @examples
#' plot(1, ann = FALSE)
#' ctext(x = 1, y = 1, text = c('hello','little','point'), cols = 1:3, pos = 1)
#' cmtext(c('a','side','label'), 1:2, space = FALSE, side = 4, cex = 3)
#' 
#' ## note that line, cex, font, etc will be recycled
#' ctitle(main = c('the','main','label'), xlab = c('x','label'),
#'        ylab = c('y','label'), sub = c('sub', 'label'), col = 3:5)
#' ctitle(xlab = c('another','label'), ylab = c('another','label'),
#'        font = 3, col = 1:2, line = 2, cex = 1.5)
#'
#' @export

ctext <- function(text, cols, space = TRUE, ...) {
  if (missing(cols))
    cols <- rep(1, length(text))
  l <- ctext_(text, cols, space)
  for (ii in seq_along(l$text))
    text(labels = l$text[[ii]], col = l$colors[ii], ...)
  invisible(NULL)
}

#' @rdname ctext
#' @export
cmtext <- function(text, cols, space = TRUE, ...) {
  if (missing(cols))
    cols <- rep(1, length(text))
  l <- ctext_(text, cols, space)
  for (ii in seq_along(l$text))
    mtext(text = l$text[[ii]], col = l$colors[ii], ...)
  invisible(NULL)
}

#' @rdname ctext
#' @export
ctitle <- function(main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                   cols, space = TRUE, ...) {
  m <- match.call(expand.dots = FALSE)
  dots <- m$...
  ml <- dots$line
  dots$line <- NULL
  if (missing(cols))
    cols <- rep(1, length(text))
  wh <- c('main','sub','xlab','ylab')
  l <- setNames(lapply(wh, function(x)
    ## if c() not used to pass text, this could cause problems
    ctext_(as.character(m[[x]])[-1], cols, space)), wh)
  m <- par('mgp')
  if (length(l$main$text)) {
    ll <- l$main
    for (ii in seq_along(ll$text))
      do.call('mtext', c(list(text = ll$text[[ii]], col = ll$colors[ii],
                     side = 3, font = 2, line = ml %||% (m[1] * .5)), dots))
  }
  if (length(l$sub$text)) {
    ll <- l$sub
    for (ii in seq_along(ll$text))
      do.call('mtext', c(list(text = ll$text[[ii]], col = ll$colors[ii],
                              side = 1, line = ml %||% (m[1] + 1)), dots))
  }
  if (length(l$xlab$text)) {
    ll <- l$xlab
    for (ii in seq_along(ll$text))
      do.call('mtext', c(list(text = ll$text[[ii]], col = ll$colors[ii],
                              side = 1, line = ml %||% m[1]), dots))
  }
  if (length(l$ylab$text)) {
    ll <- l$ylab
    for (ii in seq_along(ll$text))
      do.call('mtext', c(list(text = ll$text[[ii]], col = ll$colors[ii],
                         side = 2, line = ml %||% m[1]), dots))
  }
  invisible(NULL)
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

river <- function(data, id, at = id, legend = 'topleft', xlim, ylim, ...) {
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
  cols <- c('transparent','transparent','yellow','orange','blue','green4')
  
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
  plot.window(if (!missing(xlim)) xlim else c(0, rx[2]),
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
