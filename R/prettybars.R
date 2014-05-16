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