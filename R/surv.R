### survival stuff
# kmplot, ggsurv, survdat
###

#' Survival curves in base graphics
#' 
#' Function to plot Kaplan-Meier or Cox proportional hazards plots with 
#' optional at risk table in base graphics
#' 
#' @usage
#' kmplot(s, 
#'        # basic plot options
#'        lty.surv = 1, lwd.surv = 1, col.surv = 1, 
#'        mark = 3,
#'       
#'        # confidence options
#'        lty.ci = 0, lwd.ci = 1, col.ci = col.surv, 
#'        col.band = col.surv,
#'        
#'        # at risk table options
#'        atrisk = TRUE, atrisk.lab = 'Number at risk', 
#'        atrisk.lines = TRUE, strata.lab = NULL, 
#'        strata.order = seq(length(s$n)), 
#'        extra.margin = 5, 
#'        
#'        # aesthetics
#'        xlim = c(0, max(s$time)), ylim = c(0, 1),
#'        xaxis.at = pretty(s$time), xaxis.lab = xaxis.at, 
#'        yaxis.at = pretty(ylim), yaxis.lab = yaxis.at, 
#'        xlab = 'Time', ylab = 'Survival probability', 
#'        main = '', cex.axis = 1, 
#'        legend = !is.null(s$strata), legend.pos = 'bottomleft', 
#'        
#'        # other options
#'        grid = TRUE, lty.grid = 1, lwd.grid = 1, 
#'        col.grid = grey(.9),
#'        dev = TRUE, add = FALSE, ...)
#' 
#' @param s object of class \code{\link[survival]{survfit}} or 
#' \code{survfit.cox}
#' @param lty.surv line type for survival curve(s); see \code{\link{par}}
#' @param lwd.surv line width for survival curve(s)
#' @param col.surv line color for survival curve(s); either numeric or 
#' character string(s); hexadecimal format also works
#' @param mark numeric plotting character (\code{\link{pch}}) or character 
#' string, i.e., \code{''}, \code{'|'}, etc
#' @param lty.ci line type for confidence interval(s); not plotted (\code{= 0})
#' by default
#' @param lwd.ci line width for confidence interval(s)
#' @param col.ci line color for confidence interval(s); either numeric or 
#' character string(s); hexadecimal format also works
#' @param col.band line color for confidence band(s); either numeric or 
#' character string(s); hexadecimal format also works; note that this is not a 
#' true confidence band; see details
#' @param atrisk logical; if \code{TRUE}, draws at risk table
#' @param atrisk.lab heading for at risk table
#' @param atrisk.lines logical; draw lines next to strata in at risk table
#' @param strata.lab labels used in legend and at risk table for strata; if 
#' \code{NULL} (default), labels created in \code{survfit} are used; if only
#' one strata is present, "All" is used by default
#' @param strata.order order of strata in legend and at risk table
#' @param extra.margin increase left margin when strata labels in at risk table
#' are long
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param xaxis.at positions for x-axis labels and ticks
#' @param xaxis.lab x-axis labels
#' @param yaxis.at positions for y-axis labels and ticks
#' @param yaxis.lab y-axis labels
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param main title
#' @param cex.axis text size for axes labels, legend, at risk table
#' @param legend logical; draw legend
#' @param legend.pos legend position
#' @param grid logical; draw grid in background
#' @param lty.grid line type for grid
#' @param lwd.grid line width for grid
#' @param col.grid line color for grid
#' @param dev logical; if \code{TRUE}, will plot on a platform-specific
#' graphics device; see details
#' @param add logical; if \code{TRUE}, \code{par} is not refreshed; allows for
#' multiple panels, e.g., when using \code{par(mfrow = c(1, 2))}
#' @param ... additional parameters (\code{font}, \code{mfrow}, \code{bty}, 
#' \code{tcl}, \code{cex.lab}, \code{xaxs}, etc) passed to \code{par}
#' 
#' @details
#' Line specifications (\code{lty.surv}, \code{lwd.surv}, etc) will be recycled
#' if needed.
#' 
#' If \code{col.band != NULL}, a confidence band is plotted; however, this is
#' not a confidence band in the statistical sense, i.e., a xx-percent chance 
#' of containing the entire population of the survival curve which are wider 
#' than the point-wise confidence limits. Rather, it refers to a band of color 
#' plotted between the confidence limits calculated in the survfit object. That
#' is, the xx-percent confidence interval (plotted when \code{lty.ci != 0}) and
#' the confidence bands are identical, just two ways of plotting the same 
#' invervals.
#' 
#' When \code{dev = TRUE}, a platform-specific graphics device is used to 
#' displace the plot. Defaults are \code{\link{quartz}}, \code{\link{windows}},
#' and \code{\link{x11}} for apple, windows, and unix platforms, respectively.
#' However, any device can be used by setting \code{dev = FALSE} and opening
#' a device before running \code{kmplot}. If \code{dev} is \code{FALSE}, the
#' plot will open in the current device; see \code{\link{.Device}}. Close the
#' device pane or use \code{dev.off()} to turn off the current device.
#' 
#' When saving plots, it is highly recommended to use \code{\link{png}}, 
#' \code{\link{svg}}, \code{\link{pdf}}, etc instead of exporting directly from
#' the \code{R} graphics device. Doing so may cause the at risk table or 
#' legend to be mis-aligned. Be sure to set \code{dev = FALSE} when saving
#' an image to another device.
#' 
#' @references \url{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}
#' @seealso \code{\link[rawr]{ggsurv}}; \code{survival:::plot.survfit}
#' 
#' @examples
#' library(survival)
#' kmfit1 <- survfit(Surv(time, status) ~ sex, data = colon)
#' kmfit2 <- survfit(Surv(time, status) ~ rx + adhere, data = colon)
#' 
#' \dontrun{
#' ## simple example, draw in r window
#' kmplot(kmfit1, dev = FALSE)
#' 
#' ## using mfrow options, use ADD = TRUE
#' ## when saving to another device, use dev = FALSE (see details)
#' png('./desktop/kmplot2.png', width = 750, height = 1200, pointsize = 14)
#' par(mfrow = c(2, 1))
#' kmplot(kmfit1, add = TRUE, dev = FALSE)
#' kmplot(kmfit2, add = TRUE, dev = FALSE, extra.margin = 8)
#' dev.off()
#' 
#' ## more complex example
#' pdf('./tmp.pdf', height = 8, width = 11, pointsize = 12)
#' kmplot(kmfit2, 
#'        dev = FALSE,  # view in r window
#'        mark = '',    # no censor mark
#'        lty.ci = 2,   # dashed line for CIs
#'        xaxis.at = c(0, .5, 1:9) * 365,    # change days to years
#'        xaxis.lab = c(0, .5, 1:9),         # label years
#'        yaxis.lab = pretty(c(0, 1)) * 100, # change to percent
#'        xlab = 'Time (years)', 
#'        ylab = 'Percent survival', 
#'        col.surv = c('blue', 'red', 'green', 'black','purple','orange'), 
#'        strata.lab = c('Obs ','Obs+ ','Lev ','Lev+ ','Lev5fu','Lev5fu+'),
#'        extra.margin = 6, # increase margin for long strata labels
#'        strata.order = c(5, 6, 3, 1, 4, 2), 
#'        # col.band = NULL, # remove confidence bands
#'        font = 2,  # bold table text
#'        bty = 'l', # L box type around plot
#'        tcl = .5)  # change length/direction of ticks
#' title(main='Chemotherapy for stage B/C colon cancer', 
#'       adj = .5, font.main = 1, line = 0.5, cex.main = 1)
#' dev.off()
#' }
#' @export

kmplot <- function(s, 
                   # basic plot options
                   lty.surv = 1, lwd.surv = 1, col.surv = 1, 
                   mark = 3,
                   
                   # confidence options
                   lty.ci = 0, lwd.ci = 1, col.ci = col.surv, 
                   col.band = col.surv,
                   
                   # at risk table options
                   atrisk = TRUE, atrisk.lab = 'Number at risk', 
                   atrisk.lines = TRUE, strata.lab = NULL, 
                   strata.order = seq(length(s$n)), 
                   extra.margin = 5, 
                   
                   # aesthetics
                   xlim = c(0, max(s$time)), ylim = c(0, 1),
                   xaxis.at = pretty(s$time), xaxis.lab = xaxis.at, 
                   yaxis.at = pretty(ylim), yaxis.lab = yaxis.at, 
                   xlab = 'Time', ylab = 'Survival probability', 
                   main = '', cex.axis = 1, 
                   legend = !is.null(s$strata), legend.pos = 'bottomleft', 
                   
                   # other options
                   grid = TRUE, lty.grid = 1, lwd.grid = 1, 
                   col.grid = grey(.9),
                   dev = TRUE, add = FALSE, ...) {
  
  #### to do:
  #
  
  ## error checks
  if (!inherits(s, 'survfit')) 
    stop('s must be a survfit object')
  
  ## save current par settings
  op <- par(no.readonly = TRUE)
  
  if (dev) {
    if (grepl('apple', sessionInfo()$platform))
      quartz()
    else if (grepl('mingw', sessionInfo()$platform))
      windows()
    else 
      x11()
  }
  
  ng0 <- length(s$strata)
  ng <- max(ng0, 1) 
  
  ## single strata
  if (ng0 == 0) {
    s$strata <- length(s$time)
    names(s$strata) <- 'All'
    legend <- atrisk.lines <- FALSE
  } 
  
  lty.surv <- rep(lty.surv, ng)
  lwd.surv <- rep(lwd.surv, ng)
  col.surv <- rep(col.surv, ng)
  
  lty.ci <- rep(lty.ci, ng)
  lwd.ci <- rep(lwd.ci, ng)
  col.ci <- rep(col.ci, ng)
  
  col.band <- rep(col.band, ng)
  
  ## color lines in table/legend if col.surv is undifferentiable 
  ## between strata by using band color
  if (is.null(col.band))
    col.band <- NA
  if (all(sapply(1:(length(col.surv) - 1), function(x) 
    identical(col.surv[x], col.surv[x + 1]))) && !(is.na(col.band)))
    col.lines <- col.band
  else 
    col.lines <- col.surv
  
  ## group names and more error checks
  gr <- c(s$strata)
  if(is.null(strata.lab))
    strata.lab <- names(s$strata)
  if(length(unique(strata.lab)) != ng)
    stop('\n','length(unique(strata.lab)) != number of groups')
  if(suppressWarnings(any(sort(strata.order) != 1:ng)))
    stop('\n', 'sort(strata.order) must equal 1:', ng)
  strata.lab <- gsub(' *$', '', strata.lab)  # remove white space
  if (ng == 1 & (strata.lab[1] == 'strata.lab')) {
    strata.lab <- 'Number at risk'
    atrisk.lab <- ifelse(is.null(atrisk.lab), strata.lab, atrisk.lab)
  }
  
  ## graphic parameters
  par(mar = c(4 + ng, 4 + extra.margin, 4, 2) + .1)
  
  if (!add) {
    par(list(mar = c(4 + ng, 4 + extra.margin, 4, 2) + .1,
             oma = c(1, 1, 1, 1)))
    if (!atrisk) 
      par(mar = c(3, 4, 2, 1) + .1)
    par(list(...))
  }
  
  ## reformat survival estimates
  dat <- with(s, data.frame(time = time, 
                            n.risk = n.risk, 
                            n.event = n.event, 
                            survival = surv, 
                            std.err = std.err, 
                            lower = lower, 
                            upper = upper, 
                            group = rep(strata.lab, gr),
                            order = rep(1:ng, gr)))
  dat.list <- split(dat, f = dat$order)
  
  ## plot (but not survival curves) 
  plot(0, type = 'n', xlim = xlim, ylim = ylim, 
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  box(bty = par('bty'))
  if (grid) {
    par('xpd' = FALSE)
    abline(v = xaxis.at, lty = lty.grid, lwd = lwd.grid, col = col.grid)
    abline(h = pretty(c(0, 1)), lty = lty.grid, lwd = lwd.grid, col = col.grid)
  }
  axis(side = 2, at = yaxis.at, las = 1, 
       labels = yaxis.lab, cex.axis = cex.axis)
  axis(side = 1, at = xaxis.at, 
       labels = xaxis.lab, line = -0.5, 
       tick = FALSE, cex.axis = cex.axis)
  axis(side = 1, at = xaxis.at, 
       labels = rep('', length(xaxis.at)), 
       line = 0, tick = TRUE)
  title(xlab = xlab, line = 1.5, adj = .5, ...)
  title(ylab = ylab, ...)
  title(main = main, ...)
  
  ## at risk table below surv plot
  if (atrisk) {
    
    ## write group names
    group.name.pos <- (par()$usr[2] - par()$usr[1]) / -8
    padding <- abs(group.name.pos / 8)
    line.pos <- (1:ng)[order(strata.order)] + 2
    mtext(strata.lab, side = 1, line = line.pos, at = group.name.pos, 
          adj = 1, col = 1, las = 1, cex = cex.axis)
    
    ## draw matching lines for n at risk  
    if (atrisk.lines) {  
      par('xpd' = TRUE)
      for(i in 1:ng) {
        axis(side = 1, at = c(group.name.pos + padding, 0 - 2 * padding), 
             labels = FALSE, line = line.pos[i] + 0.6, lwd.ticks = 0,
             col = col.lines[i], lty = lty.surv[i], lwd = lwd.surv[i])
      }
    }
    
    ## numbers at risk
    ss <- summary(s, times = xaxis.at)
    if (is.null(ss$strata)) 
      ss$strata <- rep(1, length(ss$time))
    d1 <- with(ss, data.frame(time = time, 
                              n.risk = n.risk, 
                              strata = c(strata)))
    d2 <- split(d1, f = d1$strata)
    
    ## right-justify numbers 
    ndigits <- lapply(d2, function(x) nchar(x[ , 2]))
    max.len <- max(sapply(ndigits, length))
    L <- do.call(rbind, lapply(ndigits, function(z) { 
      length(z) <- max.len
      z
    }))
    nd <- apply(L, 2, max, na.rm = TRUE)
    for (i in seq(ng) ) {
      tmp <- d2[[i]] 
      w.adj <- strwidth('0', cex = cex.axis, 
                        font = par('font')) / 2 * nd[1:nrow(tmp)]
      mtext(side = 1, at = tmp$time + w.adj, text = tmp$n.risk, 
            line = line.pos[i], cex = cex.axis, adj = 1, col = 1, las = 1)
    }
    if (!is.null(atrisk.lab)) 
      mtext(side = 1, text = atrisk.lab, at = group.name.pos, 
            line = 1.5, adj = 1, col = 1, las = 1, cex = cex.axis)
  } ## /if (atrisk)  
  
  ## legend
  rlp <- strata.order
  if (legend) {
    bgc <- ifelse(par('bg') == 'transparent', 'white', par('bg'))
    legend(x = legend.pos, legend = strata.lab[rlp], col = col.lines[rlp], 
           lty = lty.surv[rlp], lwd = lwd.surv[rlp], bty = 'o', cex = cex.axis,
           bg = bgc, box.col = 'transparent', inset = .01)
  }
  
  ## confidence intervals
  tcol <- function(color, trans = 100) {
    # helper function for transparent colors
    # see ?rawr::tcol for details
    if (length(color) != length(trans) & 
          !any(c(length(color), length(trans)) == 1)) 
      stop('Vector lengths not correct')
    if (length(color) == 1 & length(trans) > 1) 
      color <- rep(color, length(trans))
    if (length(trans) == 1 & length(color) > 1) 
      trans <- rep(trans, length(color))
    num2hex <- function(x) {
      hex <- unlist(strsplit('0123456789ABCDEF', split = ''))
      return(paste0(hex[(x - x %% 16) / 16 + 1], hex[x %% 16 + 1]))}
    rgb <- rbind(col2rgb(color), trans)
    res <- paste0('#', apply(apply(rgb, 2, num2hex), 2, paste, collapse = ''))
    return(res)
  }
  
  for (i in 1:ng) {
    tmp <- dat.list[[i]]
    x <- tmp$time
    L <- tmp$lower
    U <- tmp$upper
    S <- tmp$survival
    naL <- which(is.na(L))
    L[naL] <- L[naL - 1]
    U[naL] <- U[naL - 1]
    lines(x, L, type = 's', col = col.ci[i], lty = lty.ci[i], lwd = lwd.ci[i])
    lines(x, U, type = 's', col = col.ci[i], lty = lty.ci[i], lwd = lwd.ci[i])
    
    ## confidence bands
    if (!is.null(col.band))
      col.band <- tcol(col.band)
    
    polygon(c(x, rev(x)), c(U, rev(L)), border = NA,
            col = col.band[i])
    
    ## survival curves
    lines(s[i], conf.int = FALSE, col = col.surv[i], lty = lty.surv[i], 
          lwd = lwd.surv, mark = mark, xpd = FALSE )
  }
  
  # reset par settings
  if (!add)
    par(op)
}

#' Survival curves with ggplot
#' 
#' Function to plot Kaplan-Meier or Cox proportional hazards plots with 
#' optional at risk table using \code{\link[ggplot2]{ggplot}}
#'   
#' @usage 
#' ggsurv(s, 
#'        # basic plot options
#'        col.surv = NULL, lty.surv = 1,
#'        censor = TRUE, col.cens = NULL, mark = 3, 
#'       
#'        # confidence options
#'        confin = TRUE, confband = FALSE, col.band = NULL, 
#'              
#'        # extra plot options
#'        median = FALSE, atrisk = TRUE, col.atrisk = NULL, 
#'        pval = NULL, basehaz = FALSE,
#'              
#'        # aesthetics
#'        ticks = NULL, median.ticks = TRUE,
#'        xlab = NULL, ylab = NULL, main = NULL, 
#'        xlim = NULL, ylim = NULL,
#'        legend = 'right', legend.labels = NULL, 
#'        grid = TRUE, ggdefault = FALSE,
#'              
#'        # other options
#'        plot.margin = NULL, table.margin = NULL, 
#'        data = FALSE, ...)
#' 
#' @param s \code{\link{survfit}} or \code{\link{survfit.cox}} object
#' @param col.surv color of survival lines; should be one color or match 
#' number of strata
#' @param lty.surv line type used for survival line; default is 1 (solid line)
#' @param censor logical; if \code{TRUE}, plots censored observations (default)
#' @param col.cens color for censore
#' @param mark plotting character for censored observations
#' @param confin logical; plot confidence bounds around survival estimate
#' @param confband logical; plot confidence band; note that this is not a 
#' confidence band in the statistical sense; see details
#' @param col.band band colors; if \code{NULL}, uses default ggplot colors 
#' (default); should be one color or match number of strata
#' @param median logical; if \code{TRUE}, plots line corresponding to median 
#' survival (inherits \code{col.surv})
#' @param atrisk logical; if \code{TRUE}, adds table with number of at-risk
#' observatons at each \code{tick} timepoint; color of text inherits from 
#' \code{col.surv}
#' @param col.atrisk optional color for at risk text, e.g., 'black'
#' @param pval two-element numeric vector corresponding to x- and y-coordinates
#' to plot a p-value; p-value based on log-rank test for significant 
#' differences in Kaplan-Meier curves (see \code{\link{survdiff}}); if 
#' \code{NULL}, no test is performed (default)
#' @param basehaz logical; if \code{TRUE}, returns baseline survival curve of a 
#' \code{\link[survival]{coxph}} object; see \code{\link[survival]{basehaz}}
#' @param ticks three-element numeric vector corresponding to the "from," "to,"
#' and "by" arguments of \code{seq}, respectively; if \code{NULL}, \code{ticks}
#' will be the default values; if \code{NULL} and \code{atrist = TRUE}, 
#' defaults to \code{seq(0, max(time), length.out = 10)}
#' @param median.ticks logical; if \code{TRUE}, tick labels will be shown for 
#' median survival times (user should provide \code{ticks} argument to avoid
#' overlapping labels); if \code{atrisk = TRUE}, plot will also provide number
#' at risk at median survival time(s)
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param main title
#' @param xlim two-element numeric vector of x-axis range; see 
#' \code{\link[ggplot2]{xlim}}; if \code{NULL}, default is used
#' @param ylim two-element numeric vector of y-axis range; see 
#' \code{\link[ggplot2]{ylim}}; if \code{NULL}, default is used
#' @param legend legend position (no legend if no strata present); takes values
#' of \code{TRUE}, \code{FALSE}, "none," "right," "left," "top," "bottom," or 
#' two-element numeric vector
#' @param legend.labels labels to use for strata in legend; defaults are made 
#' by \code{\link[survival]{survfit}}
#' @param grid logical; if \code{TRUE}, grid lines are drawn at major and 
#' minor ticks
#' @param ggdefault logical; use default ggplot background; if \code{FALSE}, 
#' white background is used (default)
#' @param plot.margin numeric; extra "lines" added to left margin of plot; see
#' details
#' @param table.margin numeric; extra "lines" added to left margin of at risk
#' table; see details
#' @param data logical; if \code{TRUE}, saves data frame used to create graphs
#' @param ... for backwards compatibility with deprecated arguments
#' 
#' @details
#' The argument \code{confband = TRUE} does not plot a confidence band in the 
#' statistical sense, i.e., xx-percent chance of containing entire population 
#' of the survival curve which are wider than the point-wise confidence limits.
#' Rather, it refers to a band of color in \code{ggplot} objects, specifically 
#' the use of a \code{\link[ggplot2]{geom_ribbon}} geometric shape. The band is
#' bounded by the confidence limits calculated in the \code{survfit} object
#' which is passed to \code{ggsurv} in the initial function call.
#' 
#' Long strata labels can mis-align the at risk numbers and plot ticks. If the
#' arguments \code{plot.margin} and \code{table.margin} are \code{NULL}, the
#' function will make a guess based on the number of characters in the strata
#' labels. If this is not perfect, \code{plot.margin} and \code{table.margin}
#' can be specified explicitly by providing a single numeric corresponding to
#' the number of "lines" of padding (see \code{\link{unit}}). Note that the 
#' default for \code{ggplot} is \code{unit(.25, "lines")}.
#' 
#' @seealso \code{\link[rawr]{kmplot}}; \code{survival:::plot.survfit}
#'
#' @examples
#' library(survival)
#' data(cancer)
#' 
#' ### data to use
#' cancer <- within(cancer, {
#'   age.cat <- factor(as.numeric(cut(age, c(-Inf, 50, 60, 70, Inf))))
#'   meal.cat <- factor(as.numeric(cut(meal.cal, 
#'     c(-Inf,quantile(meal.cal, c(.25,.5,.75), na.rm = TRUE), Inf))))
#'   wt.cat <- factor(as.numeric(cut(wt.loss, c(-Inf,quantile(wt.loss, 
#'     c(.25,.5,.75), na.rm = TRUE),Inf))))
#' })
#' 
#' ### fitting models
#' # kaplan-meier
#' kmfit0 <- survfit(Surv(time = time, event = status) ~ 1, data = cancer, 
#'   conf.type = 'log-log')
#' kmfit1 <- survfit(Surv(time = time, event = status) ~ sex, data = cancer, 
#'   conf.type = 'log-log')
#' # cox proportional hazards
#' coxfit0 <- survfit(coxph(Surv(time = time, event = status) ~ strata(age.cat),
#'   data = cancer))
#' coxfit1 <- survfit(coxph(Surv(time = time, event = status) ~ strata(I(age > 45)),
#'   data = cancer))
#' 
#' ### example plots
#' library(ggplot2)
#' ggsurv(kmfit0)
#' 
#' ggsurv(kmfit1, confin = FALSE, lty.surv = 1:2, col.cens = 'blue', 
#'   grid = FALSE, pval = c(500, .75))
#' 
#' ggsurv(kmfit1, confin = FALSE, lty.surv = 1:2, col.cens = 'red', 
#'   confband = TRUE, col.band = c('blue','red'), 
#'   legend.labels = c('Male', 'Female'), median = TRUE, ticks = c(0, 1000, 200))
#'   
#' ggsurv(coxfit0, basehaz = TRUE)
#' 
#' # this long label mis-aligns the table numbers, so we can use plot.margin
#' # to adjust; it may be easier to adjust plot.margin instead of table.margin
#' ggsurv(coxfit0, confin = FALSE, col.atrisk = 'black',
#'   col.surv = c('red','green','blue','black'),
#'   legend.labels = c('Less than 50','50-60','60-70','70+'),
#'   plot.margin = 3)
#' 
#' \dontrun{
#' png('./plot.png', height = 600, width = 750)
#' ggsurv(coxfit1, confin = FALSE, median = TRUE, confband = FALSE,
#'   legend.labels = c('< 45','> 45'), 
#'   col.surv = c('red','green'), mark = '#', col.cens = 'black', 
#'   legend = FALSE)
#' dev.off()
#' }
#' @export

ggsurv <- function(s, 
                   # basic plot options
                   col.surv = NULL, lty.surv = 1,
                   censor = TRUE, col.cens = NULL, mark = 3,
                   
                   # confidence options
                   confin = TRUE, confband = FALSE, col.band = NULL, 
                   
                   # extra plot options
                   median = FALSE, atrisk = TRUE, col.atrisk = NULL,
                   pval = NULL, basehaz = FALSE,
                   
                   # aesthetics
                   ticks = NULL, median.ticks = TRUE,
                   xlab = NULL, ylab = NULL, main = NULL, 
                   xlim = NULL, ylim = NULL,
                   legend = 'right', legend.labels = NULL, 
                   grid = TRUE, ggdefault = FALSE,
                   
                   # other options
                   plot.margin = NULL, table.margin = NULL,
                   data = FALSE, ...) {
  
  ## to do:
  # y axis ticks
  # specific axes ticks?, eg, at = c(.5, 1, 4, 10)
  # ticks labels with atrisk and median?
  
  require(ggplot2)
  require(survival)
  
  #### error checks
  if (!inherits(s, 'survfit')) 
    stop('s must be a survfit object')
  if (basehaz && !inherits(s, 'survfit.cox')) 
    stop('s must be a survfit.cox object')
  if (confin & confband) 
    warning('choose confidence interval or confidence band')
  if (atrisk && is.null(ticks)) 
    message('ticks not specified: \ntick marks defaulting to seq(0, max(time), length.out = 10)\n')
  
  #### allow for backwards compatibility
  m <- match.call(expand.dots = FALSE)
  if (!is.null(m$...)) {
    depr <- c('surv.col','surv.lty','cens.col',
              'cens.shape','band.col','atrisk.col')
    if (any(depr %in% names(m$...))) {
      warning('\n NOTE: using deprecated arguments: ', 
              paste(depr[depr %in% names(m$...)], collapse = ' '))
    }
    if (!is.null(m$...) && !all(names(m$...) %in% depr))
        warning('\n NOTE: unused arguments: ', 
                paste(names(m$...)[names(m$...) %ni% depr], collapse = ' '))
    
    try(list(if (!is.null(m$...$surv.col)) col.surv <- eval(m$...$surv.col),
             if (!is.null(m$...$surv.lty)) lty.surv <- eval(m$...$surv.lty),
             if (!is.null(m$...$cens.col)) col.cens <- eval(m$...$cens.col),
             if (!is.null(m$...$cens.shape)) mark <- eval(m$...$cens.shape),
             if (!is.null(m$...$band.col)) col.band <- eval(m$...$band.col),
             if (!is.null(m$...$atrisk.col)) col.atrisk <- eval(m$...$atrisk.col)), 
        silent = TRUE)
  }
  
  #### create data from survfit object in proper format for ggplot
  survdat <- function(s) {
    survdat <- NULL
    if (is.null(s$strata)) {   
      # if no strata are defined
      # initiate 100% survival at time == 0
      survdat <- data.frame(time = c(0, s$time),
                            n.risk = c(s$n, s$n.risk),
                            n.event = c(0, s$n.event),
                            n.censor = c(0, s$n.censor),
                            surv = c(1, s$surv),
                            lower = c(1, s$lower),  
                            upper = c(1, s$upper))
    } else {
      # if there is one or more strata
      surv.strata <- NULL
      for (i in 1:length(s$strata)) 
        surv.strata <- c(surv.strata, rep(names(s$strata)[i], s$strata[i]))
      survdat <- data.frame(time = s$time, 
                            n.risk = s$n.risk, 
                            n.event = s$n.event, 
                            n.censor = s$n.censor, 
                            surv = s$surv,
                            lower = s$lower, 
                            upper = s$upper, 
                            strata = factor(surv.strata))
      # initiate 100% survival at time == 0
      tmp <- NULL
      for (i in 1:length(s$strata)) {
        tmp0 <- subset(survdat, strata == names(s$strata)[i])
        tmp1 <- data.frame(time = 0, 
                           n.risk = s[i]$n, 
                           n.event = 0, 
                           n.censor = 0,
                           surv = 1,
                           lower = 1,
                           upper = 1,
                           strata = names(s$strata)[i])
        tmp1 <- rbind(tmp1, tmp0)
        tmp <- rbind(tmp, tmp1)
      }
      survdat <- tmp[order(tmp$strata, tmp$time), ]
      rownames(survdat) <- NULL   
    }
    return(survdat)
  }
  
  survdat <- survdat(s)
  
  #### save data
  if (data) { 
    assign(paste(deparse(substitute(s)), 'data', sep = '.'), survdat, envir = .GlobalEnv)
    cat(paste(deparse(substitute(s)), 'data', sep = '.'), ' created')
  }
  
  #### for custom ribbon color with no strata present
  if (is.null(survdat$strata) && !is.null(col.band)) survdat$col.band <- col.band
  
  #### change levels in strata
  if (!is.null(survdat$strata) && !(is.null(legend.labels))) {
    if (length(unique(levels(survdat$strata))) != length(legend.labels)) {
      warning('legend labels not equal to number of strata')
    }
    recodes <- list(original = levels(survdat$strata),
                    replaced = legend.labels)
    cat('\nstrata labels recoded as follows:\n\n')
    print(do.call(data.frame, recodes))
    if (inherits(res <- try(rawr::recoder), 'try-error')) {
      survdat$strata <- factor(survdat$strata, 
                               levels = levels(survdat$strata),
                               labels = legend.labels)
    } else { 
      survdat$strata <- rawr::recoder(object = survdat$strata, 
                                      pattern = levels(survdat$strata),
                                      replacement = legend.labels)
    }
    survdat$strata <- droplevels(survdat$strata)
  }
  
  #### graph with no strata ####
  
  if (is.null(survdat$strata)) {
    if (length(col.band) > 1) warning('more colors chosen than bands')
    if (length(col.surv) > 1) warning('more colors chosen than lines')
    
    if (is.null(col.surv)) col.surv <- 'black'
    
    # step plot
    tmp <- ggplot(data = survdat, aes(x = time, y = surv)) 
    if (is.null(col.surv)) {
      tmp <- tmp + geom_step(colour = 'black', lty = lty.surv, direction = 'hv')
    } else {
      tmp <- tmp + geom_step(colour = col.surv, lty = lty.surv, direction = 'hv')
    }
    # add censored observations
    if (censor) {
      if (is.null(col.cens) & !is.null(col.surv)) {
        tmp <- tmp + geom_point(data = subset(survdat, n.censor > 0), aes(x = time, y = surv), colour = col.surv, shape = mark) 
      } else { 
        if (is.null(col.cens) & is.null(col.surv)) {
          tmp <- tmp + geom_point(data = subset(survdat, n.censor > 0), aes(x = time, y = surv), colour = 'black', shape = mark)
        } else {
          tmp <- tmp + geom_point(data = subset(survdat, n.censor > 0), aes(x = time, y = surv), colour = col.cens, shape = mark)
        }
      }
    }
    # add confidence interval
    if (confin) {
      tmp <- tmp + 
        geom_step(aes(x = time, y = upper), direction = 'hv', linetype = 2, colour = col.surv) + 
        geom_step(aes(x = time, y = lower), direction = 'hv', linetype = 2, colour = col.surv)
    }
    # add confidence band
    if (confband) {
      if (is.null(survdat$col.band)) {
        tmp <- tmp + geom_ribbon(aes(x = time, ymax = upper, ymin = lower), directions = 'hv',alpha = 0.25)
      } else {
        tmp <- tmp + geom_ribbon(aes(x = time, ymax = upper, ymin = lower, fill = col.band), direction = 'hv', alpha = 0.25) + 
          scale_fill_manual(values = col.band)
      }
    }
    # median survival line
    if (median) {
      options(survfit.rmean = 'individual')
      tmp.med <- summary(s)$table['median']
      cat('median survival time:',tmp.med)
      tmp.med <- data.frame(time = rep(unname(tmp.med), 2), 
                            quant = c(.5, 0))
      tmp.med <- subset(tmp.med, !is.na(time))
      if (nrow(tmp.med) == 0) {
        cat('\nmedian survival not reached\n')
      } else {
        tmp <- tmp + geom_line(data = tmp.med, aes(x = time, y = quant), colour = col.surv, linetype = 3) + 
          geom_point(data = tmp.med, aes(x = time, y = quant), colour = col.surv)
      }
    }
    
  } else { 
    
    #### graph with strata ####
    
    # make sure options are compatible
    # line colors
    col.survs <- if (length(col.surv == 1)) {
      scale_colour_manual(values = rep(col.surv, length(unique(survdat$strata))))
    } else {
      scale_colour_manual(values = col.surv)
    } 
    if (is.null(col.surv)) col.survs <- NULL
    # line types
    lty.survs <- if (length(lty.surv == 1)) {
      scale_linetype_manual(values = rep(lty.surv, length(unique(survdat$strata))))
    } else {
      scale_linetype_manual(values = lty.surv)
    }
    # censor shapes
    marks <- rep(mark, times = length(unique(survdat$strata)))
    
    # step plot
    tmp <- ggplot(data = survdat, aes(x = time, y = surv, group = strata, colour = strata)) +
      geom_step(aes(colour = strata, group = strata, linetype = strata), direction = 'hv') +
      col.survs + lty.survs
    # add censored observations
    if (censor) {
      if (is.null(col.cens)) {
        tmp <- tmp + geom_point(data = subset(survdat, n.censor > 0), aes(x = time, y = surv, colour = strata, group = strata), shape = mark) 
      } else { 
        tmp <- tmp + geom_point(data = subset(survdat, n.censor > 0), aes(x = time, y = surv, colour = strata, group = strata, shape = strata), colour = col.cens) + 
          scale_shape_manual(values = marks)
      }
    }
    # add confidence interval
    if (confin) {      
      tmp <- tmp + 
        geom_step(aes(x = time, y = upper), direction = 'hv', linetype = 2, alpha = 0.5) + 
        geom_step(aes(x = time, y = lower), direction = 'hv', linetype = 2, alpha = 0.5)  
    }
    # add confidence band
    if (confband) {
      tmp <- tmp + geom_ribbon(aes(x = time, ymax = upper, ymin = lower, fill = strata), directions = "hv", linetype = 0 ,alpha = 0.25)
      # custom conf band fill colors
      if (!is.null(col.band)) tmp <- tmp + scale_fill_manual(values = rep(col.band, length(unique(survdat$strata))))
    }
    
    # median survival line
    if (median) {
      options(survfit.rmean = 'individual')
      tmp.med <- summary(s)$table[ ,'median']
      cat('median survival times:\n')
      print(tmp.med)
      cols <- if (is.null(col.surv)) {
        rep(ggcols(length(names(tmp.med))), each = 2)
      } else {
        rep(col.surv, each = 2)
      }
      tmp.med <- data.frame(time = rep(tmp.med, each = 2), 
                            quant = rep(c(.5,0), times = length(tmp.med)), 
                            group = rep(names(tmp.med), each = 2),
                            col.surv = cols)
      tmp.med <- subset(tmp.med, !is.na(time))
      if (nrow(tmp.med) == 0) {
        cat('\nmedian survival not reached\n')
      } else {
        tmp <- tmp + geom_line(data = tmp.med, aes(time, quant, group = group), colour = tmp.med$col.surv, linetype = 3) + 
          geom_point(data = tmp.med, aes(time, quant, group = group), colour = tmp.med$col.surv)
      }
    }
  }
  
  tmp <- tmp + xlab('Time') + ylab('Survival')
  
  ### test
  ### plotting baseline hazard instead of survival
  if (basehaz && inherits(s, 'survfit.cox')) {
    tmp.haz <- basehaz(eval(s$call$formula))
    
    if (is.null(tmp.haz$strata)) {
      tmp <- ggplot(data = tmp.haz, aes(x = time, y = hazard)) + 
        geom_line() + xlab('Time') + ylab('Hazard')
    } else {
      tmp <- ggplot(data = tmp.haz, aes(x = time, y = hazard, colour = strata)) + 
        geom_line() + xlab('Time') + ylab('Hazard') + col.survs + lty.survs
    }
  }
  ### / test
  
  # label options
  if (!is.null(xlab)) tmp <- tmp + xlab(xlab)
  if (!is.null(ylab)) tmp <- tmp + ylab(ylab)
  if (!is.null(main)) tmp <- tmp + ggtitle(main)
  
  # background options
  if (ggdefault == FALSE) tmp <- tmp + theme_bw()
  if (grid == FALSE) tmp <- tmp + theme(panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank(),
                                        axis.line = element_line(colour = 'black'))
  
  # need to remove legend if custom band color supplied for no strata plot
  if (is.null(s$strata)) {
    tmp <- tmp + theme(legend.position = 'none')
  } else { 
    if (is.logical(legend)) {
      if (legend == FALSE) legend <- 'none'
      if (legend == TRUE) legend <- 'right'
    }
    tmp <- tmp + theme(legend.position = legend)
  }
  
  if (!is.null(pval)) {
    # log-rank/Mantel-Haenszel (rho = 0)
    sdiff <- survdiff(eval(s$call$formula), data = eval(s$call$data), rho = 0)
    pval.chisq <- pchisq(sdiff$chisq, length(sdiff$n) - 1, lower.tail = FALSE)
    pvaltxt <- ifelse(pval.chisq < 0.001, 'p < 0.001', paste('p =', signif(pval.chisq, 3)))
    tmp <- tmp + annotate('text', x = pval[1], y = pval[2], label = pvaltxt)
    print(sdiff)
  }
  
  ## added labels here
  if (!is.null(ticks)) 
    tmp <- tmp + scale_x_continuous(breaks = seq(ticks[1], ticks[2], by = ticks[3]),
                                    labels = format(seq(ticks[1], ticks[2], by = ticks[3]), nsmall = 0))
  ## here
  if (!is.null(ticks) && median && median.ticks) 
    tmp <- tmp + scale_x_continuous(breaks = sort(c(tmp.med$time, seq(ticks[1], ticks[2], by = ticks[3]))),
                                    labels = format(sort(c(tmp.med$time, seq(ticks[1], ticks[2], by = ticks[3]))), nsmall = 0))
  if (!is.null(xlim)) tmp <- tmp + xlim(xlim)
  if (!is.null(ylim)) tmp <- tmp + ylim(ylim)
  tmp <- tmp + theme(legend.title = element_blank())
  
  if (atrisk) {
    
    require(gridExtra)
    
    # fix legend
    if (!(legend %in% c(FALSE, 'none', 'bottom', 'top'))) {
      legend <- 'bottom'
      message("when atrisk == TRUE, legend should be FALSE, 'none', 'bottom', or 'top'\nposition defaulting to 'bottom'")
    }
    
    # set up tick marks for atrisk alignment
    options(survfit.rmean = 'individual')
    if (is.null(ticks)) {
      tick.seq <- seq(0, max(s$time), length = 10)
      if (median && median.ticks) 
        tick.seq <- sort(c(tmp.med$time, tick.seq))
      tmp <- tmp + scale_x_continuous(breaks = round(tick.seq))
    } else {
      tick.seq <- seq(ticks[1], ticks[2], ticks[3])
      if (median && median.ticks) tick.seq <- sort(c(tmp.med$time, tick.seq))
    }
    n.ticks <- length(tick.seq)
    
    # create data of at risk
    tmp.risk <- summary(s, times = tick.seq)
    risk.table <- data.frame(
      time = tmp.risk$time,
      n.risk = tmp.risk$n.risk)
    if (is.null(s$strata)) {
      risk.table$strata <- '1'
    } else risk.table$strata <- tmp.risk$strata
    
    # reverse order of risk.table by group
    risk.table <- do.call(rbind, 
                          lapply(rev(unique(risk.table$strata)), 
                                 function(x) 
                                   risk.table[risk.table$strata == x, ]))
    risk.table$strata <- factor(risk.table$strata, levels = rev(levels(risk.table$strata)))
    if (is.null(s$strata)) {
      risk.table$strata <- factor(' ')
      survdat$strata <- factor(' ')
    }
    
    # increase margins with long labels in risk.table
    if (is.null(plot.margin))
      plot.margin <- .5 * (max(nchar(levels(survdat$strata))) - 1) - .3
    
    tmp <- tmp + theme(plot.margin = unit(c(.25, .25, .4, plot.margin), 'lines'))
    
    # create table in ggplot
    gg.table <- ggplot(risk.table, aes(x = time, y = strata, colour = strata, label = format(n.risk, nsmall = 0))) + 
      theme_bw() + 
      scale_y_discrete(breaks = as.character(levels(risk.table$strata)), labels = rev(unique(survdat$strata))) +
      #       scale_y_discrete(breaks = as.character(levels(risk.table$strata)), labels = rep('', length(unique(survdat$strata)))) +
      scale_x_continuous('Number at risk', breaks = tick.seq, limits = c(0, max(s$time))) + 
      theme(axis.title.x = element_text(size = 10, vjust = 1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(), axis.text.x = element_blank(),
            axis.ticks = element_blank(), axis.text.y = element_text(face = 'bold', hjust = 1))
    #       annotate('text', x = 0, y = length(unique(risk.table$strata)) + .5, label = 'Number at risk')
    #       ggtitle('Number at risk') 
    #       theme(plot.title = element_text(size = rel(1)))
    if (!is.null(col.atrisk))
      gg.table <- gg.table + geom_text(size = 3.5, colour = col.atrisk)
    else gg.table <- gg.table + geom_text(size = 3.5)
    
    if (is.null(table.margin))
      table.margin <- 2
    gg.table <- gg.table + 
      theme(plot.margin = unit(c(-2, .5, .1, table.margin), 'lines'),
            legend.position = 'none') + xlab(NULL) + ylab(NULL)
    
    if (is.null(col.surv)) {
      gg.table <- gg.table + 
        scale_colour_manual(values = rev(ggcols(length(unique(risk.table$strata)))))
    } else {
      if (length(col.surv) == 1) col.surv <- rep(col.surv, length(unique(survdat$strata)))
      gg.table <- gg.table + 
        scale_colour_manual(values = rev(col.surv))
    }
    
    # blank plot for place-holding
    blank.plot <- ggplot(data = survdat, aes(x = time, y = surv)) +
      geom_blank() + theme_bw() +
      theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(), panel.border = element_blank())
    
    # left/right legend location throws off at risk table alignment
    if (legend != FALSE)
      tmp <- tmp + theme(legend.position = legend)
    if (is.null(s$strata)) 
      tmp <- tmp + theme(legend.position = 'none')
    
    tmp <- arrangeGrob(tmp, blank.plot, gg.table,
                       clip = FALSE, nrow = 3, ncol = 1,
                       heights = unit(c(2, .1, .25), c('null','null','null')))
  }
  
  tmp
}