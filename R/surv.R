### survival stuff
# kmplot, kmplot_by, local_coxph_test, surv_cp, surv_summary, surv_table,
# survdiff_pairs
###


#' Survival curves
#' 
#' Plot Kaplan-Meier or Cox proportional hazards models with at-risk table.
#' 
#' Line specifications (\code{lty.surv}, \code{lwd.surv}, etc) will be
#' recycled if needed.
#' 
#' \code{xaxs} is the style of the x-axis; see \code{\link{par}}. The default
#' for \code{kmplot} is \code{"S"} which is equivalent to \code{xaxs = "i"}
#' but with the maximum \code{xlim} value increased by 4\%. Other styles for
#' \code{xaxs} currently implemented in \code{R} are \code{"r"} (default for
#' plotting and the previous value for \code{kmplot}) and \code{"i"} which
#' will \emph{not} add padding to the ends of the axes.
#' 
#' If \code{col.band != NULL}, a confidence band is plotted; however, this is
#' not a confidence band in the statistical sense, i.e., a xx-percent chance 
#' of containing the entire population of the survival curve which are wider 
#' than the point-wise confidence limits.
#' 
#' Rather, it refers to a band of color plotted between the confidence 
#' limits calculated in the survfit object. That is, the xx-percent 
#' confidence interval (plotted when \code{lty.ci != 0}) and the confidence
#' bands are identical, just two ways of plotting the same invervals.
#' 
#' When saving plots, it is highly recommended to use \code{\link{png}}, 
#' \code{\link{svg}}, \code{\link{pdf}}, etc instead of exporting directly
#' from the \code{R} graphics device. Doing so may cause the at risk table or
#' legend to be mis-aligned.
#' 
#' @param s object of class \code{\link[survival]{survfit}} or 
#' \code{survfit.cox}
#' @param lty.surv line type for survival curve(s); see \code{\link{par}}
#' @param lwd.surv line width for survival curve(s)
#' @param col.surv line color for survival curve(s); either numeric or 
#' character string(s); hexadecimal format also works
#' @param mark numeric plotting character (\code{\link{pch}}) or character 
#' string, i.e., \code{''}, \code{'|'}, etc
#' @param mar margins; see \code{mar} section in \code{\link{par}}
#' @param lty.ci line type for confidence interval(s); not plotted (\code{= 0})
#' by default
#' @param lwd.ci line width for confidence interval(s)
#' @param col.ci line color for confidence interval(s); either numeric or 
#' character string(s); hexadecimal format also works
#' @param col.band line color for confidence band(s); either \code{NULL}, 
#' numeric, or character string(s); hexadecimal format also works; note that 
#' this is not a true confidence band; see details
#' @param atrisk logical; if \code{TRUE}, draws at risk table
#' @param atrisk.lab heading for at risk table
#' @param atrisk.lines logical; draw lines next to strata in at risk table
#' @param strata.lab labels used in legend and at risk table for strata; if 
#' \code{NULL} (default), labels created in \code{survfit} are used; if only
#' one strata is present, "All" is used by default; if \code{FALSE}, labels
#' are not used
#' @param strata.expr an alternative to \code{strata.lab} which allows for 
#' \code{\link{bquote}} or \code{\link{expression}} to be passed to labels for
#' at risk table; note that \code{strata.expr} trumps \code{strata.lab}
#' @param strata.order order of strata in legend and at risk table
#' @param extra.margin increase left margin when strata labels in at risk table
#' are long
#' @param xaxs style of axis; see details
#' @param xlim,ylim x- and y-axis limits
#' @param xaxis.at,yaxis.at positions for x- and y-axis labels and ticks
#' @param xaxis.lab,yaxis.lab x- and y-axis tick labels
#' @param xlab,ylab x-axis label
#' @param main title of plot
#' @param cex.axis text size for axes labels, legend, at risk table
#' @param legend logical; draw legend
#' @param legend.pos legend position
#' @param grid logical; draw grid in background
#' @param lty.grid line type for grid
#' @param lwd.grid line width for grid
#' @param col.grid line color for grid
#' @param add logical; if \code{TRUE}, \code{par} is not refreshed; allows for
#' multiple panels, e.g., when using \code{par(mfrow = c(1, 2))}
#' @param panel.first an expression to be evaluated after the plot axes are
#' set up but before any plotting takes place
#' @param panel.last an expression to be evaluated after plotting but before
#' returning from the function
#' @param ... additional parameters (\code{font}, \code{mfrow}, \code{bty}, 
#' \code{tcl}, \code{cex.lab}, \code{xaxs}, etc) passed to \code{par}
#' 
#' @references
#' \url{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}
#' 
#' @seealso
#' \code{\link[plotr]{ggsurv}}; \code{survival:::plot.survfit};
#' \code{\link{kmplot_by}}
#' 
#' @examples
#' \dontrun{
#' library('survival')
#' kmfit1 <- survfit(Surv(time, status) ~ sex, data = colon)
#' kmfit2 <- survfit(Surv(time, status) ~ rx + adhere, data = colon)
#' 
#' ## simple example, draw in r window
#' kmplot(kmfit1)
#' 
#' ## expressions in at risk table
#' kmplot(kmfit1, strata.lab = c('Female','Male'),
#'        strata.expr = expression(widetilde(ring(Female)), 
#'                                 phantom() >= Male))
#' 
#' ## using mfrow options, use ADD = TRUE
#' png('kmplot2.png', width = 750, height = 1200, pointsize = 14)
#' par(mfrow = c(2, 1))
#' kmplot(kmfit1, add = TRUE)
#' kmplot(kmfit2, add = TRUE, extra.margin = 8)
#' dev.off()
#' 
#' ## more complex example
#' pdf('tmp.pdf', height = 8, width = 11, pointsize = 12)
#' kmplot(kmfit2, 
#'        mark = '',                         # no censor mark
#'        lty.ci = 2,                        # dashed line for CIs
#'        xaxis.at = c(0, .5, 1:9) * 365,    # change days to years
#'        xaxis.lab = c(0, .5, 1:9),         # label years
#'        yaxis.lab = pretty(c(0, 1)) * 100, # change to percent
#'        xlab = 'Time (years)', ylab = 'Percent survival', 
#'        col.surv = c('blue', 'red', 'green', 'black','purple','orange'), 
#'        strata.lab = c('Obs ','Obs+ ','Lev ','Lev+ ','Lev5fu','Lev5fu+'),
#'        extra.margin = 6,        # increase margin for long strata labels
#'        strata.order = c(5, 6, 3, 1, 4, 2), 
#'        # col.band = NULL,       # remove confidence bands
#'        font = 2,                # bold table text
#'        bty = 'l',               # L box type around plot
#'        tcl = .5)                # change length/direction of ticks
#' title(main='Chemotherapy for stage B/C colon cancer', 
#'       adj = .5, font.main = 1, line = 0.5, cex.main = 1)
#' dev.off()
#' }
#' @export

kmplot <- function(s,
                   ## basic plot options
                   lty.surv = 1, lwd.surv = 1,
                   col.surv = seq_along(s$strata) %||% 1,
                   mark = 3, mar = NULL,
                   
                   ## confidence options
                   lty.ci = 0, lwd.ci = 1, col.ci = col.surv,
                   col.band = col.surv,
                   
                   ## at risk table options
                   atrisk = TRUE, atrisk.lab = 'Number at risk',
                   atrisk.lines = TRUE, strata.lab = NULL,
                   strata.expr = NULL,
                   strata.order = seq(length(s$n)),
                   extra.margin = 5,
                   
                   ## aesthetics
                   xaxs = 'S',
                   xlim = c(0, max(s$time)), ylim = c(0, 1),
                   xaxis.at = pretty(s$time), xaxis.lab = xaxis.at,
                   yaxis.at = pretty(ylim), yaxis.lab = yaxis.at,
                   xlab = 'Time', ylab = 'Survival probability',
                   main = '', cex.axis = 1,
                   legend = !is.null(s$strata), legend.pos = 'bottomleft',
                   
                   ## other options
                   grid = FALSE, lty.grid = 1, lwd.grid = 1,
                   col.grid = grey(.9),
                   add = FALSE, panel.first = NULL, panel.last = NULL, ...) {
  
  ## error checks
  if (!inherits(s, 'survfit')) 
    stop('s must be a \'survfit\' object')
  
  ## save current par settings
  op <- par(no.readonly = TRUE)
  
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
  col.lines <- if (all(sapply(1:(length(col.surv) - 1), function(x)
    identical(col.surv[x], col.surv[x + 1]))) && !(is.na(col.band)))
    col.band else col.surv
  # if (any(is.na(col.band)))
  #   col.lines <- ifelse(is.na(col.band), col.surv, col.band)
  
  ## test:
  ## atrisk lines inherit from col.surv if is.na(col.band)
  ## but inherits col.band if given and not NA
  ## kmplot(kmfit1, dev = FALSE, col.band = c('red',NA), col.surv = 'black')
  
  ## group names and more error checks
  gr <- c(s$strata)
  if (!is.null(strata.lab) && strata.lab == TRUE)
    strata.lab <- NULL
  if (!is.null(strata.lab) && strata.lab == FALSE)
    strata.lab <- rep(FALSE, ng)
  if (is.null(strata.lab))
    strata.lab <- names(s$strata)
  if (length(unique(strata.lab)) != ng && strata.lab[1] != FALSE)
    stop('\n','length(unique(strata.lab)) != number of groups')
  if (suppressWarnings(any(sort(strata.order) != 1:ng)))
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
  if (!is.null(mar)) 
    par(mar = mar)

  ## as in plot.survfit to adjust x-axis to start at 0
  if (xaxs == 'S') {
    xaxs <- 'i'
    xlim[2] <- xlim[2] * 1.04
  } else xaxs <- 'r'
  
  ## reformat survival estimates
  dat <- with(s, data.frame(time = time, n.risk = n.risk, n.event = n.event,
                            survival = surv, std.err = std.err, lower = lower,
                            upper = upper, group = rep(strata.lab, gr),
                            order = rep(1:ng, gr)))
  dat.list <- split(dat, f = dat$order)
  
  ## plot (but not survival curves) 
  plot(0, type = 'n', xlim = xlim, ylim = ylim, ann = FALSE, bty = 'n',
       xaxt = 'n', yaxt = 'n', xaxs = xaxs)
  panel.first
  box(bty = par('bty'))
  if (grid) {
    par(xpd = FALSE)
    abline(v = xaxis.at, lty = lty.grid, lwd = lwd.grid, col = col.grid)
    abline(h = pretty(c(0, 1)), lty = lty.grid, lwd = lwd.grid, col = col.grid)
  }
  axis(side = 2, at = yaxis.at, las = 1,
       labels = yaxis.lab, cex.axis = cex.axis)
  axis(side = 1, at = xaxis.at, labels = xaxis.lab, line = -0.5, 
       tick = FALSE, cex.axis = cex.axis)
  axis(side = 1, at = xaxis.at, labels = rep('', length(xaxis.at)), 
       line = 0, tick = TRUE)
  title(xlab = xlab, line = 1.5, adj = .5, ...)
  title(ylab = ylab, main = main, ...)
  
  ## at risk table below surv plot
  if (atrisk) {
    ## write group names
    group.name.pos <- diff(par('usr')[1:2]) / -8
    padding <- abs(group.name.pos / 8)
    line.pos <- (1:ng)[order(strata.order)] + 2
    if (strata.lab[1] != FALSE) {
      if (!is.null(strata.expr))
        sapply(1:length(strata.expr), function(x)
          mtext(strata.expr[[x]], side = 1, line = line.pos[x], adj = 1,
                at = group.name.pos, col = 1, las = 1, cex = cex.axis))
      else mtext(strata.lab, side = 1, line = line.pos, at = group.name.pos,
              adj = 1, col = 1, las = 1, cex = cex.axis)
    }
    
    ## draw matching lines for n at risk  
    if (atrisk.lines) {  
      par(xpd = TRUE)
      for (i in 1:ng) {
        ## mess with the 2 here to adjust the length of the atrisk.line
        axis(side = 1, at = c(group.name.pos + padding, 0 - 2 * padding),
             labels = FALSE, line = line.pos[i] + 0.6, lwd.ticks = 0,
             col = col.lines[i], lty = lty.surv[i], lwd = lwd.surv[i])
      }
    }
    
    ## numbers at risk
    ss <- summary(s, times = xaxis.at)
    if (is.null(ss$strata))
      ss$strata <- rep(1, length(ss$time))
    d1 <- with(ss,
               data.frame(time = time, n.risk = n.risk, strata = c(strata)))
    d2 <- split(d1, f = d1$strata)
    
    ## right-justify numbers 
    ndigits <- lapply(d2, function(x) nchar(x[, 2]))
    max.len <- max(sapply(ndigits, length))
    L <- do.call(rbind, lapply(ndigits, function(z) {
      length(z) <- max.len
      z
    }))
    nd <- apply(L, 2, max, na.rm = TRUE)
    for (i in seq(ng) ) {
      tmp <- d2[[i]]
      w.adj <- strwidth('0', cex = cex.axis, font = par('font')) /
        2 * nd[1:nrow(tmp)]
      mtext(side = 1, at = tmp$time + w.adj, text = tmp$n.risk,
            line = line.pos[i], cex = cex.axis, adj = 1, col = 1, las = 1)
    }
    if (!is.null(atrisk.lab))
      # mtext(side = 1, text = atrisk.lab, at = group.name.pos,
      #       line = 1.5, adj = 1, col = 1, las = 1, cex = cex.axis)
      mtext(side = 1, text = atrisk.lab, at = par('usr')[1],
            line = 1.5, adj = 1, col = 1, las = 1, cex = cex.axis)
  } ## /if (atrisk)

  ## legend
  rlp <- strata.order
  if (legend) {
    bgc <- ifelse(par('bg') == 'transparent', 'white', par('bg'))
    if (!is.null(strata.expr)) {
      legend(x = legend.pos, legend = strata.expr[rlp], col = col.lines[rlp], 
             lty = lty.surv[rlp], lwd = lwd.surv[rlp], bty = 'o',
             cex = cex.axis, bg = bgc, box.col = 'transparent', inset = .01)
    } else {
      if (strata.lab[1] == FALSE)
        strata.lab <- names(s$strata)
      legend(x = legend.pos, legend = strata.lab[rlp], col = col.lines[rlp],
             lty = lty.surv[rlp], lwd = lwd.surv[rlp], bty = 'o',
             cex = cex.axis, bg = bgc, box.col = 'transparent', inset = .01)
    }
  }

  ## survival and confidence lines
  for (i in 1:ng) {
    tmp <- dat.list[[i]]
    if (nrow(tmp) < 2) {
      message('Note: strata level with one observation - no CI plotted.')
    } else {
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
      if (any(!is.na(col.band))) {
        col.band[i] <- tcol(col.band[i], 100)
        polygon(c(x, rev(x)), c(U, rev(L)), border = NA, col = col.band[i])
      }
    }
    
    ## survival curves
    lines(s[i], conf.int = FALSE, col = col.surv[i], lty = lty.surv[i], 
          lwd = lwd.surv, mark = mark, xpd = FALSE)
  }
  panel.last
  
  ## reset par settings
  if (!add)
    par(op)
  invisible()
}

#' kmplot_by
#' 
#' This function helps create stratified \code{\link{kmplot}}s quickly with
#' panel labels and log-rank tests for the subsets.
#' 
#' The data used should have at least three variables: \code{strata},
#' \code{*_time}, and \code{*_ind} where \code{*} is \code{event}. For
#' example, to use progression-free survival, \code{dat} should have columns
#' \code{"pfs_time"} and \code{"pfs_ind"} and optionally the \code{strata}
#' column unless \code{strata = "1"}.
#' 
#' @param strata character string of the strata variable
#' @param event character string indicating the event (pfs, os, ttp, etc);
#' see details
#' @param data data frame to use
#' @param by optional character string of stratification variable
#' @param single logical; if \code{TRUE}, each level of \code{by} will be
#' drawn in a separate window
#' @param lr_test logical; if \code{TRUE}, a log-rank test will be performed
#' and the results added to the top-right corner of the plot
#' @param ylab y-axis label
#' @param sub sub-title displayed in upper left corner; should be a character
#' vector with length equal to the number of panels (i.e., the number of
#' levels of \code{by} or length one if \code{by} was not given)
#' @param strata_lab at-risk table strata labels; should be a character vector
#' with length equal to the number of strata; otherwise, the variable labels
#' will be removed; see examples
#' @param fig_lab figure panel labels; should be a character vector with
#' length equal to the number of panels (i.e., the number of levels of
#' \code{by} or length one if \code{by} was not given)
#' @param ... additional arguments passed to \code{\link{kmplot}} or
#' graphical parameters subsequently passed to \code{\link{par}}
#' 
#' @return
#' Invisibly returns a list of \code{\link{survfit}} objects for each plot.
#' 
#' @seealso
#' \code{\link{kmplot}}, \code{\link{survdiff}}
#' 
#' @examples
#' library('survival')
#' data(colon)
#' colon <- within(colon[duplicated(colon$id), ], {
#'   pfs_time <- time
#'   pfs_ind <- status
#'   sex <- c('Female','Male')[sex + 1]
#' })
#' 
#' kmplot_by('1', data = colon)
#' 
#' ## return value is a list of survfit objects
#' tmp <- kmplot_by('sex', data = colon, fig_lab = 'Figure I',
#'   strata_lab = c('F','M'), sub = 'PFS, by sex')
#' kmplot(tmp$sex)
#'   
#' kmplot_by('rx', data = colon, col.surv = 1:3,
#'   strata_lab = FALSE, col.band = NA)
#' 
#' ## multiple variables can be combined  
#' kmplot_by('rx + sex', data = colon, strata_lab = '',
#'   lty.surv = 1:6, col.band = NA)
#'
#' ## if "by" is given, default is to plot separately
#' kmplot_by('rx', data = colon, by = 'sex', col.surv = 1:3,
#'   strata_lab = c('Observation','Trt','Trt + 5-FU'))
#' 
#' ## if single = FALSE, uses n2mfrow function to set par('mfrow')
#' kmplot_by('rx', data = colon, by = 'sex', col.surv = 1:3, single = FALSE,
#'   strata_lab = c('Observation','Trt','Trt + 5-FU'))
#'   
#' ## if par('mfrow') is anything other than c(1,1), uses current setting
#' par(mfrow = c(2,2))
#' kmplot_by('rx', data = colon, by = 'sex', col.surv = 1:3, single = FALSE,
#'   strata_lab = c('Observation','Trt','Trt + 5-FU'))
#'   
#' @export

kmplot_by <- function(strata, event = 'pfs', data, by, single = TRUE,
                      lr_test = TRUE, ylab, sub, strata_lab, fig_lab, ...) {
  # on.exit(par(xpd = FALSE))
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  if (!missing(by)) {
    if (single) {
      add <- FALSE
      par(mfrow = c(1,1))
    } else {
      add <- TRUE
      if (all(par('mfrow') == c(1L, 1L)))
        par(mfrow = n2mfrow(length(unique(data[, by]))))
    }
    sp <- split(data, data[, by])
  } else {
    add <- FALSE
    par(mfrow = c(1,1))
    sp <- list(data)
  }
  mlabs <- missing(strata_lab)
  msub <- missing(sub)
  fig <- if (length(sp) > 1 & missing(fig_lab))
    LETTERS[seq_along(sp)] else if (missing(fig_lab)) '' else fig_lab
  ylab <- if (missing(ylab))
    sprintf('%s probability', toupper(event)) else ylab
  
  l <- lapply(seq_along(sp), function(x) {
    form <- as.formula(sprintf('Surv(%s_time, %s_ind) ~ %s',
                               event, event, strata))
    s <- s0 <- survfit(form, data = sp[[x]], conf.type = 'log-log')
    
    if (strata == '1')
      strata <- ''
    names(s$strata) <- if (mlabs)
      names(s$strata) else if (length(strata_lab) == length(s$strata))
        strata_lab else gsub('\\w+=', '', names(s$strata))
    
    kmplot(s, add = add, legend = FALSE, main = names(sp)[x], ylab = ylab, ...,
           panel.first = {
             ## add plot text
             p <- par('usr')
             mtext(if (!msub) sub[x] else strata, side = 3, font = 3,
                   at = 0, line = .5, adj = 0)
             mtext(fig[x], side = 3, at = 0 - p[2] * .05,
                   font = 2, cex = 1.5, line = 1.2)
             
             ## add survdiff text in upper right corner
             if (lr_test && strata != '') {
               sd <- survdiff(form, data = sp[[x]])
               df <- sum(1 * (colSums(if (is.matrix(sd$obs))
                 sd$exp else t(sd$exp)) > 0)) - 1
               pv <- 1 - pchisq(sd$chisq, df)
               txt <- sprintf('%s (%s df), %s', roundr(sd$chisq, 1),
                              df, pvalr(pv, show.p = TRUE))
               txt <- bquote(paste(chi^2, ' = ', .(txt)))
               mtext(txt, side = 3, at = p[2], adj = 1,
                     font = 3, cex = .8, line = .5)
             }
           })
    s0
  })
  names(l) <- names(sp) %||% strata
  invisible(l)
}

#' Compute local p-value from coxph
#' 
#' Checks the null hypothesis: C * beta.hat = c, i.e., the local
#' p-value of one or more factors in a model; can also be used to test more
#' comlex hypotheses.
#' 
#' @param s survival object of class \code{\link[survival]{coxph}}
#' @param pos vector of positions of \code{\link{coefficients}} of interest 
#' from \code{summary(coxph)}; defaults to \code{1:length(coef(s))}
#' @param C,d \code{C}, a q-by-p matrix, and \code{d}, a q-by-1 matrix, define
#' the null hypothesis being checked; default is a global test on the variables
#' in \code{pos}, i.e., \code{C} is the identity matrix, and \code{d} is a
#' vector of zeros
#' @param digits number of significant figures in output
#' 
#' @references
#' \url{http://www.ddiez.com/teac/surv/}
#' 
#' @examples
#' library(survival)
#' fit <- coxph(Surv(time, status) ~ sex + ph.ecog, data = cancer)
#' 
#' ## compare to summary(fit)
#' local_coxph_test(fit)
#' local_coxph_test(fit, 2)
#' 
#' @export

local_coxph_test <- function(s, pos, C = NULL, d = NULL, digits = 3) {
  if (missing(pos))
    pos <- 1:length(coef(s))
  n <- length(pos)
  if (is.null(C)) {
    C <- matrix(0, n, n)
    diag(C) <- 1
  } else
    if (dim(C)[1] != n)
      stop("C has improper dimensions\n")
  if (is.null(d))
    d <- matrix(0, n, 1)
  if (dim(d)[1] != dim(C)[1])
    stop("C and d do not have appropriate dimensions\n")
  I. <- s$var[pos, pos]
  est <- matrix(as.vector(s$coeff[pos]), dim(C)[2])
  X <- as.numeric(t(C %*% est - d) %*% solve(t(C) %*% I. %*% C ) %*% 
                    (C %*% est - d))
  signif(1 - pchisq(X, dim(C)[1]), digits)
}

#' Create counting process data
#' 
#' Converts a data frame to counting process notation and allows for time-
#' dependent variables to be introduced.
#' 
#' @param data data frame with survival time, survival status, and other 
#' covariates
#' @param time.var \code{data} variable name representing survival time
#' @param status.var \code{data} variable name representing status
#' @param covars other covariates to retain
#' 
#' @return
#' A data frame with events in counting process notation.
#' 
#' @references
#' \url{http://www.ddiez.com/teac/surv/}
#' 
#' @examples
#' library(survival)
#' 
#' cp <- surv_cp(aml, 'time', 'status')
#' coxph(Surv(start, stop, status) ~ x, data = cp)
#' 
#' ## compare to
#' coxph(Surv(time, status) ~ x, data = aml)
#' 
#' @export

surv_cp <- function(data, time.var, status.var, 
                      covars = setdiff(names(data), c(time.var, status.var))) {
  
  ## sorted times, append to 0
  t.sort <- c(0, sort(unique(data[[time.var]])))
  
  ## for each data point find times less than or equal to the obs time
  t.list <- lapply(data[[time.var]], function(x) t.sort[t.sort <= x])
  
  ## create list of datasets with covariates and all relevant start/stop times
  ## remove one from end of x, stop by removing first of x
  ## include the status variable and covariates in the dataframe
  f <- function(i)
    data.frame(start = head(t.list[[i]], -1),
               stop = tail(t.list[[i]], -1),
               data[i, c(status.var, covars)], 
               row.names = NULL)
  
  n <- length(t.list)
  datl <- Map(f, 1:n)
  data <- do.call(rbind, datl)
  
  ## create the correct status need last time for each
  ## subject with status=1 to to be status=1 but all others status=0
  
  ## lapply creates vectors 0,0,0,...,1 based on length of t.list
  ## substract 2 because the lag takes one away, then need one for the 1 at end
  ## this is then multiplied by status to correct it
  keep.status <- do.call(c, lapply(t.list, function(x) 
    c(rep(0, length(x) - 2), 1)))
  data[status.var] <- data[status.var] * keep.status
  data
}

#' Summary of a survival curve
#' 
#' Prints and returns a list containing the survival curve, confidence limits 
#' for the curve, and other information.
#' 
#' @param s \code{\link[survival]{survfit}} object
#' @param digits number of digits to use in printing numbers
#' @param ... additional arguments passed to 
#' \code{\link[survival]{summary.survfit}}
#' 
#' @return
#' A list with summaries for each strata; see 
#' \code{\link[survival]{summary.survfit}}
#' 
#' @seealso
#' \code{\link[survival]{survfit}}, 
#' \code{\link[survival]{print.summary.survfit}}
#' 
#' @examples
#' library(survival)
#' data(cancer)
#' fit1 <- survfit(coxph(Surv(time, status) ~ strata(I(age > 60)), 
#'                       data = cancer),
#'                 conf.type = 'log-log')
#' surv_summary(fit1, times = c(0, 100, 200))
#' 
#' @export

surv_summary <- function(s, digits = max(getOption('digits') - 4, 3), ...) {
  ## error checks
  if (!inherits(s, 'survfit')) 
    stop('s must be a survfit object')
  x <- summary(s, ...)
  
  savedig <- options(digits = digits)
  on.exit(options(savedig))
  if (!is.null(cl <- x$call)) {
    cat("Call: ")
    dput(cl)
  }
  omit <- x$na.action
  if (length(omit)) 
    cat(naprint(omit), "\n")
  if (x$type == "right" || is.null(x$n.enter)) {
    mat <- cbind(x$time, x$n.risk, x$n.event, x$surv)
    cnames <- c("time", "n.risk", "n.event")
  } else 
    if (x$type == "counting") {
      mat <- cbind(x$time, x$n.risk, x$n.event, x$n.enter, x$n.censor, x$surv)
      cnames <- c("time", "n.risk", "n.event", "entered", "censored")
    }
  if (is.matrix(x$surv)) 
    ncurve <- ncol(x$surv)
  else ncurve <- 1
  if (ncurve == 1) {
    cnames <- c(cnames, "survival")
    if (!is.null(x$std.err)) {
      if (is.null(x$lower)) {
        mat <- cbind(mat, x$std.err)
        cnames <- c(cnames, "std.err")
      } else {
        mat <- cbind(mat, x$std.err, x$lower, x$upper)
        cnames <- c(cnames, "std.err", 
                    paste("lower ", x$conf.int * 100, "% CI", sep = ""),
                    paste("upper ", x$conf.int * 100, "% CI", sep = ""))
      }
    }
  } else 
    cnames <- c(cnames, paste("survival", seq(ncurve), sep = ""))
  if (!is.null(x$start.time)) {
    mat.keep <- mat[ , 1] >= x$start.time
    mat <- mat[mat.keep, , drop = FALSE]
    if (is.null(dim(mat))) 
      stop(paste("No information available using start.time =", 
                 x$start.time, "."))
  }
  if (!is.matrix(mat)) 
    mat <- matrix(mat, nrow = 1)
  if (!is.null(mat)) {
    dimnames(mat) <- list(NULL, cnames)
    if (is.null(x$strata)) {
      cat("\n")
      invisible(prmatrix(mat, rowlab = rep("", nrow(mat))))
    } else {
      strata <- x$strata
      if (!is.null(x$start.time))
        strata <- strata[mat.keep]
      invisible(setNames(lapply(levels(strata), function(i) {
        who <- (strata == i)
        cat("\n               ", i, "\n")
        if (sum(who) == 1)
          prmatrix(mat[who, ])
        else prmatrix(mat[who, ], rowlab = rep("", sum(who)))
      }), levels(strata)))
    }
  } else 
    stop("There are no events to print. Use the option censored = TRUE ",
         "with the summary function to see the censored observations.")
}

#' Summary table
#' 
#' Prints a formatted summary table for \code{\link[survival]{survfit}} objects
#' 
#' @param s \code{\link[survival]{survfit}} object
#' @param digits number of digits to use in printing numbers
#' @param times vector of times
#' @param ... additional arguments passed to 
#' \code{\link[survival]{summary.survfit}}
#' 
#' @return
#' A matrix (or list of matrices) with formatted summaries for each strata; see 
#' \code{\link[survival]{summary.survfit}}
#' @seealso
#' \code{\link[survival]{survfit}}, 
#' \code{\link[survival]{print.summary.survfit}}
#' 
#' @examples
#' library(survival)
#' data(cancer)
#' 
#' fit0 <- survfit(coxph(Surv(time, status) ~ 1, 
#'                       data = cancer),
#'                 conf.type = 'log-log')
#' surv_table(fit0, times = c(0, 100, 200))
#' 
#' ## also works for list of tables
#' fit1 <- survfit(coxph(Surv(time, status) ~ strata(I(age > 60)), 
#'                       data = cancer),
#'                 conf.type = 'log-log', conf.int = 0.9)
#' surv_table(fit1)
#' 
#' \dontrun{
#' library(htmlTable)
#' s <- `colnames<-`(surv_table(fit0, times = c(0, 200, 400, 600, 800), 
#'                              digits = 2)[ , -4], 
#'                   c('Time','No. at risk','No. of events','OS (95% CI)'))
#' htmlTable(s)
#' }
#' 
#' @export

surv_table <- function(s, digits = 3, times = pretty(range(s$time)), ...) {
  tmp <- capture.output(summ <- surv_summary(s, digits = digits,
                                             times = times, ...))
  f <- function(x, d = digits, vars = vars) {
    vars = colnames(x)
    tmpvar <- colnames(x)[grep('survival|std.err|lower|upper', colnames(x))]
    x[, tmpvar] <- roundr(x[, tmpvar], digits = d)
    surv <- sprintf('%s (%s, %s)', 
                    x[, colnames(x)[grepl('survival', colnames(x))]],
                    x[, colnames(x)[grepl('lower', colnames(x))]],
                    x[, colnames(x)[grepl('upper', colnames(x))]])
    `colnames<-`(cbind(x[, c(setdiff(vars, tmpvar), 'std.err')], surv),
                 c('Time','No. at risk','No. event','Std.Error',
                   sprintf('OR (%s%% CI)', s$conf.int * 100)))
  }
  if (is.list(summ)) Map(f = f, summ) else f(summ)
}

#' Pairwise survival group comparisons
#' 
#' Evaluate differences in survival curves for all pairs of a grouping
#' variable. This function currently works for one \code{factor}-like
#' variable, and all unqiue levels are treated as a sub-group.
#' 
#' @param s an object of class \code{\link[survival]{survdiff}} or
#' \code{\link[survival]{survfit}}
#' @param ... additional arguments passed to \code{\link{survdiff}} such as
#' \code{na.action} to filter missing data or \code{rho} to control the type
#' of test
#' @param method p-value correction method; see \code{\link{p.adjust}}
#' @param digits integer indicating the number of decimal places to be used
#' 
#' @return
#' A list of length three giving the \code{n}s, \code{chi.sq} statistic, and
#' \code{p.value} for each comparison. Note that the lower triangle of
#' \code{p.value} are uncorrected and the upper triangle is adjusted using
#' \code{method} (the default is a Bonferroni correction, see
#' \code{\link{p.adjust}})
#' 
#' @seealso
#' \code{\link[rawr]{pvalr}}; \code{\link{survdiff}}; \code{\link{p.adjust}};
#' \code{\link[rms]{contrast}}
#' 
#' @examples
#' library('survival')
#' fit1 <- survdiff(Surv(time, status) ~ sex, data = lung)
#' survdiff_pairs(fit1)
#' 
#' ## note that despite a numeric group variable, contrasts
#' ## are calculated for each unique level combination
#' dd <- lung[lung$ph.ecog %in% 0:2, ]
#' fit2 <- survdiff(Surv(time, status) ~ ph.ecog, data = dd)
#' survdiff_pairs(fit2)
#' 
#' ## compare
#' survdiff(Surv(time, status) ~ ph.ecog, data = dd[dd$ph.ecog %in% 0:1, ])
#' 
#' ## for interactions, create a new variable with all levels
#' dd$int <- with(dd, interaction(sex, ph.ecog))
#' fit3 <- survdiff(Surv(time, status) ~ int, data = dd)
#' survdiff_pairs(fit3)
#' 
#' @export

survdiff_pairs <- function(s, ..., method = 'bonferroni',
                           digits = getOption('digits')) {
  stopifnot(inherits(s, c('survdiff', 'survfit')))
  rhs <- all.vars(s$call$formula)[-(1:2)]
  stopifnot(length(rhs) == 1L)
  data <- eval(s$call$data, envir = parent.frame())
  unq <- sort(unique(data[, rhs]))
  
  res <- matrix(0, length(unq), length(unq), dimnames = list(unq, unq))
  nn <- outer(as.character(unq), as.character(unq), Vectorize(function(x, y)
    nrow(data[data[, rhs] %in% c(x, y), ])))
  
  dimnames(nn) <- list(unq, unq)
  names(dimnames(res)) <- names(dimnames(nn)) <- c(rhs, rhs)
  
  for (ii in seq_along(unq))
    for (jj in (seq_along(unq))[-ii])
      res[ii, jj] <- survdiff(as.formula(s$call$formula), ...,
                        data = data[data[, rhs] %in% unq[c(ii, jj)], ])$chisq
  
  pvu <- apply(res, 1:2, function(x) pchisq(x, 1, lower.tail = FALSE))
  pvc <- t(pvu)[upper.tri(pvu)]
  pvc <- p.adjust(pvc, method = method, n = length(pvc))
  pvu[upper.tri(pvu)] <- pvc
  
  lapply(list(n = nn, chi.sq = res, p.value = pvu), round, digits = digits)
}
