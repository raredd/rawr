### survival stuff
# kmplot, kmplot_by, local_coxph_test, surv_cp, surv_summary, surv_table,
# survdiff_pairs, landmark
#
# unexported: points.kmplot, kmplot_data_, lr_text, lr_pval
###


#' Survival curves
#' 
#' Plot Kaplan-Meier or Cox regression models with optional at-risk table,
#' median survival summary, and other features.
#' 
#' Line specifications (e.g., \code{lty.surv}, \code{lwd.surv}, etc) will be
#' recycled as needed.
#' 
#' If \code{col.band} is not \code{NULL}, \code{NA}, or \code{FALSE}, a
#' confidence band is plotted; however, this is not a confidence band in the
#' statistical sense, i.e., a xx-percent chance of containing the entire
#' population of the survival curve which are wider than the point-wise
#' confidence limits. Rather, it refers to a band of color plotted between
#' the confidence limits calculated in the survfit object. That is, the xx-\% 
#' confidence interval (plotted when \code{lty.ci != 0}) and the confidence
#' bands are identical--just two ways of plotting the same invervals.
#' 
#' \code{xaxs} is the style of the x-axis; see \code{\link{par}}. The default
#' for \code{kmplot} is \code{"S"} which is equivalent to \code{xaxs = "i"}
#' but with the maximum \code{xlim} value increased by 4\%. Other styles for
#' \code{xaxs} currently implemented in \code{R} are \code{"r"} (default for
#' plotting and the previous value for \code{kmplot}) and \code{"i"} which
#' will \emph{not} add padding to the ends of the axes.
#' 
#' When saving plots, it is highly recommended to use \code{\link{png}},
#' \code{\link{svg}}, \code{\link{pdf}}, etc. instead of exporting directly
#' from the \code{R} graphics device. Doing so may cause the at-risk table or
#' legend to be mis-aligned.
#' 
#' @param s an object of class \code{\link{survfit}} or \code{survfit.cox}
#' @param lty.surv,lwd.surv,col.surv line type, width, and color for survival
#' curve(s); colors may be either numeric, color names as character string(s),
#' or hexadecimal string(s)
#' 
#' colors will be assigned to curves in order of strata or may be mapped using
#' a named vector (the names must match the ones given to \code{strata.lab} or
#' those that are created by \code{\link[survival]{strata}}); this can be
#' useful to set colors for specific strata levels in multiple plots if some
#' levels are missing; see examples in \code{\link{kmplot_by}}
#' @param mark,lwd.mark numeric plotting character (\code{\link{pch}}) or
#' character string, e.g., \code{''}, \code{'|'}; if \code{mark = 'bump'}, a
#' mark will be drawn only above the curve; \code{lwd.mark} controls the line
#' width when a \code{pch} or \code{"bump"} is used
#' @param lty.ci,lwd.ci,col.ci line type, width, and color for confidence
#' interval(s); not plotted (i.e., \code{= 0}) by default
#' @param col.band color for confidence band(s); either as numeric, color
#' string(s), or hexadecimal string(s); if \code{TRUE}, \code{col.surv}
#' values will be used; if \code{FALSE}, \code{NA}, or \code{NULL}, no
#' bands will be plotted; also note that this is not a true confidence band;
#' see details
#' @param atrisk logical; if \code{TRUE} (default), draws at-risk table
#' @param atrisk.lab heading for at-risk table
#' @param atrisk.lines logical; draw lines next to strata in at-risk table
#' @param atrisk.col logical or a vector with colors for at-risk table text;
#' if \code{TRUE}, \code{col.surv} will be used
#' @param strata.lab labels used in legend and at-risk table for strata; if
#' \code{NULL} (default), labels created in \code{survfit} are used; if only
#' one strata is present, "All" is used by default; if \code{FALSE}, labels
#' are not used; if \code{TRUE}, labels will be stripped of variable names
#' @param strata.expr an alternative to \code{strata.lab} which allows for
#' \code{\link{bquote}} or \code{\link{expression}} to be passed to labels
#' for at-risk table; note that \code{strata.expr} trumps \code{strata.lab}
#' @param strata.order order of strata in legend and at-risk table
#' @param extra.margin increase left margin when strata labels in at-risk
#' table are long
#' @param median logical; if \code{TRUE}, median and confidence interval for
#' each curve is added to at-risk table
#' @param xaxs style of axis; see details or \code{\link{par}}
#' @param xlim,ylim x- and y-axis limits
#' @param xaxis.at,yaxis.at positions for x- and y-axis labels and ticks
#' @param xaxis.lab,yaxis.lab x- and y-axis tick labels
#' @param xlab,ylab x- and y-axis labels
#' @param main title of plot
#' @param cex.axis text size for axes labels, legend, at-risk table
#' @param legend logical, a vector of x/y coordinates, or a keyword (see
#' \code{\link{legend}}); if \code{TRUE}, the default position is
#' \code{"bottomleft"}
#' @param lr_test logical or numeric; if \code{TRUE}, a log-rank test will be
#' performed and the results added to the top-right corner of the plot; if
#' numeric, the value is passed as \code{rho} controlling the type of test
#' performed; see \code{\link{survdiff}}
#' @param mar margins; see \code{mar} section in \code{\link{par}}
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
#' \code{\link{kmplot_by}}; \code{survival:::plot.survfit};
#' \code{\link[plotr]{ggsurv}}; \code{\link{kmplot_data_}}; \code{\link{lr_text}};
#' \code{\link{lr_pval}}; \code{\link{points.kmplot}}
#' 
#' @examples
#' library('survival')
#' km1 <- survfit(Surv(time, status) ~ sex, data = colon)
#' km2 <- survfit(Surv(time, status) ~ I(rx == "Obs") + adhere, data = colon)
#' 
#' ## basic usage
#' kmplot(km1)
#' kmplot(km1, atrisk.col = c('grey50','tomato'), lr_test = TRUE)
#' kmplot(km1, mark = 'bump', atrisk.lines = FALSE, median = TRUE)
#' kmplot(km2, atrisk = FALSE, lwd.surv = 2, lwd.mark = .5,
#'        col.surv = 1:4, col.band = c(1,0,0,4))
#' 
#' 
#' ## expressions in at-risk table (strata.expr takes precedence)
#' kmplot(km1, strata.lab = c('\u2640', '\u2642'))
#' kmplot(km1, strata.lab = c('\u2640', '\u2642'),
#'                strata.expr = expression(widetilde(ring(Female)),
#'                                         phantom() >= Male))
#' 
#' ## character vectors passed to strata.expr will be parsed
#' kmplot(km1, strata.expr = c('Sex[Female]', 'Sex[Male]'))
#' kmplot(km1, strata.lab  = c('Sex[Female]', 'Sex[Male]'))
#' 
#' 
#' ## when using mfrow options, use add = TRUE and same mar to align axes
#' mar <- c(8,6,2,2)
#' par(mfrow = c(1, 2))
#' kmplot(km1, add = TRUE, mar = mar)
#' kmplot(km2, add = TRUE, strata.lab = TRUE, mar = mar)
#' 
#' 
#' \dontrun{
#' pdf(tf <- tempfile(fileext = '.pdf'), height = 8, width = 11,
#'     pointsize = 12, family = 'serif')
#' kmplot(survfit(Surv(time, status) ~ rx + adhere, data = colon),
#'        panel.first = abline(v = c(0, .5, 1:9) * 365, lty = 3),
#'        mark = 'bump',                     # no censor mark
#'        lty.ci = 2, lwd.ci = .3,           # dashed line for CIs
#'        xaxis.at = c(0, .5, 1:9) * 365,    # change days to years
#'        xaxis.lab = c(0, .5, 1:9),         # label years
#'        yaxis.lab = pretty(0:1) * 100,     # change to percent
#'        xlab = 'Time (years)', ylab = 'Percent survival',
#'        col.surv = c('blue','red','green','black','purple','orange'),
#'        col.ci   = c(0,0,0,0,'purple',0),  # CI only for one group
#'        extra.margin = 6,        # increase margin for long strata labels
#'        strata.lab = c('Obs','Obs+','Lev','Lev+','Lev5fu','Lev5fu+'),
#'        strata.order = c(5,6,3,1,4,2),     # order by survival estimates
#'        median = TRUE,                 # add median and CI
#'        oma = c(0,0,0,1),              # add extra line on right for median
#'        font = 2, bty = 'l', tcl = .5) # bold table text and other options
#' title(main = 'Chemotherapy for stage B/C colon cancer',
#'       font.main = 1, line = 2.5)
#' dev.off()
#' system2(getOption('pdfviewer'), tf)
#' }
#' 
#' @export

kmplot <- function(s,
                   ## basic plot options
                   lty.surv = par('lty'), lwd.surv = par('lwd'),
                   col.surv = seq_along(s$n), mark = 3,
                   lwd.mark = lwd.surv,
                   
                   ## confidence options
                   lty.ci = 0, lwd.ci = lwd.surv,
                   col.ci = col.surv, col.band = FALSE,
                   
                   ## at-risk table options
                   atrisk = TRUE, atrisk.lab = 'Number at risk',
                   atrisk.lines = TRUE, atrisk.col = !atrisk.lines,
                   strata.lab = NULL,
                   strata.expr = NULL, strata.order = seq_along(s$n),
                   extra.margin = 5, median = FALSE,
                   
                   ## aesthetics
                   xaxs = 's', xlim = c(0, max(s$time)), ylim = c(0, 1),
                   xaxis.at = pretty(c(0, s$time)), xaxis.lab = xaxis.at,
                   yaxis.at = pretty(ylim), yaxis.lab = yaxis.at,
                   xlab = 'Time', ylab = 'Survival probability',
                   main = '', cex.axis = par('cex.axis'),
                   legend = !atrisk && !is.null(s$strata),
                   
                   ## other options
                   lr_test = FALSE, mar = NULL, add = FALSE,
                   panel.first = NULL, panel.last = NULL, ...) {
  if (!inherits(s, 'survfit'))
    stop('\'s\' must be a \'survfit\' object')
  
  op <- par(no.readonly = TRUE)
  if (!add)
    on.exit(par(op))
  if (is.logical(lr_test)) {
    rho <- 0
  } else if (is.numeric(lr_test)) {
    rho <- lr_test
    lr_test <- TRUE
  } else {
    rho <- 0
    lr_test <- TRUE
  }
  
  svar <- as.character(form <- s$call$formula)
  svar <- svar[-(1:2)]
  sdat <- s$.data %||% {
    sdat <- deparse(s$call$data)
    sname <- gsub('[$[].*', '', sdat)
    tryCatch(if (identical(sdat, sname))
      get(sdat, where(sname)) else eval(parse(text = sdat), where(sname)),
      error = function(e) NULL)
  }
  ## remove missing here for special case: all NA for one strata level
  ## drops level in s$strata but not in table(sdat[, svar])
  sdat <- na.omit(sdat[, all.vars(form)])
  
  ## single strata
  one <- identical(svar, '1')
  if (!(ng <- length(s$strata))) {
    s$strata <- length(s$time)
    if (length(svar) == 1L & !one) {
      svar <- tryCatch({
        tbl <- table(sdat[, svar])
        names(tbl)[tbl > 0L]
      }, error = function(e) NULL)
    }
    names(s$strata) <- if (!is.null(svar) & !one) svar else 'All'
    # legend <- atrisk.lines <- FALSE
    legend <- FALSE
  }
  
  cs <- col.surv
  ng <- max(ng, 1L)
  if (length(col.band) <= 1L)
    if (is.null(col.band) || is.na(col.band))
      col.band <- FALSE
  
  col.surv <- if (missing(col.surv) || is.null(col.surv)) {
    if (isTRUE(col.band) || all(!col.band))
      seq_along(s$n)
    else rep_len(col.band, length.out = ng)
  } else rep(col.surv, length.out = ng)
  ## must use rep instead of rep_len for names
  
  col.band <- if (isTRUE(col.band))
    col.surv else if (length(col.band) == 1L & identical(col.band, FALSE))
      rep_len(NA, ng) else rep_len(col.band, ng)
  
  mark <- rep_len(mark, ng)
  lty.surv <- rep_len(lty.surv, ng)
  lwd.surv <- rep_len(lwd.surv, ng)
  lwd.mark <- rep_len(lwd.mark, ng)
  lty.ci <- rep_len(lty.ci, ng)
  lwd.ci <- rep_len(lwd.ci, ng)
  col.ci <- rep_len(col.ci, ng)
  
  ## group names and more error checks
  gr <- c(s$strata)
  if (isTRUE(strata.lab)) {
    svar <- colnames(model.frame(form, sdat)[, -1, drop = FALSE])
    cl <- c(list(survival::strata),
            lapply(svar, as.symbol),
            shortlabel = TRUE)
    mode(cl) <- 'call'
    names(s$strata) <- levels(eval(cl, model.frame(form, sdat)))
  }
  
  if (!is.null(strata.lab) && isTRUE(strata.lab))
    strata.lab <- NULL
  if (!is.null(strata.lab) && identical(strata.lab, FALSE))
    strata.lab <- rep(FALSE, ng)
  if (is.null(strata.lab))
    strata.lab <- names(s$strata)
  if (length(unique(strata.lab)) != ng && strata.lab[1] != FALSE) {
    strata.lab <- strata.lab[seq.int(ng)]
    warning('length(unique(strata.lab)) != number of groups')
  }
  if (suppressWarnings(any(sort(strata.order) != seq.int(ng))))
    stop('sort(strata.order) must equal 1:', ng)
  if (ng == 1L & (strata.lab[1] == 'strata.lab')) {
    strata.lab <- 'Number at risk'
    atrisk.lab <- ifelse(is.null(atrisk.lab), strata.lab, atrisk.lab)
  }
  
  ## if cols are to be mapped by name, override col.surv
  if (!is.null(names(cs))) {
    cs <- cs[match(strata.lab, names(cs))]
    if (!anyNA(cs))
      col.surv <- col.atrisk <- cs
    else warning(
      sprintf('Mismatches in names(col.surv) - %s\n and strata.lab - %s\n',
              toString(shQuote(names(col.surv))),
              toString(shQuote(strata.lab))),
      sprintf('\nTry col.surv = c(%s)', catlist(
        setNames(as.list(shQuote(col.surv)), shQuote(strata.lab)))),
      call. = FALSE
    )
  }
  col.surv <- tcol(col.surv)
  col.band <- tcol(col.band)
  
  ## guess margins based on atrisk table options
  par(mar = c(4 + ng * atrisk,
              4 + pmax(4, extra.margin) - 3 * !atrisk,
              2,
              2 + 6 * (median & atrisk))
  )
  par(...)
  if (!is.null(mar))
    par(mar = mar)
  
  ## as in survival:::plot.survfit, adjust x-axis to start at 0
  xaxs <- if (tolower(xaxs) == 's') {
    xlim[2] <- xlim[2] * 1.04
    'i'
  } else 'r'
  
  ## reformat survival estimates
  dat <- kmplot_data_(s, strata.lab)
  dat.list <- split(dat, dat$order)
  
  ## base plot
  plot(0, type = 'n', xlim = xlim, ylim = ylim,
       ann = FALSE, axes = FALSE, xaxs = xaxs,
       panel.first = panel.first,
       panel.last = {
         box(bty = par('bty'))
         axis(1, xaxis.at, FALSE, lwd = 0, lwd.ticks = 1)
         axis(1, xaxis.at, xaxis.lab, FALSE, -0.5, cex.axis = cex.axis)
         axis(2, yaxis.at, yaxis.lab, las = 1, cex.axis = cex.axis)
         title(xlab = xlab, line = 1.5, adj = .5, ...)
         title(ylab = ylab, main = main, ...)
       })
  
  ## at-risk table below surv plot
  if (atrisk) {
    ## set colors for lines of text
    col.atrisk <- if (isTRUE(atrisk.col))
      col.surv else if (!identical(atrisk.col, FALSE) &&
                        length(atrisk.col) == ng)
        atrisk.col else rep_len(1L, ng)
    
    ## labels for each row in at-risk table
    usr <- par('usr')
    group.name.pos <- diff(usr[1:2]) / -8
    padding  <- abs(group.name.pos / 8)
    line.pos <- seq.int(ng)[order(strata.order)] + 2L
    
    if (!identical(unique(strata.lab), FALSE)) {
      if (!is.null(strata.expr)) {
        if (is.character(strata.expr))
          strata.expr <- parse(text = strata.expr)
        sapply(seq.int(length(strata.expr)), function(x)
          mtext(strata.expr[[x]], side = 1, line = line.pos[x], adj = 1,
                at = group.name.pos, col = col.atrisk[x], las = 1,
                cex = cex.axis))
      } else mtext(strata.lab, side = 1, line = line.pos, adj = 1, las = 1,
                   col = col.atrisk, at = group.name.pos, cex = cex.axis)
    }
    
    ## draw matching lines for n at risk  
    if (atrisk.lines)
      for (i in seq.int(ng))
        ## mess with the 4 here to adjust the length of the atrisk.line
        axis(1, c(group.name.pos + padding, 0 - 4 * padding), xpd = NA,
             labels = FALSE, line = line.pos[i] + 0.6, lwd.ticks = 0,
             col = col.surv[i], lty = lty.surv[i], lwd = lwd.surv[i])
    
    ## numbers at risk
    ss <- summary(s, times = xaxis.at)
    if (is.null(ss$strata))
      ss$strata <- rep_len(1L, length(ss$time))
    d1 <- data.frame(time = ss$time, n.risk = ss$n.risk, strata = c(ss$strata))
    d2 <- split(d1, d1$strata)
    
    ## right-justify numbers
    ndigits <- lapply(d2, function(x) nchar(x[, 2]))
    max.len <- max(sapply(ndigits, length))
    L <- do.call('rbind', lapply(ndigits, `length<-`, max.len))
    nd <- apply(L, 2, max, na.rm = TRUE)
    
    for (i in seq.int(ng)) {
      tmp <- d2[[i]]
      w.adj <- strwidth('0', cex = cex.axis, font = par('font')) /
        2 * nd[seq.int(nrow(tmp))]
      mtext(tmp$n.risk, side = 1, at = tmp$time + w.adj, las = 1,
            line = line.pos[i], cex = cex.axis, adj = 1, col = col.atrisk[i])
    }
    
    if (!(identical(atrisk.lab, FALSE) | is.null(atrisk.lab)))
      mtext(atrisk.lab, side = 1, at = usr[1], # at = group.name.pos,
            line = 1.5, adj = 1, col = 1, las = 1, cex = cex.axis)
    
    ## median (ci) text on right of at-risk
    if (median) {
      st <- ss$table
      st <- if (length(s$n) != 1L)
        as.data.frame(st)[strata.order, ] else as.data.frame(t(st))
      tt <- do.call('sprintf', c(list(
        fmt = '%s (%s, %s)'),
        tail(lapply(st, roundr, digits = 0L), 3L))
      )
      tt <- ifelse(is.na(st$median), '-', gsub('NA', '-', tt, fixed = TRUE))
      at <- usr[2] + diff(usr[1:2]) / 8
      mtext(sprintf('Median (%s%% CI)', s$conf.int * 100), side = 1,
            at = at, adj = .5, line = 1.5, col = 1, las = 1)
      mtext(tt, side = 1, line = line.pos, las = 1,
            at = at, adj = .5, col = col.atrisk)
    }
  }
  
  ## legend
  rlp <- strata.order
  if (!identical(legend, FALSE)) {
    if (isTRUE(legend))
      legend <- 'bottomleft'
    bgc <- if (par('bg') == 'transparent')
      'white' else par('bg')
    if (!is.null(strata.expr)) {
      legend(legend[1], if (length(legend) > 1L) legend[2] else NULL,
             legend = strata.expr[rlp], col = col.surv[rlp],
             lty = lty.surv[rlp], lwd = lwd.surv[rlp], bty = 'o', xpd = NA,
             cex = cex.axis, bg = bgc, box.col = 'transparent', inset = .01)
    } else {
      if (identical(strata.lab, FALSE))
        strata.lab <- names(s$strata)
      legend(legend[1], if (length(legend) > 1L) legend[2] else NULL,
             legend = strata.lab[rlp], col = col.surv[rlp],
             lty = lty.surv[rlp], lwd = lwd.surv[rlp], bty = 'o', xpd = NA,
             cex = cex.axis, bg = bgc, box.col = 'transparent', inset = .01)
    }
  }
  
  ## survival and confidence lines
  for (i in seq.int(ng)) {
    tmp <- dat.list[[i]]
    if (nrow(tmp) < 2L) {
      if (any(!is.na(col.band)))
        message('Note: strata level with one observation - no CI plotted.')
    } else {
      x <- tmp$time
      L <- tmp$lower
      U <- tmp$upper
      S <- tmp$survival
      naL <- which(is.na(L))
      L[naL] <- L[naL - 1L]
      U[naL] <- U[naL - 1L]
      lines(x, L, type = 's', col = col.ci[i], lty = lty.ci[i], lwd = lwd.ci[i])
      lines(x, U, type = 's', col = col.ci[i], lty = lty.ci[i], lwd = lwd.ci[i])
      
      ## confidence bands
      if (any(!is.na(col.band))) {
        col.band[i] <- tcol(col.band[i], alpha = .5)
        polygon(c(x, rev(x)), c(U, rev(L)), border = NA, col = col.band[i])
      }
    }
    
    ## survival curves
    lines(s[i], conf.int = FALSE, col = col.surv[i], lty = lty.surv[i],
          lwd = lwd.surv, mark.time = FALSE, xpd = FALSE)
    ## uncomment when bug in survival v2.39-5 fixed
    # points(s[i], lwd = lwd.surv, pch = mark, col = col.surv[i], xpd = FALSE)
    ## actually use this for more features
    if (!mark[i] == FALSE)
      points.kmplot(s[i], col = col.surv[i], pch = mark[i],
                    censor = TRUE, event = FALSE, bump = mark[i] == 'bump',
                    plot = TRUE, lwd = lwd.mark[i], xpd = FALSE)
  }
  
  ## add survdiff text in upper right corner
  if (lr_test) {
    txt <- tryCatch(lr_text(as.formula(form), sdat, rho),
                    error = function(e) 'n/a')
    if (identical(txt, FALSE))
      message('There is only one group',
              if (svar == '1') '' else paste(' for', svar),
              ' -- no lr test performed')
    else mtext(txt, side = 3, at = par('usr')[2], adj = 1,
               font = 3, cex = .8, line = .5)
  }
  
  panel.last
  
  invisible(dat)
}

#' \code{kmplot} points
#' 
#' \code{survival:::points.survfit} with additional features
#' 
#' @param x,xscale,xmax,fun,col,pch see \code{\link{points.survfit}}
#' @param censor,event logical; \code{TRUE} will draw points at censored
#' and event times, respectively
#' @param bump logical; \code{TRUE} will draw a bump mark rather than pch
#' @param plot logical; \code{FALSE} will not plot but return plotting data
#' @param ... additional graphical parameters passed to \code{\link{par}}
points.kmplot <- function(x, xscale, xmax, fun,
                          col = par('col'), pch = par('pch'),
                          censor = TRUE, event = FALSE,
                          bump = FALSE, plot = TRUE, ...) {
  conf.int <- FALSE
  if (inherits(x, "survfitms")) {
    x$surv <- 1 - x$pstate
    if (is.matrix(x$surv)) {
      dimnames(x$surv) <- list(NULL, x$states)
      if (ncol(x$surv) > 1 && any(x$states == "")) {
        x$surv <- x$surv[, x$states != ""]
        if (is.matrix(x$p0)) 
          x$p0 <- x$p0[, x$states != ""]
        else x$p0 <- x$p0[x$states != ""]
      }
    }
    if (!is.null(x$lower)) {
      x$lower <- 1 - x$lower
      x$upper <- 1 - x$upper
    }
    if (missing(fun)) 
      fun <- "event"
  }
  
  firstx <- firsty <- NA # part of the common args, but irrelevant for points
  ssurv <- as.matrix(x$surv)
  stime <- x$time
  if(!is.null(x$upper)) {
    supper <- as.matrix(x$upper)
    slower <- as.matrix(x$lower)
  } else {
    conf.int <- FALSE
    supper   <- NULL  ## marker for later code
  }
  
  ## set up strata
  if (is.null(x$strata)) {
    nstrat <- 1
    stemp <- rep(1, length(x$time)) ## same length as stime
  } else {
    nstrat <- length(x$strata)
    stemp <- rep(seq.int(nstrat), x$strata) ## same length as stime
  }
  ncurve <- nstrat * ncol(ssurv)
  firsty <- matrix(firsty, nrow = nstrat, ncol = ncol(ssurv))
  if (!missing(xmax) && any(x$time>xmax)) {
    ## prune back the survival curves
    ## I need to replace x's over the limit with xmax, and y's over the
    ## limit with either the prior y value  or firsty
    keepx  <- keepy <- NULL  ## lines to keep
    tempn  <- table(stemp)
    offset <- cumsum(c(0, tempn))
    for (i in 1:nstrat) {
      ttime <- stime[stemp == i]
      if (all(ttime <= xmax)) {
        keepx <- c(keepx, 1:tempn[i] + offset[i])
        keepy <- c(keepy, 1:tempn[i] + offset[i])
      } else {
        bad <- min((1:tempn[i])[ttime > xmax])
        if (bad == 1)  {  ## lost them all
          if (!is.na(firstx)) { ## and we are plotting lines
            keepy <- c(keepy, 1 + offset[i])
            ssurv[1 + offset[i], ] <- firsty[i, ]
          }
        } else  keepy <- c(keepy, c(1:(bad - 1), bad - 1) + offset[i])
        keepx <- c(keepx, (1:bad) + offset[i])
        stime[bad + offset[i]] <- xmax
        x$n.event[bad + offset[i]] <- 1   ## don't plot a tick mark
      }
    }
    
    ## ok, now actually prune it
    stime <- stime[keepx]
    stemp <- stemp[keepx]
    x$n.event <- x$n.event[keepx]
    if (!is.null(x$n.censor))
      x$n.censor <- x$n.censor[keepx]
    ssurv <- ssurv[keepy, , drop = FALSE]
    if (!is.null(supper)) {
      supper <- supper[keepy, , drop = FALSE]
      slower <- slower[keepy, , drop = FALSE]
    }
  }
  # stime <- stime/xscale  ## scaling is deferred until xmax processing is done
  
  if (!missing(fun)) {
    if (is.character(fun)) {
      tfun <- switch(fun,
                     'log'      = function(x) x,
                     'event'    = function(x) 1 - x,
                     'cumhaz'   = function(x) -log(x),
                     'cloglog'  = function(x) log(-log(x)),
                     'pct'      = function(x) x * 100,
                     ## special case further below
                     'logpct'   = function(x) 100 * x,
                     'identity' = function(x) x,
                     stop('Unrecognized function argument')
      )
    } else if (is.function(fun))
      tfun <- fun
    else stop('Invalid \'fun\' argument')
    
    ssurv <- tfun(ssurv)
    if (!is.null(supper)) {
      supper <- tfun(supper)
      slower <- tfun(slower)
    }
    firsty <- tfun(firsty)
  }
  
  out <- cbind(stime = stime,
               ssurv = if (ncol(ssurv) == 1L) c(ssurv) else ssurv,
               n.event = x$n.event)
  if (!plot)
    return(out)
  
  if (ncurve == 1L || length(col) == 1L) {
    if (censor) {
      st <- stime[x$n.event == 0]
      ss <- ssurv[x$n.event == 0]
      if (bump)
        segments(st, ss, st, ss + diff(par('usr')[3:4]) / 100, col = col, ...)
      else points(st, ss, col = col, pch = pch, ...)
    }
    if (event) {
      st <- stime[x$n.event > 0]
      ss <- ssurv[x$n.event > 0]
      if (bump)
        segments(st, ss, st, ss + diff(par('usr')[3:4]) / 100, col = col, ...)
      else points(st, ss, col = col, pch = pch, ...)
    }
  } else {
    c2 <- 1
    col <- rep(col, length = ncurve)
    if (!missing(pch))
      pch2 <- rep(pch, length = ncurve)
    for (j in 1:ncol(ssurv)) {
      for (i in unique(stemp)) {
        if (censor) {
          who <- which(stemp == i & x$n.event == 0)
          st <- stime[who]
          ss <- ssurv[who, j]
          if (bump)
            segments(st, ss, st, ss + diff(par('usr')[3:4]) / 100, col = col, ...)
          else points(st, ss, col = col[c2], pch = pch2[c2], ...)
        }
        if (event) {
          who <- which(stemp == i & x$n.event > 0)
          st <- stime[who]
          ss <- ssurv[who, j]
          if (bump)
            segments(st, ss, st, ss + diff(par('usr')[3:4]) / 100, col = col, ...)
          else points(st, ss, col = col[c2], pch = pch2[c2], ...)
        }
        c2 <- c2 + 1L
      }
    }
  }
  invisible(out)
}

#' Create data frame to plot survival data
#' 
#' @param s a \code{\link{survfit}} object
#' @param strata.lab character vector of strata labels; must be same length
#' as s$n
#' @seealso \code{\link{kmplot}}; \code{\link{kmplot_by}}

kmplot_data_ <- function(s, strata.lab) {
  stopifnot(inherits(s, 'survfit'))
  gr <- c(s$strata)
  ng <- max(length(gr), 1L)
  with(s, {
    data.frame(
      time = time, n.risk = n.risk, n.event = n.event,
      survival = surv, std.err = std.err, lower = lower,
      upper = upper, group = rep(strata.lab, gr),
      order = rep(seq.int(ng), gr)
    )
  })
}

#' Format results from \code{survdiff} for plotting
#' 
#' @param formula,data,rho,... passed to \code{\link{survdiff}}
#' @seealso \code{\link{lr_pval}}

lr_text <- function(formula, data, rho = 0, ...) {
  # lr_text(Surv(time, status) ~ 1, lung) ## error
  # lr_text(Surv(time, status == 1) ~ sex, lung[lung$time < 50, ]) ## warning
  # lr_text(Surv(time, status) ~ sex, lung)
  
  capture.output(
    ## Warning message: In pchisq(x$chisq, df) : NaNs produced
    ## this warning is only produced during print.survdiff
    sd <- tryCatch(print(survdiff(formula, data, rho = rho, ...)),
                   warning = function(w) '',
                   error = function(e) {
                     if (any(grepl('(?i)[1no]+ group', e)))
                       TRUE else e
                   })
  )
  if (isTRUE(sd))
    return(FALSE)
  if (identical(sd, ''))
    return(sd)
  if (!inherits(sd, 'survdiff'))
    stop(sd)
  df <- sum(1 * (colSums(if (is.matrix(sd$obs))
    sd$exp else t(sd$exp)) > 0)) - 1
  p.value <- 1 - pchisq(sd$chisq, df)
  txt <- sprintf('%s (%s df), %s', roundr(sd$chisq, 1),
                 df, pvalr(p.value, show.p = TRUE))
  bquote(paste(chi^2, ' = ', .(txt)))
}

#' Extract values from \code{survdiff} call
#' 
#' @param s a \code{\link{survfit}} or \code{\link{survdiff}} object
#' @param details logical; \code{TRUE} returns statistic, degrees of freedom,
#' and p-value where \code{FALSE} returns only a pvalue
#' @seealso \code{\link{lr_text}}

lr_pval <- function(s, details = FALSE) {
  # lr_pval(sdif); lr_pval(sfit, TRUE)
  if (inherits(s, 'survfit'))
    s <- survdiff(as.formula(s$call$formula), eval(s$.data %||% s$call$data))
  stopifnot(inherits(s, 'survdiff'))
  pv <- pchisq(chi <- s$chisq, df <- length(s$n) - 1L, lower.tail = FALSE)
  if (details)
    list(chisq = chi, df = df, p.value = pv) else pv
}

#' kmplot_by
#' 
#' @description
#' This function helps create stratified \code{\link{kmplot}}s quickly with
#' panel labels and log-rank tests for each plot.
#' 
#' \code{data} should have at least three variables: \code{strata},
#' \code{*_time}, and \code{*_ind} where \code{*} is \code{event}. For
#' example, to use progression-free survival, \code{data} should have columns
#' \code{"pfs_time"} and \code{"pfs_ind"} (in this case the user should use
#' \code{event = 'pfs'}) and optionally the \code{strata} column unless only
#' the null model (\code{strata = "1"} is needed (default).
#' 
#' Alternatively, the \code{time} argument may be used instead of following
#' the above; in this case, \code{time} and \code{event} must be variable
#' names in \code{data}. However, the method described above is more
#' efficient and preferred.
#' 
#' @param strata character string of the strata variable
#' @param event character string indicating the event (pfs, os, ttp, etc);
#' see details
#' @param data data frame to use
#' @param by optional character string of stratification variable
#' @param single logical; if \code{TRUE}, each level of \code{by} will be
#' drawn in a separate window
#' @param lr_test logical or numeric; if \code{TRUE}, a log-rank test will be
#' performed and the results added to the top-right corner of the plot; if
#' numeric, the value is passed as \code{rho} controlling the type of test
#' performed; see \code{\link{survdiff}}
#' @param main title of plot
#' @param ylab y-axis label
#' @param sub sub-title displayed in upper left corner; should be a character
#' vector with length equal to the number of panels (i.e., the number of
#' levels of \code{by} or length one if \code{by} was not given)
#' @param strata_lab at-risk table strata labels; should be a character vector
#' with length equal to the number of strata; \code{TRUE} (or equivalently
#' missing) is the default, and \code{FALSE} trims the labels; see examples
#' @param fig_lab figure panel labels; should be a character vector with
#' length equal to the number of panels (i.e., the number of levels of
#' \code{by} or length one if \code{by} was not given)
#' @param col.surv color for individual survival curves or for all curves in
#' a plot if \code{by} is given and \code{map.col = TRUE}; if \code{col.surv}
#' is a named vector which matches the at risk labels, then colors are mapped
#' to the corresponding strata; see \code{\link{kmplot}}
#' @param map.col logical; if \code{TRUE}, \code{col.surv} will be the color
#' of all curves in each plot (only used when \code{by} is non-missing)
#' @param time character string of the time variable (optional)
#' @param add logical; if \code{FALSE} (default), resets graphical parameters
#' to settings before \code{kmplot_by} was called; set to \code{TRUE} for
#' adding to existing plots
#' @param plot logical; if \code{FALSE}, no plot is created but a list with
#' \code{survfit}s is returned
#' @param ... additional arguments passed to \code{\link{kmplot}} or
#' graphical parameters subsequently passed to \code{\link{par}}
#' 
#' @return
#' Invisibly returns a list of \code{\link{survfit}} object(s) used to generate
#' plot(s). If \code{by} was used, there will be a list element for each unique
#' value.
#' 
#' @seealso
#' \code{\link{kmplot}}, \code{\link{survdiff}}; \code{\link{kmplot_data_}};
#' \code{\link{lr_text}}; \code{\link{lr_pval}}; \code{\link{points.kmplot}}
#' 
#' @examples
#' library('survival')
#' kmplot_by(time = 'time', event = 'status', data = colon)
#' 
#' 
#' ## create *_ind, *_time variables, see details
#' colon2 <- within(colon[duplicated(colon$id), ], {
#'   pfs_time <- time
#'   pfs_ind  <- status
#'   sex <- c('Female','Male')[sex + 1L]
#'   sex1 <- 'Male'
#' })
#' 
#' ## these are equivalent (with minor differences in aesthetics)
#' kmplot_by(event = 'pfs', data = colon2)
#' kmplot_by('sex1', 'pfs', colon2)
#' 
#' kmplot_by('rx', 'pfs', data = colon2, col.surv = 1:3,
#'   strata_lab = FALSE, col.band = NA)
#' 
#' 
#' ## return value is a list of survfit objects
#' l <- kmplot_by('sex', 'pfs', colon2, 'rx', plot = FALSE)
#' str(lapply(l, kmplot))
#' str(lapply(l, kmplot_by))
#' 
#' ## multiple variables can be combined
#' kmplot_by('rx + sex', 'pfs', colon2, strata_lab = FALSE, lty.surv = 1:6)
#' 
#' 
#' ## if "by" is given, default is to plot separately
#' kmplot_by('rx', 'pfs', colon2, by = 'sex', col.surv = 1:3,
#'   strata_lab = c('Observation','Trt','Trt + 5-FU'))
#'   
#' ## if "by" is given, use map.col  to map colors to plots
#' kmplot_by('rx', 'pfs', colon2, by = 'rx', map.col = TRUE, single = FALSE)
#' kmplot_by('sex', 'pfs', colon2, by = 'rx', map.col = TRUE, single = FALSE)
#' 
#' ## to keep colors mapped to the same strata across plots, use named vector
#' kmplot_by('rx', 'pfs', colon2, by = 'rx', xlim = c(0, 3000),
#'           col.surv = c(Lev = 'brown', Obs = 'blue', 'Lev+5FU' = 'purple'))
#' 
#' ## if single = FALSE, uses n2mfrow function to set par('mfrow')
#' kmplot_by('rx', 'pfs', colon2, by = 'sex', col.surv = 1:3, single = FALSE,
#'   strata_lab = c('Observation','Trt','Trt + 5-FU'),
#'   main = levels(factor(colon2$sex)))
#'   
#' ## if par('mfrow') is anything other than c(1,1), uses current setting
#' par(mfrow = c(2,2))
#' kmplot_by('rx', 'pfs', colon2, by = 'sex', col.surv = 1:3, single = FALSE,
#'   strata_lab = c('Observation','Trt','Trt + 5-FU'))
#' 
#' ## use add = TRUE to add to a figure region without using the by argument
#' par(mfrow = c(1,2))
#' mar <- c(8,6,3,2)
#' kmplot_by( 'rx', 'pfs', colon2, strata_lab = FALSE, add = TRUE, mar = mar)
#' kmplot_by('sex', 'pfs', colon2, strata_lab = FALSE, add = TRUE, mar = mar)
#'   
#' @export

kmplot_by <- function(strata = '1', event, data, by, single = TRUE,
                      lr_test = TRUE, main, ylab, sub, strata_lab, fig_lab,
                      col.surv = NULL, map.col = FALSE, time, add = FALSE,
                      plot = TRUE, ...) {
  op <- par(no.readonly = TRUE)
  if (is.logical(lr_test)) {
    rho <- 0
  } else if (is.numeric(lr_test)) {
    rho <- lr_test
    lr_test <- TRUE
  } else {
    rho <- 0
    lr_test <- TRUE
  }
  
  if (inherits(strata, 'survfit')) {
    ## if survfit object is given, extract needed vars and run as usual
    if (is.null(data <- strata$.data)) {
      data <- deparse(strata$call$data)
      data <- get(data, where(gsub('[$[].*', '', data)))
    }
    form   <- as.character(strata$call$formula)[-1]
    strata <- form[length(form)]
    event  <- gsub('(\\S+)\\)|.', '\\1', form[1])
    time   <- gsub('\\((\\w+)|.', '\\1', form[1])
  }
  
  form  <- if (!missing(time))
    sprintf('Surv(%s, %s) ~ %s', time, event, strata) else
      sprintf('Surv(%s_time, %s_ind) ~ %s', event, event, strata)
  form  <- as.formula(form)
  
  if (plot & (!add | !single))
    on.exit(par(op))
  if (!missing(by)) {
    data[, by] <- as.factor(data[, by])
    if (single) {
      add <- FALSE
      par(mfrow = c(1L,1L))
    } else {
      add <- TRUE
      if (all(par('mfrow') == c(1L,1L)))
        par(mfrow = n2mfrow(length(unique(data[, by]))))
    }
    sp <- split(data, droplevels(data[, by]))
    ## list names will be main title(s)
    names(sp) <- rep_len(if (missing(main))
      paste(by, names(sp), sep = '=') else main, length(sp))
  } else {
    if (missing(add)) {
      add <- FALSE
      par(mfrow = c(1L,1L))
    }
    map.col <- FALSE
    sp <- list(data)
  }
  
  ## define these before passing to loop
  mlabs <- missing(strata_lab)
  msub  <- missing(sub)
  fig   <- if (length(sp) > 1L & missing(fig_lab))
    LETTERS[seq_along(sp)] else if (missing(fig_lab)) '' else fig_lab
  fig   <- rep_len(fig, length(sp))
  ylab  <- if (missing(ylab))
    sprintf('%s probability', toupper(event)) else ylab
  
  ## possible names for strata labels/title
  dots <- lapply(dots(...), eval)
  strata_names <- dots$strata.lab %||% dots$strata.expr %||%
    if (mlabs) NULL else strata_lab
  if (is.logical(strata_names) | anyNA(strata_names) | is.null(strata_names))
    strata_names <- NULL
  if (!is.null(names(strata_names)))
    names(sp) <- strata_names
  
  if (!missing(by) & is.null(names(col.surv))) {
    sl <- survfit(form, data)
    col.surv <- setNames(
      col.surv %||% seq_along(sl$n),
      if (by == strata) NULL else strata_names %||% names(sl$strata)
    )
    if (map.col & length(sp) != length(col.surv))
      col.surv <- seq_along(sp)
  }
  
  sl <- lapply(seq_along(sp), function(x) {
    tryCatch({
      eval(substitute(
        survfit(form, data = sp[[x]], type = 'kaplan-meier',
                conf.type = 'log-log', error = 'greenwood',
                conf.int = 0.95, se.fit = TRUE),
        list(form = form))
      )
    }, error = function(e) {
      if (grepl('first argument must be of mode character', e)) {
        warning('Error in survfit:\n', e, '\n',
                'Check that \'strata\' has at least one non-missing value',
                '\n\n', 'Returning null model: ',
                deparse(update(form, . ~ 1)))
        eval(substitute(
          survfit(form, data = sp[[x]], type = 'kaplan-meier',
                  conf.type = 'log-log', error = 'greenwood',
                  conf.int = 0.95, se.fit = TRUE),
          list(form = update(form, . ~ 1))))
      } else stop(e)
    })
  })
  
  l <- lapply(seq_along(sp), function(x) {
    s <- s0 <- sl[[x]]
    s$.data <- s0$.data <- sp[[x]]
    
    if (strata == '1')
      strata <- ''
    if (!is.null(s$strata)) {
      ns <- names(s$strata)
      ns <- if (mlabs || isTRUE(strata_lab)) ns else
        if (identical(strata_lab, FALSE)) {
          svar <- colnames(model.frame(form, s$.data)[, -1, drop = FALSE])
          cl <- c(list(survival::strata),
                  lapply(svar, as.symbol),
                  shortlabel = TRUE)
          mode(cl) <- 'call'
          levels(eval(cl, model.frame(form, s$.data)))
        } else if (!length(strata_lab) == length(ns)) {
          warning('length(strata_lab) does not equal length(s$strata)',
                  call. = FALSE)
          ns
        } else if (length(strata_lab) == length(ns))
          strata_lab else ns
      names(s$strata) <- trimws(ns)
    }
    if (!plot)
      return(s0)
    
    
    kmplot(s, add = add, legend = FALSE, main = names(sp)[x], ylab = ylab, ...,
           col.surv = if (map.col)
             unname(col.surv)[x] else col.surv,
           panel.first = {
             p <- par('usr')
             mtxt <- if (!msub)
               rep_len(sub, length(sp))[x] else strata
             mtext(mtxt, 3, 0.5, FALSE, 0, 0, font = 3)
             mtext(fig[x], 3, 0.5, FALSE, 0 - p[2] * .05, font = 2, cex = 1.5)
             
             ## add survdiff text in upper right corner
             if (lr_test) {
               txt <- tryCatch(lr_text(form, sp[[x]], rho),
                               error = function(e) 'n/a')
               if (identical(txt, FALSE))
                 message('There is only one group',
                         if (nzchar(mtxt)) paste(' for', mtxt) else '',
                         ' -- no lr test performed')
               else mtext(txt, side = 3, at = p[2], adj = 1,
                          font = 3, cex = .8, line = .5)
             }
           })
    s0
  })
  names(l) <- names(sp) %||% strata
  invisible(l)
}

#' Add ticks to \code{kmplot}
#' 
#' Add tick marks to a \code{\link{kmplot}} representing a specific event,
#' \code{what}, occurring within a variable, \code{by_var}.
#' 
#' @param s an object of class \code{\link{survfit}}
#' @param data the data set used to fit \code{s} which should also contain
#' \code{by_var} and optionally \code{time}, \code{event}, and \code{strata};
#' \code{kmplot_ticks} will attempt to select these variables based on the
#' call to \code{survfit}
#' @param by_var,what a variable, \code{by_var} in \code{data} for which
#' tick marks are to be placed at each occurrence of \code{what}
#' @param time,event,strata (optional) variables used to fit \code{s}
#' @param col a vector of colors (one for each strata level of \code{s}) for
#' tick marks; note these colors should match the curves of the survival plot
#' @param ... additional arguments passed to \code{\link{points}}
#' 
#' @seealso
#' \code{\link{kmplot}}; \code{\link{kmplot_by}}
#' @examples
#' library('survival')
#' s <- survfit(Surv(futime, fustat) ~ rx, ovarian)
#' 
#' kmplot(s, add = TRUE)
#' kmplot_ticks(s, by_var = 'resid.ds', what = 1, col = 4:5)
#' kmplot_ticks(s, by_var = 'resid.ds', what = 2, pch = '*')
#' 
#' @export

kmplot_ticks <- function(s, data = eval(s$call$data), by_var, what,
                         time, event, strata,
                         col = nlevels(factor(data[, strata])), ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  ## lifted from kmplot_by
  if (inherits(s, 'survfit')) {
    if (is.null(data <- s$.data)) {
      data <- deparse(s$call$data)
      data <- get(data, where(gsub('[$[].*', '', data)))
    }
    terms <- c(survival:::terms.inner(s$call$formula),
               tail(as.character(s$call$formula), 1L))
    if (missing(time))
      time <- terms[1L]
    if (missing(event))
      event <- terms[2L]
    if (missing(strata))
      strata <- terms[3L]
  }
  
  nc <- length(unique(data[, strata]))
  col <- if (missing(col))
    seq.int(nc) else rep_len(col, nc)
  data[, strata] <- as.factor(data[, strata])
  data <- data[data[, by_var] %in% what, ]
  
  if (!nrow(data)) {
    par(op)
    return(invisible(NULL))
  }
  # segments(data[, time], y0 = rep(0, nrow(data)), y1 = par('usr')[3],
  #          col = col[as.numeric(data[, strata])], lwd = 2, ...)
  points(data[, time], rep(0, nrow(data)),
         col = col[as.numeric(data[, strata])], ...)
  
  invisible(data)
}

#' Compute local p-value from coxph
#' 
#' Checks the null hypothesis: C * beta.hat = c, i.e., the local
#' p-value of one or more factors in a model; can also be used to test more
#' comlex hypotheses.
#' 
#' @param s survival object of class \code{\link[survival]{coxph}}
#' @param pos vector of positions of \code{\link{coefficients}} of interest
#' from \code{summary(coxph)}; defaults to \code{seq_along(coef(s))}
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
#' library('survival')
#' fit <- coxph(Surv(time, status) ~ sex + ph.ecog, data = cancer)
#' 
#' ## compare to summary(fit)
#' local_coxph_test(fit)
#' local_coxph_test(fit, 2)
#' 
#' 
#' @export

local_coxph_test <- function(s, pos, C = NULL, d = NULL, digits = 3) {
  stopifnot(inherits(s, 'coxph'))
  if (missing(pos))
    pos <- seq_along(coef(s))
  n <- length(pos)
  if (!(n <- length(pos)) || !length(coef(s)))
    stop('Model has no coefficients')
  if (is.null(C)) {
    C <- matrix(0, n, n)
    diag(C) <- 1
  } else if (dim(C)[1] != n)
      stop('C has improper dimensions\n')
  if (is.null(d))
    d <- matrix(0, n, 1)
  if (dim(d)[1] != dim(C)[1])
    stop('C and d do not have appropriate dimensions')
  I. <- s$var[pos, pos]
  est <- matrix(as.vector(s$coeff[pos]), dim(C)[2])
  chisq <- as.numeric(t(C %*% est - d) %*% solve(t(C) %*% I. %*% C ) %*%
                        (C %*% est - d))
  df <- dim(C)[1]
  p.value <- signif(1 - pchisq(chisq, df), digits)
  list(est = est, chisq = chisq, df = df, p.value = p.value)
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
#' library('survival')
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
    data.frame(start = head(t.list[[i]], -1L),
               stop = tail(t.list[[i]], -1L),
               data[i, c(status.var, covars)],
               row.names = NULL)
  data <- do.call('rbind', Map('f', seq_along(t.list)))
  
  ## create the correct status need last time for each
  ## subject with status=1 to to be status=1 but all others status=0
  
  ## lapply creates vectors 0,0,0,...,1 based on length of t.list
  ## substract 2 because the lag takes one away, then need one for the 1 at end
  ## this is then multiplied by status to correct it
  keep.status <- do.call('c', lapply(t.list, function(x)
    c(rep(0L, length(x) - 2L), 1L)))
  data[status.var] <- data[status.var] * keep.status
  data
}

#' Summary of a survival curve
#' 
#' Prints and returns a list containing the survival curve, confidence limits
#' for the curve, and other information which can be more useful than the
#' acutal return value of \code{survival:::print.summary.survfit}.
#' 
#' @param s a \code{\link{survfit}} object
#' @param digits number of digits to use in printing numbers
#' @param ... additional arguments passed to
#' \code{\link[survival]{summary.survfit}}
#' 
#' @return
#' A list with \code{survival:::print.summary.survfit} matrices for each
#' strata, i.e., the output printed.
#' 
#' @seealso
#' \code{\link{survfit}}, \code{\link{print.summary.survfit}}
#' 
#' @examples
#' library('survival')
#' fit1 <- survfit(coxph(Surv(time, status) ~ strata(I(age > 60)),
#'                       data = cancer),
#'                 conf.type = 'log-log')
#' surv_summary(fit1, times = c(0, 100, 200))
#' 
#' @export

surv_summary <- function(s, digits = 3L, ...) {
  if (!inherits(s, 'survfit'))
    stop('\'s\' must be a \'survfit\' object')
  oo <- options()
  on.exit(options(oo))
  options(digits = digits)
  
  x <- summary(s, ...)
  if (!is.null(cl <- x$call)) {
    cat('Call: ')
    dput(cl)
  }
  omit <- x$na.action
  if (length(omit))
    cat(naprint(omit), '\n')
  if (x$type == 'right' || is.null(x$n.enter)) {
    mat <- cbind(x$time, x$n.risk, x$n.event, x$surv)
    cnames <- c('time', 'n.risk', 'n.event')
  } else if (x$type == 'counting') {
    mat <- cbind(x$time, x$n.risk, x$n.event, x$n.enter, x$n.censor, x$surv)
    cnames <- c(time, 'n.risk', 'n.event', 'entered', 'censored')
  }
  ncurve <- if (is.matrix(x$surv))
    ncol(x$surv) else 1L
  if (ncurve == 1L) {
    cnames <- c(cnames, 'survival')
    if (!is.null(x$std.err)) {
      if (is.null(x$lower)) {
        mat <- cbind(mat, x$std.err)
        cnames <- c(cnames, 'std.err')
      } else {
        mat <- cbind(mat, x$std.err, x$lower, x$upper)
        cnames <- c(cnames, 'std.err',
                    paste0('lower ', x$conf.int * 100, '% CI'),
                    paste0('upper ', x$conf.int * 100, '% CI'))
      }
    }
  } else cnames <- c(cnames, paste0('survival', seq.int(ncurve)))
  if (!is.null(x$start.time)) {
    mat.keep <- mat[, 1] >= x$start.time
    mat <- mat[mat.keep, , drop = FALSE]
    if (is.null(dim(mat)))
      stop(paste('No information available using start.time =', x$start.time))
  }
  if (!is.matrix(mat))
    mat <- matrix(mat, nrow = 1L)
  if (!is.null(mat)) {
    dimnames(mat) <- list(NULL, cnames)
    if (is.null(x$strata)) {
      cat('\n')
      invisible(prmatrix(mat, rowlab = rep('', nrow(mat))))
    } else {
      strata <- x$strata
      if (!is.null(x$start.time))
        strata <- strata[mat.keep]
      invisible(setNames(lapply(levels(strata), function(i) {
        who <- (strata == i)
        cat('\n               ', i, '\n')
        if (sum(who) == 1)
          prmatrix(mat[who, , drop = FALSE])
        else prmatrix(mat[who, , drop = FALSE], rowlab = rep('', sum(who)))
      }), levels(strata)))
    }
  } else stop('There are no events to print. Use the option censored = TRUE ',
              'with the summary function to see the censored observations.')
}

#' Summary table
#' 
#' Prints a formatted summary table for \code{\link{survfit}} objects.
#' 
#' @param s a \code{\link[survival]{survfit}} object
#' @param digits number of digits to use in printing numbers
#' @param times vector of times
#' @param ... additional arguments passed to \code{\link{summary.survfit}}
#' @param maxtime logical; if \code{TRUE}, adds the maximum time for which an
#' even occurs; if \code{FALSE}, number of events may not sum to total
#' 
#' @return
#' A matrix (or list of matrices) with formatted summaries for each strata; see
#' \code{\link{summary.survfit}}
#' 
#' @seealso
#' \code{\link{survfit}}, \code{\link{print.summary.survfit}}
#' 
#' @examples
#' library('survival')
#' fit0 <- survfit(coxph(Surv(time, status == 2) ~ 1,
#'                       data = cancer),
#'                 conf.type = 'log-log')
#' surv_table(fit0, times = 0:2 * 100, maxtime = FALSE)
#' 
#' ## also works for list of tables
#' fit1 <- survfit(coxph(Surv(time, status == 2) ~ strata(I(age > 60)),
#'                       data = cancer),
#'                 conf.type = 'log-log', conf.int = 0.9)
#' surv_table(fit1)
#' 
#' library('htmlTable')
#' s <- `colnames<-`(surv_table(fit0, times = 0:8 * 100, digits = 2)[, -4],
#'                   c('Time','No. at risk','No. of events','Surv (95% CI)'))
#' htmlTable(s, caption = 'Table: Overall survival.')
#' 
#' @export

surv_table <- function(s, digits = 3, times = pretty(s$time),
                       maxtime = TRUE, ...) {
  if (maxtime) {
    idx <- s$n.event > 0
    maxtime <- max(s$time[if (any(idx))
      idx else seq_along(idx) == length(idx)])
    times <- c(times[times <= maxtime], maxtime)
  }
  capture.output(
    summ <- surv_summary(s, digits = digits, times = unique(times), ...)
  )
  f <- function(x, d = digits, vars = vars) {
    vars = colnames(x)
    g <- function(wh, x = x, cn = vars) cn[grepl(wh, cn)]
    tmpvar <- g('survival|std.err|lower|upper')
    x[, tmpvar] <- roundr(x[, tmpvar], digits = d)
    surv <- sprintf('%s (%s, %s)',
                    x[, g('survival')], x[, g('lower')], x[, g('upper')])
    cn <- c('Time','No. at risk','No. event','Std.Error',
            sprintf('Surv (%s%% CI)', s$conf.int * 100))
    `colnames<-`(cbind(x[, c(setdiff(vars, tmpvar), 'std.err'),
                         drop = FALSE], surv), cn)
  }
  if (is.list(summ))
    Map('f', summ) else f(summ)
}

#' Pairwise \code{survdiff} comparisons
#' 
#' Evaluate pairwise group differences in survival curves with
#' \code{\link[survival]{survdiff}}. This function currently works for
#' \emph{one} \code{factor}-like variable, and all unique values are treated
#' as separate groups. To use multiple predictors, create a new variable
#' as the \code{\link{interaction}} of two or more predictors.
#' 
#' @param s an object of class \code{\link[survival]{survdiff}} or
#' \code{\link[survival]{survfit}}
#' @param ... additional arguments passed to \code{\link{survdiff}} such as
#' \code{na.action} or \code{rho} to control the test
#' @param method p-value correction method (default is \code{'holm'}; see
#' \code{\link{p.adjust}}
#' @param digits integer indicating the number of decimal places to be used
#' 
#' @return
#' A list with three elements:
#' \item{\code{n}}{the number of subjects in each pair of groups}
#' \item{\code{chi.sq}}{the chi-square statistic for a test of equality
#' between pairs of groups}
#' \item{\code{p.value}}{significance for each test. The lower and upper
#' triangles of the matrix are uncorrected and adjusted, respectively, for
#' multiple comparisons using \code{method} (the default is the Holm
#' correction, see \code{\link{p.adjust}})}
#' 
#' @seealso
#' \code{\link[rawr]{pvalr}}; \code{\link{survdiff}}; \code{\link{p.adjust}};
#' \code{\link[rms]{contrast}} from the \pkg{\link[rms]{rms}} package;
#' \code{\link{pairwise.table}}
#' 
#' @examples
#' library('survival')
#' sdif <- survdiff(Surv(time, status) ~ sex, data = lung)
#' sfit <- survfit(Surv(time, status) ~ sex, data = lung)
#' 
#' stopifnot(identical(survdiff_pairs(sdif), survdiff_pairs(sfit)))
#'
#'  
#' ## numeric and integer variables will be treated as factor-like
#' sfit <- survfit(Surv(time, status) ~ extent, data = colon)
#' kmplot(sfit)
#' survdiff_pairs(sfit)
#' 
#' ## compare
#' survdiff(Surv(time, status) ~ extent, data = colon[colon$extent %in% 1:2, ])
#' 
#' 
#' ## for interactions, create a new variable with all levels
#' colon$int <- with(colon, interaction(sex, extent))
#' sfit <- survfit(Surv(time, status) ~ int, data = colon)
#' survdiff_pairs(sfit, rho = 1, method = 'BH')
#' 
#' @export

survdiff_pairs <- function(s, ..., method = p.adjust.methods, digits = 3L) {
  method <- match.arg(method)
  stopifnot(inherits(s, c('survdiff', 'survfit')),
            length(rhs <- all.vars(s$call$formula)[-(1:2)]) == 1L)
  data <- eval(s$call$data, envir = parent.frame())
  unq  <- as.character(sort(unique(data[, rhs])))
  
  pwchisq <- function(i, j) {
    data <- data[data[, rhs] %in% c(unq[i], unq[j]), ]
    survdiff(as.formula(s$call$formula), data = data, ...)$chisq
  }
  
  nn <- outer(as.character(unq), as.character(unq), Vectorize(function(x, y)
    nrow(data[data[, rhs] %in% c(x, y), ])))
  nn[upper.tri(nn, FALSE)] <- NA
  
  chisq <- rbind(NA, cbind(pairwise.table(pwchisq, unq, 'none'), NA))
  dimnames(chisq) <- dimnames(nn) <- list(unq, unq)
  
  p.value <- apply(chisq, 1:2, function(x) pchisq(x, 1, lower.tail = FALSE))
  p.value[upper.tri(p.value, FALSE)] <-
    p.adjust(na.omit(c(t(p.value))), method = method)
  
  lapply(list(n = nn, chi.sq  = chisq, p.value = p.value), round, digits)
}

## landmark

#' Landmark
#' 
#' Fit survival curves for landmark time points.
#' 
#' @param s a \code{\link[survival]{survfit}} object
#' @param times a vector of landmark times
#' @param lr_test logical or numeric; if \code{TRUE}, a log-rank test will be
#' performed and the results added to the top-right corner of the plot; if
#' numeric, the value is passed as \code{rho} controlling the type of test
#' performed; see \code{\link{survdiff}}
#' @param adjust_start logical; if \code{TRUE}, each landmark plot will begin
#' at the y-axis
#' @param ... additional parameters passed to \code{\link{kmplot}}
#' @param add logical; if \code{TRUE}, plots are added to current plot
#' 
#' @return
#' A data frame with the sample size, chi-square statistic, degrees of
#' freedom, and p-value for the test (the type of test can be controlled by
#' using a numeric value for \code{lr_test}, passed as \code{rho} to
#' \code{\link{survdiff}}).
#' 
#' @examples
#' library('survival')
#' s <- survfit(Surv(time, status) ~ node4, colon)
#' landmark(s, times = c(500, 1000, 2500))
#' 
#' layout(matrix(c(1,1,1,1,2,3), 2))
#' landmark(s, times = c(500, 2500), adjust_start = TRUE, add = TRUE)
#' 
#' @export

landmark <- function(s, times = NULL, lr_test = TRUE, adjust_start = FALSE,
                     ..., add = FALSE) {
  form <- s$call$formula
  data <- eval(s$call$data)
  tvar <- survival:::terms.inner(form)[1]
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  if (!add) {
    plot.new()
    par(mfrow = n2mfrow(length(times) + 1L))
  }
  
  st <- data.frame(n = sum(s$n), lr_pval(s, TRUE), row.names = 'Total')
  kmplot(s, ..., add = TRUE, lr_test = lr_test,
         panel.first = {
           abline(v = times, col = 2)
           if (length(times))
             mtext(seq_along(times), at = times, cex = .8, col = 2)
         })
  
  sd <- 
    if (length(times))
      lapply(times, function(ii) {
        tmp <- data[data[, tvar] > ii, ]
        if (adjust_start)
          tmp[, tvar] <- tmp[, tvar] - ii
        si <- eval(substitute(survfit(formula, tmp),
                              list(formula = as.formula(form))))
        si$.data <- tmp
        at <- pretty(si$time)
        
        kmplot(si, ..., add = TRUE, lr_test = lr_test, xaxis.at = at,
               xaxis.lab = ifelse(adjust_start, ii, 0) + at,
               panel.first = {
                 abline(v = ii, col = if (adjust_start) 0 else 2)
                 mtext(which(times %in% ii), at = par('usr')[1],
                       col = 2, cex = 1.5, font = 2, line = 1)
               })
        data.frame(n = sum(si$n), lr_pval(si, TRUE), row.names = ii)
      }) else NULL
  
  invisible(do.call('rbind', c(list(st), sd)))
}
