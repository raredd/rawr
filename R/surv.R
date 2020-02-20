### survival
# kmplot, kmplot_by, kmplot_ticks, local_coxph_test, surv_cp, surv_summary,
# surv_table, survdiff_pairs, landmark, surv_extract, surv_median, surv_prob
#
# unexported:
# stratify_formula, points.kmplot, kmplot_data_, terms.inner
# 
# surv_test (unexported):
# lr_text, lr_pval, tt_text, tt_pval, hr_text, hr_pval
###


stratify_formula <- function(formula, vars = NULL) {
  # stratify_formula(Surv(a, b) ~ x, c('y', 'z'))
  if (is.null(vars))
    return(as.formula(formula))
  
  st <- paste(sprintf('strata(%s)', vars), collapse = ' + ')
  ff <- as.character(formula)
  
  as.formula(sprintf('%s ~ %s + %s', ff[2L], ff[3L], st))
}

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
#' @param atrisk.table logical; if \code{TRUE} (default), draws at-risk table
#' @param atrisk.type a character string giving the type of at-risk table to
#' show; one of \code{"atrisk"} (number at-risk), \code{"events"} (cumulative
#' number of events), \code{"atrisk-events"} (both), \code{"survival"}
#' (survival estimate), or \code{"percent"} (percent); \code{"survival-ci"}
#' and \code{"percent-ci"} are similar but add confidence intervals
#' @param atrisk.digits when survival estimates are shown in at-risk table
#' (see \code{atrisk.type}), number of digits past the decimal to show
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
#' table are long (note that this will be overridden by \code{mar})
#' @param mar margins; see \code{mar} section in \code{\link{par}}
#' @param median logical or numeric; if \code{TRUE}, median and confidence
#' interval for each curve is added to at-risk table at a calculated
#' position; for more control, use a specific x-coordinate
#' @param digits.median number of digits past the decimal point to keep for
#' median(s)
#' @param ci.median logical; if \code{TRUE}, confidence interval for medians
#' are shown
#' @param xaxs style of axis; see details or \code{\link{par}}
#' @param xlim,ylim x- and y-axis limits
#' @param xaxis.at,yaxis.at positions for x- and y-axis labels and ticks
#' @param atrisk.at x-coordinates to show at-risk table (default is
#' \code{xaxis.at})
#' @param xaxis.lab,yaxis.lab x- and y-axis tick labels
#' @param xlab,ylab x- and y-axis labels
#' @param main title of plot
#' @param cex.axis text size for axes labels, legend, at-risk table
#' @param legend logical, a vector of x/y coordinates, or a keyword (see
#' \code{\link{legend}}); if \code{TRUE}, the default position is
#' \code{"bottomleft"}
#' @param args.legend an optional \emph{named} list of \code{\link{legend}}
#' arguments controlling the \code{legend}
#' @param lr_test logical or numeric; if \code{TRUE}, a log-rank test will be
#' performed and the results added to the top-right corner of the plot; if
#' numeric, the value is passed as \code{rho} controlling the type of test
#' performed; see \code{\link{survdiff}}
#' @param tt_test logical; if \code{TRUE}, Tarone's trend test will be
#' performed and the resultls added to the top-right corner of the plot; note
#' that this will override \code{lr_test}
#' @param test_details logical; if \code{TRUE} (default), all test details
#' (test statistic, degrees of freedom, p-value) are shown; if \code{FALSE},
#' only the p-value is shown
#' @param args.test an optional \emph{named} list of \code{\link{mtext}}
#' arguments controlling the \code{*_test} text
#' @param hr_text logical; if \code{TRUE}, a \code{\link{coxph}} model is fit,
#' and a summary (hazard ratios, confidence intervals, and Wald p-values)
#' is shown
#' @param args.hr an optional \emph{named} list of \code{\link{legend}}
#' arguments controlling the \code{hr_text} legend
#' @param pw_test logical; if \code{TRUE}, all pairwise tests of survival
#' curves are performed, and p-valuees are shown
#' @param args.pw an optional \emph{named} list of \code{\link{legend}}
#' arguments controlling the \code{pw_text} legend
#' @param format_pval logical; if \code{TRUE}, p-values are formatted with
#' \code{\link{pvalr}}; if \code{FALSE}, no formatting is performed;
#' alternatively, a function can be passed which should take a numeric value
#' and return a character string (or a value to be coerced) for printing
#' @param stratify (dev) \code{\link[survival]{strata}} variables
#' @param fun survival curve transformation, one of \code{"S"} or \code{"F"}
#' for the usual survival curve or empirical CDF, respectively
#' @param add logical; if \code{TRUE}, \code{par}s are not reset; allows for
#' multiple panels, e.g., when using \code{par(mfrow = c(1, 2))}
#' @param panel.first an expression to be evaluated after the plot axes are
#' set up but before any plotting takes place
#' @param panel.last an expression to be evaluated after plotting but before
#' returning from the function
#' @param ... additional parameters (\code{font}, \code{mfrow}, \code{bty},
#' \code{tcl}, \code{cex.lab}, etc) passed to \code{par}
#' 
#' @references
#' \url{http://biostat.mc.vanderbilt.edu/wiki/Main/TatsukiRcode}
#' 
#' @seealso
#' \code{\link{kmplot_by}}; \code{survival:::plot.survfit};
#' \code{\link[plotr]{ggsurv}}; \code{\link{kmplot_data_}};
#' \code{\link{lr_text}}; \code{\link{lr_pval}}; \code{\link{points.kmplot}}
#' 
#' @examples
#' library('survival')
#' km1 <- survfit(Surv(time, status) ~ sex, data = colon)
#' km2 <- survfit(Surv(time, status) ~ I(rx == "Obs") + adhere, data = colon)
#' 
#' ## basic usage
#' kmplot(km1)
#' kmplot(km1, fun = 'F')
#' kmplot(km1, atrisk.col = c('grey50','tomato'),
#'        args.test = list(col = 'red', cex = 1.5, line = 0))
#' kmplot(km1, mark = 'bump', atrisk.lines = FALSE, median = TRUE)
#' kmplot(km1, mark = 'bump', atrisk.lines = FALSE, median = 3700)
#' kmplot(km2, atrisk.table = FALSE, lwd.surv = 2, lwd.mark = .5,
#'        col.surv = 1:4, col.band = c(1,0,0,4))
#' 
#' 
#' ## use stratified models
#' kmplot(km1, stratify = 'differ')
#' kmplot(km1, stratify = c('differ', 'adhere'))
#' 
#' 
#' ## for hazard ratios, use factors to fit the proper cox model
#' kmplot(survfit(Surv(time, status) ~ factor(sex), data = colon),
#'        hr_text = TRUE, strata.lab = TRUE)
#' kmplot(survfit(Surv(time, status) ~ interaction(sex, rx), data = colon),
#'        hr_text = TRUE, atrisk.table = FALSE, legend = FALSE)
#' kmplot(survfit(Surv(time, status) ~ interaction(sex, rx), data = colon),
#'        hr_text = TRUE, atrisk.table = FALSE, legend = FALSE,
#'        format_pval = function(x) format(x, digits = 2, scipen = 0))
#' 
#' 
#' ## at-risk and p-value options
#' kmplot(km2, tt_test = TRUE, test_details = FALSE)
#' kmplot(km2, tt_test = TRUE, format_pval = format.pval)
#' kmplot(km1, atrisk.type = 'survival', atrisk.digits = 3L)
#' kmplot(km1, atrisk.type = 'atrisk-events')
#' kmplot(survfit(Surv(time, status) ~ rx, data = colon),
#'        pw_test = TRUE, args.pw = list(text.col = 1:3, x = 'bottomleft'))
#' 
#' ## expressions in at-risk table (strata.expr takes precedence)
#' kmplot(km1, strata.lab = c('\u2640', '\u2642'))
#' kmplot(km1, strata.lab = c('\u2640', '\u2642'),
#'                strata.expr = expression(widetilde(ring(Female)),
#'                                         phantom() >= Male))
#' 
#' ## character vectors passed to strata.expr will be parsed
#' kmplot(km1, strata.expr = c('Sex[Female]', 'Sex[Male]'))
#' ## but not those passed to strata.lab
#' kmplot(km1, strata.lab  = c('Sex[Female]', 'Sex[Male]'))
#' 
#' 
#' ## when using mfrow options, use add = TRUE and same mar to align axes
#' mar <- c(8, 6, 2, 2)
#' par(mfrow = c(1, 2))
#' kmplot(km1, add = TRUE, mar = mar)
#' kmplot(km2, add = TRUE, strata.lab = TRUE, mar = mar)
#' 
#' 
#' \dontrun{
#' ## more customized figure
#' pdf(tf <- tempfile(fileext = '.pdf'), height = 8, width = 11,
#'     pointsize = 12, family = 'serif')
#' 
#' kmplot(
#'   survfit(Surv(time, status) ~ rx + adhere, data = colon),
#'   panel.first = abline(v = c(0, 0.5, 1:9) * 365, lty = 3),
#'   mark = 'bump',                     # bump censor mark
#'   lty.ci = 2, lwd.ci = 0.3,          # dashed line for CIs
#'   xaxis.at = c(0, 0.5, 1:9) * 365,   # change days to years
#'   xaxis.lab = c(0, 0.5, 1:9),        # label years
#'   yaxis.lab = pretty(0:1) * 100,     # change to percent
#'   xlab = 'Time (years)',
#'   ylab = 'Percent survival',
#'   lr_test = TRUE, test_details = FALSE, # custom test output
#'   args.test = list(line = -2, col = 'red', cex = 2, at = 11 * 365),
#'   col.surv = c('blue', 'red', 'green', 'black', 'purple', 'orange'),
#'   col.ci   = c(0,0,0,0,'purple',0),  # CI only for one group
#'   extra.margin = 6,        # increase margin for long strata labels
#'   strata.lab = c('Obs', 'Obs+', 'Lev', 'Lev+', 'Lev5fu', 'Lev5fu+'),
#'   strata.order = c(5,6,3,1,4,2),     # order table by curve positions
#'   median = 10.5 * 365,               # add median and CI
#'   atrisk.col = TRUE,                 # color at-risk text
#'   font = 2, bty = 'l', tcl = 0.5     # bold table text, other options
#' )
#' title(main = 'Chemotherapy for stage B/C colon cancer', line = 2.5)
#' 
#' dev.off()
#' 
#' system2(getOption('pdfviewer'), tf)
#' unlink(tf)
#' }
#' 
#' @export

kmplot <- function(s,
                   ## basic plot options
                   lty.surv = par('lty'), lwd.surv = par('lwd'),
                   col.surv = seq_along(s$n), mark = 3L, lwd.mark = lwd.surv,
                   
                   ## confidence options
                   lty.ci = 0, lwd.ci = lwd.surv,
                   col.ci = col.surv, col.band = FALSE,
                   
                   ## at-risk table options
                   atrisk.table = TRUE, atrisk.lab = NULL,
                   atrisk.type = c('atrisk', 'events', 'atrisk-events',
                                   'survival', 'survival-ci',
                                   'percent', 'percent-ci'),
                   atrisk.digits = 2L,
                   atrisk.lines = TRUE, atrisk.col = !atrisk.lines,
                   strata.lab = NULL,
                   strata.expr = NULL, strata.order = seq_along(s$n),
                   extra.margin = 5, mar = NULL,
                   median = FALSE, digits.median = 0L, ci.median = TRUE,
                   
                   ## aesthetics
                   xaxs = 's', xlim = NULL, ylim = NULL,
                   xaxis.at = pretty(xlim), xaxis.lab = xaxis.at,
                   atrisk.at = xaxis.at,
                   yaxis.at = pretty(0:1), yaxis.lab = yaxis.at,
                   xlab = 'Time', ylab = 'Probability',
                   main = NULL, cex.axis = par('cex.axis'),
                   legend = !atrisk.table && !is.null(s$strata),
                   args.legend = list(),
                   
                   ## test/hazard ratio options
                   lr_test = TRUE, tt_test = FALSE, test_details = TRUE,
                   args.test = list(),
                   hr_text = FALSE, args.hr = list(),
                   pw_test = FALSE, args.pw = list(),
                   format_pval = TRUE,
                   
                   ## other options
                   stratify = NULL, fun = c('S', 'F'),
                   add = FALSE, panel.first = NULL, panel.last = NULL, ...) {
  if (!inherits(s, 'survfit'))
    stop('\'s\' must be a \'survfit\' object')
  
  if (length(form <- s$call$formula) == 1L)
    s$call$formula <- as.formula(eval(form, parent.frame(1L)))
  
  if (is.logical(lr_test)) {
    rho <- 0
  } else {
    rho <- if (is.numeric(lr_test))
      lr_test else 0
    lr_test <- TRUE
  }
  if (tt_test)
    lr_test <- FALSE
  
  fun <- match.arg(fun)
  
  svar <- as.character(form <- s$call$formula)
  svar <- svar[-(1:2)]
  
  ## formula with strata (if given), used for tests/coxph
  sform <- stratify_formula(form, stratify)
  
  sdat <- s$.data %||% {
    sdat <- deparse(s$call$data)
    sname <- gsub('[$[].*', '', sdat)
    tryCatch(if (identical(sdat, sname))
      get(sdat, where(sname)) else eval(parse(text = sdat), where(sname)),
      error = function(e) NULL)
  }
  ## remove missing here for special case: all NA for one strata level
  ## drops level in s$strata but not in table(sdat[, svar])
  if (!is.null(sdat))
    sdat <- na.omit(sdat[, c(all.vars(form), stratify)])
  
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
  
  col.surv <- if (is.null(col.surv)) {
    if (isTRUE(col.band) || all(!col.band))
      seq_along(s$n)
    else rep_len(col.band, ng)
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
    svar <- colnames(model.frame(form, sdat)[, -1L, drop = FALSE])
    cl <- c(list(survival::strata),
            lapply(svar, as.symbol),
            shortlabel = TRUE)
    mode(cl) <- 'call'
    names(s$strata) <- levels(eval(cl, model.frame(form, sdat)))
  }
  
  if (!is.null(strata.lab) && isTRUE(strata.lab))
    strata.lab <- NULL
  if (!is.null(strata.lab) && identical(strata.lab, FALSE))
    strata.lab <- rep_len('', ng)
  if (is.null(strata.lab))
    strata.lab <- names(s$strata)
  if (length(strata.lab) != ng && strata.lab[1L] != FALSE) {
    strata.lab <- strata.lab[seq.int(ng)]
    warning('length(strata.lab) != number of groups')
  }
  if (suppressWarnings(any(sort(strata.order) != seq.int(ng))))
    stop('sort(strata.order) must equal 1:', ng)
  if (ng == 1L & (strata.lab[1L] == 'strata.lab')) {
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
  
  ## run thru tcol to convert integers and strings? idk but
  ## running a string with alpha trans resets to no alpha/trans
  ## so skip if already a hex color with alpha/trans
  if (!any(grepl('(?i)#[a-z0-9]{8}', col.surv)))
    col.surv <- tcol(col.surv)
  if (!any(grepl('(?i)#[a-z0-9]{8}', col.band)))
    col.band <- tcol(col.band)
  
  if (!identical(median, FALSE)) {
    median.at <- median
    median <- TRUE
  }
  
  op <- par(no.readonly = TRUE)
  if (!add)
    on.exit(par(op))

  ## guess margins based on atrisk table options
  par(mar = c(4 + ng * atrisk.table,
              4 + pmax(4, extra.margin) - 3 * !atrisk.table,
              2,
              2 + 6 * (median & atrisk.table)))
  par(...)
  if (!is.null(mar))
    par(mar = mar)
  
  if (is.null(xlim))
    xlim <- c(0, max(s$time))
  oxlim <- xlim
  if (is.null(ylim))
    ylim <- c(0, 1.025)
  
  ## if default *lim not used, get new *-axis positions
  if (is.null(xaxis.at))
    xaxis.at <- pretty(xlim)
  if (is.null(yaxis.at))
    yaxis.at <- pretty(ylim)
  
  ## as in survival:::plot.survfit, adjust x-axis to start at 0
  xaxs <- if (tolower(xaxs)[1L] == 's') {
    xlim[2L] <- xlim[2L] * 1.04
    'i'
  } else 'r'
  
  ## reformat survival estimates
  dat <- kmplot_data_(s, strata.lab)
  dat.list <- split(dat, dat$order)
  
  ## base plot
  plot(
    0, type = 'n', xlim = xlim, ylim = ylim, ann = FALSE,
    axes = FALSE, xaxs = xaxs, panel.first = panel.first,
    panel.last = {
      box(bty = par('bty'))
      axis(1L, xaxis.at, FALSE, lwd = 0, lwd.ticks = 1)
      axis(1L, xaxis.at, xaxis.lab, FALSE, -0.5, cex.axis = cex.axis)
      axis(2L, yaxis.at, yaxis.lab, las = 1L, cex.axis = cex.axis)
      title(xlab = xlab, line = 1.5, adj = 0.5, ...)
      title(ylab = ylab, main = main, ...)
    }
  )
  
  ## at-risk table below surv plot
  if (atrisk.table) {
    usr <- par('usr')
    
    atrisk.at <- atrisk.at[atrisk.at <= usr[2L]]
    
    ## set colors for lines of text
    col.atrisk <- if (isTRUE(atrisk.col))
      col.surv else if (!identical(atrisk.col, FALSE) && length(atrisk.col) == ng)
        atrisk.col else rep_len(1L, ng)
    
    ## labels for each row in at-risk table
    group.name.pos <- diff(usr[1:2]) / -8
    padding  <- abs(group.name.pos / 8)
    line.pos <- seq.int(ng)[order(strata.order)] + 2L
    
    if (!identical(unique(strata.lab), FALSE)) {
      if (!is.null(strata.expr)) {
        if (is.character(strata.expr))
          strata.expr <- parse(text = strata.expr)
        sapply(seq.int(length(strata.expr)), function(x)
          mtext(strata.expr[[x]], side = 1L, line = line.pos[x], adj = 1,
                at = group.name.pos, col = col.atrisk[x], las = 1L,
                cex = cex.axis))
      } else mtext(strata.lab, side = 1L, line = line.pos, adj = 1, las = 1L,
                   col = col.atrisk, at = group.name.pos, cex = cex.axis)
    }
    
    ## draw matching lines for n at risk  
    if (atrisk.lines)
      for (ii in seq.int(ng))
        ## mess with the 4 here to adjust the length of the atrisk.line
        axis(1L, c(group.name.pos + padding, 0 - 4 * padding), xpd = NA,
             labels = FALSE, line = line.pos[ii] + 0.6, lwd.ticks = 0,
             col = col.surv[ii], lty = lty.surv[ii], lwd = lwd.surv[ii])
    
    ## at-risk table
    atrisk.type <- match.arg(atrisk.type)
    wh <- switch(
      atrisk.type,
      atrisk = 'n.risk',
      events = 'events',
      'atrisk-events' = 'atrisk-events',
      survival = 'survival',
      'survival-ci' = 'survival.ci',
      percent = 'percent',
      'percent-ci' = 'percent.ci'
    )
    
    ss <- summary(s, times = atrisk.at)
    if (is.null(ss$strata))
      ss$strata <- rep_len(1L, length(ss$time))
    d1 <- data.frame(
      time = ss$time, n.risk = ss$n.risk, n.event = ss$n.event,
      strata = c(ss$strata), surv = ss$surv, lci = ss$lower, uci = ss$upper
    )
    
    mi <- missing(atrisk.digits)
    d2 <- split(d1, d1$strata)
    d2 <- lapply(d2, function(x) {
      if (fun == 'F')
        x$surv <- 1 - x$surv
      
      x$atrisk <- x$n.risk
      x$events <- cumsum(x$n.event)
      x[, 'atrisk-events'] <- sprintf('%s (%s)', x$atrisk, x$events)
      
      x$survival <- roundr(x$surv, atrisk.digits)
      x$survival.ci <- sprintf(
        '%s (%s, %s)', x$survival,
        roundr(x$lci, atrisk.digits),
        roundr(x$uci, atrisk.digits)
      )
      
      x$percent  <- roundr(x$surv * 100, ifelse(mi, 0L, atrisk.digits))
      x$percent.ci <- sprintf(
        '%s (%s, %s)', x$percent,
        roundr(x$lci * 100, ifelse(mi, 0L, atrisk.digits)),
        roundr(x$uci * 100, ifelse(mi, 0L, atrisk.digits))
      )
      
      if (0 %in% atrisk.at) {
        zeros <- x[x$time %in% 0, c('survival.ci', 'percent.ci')]
        x[x$time %in% 0, c('survival.ci', 'percent.ci')] <-
          gsub('\\s+.*', '', zeros)
      }
      
      x
    })
    
    ## right-justify numbers
    ndigits <- lapply(d2, function(x) nchar(x[, 2L]))
    max.len <- max(sapply(ndigits, length))
    L <- do.call('rbind', lapply(ndigits, `length<-`, max.len))
    nd <- apply(L, 2L, max, na.rm = TRUE)
    
    for (ii in seq.int(ng)) {
      tmp <- d2[[ii]]
      w.adj <- strwidth('0', cex = cex.axis, font = par('font')) /
        2 * nd[seq.int(nrow(tmp))]
      
      ## right-justify
      right <- c('atrisk', 'events', 'atrisk-events')
      right <- ''
      
      mtext(
        tmp[, wh], side = 1L, col = col.atrisk[ii],
        las = 1L, line = line.pos[ii], cex = cex.axis,
        # at = tmp$time + w.adj, adj = 1,
        # at = tmp$time + w.adj * atrisk.type %ni% right,
        # adj = 1 - 0.5 * atrisk.type %in% right
        at = tmp$time, adj = if (atrisk.type %in% right) 1 else 0.5
      )
    }
    
    if (is.null(atrisk.lab))
      atrisk.lab <- switch(
        atrisk.type,
        atrisk = 'Number at risk',
        events = 'Cumulative events',
        'atrisk-events' = 'At risk (Events)',
        survival = 'Probability',
        'survival-ci' = sprintf('Probability (%s%% CI)', ss$conf.int * 100),
        percent = 'Percent',
        'percent-ci' = sprintf('Percent (%s%% CI)', ss$conf.int * 100)
      )
    
    if (!(identical(atrisk.lab, FALSE)))
      mtext(atrisk.lab, side = 1L, at = usr[1L], # at = group.name.pos,
            line = 1.5, adj = 1, col = 1L, las = 1L, cex = cex.axis)
    
    ## median (ci) text on right of at-risk
    if (median) {
      st <- ss$table
      st <- if (length(s$n) != 1L)
        as.data.frame(st) else as.data.frame(t(st))
      st <- st[, grep('(?i)median|[ul]cl', names(st)), drop = FALSE]
      tt <- if (ncol(st) < 3L)
        roundr(st[, 'median'], digits.median) else
          do.call('sprintf', c(list(
            fmt = '%s (%s, %s)'), lapply(st, roundr, digits = digits.median))
          )
      tt <- ifelse(
        # is.na(st$median)
        rowSums(is.na(st)) == ncol(st),
        '-', gsub('NA', '-', tt, fixed = TRUE)
      )
      if (!ci.median) {
        s$conf.int <- NULL
        tt <- gsub('\\s*\\(.*$', '', tt)
      }
      at <- if (isTRUE(median.at))
        usr[2L] + diff(usr[1:2]) / 8 else median.at
      mtext(if (!is.null(s$conf.int))
        sprintf('Median (%s%% CI)', s$conf.int * 100) else 'Median',
        side = 1L, at = at, adj = .5, line = 1.5, col = 1L, las = 1L
      )
      mtext(tt, side = 1L, line = line.pos, las = 1L,
            at = at, adj = 0.5, col = col.atrisk)
    }
  }
  
  ## legend
  if (!identical(legend, FALSE)) {
    ## defaults passed to legend
    largs <- list(
      x = if (isTRUE(legend)) 'bottomleft' else legend[1L],
      y = if (length(legend) > 1L) legend[2L] else NULL,
      legend = if (!is.null(strata.expr))
        strata.expr[strata.order] else
          (if (identical(strata.lab, FALSE) || all(strata.lab %in% ''))
            names(s$strata) else strata.lab)[strata.order],
      col = col.surv[strata.order], bty = 'n',
      lty = lty.surv[strata.order], lwd = lwd.surv[strata.order]
    )
    
    if (!islist(args.legend))
      args.legend <- list()
    do.call('legend', modifyList(largs, args.legend))
  }
  
  ## hazard ratios
  if (!identical(hr_text, FALSE)) {
    txt <- tryCatch(
      hr_text(sform, sdat, pFUN = format_pval),
      error = function(e) ''
    )
    
    if (length(nchar(na.omit(txt))) != ng)
      warning(
        'Number of strata levels does not equal number of HR estimates:\n',
        '\tRHS of formula should be one factor with all combinations of\n',
        '\tthe strata levels (eg, see ?interaction)',
        call. = FALSE
      )
    
    largs <- list(
      x = 'bottomleft', y = NULL, bty = 'n',
      legend = txt[strata.order], col = col.surv[strata.order],
      lty = lty.surv[strata.order], lwd = lwd.surv[strata.order]
    )
    
    if (!islist(args.hr))
      args.hr <- list()
    do.call('legend', modifyList(largs, args.hr))
  }
  
  ## pairwise tests
  if (!identical(pw_test, FALSE)) {
    txt <- pw_text(sform, sdat, pFUN = format_pval)
    
    largs <- list(x = 'topright', legend = txt, bty = 'n')
    
    if (!islist(args.pw))
      args.pw <- list()
    do.call('legend', modifyList(largs, args.pw))
  }
  
  ## survival and confidence lines
  u0 <- u1 <- par('usr')
  u1[2L] <- oxlim[2L] * 1.01
  do.call('clip', as.list(u1))

  for (ii in seq.int(ng)) {
    tmp <- dat.list[[ii]]
    if (nrow(tmp) < 2L) {
      if (any(!is.na(col.band)))
        message('Note: strata level with one observation - no CI plotted.')
    } else {
      x <- tmp$time
      L <- tmp$lower
      U <- tmp$upper
      if (is.na(L[1L]))
        L[1L] <- 1
      if (is.na(U[1L]))
        U[1L] <- 1
      S <- tmp$survival
      naL <- which(is.na(L))
      L[naL] <- L[naL - 1L]
      U[naL] <- U[naL - 1L]
      
      lines(x, L, type = 's', col = col.ci[ii],
            lty = lty.ci[ii], lwd = lwd.ci[ii])
      lines(x, U, type = 's', col = col.ci[ii],
            lty = lty.ci[ii], lwd = lwd.ci[ii])
      
      ## confidence bands
      if (any(!is.na(col.band))) {
        col.band[ii] <- tcol(col.band[ii], alpha = .5)
        polygon(c(x, rev(x)), c(U, rev(L)),
                border = NA, col = col.band[ii])
      }
    }
    
    ## survival curves
    lines(s[ii], conf.int = FALSE, col = col.surv[ii], fun = fun,
          lty = lty.surv[ii], lwd = lwd.surv, mark.time = FALSE)
    
    if (!mark[ii] == FALSE)
      points.kmplot(
        s[ii], col = col.surv[ii], pch = mark[ii], censor = TRUE, plot = TRUE,
        event = FALSE, bump = mark[ii] == 'bump', lwd = lwd.mark[ii], fun = fun
      )
  }
  
  do.call('clip', as.list(u0))
  
  ## add test text in upper right corner
  if (lr_test | tt_test) {
    FUN <- if (tt_test)
      function(f, d, r, ...) tt_text(f, d, ...)
    else lr_text
    txt <- if (identical(svar, '1'))
      '' else
        tryCatch(
          FUN(sform, sdat, rho, details = test_details, pFUN = format_pval),
          error = function(e) 'n/a'
        )
    if (identical(txt, FALSE))
      message('There is only one group for ', svar, ' -- no test performed')
    else {
      largs <- alist(
        text = txt, side = 3L, at = par('usr')[2L], adj = 1, line = 0.25
      )
      
      if (!islist(args.test))
        args.test <- list()
      do.call('mtext', modifyList(largs, args.test))
    }
  }
  
  panel.last
  
  invisible(dat)
}

#' \code{kmplot} points
#' 
#' \code{survival:::points.survfit} with additional features.
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
  if (inherits(x, 'survfitms')) {
    x$surv <- 1 - x$pstate
    if (is.matrix(x$surv)) {
      dimnames(x$surv) <- list(NULL, x$states)
      if (ncol(x$surv) > 1 && any(x$states == '')) {
        x$surv <- x$surv[, x$states != '']
        if (is.matrix(x$p0)) 
          x$p0 <- x$p0[, x$states != '']
        else x$p0 <- x$p0[x$states != '']
      }
    }
    if (!is.null(x$lower)) {
      x$lower <- 1 - x$lower
      x$upper <- 1 - x$upper
    }
    if (missing(fun)) 
      fun <- 'event'
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
    keepx  <- keepy <- NULL ## lines to keep
    tempn  <- table(stemp)
    offset <- cumsum(c(0, tempn))
    for (ii in seq.int(nstrat)) {
      ttime <- stime[stemp == ii]
      if (all(ttime <= xmax)) {
        keepx <- c(keepx, seq.int(tempn)[ii] + offset[ii])
        keepy <- c(keepy, seq.int(tempn)[ii] + offset[ii])
      } else {
        bad <- min((seq.int(tempn)[ii])[ttime > xmax])
        if (bad == 1)  { ## lost them all
          if (!is.na(firstx)) { ## and we are plotting lines
            keepy <- c(keepy, 1 + offset[ii])
            ssurv[1 + offset[ii], ] <- firsty[ii, ]
          }
        } else  keepy <- c(keepy, c(seq.int(bad - 1), bad - 1) + offset[ii])
        keepx <- c(keepx, (seq.int(bad)) + offset[ii])
        stime[bad + offset[ii]] <- xmax
        x$n.event[bad + offset[ii]] <- 1 ## don't plot a tick mark
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
      tfun <- switch(
        fun,
        'log'      = function(x) x,
        'event'    = function(x) 1 - x,
        'cumhaz'   = function(x) -log(x),
        'cloglog'  = function(x) log(-log(x)),
        'pct'      = function(x) x * 100,
        ## special case further below
        'logpct'   = function(x) 100 * x,
        'identity' = function(x) x,
        'S'        = function(x) x,
        'F'        = function(x) 1 - x,
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
  
  res <- cbind(stime = stime,
               ssurv = if (ncol(ssurv) == 1L) c(ssurv) else ssurv,
               n.event = x$n.event)
  if (!plot)
    return(res)
  
  if (ncurve == 1L || length(col) == 1L) {
    if (censor) {
      st <- stime[x$n.event == 0]
      ss <- ssurv[x$n.event == 0]
      if (bump)
        segments(st, ss, st, ss + diff(par('usr')[3:4]) / 100,
                 col = col, ...)
      else points(st, ss, col = col, pch = pch, ...)
    }
    if (event) {
      st <- stime[x$n.event > 0]
      ss <- ssurv[x$n.event > 0]
      if (bump)
        segments(st, ss, st, ss + diff(par('usr')[3:4]) / 100,
                 col = col, ...)
      else points(st, ss, col = col, pch = pch, ...)
    }
  } else {
    c2 <- 1
    col <- rep(col, length = ncurve)
    if (!missing(pch))
      pch2 <- rep(pch, length = ncurve)
    for (jj in seq.int(ncol(ssurv))) {
      for (ii in unique(stemp)) {
        if (censor) {
          who <- which(stemp == ii & x$n.event == 0)
          st <- stime[who]
          ss <- ssurv[who, jj]
          if (bump)
            segments(st, ss, st, ss + diff(par('usr')[3:4]) / 100,
                     col = col, ...)
          else points(st, ss, col = col[c2], pch = pch2[c2], ...)
        }
        if (event) {
          who <- which(stemp == ii & x$n.event > 0)
          st <- stime[who]
          ss <- ssurv[who, jj]
          if (bump)
            segments(st, ss, st, ss + diff(par('usr')[3:4]) / 100,
                     col = col, ...)
          else points(st, ss, col = col[c2], pch = pch2[c2], ...)
        }
        c2 <- c2 + 1L
      }
    }
  }
  
  invisible(res)
}

#' Create data frame to plot survival data
#' 
#' @param s a \code{\link{survfit}} object
#' @param strata.lab character vector of strata labels; must be same length
#' as \code{s$n}
#' @seealso \code{\link{kmplot}}; \code{\link{kmplot_by}}

kmplot_data_ <- function(s, strata.lab) {
  stopifnot(inherits(s, 'survfit'))
  gr <- c(s$strata)
  ng <- max(length(gr), 1L)
  
  ## if conf.type = 'none', upper/lower defined as s$surv for plotting
  if (is.null(s$lower))
    s <- c(s, list(lower = s$surv))
  if (is.null(s$upper))
    s <- c(s, list(upper = s$surv))
  
  with(s, {
    data.frame(
      time, n.risk, n.event, survival = surv, lower, upper,
      group = rep(strata.lab, gr), order = rep(seq.int(ng), gr)
    )
  })
}

#' Survival curve tests
#' 
#' @description
#' Internal functions for \code{\link{survdiff}} and \code{\link{survfit}}
#' objects. Current methods include log-rank (\code{lr_*}) and pairwise
#' (\code{pw_*}) log-rank tests (by default although the exact test may be
#' controlled with the \code{rho} parameter passed to \code{survdiff}); a
#' trend test described by Tarone (\code{tt_*}); and Wald tests of
#' coefficients in a Cox regression (\code{hr_*}).
#' 
#' \code{*_pval} functions take (\code{survfit} or \code{survdiff}) objects
#' or formulas and compute test statistics, p-values, etc. and return a
#' numeric vector or list.
#' 
#' \code{*_text} functions format the test results for plotting and return
#' an expression or vector of character strings.
#' 
#' Note that \code{pw_pval} and \code{pw_text} do not support formulas
#' with more than one predictor, e.g., \code{y ~ a + b}. An equivalent
#' formula is acceptable, e.g., \code{y ~ x} where \code{x} is the
#' \code{interaction(a, b)} or similar. See \code{\link{survdiff_pairs}}
#' for more details and examples.
#' 
#' @param formula,data,rho,... passed to \code{\link{survdiff}} or
#' \code{\link{coxph}}
#' @param object a \code{\link{survfit}}, \code{\link{survdiff}}, or
#' \code{\link{coxph}} object; alternatively a \code{\link[=Surv]{survival
#' formula}} (\code{data} must be given)
#' @param details logical; \code{TRUE} returns statistic, degrees of freedom,
#' and p-value where \code{FALSE} returns only a pvalue
#' @param pFUN logical; if \code{TRUE}, p-values are formatted with
#' \code{\link{pvalr}}; if \code{FALSE}, no formatting is performed;
#' alternatively, a function can be passed which should take a numeric value
#' and return a character string (or a value to be coerced) for printing
#' @param method for \code{pw_*}, the method used to adjust p-values for
#' multiple comparisons (default is \code{"none"}); see
#' \code{\link{p.adjust.methods}}
#' 
#' @references
#' Tarone, Robert E. Tests for Trend in Life Table Analysis. \emph{Biometrika}
#' \strong{62} vol. 62 (Dec 1975), 679-82.
#' 
#' \url{https://stat.ethz.ch/pipermail/r-help/2008-April/160209.html}
#' 
#' @seealso
#' \code{\link{survdiff_pairs}}
#' 
#' @examples
#' \dontrun{
#' library('survival')
#' data('larynx', package = 'KMsurv')
#' larynx$stage <- factor(larynx$stage)
#' 
#' form <- Surv(time, delta) ~ stage
#' sf <- survfit(form, larynx)
#' sd <- survdiff(form, larynx)
#' 
#' kmplot(sf, lr_test = TRUE)
#' 
#' 
#' ## log-rank
#' rawr:::lr_pval(sf)
#' rawr:::lr_pval(sd, TRUE)
#' rawr:::lr_text(Surv(time, delta) ~ stage, larynx)
#' 
#' 
#' ## pairwise log-rank
#' sf$call$formula <- form
#' rawr:::pw_pval(sf)
#' rawr:::pw_text(sf)
#' 
#' 
#' ## tarone trend
#' rawr:::tt_pval(sf)
#' rawr:::tt_pval(sd, TRUE)
#' rawr:::tt_text(Surv(time, delta) ~ stage, larynx)
#' 
#' ## compare
#' chi <- coxph(Surv(time, delta) ~ stage, larynx)$score
#' list(chi = chi, p.value = pchisq(chi, 1, lower.tail = FALSE))
#' 
#' 
#' ## hazard ratio/wald p-values
#' rawr:::hr_pval(sf)
#' rawr:::hr_pval(sd, TRUE)
#' rawr:::hr_text(Surv(time, delta) ~ stage, larynx)
#' }
#' 
#' @name surv_test

#' @rdname surv_test
lr_pval <- function(object, details = FALSE, data = NULL, ...) {
  object <- if (inherits(object, 'survfit')) {
    if (length(form <- object$call$formula) == 1L)
      object$call$formula <- eval(object$call$formula, parent.frame(1L))
    survdiff(as.formula(object$call$formula),
             eval(data %||% object$.data %||% object$call$data))
  } else if (inherits(object, c('formula', 'call')))
    survdiff(object, data, ...)
  else object
  
  stopifnot(
    inherits(object, 'survdiff')
  )
  
  chi <- object$chisq
  df  <- length(object$n) - 1L
  pv  <- pchisq(chi, df, lower.tail = FALSE)
  
  if (details)
    list(chisq = chi, df = df, p.value = pv)
  else pv
}

#' @rdname surv_test
lr_text <- function(formula, data, rho = 0, ..., details = TRUE, pFUN = NULL) {
  pFUN <- if (is.null(pFUN) || isTRUE(pFUN))
    function(x) pvalr(x, show.p = TRUE)
  else if (identical(pFUN, FALSE))
    identity else match.fun(pFUN)
  
  object <- if (inherits(formula, 'survdiff'))
    formula
  else if (inherits(formula, 'survfit'))
    survdiff(as.formula(formula$call$formula),
             if (!missing(data)) data else eval(formula$call$data), ...)
  else survdiff(formula, data, rho = rho, ...)
  
  capture.output(
    ## Warning message: In pchisq(x$chisq, df) : NaNs produced
    ## this warning is only produced during print.survdiff
    sd <- tryCatch(
      print(object),
      warning = function(w) '',
      error   = function(e) {
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
  
  ## does not work with stratified models
  # df <- sum(1 * (colSums(if (is.matrix(sd$obs))
  #   sd$exp else t(sd$exp)) > 0)) - 1
  
  df <- length(sd$n) - 1L
  pv <- pchisq(sd$chisq, df, lower.tail = FALSE)
  
  txt <- sprintf('%s (%s df), %s', roundr(sd$chisq, 1L), df, pFUN(pv))
  
  if (details)
    bquote(paste(chi^2, ' = ', .(txt)))
  else pFUN(pv)
}

#' @rdname surv_test
tt_pval <- function(object, details = FALSE, data = NULL, ...) {
  object <- if (inherits(object, c('survdiff', 'survfit'))) {
    if (length(form <- object$call$formula) == 1L)
      object$call$formula <- eval(object$call$formula, parent.frame(1L))
    coxph(as.formula(object$call$formula),
          eval(data %||% object$.data %||% object$call$data))
  } else if (inherits(object, c('formula', 'call')))
    coxph(object, data, ...)
  else object
  
  stopifnot(
    inherits(object, 'coxph')
  )
  
  chi <- object$score
  pv <- pchisq(chi, 1L, lower.tail = FALSE)
  
  if (details)
    list(chisq = chi, df = 1L, p.value = pv)
  else pv
}

#' @rdname surv_test
tt_text <- function(formula, data, ..., details = TRUE, pFUN = NULL) {
  pFUN <- if (is.null(pFUN) || isTRUE(pFUN))
    function(x) pvalr(x, show.p = TRUE)
  else if (identical(pFUN, FALSE))
    identity else match.fun(pFUN)
  
  object <- if (inherits(formula, 'coxph'))
    formula
  else if (inherits(formula, 'survfit'))
    coxph(as.formula(formula$call$formula),
          if (!missing(data)) data else eval(formula$call$data), ...)
  else formula
  
  cph <- tryCatch(
    if (inherits(object, 'coxph'))
      object else coxph(formula, data, ...),
    warning = function(w) '',
    error   = function(e) e
  )
  
  if (isTRUE(cph))
    return(FALSE)
  if (identical(cph, ''))
    return(cph)
  if (!inherits(cph, 'coxph'))
    stop(cph)
  
  chi <- cph$score
  pv  <- pchisq(chi, 1L, lower.tail = FALSE)
  txt <- sprintf('%s (1 df), %s', roundr(chi, 1L), pFUN(pv))
  
  if (details)
    bquote(paste(chi^2, ' = ', .(txt)))
  else pFUN(pv)
}

#' @rdname surv_test
hr_pval <- function(object, details = FALSE, data = NULL, ...) {
  object <- if (inherits(object, c('survdiff', 'survfit'))) {
    if (length(form <- object$call$formula) == 1L)
      object$call$formula <- eval(object$call$formula, parent.frame(1L))
    coxph(as.formula(object$call$formula),
          eval(data %||% object$.data %||% object$call$data))
  } else if (inherits(object, c('formula', 'call')))
    coxph(object, data, ...)
  else object
  
  stopifnot(
    inherits(object, 'coxph')
  )
  
  obj <- summary(object)
  obj <- cbind(obj$conf.int[, -2L, drop = FALSE],
               p.value = obj$coefficients[, 'Pr(>|z|)'])
  
  if (details)
    obj
  else obj[, 'p.value']
}

#' @rdname surv_test
hr_text <- function(formula, data, ..., details = TRUE, pFUN = NULL) {
  pFUN <- if (is.null(pFUN) || isTRUE(pFUN))
    function(x) pvalr(x, show.p = TRUE)
  else if (identical(pFUN, FALSE))
    identity else match.fun(pFUN)
  
  object <- if (inherits(formula, 'coxph'))
    formula
  else if (inherits(formula, 'survfit'))
    coxph(as.formula(formula$call$formula),
          if (!missing(data)) data else eval(formula$call$data), ...)
  else formula
  
  suppressWarnings({
    cph <- tryCatch(
      if (inherits(object, 'coxph'))
        object else coxph(formula, data, ...),
      # warning = function(w) '',
      error   = function(e) e
    )
  })
  
  if (isTRUE(cph))
    return(FALSE)
  if (identical(cph, ''))
    return(cph)
  if (!inherits(cph, 'coxph'))
    stop(cph)
  
  obj <- hr_pval(cph, details = TRUE)
  
  txt <- apply(obj, 1L, function(x)
    sprintf('HR %.2f (%.2f, %.2f), %s', x[1L], x[2L], x[3L],
            {pv <- pFUN(x[4L]); if (is.na(pv)) 'p > 0.99' else pv}))
  lbl <- attr(terms(cph), 'term.labels')
  txt <- paste(cph$xlevels[[lbl[!grepl('strata\\(', lbl)]]],
               c('Reference', txt), sep = ': ')
  
  if (is.null(cph$xlevels))
    c(NA, gsub('^.*: ', '', txt)[-1L])
  else txt
}

#' @rdname surv_test
pw_pval <- function(object, details = FALSE, data = NULL, ...,
                    method = 'none') {
  object <- if (inherits(object, 'survdiff_pairs'))
    object
  else if (inherits(object, c('survdiff', 'survfit'))) {
    survdiff_pairs(object, ..., digits = 10L)
  } else if (inherits(object, c('formula', 'call'))) {
    object <- eval(
      substitute(survdiff(form, data = data, ...), list(form = object))
    )
    survdiff_pairs(object)
  } else stop('pw_pval - Invalid object', call. = FALSE)
  
  stopifnot(
    inherits(object, 'survdiff_pairs')
  )
  
  m <- object$p.value
  p <- m[lower.tri(m)]
  p <- p.adjust(p, method = method)
  n <- sprintf('%s vs %s', colnames(m)[col(m)[lower.tri(m, FALSE)]],
               rownames(m)[row(m)[lower.tri(m, FALSE)]])
  
  setNames(p, n)
}

#' @rdname surv_test
pw_text <- function(formula, data, ..., details = TRUE, pFUN = NULL,
                    method = 'none') {
  pFUN <- if (is.null(pFUN) || isTRUE(pFUN))
    function(x) pvalr(x, show.p = TRUE)
  else if (identical(pFUN, FALSE))
    identity else match.fun(pFUN)
  
  obj <- pw_pval(object = formula, data = data, method = method, ...)
  
  sprintf('%s: %s', names(obj), pFUN(obj))
}

#' @rdname surv_test
c_text <- function(formula, data, tau = NULL, iter = 1000L, seed = 1L,
                   digits = 2L, conf = 0.95, show_conf = TRUE, ...) {
  vv <- all.vars(formula(formula))
  data <- data[, vv]
  
  ## remove rows with missing data, indicator functions for factors
  idx <- complete.cases(data)
  if (any(!idx)) {
    message(sum(!idx), ' observations removed due to missingness', domain = NA)
    data <- data[idx, ]
  }
  
  mf <- model.matrix(reformulate(vv[-(1:2)]), data)[, -1L, drop = FALSE]
  data <- cbind(data[, vv[1:2]], mf)
  
  if (is.null(tau))
    tau <- max(data[, 1L], na.rm = TRUE)
  alpha <- abs(0:1 - (1 - conf) / 2)
  
  res <- survC1::Inf.Cval(data, tau, iter, seed)
  res <- res$Dhat + res$se * c(0, qnorm(alpha))
  res <- roundr(res, digits)
  res <- sprintf('%s (%s%% CI: %s - %s)', res[1L], conf * 100, res[2L], res[3L])
  
  if (show_conf)
    res else gsub(' .*', '', res)
}

#' kmplot_by
#' 
#' @description
#' This function helps create stratified \code{\link{kmplot}}s quickly with
#' panel labels, survival curve test(s), and/or Cox regression summaries for
#' each plot.
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
#' @param strata,event,time,by character strings of the strata, event (pfs, os,
#' ttp, etc; see details), time (optional), and stratification variables;
#' additionally, vectors for each are allowed
#' @param data a data frame
#' @param single logical; if \code{TRUE}, each level of \code{by} will be
#' drawn in a separate window
#' @param lr_test logical or numeric; if \code{TRUE}, a log-rank test will be
#' performed and the results added to the top-right corner of the plot; if
#' numeric, the value is passed as \code{rho} controlling the type of test
#' performed; see \code{\link{survdiff}}
#' @param main title of plot(s)
#' @param ylab y-axis label
#' @param sub sub-title displayed in upper left corner; should be a character
#' vector with length equal to the number of panels (i.e., the number of
#' unique values of \code{by} or length one if \code{by} was not given)
#' @param strata_lab at-risk table strata labels; should be a character vector
#' with length equal to the number of strata; \code{TRUE} (or equivalently
#' missing) is the default, and \code{FALSE} trims the labels; see examples
#' @param fig_lab figure panel labels; should be a character vector with
#' length equal to the number of panels (i.e., the number of unique values of
#' \code{by} or length one if \code{by} was not given)
#' @param col.surv color for individual survival curves or for all curves in
#' a plot if \code{by} is given and \code{map.col = TRUE}; if \code{col.surv}
#' is a named vector which matches the at risk labels, then colors are mapped
#' to the corresponding strata; see \code{\link{kmplot}}
#' @param map.col logical; if \code{TRUE}, \code{col.surv} will be the color
#' of all curves in each plot (only used when \code{by} is non-missing)
#' @param legend logical, a vector of x/y coordinates, or a keyword (see
#' \code{\link{legend}}); if \code{TRUE}, the default position is
#' \code{"bottomleft"}
#' @param add logical; if \code{FALSE} (default), resets graphical parameters
#' to settings before \code{kmplot_by} was called; set to \code{TRUE} for
#' adding to existing plots
#' @param plot logical; if \code{FALSE}, no plot is created but a list with
#' \code{survfit}s is returned
#' @param args.survfit a \emph{named} list of optional arguments passed to
#' \code{\link{survfit.formula}}; relevant arguments include \code{type}
#' (default is \code{"kaplan-meier"}), \code{error} (\code{"greenwood"}),
#' \code{conf.int} (\code{0.95}), \code{"conf.type"} (\code{"log"}), and
#' \code{"se.fit"} (\code{TRUE})
#' @param stratify (dev) \code{\link[survival]{strata}} variables
#' @param panel.first,panel.last,... additional arguments passed to
#' \code{\link{kmplot}} or graphical parameters subsequently passed to
#' \code{\link{par}}
#' 
#' @return
#' Invisibly returns a list of \code{\link{survfit}} object(s) used to generate
#' plot(s). If \code{by} was used, there will be a list element for each unique
#' value.
#' 
#' @seealso
#' \code{\link{kmplot}}; \code{\link{survdiff}}; \code{\link{kmplot_data_}};
#' \code{\link{lr_text}}; \code{\link{lr_pval}}; \code{\link{points.kmplot}}
#' 
#' @examples
#' library('survival')
#' kmplot_by(time = 'time', event = 'status', data = colon)
#' with(colon, kmplot_by('All', time = time, event = status))
#' 
#' 
#' ## create *_ind, *_time variables, see details
#' colon2 <- within(colon[duplicated(colon$id), ], {
#'   pfs_time <- time
#'   pfs_ind  <- status
#'   sex <- factor(sex, 0:1, c('Female','Male'))
#' })
#' 
#' kmplot_by('rx', 'pfs', data = colon2, col.surv = 1:3,
#'   strata_lab = FALSE, median = TRUE)
#' kmplot_by('rx', 'pfs', data = colon2, col.surv = 1:3,
#'   strata_lab = FALSE, median = TRUE, args.survfit = list(conf.int = 0.90))
#' 
#' 
#' ## these are equivalent (with minor differences in aesthetics)
#' kmplot_by(time = 'pfs_time', event = 'pfs_ind', data = colon2)
#' kmplot_by('1', 'pfs', colon2)
#' 
#' 
#' ## return value is a list of survfit objects
#' l <- kmplot_by('sex', 'pfs', colon2, 'rx', plot = FALSE)
#' str(lapply(l, kmplot))
#' str(lapply(l, kmplot_by))
#' 
#' 
#' ## multiple variables can be combined
#' kmplot_by('rx + sex', 'pfs', colon2, strata_lab = FALSE, lty.surv = 1:6)
#' 
#' 
#' ## if "by" is given, default is to plot separately
#' kmplot_by('rx', 'pfs', colon2, by = 'sex', col.surv = 1:3,
#'   strata_lab = c('Observation', 'Trt', 'Trt + 5-FU'))
#'   
#' ## if "by" is given, use map.col to map colors to plots
#' kmplot_by( 'rx', 'pfs', colon2, by = 'rx', map.col = TRUE, single = FALSE)
#' kmplot_by('sex', 'pfs', colon2, by = 'rx', map.col = TRUE, single = FALSE)
#' kmplot_by('sex', 'pfs', colon2, by = 'age > 60', map.col = TRUE, single = FALSE)
#' 
#' ## to ensure colors are mapped to the same strata across plots (eg, if
#' ## all sub plots do not have the same groups), use a _named_ vector
#' kmplot_by('rx', 'pfs', colon2, by = 'rx', xlim = c(0, 3000),
#'           col.surv = c(Lev = 'brown', Obs = 'blue', 'Lev+5FU' = 'purple'))
#' 
#' 
#' ## if single = FALSE, uses n2mfrow function to set par('mfrow')
#' kmplot_by('rx', 'pfs', colon2, by = 'sex', col.surv = 1:3, single = FALSE,
#'   strata_lab = c('Observe', 'Trt', 'Trt + 5-FU'), main = levels(colon2$sex))
#' 
#' 
#' ## if par('mfrow') is anything other than c(1,1), uses current setting
#' par(mfrow = c(2,2))
#' kmplot_by('rx', 'pfs', colon2, by = 'sex', col.surv = 1:3, single = FALSE,
#'   strata_lab = c('Observation','Trt','Trt + 5-FU'), add = TRUE)
#' kmplot_by('1', 'pfs', colon2, by = 'sex', col.surv = 1:3, single = FALSE,
#'   strata_lab = FALSE, fig = c('C', 'D'), lr_test = FALSE)
#' 
#' 
#' ## use add = TRUE to add to a figure region without using the by argument
#' par(mfrow = c(1, 2))
#' mar <- c(8, 6, 3, 2) ## to align axes
#' kmplot_by( 'rx', 'pfs', colon2, strata_lab = FALSE, add = TRUE, mar = mar)
#' kmplot_by('sex', 'pfs', colon2, strata_lab = FALSE, add = TRUE, mar = mar)
#' 
#' @export

kmplot_by <- function(strata = '1', event = NULL, data = NULL, by = NULL,
                      time = NULL, single = TRUE, lr_test = TRUE, main = NULL,
                      ylab = NULL, sub = NULL, strata_lab = NULL, fig_lab = NULL,
                      col.surv = NULL, map.col = FALSE, legend = FALSE,
                      add = FALSE, plot = TRUE, args.survfit = list(),
                      stratify = NULL, panel.first = NULL, panel.last = NULL, ...) {
  op <- par(no.readonly = TRUE)
  
  if (is.logical(lr_test)) {
    rho <- 0
  } else {
    rho <- if (is.numeric(lr_test))
    lr_test else 0
    lr_test <- TRUE
  }
  
  ## defaults
  type      <- args.survfit$type      %||% 'kaplan-meier'
  error     <- args.survfit$error     %||% 'greenwood'
  conf.int  <- args.survfit$conf.int  %||% 0.95
  conf.type <- args.survfit$conf.type %||% 'log'
  se.fit    <- args.survfit$se.fit    %||% TRUE
  
  if (length(event) > 1L) {
    data <- data.frame(
      strata, event, time, by = by %||% NA,
      stringsAsFactors = FALSE
    )
    strata <- 'strata'
    event  <- 'event'
    time   <- 'time'
    by     <- if (is.null(by)) NULL else 'by'
  }
  
  if (inherits(strata, 'survfit')) {
    ## if survfit object is given, extract needed vars and run as usual
    if (is.null(data <- strata$.data)) {
      data <- deparse(strata$call$data)
      data <- get(data, where(gsub('[$[].*', '', data)))
    }
    
    conf.int  <- strata$conf.int  %||% conf.int
    conf.type <- strata$conf.type %||% conf.type
    se.fit    <- !is.null(strata$std.err)
    
    form   <- as.character(strata$call$formula)[-1L]
    strata <- form[length(form)]
    event  <- gsub('(\\S+)\\)|.', '\\1', form[1L])
    time   <- gsub('\\((\\w+)|.', '\\1', form[1L])
  }
  
  form  <- if (!is.null(time)) {
    ev <- event
    sprintf('Surv(%s, %s) ~ %s', time, event, strata)
  } else {
    ev <- paste0(event, '_ind')
    sprintf('Surv(%s_time, %s_ind) ~ %s', event, event, strata)
  }
  form  <- as.formula(form)
  
  if (is.factor(data[, ev])) {
    warning(sprintf('factor (%s) used for event - coercing to integer',
                    shQuote(event)), call. = FALSE)
    data[, ev] <- as.integer(data[, ev])
  }
  
  if (plot & (!add | !single))
    on.exit(par(op))
  if (add)
    on.exit(NULL)
    
  if (!is.null(by)) {
    data[, by] <- if (by %in% names(data))
      as.factor(data[, by])
    else factor(with(data, eval(parse(text = by))))
    if (single) {
      add <- FALSE
      par(mfrow = c(1L,1L))
    } else {
      add <- TRUE
      if (identical(par('mfrow'), c(1L,1L)))
        par(mfrow = n2mfrow(lunique(data[, by], na.rm = TRUE)))
    }
    sp <- split(data, droplevels(data[, by]))
    ## list names will be main title(s)
    names(sp) <- rep_len(main %||% paste(by, names(sp), sep = '='), length(sp))
  } else {
    if (missing(add)) {
      add <- FALSE
      par(mfrow = c(1L, 1L))
    }
    map.col <- FALSE
    sp <- list(data)
    names(sp) <- main
  }
  
  ## define these before passing to loop
  mlabs <- is.null(strata_lab)
  msub  <- is.null(sub)
  fig   <- if (length(sp) > 1L & is.null(fig_lab))
    LETTERS[seq_along(sp)] else if (is.null(fig_lab)) '' else fig_lab
  fig   <- rep_len(fig, length(sp))
  ylab  <- ylab %||% sprintf('%s probability', toupper(event))
  
  ## possible names for strata labels/title
  dots <- lapply(dots(...), function(x)
    tryCatch(eval(x), error = function(e) NULL))
  strata_names <- dots$strata.lab %||% dots$strata.expr %||%
    if (mlabs) NULL else strata_lab
  if (is.logical(strata_names) |
      !is.expression(strata_names) && anyNA(strata_names) |
      is.null(strata_names))
    strata_names <- NULL
  if (!is.null(names(strata_names)))
    names(sp) <- strata_names
  
  if (!is.null(by) & is.null(names(col.surv))) {
    sl <- survfit(
      form, data, se.fit = se.fit, type = type, error = error,
      conf.int = conf.int, conf.type = conf.type
    )
    col.surv <- setNames(
      col.surv %||% seq_along(sl$n),
      if (by == strata || is.null(strata_lab) || identical(strata_lab, FALSE))
        NULL else strata_names %||% names(sl$strata)
    )
    if (length(sp) != length(col.surv))
      col.surv <- if (map.col)
        seq_along(sp) else rep(col.surv, length(sp))
  }
  
  sl <- lapply(seq_along(sp), function(x) {
    tryCatch({
      eval(substitute(
        survfit(form, data = sp[[x]], type = type,
                conf.type = conf.type, error = error,
                conf.int = conf.int, se.fit = se.fit),
        list(form = form))
      )
    }, error = function(e) {
      if (grepl('first argument must be of mode character', e)) {
        warning('Error in survfit:\n', e, '\n',
                'Check that \'strata\' has at least one non-missing value',
                '\n\n', 'Returning null model: ',
                deparse(update(form, . ~ 1)))
        eval(substitute(
          survfit(form, data = sp[[x]], type = type,
                  conf.type = conf.type, error = error,
                  conf.int = conf.int, se.fit = se.fit),
          list(form = update(form, . ~ 1))))
      } else if (grepl('less than one element in integerOneIndex', e))
        stop('Check that \'event\' has at least one non-missing value')
      else stop(e)
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
          svar <- colnames(model.frame(form, s$.data)[, -1L, drop = FALSE])
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
    
    kmplot(s, add = add, legend = legend, main = names(sp)[x], ylab = ylab,
           lr_test = lr_test, ..., panel.last = panel.last, stratify = stratify,
           col.surv = if (map.col) unname(col.surv)[x] else col.surv,
           panel.first = {
             ## sub label - top left margin (default: strata var)
             mtxt <- if (!msub)
               rep_len(sub, length(sp))[x] else strata
             mtext(mtxt, side = 3L, line = 0.25, outer = FALSE,
                   at = 0, adj = 0, font = 3L)
             
             ## figure label - top left outer margin (eg, A, B, C)
             mtext(fig[x], side = 3L, line = 0.25, outer = FALSE,
                   at = 0 - par('usr')[2L] * .05, font = 2, cex = 1.2)
             
             panel.first
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
#' @param y the y-coordinate(s) for each point, recycled as needed
#' @param time,event,strata (optional) variables used to fit \code{s}
#' @param col a vector of colors (one for each strata level of \code{s}) for
#' tick marks; note these colors should match the curves of the survival plot
#' @param pch a vector of plotting characters to distinguish censoring and
#' events
#' @param ... additional arguments passed to \code{\link{points}}
#' 
#' @seealso
#' \code{\link{kmplot}}; \code{\link{kmplot_by}}
#' @examples
#' library('survival')
#' s <- survfit(Surv(futime, fustat) ~ rx, ovarian)
#' 
#' kmplot(s, add = TRUE)
#' kmplot_ticks(s, by_var = 'resid.ds', what = 1)
#' kmplot_ticks(s, by_var = 'resid.ds', what = 2, pch = '*', y = 0.05)
#' 
#' @export

kmplot_ticks <- function(s, data = eval(s$call$data), by_var, what,
                         y = 0, time, event, strata, col = NULL,
                         pch = NULL, ...) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  ## lifted from kmplot_by
  if (inherits(s, 'survfit')) {
    if (is.null(data <- s$.data)) {
      data <- deparse(s$call$data)
      data <- get(data, where(gsub('[$[].*', '', data)))
    }
    terms <- c(terms.inner(s$call$formula),
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
  
  col <- col[as.numeric(data[, strata])]
  pch <- rep_len(pch %||% 3:4, 2L)
  pch <- pch[(as.integer(data[, event]) == 1L) + 1L]
  points(data[, time], rep_len(y, nrow(data)), col = col, pch = pch, ...)
  
  invisible(data)
}

terms.inner <- function(x) {
  # survival:::terms.inner
  if (inherits(x, 'formula')) {
    if (length(x) == 3L)
      c(terms.inner(x[[2L]]), terms.inner(x[[3L]]))
    else terms.inner(x[[2L]])
  } else if (class(x) == 'call' &&
             (x[[1L]] != as.name('$') &&
              x[[1L]] != as.name('['))) {
    if (x[[1L]] == '+' || x[[1]] == '*' || x[[1]] == '-') {
      if (length(x) == 3L)
        c(terms.inner(x[[2L]]), terms.inner(x[[3L]]))
      else terms.inner(x[[2L]])
    } else if (x[[1L]] == as.name('Surv'))
      unlist(lapply(x[-1L], terms.inner))
    else terms.inner(x[[2L]])
  } else (deparse(x))
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
  } else if (dim(C)[1L] != n)
      stop('C has improper dimensions\n')
  
  if (is.null(d))
    d <- matrix(0, n, 1)
  if (dim(d)[1L] != dim(C)[1L])
    stop('C and d do not have appropriate dimensions')
  
  I. <- s$var[pos, pos]
  est <- matrix(as.vector(s$coeff[pos]), dim(C)[2L])
  chisq <- as.numeric(t(C %*% est - d) %*% solve(t(C) %*% I. %*% C ) %*%
                        (C %*% est - d))
  df <- dim(C)[1L]
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
#' \code{\link{survfit}}; \code{\link{print.summary.survfit}}
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
  oo <- options(digits = digits)
  on.exit(options(oo))
  
  x <- summary(s, ...)
  if (!is.null(cl <- x$call)) {
    cat('Call: ')
    dput(cl)
  }
  omit <- x$na.action
  if (length(omit))
    cat(naprint(omit), '\n')
  if (x$type == 'right' || is.null(x$n.enter)) {
    mat    <- cbind(x$time, x$n.risk, x$n.event, x$surv)
    cnames <- c('time', 'n.risk', 'n.event')
  } else if (x$type == 'counting') {
    mat    <- cbind(x$time, x$n.risk, x$n.event, x$n.enter, x$n.censor, x$surv)
    cnames <- c(time, 'n.risk', 'n.event', 'entered', 'censored')
  }
  
  ncurve <- if (is.matrix(x$surv))
    ncol(x$surv) else 1L
  if (ncurve == 1L) {
    cnames <- c(cnames, 'survival')
    if (!is.null(x$std.err)) {
      if (is.null(x$lower)) {
        mat    <- cbind(mat, x$std.err)
        cnames <- c(cnames, 'std.err')
      } else {
        mat    <- cbind(mat, x$std.err, x$lower, x$upper)
        cnames <- c(cnames, 'std.err',
                    paste0('lower ', x$conf.int * 100, '% CI'),
                    paste0('upper ', x$conf.int * 100, '% CI'))
      }
    }
  } else cnames <- c(cnames, paste0('survival', seq.int(ncurve)))
  
  if (!is.null(x$start.time)) {
    mat.keep <- mat[, 1L] >= x$start.time
    mat <- mat[mat.keep, , drop = FALSE]
    if (is.null(dim(mat)))
      stop('No information available using start.time = ', x$start.time)
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
        who <- strata == i
        cat('\n               ', i, '\n')
        if (sum(who) == 1L)
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
#' @param percent logical; if \code{TRUE}, percentages are shown instead of
#' probabilities
#' 
#' @return
#' A matrix (or list of matrices) with formatted summaries for each strata; see
#' \code{\link{summary.survfit}}
#' 
#' @seealso
#' \code{\link{survfit}}; \code{\link{print.summary.survfit}}
#' 
#' @examples
#' library('survival')
#' fit0 <- survfit(Surv(time, status == 2) ~ 1, data = cancer)
#' surv_table(fit0, times = 0:2 * 100, maxtime = FALSE)
#' 
#' ## also works for list of tables
#' fit1 <- survfit(Surv(time, status == 2) ~ sex, data = cancer, conf.int = 0.9)
#' surv_table(fit1)
#' rawr::combine_table(surv_table(fit1))
#' 
#' 
#' s <- `colnames<-`(
#'   surv_table(fit0, times = 0:8 * 100, digits = 2)[, -4],
#'   c('Time', 'No. at risk', 'No. of events', 'Surv (95% CI)')
#' )
#' ht <- htmlTable::htmlTable(s, caption = 'Table: Overall survival.')
#' structure(ht, class = 'htmlTable')
#' 
#' @export

surv_table <- function(s, digits = ifelse(percent, 0L, 3L),
                       times = pretty(s$time), maxtime = FALSE,
                       percent = FALSE, ...) {
  if (maxtime) {
    idx <- s$n.event > 0
    maxtime <- max(s$time[if (any(idx))
      idx else seq_along(idx) == length(idx)])
    times <- c(times[times <= maxtime], maxtime)
  }
  
  capture.output(
    ss <- surv_summary(s, digits = 7L, times = unique(times), ...)
  )
  
  if (percent) {
    ss <- if (islist(ss)) {
      lapply(ss, function(x) {
        idx <- grep('survival|lower|upper', colnames(x))
        x[, idx] <- x[, idx] * 100
        x
      })
    } else {
      idx <- grep('survival|lower|upper', colnames(ss))
      ss[, idx] <- ss[, idx] * 100
      ss
    }
  }
  
  f <- function(x, d = digits, vars = vars) {
    vars <- colnames(x)
    
    g <- function(wh, x = x, cn = vars) {
      cn[grepl(wh, cn)]
    }
    
    tmpvar <- g('survival|std.err|lower|upper')
    x[, tmpvar] <- roundr(x[, tmpvar], digits = d)
    surv <- sprintf('%s (%s, %s)',
                    x[, g('survival')], x[, g('lower')], x[, g('upper')])
    cn <- c('Time', 'No. at risk', 'No. event', 'Std.Error',
            sprintf('Surv (%s%% CI)', s$conf.int * 100))
    `colnames<-`(cbind(x[, c(setdiff(vars, tmpvar), 'std.err'),
                         drop = FALSE], surv), cn)
  }
  
  if (is.list(ss))
    Map('f', ss) else f(ss)
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
#' stopifnot(
#'   identical(survdiff_pairs(sdif), survdiff_pairs(sfit))
#' )
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
#' 
#' ## stratified models are accepted
#' sdif2 <- survdiff(Surv(time, status) ~ sex + strata(inst), data = lung)
#' survdiff_pairs(sdif2)
#' 
#' @export

survdiff_pairs <- function(s, ..., method = p.adjust.methods,
                           digits = getOption('digits')) {
  pwchisq <- function(i, j) {
    data <- data[data[, rhs] %in% c(unq[i], unq[j]), ]
    survdiff(as.formula(s$call$formula), data = data, ...)$chisq
  }

  # rhs <- all.vars(s$call$formula)[-(1:2)]
  rhs <- strsplit(as.character(s$call$formula)[-(1:2)], '\\s*\\+\\s*')[[1L]]
  rhs <- grep('strata\\(', rhs, value = TRUE, invert = TRUE)
  
  stopifnot(
    inherits(s, c('survdiff', 'survfit')),
    length(rhs) == 1L
  )
  
  method <- match.arg(method)
  data   <- eval(s$call$data, envir = parent.frame(1L))
  unq    <- as.character(sort(unique(data[, rhs])))
  
  nn <- outer(as.character(unq), as.character(unq), Vectorize(function(x, y)
    nrow(data[data[, rhs] %in% c(x, y), ])))
  nn[upper.tri(nn, FALSE)] <- NA

  chisq <- rbind(NA, cbind(pairwise.table(pwchisq, unq, 'none'), NA))
  dimnames(chisq) <- dimnames(nn) <- list(unq, unq)
  
  p.value <- apply(chisq, 1:2, function(x)
    pchisq(x, 1L, lower.tail = FALSE))

  tpv <- t(p.value)
  tpv[lower.tri(p.value)] <-
    p.adjust(p.value[lower.tri(p.value)], method = method)
  p.value <- t(tpv)
  
  res <- list(n = nn, chi.sq = chisq, p.value = p.value)
  
  structure(
    lapply(res, round, digits = digits),
    class = 'survdiff_pairs'
  )
}

#' Landmark
#' 
#' Fit survival curves for landmark time points.
#' 
#' @param s a \code{\link[survival]{survfit}} object
#' @param times a vector of landmark times
#' @param col color for \code{times} annotations; use \code{col = 0} to
#' suppress labels
#' @param plot,plot.main logicals; if \code{TRUE}, landmark and \code{s} are
#' plotted, respectively
#' @param lr_test logical or numeric; if \code{TRUE}, a log-rank test will be
#' performed and the results added to the top-right corner of the plot; if
#' numeric, the value is passed as \code{rho} controlling the type of test
#' performed; see \code{\link{survdiff}}
#' @param adjust_start logical; if \code{TRUE}, each landmark plot will begin
#' at the y-axis
#' @param ... additional arguments passed to \code{\link{kmplot}}
#' @param single logical; if \code{TRUE}, plots drawn on a single frame
#' 
#' @return
#' A list with the following elements:
#' 
#' \item{\code{$table}}{data frame with the sample size, chi-square
#' statistic, degrees of freedom, and p-value for the test (the type of test
#' can be controlled by using a numeric value for \code{lr_test}, passed as
#' \code{rho} to \code{\link{survdiff}})}
#' \item{\code{$survfit}}{a list with each \code{\link[survival]{survfit}}
#' object}
#' 
#' @examples
#' library('survival')
#' s <- survfit(Surv(time, status) ~ rx, colon)
#' l <- landmark(s, times = c(500, 1000, 2000), single = TRUE)
#' l$table
#' 
#' layout(matrix(c(1, 1, 2, 3), 2))
#' landmark(s, times = c(500, 2000), adjust_start = TRUE,
#'          hr_text = TRUE, col.surv = 4:6)
#' 
#' @export

landmark <- function(s, times = NULL, col = 2L, plot = TRUE, plot.main = plot,
                     lr_test = TRUE, adjust_start = FALSE, ..., single = FALSE) {
  form <- s$call$formula
  data <- eval(s$call$data)
  tvar <- terms.inner(form)[1L]
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  if (single)
    par(mfrow = n2mfrow(length(times) + plot.main))
  
  st <- data.frame(n = sum(s$n), lr_pval(s, TRUE), row.names = 'Total')
  if (plot.main)
    kmplot(
      s, ..., add = TRUE, lr_test = lr_test,
      panel.first = {
        abline(v = times, col = col)
        if (length(times))
          mtext(seq_along(times), at = times, cex = par('cex.lab'), col = col)
      }
    )
  
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
        
        if (plot)
          kmplot(
            si, ..., add = TRUE, lr_test = lr_test, xaxis.at = at,
            xaxis.lab = ifelse(adjust_start, ii, 0) + at,
            panel.first = {
              abline(v = ii, col = if (adjust_start) 0L else col)
              mtext(which(times %in% ii), at = par('usr')[1L], col = col,
                    cex = par('cex.main'), font = par('font.main'))
            }
          )
        
        list(
          table = data.frame(n = sum(si$n), lr_pval(si, TRUE), row.names = ii),
          survfit = si
        )
      }) else NULL
  
  ss <- lapply(sd, '[[', 'survfit')
  tt <- lapply(sd, '[[', 'table')
  
  res <- list(
    table   = do.call('rbind', c(list(st), tt)),
    survfit = c(list(Total = s), setNames(ss, sprintf('time=%s', times)))
  )
  
  invisible(res)
}

#' Extract \code{survfit} summaries
#' 
#' @description
#' Convenience functions to print results from \code{summary(x)$table}
#' where \code{x} is a \code{\link[survival]{survfit}} object.
#' 
#' \code{surv_median} and \code{surv_prob} extract median(s) and survival
#' estimate(s) with optional confidence intervals, respectively.
#' \code{surv_extract} is a generic extractor.
#' 
#' @param x an object of class \code{\link[survival]{survfit}}
#' @param what the data to return, either the index, character string(s), or
#' \code{NULL} (returns the entire table)
#' @param ci logical; if \code{TRUE}, the confidence interval is printed
#' @param digits number of digits past the decimal point to keep
#' @param which optional integer or character vector to select or re-order
#' the output; \code{which = NULL} and returns results for all strata
#' @param print logical; if \code{TRUE}, output is prepared for in-line
#' printing
#' @param na a character string to replace \code{NA} when median
#' times have not yet been reached
#' @param times vector of times passed to \code{\link{surv_table}}
#' @param show_conf logical; if \code{TRUE}, includes the confidence level
#' @param percent logical; if \code{TRUE}, percentages are shown instead of
#' probabilities
#' 
#' @examples
#' library('survival')
#' sfit1 <- survfit(Surv(time, status) ~ 1, lung)
#' sfit2 <- survfit(Surv(time, status) ~ sex, lung, conf.int = 0.9)
#' 
#' surv_extract(sfit1, NULL)
#' surv_extract(sfit1, 2:3)
#' surv_extract(sfit2, 'median|CL')
#' surv_extract(sfit1, c('events', 'median'))
#' 
#' 
#' surv_median(sfit1)
#' surv_median(sfit2)
#' surv_median(sfit2, print = FALSE)
#' 
#' surv_median(sfit1, ci = TRUE)
#' surv_median(sfit2, ci = TRUE)
#' surv_median(sfit2, ci = TRUE, print = FALSE)
#' 
#' 
#' times <- 365.242 * c(0.5, 1, 2)
#' surv_prob(sfit1, times)
#' ## compare
#' summary(sfit1, times)
#' 
#' surv_prob(sfit2, times, which = 'sex=2', digits = 3)
#' ## compare
#' summary(sfit2, times)
#' 
#' surv_prob(sfit2, times = 365, ci = TRUE, which = NULL)
#' 
#' @export

surv_extract <- function(x, what = 'median') {
  stopifnot(
    inherits(x, 'survfit')
  )
  
  oo <- options(survfit.rmean = 'individual')
  on.exit(options(oo))
  
  tbl <- summary(x)$table
  if (is.null(what))
    return(tbl)
  
  what <- if (is.character(what))
    grep(paste0(what, collapse = '|'), names(tbl) %||% colnames(tbl))
  else what
  
  if (length(dim(tbl)))
    tbl[, what] else tbl[what]
}

#' @rdname surv_extract
#' @export
surv_median <- function(x, ci = FALSE, digits = 0L, which = NULL,
                        print = TRUE, na = ifelse(ci, 'NR', 'not reached'),
                        show_conf = TRUE) {
  nr <- function(x) {
    gsub('NA', na, x)
  }
  nst <- pmax(1L, length(x$strata))
  which <- if (is.null(which))
    seq.int(nst) else which
  
  if (!ci) {
    res <- surv_extract(x, 'median')[which]
    return(
      if (print)
        nr(iprint(res, digits = digits)) else res
    )
  }
  
  res <- surv_extract(x, 'median|.CL')
  res <- roundr(res, digits)
  
  f <- function(.x) {
    if (show_conf)
        sprintf('%s (%s%% CI: %s - %s)',
                .x[1L], x$conf.int * 100, .x[2L], .x[3L])
    else sprintf('%s (%s - %s)', .x[1L], .x[2L], .x[3L])
  }
  
  res <- if (is.null(dim(res)))
    f(res) else apply(res, 1L, f)
  
  if (print)
    nr(iprint(res)) else res
}

#' @rdname surv_extract
#' @export
surv_prob <- function(x, times = pretty(x$time), ci = FALSE,
                      digits = ifelse(percent, 0L, 2L), which = 1L,
                      print = TRUE, show_conf = TRUE, percent = FALSE) {
  res <- surv_table(x, digits, times, FALSE, percent = percent)
  if (!islist(res))
    res <- list(res)
  
  res <- lapply(res, function(y) {
    y <- unname(y[, grep('Surv', colnames(y))])
    y <- if (!ci)
      gsub(' .*', '', y) else gsub(', ', ' - ', y)
    
    if (show_conf)
      y <- gsub('(?<=\\()', sprintf('%s%% CI: ', x$conf.int * 100), y, perl = TRUE)
    
    if (print)
      iprint(y) else y
  })
  
  if (length(which) == 1L)
    res[[which]]
  else if (print & length(times) == 1L)
    iprint(res) else res
}
