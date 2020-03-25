bp.test <- function(formula, data, which = NULL, at = NULL, line = NULL,
                    test = wilcox.test, ...) {
  op <- par(..., no.readonly = TRUE)
  on.exit(par(op))
  
  bp <- boxplot(formula, data, plot = FALSE)
  ng <- length(bp$n)
  if (ng == 1L) {
    message('only one group -- no test performed')
    return(invisible(NULL))
  }
  
  pv <- if (ng == 2L)
    test(formula, data)
  else cuzick.test(formula, data, details = test)$details$pairs
  pv <- pvalr(pv$p.value, show.p = TRUE)
  
  which <- if (is.null(which))
    seq_along(pv) else which[which %in% seq_along(pv)]
  at <- if (is.null(at))
    seq.int(ng) else at
  line <- if (is.null(line) || length(line) == 1L)
    1.25 * (seq_along(which) - 1) + line %||% 0 else line
  
  seg <- function(x1, y, x2) {
    pad <- diff(par('usr')[3:4]) / 100
    col <- par('fg')
    
    segments(x1, y, x2, y,       col = col, xpd = NA)
    segments(x1, y, x1, y - pad, col = col, xpd = NA)
    segments(x2, y, x2, y - pad, col = col, xpd = NA)
    
    c(x = x1 + (x2 - x1) / 2, y = y + pad * 3)
  }
  
  yat <- coords(line = line, side = 3L)
  cbn <- combn(at, 2L)
  
  res <- sapply(seq_along(which), function(ii) {
    xat <- cbn[, which[ii]]
    xat <- seg(xat[1L], yat[ii], xat[2L])
    text(xat[1L], xat[2L], pv[which[ii]], xpd = NA)
    xat
  })
  
  invisible(t(res))
}
