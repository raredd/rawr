### plot utils, helpers for plotting functions
# unexported:
# %|%, do_sub_, dodge_, grouping_, jit_, do_rect_, do_seg_, coords
###


`%|%` <- Vectorize(function(x, y) if (is.na(x)) y else x)

do_sub_ <- function(x, n, s) {
  if (length(x) == n)
    x[s] else x
}

dodge_ <- function(x, at, dist, jit) {
  ## add jitter to points in a single group and return adjusted values
  # rawr:::dodge_(rep(1, 5), rep(1, 5), .1, .1)
  x <- x[nas <- !is.na(x)]
  g <- grouping_(x, dist)
  offset <- jit_(g$g.si, g$hmsf) * jit
  data.frame(x = at + (offset - mean(offset)), y = g$vs)
}

grouping_ <- function(v, dif, max.n = Inf) {
  ## turn values in each group into their plotting points
  # rawr:::grouping_(rep(1:2, 4), .1)
  hmsf_ <- function(x)
    ave(x, x, FUN = seq_along)
  vs <- sort(v)
  together <- c(FALSE, diff(vs) <=  dif)
  together[is.na(together)] <- FALSE
  together <- tryCatch(
    together_(together, max.n),
    error = function(e) {
      print(e$message)
      together
    }
  )
  g.id <- cumsum(!together)
  g.si <- rep(x <- as.vector(table(g.id)), x)
  # vg <- cbind(vs = vs, g.id = g.id, g.si = g.si)[rank(v), ]
  vg <- cbind(vs = vs, g.id = g.id, g.si = g.si)[order(v), ]
  if (length(v) == 1L)
    vg <- as.data.frame(t(vg))
  data.frame(vg, hmsf = hmsf_(vg[, 2L]))
}

together_ <- function(x, n = Inf) {
  rl <- rle(x)
  if (n == Inf || all(idx <- rl$lengths <= n | !rl$values))
    return(x)
  x[min(length(x), sum(rl$lengths[cumprod(idx) == 1]) + n + 1L)] <- FALSE
  Recall(x, n)
}

jit_ <- function(g.si, hmsf) {
  # hmsf - (g.si + 1) / 2
  ave(seq_along(g.si), g.si, FUN = function(ii)
    scale(hmsf[ii] - (g.si[ii] + 1 / 2), scale = FALSE))
}

do_rect_ <- function(n, x, y, single = FALSE, border = NA, col = NA,
                     adj = 0.25, ...) {
  ## used in river/river2 to add rects for each n
  lx <- length(x)
  if (lx == 1L && is.na(x))
    return(invisible(NULL))
  if (single || length(x) == 1L) {
    rect(x[1L], n[1L] - 1 * adj, y[1L], n[1L] + 1 * adj, border = border[1L],
         col = col[1L], ...)
  } else {
    ## x is a vector of start times, y is a vector of end times
    for (ii in seq_along(x))
      rect(x[ii], n - 1 * adj, y[ii], n + 1 * adj, border = border[ii],
           col = col[ii], ...)
  }
  invisible(NULL)
}

do_seg_ <- function(n, x, y, arrow, single = FALSE, lwd = 2, ...) {
  ## used in river/river2 to add segs for each n
  if (single) {
    n <- n[1L]
    x <- x[1L]
    y <- y[1L]
  }
  if (is.na(x))
    return(invisible(NULL))
  if (arrow[1L])
    arrows(x, n, pmax(y, 1, na.rm = TRUE), n, lwd = lwd,
           angle = 30, length = 0.15, ...)
  else segments(x, n, y, n, lwd = lwd, ...)
  invisible(NULL)
}

coords <- function(x = 0:1, y = x, to = 'user', line, side) {
  ## plotr::coords
  xy <- cbind(x, y)
  x  <- xy[, 1L]
  y  <- xy[, 2L]
  
  if (!missing(line) | !missing(side)) {
    lh <- par('cin')[2L] * par('cex') * par('lheight')
    
    sapply(line, function(li) {
      li <- li + 0.5
      x  <- diff(grconvertX(x, 'in', 'user')) * lh * li
      y  <- diff(grconvertY(y, 'in', 'user')) * lh * li
      
      (par('usr')[c(3, 1, 4, 2)] + c(-y, -x, y, x))[match(side, 1:4)]
    })
  } else
    list(
      plot   = list(x = grconvertX(x, 'npc', to), y = grconvertY(y, 'npc', to)),
      figure = list(x = grconvertX(x, 'nfc', to), y = grconvertY(y, 'nfc', to)),
      inner  = list(x = grconvertX(x, 'nic', to), y = grconvertY(y, 'nic', to)),
      device = list(x = grconvertX(x, 'ndc', to), y = grconvertY(y, 'ndc', to))
    )
}
