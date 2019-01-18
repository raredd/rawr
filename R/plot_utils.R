### plot utils, helpers for plotting functions
# unexported:
# %|%, do_sub_, dodge_, grouping_, jit_, do_rect_, do_seg_
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
  gr <- grouping_(x, dist)
  offset <- jit_(gr$g.si, gr$hmsf) * jit
  data.frame(x = at + (offset - mean(offset)), y = gr$vs)
}

grouping_ <- function(v, dif) {
  ## turn values in each group into their plotting points
  # rawr:::grouping_(rep(1:2, 4), .1)
  hmsf_ <- function(x)
    ave(x, x, FUN = seq_along)
  vs <- sort(v)
  together <- c(FALSE, diff(vs) <=  dif)
  together[is.na(together)] <- FALSE
  g.id <- cumsum(!together)
  g.si <- rep(x <- as.vector(table(g.id)), x)
  vg <- cbind(vs = vs, g.id = g.id, g.si = g.si)[rank(v), ]
  if (length(v) == 1L)
    vg <- as.data.frame(t(vg))
  data.frame(vg, hmsf = hmsf_(vg[, 2L]))
}

jit_ <- function(g.si, hmsf) {
  hmsf - (g.si + 1) / 2
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

do_seg_ <- function(n, x, y, arrow, single = FALSE, ...) {
  ## used in river/river2 to add segs for each n
  if (single) {
    n <- n[1L]
    x <- x[1L]
    y <- y[1L]
  }
  if (is.na(x))
    return(invisible(NULL))
  if (arrow[1L])
    arrows(x, n, pmax(y, 1, na.rm = TRUE), n, lwd = 2,
           angle = 30, length = 0.15, ...)
  else segments(x, n, y, n, ...)
  invisible(NULL)
}
