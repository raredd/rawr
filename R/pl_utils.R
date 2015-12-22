### plot utils
# %|%, do_sub_, dodge_, jit_, grouping_, hmsf_, do_rect_, do_seg_
###


'%|%' <- Vectorize(function(x, y) if (is.na(x)) y else x)

do_sub_ <- function(x, n, s) if (length(x) == n) x[s] else x

dodge_ <- function(x, at, dist, jit) {
  ## add jitter to points in a single group and return adjusted values
  # dodge_(rep(1, 5), rep(1, 5), .1, .1)
  x <- x[nas <- !is.na(x)]
  gr <- grouping_(x, dist)
  offset <- jit_(gr$g.si, gr$hmsf) * jit
  data.frame(x = at + (offset - mean(offset)), y = gr$vs)
}

jit_ <- function(g.si, hm.sf) hm.sf - (g.si + 1) / 2

grouping_ <- function(v, dif) {
  ## turn values in each group into their plotting points
  # grouping_(rep(1:2, 4), .1)
  hmsf_ <- function(x) ave(x, x, FUN = seq_along)
  vs <- sort(v)
  together <- c(FALSE, diff(vs) <=  dif)
  g.id <- cumsum(!together)
  g.si <- rep(x <- as.vector(table(g.id)), x)
  vg <- cbind(vs = vs, g.id = g.id, g.si = g.si)[rank(v), ]
  if (length(v) == 1) 
    vg <- as.data.frame(t(vg))
  data.frame(vg, hmsf = hmsf_(vg[, 2]))
}

do_rect_ <- function(n, x, y, single = FALSE, border = NA, col = NA, ...) {
  ## used in river/river2 to add rects for each n
  adj <- 0.25
  lx <- length(x)
  if (lx == 1L && is.na(x)) return()
  if (single || length(x) == 1) {
    rect(x[1], n[1] - 1 * adj, y[1], n[1] + 1 * adj, border = border[1],
         col = col[1], ...)
  } else {
    ## x is a vector of start times, y is a vector of end times
    for (ii in seq_along(x))
      rect(x[ii], n - 1 * adj, y[ii], n + 1 * adj, border = border[ii],
           col = col[ii], ...)
  }
  invisible()
}

do_seg_ <- function(n, x, y, arrow, single = FALSE, ...) {
  ## used in river/river2 to add segs for each n
  if (single) {
    n <- n[1]
    x <- x[1]
    y <- y[1]
  }
  if (is.na(x)) return()
  if (arrow[1])
    arrows(x, n, pmax(y,1, na.rm = TRUE), n, lwd = 2,
           angle = 30, length = .15, ...)
  else segments(x, n, y, n, ...)
  invisible()
}
