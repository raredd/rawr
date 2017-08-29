### plot utils, helpers for plotting functions
# unexported:
# %|%, do_sub_, dodge_, jit_, grouping_, hmsf_, do_rect_, do_seg_, d2r, r2d,
# p2c, c2p, p2r, p2d
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
  if (length(v) == 1L)
    vg <- as.data.frame(t(vg))
  data.frame(vg, hmsf = hmsf_(vg[, 2]))
}

do_rect_ <- function(n, x, y, single = FALSE, border = NA, col = NA,
                     adj = 0.25, ...) {
  ## used in river/river2 to add rects for each n
  lx <- length(x)
  if (lx == 1L && is.na(x)) return()
  if (single || length(x) == 1L) {
    rect(x[1], n[1] - 1 * adj, y[1], n[1] + 1 * adj, border = border[1],
         col = col[1], ...)
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
    n <- n[1]
    x <- x[1]
    y <- y[1]
  }
  if (is.na(x)) return()
  if (arrow[1])
    arrows(x, n, pmax(y,1, na.rm = TRUE), n, lwd = 2,
           angle = 30, length = .15, ...)
  else segments(x, n, y, n, ...)
  invisible(NULL)
}

## convert degrees to radians or vice versa
d2r <- function(degrees = 1) degrees * (pi / 180)
r2d <- function(radians = 1) radians * (180 / pi)

## convert polar to cartesian or vice versa
p2c <- function(radius, theta, degree = FALSE) {
  # p2c(c2p(0, 1)$r, c2p(0, 1)$t)
  if (degree)
    theta <- d2r(theta)
  list(x = radius * cos(theta),
       y = radius * sin(theta))
}
c2p <- function(x, y, degree = FALSE) {
  # c2p(p2c(1, 30, TRUE)$x, p2c(1, 30, TRUE)$y, TRUE)
  list(radius = sqrt(x ** 2 + y ** 2),
       theta = atan2(y, x) * if (degree) r2d() else 1)
}

## x,y coords to radians/degrees
p2r <- function(x, y, cx = 0, cy = 0) {
  # p2r(0,1)
  atan2(y - cy, x - cx)
  # ifelse(r < 0, pi / 2 + abs(r), r)
}
p2d <- function(x, y, cx = 0, cy = 0) {
  # p2d(0,1)
  r2d(atan2(y - cy, x - cx))
}

