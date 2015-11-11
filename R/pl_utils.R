### plot utils
# %|%, do_sub_, dodge_, jit_, grouping_, hmsf_, pstar_, do_rect_, do_seg_
###


'%|%' <- Vectorize(function(x, y) if (is.na(x)) y else x)

do_sub_ <- function(x, n, s) if (length(x) == n) x[s] else x

dodge_ <- function(x, at, dist, jit) {
  x <- x[nas <- !is.na(x)]
  gr <- grouping_(x, dif = dist)
  x <- rep(at, nrow(gr)) + jit_(gr$g.si, gr$hmsf) * jit
  y <- gr$vs
  data.frame(x, y)
}

## compute jittering
jit_ <- function(g.si, hm.sf) hm.sf - (g.si + 1) / 2

## turn values in each group into their plotting points
grouping_ <- function(v, dif) {
  vs <- sort(v)
  together <- c(FALSE, diff(vs) <=  dif)
  g.id <- cumsum(!together)
  g.si <- rep(x <- as.vector(table(g.id)), x)
  vg <- cbind(vs, g.id, g.si)[rank(v), ]
  if (length(v) == 1) 
    vg <- as.data.frame(t(vg))
  hmsf <- hmsf_(vg[, 2])
  data.frame(vg, hmsf)
}

## how many so far
hmsf_ <- function(g) {
  out <- NULL
  u <- unique(g)
  for (i in 1:length(u)) {
    j <- g == u[i]
    out[which(j)] <- 1:sum(j)
  }
  out
}

do_rect_ <- function(n, x, y, border = NA, ...) {
  adj <- 0.25
  if (is.na(x)) return()
  rect(x, n - 1 * adj, y, n + 1 * adj, border = border, ...)
}

do_seg_ <- function(n, x, y, arrow, ...) {
  if (is.na(x)) return()
  if (arrow)
    arrows(x, n, pmax(y,1, na.rm = TRUE), n, lwd = 2,
           angle = 30, length = .15, ...)
  else segments(x, n, y, n, ...)
}
