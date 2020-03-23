### plot utils, helpers for plotting functions
# imgpal, rawr_palettes, rawr_pal, show_pal
# 
# unexported:
# %|%, do_sub_, dodge_, grouping_, jit_, do_rect_, do_seg_
###


#' Image palettes
#' 
#' Extract unique and most commonly-used unique colors from an image file
#' (requires \href{https://imagemagick.org/index.php}{ImageMagick}).
#' 
#' @param path full file path to image
#' @param n maximum number of colors to extract, result will be <= \code{n},
#' and the calculated number of unique colors will also be provided
#' @param x an object of class \code{"imgpal"}
#' @param fullrange logical; if \code{TRUE}, all <=\code{n} colors will be
#' displayed
#' @param ... ignored
#' 
#' @return
#' A list of class \code{"imgpal"} with the following elements:
#' \item{filename}{the image file name}
#' \item{n_unique}{the calculated number of unique colors}
#' \item{col}{a vector of colors}
#' \item{counts}{frequency counts for each \code{col}}
#' \item{call}{the call made to \code{convert}}
#' \item{convert}{the result of \code{call}}
#' 
#' @examples
#' \dontrun{
#' ip <- imgpal('https://www.google.com/images/branding/googlelogo/2x/googlelogo_color_272x92dp.png')
#' show_pal(ip)
#' 
#' img <- system.file('fig', package = 'rawr')
#' img <- list.files(img, full.names = TRUE, pattern = 'g$')
#' op <- par(mfrow = n2mfrow(length(img)))
#' sapply(img, function(x) show_pal(imgpal(x), fullrange = TRUE))
#' par(op)
#' }
#' 
#' @export

imgpal <- function(path, n = 10L) {
  cmd <- sprintf(
    'convert %s +dither -colors %s -define \\
    histogram:unique-colors=true -format "%%f, n=%%k\n%%c\n" histogram:info:',
    path, n
  )
  res <- system(cmd, intern = TRUE)
  res <- trimws(res[nzchar(res)])
  
  dat <- read.table(
    comment.char = '', stringsAsFactors = FALSE,
    text = gsub('\\s*(\\d+):.*(#\\S+).*', '\\1 \\2', res[-1L])
  )
  dat <- dat[order(dat[, 1L], decreasing = TRUE), ]
  
  ## remove fully transparent or white-ish
  idx <- grepl('(?i)#(.{6}00|FFFFFF)', dat[, 2L])
  dat <- dat[!idx, ]
  
  res <- list(
    filename = gsub(', n.*', '', res[1L]),
    n_unique = type.convert(gsub('n=(\\d+)|.', '\\1', res[1L])),
    col = dat[, 2L], counts = dat[, 1L], call = cmd, convert = res
  )
  
  structure(res, class = 'imgpal')
}

#' rawr palettes
#' 
#' @param name the palette name to be used
#' @param n the number of colors from the palette to use
#' @param z for \code{type = 'continuous'}, the number of colors desired
#' @param type return a discrete or continuous (gradient) of colors
#' @param rev logical; if \code{TRUE}, the palette is reversed
#' @param x one of 1) a \code{rawr_palette} name; 2) a vector of two or more
#' colors; 3) an \code{\link{imgpal}} object
#' @param fullrange logical; for \code{\link{imgpal}} objects, if \code{TRUE},
#' the entire palette is shown; otherwise, only the calculated number of
#' unique colors
#' @param counts logical; for \code{\link{imgpal}} objects, if \code{TRUE},
#' the color frequencies are shown for each
#' 
#' @seealso
#' \code{\link{imgpal}}; \code{\link{palette}}; \code{\link{colorRampPalette}};
#' \code{wesanderson::wes_palettes}; \code{nord::nord_palettes}
#' 
#' 
#' @examples
#' ## some built-in palettes
#' rawr_palettes
#' 
#' ## use or generate new palettees from existing
#' p <- rawr_pal('dfci')
#' show_pal(p)
#' p <- rawr_pal('dfci', 4)
#' show_pal(p)
#' p <- rawr_pal('dfci', 4, 100, type = 'continuous')
#' show_pal(p)
#' 
#' filled.contour(volcano, col = rawr_pal('dfci', 4, 21, type = 'c'))
#' filled.contour(volcano, col = rawr_pal('dfci', z = 21, type = 'c'))
#'
#' @export

rawr_palettes <- list(
  BIDMC =
    c('#171F69FE', '#16145FFD', '#171A642F', '#171A6491',
      '#171A655E', '#171A64D1', '#171B65B1'),
  DFCI =
    c('#63666B', '#0F699B', '#3CC5F1', '#F39625',
      '#9FA2A6', '#D5D7D9', '#847F5F', '#40A0C9'),
  Harvard =
    c('#C5112EFF', '#231F20FF', '#080505FC', '#7316255C',
      '#6B17252E', '#B30F2AE7', '#9F9F9FFF', '#5D343BD4'),
  MGH =
    c('#007DA2FF', '#374249FF', '#2C4E5B59', '#2F4A55B2',
      '#2555652B', '#007DA2E3', '#007DA270', '#007DA2C2'),
  pokrie =
    c('#612D13', '#E1A863', '#245967', '#232324', '#C49E67',
      '#57503E',  '#975A2E', '#DFC597', '#257589', '#4F8B93')
)

#' @rdname rawr_palettes
#' @export
rawr_pal <- function(name, n, z = n, type = c('discrete', 'continuous'),
                     rev = FALSE) {
  type <- match.arg(type)
  
  pal <- rawr::rawr_palettes
  names(pal) <- tolower(names(pal))
  nn <- names(pal)
  pal <- pal[[name]]
  name <- names(rawr::rawr_palettes)[match(name, nn)]
  
  if (rev)
    pal <- rev(pal)
  
  if (missing(n))
    n <- length(pal)
  
  if (is.null(pal))
    stop(sprintf('palette %s not found', shQuote(name)))
  
  if (type == 'discrete' && n > length(pal)) {
    warning(sprintf('palette %s has max %s colors, try type = \'continuous\'',
                    shQuote(name), length(pal)))
    n <- length(pal)
  }
  
  res <- switch(
    type,
    continuous = grDevices::colorRampPalette(pal[seq.int(n)])(z),
    discrete = pal[seq.int(n)]
  )
  
  structure(res, class = 'rawr_pal', name = name)
}

#' @rdname rawr_palettes
#' @export
show_pal <- function(x, fullrange = FALSE, counts = TRUE) {
  imgpal <- inherits(x, 'imgpal')
  
  if (inherits(x, 'rawr_pal')) {
    name <- attr(x, 'name')
    pal <- x
  } else if (length(x) == 1L) {
    pal <- rawr::rawr_palettes
    idx <- match(tolower(x), tolower(names(pal)), nomatch = 0L)
    if (idx == 0L)
      stop(sprintf('palette %s not found', shQuote(x)), call. = FALSE)
    pal <- pal[[idx]]
    name <- x
  } else if (imgpal) {
    obj <- x
    pal <- obj$col
    name <- obj$filename
    pal <- pal[seq.int(if (fullrange) length(pal) else obj$n_unique)]
  } else {
    pal <- x
    name <- deparse(substitute(x))
  }
  
  n <- length(pal)
  
  op <- par(mar = rep_len(1, 4L))
  on.exit(par(op))
  image(seq.int(n), 1, matrix(seq.int(n)), col = pal, ann = FALSE, axes = FALSE)
  abline(v = seq.int(n) + 0.5, col = 'white')
  
  ## add bars of color frequencies
  if (imgpal && counts) {
    ht <- obj$counts[seq.int(n)]
    ht <- rescaler(ht, par('usr')[3:4], c(0, max(ht)))
    rect(seq.int(n) - 0.5, par('usr')[3L], seq.int(n) + 0.25, ht,
         col = 'white', density = 10, angle = 45)
    rect(seq.int(n) - 0.5, par('usr')[3L], seq.int(n) + 0.25, ht,
         col = 'white', density = 10, angle = -45)
  }
  
  rect(0, 0.9, n + 1, 1.1, col = adjustcolor('white', 0.8), border = NA)
  text((n + 1) / 2, 1, labels = name)
  
  invisible(pal)
}

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
