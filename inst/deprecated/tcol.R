#' Transparent colors
#' 
#' Add alpha transparency to colors.
#' 
#' This is a vectorized function to add transparency to colors. \code{color}
#' and \code{trans} (or \code{alpha}) must either be the same length or one
#' of the two must have length one. 
#' 
#' The function adds integers (in hexadecimal) between 0 (fully transparent)
#' and 255 (fully visible) to the color(s) given. \code{color} values are
#' converted to RGB with transparency.
#' 
#' @param colors a vector of color names as character strings (or hexadecimal
#' strings) or integers corresponding to colors in the current
#' \code{\link{palette}}
#' @param trans alpha transparency defined as an integer in the range
#' \code{[0, 255]} where \code{0} is fully transparent and \code{255} is fully
#' visible; see details
#' @param alpha the alpha transparency in \code{[0,1]}; \code{trans} is
#' ignored if \code{alpha} is given
#' 
#' @seealso
#' \code{\link{as.hexmode}}, \code{\link{col2rgb}}, \code{\link{adjustcolor}}
#' 
#' @examples
#' cols <- c('red', 'green', 'blue')
#' 
#' ## a normal plot
#' plot(rnorm(100), col = tcol(cols), pch = 16, cex = 4)
#' 
#' ## more transparent
#' plot(x <- rnorm(100), col = tcol(cols, 100), pch = 16, cex = 4)
#' ## or equivalently using alpha
#' plot(x, col = tcol(cols, alpha = .4), pch = 16, cex = 4)
#' 
#' ## hexadecimal colors also work
#' cols <- c('#FF0000','#00FF00','#0000FF')
#' plot(rnorm(100), col = tcol(cols, c(50, 100, 255)), pch = 16, cex = 4)
#' 
#' @export

tcol <- function(colors, trans = NULL, alpha = NULL) {
  trans <- trans %||% 255L
  stopifnot(
    trans %inside% c(0L, 255L) | is.na(trans)
  )
  
  ## convert alpha to trans
  if (!is.null(alpha)) {
    stopifnot(
      alpha %inside% c(0, 1) | is.na(alpha)
    )
    trans <- as.integer(rescaler(alpha, to = c(0, 255), from = c(0, 1)))
  }
  
  ## get color and trans to conformable lengths
  if (length(colors) != length(trans) & 
      !any(c(length(colors), length(trans)) == 1L))
    stop('Vector lengths are not conformable')
  if (length(colors) == 1L & length(trans) > 1L)
    colors <- rep_len(colors, length(trans))
  if (length(trans) == 1L & length(colors) > 1L)
    trans <- rep_len(trans, length(colors))
  
  ## if color == 0, returns NA
  if (length(nocol <- which(colors == 0))) {
    colors[nocol] <- 1
    trans[nocol] <- NA
  }
  
  res <- paste0('#', apply(apply(rbind(col2rgb(colors)), 2L, function(x)
    format(as.hexmode(x), width = 2L)), 2L, paste, collapse = ''))
  res <- Map(paste0, res, tryCatch(
    as.character(as.hexmode(trans)),
    error = function(e) '', warning = function(w) ''
  ))
  res <- unname(unlist(res))
  
  ## return NAs and/or set color to transparent
  res[is.na(colors) | is.na(trans)] <- NA
  res[colors %in% 'transparent'] <- 'transparent'
  
  res
}
