lss <- function(pos = 1L, pattern, by = NULL, all.names = FALSE,
                decreasing = TRUE, n = 40L) {
  if (!length(ls(envir = as.environment(pos))))
    return(character(0L))
  
  napply <- function(names, fn, ..., simplify = TRUE)
    sapply(names, function(x) fn(get(x, pos = pos), ...), simplify = simplify)
  names <- ls(pos = pos, pattern = pattern, all.names = all.names)
  
  cl <- napply(names, function(x) as.character(class(x))[1L])
  mo <- napply(names, mode)
  
  type  <- ifelse(is.na(mo), mo, cl)
  size  <- napply(names, object.size, simplify = FALSE)
  sizef <- sapply(size, format, units = 'auto')
  dims  <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
  idx   <- is.na(dims)[, 1L] & (type != 'function')
  dims[idx, 1L] <- napply(names, length)[idx]
  
  res <- data.frame(
    type, size = unlist(size), sizef, nrow = dims[, 1L], ncol = dims[, 2L],
    ' ' = symnum(unlist(size), corr = FALSE, na = FALSE,
                 cutpoints = c(-Inf, 0.001, 0.1, 0.5, 1, Inf) * 1024 ^ 3,
                 symbols = c(' ', '.', '*', '**', '***')),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  
  if ((mb <- sum(napply(names, object.size)) / (1024 ^ 2)) > 1)
    on.exit(message(sprintf('Total size: %s Mb', roundr(mb, 1L))))
  
  if (!is.null(by))
    res <- res[order(res[, by], decreasing = decreasing), ]
  
  head(res, n)
}
