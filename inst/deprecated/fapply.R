fapply <- function(X, FUN, ...) {
  fn <- as.character(match.call()$FUN)[-1L]
  nn <- if (is.null(names(FUN)))
    fn else replace(names(FUN), !nzchar(names(FUN)), fn[!nzchar(names(FUN))])
  res <- lapply(FUN, mapply, X, ...)
  setNames(data.frame(res, stringsAsFactors = FALSE), nn)
}
