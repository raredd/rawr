parse_formats_file <- function(x, invert) {
  x <- rm_nonascii(readLines(x))
  x <- rm_sas_comments(paste0(x, collapse = '_$$$_'))
  x <- strsplit(x, '_$$$_', fixed = TRUE)[[1L]]
  
  ## extract format names and definitions
  ## TODO: fails if "value" is not followed by the value, i.e.,
  ##       fails if they are on separate lines due to strsplit
  ##       maybe use capture regex on an entire string
  ## TODO: probably fails for multiple formats on same line and/or
  ##       e.g., value $val '1','2-3'='something' cases
  name <- gsub('(?i)value\\s*([a-z0-9_$]+)|.', '\\1', x, perl = TRUE)
  name <- locf(name)
  
  ## ENHC: apply formats for ranges of numeric data
  p <- '[\'\"].+[\'\"]|[\\w\\d-]+'
  def <- gsub(sprintf('(?i)\\s*(%s)\\s*(=)\\s*(%s)|.', p, p),
              '\\1\\2\\3', x, perl = TRUE)
  
  ## separate and clean format values/labels
  dd <- data.frame(n = name, d = def, stringsAsFactors = FALSE)
  dd <- dd[with(dd, is.na(n) | nzchar(d)), ]
  sp <- split(dd$d, dd$n)
  
  lapply(sp, function(x)
    parse_formats_string(toString(x), invert))
}
