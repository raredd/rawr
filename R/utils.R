### utilities
# psum, rescaler, clc, clear, bind_all, cbindx, rbindx, rbindfill, rbindfill2,
# rbindlist, rbindlist2, interleave, outer2, merge2, locf, roll_fun,
# classMethods, getMethods, regcaptures, regcaptures2, cast, melt, View2, view,
# clist, rapply2, sort_matrix, insert, insert_matrix, tryCatch2, rleid,
# droplevels2, combine_levels, combine_regex, rownames_to_column,
# column_to_rownames, split_nth, sort2
# 
# rawr_ops:
# %ni%, %==%, %||%, %sinside%, %winside%, %inside%, %:%
# 
# rawr_ls:
# lss, lsf, lsp
# 
# rawr_parse:
# parse_yaml, parse_index, parse_news, parse_namespace
# 
# unexported:
# islist, done, where_, where, dots, name_or_index, insert_
###


islist <- function(x) {
  ## is.list(data.frame()); rawr:::islist(data.frame())
  inherits(x, 'list')
}

done <- function(type = c('notifier', 'beep')) {
  type <- match.arg(type)
  switch(type,
    notifier = notifier::notify(
      sprintf('R task is complete - %s', format(Sys.time(), '%I:%M %p')),
      if (nzchar(Sys.getenv('RSTUDIO')))
        'RStudio' else 'R'
    ),
    beep = while (TRUE) {
      beepr::beep(3)
      Sys.sleep(3)
    }
  )
}

where_ <- function(x, env = environment(NULL)) {
  ## recursively find env where x is defined
  stopifnot(
    is.character(x),
    length(x) == 1L
  )
  if (identical(env, emptyenv())) {
    stop(x, ' not found', call. = FALSE)
  }
  
  if (exists(x, env, inherits = FALSE))
    env else Recall(x, parent.env(env))
}

where <- function(x) {
  ## recursively find env where x is defined
  stopifnot(
    is.character(x),
    length(x) == 1L
  )
  sys <- sys.frames()
  
  for (env in sys)
    if (exists(x, env, inherits = FALSE))
      return(env)
  
  where_(x)
}

dots <- function(...) {
  ## rawr:::dots(mean, 1, x = y, rnorm(5))
  # eval(sys.call())
  eval(substitute(alist(...)))
}

name_or_index <- function(x, y = NULL) {
  ## return integer vector where x occurs in y
  ## rawr:::name_or_index(c('1', '3', 'e'))
  ## rawr:::name_or_index(c('a', 'c', 'e'), letters)
  ## table is given priority over integer, eg, idx = 27 instead of 4
  ## rawr:::name_or_index(c('a', '4', 'e'), c(letters, '4'))
  suppressWarnings(
    ix <- as.integer(x)
  )
  
  if (!is.null(y)) {
    iy <- match(x, y)
    replace(iy, is.na(iy), ix[is.na(iy)])
  } else ix
}

#' rawr operators
#' 
#' Some useful binary operators.
#' 
#' \code{\%ni\%} is the negation of \code{\link[base]{\%in\%}}.
#' 
#' \code{\%winside\%} and \code{\%sinside\%} return a logical vector
#' indicating if \code{x} is weakly or strongly inside \code{interval}.
#' \code{\%inside\%} is an alias for \code{\%winside\%} to preserve existing
#' code.
#'
#' \code{\%=\%} is an operator combining the qualities of \code{\link{==}} and
#' \code{\link{\%in\%}} to compare vectors in a pairwise manner which may
#' include \code{\link{NA}}s.
#' 
#' \code{\%||\%} is useful for a function, \code{f}, that may return a value
#' or \code{NULL}; however if \code{NULL} is the return value of \code{f}, it
#' is desirable to return some other default value.
#' 
#' \code{\%:\%} is useful for obtaining a range of a vector (usually
#' \code{colnames} or \code{names} of a matrix or data frame) by literal
#' character strings rather than by index.
#' 
#' @param x vector or \code{NULL}; the values to be matched
#' @param table vector or \code{NULL}; the values to be matched against
#' @param interval numeric vector of length two representing the interval
#' @param a,b raw, logical, "number-like" vectors or objects
#' @param object a \emph{named} vector or list, a matrix or data frame
#' @param range a numeric or character vector of length two with the indices
#' or names from \code{object}, generally of the structure \code{c(from, to)}
#' 
#' @seealso
#' \code{\link{==}}, \code{\link{\%in\%}}, \code{\link{||}}
#' 
#' @examples
#' 1:5 %ni% 3:5
#' 
#' 
#' c(0,4) %winside% c(0, 4)
#' c(0,4) %sinside% c(0, 4)
#' 
#' -5:5 %winside% c(0,5)
#' -5:5 %sinside% c(0,5)
#' 
#' 
#' a <- c(1, NA, 2)
#' b <- c(2, NA, 1)
#' ## not desired
#' a == b     # FALSE NA   FALSE
#' a %in% b   # TRUE  TRUE TRUE
#' ## desired results
#' a %==% b   # FALSE TRUE FALSE
#' 
#' 
#' # NULL || TRUE   # error
#' NULL %||% TRUE   # TRUE
#' 
#' 
#' 1:5 %:% c(3,5)
#' letters %:% c('e', 'n')
#' 
#' ## these are equivalent
#' mtcars %:% c('hp','vs')
#' mtcars %:% c(4, 8)
#' names(mtcars[, 4:8])
#' 
#' @aliases oror notin inside
#' @name rawr_ops
NULL

#' @rdname rawr_ops
#' @export
'%ni%' <- function(x, table) {
  !(match(x, table, nomatch = 0L) > 0L)
}

#' @rdname rawr_ops
#' @export
`%sinside%` <- function(x, interval) {
  interval <- sort(interval)
  x > interval[1L] & x < interval[2L]
}

#' @rdname rawr_ops
#' @export
`%winside%` <- function(x, interval) {
  interval <- sort(interval)
  x >= interval[1L] & x <= interval[2L]
}

#' @rdname rawr_ops
#' @export
`%inside%` <- `%winside%`

#' @rdname rawr_ops
#' @export
`%==%` <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

#' @rdname rawr_ops
#' @export
`%||%` <- function(a, b) {
  if (!is.null(a))
    a else b
}

#' @rdname rawr_ops
#' @export
`%:%` <- function(object, range) {
  FUN <- if (!is.null(dim(object))) {
    if (is.matrix(object)) colnames else names
  } else identity
  
  wh <- if (is.numeric(range))
    range else which(FUN(object) %in% range)
  
  FUN(object)[seq(wh[1L], wh[2L])]
}

#' List utilities
#' 
#' These functions begin with \code{ls} and list different things. \code{lss}
#' is an "improved" \code{\link{ls}} which gives more details of objects in
#' the workspace such as type, size, and dimensions; \code{lsp} lists
#' \code{p}ackage contents, i.e., exported and/or non exported obejcts,
#' methods, lazy data, etc.; and \code{lsf} lists package \code{f}iles. See
#' details and examples.
#' 
#' \code{lsf} prints package files (e.g., \code{DESCRIPTION}, \code{NEWS},
#' \code{INDEX}, \code{NAMESPACE}, etc.) to console and returns (invisibly)
#' the files parsed for easier use.
#' 
#' \code{lsp} is a helper function to list exported (\code{?'::'}) and non
#' exported (\code{?':::'}) functions (and other features from a package's
#' \code{NAMESPACE} file).
#' 
#' Note that \code{base} and older packages do not have a \code{NAMESPACE}
#' file in which case, for \code{base} packages, \code{lsp} returns
#' \code{ls(.BaseNamespaceEnv, all.names = TRUE)}, and throws an error
#' otherwise.
#' 
#' Possible values for \code{what} are "\code{all}" (default), \code{NULL},
#' "\code{exports}", "\code{imports}", "\code{dynlibs}", "\code{lazydata}",
#' "\code{path}", "\code{S3methods}", "\code{spec}", and others depending on
#' the package.
#' 
#' \code{lsp(packagename, '?')} to see options for a specific package.
#' 
#' \code{lsp(packagename, NULL)} to return all information in a list.
#' 
#' @param pos argument specifying the environment as a position in search list
#' or a "list-like" object such as a data frame or list of objects
#' @param pattern optional \code{\link{regex}}; for \code{lss} only names
#' matching  \code{pattern} are returned; \code{\link{glob2rx}} can be used to
#' convert wildcard patterns to regular expressions; for \code{lsp} a text
#' pattern or regular expression passed to \code{\link{grep}} to filter the
#' results
#' @param by variable to order output ('type', 'size' (default), 'sizef',
#' 'nrow', or 'ncol')
#' @param all.names logical; if \code{TRUE}, all object names are returned; if
#' \code{FALSE}, names which begin with a \code{.} are omitted
#' @param decreasing logical; if \code{TRUE}, displays output in decreasing
#' order
#' @param n number of objects to displace if \code{head} is \code{TRUE}
#' @param package package name, as a \code{\link{name}} or literal character
#' string
#' @param file file to return as a character string; usual options are
#' \code{'DESCRIPTION'}, \code{'NEWS'}, \code{'INDEX'}, \code{'NAMESPACE'};
#' partial matching is supported and case is ignored
#' @param what what to get; \code{'all'} is default which returns all exported
#' and non exported functions in \code{package}; see details for more
#' 
#' @return
#' \code{lss} (invisibly) returns a data frame of the printed output.
#' \code{lsf} (invisibly) returns the contents of \code{file}. \code{lsp}
#' returns a vector of character strings.
#' 
#' @seealso
#' \code{\link{parse_yaml}}, \code{\link{parse_index}},
#' \code{\link{parse_news}}, \code{\link{parse_namespace}}; \code{\link{ls}},
#' \code{lss} adapted from \url{http://stackoverflow.com/q/1358003/2994949};
#' \code{\link{search}}
#' 
#' @examples
#' ## lss: use like ls
#' lss()
#' a <- rnorm(100000)
#' b <- matrix(1, 1000, 100)
#' lss()
#' 
#' ## lsf: these are equivalent ways to use
#' lsf(rawr)
#' lsf(rawr, 'DESCRIPTION')
#' lsf('rawr', 'des')
#' 
#' ## lsp: see the contents of a package
#' lsp(rawr)
#' 
#' ## return all one or two character functions from base package
#' lsp(base, pat = '^.{1,2}$')
#' 
#' ## for "what" options
#' lsp('rawr', '?')
#' 
#' ## library path
#' lsp('rawr', 'path')
#' 
#' ## to return everything
#' lsp('rawr')
#' 
#' ## data sets
#' lsp('rawr', 'lazydata')
#' 
#' @name rawr_ls
NULL

#' @rdname rawr_ls
#' @export
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
    on.exit(message(sprintf('Total size: %s Mb', rawr::roundr(mb, 1L))))
  
  if (!is.null(by))
    res <- res[order(res[, by], decreasing = decreasing), ]
  
  head(res, n)
}

#' @rdname rawr_ls
#' @export
lsf <- function(package, file = 'DESCRIPTION') {
  ## DESCRIPTION, INDEX, NEWS, NAMESPACE
  package <- as.character(substitute(package))
  
  p  <- do.call('lsp', list(package = package, what = 'path'))
  lf <- list.files(p)
  ff <- lf[grepl(sprintf('(?i)^%s.*$', file), lf, perl = TRUE)]
  
  if (!length(ff)) {
    message(sprintf('File \'%s\' not found', file))
    return(invisible(NULL))
  }
  
  f <- readLines(con <- file(fp <- file.path(p, ff)))
  on.exit(close(con))
  message(sprintf('Showing file:\n%s\n', fp))
  cat(f, sep = '\n')
  
  invisible(f)
}

#' @rdname rawr_ls
#' @export
lsp <- function(package, what, pattern) {
  if (!is.character(substitute(package)))
    package <- deparse(substitute(package))
  ns <- asNamespace(package)
  if (missing(pattern))
    pattern <- '.*'
  
  ## base package does not have NAMESPACE
  if (isBaseNamespace(ns)) {
    res <- ls(.BaseNamespaceEnv, all.names = TRUE)
    res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)]
  } else {
    ## for non base packages
    if (exists('.__NAMESPACE__.', envir = ns, inherits = FALSE)) {
      wh <- get('.__NAMESPACE__.', inherits = FALSE,
                envir = asNamespace(package, base.OK = FALSE))
      what <- if (missing(what)) 'all'
      else if ('?' %in% what)
        return(ls(wh)) 
      else ls(wh)[pmatch(what[1L], ls(wh))]
      
      if (!is.null(what) && !any(what %in% c('all', ls(wh))))
        stop('\'what\' should be one of ',
             paste0(shQuote(ls(wh)), collapse = ', '),
             ', or \'all\'', domain = NA)
      res <- sapply(ls(wh), getNamespaceInfo, ns = ns)
      res <- rapply(res, ls, classes = 'environment',
                    how = 'replace', all.names = TRUE)
      
      if (is.null(what))
        return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
      
      if (what %in% 'all') {
        res <- ls(getNamespace(package), all.names = TRUE)
        return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
      }
      
      if (!any(what %in% ls(wh)))
        message(sprintf('No NAMESPACE file found for package %s.', package))
      else {
        res <- res[[what]]
        res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)]
      }
    }
  }
}

#' Parsers
#' 
#' Some (mostly internal) simple parsers. \code{parse_yaml} can process
#' simple, single-level yaml-like files such as the \code{DESCRIPTION} files
#' of \code{R} packages; \code{parse_index}, \code{parse_news}, and
#' \code{parse_namespace} process their respective files. These have not
#' been tested for every case, particularly \code{parse_news} since there is
#' not a requirememnt on the structure of \code{NEWS} files.
#' 
#' @param x a vector of character strings
#' @param wh for \code{parse_namespace}, which types to parse and return
#' 
#' @return
#' A named list for each section type.
#' 
#' @seealso
#' \code{\link{parseNamespaceFile}}, \code{\link{lss}}, \code{\link{lsf}},
#' \code{\link{lsp}}
#' 
#' @examples
#' parse_yaml(lsf(rawr, 'desc'))
#' parse_index(lsf(rawr, 'index'))
#' parse_news(lsf(rawr, 'news'))
#' parse_namespace(lsf(rawr, 'name'))
#' 
#' @name rawr_parse
NULL

#' @rdname rawr_parse
#' @export
parse_yaml <- function(x) {
  pattern <- '(^[^:]+):\\s+?(.*)$'
  setNames(as.list(gsub(pattern, '\\2', x)), gsub(pattern, '\\1', x))
}

#' @rdname rawr_parse
#' @export
parse_index <- function(x) {
  ## collapse descriptions >1 line
  x <- strsplit(gsub('\\$\\$\\s+', ' ', paste0(x, collapse = '$$')),
                split = '\\$\\$')[[1L]]
  x <- strsplit(x, '\\s{2,}')
  
  as.list(sapply(x, function(xx) setNames(xx[2L], xx[1L])))
}

#' @rdname rawr_parse
#' @export
parse_news <- function(x) {
  ## assume some separator
  x <- Filter(nzchar, gsub('[-=_]{2,}', '', x))
  
  ## assume version updates state with letter, not -, *, etc
  nn <- x[idx <- grepl('^\\w+', x)]
  sp <- split(x, cumsum(idx))
  
  setNames(lapply(sp, '[', -1L), nn)
}

#' @rdname rawr_parse
#' @export
parse_namespace <- function(
  x, wh = c('import', 'export', 'exportPattern', 'importClass',
            'importMethod', 'exportClass', 'exportMethod',
            'exportClassPattern', 'useDynLib', 'nativeRoutine', 'S3method')
  ) {
  ## remove comments and collapse
  x <- paste0(gsub('#.*$', '', x), collapse = '')
  
  mm <- lapply(wh, function(xx)
    gregexpr(sprintf('(?i)%s\\((.*?)\\)', xx), x, perl = TRUE))
  
  setNames(
    lapply(mm, function(xx)
      gsub('^\\s+|\\s+$|\\s{2,}', '',
           unlist(strsplit(unlist(regcaptures(x, xx)), ',')))),
    wh)
}

#' Pairwise sum
#' 
#' Compute the pairwise sum of two or more vectors.
#' 
#' @param ... numeric vectors equal in length
#' @param na.rm logical; if \code{TRUE}, omits missing values (including
#' \code{\link{NaN}}) from calculations
#' 
#' @return
#' A single vector of element-wise sums.
#' 
#' @seealso
#' Adapted from \url{https://stackoverflow.com/a/13123779/2994949};
#' \code{\link{pmin}}; \code{\link{pmax}}
#' 
#' @examples
#' x <- c(-1, NA, 4, 15)
#' y <- c(NA, NA, 6, -1)
#' psum(x, y)
#' psum(x, y, na.rm = TRUE)
#' 
#' @export

psum <- function(..., na.rm = FALSE) {
  dat <- do.call('cbind', list(...))
  res <- rowSums(dat, na.rm = na.rm)
  
  idx <- !rowSums(!is.na(dat))
  res[idx] <- NA
  
  res
}

#' Rescale numeric vector
#' 
#' Rescale a numeric vector to have specified maximum and minimum; modified
#' from the \pkg{scales} package.
#' 
#' @param x numeric vector of values
#' @param to output range (numeric vector of length two)
#' @param from input range (numeric vector of length two); if not given,
#' \code{from} is calculated from the range of \code{x}
#' 
#' @seealso
#' \code{\link[scales]{rescale}}; \code{\link[scales]{zero_range}}
#' 
#' @examples
#' rescaler(1:100)
#' rescaler(runif(10), c(0.5, 1))
#' rescaler(1)
#' 
#' @export

rescaler <- function (x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  zero_range <- function(x, tol = .Machine$double.eps * 100) {
    if (length(x) == 1L)  return(TRUE)
    if (length(x) != 2L)  stop('\'x\' must be length one or two')
    if (any(is.na(x)))    return(NA)
    if (x[1L] == x[2L])   return(TRUE)
    if (all(is.infinite(x))) return(FALSE)
    m <- min(abs(x))
    if (m == 0) return(FALSE)
    abs((x[1L] - x[2L]) / m) < tol
  }
  
  if (zero_range(from) || zero_range(to))
    return(rep(mean(to), length(x)))
  
  (x - from[1L]) / diff(from) * diff(to) + to[1L]
}

#' Clear workspace
#' 
#' Clear the workspace by removing all objects in \code{\link{ls}} followed
#' by \code{\link[=gc]{garbage collection}}.
#' 
#' @param all.names logical; if \code{TRUE}, also removes hidden (dot) objects
#' 
#' @seealso
#' \code{\link{clear}}
#' 
#' @export

clc <- function(all.names = FALSE) {
  rm(list = ls(.GlobalEnv, all.names = all.names), envir = .GlobalEnv)
  gc(TRUE)
  invisible(NULL)
}

#' Clear console
#' 
#' Clear the console window.
#' 
#' @param ... ignored
#' 
#' @seealso
#' \code{\link{clc}}
#' 
#' @export

clear <- function(...) {
  cat('\014')
}

#' Bind objects
#' 
#' Utilities for binding objects with inconsistent dimensions.
#' 
#' \code{bind_all} and \code{rbindfill} are used for binding vectors,
#' the latter specifically for \code{\link{rbind}}ing \emph{named} vectors
#' \emph{a la} a "stacking" merge.
#' 
#' \code{cbindx} and \code{rbindx} take vector-, matrix-, and data frame-like
#' objects and bind normally, filling with \code{NA}s where dimensions are
#' not equal.
#' 
#' \code{rbindfill} stacks \emph{named} vectors by initializing a matrix
#' with all names and filling with \code{...} by row in the order given.
#' The column names will be a vector of the unique names in the order they
#' were given.
#' 
#' \code{rbindfill2} row-binds data frames with zero or more common column
#' names. \code{rbindfill2} starts with the first data frame given and
#' \code{rbind}s subsequent data frames adding new columns of \code{NA} as
#' needed to bind. Any columns with matching names will be aggregated;
#' otherwise, data frames without a matching column of data will be filled
#' with \code{NA}.
#' 
#' \code{rbindlist} converts a list of vectors into a long data frame with
#' two columns: the list index where each value was stored and the values
#' themselves. A third column will be added if \code{use.names = TRUE} which
#' will keep the names of each vector.
#' 
#' \code{rbindlist2} uses \code{rbindlist} to expand data frames with one or
#' more nested columns.
#' 
#' @param ... for \code{bind_all} and \code{rbindfill}, vectors;
#' \code{cbindx} and \code{rbindx} will accept vectors, matrices, data frames;
#' data frames should be used with \code{rbindfill2} but matrices (or a
#' combination) will work, but a data frame is returned; \code{rbindlist}
#' accepts vectors or one or more lists
#' @param which joining method; \code{'rbind'} or \code{'cbind'}
#' @param deparse.level integer controlling the construction of labels in
#' the case of non-matrix-like arguments (for the default method):\cr
#' \code{deparse.level = 0} constructs no labels; the default; \cr
#' \code{deparse.level = 1} or \code{2} constructs labels from the argument
#' names \cr see \code{\link{cbind}}
#' @param use.rownames logical; for \code{rbindfill2}, if \code{TRUE}, data
#' frames in a \emph{named} list will retain corresponding rownames; the
#' default is to remove rownames (note that this parameter is ignored if
#' \code{...} is not a named list)
#' 
#' for \code{rbindlist} and \code{rbindlist2}, return a data frame with or
#' without rownames; for \code{rbindlist2}, if \code{data} has no row names
#' set (i.e., are \code{"1", "2", ...}), then the default is \code{FALSE}
#' @param use.names logical; if \code{TRUE} and vectors are named, names
#' are preserved and added as a column
#' @param data a matrix or data frame
#' @param column for \code{rbindlist2}, the column(s) to be unnested
#' @param split,fixed,perl arguments passed to \code{\link{strsplit}}
#' controlling how nested column text should be split
#' 
#' @seealso
#' \code{\link{cbind}}; \code{\link{rbind}}; \code{\link{interleave}};
#' \code{\link{clist}}; \pkg{qpcR}
#' 
#' @examples
#' bind_all(1:5, 1:3, which = 'cbind')
#' bind_all(1:5, 1:3, which = 'rbind')
#' 
#' m1 <- matrix(1:4)
#' m2 <- matrix(1:4, 1)
#' 
#' cbindx(m1, m2)
#' rbindx(m1, m2)
#' rbindx(mtcars, m2)
#' 
#' 
#' ## "stack" named vectors
#' f <- function(x) setNames(letters[x], LETTERS[x])
#' x <- lapply(list(1:5, 3:6, 2:7, 26), f)
#' do.call('rbindfill', x)
#' 
#' ## "stack" matrices or data frames
#' colnames(m1) <- 'B'
#' colnames(m2) <- LETTERS[1:4]
#' rbindfill2(m1, m2)
#' rbindfill2(m2, m1)
#' 
#' set.seed(1)
#' dd <- matrix(NA, nrow = 1, ncol = 10)
#' dd <- as.data.frame(col(dd))
#' l <- setNames(lapply(1:5, function(x) dd[, sample(x), drop = FALSE]),
#'               letters[1:5])
#' 
#' Reduce('rbindfill2', l) ## or do.call('rbindfill2', l)
#' do.call('rbindfill2', c(l, use.rownames = TRUE))
#' rbindfill2(l$c, l$e)
#' 
#' rbindfill2(head(mtcars), head(cars))
#' 
#' 
#' ## "stack" a list of vectors with differing lengths
#' rbindlist(1:5)
#' rbindlist(1:5, 1:5)
#' 
#' l <- lapply(1:4, sequence)
#' rbindlist(l)
#' rbindlist(l[4L])
#' 
#' names(l) <- LETTERS[1:4]
#' rbindlist(l)
#' rbindlist(l[4L])
#' 
#' l <- lapply(l, function(x) setNames(x, letters[x]))
#' rbindlist(l, use.names = TRUE)
#' rbindlist(unname(l), use.names = TRUE)
#' 
#' 
#' ## unnest and stack a data frame or matrix
#' dd <- data.frame(
#'   id = 1:4, x = rnorm(4), y = sapply(l, toString),
#'   z = sapply(l, paste, collapse = '...')
#' )
#' rbindlist2(dd, 'y')
#' 
#' ## rownames are kept if data contains non default rownames
#' rbindlist2(`rownames<-`(dd, letters[1:4]), 'y')
#' 
#' ## multiple columns can be expanded sequentially
#' rbindlist2(dd, c('y', 'z'))
#' rbindlist2(dd, 'y', split = '\\W+(?![^3]+3)', perl = TRUE)
#' 
#' @name bindx
NULL

#' @rdname bindx
#' @export
bind_all <- function(..., which) {
  if (missing(which))
    stop('specify combining method: \'rbind\' or \'cbind\'')
  
  l <- list(...)
  if (any(sapply(l, function(x) !is.null(dim(x)))))
    warning('bind_all is intended for vectors: use cbindx or rbindx instead.')
  l <- lapply(l, `length<-`, max(sapply(l, length)))
  
  do.call(which, l)
}

#' @rdname bindx
#' @export
cbindx <- function (..., deparse.level = 1L) {
  na <- nargs() - (!missing(deparse.level))    
  deparse.level <- as.integer(deparse.level)
  stopifnot(
    0L <= deparse.level,
    deparse.level <= 2L
  )
  argl <- list(...)   
  while (na > 0L && is.null(argl[[na]])) {
    argl <- argl[-na]
    na <- na - 1L
  }
  if (na == 0L)
    return(NULL)
  if (na == 1L) {
    if (isS4(..1))
      return(cbind2(..1))
    else return(matrix(...))  ##.Internal(cbind(deparse.level, ...)))
  }
  
  if (deparse.level) {
    symarg <- as.list(sys.call()[-1L])[1L:na]
    Nms <- function(i) {
      if (is.null(r <- names(symarg[i])) || r == '') {
        if (is.symbol(r <- symarg[[i]]) || deparse.level == 2L)
          deparse(r)
      } else r
    }
  }
  ## deactivated, otherwise no fill in with two arguments
  if (na == 0) {
    r <- argl[[2L]]
    fix.na <- FALSE
  } else {
    nrs <- unname(lapply(argl, nrow))
    iV <- sapply(nrs, is.null)
    fix.na <- identical(nrs[(na - 1):na], list(NULL, NULL))
    ## deactivated, otherwise data will be recycled
    #if (fix.na) {
    #    nr <- max(if (all(iV)) sapply(argl, length) else unlist(nrs[!iV]))
    #    argl[[na]] <- cbind(rep(argl[[na]], length.out = nr),
    #        deparse.level = 0)
    #}
    if (deparse.level) {
      if (fix.na)
        fix.na <- !is.null(Nna <- Nms(na))
      if (!is.null(nmi <- names(argl)))
        iV <- iV & (nmi == '')
      ii <- if (fix.na)
        2:(na - 1L) else 2:na
      if (any(iV[ii])) {
        for (i in ii[iV[ii]])
          if (!is.null(nmi <- Nms(i)))
            names(argl)[i] <- nmi
      }
    }
    
    ## filling with NA's to maximum occuring nrows
    nRow <- as.integer(sapply(argl, function(x) NROW(x)))
    maxRow <- max(nRow, na.rm = TRUE)
    argl <- lapply(argl, function(x)
      if (is.null(nrow(x))) {
        c(x, rep(NA, maxRow - length(x)))
      } else rbindx(x, matrix(nrow = maxRow - nrow(x), ncol = ncol(x))))
    r <- do.call('cbind', c(argl[-1L], list(deparse.level = deparse.level)))
  }
  d2 <- dim(r)
  r <- cbind2(argl[[1L]], r)
  r <- rm_na_dimnames(r)
  if (deparse.level == 0L)
    return(r)
  ism1 <- !is.null(d1 <- dim(..1)) && length(d1) == 2L
  ism2 <- !is.null(d2) && length(d2) == 2L && !fix.na
  if (ism1 && ism2)
    return(r)
  Ncol <- function(x) {
    d <- dim(x)
    if (length(d) == 2L)
      d[2L]
    else as.integer(length(x) > 0L)
  }
  nn1 <- !is.null(N1 <- if ((l1 <- Ncol(..1)) && !ism1) Nms(1L))
  nn2 <- !is.null(N2 <- if (na == 2L && Ncol(..2) && !ism2) Nms(2L))
  if (nn1 || nn2 || fix.na) {
    if (is.null(colnames(r)))
      colnames(r) <- rep.int('', ncol(r))
    setN <- function(i, nams) colnames(r)[i] <<- if (is.null(nams)) ''
    else nams
    if (nn1)
      setN(1L, N1)
    if (nn2)
      setN(1L + l1, N2)
    if (fix.na)
      setN(ncol(r), Nna)
  }
  
  r
}

#' @rdname bindx
#' @export
rbindx <- function (..., deparse.level = 1L) {
  na <- nargs() - (!missing(deparse.level))
  deparse.level <- as.integer(deparse.level)
  stopifnot(
    0L <= deparse.level,
    deparse.level <= 2L
  )
  argl <- list(...)
  while (na > 0L && is.null(argl[[na]])) {
    argl <- argl[-na]
    na <- na - 1L
  }
  if (na == 0L)
    return(NULL)
  if (na == 1L) {
    if (isS4(..1))
      return(rbind2(..1))
    else return(matrix(..., nrow = 1L)) ##.Internal(rbind(deparse.level, ...)))
  }
  
  if (deparse.level) {
    symarg <- as.list(sys.call()[-1L])[1L:na]
    Nms <- function(i) {
      if (is.null(r <- names(symarg[i])) || r == '') {
        if (is.symbol(r <- symarg[[i]]) || deparse.level == 2)
          deparse(r)
      } else r
    }
  }
  ## deactivated, otherwise no fill in with two arguments
  if (na == 0L) {
    r <- argl[[2L]]
    fix.na <- FALSE
  } else {
    nrs <- unname(lapply(argl, ncol))
    iV <- sapply(nrs, is.null)
    fix.na <- identical(nrs[(na - 1L):na], list(NULL, NULL))
    ## deactivated, otherwise data will be recycled
    #if (fix.na) {
    #    nr <- max(if (all(iV)) sapply(argl, length) else unlist(nrs[!iV]))
    #    argl[[na]] <- rbind(rep(argl[[na]], length.out = nr),
    #        deparse.level = 0)
    #}
    if (deparse.level) {
      if (fix.na)
        fix.na <- !is.null(Nna <- Nms(na))
      if (!is.null(nmi <- names(argl)))
        iV <- iV & (nmi == '')
      ii <- if (fix.na)
        2:(na - 1L) else 2:na
      if (any(iV[ii])) {
        for (i in ii[iV[ii]])
          if (!is.null(nmi <- Nms(i)))
            names(argl)[i] <- nmi
      }
    }
    
    ## filling with NAs to maximum occuring ncols
    nCol <- as.integer(sapply(argl, function(x)
      if (is.null(ncol(x)))
        length(x) else ncol(x)))
    maxCol <- max(nCol, na.rm = TRUE)
    argl <- lapply(argl, function(x)
      if (is.null(ncol(x))) {
        c(x, rep(NA, maxCol - length(x)))
      } else cbind(x, matrix(nrow = nrow(x), ncol = maxCol - ncol(x))))
    
    ## create a common name vector from the
    ## column names of all 'argl' items
    namesVEC <- rep(NA, maxCol)
    for (i in seq_along(argl)) {
      CN <- colnames(argl[[i]])
      m <- !(CN %in% namesVEC)
      namesVEC[m] <- CN[m]
    }
    
    ## make all column names from common 'namesVEC'
    for (j in seq_along(argl)) {
      if (!is.null(ncol(argl[[j]]))) colnames(argl[[j]]) <- namesVEC
    }
    r <- do.call('rbind', c(argl[-1L], list(deparse.level = deparse.level)))
  }
  d2 <- dim(r)
  
  ## make all column names from common 'namesVEC'
  colnames(r) <- colnames(argl[[1L]])
  r <- rbind2(argl[[1L]], r)
  r <- rm_na_dimnames(r)
  
  if (deparse.level == 0L)
    return(r)
  ism1 <- !is.null(d1 <- dim(..1)) && length(d1) == 2L
  ism2 <- !is.null(d2) && length(d2) == 2L && !fix.na
  if (ism1 && ism2)
    return(r)
  Nrow <- function(x) {
    d <- dim(x)
    if (length(d) == 2L)
      d[1L] else as.integer(length(x) > 0L)
  }
  nn1 <- !is.null(N1 <- if ((l1 <- Nrow(..1)) && !ism1) Nms(1L))
  nn2 <- !is.null(N2 <- if (na == 2 && Nrow(..2) && !ism2) Nms(2L))
  if (nn1 || nn2 || fix.na) {
    if (is.null(rownames(r)))
      rownames(r) <- rep.int('', nrow(r))
    setN <- function(i, nams) rownames(r)[i] <<- if (is.null(nams)) ''
    else nams
    if (nn1)
      setN(1L, N1)
    if (nn2)
      setN(1L + l1, N2)
    if (fix.na)
      setN(nrow(r), Nna)
  }
  
  r
}

## set all NA dnn to NULL, mixture of NA/names to ''
rm_na_dimnames <- function(x, which = c('row', 'col'), rm_null = TRUE) {
  nn <- rownames(x)
  if (!is.null(nn) && 'row' %in% which) {
    if (all(na <- is.na(nn)))
      rownames(x) <- NULL
    else rownames(x)[na] <- ''
  }
  
  nn <- colnames(x)
  if (!is.null(nn) && 'col' %in% which) {
    if (all(na <- is.na(nn)))
      colnames(x) <- NULL
    else colnames(x)[na] <- ''
  }
  
  if (rm_null)
    if (all(sapply(dimnames(x), is.null)))
      dimnames(x) <- NULL
  
  x
}

#' @rdname bindx
#' @export
rbindfill <- function(...) {
  l <- list(...)
  if (length(l) == 1L)
    return(..1)
  
  nn <- lapply(l, names)
  if (any(vapply(nn, is.null, NA)))
    stop('\'rbindlist\' requires all vectors to be named', call. = FALSE)
    
  un <- unique(unlist(nn))
  ll <- sapply(l, length)
  
  res <- vector('list', length(ll))
  for (ii in seq_along(ll))
    res[[ii]] <- unname(l[[ii]])[match(un, nn[[ii]])]
  
  `colnames<-`(do.call('rbind', res), un)
}

#' @rdname bindx
#' @export
rbindfill2 <- function(..., use.rownames = FALSE) {
  l <- list(...)
  nn <- lapply(l, colnames)
  un <- unique(unlist(nn))
  
  if (any(vapply(nn, is.null, NA))) {
    warning('\'rbindfill2\' requires objects with column names\n\n',
            'Returning rbindx(...)')
    return(do.call('rbindx', l))
  }
  
  res <- lapply(l, function(x) {
    if (!all(wh <- un %in% names(x))) {
      mat <- matrix(NA, nrow = nrow(x), ncol = sum(!wh),
                    dimnames = list(NULL, un[!wh]))
      res <- do.call('cbind', list(x, mat))
      res[, un]
    } else x[, un]
  })
  res <- do.call('rbind', res)
  
  if (use.rownames)
    res else `rownames<-`(res, NULL)
}

#' @rdname bindx
#' @export
rbindlist <- function(..., use.rownames = FALSE, use.names = FALSE) {
  l <- if (is.list(..1))
    c(...) else list(...)
  if (length(l) == 1L) {
    # return(..1)
    names(l) <- names(l) %||% 1
    l <- c(l, list(extra = NA))
    l <- Recall(l, use.rownames = use.rownames, use.names = use.names)
    
    return(head(l, -1L))
  }
  
  # nn <- if (is.null(names(l)))
  #   seq_along(l) else make.unique(names(l))
  nn <- names(l) %||% seq_along(l)
  nn <- rep(nn, vapply(l, length, integer(1L)))
  res <- data.frame(idx = nn, value = unlist(l), stringsAsFactors = FALSE)
  
  if (use.names)
    res <- cbind(res, names = unlist(lapply(l, function(x)
      if (is.null(nn <- names(x))) rep(NA, length(x)) else nn)))
  
  if (use.rownames)
    res else `rownames<-`(res, NULL)
}

#' @rdname bindx
#' @export
rbindlist2 <- function(data, column, split = '\\W+', fixed = FALSE, perl = FALSE,
                       use.rownames = any(rownames(data) != seq.int(nrow(data)))) {
  if (missing(column) || any(column %ni% colnames(data)))
    return(data)
  force(use.rownames)
  
  while (length(column) > 1L) {
    data <- Recall(data, column[1L], split, fixed, perl, use.rownames)
    column <- column[-1L]
  }
  
  l <- strsplit(as.character(data[, column]), split, fixed, perl)
  if (all(lengths(l) == 1L)) {
    message('No rows were split for column ', shQuote(column))
    return(data)
  }
  l <- rawr::rbindlist(l, use.rownames = FALSE, use.names = FALSE)
  
  data <- data[l[, 1L], ]
  data[, column] <- as.character(data[, column])
  data[, column] <- l[, 2L]
  
  if (use.rownames)
    data else `rownames<-`(data, NULL)
}

#' Interleave rows or columns
#' 
#' Interleave rows (or columns) of vectors, matrices, data frames, or lists.
#' 
#' @param ... vectors, matrices, data frames, or lists
#' @param which joining method to use (\code{'rbind'} or \code{'cbind'}) when
#' \code{...} are matrices or data frames
#' @seealso \code{\link{bindx}}
#' 
#' @examples
#' interleave(letters[1:3], LETTERS[3:1], letters[26:24])
#' 
#' 
#' m1 <- matrix(1:9, 3, 3)
#' m2 <- matrix(1:9 * 10, 3, 3)
#' interleave(t(m1), t(m2), which = 'rbind')
#' interleave(m1, m2, which = 'cbind')
#' 
#' 
#' d1 <- data.frame(m1)
#' interleave(d1, d1, which = 'cbind')
#' 
#' l <- list(d1, d1)
#' interleave(l, which = 'cbind')
#' interleave(l, l, l, which = 'cbind')
#' 
#' 
#' # interleave(d1, m2, which = 'rbind')  ## error
#' interleave(d1, m2, which = 'rbindx')   ## works
#' 
#' @export

interleave <- function(..., which) {
  l <- if (islist(..1))
    c(...) else list(...)
  if (all(sapply(l, function(x) is.null(dim(x)))))
    return(c(do.call('rbind', l)))
  
  which <- match.arg(which, c('rbind','cbind','rbindx','cbindx'))
  
  if (which %in% c('rbind', 'rbindx'))
    do.call(which, l)[order(sequence(sapply(l, nrow))), ]
  else if (which %in% c('cbind', 'cbindx'))
    do.call(which, l)[, order(sequence(sapply(l, ncol)))]
  else stop('Invalid \'which\' argument')
}

#' Outer product of n-dimensional arrays
#' 
#' The outer product of the arrays \code{X}, \code{Y}, ... with dimensions
#' \code{c(dim(X), dim(Y), ...)}; see \code{\link{outer}}.
#' 
#' @param ... arguments passed to \code{FUN} in the order given
#' @param FUN a function to use on the outer products
#' 
#' @seealso \code{\link{outer}}; \code{\link{Vectorize}}
#' 
#' @examples
#' outer2(LETTERS[1:3], letters[1:3], LETTERS[24:26], FUN = paste0)
#' outer2(1:3, 1:3, 1:2, FUN = prod)
#' 
#' ## three-way example in ?outer
#' x <- setNames(1:9, 1:9); y <- setNames(2:8, paste0(2:8, ':'))
#' x %o% x %o% y[1:3]
#' outer2(x, x, y[1:3], FUN = prod)
#' 
#' @export

outer2 <- function(..., FUN) {
  vf <- Vectorize(function(x, y)
    c(as.list(x), as.list(y)), SIMPLIFY = FALSE)
  f <- function(l)
    Reduce(function(x, y) outer(x, y, vf), l)
  
  args <- f(list(...))
  res <- apply(args, 1:length(dim(args)), function(x)
    do.call(FUN, x[[1L]]))
  
  array(res, dim = dim(res), dimnames = list(...))
}

#' Recursively merge a list of data frames
#' 
#' Use \code{\link{merge}} to join \code{n} data frames.
#' 
#' @param l list of data frames or objects to be coerced
#' @param ... additional arguments passed to \code{merge} (eg, \code{by},
#' \code{all}, etc)
#' 
#' @seealso \code{\link[plyr]{join_all}}
#' 
#' @examples
#' a <- data.frame(id = 1:10, a = rnorm(10))
#' b <- data.frame(id = 4:6, b = rnorm(3))
#' c <- data.frame(id = 4:14, c = rpois(11, 1))
#' d <- matrix(c(1:5, rnorm(5)), nrow = 5,
#'             dimnames = list(NULL, c('id', 'd')))
#' 
#' merge2(list(a, b, c))
#' merge2(list(a, b, c), all = TRUE)
#' merge2(list(a, b, c, d), all = TRUE)
#' 
#' @export

merge2 <- function(l, ...) {
  Reduce(function(x, y) merge(x, y, ...), l)
}

#' Last observation carried forward
#' 
#' Replaces \code{NA} in vectors, data frames, or matrices with most recent
#' non-\code{NA} value.
#' 
#' @param x a vector, matrix, or data frame
#' @param fromLast logical or vector of logicals; if \code{TRUE}, observations
#' are carried backward rather than forward; other options such as
#' \code{c(TRUE, FALSE)} will call \code{locf} first for \code{fromLast = TRUE}
#' follwed by calling with \code{fromLast = FALSE} on the result
#' @param na.strings a vector of values to be treated as \code{NA}; optionally
#' a list of single elements can be used for mixed data types; see examples
#' 
#' @examples
#' x <- c('','','a','','b','','','','c')
#' locf(x)
#' locf(x, c(FALSE, TRUE))
#' locf(x, TRUE)
#' 
#' dd <- data.frame(V1 = c('Bob', NA, NA, 'Joe', NA, NA),
#'                  V2 = c(NA, 1, NA, NA, 2, NA), stringsAsFactors = FALSE)
#' 
#' locf(dd)
#' locf(dd, c(FALSE, TRUE))
#' locf(dd, na.strings = 2)
#' 
#' ## note the differences for numeric and character na.strings
#' locf(dd, na.strings = c('Joe', 2))
#' locf(dd, na.strings = list('Joe', 02))
#' locf(dd, na.strings = list('Joe', '02'))
#' 
#' 
#' ## with dates
#' dd$V2 <- as.Date(dd$V2, origin = '2000-01-01')
#' locf(dd)
#' locf(dd, TRUE)
#' locf(dd, c(FALSE, TRUE))
#' 
#' @export

locf <- function(x, fromLast = FALSE, na.strings = '') {
  if (length(fromLast) > 1L)
    x <- Recall(Recall(x, fromLast[1L], na.strings),
                fromLast[-1L], na.strings)
  if (!(ok <- !is.null(nrow(x))) || is.matrix(x))
    x <- data.frame(x, stringsAsFactors = FALSE)
  
  fromLast <- rep(fromLast, ncol(x))
  
  if (length(na.strings))
    for (ii in seq_along(na.strings))
      try({
        ## this throws an error for special classes like dates
        x[x == unlist(na.strings[ii])] <- NA
      }, silent = TRUE)
  indx <- !is.na(x)
  
  x[] <- lapply(seq_along(x), function(ii) {
    if (fromLast[ii]) {
      idx <- rev(cumsum(rev(indx[, ii])))
      idx[idx == 0] <- NA
      rev(x[, ii])[rev(indx[, ii])][idx]
    } else {
      idx <- cumsum(indx[, ii])
      idx[idx == 0] <- NA
      x[, ii][indx[, ii]][idx]
    }
  })
  
  if (ok)
    x else x[, 1L]
}

#' Rolling functions
#' 
#' Apply rolling functions
#' 
#' @param x a vector
#' @param n size of groups
#' @param FUN a function to apply as a \code{\link{name}} or literal character
#' string
#' @param ... additional arguments passed to \code{FUN}
#' @param fromLast logical; if \code{TRUE}, \code{roll_fun} is applied to
#' \code{x} in reverse order
#' @param keep logical; if \code{TRUE}, the rolling \code{FUN} is applied to
#' the first \code{n} elements of \code{x}; if \code{FALSE} (default), then
#' \code{FUN} is applied to \code{x[1]}, \code{x[1:2]}, ..., \code{x[1:n]},
#' ie, until groups of size \code{n} are possible
#' 
#' @return A vector of the same length of \code{x} with calculations obtained
#' by \code{FUN}.
#' 
#' @examples
#' cbind(1:10, roll_fun(1:10, 2, keep = TRUE))
#' cbind(rep(1, 10), roll_fun(rep(1, 10), 5, sum))
#' 
#' dat <- data.frame(x = c(1,1,2,2,2,3,4,5,5,5),
#'                   y = 1:10)
#' ## compare:
#' within(dat,
#'   z <- unlist(by(dat, dat$x, function(ii)
#'           roll_fun(ii$y, length(ii$y), sum))))
#' do.call('rbind', by(dat, dat$x, cumsum))
#' 
#' @export

roll_fun <- function(x, n = 5L, FUN = mean, ...,
                     fromLast = FALSE, keep = FALSE) {
  l <- lapply(seq_along(x), function(ii) {
    if (fromLast)
      x[length(x) + 1L - tail(sequence(ii), n)]
    else x[tail(sequence(ii), n)]
  })
  
  if (keep)
    l[1:n] <- lapply(1:n, function(x) l[[n]])
  
  sapply(if (fromLast) rev(l) else l, FUN, ...)
}

#' Show or get methods
#' 
#' List available methods for a given class or identify a specific method
#' when a \code{generic} is called with \code{object}.
#' 
#' @param object an object or character vector of classes
#' @param generic an S3 generic function like \code{plot} or \code{summary}
#' 
#' @seealso
#' \code{\link{methods}}, \code{\link{S3Methods}}, \code{\link{class}}
#' 
#' @references
#' \url{https://gist.github.com/MrFlick/55ed854eb935e5c21f71};
#' \url{https://stackoverflow.com/q/42738851/2994949}
#' 
#' @examples
#' fit <- glm(vs ~ mpg, data = mtcars)
#' classMethods(fit)
#' classMethods(c('glm', 'lm'))
#' 
#' classMethods(1, plot)
#' classMethods(data.frame(1), plot)
#' classMethods(density(1:2), plot)
#' 
#' classMethods(1, print)
#' classMethods(ordered(1), print)
#' classMethods(mtcars, summary)
#' 
#' @export

classMethods <- function(object, generic = NULL) {
  if (!is.null(generic)) {
    generic <- if (is.character(generic))
      generic else deparse(substitute(generic))
    return(genericMethods(object, generic))
  }
  
  class <- if (!is.character(object))
    class(object) else object
  message('S3 methods for objects of class ', toString(class), '\n')
  
  ml <- lapply(class, function(x) {
    sname <- gsub('([.[])', '\\\\\\1', paste0('.', x, '$'))
    m <- methods(class = x)
    if (length(m)) {
      data.frame(m = as.vector(m), c = x, n = sub(sname, '', as.vector(m)),
                 attr(m, 'info'), stringsAsFactors = FALSE)
    } else NULL
  })
  
  dd <- do.call('rbind', ml)
  dd <- dd[!duplicated(dd$n), ]
  
  structure(
    dd$m, byclass = FALSE, class = 'MethodsFunction',
    info = data.frame(visible = dd$visible, from = dd$from,
                      generic = dd$generic, isS4 = dd$isS4,
                      row.names = dd$m)
  )
}

genericMethods <- function(object, generic) {
  generic <- if (is.character(generic))
    generic else deparse(substitute(generic))
  
  f <- X <- function(x, object) UseMethod('X')
  
  for (m in methods(generic))
    assign(sub(generic, 'X', m), `body<-`(f, value = m))
  
  X(object)
}

#' Extract captured substrings
#' 
#' Extract the captured substrings from match data obtained by
#' \code{\link{regexpr}}, \code{\link{gregexpr}}, or \code{\link{regexec}}.
#' \code{regcaptures2} is a convenience wrapper for \code{regcaptures}; see
#' examples.
#' 
#' @param x a character vector
#' @param m an object with match data
#' @param use.names logical; if \code{FALSE}, all names (capture names and
#' list names) will be stripped; if \code{TRUE} (default) and capture groups
#' have names, these will be used; otherwise, match start positions will be
#' used
#' @param pattern a character string containing a Perl-compatible regular
#' expression
#' 
#' @return
#' A list with a matrix of captures for each string in \code{x}. Note that the
#' column names of each matrix will be the starting positions of the captures.
#' 
#' @seealso
#' \code{\link{regmatches}}; \code{\link{grep}}; \code{\link{regex}}
#' 
#' @references
#' Adapted from \url{https://gist.github.com/MrFlick/10413321}
#' 
#' @examples
#' x <- c('larry:35,M', 'alison:22,F', 'dave:,M', 'lily:55,F', 'no data')
#' p1 <- '(.*):(\\d+)?,([MF])?'
#' p2 <- '(?<name>.*):(?<age>\\d+)?,(?<sex>[MF])?'
#' 
#' m <- regexpr(p1, x, perl = TRUE)
#' regcaptures(x, m)
#' do.call('rbind.data.frame', regcaptures(x, m, use.names = FALSE))
#' 
#' ## regcaptures2 is a convenience function for the two step above
#' regcaptures2(x, p1)
#' 
#' ## both will use named captures (if perl = TRUE)
#' regcaptures(x, gregexpr(p2, x, perl = TRUE))
#' do.call('rbind.data.frame', regcaptures2(x, p2))
#' 
#' 
#' ## capture overlapping matches
#' x <- 'ACCACCACCCAC'
#' m <- gregexpr('(?=([AC]C))', x, perl = TRUE)
#' regcaptures(x, m)[[1]]
#' 
#' m <- gregexpr('(?=(CC))', x, perl = TRUE)
#' regcaptures(x, m)[[1]]
#' 
#' ## compare:
#' mapply(function(xx) substr(x, xx, xx + 1L), m[[1]])
#' 
#' @export

regcaptures <- function(x, m, use.names = TRUE) {
  if (length(x) != length(m))
    stop('\'x\' and \'m\' must have the same length')
  oo <- options(stringsAsFactors = FALSE)
  on.exit(options(oo))
  
  ili <- is.list(m)
  useBytes <- if (ili)
    any(unlist(lapply(m, attr, 'useBytes'))) else any(attr(m, 'useBytes'))
  if (useBytes) {
    asc <- iconv(x, 'latin1', 'ASCII')
    ind <- is.na(asc) | (asc != x)
    if (any(ind))
      Encoding(x[ind]) <- 'bytes'
  }
  
  msg <- 'No capture data found'
  if (ili) {
    if (any(sapply(m, function(x) is.null(attr(x, 'capture.start')))))
      stop(msg)
    cs <- lapply(m, function(x) attr(x, 'capture.start'))
    cl <- lapply(m, function(x) attr(x, 'capture.length'))
    cn <- lapply(m, function(x) attr(x, 'capture.names'))
  } else {
    if (is.null(attr(m, 'capture.start')))
      stop(msg)
    cs <- data.frame(t(attr(m, 'capture.start')))
    cl <- data.frame(t(attr(m, 'capture.length')))
    cn <- data.frame(  attr(m, 'capture.names'))
  }
  
  Substring <- function(x, starts, lengths, names) {
    if (!all(starts < 0L)) {
      ss <- t(mapply(function(x, st, ln)
        substring(x, st, st + ln - 1L), x, data.frame(t(starts)),
        data.frame(t(lengths)), USE.NAMES = FALSE))
      tryCatch(`colnames<-`(ss, if (!use.names) NULL else
        if (all(!nzchar(names))) starts else names),
        error = function(e) if (grepl('not equal to array extent', e))
          `colnames<-`(ss, NULL) else e)
    } else character(0L)
  }
  
  Map(Substring, x, cs, cl, cn, USE.NAMES = use.names)
}

#' @rdname regcaptures
#' @export

regcaptures2 <- function(x, pattern, use.names = TRUE) {
  regcaptures(x, gregexpr(pattern, x, perl = TRUE), use.names)
}

#' Reshape data
#' 
#' Convenience wrappers for \code{\link{reshape}} where \code{direction}
#' is \code{"long"} (\code{melt}) or \code{"wide"} (\code{cast}).
#' 
#' These functions set defaults for reshaping data; any defaults may be
#' overwritten by simply passing arguments to \dots (names must match exactly
#' and no partial matching is allowed or they will be ignored).
#' 
#' By default, \code{cast} assumes \code{data} is a data frame with at least
#' three columns representing id, time point, and value variables (any
#' additional columns will be considered value variables as well). \code{melt}
#' by default assumes no id variables and will melt all columns.
#' 
#' \code{idvar}, \code{timevar}, \code{v.names}, and \code{varying} can be
#' passed as a \emph{vector} of column indices or character strings of
#' variable names or as a \emph{list} of indices or character strings names
#' for convenience.
#' 
#' @param data a data frame
#' @param idvar id variable(s)
#' @param timevar time point variable(s)
#' @param v.names long variable(s) to be flattened
#' @param varying variable(s) to melt into column(s)
#' @param ... additional arguments passed to \code{reshape}
#' 
#' @return
#' The reshaped data frame with added attributes to simplify reshaping back
#' to the original form.
#' 
#' @seealso
#' \code{\link{reshape}}
#' 
#' @examples
#' ## default of melt is no id variable so all are melted
#' melt(mtcars)
#' 
#' ## compare to
#' reshape(mtcars, dir = 'long', varying = list(names(mtcars)))
#' 
#' 
#' dat <- data.frame(id = rep(1:4, rep(2, 4)),
#'                   visit = I(rep(c('Before', 'After'), 4)),
#'                   x = abs(rpois(4, 10)), y = -runif(4))
#' 
#' ## equivalent ways to give `varying`
#' melt(dat, list(3:4))
#' melt(dat, 3:4)
#' l <- melt(dat, c('x','y'))
#' 
#' ## equivalent ways to give `idvar`, `timevar`, `v.names`
#' cast(l, c('id','visit'), 'variable', 'value')
#' cast(l, list('id','visit'), 3, 4)
#' cast(l, 1:2, 3, list('value'))
#' 
#' ## cast assumes columns are id, time, followed by all varying:
#' cast(dat)
#' 
#' 
#' dat <- within(dat, {
#'   xx <- abs(rpois(4, 10))
#'   yy <- -runif(4, 1, 2)
#' })
#' 
#' ## melting multiple variables simultaneously
#' melt(dat, list(c(3,6), c(4:5)))
#' 
#' ## note that a vector cannot be used here for the same results
#' melt(dat, c('x','xx','y','yy'))
#' 
#' ## use a list instead
#' melt(dat, list(c('x','xx'), c('y','yy')))
#' 
#' 
#' ## melt or cast can also be undone as other reshape objects
#' w1 <- reshape(Indometh, v.names = "conc", idvar = "Subject",
#'               timevar = "time", direction = "wide", new.row.names = 1:6)
#' w2 <- cast(Indometh, v.names = "conc", idvar = "Subject",
#'            timevar = "time", direction = "wide")
#' 
#' identical(w1, w2) ## TRUE
#' identical(reshape(w1), reshape(w2)) ## TRUE
#' 
#' @name Reshape
NULL

#' @rdname Reshape
#' @export
cast <- function(data, idvar = list(1), timevar = list(2),
                 v.names = list(3:ncol(data)), ...) {
  n <- names(data)
  f <- function(x) if (is.numeric(y <- unlist(x))) n[y] else y
  idvar <- f(idvar)
  timevar <- f(timevar)
  v.names <- f(v.names)
  
  ## use reshape defaults and set cast defaults
  # l <- c(as.list(formals(reshape)), list(...))
  l <- as.list(formals(reshape))
  l$direction <- 'wide'
  l$idvar     <- idvar
  l$v.names   <- v.names
  l$data      <- data
  l$timevar   <- timevar
  l$times     <- l$ids <- NULL
  
  ## also allow any cast defaults to be overridden, ie, direction = 'long'
  ## but names must match exactly, ie, dir = 'long' will not work
  # l <- l[!duplicated(names(l), fromLast = TRUE)]
  l <- modifyList(l, list(...))
  res <- do.call('reshape', l)
  
  `rownames<-`(res, NULL)
}

#' @rdname Reshape
#' @export
melt <- function(data, varying = list(1:ncol(data)), ...) {
  n <- names(data)
  if (!is.list(varying))
    varying <- if (is.numeric(varying))
      list(varying) else list(which(n %in% varying))
  vl <- length(varying) == 1L
  
  ## use reshape defaults and set melt defaults
  # l <- c(as.list(formals(reshape)), list(...))
  l <- as.list(formals(reshape))
  l$direction <- 'long'
  l$varying   <- varying
  l$times     <- if (vl) n[varying[[1L]]] else seq_along(varying[[1L]])
  l$data      <- data
  l$idvar     <- '_id_'
  l$timevar   <- if (vl) 'variable' else 'time'
  l$v.names   <- if (vl) 'value' else paste0('value', 1:length(varying))
  
  ## also allow any melt defaults to be overridden, ie, direction = 'wide'
  ## but names must match exactly, ie, dir = 'wide' will not work
  # l <- l[!duplicated(names(l), fromLast = TRUE)]
  l <- modifyList(l, list(...))
  res <- do.call('reshape', l)
  res$'_id_' <- NULL
  
  `rownames<-`(res, NULL)
}

#' View data
#' 
#' Convenience functions to use the base \code{R} data viewer (\code{View2}
#' always invokes \code{\link[utils]{View}} instead of the rstudio viewer)
#' or the default browser (\code{view} which can open html or widgets in
#' the browser or to view data frame- or matrix-like objects using
#' \code{\link[DT]{datatable}}.
#' 
#' @param x an \code{R} object which can be coerced to a data frame with
#' non-zero numbers of rows and columns or an \code{htmlwidget} object
#' @param title title for viewer window, defaults to name of \code{x}
#' prefixed by \code{Data:}
#' @param ... additional arguments passed to \code{\link[DT]{datatable}}
#' @param use_viewer logical; if \code{TRUE}, opens in the
#' \code{\link[rstudioapi]{viewer}} if available; otherwise, opens in the
#' default browser
#' 
#' @examples
#' \dontrun{
#' ## data frame-like objects
#' View2(mtcars)
#' view(mtcars)
#' 
#' ## html
#' view(htmlTable::htmlTable(mtcars))
#' 
#' ## widgets
#' view(qtlcharts::iplot(1:5, 1:5))
#' }
#' 
#' @name rawr_view
NULL

#' @rdname rawr_view
#' @export
View2 <- function(x, title) {
  utils::View(x, title)
}

#' @rdname rawr_view
#' @export
view <- function(x, use_viewer = FALSE, ...) {
  htmlFile <- tempfile(fileext = '.html')
  
  if (is.data.frame(x) | is.matrix(x))
    x <- DT::datatable(x, filter = 'top', ...)
  
  if (inherits(x, 'htmlwidget'))
    htmlwidgets::saveWidget(x, htmlFile, selfcontained = TRUE) else
      writeLines(x, con = htmlFile)
  
  if (use_viewer)
    tryCatch(
      rstudioapi::viewer(htmlFile),
      error = function(e) {
        message('Viewer not available - opening in browser.\n',
                'In RStudio, try installing the \'rstudioapi\' package.',
                domain = NA)
        browseURL(htmlFile)
      }) else browseURL(htmlFile)
  
  invisible(NULL)
}

#' Concatenate lists
#' 
#' Combine lists with mixed data types "horizontally."
#' 
#' @param x,y \emph{uniquely-named} lists or nested lists with each pair
#' of non-\code{NULL} elements having identical classes
#' @param how the joining method for matrics and data frames, one of
#' \code{"cbind"} (default) or \code{"rbind"}
#' 
#' @return
#' A lists with all elements from \code{x} and \code{y} joined using
#' \code{\link{cbind}} for matrices, \code{\link{cbind.data.frame}} for
#' data frames, lists for factors, and \code{\link{c}} otherwise.
#' 
#' @seealso
#' \code{\link{nestedMerge}}, \code{\link{modifyList}}; \code{\link{bindx}}
#' 
#' @examples
#' ## boxplot stats created from subsets should be identical to
#' ## the stats generated from a single boxplot
#' 
#' f <- function(x) boxplot(mpg ~ vs, data = x, plot = FALSE)
#' 
#' bp1 <- f(mtcars[mtcars$vs == 0, ])
#' bp2 <- f(mtcars[mtcars$vs == 1, ])
#' bp  <- f(mtcars)
#' 
#' identical(clist(bp1, bp2), bp)
#' # [1] TRUE
#' 
#' 
#' l1 <- list(x = factor(1:5), y = matrix(1:4, 2),
#'            z = head(cars), l = list(zz = 1:5))
#' l2 <- list(z = head(cars), x = factor('a'),
#'            l = list(zz = 6:10))
#' l3 <- list(x = factor(1:5), y = matrix(1),
#'            z = head(cars), l = list(zz = 1:5))
#' 
#' clist(l1, l2, how = 'rbind')
#' 
#' clist(l1, l3, how = 'rbindx')[['y']]
#' # clist(l1, l3, how = 'rbind')[['y']] ## error
#' 
#' clist(l1, l3, how = 'cbindx')[['y']]
#' # clist(l1, l3, how = 'cbind')[['y']] ## error
#' 
#' 
#' ## elements of y not in x are added to result
#' clist(l1, l2, how = 'cbind')
#' clist(l1, list(zzz = data.frame(1), l = list(zz = 5:1)))
#' 
#' @export

clist <- function (x, y, how = c('cbind', 'rbind', 'cbindx', 'rbindx')) {
  if (missing(y))
    return(x)
  
  stopifnot(islist(x), islist(y))
  how <- match.arg(how)
  cbindx.data.frame <- cbindx
  rbindx.data.frame <- rbindx
  
  nn <- names(rapply(c(x, y), names, how = 'list'))
  if (is.null(nn) || any(!nzchar(nn)))
    stop('All non-NULL list elements should have unique names', domain = NA)
  
  nn <- unique(c(names(x), names(y)))
  z  <- setNames(vector('list', length(nn)), nn)
  
  bind <- function(x, y)
    switch(class(x %||% y),
           matrix = match.fun(how),
           data.frame = function(x, y)
             do.call(sprintf('%s.data.frame', how),
                     Filter(Negate(is.null), list(x, y))),
           factor = function(...) unlist(list(...)), c)
  
  for (ii in nn)
    z[[ii]] <- if (islist(x[[ii]]) && islist(y[[ii]]))
      Recall(x[[ii]], y[[ii]]) else
        (bind(x[[ii]], y[[ii]]))(x[[ii]], y[[ii]])
  
  z
}

#' Recursively apply a function to a list
#' 
#' Iterates over a (possibly nested) list and applies a function if a specific
#' \code{\link{class}} is found.
#' 
#' @param l a list
#' @param FUN the function to be applied to each \code{classes} element of
#' \code{l}
#' @param classes a character vector of \code{\link{class}} names or
#' \code{"ANY"} to apply to every non-\code{\link{list}} element of \code{l}
#' @param ... additional arguments passed to \code{FUN}
#' @param check.nested logical; if \code{TRUE}, for nested lists,
#' \code{rapply2} will continue to walk down the list rather than stop at
#' the first list (only if \code{"list" \%in\% classes})
#' 
#' @return
#' A list having the same structure as \code{l} with \code{FUN} applied to
#' all (including nested) elements with a class matching \code{classes}.
#' 
#' @seealso
#' \code{\link{rapply}}
#' 
#' @examples
#' ll <- list(list(list(head(cars), list(head(cars)))), letters[1:4],
#'            factor(1:4), 1:3, head(cars))
#' rapply2(ll, class)
#' rapply2(ll, log, classes = 'data.frame', base = 10)
#' 
#' 
#' ## note that data.frames are not considered lists unless explicit
#' str(rapply2(ll, unlist, classes = 'list'))
#' str(rapply2(ll, unlist, classes = c('list', 'data.frame')))
#' 
#' ## compare
#' str(rapply2(ll, unlist))
#' 
#' 
#' ## remove all elements by name
#' f <- function(x) if (!is.null(names(x))) x[names(x) %ni% 'id'] else x
#' ll <- list(a = list(id = 1, name = 'a-1'),
#'            b = list(id = 1, list(id = 2, name = 'b-2'),
#'                     list(id = 3, name = 'b-3', id = 3)),
#'            c = list(id = 4),
#'            id = 'n/a')
#' 
#' ## compare
#' str(rapply2(ll, f, 'list', check.nested = FALSE))
#' str(rapply2(ll, f, 'list', check.nested = TRUE))
#' 
#' @export


rapply2 <- function(l, FUN, classes = 'ANY', ...,
                    check.nested = 'list' %in% classes) {
  stopifnot(islist(l))
  FUN <- match.fun(FUN)
  
  is.nested <- if (check.nested)
    function(l) any(vapply(l, islist, NA)) else function(l) FALSE
  
  for (ii in seq_along(l)) {
    if (is.null(l[[ii]]))
      next
    l[[ii]] <- if (is.nested(l[[ii]]) ||
                   (islist(l[[ii]]) & ('list' %ni% classes)))
      Recall(l[[ii]], FUN, classes, ..., check.nested = check.nested) else
        if (any(toupper(classes) == 'ANY') || inherits(l[[ii]], classes))
          FUN(l[[ii]], ...) else l[[ii]]
  }
  
  if ('list' %in% classes & !identical(FUN, unlist))
    FUN(l, ...) else l
}

#' Sort matrix
#' 
#' Sort a matrix (or an object that can be coerced) by values in rows or
#' columns.
#' 
#' @param m a matrix, usually an integer matrix
#' @param margin margin to sort by; default is to sort on row values
#' (\code{margin = 1}); sort on column values using \code{margin = 2}
#' @param order vector specifying all unique values of \code{m} in the
#' desired order; if \code{NULL}, the order will be the sorted unique values
#' of \code{m}
#' @param na.last logical; if \code{TRUE}, missing values are put last; if
#' \code{FALSE}, they are put first; see \code{\link{order}}
#' @param index.return logical; if \code{TRUE}, ordering indices are returned
#' 
#' @examples
#' set.seed(1)
#' m <- +!!matrix(rpois(5 * 10, 1), 5)
#' 
#' ## sort columns by decreasing row values
#' sort_matrix(m)
#' 
#' ## return ordering vector
#' o <- sort_matrix(m, index.return = TRUE)
#' stopifnot(
#'   identical(sort_matrix(m), m[, o])
#' )
#' 
#' ## sort rows by decreasing column values
#' sort_matrix(m, 2)
#' 
#' ## sort first by column then by row
#' sort_matrix(m, 2:1)
#' ## equivalent to
#' sort_matrix(sort_matrix(m, 2), 1)
#' 
#' ## compare: default vs sort 0s first followed by 4,3,2,1
#' set.seed(1)
#' m <- matrix(rpois(5 * 10, 1), 5)
#' sort_matrix(m, 2)
#' sort_matrix(m, order = c(0, 4:1))
#' 
#' @export

sort_matrix <- function(m, margin = 1L, order = NULL, na.last = TRUE,
                        index.return = FALSE) {
  if (length(margin) > 1L) {
    m <- Recall(m, margin[1L], order, na.last, index.return)
    return(Recall(m, margin[-1L], order, na.last, index.return))
  }
  
  stopifnot(
    sum(1:2 == margin) == 1L,
    is.logical(na.last) & !is.na(na.last)
  )
  
  m <- as.matrix(m)
  m <- if (margin == 1L)
    t(m) else m
  order <- order %||% sort(unique(c(m)), decreasing = TRUE, na.last = na.last)
  
  stopifnot(
    length(order) == length(unique(c(m)))
  )
  
  dd   <- data.frame(m)
  rownames(dd) <- names(dd) <- NULL
  dd[] <- lapply(dd, factor, levels = order)
  
  o <- do.call('order', c(dd, na.last = na.last))
  if (index.return)
    return(o)
  
  m <- m[o, ]
  if (margin == 1L)
    t(m) else m
}

#' Insert
#' 
#' Insert rows and/or columns into a matrix or elements into a vector at
#' specified indices. \code{insert} and \code{insert_matrix} are similar
#' functions with the latter defined to maintain existing code.
#' 
#' @param x a matrix
#' @param row,col index of row or column to shift right or down,
#' respectively
#' @param repl replacement values, recycled if needed
#' @param rowsep,colsep index of row or column to shift right or down,
#' respectively
#' @param rowrep,colrep row and column replacement values, recycled if needed;
#' note that rows are replaced first followed by columns, so for the opposite
#' behavior, transpose \code{m} before and after using \code{insert_matrix}
#' and swap \code{rowrep} and \code{colrep}; see examples
#' 
#' @examples
#' ## insert at index for vectors
#' insert(1:5, 4)
#' insert(1:5, 4, repl = 10)
#' 
#' m <- col(matrix(0, 5, 5))
#' insert_matrix(m, 2, c(2,4,4,4,6))
#' 
#' ## anticipate number of values needed for replacement(s)
#' # insert_matrix(m, 4, 4:5, colrep = 1:ncol(m)) ## error
#' insert_matrix(m, 4, 4:5, colrep = 1:6)
#' 
#' ## these are _almost_ identical -- rows are inserted first
#' insert_matrix(m, 5, 5, 0, 1) == t(insert_matrix(t(m), 5, 5, 1, 0))
#' 
#' @export

insert <- function(x, row = NULL, col = NULL, repl = NA) {
  if (is.null(dim(x)))
    return(insert_(x, c(row, col), repl))
  
  if (!is.null(row)) {
    n  <- nrow(x)
    rn <- rownames(x)
    idx_na <- insert_(seq.int(n), row, NA)
    idx <- locf(idx_na, fromLast = c(FALSE, TRUE))
    x   <- x[idx, , drop = FALSE]
    if (!is.null(rn))
      rownames(x) <- make.unique(rn[idx])
    x[which(is.na(idx_na)), ] <- repl
  }
  
  if (!is.null(col)) {
    n  <- ncol(x)
    cn <- colnames(x)
    idx_na <- insert_(seq.int(n), col, NA)
    idx <- locf(idx_na, fromLast = c(FALSE, TRUE))
    x   <- x[, idx, drop = FALSE]
    if (!is.null(cn))
      colnames(x) <- make.unique(cn[idx])
    x[, which(is.na(idx_na))] <- repl
  }
  
  x
}

insert_ <- function(x, where, what) {
  if (max(where <- sort(where)) > length(x))
    x <- c(x, rep(NA, max(where) - length(x) - 1L))
  c(x, what)[order(c(seq_along(x), where - 0.5))]
}

#' @rdname insert
#' @export
insert_matrix <- function(x, rowsep = NULL, colsep = NULL,
                          rowrep = NA, colrep = rowrep) {
  if (!is.null(rowsep))
    x <- insert(x, rowsep, repl = rowrep)
  if (!is.null(colsep))
    x <- insert(x, col = colsep, repl = colrep)
  
  x
}

#' Condition handling and recovery
#' 
#' Modification of \code{\link{tryCatch}} which allows recovery from warnings
#' and messages while returning the value from \code{expr}. Note that the
#' return value from errors and interruptions is \code{NULL}.
#' 
#' @param expr an expression
#' @param ... ignored
#' @param simplify logical; if \code{TRUE}, simplifies the returned list to
#' a vector if there were no exceptions
#' 
#' @return
#' If \code{expr} is evaluated with no errors, warnings, messagess, or
#' interruptions, then only the value of \code{expr} is returned unless
#' \code{simplify = FALSE} in which case a list with the return value.
#' 
#' If \code{expr} results in one of the above conditions, then a list with
#' the value of \code{expr} along with an additional element for the
#' condition(s) which can be accessed with \code{$warning}, \code{$error},
#' etc.
#' 
#' @author
#' Martin Morgan, Jan Gorecki, Robert Redd (modifications)
#' 
#' @seealso
#' \code{\link[base]{tryCatch}}; \code{\link[logR]{tryCatch2}};
#' \url{https://stackoverflow.com/q/4948361/2994949};
#' \href{https://github.com/jangorecki/logR}{\pkg{logR} github repo}
#' 
#' @examples
#' ## returns value if no errors, warnings, etc
#' tryCatch2(1)
#' tryCatch2(1, simplify = FALSE)
#' 
#' 
#' tryCatch2(stop('halt at once!'))
#' tryCatch2({warning('warning'); message('message'); 1})
#' 
#' 
#' ## compare
#' tryCatch({warning('this is your warning...'); 1}, warning = function(w) w)
#' tryCatch2({warning('this is your warning...'); 1})
#' 
#' tryCatch({message('calculating...'); 1}, message = function(m) m)
#' tryCatch2({message('calculating...'); 1})
#' 
#' @export

tryCatch2 <- function(expr, ..., simplify = TRUE) {
  E <- W <- M <- I <- NULL
  e.handler <- function(e){
    E <<- e
    NULL
  }
  w.handler <- function(w) {
    W <<- c(W, list(w))
    invokeRestart('muffleWarning')
  }
  m.handler <- function(m) {
    attributes(m$call) <- NULL
    M <<- c(M, list(m))
  }
  i.handler <- function(i) {
    I <<- i
    NULL
  }
  
  res <- suppressMessages(withCallingHandlers(
    tryCatch(expr, error = e.handler, interrupt = i.handler),
    warning = w.handler, message = m.handler)
  )
  
  l <- list(error = E, warning = W, message = M, interrupt = I)
  l <- Filter(Negate(is.null), l)
  
  if (identical(list(), unname(l)) & simplify)
    res else c(list(value = res), l)
}

#' Generate run-length type group id
#' 
#' For a vector, \code{x}, \code{rleid} creates a unique group variable for
#' sequential idenitcal elements of \code{x}.
#' 
#' @param x a vector
#' 
#' @return
#' An integer vector having the same length as \code{x}.
#' 
#' @seealso
#' \code{\link{rle}}; \code{data.table::rleid}
#' 
#' @examples
#' x <- LETTERS[c(1,1,2,1,1,2,3,3)]
#' data.frame(id = x, rleid = rleid(x))
#' 
#' @export

rleid <- function(x) {
  cumsum(c(1L, x[-length(x)] != x[-1L]))
}

#' Drop factor levels
#' 
#' Drop factor levels preserving the range, i.e., unused levels between
#' \code{min_level} and \code{max_level} will not be dropped.
#' 
#' @param x a factor variable
#' @param min_level,max_level min and max unused factor levels to include
#' 
#' @seealso
#' \code{\link{droplevels}}
#' 
#' @examples
#' x <- factor(c('b', 'd'), levels = letters[1:5])
#' droplevels(x)
#' 
#' droplevels2(x)
#' droplevels2(x, min_level = 2, max_level = 5)
#' 
#' droplevels2(factor(c(1, NA)))
#' 
#' @export

droplevels2 <- function(x, min_level = 1L,
                        max_level = max(as.integer(x), na.rm = TRUE)) {
  min_level <- as.integer(min_level)
  max_level <- as.integer(max_level)
  
  stopifnot(
    is.factor(x),
    min_level >= 1L,
    max_level <= nlevels(x)
  )
  
  factor(x, levels = levels(x)[min_level:max_level], ordered = is.ordered(x))
}

#' Combine values
#' 
#' Convenience functions to combine multiple levels of a vector into a new or
#' existing level(s). \code{combine_levels} and \code{combine_regex} are
#' similar in use, but the latter is more useful for unstructured text which
#' can be grouped by regular expressions.
#' 
#' @param x a vector
#' @param levels for \code{combine_levels}, a vector of unique values of
#' \code{x} to combine; to combine values into multiple groups, use a list
#' 
#' for \code{combine_regex} (or \code{combine_labels(..., regex = TRUE)}),
#' a vector of regular expressions; if a list is given, each list element
#' will be collapsed with an "or" statement and treated as single expressions
#' 
#' for values of \code{x} which match none of \code{levels} and if
#' \code{keep.original = FALSE}, a \emph{named} \code{NULL} list element can
#' group these values; otherwise, the smallest unused integer is used; see
#' examples
#' @param labels for \code{combine_levels}, a vector of new labels; if
#' \code{levels} is a vector, \code{labels} should be length 1; if
#' \code{levels} is a list, \code{labels} (need not be a list but) should
#' have one value for each list element of \code{levels}
#' 
#' for \code{combine_regex}, if \code{keep.original = FALSE}, one additional
#' label should be given for values that do not match any of \code{levels}
#' @param regex logical; if \code{TRUE}, \code{levels} is assumed to be
#' regular expressions, and inputs are passed to \code{combine_regex}
#' @param ... additional arguments passed to \code{combine_regex} or further
#' to \code{\link{grep}}
#' @param keep.original logical; for \code{combine_regex}, if \code{FALSE}
#' (default), all values of \code{x} not matching any patterns in
#' \code{levels} are returned as \code{labels[1L]}; if \code{TRUE}, values
#' are returned unchanged
#' 
#' @return
#' \code{combine_levels} will always return a factor. The levels will be
#' in the order of \code{x} plus any new levels (i.e., \code{labels}).
#' 
#' \code{combine_regex} returns an integer vector if character \code{labels}
#' are not given; otherwise, a character vector is returned.
#' 
#' @seealso
#' \code{\link{recoder}}
#' 
#' @examples
#' ## combine numeric, character, or factor
#' x <- rep(1:3, each = 2)
#' combine_levels(x, 1:2, 1)
#' combine_levels(x, list(1:2, 3), c('a', 'b'))
#' 
#' ## use a named list to get the same as above
#' combine_levels(x, list(a = 1:2, b = 3))
#' 
#' 
#' ## characters and factors
#' combine_levels(LETTERS[x], list(x = 'C'))
#' combine_levels(factor(x), 3, 4)
#' combine_levels(factor(x), list(3, 5), c(4, 9))
#' 
#' 
#' ## combine groups by regular expressions
#' x <- letters[1:5]
#' combine_regex(x, 'a')
#' combine_regex(x, c('a', 'b'))
#' combine_regex(x, c('a', 'b|c|e'))
#' 
#' ## character labels return a character vector
#' combine_regex(x, 'a', c('a', 'b'))
#' combine_regex(x, '[a-c]', c('ABC', 'Others'))
#' combine_regex(x, '[a-c]', c('ABC', 'Others'), keep.original = TRUE)
#' 
#' ## levels passed as a list
#' combine_regex(x, list(ABC = c('a', 'b', 'c')))
#' combine_regex(x, list(ABC = '[a-c]', Others = NULL))
#' combine_regex(x, list(ABC = c('a', 'b', 'c'), Others = NULL))
#' 
#' 
#' ## combine_levels(..., regex = TRUE) returns the same as above
#' combine_levels(x, '[a-c]', c('ABC', 'Others'), regex = TRUE)
#' combine_levels(x, '[a-c]', c('ABC', 'Others'), regex = TRUE,
#'                keep.original = TRUE)
#' 
#' @export

combine_levels <- function(x, levels, labels = NULL, regex = FALSE, ...) {
  if (regex) {
    levels <- unlist(sapply(levels, paste0, collapse = '|'))
    labels <- unlist(labels %||% seq.int(length(levels) + 1L))
    
    return(combine_regex(x, levels, labels, ...))
  }
  
  levels <- if (islist(levels))
    levels else list(levels)
  
  ## create unique labels (hopefully) distinct from any of levels
  labels  <- lapply(as.list(labels %||% names(levels)), as.character)
  set.seed(1)
  ul <- lapply(labels, function(x)
    as.character(runif(length(x))))
  labels <- unlist(labels)
  
  stopifnot(length(levels) == length(labels))
  
  xf <- as.factor(x)
  xc <- as.character(xf)
  xl <- ol <- levels(xf)
  
  ## new levels, ie, original minus replaced plus new
  nl <- c(setdiff(ol, c(unlist(levels), labels)), labels)
  nl <- as.character(sort(factor(nl, unique(c(ol, nl)))))

  for (ii in seq_along(levels)) {
    xl[xl %in% unlist(levels[[ii]])] <- unlist(ul[[ii]])
    xc[xc %in% levels[[ii]]]         <- ul[[ii]]
  }
  
  ## convert unique label back to desired
  xc <- c(ol, labels)[match(xc, c(ol, unlist(ul)))]
  xf <- factor(xc, nl, nl, NA, is.ordered(x), NA)
  
  # if (is.numeric(x))
  #   type.convert(as.character(xf)) else xf
  
  ## add order arg?
  # if (!is.null(order))
  #   factor(xf, order) else xf
  
  xf
}

#' @rdname combine_levels
#' @export
combine_regex <- function(x, levels, labels, keep.original = FALSE, ...) {
  if (islist(levels)) {
    stopifnot(
      (ok <- sum(nul <- sapply(levels, is.null))) <= 1
    )
    levels <- c(lapply(levels[!nul], paste, collapse = '|'), levels[nul])
    labels <- names(levels) %||% seq.int(length(levels))
    
    return(
      Recall(x, unlist(levels), labels,
             keep.original = keep.original, ...)
    )
  }

  labels <- if (missing(labels))
    seq.int(length(levels) + 1L)
  else if (length(levels) == length(labels))
    c(labels, length(labels) + 1L)
  else labels
  
  stopifnot(
    is.vector(levels),
    is.vector(labels),
    length(levels) == length(na.omit(unique(levels))),
    keep.original || length(labels) %in% (length(levels) + 0:1)
  )
  
  res <- if (keep.original)
    as.character(x) else rep_len(tail(labels, 1L), length(x))
  for (ii in seq_along(levels))
    res[grep(pattern = levels[ii], x = x, ...)] <- labels[ii]
  
  res
}

#' Rowname tools
#' 
#' @description
#' Convenience functions for working with row names.
#' 
#' \code{rownames_to_column} adds a column to an object using its
#' \code{\link{rownames}}; \code{column_to_rownames} adds rownames to an
#' object using a specified column.
#' 
#' @param data a matrix or data frame
#' @param column the name of the column to create or the column name used
#' to create the row names; if \code{column} is already used, the new name
#' will be passed to \code{\link{make.unique}} before used
#' @param where the location to add \code{column} or the index of the column
#' used to create new row names; for \code{rownames_to_column}, the default
#' is to add the new column first; optional for \code{column_to_rownames}
#' \code{column} will be ignored if \code{where} is given
#' 
#' @seealso
#' \code{\link{insert}}; \code{tibble::rownames}
#' 
#' @examples
#' x <- rownames_to_column(mtcars, where = 5)
#' identical(mtcars, column_to_rownames(x))
#' identical(mtcars, column_to_rownames(x, where = 5))
#' 
#' column_to_rownames(as.matrix(mtcars), 'mpg')
#' 
#' @name rawr_rownames
NULL

#' @rdname rawr_rownames
#' @export
rownames_to_column <- function(data, column = 'rownames', where = 1L) {
  column <- make.unique(c(colnames(data), column))[ncol(data) + 1L]
  data   <- insert(data, col = where, repl = rownames(data))
  
  colnames(data)[where] <- column
  rownames(data) <- NULL
  
  data
}

#' @rdname rawr_rownames
#' @export
column_to_rownames <- function(data, column = 'rownames', where = 1L) {
  where <- if (missing(where))
    which(colnames(data) %in% column) else where
  
  stopifnot(
    length(where) == 1L,
    where %in% seq.int(ncol(data))
  )
  
  rownames(data) <- make.unique(as.character(data[, where]))
  
  data[, -where, drop = FALSE]
}

#' Split at nth pattern
#' 
#' Split a string at the \code{n}th occurrence(s) of a \code{pattern}.
#' 
#' @param text,pattern the text and pattern character strings
#' @param n integer value(s) for splitting at the \code{n}th occurrence of
#' \code{pattern}; \code{NULL} will split at each \code{pattern}
#' @param keep_split logical; if \code{TRUE}, \code{text} will be split at
#' the boundaries of \code{pattern}
#' @param repl a string used internally for place-keeping during splitting;
#' to avoid unexpected results, make sure this string does not occur in
#' \code{text}
#' @param ... additional arguments passed to \code{\link{gregexpr}}
#' 
#' @examples
#' s <- 'this  is a  test string to use   for testing   purposes'
#' split_nth(s, '\\s+')
#' split_nth(s, '\\s+', 3)
#' split_nth(s, '\\s+', c(3, 5))
#' split_nth(s, '\\s+', c(3, 5), keep_split = TRUE)
#' split_nth(s, '\\s{2,}', keep_split = TRUE)
#' split_nth(s, '\\s{2,}', 2:4)
#' 
#' @export

split_nth <- function(text, pattern, n = NULL, keep_split = FALSE,
                      repl = '$$$', ...) {
  stopifnot(
    is.character(text),
    is.character(pattern),
    length(text) == 1L
  )
  
  m <- gregexpr(pattern, text, ...)
  l <- length(attr(m[[1L]], 'match.length'))
  n <- if (is.null(n))
    seq.int(l) else n[n < l]
  
  regmatches(text, m)[[1L]][n] <- if (keep_split)
    paste0(repl, regmatches(text, m)[[1L]][n], repl) else repl
  
  strsplit(text, repl, fixed = TRUE)[[1L]]
}

#' Sorting or ordering of vectors
#' 
#' Sort (or order) a vector or factor (partially) into ascending or descending
#' order keeping \code{NA} values in place.
#' 
#' @param x a numeric, complex, or logical vector
#' @param decreasing logical; if \code{TRUE}, \code{x} is sorted in decreasing
#' order
#' @param index.return logical; if \code{TRUE}, an integer vector with the
#' ordering index is returned
#' @param method the sorting method used, \code{"shell"} (default) or
#' \code{"radix"}; see \code{\link{order}}
#' 
#' @examples
#' set.seed(1)
#' x <- sample(10)
#' x[4:6] <- NA
#' 
#' ## compare
#' sort2(x)
#' sort(x, na.last = NA)
#' sort(x, na.last = TRUE)
#' sort(x, na.last = FALSE)
#' 
#' @export

sort2 <- function(x, decreasing = FALSE, index.return = FALSE,
                  method = c('auto', 'shell', 'radix')) {
  method <- match.arg(method)
  
  o <- if (anyNA(x)) {
    o <- seq_along(x)
    i <- with(rle(!is.na(x)), rep(values, lengths))
    o[i] <- order(x, decreasing = decreasing, na.last = NA, method = method)
    o
  } else {
    order(x, decreasing = decreasing, method = method)
  }
  
  if (index.return)
    o else x[o]
}
