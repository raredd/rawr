### utilities
# psum, rescaler, bind_all, cbindx, rbindx, rbindfill, rbindfill2, rbindlist,
# rbindlist2, interleave, outer2, merge2, locf, roll_fun, regcaptures,
# regcaptures2, cast, melt, View2, view, rapply2, sort_matrix, insert,
# insert_matrix, tryCatch2, rleid, rleid2, droplevels2, combine_levels,
# combine_regex, rownames_to_column, column_to_rownames, split_nth, sort2,
# response, response2, xtable, factor2
# 
# rawr_ops:
# %ni%, %==%, %||%, %sinside%, %winside%, %inside%, %:%
# 
# rawr_ls:
# lss, lsf, lsp
# 
# unexported:
# islist, where_, where, dots, name_or_index, rm_na_dimnames, insert_
###


islist <- function(x) {
  ## is.list(data.frame()); rawr:::islist(data.frame())
  inherits(x, 'list')
}

where_ <- function(x, env = environment(NULL)) {
  if (identical(env, emptyenv()))
    stop(x, ' not found', call. = FALSE)
  
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
#' \code{lss} returns a data frame with each object from the workspace with
#' type, object size, and dimensions for each. \code{lsf} (invisibly) returns
#' the contents of \code{file} after being printed to the console. \code{lsp}
#' returns a vector of character strings.
#' 
#' @seealso
#' \code{\link{ls}}; \code{\link{search}}; \code{\link[rawr2]{rawr_parse}}
#' 
#' @examples
#' ## lss(): use like ls()
#' lss()
#' a <- rnorm(100000)
#' b <- matrix(1, 1000, 100)
#' lss()
#' 
#' ## lsf(): these are equivalent ways to use
#' lsf(rawr)
#' lsf(rawr, 'DESCRIPTION')
#' lsf('rawr', 'des')
#' 
#' ## lsp: see the contents of a package
#' lsp(rawr)
#' 
#' ## return all one- or two-character functions from base package
#' lsp(base, pat = '^.{1,2}$')
#' 
#' ## for "what" options
#' lsp('rawr', '?')
#' 
#' ## library path
#' lsp('rawr', 'path')
#' system.file(package = 'rawr')
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
                decreasing = TRUE, n = Inf) {
  if (!length(ls(envir = as.environment(pos))))
    return(character(0L))
  
  lso <- ls(pos = pos, pattern = pattern, all.names = all.names)
  res <- lapply(lso, function(name) {
    obj <- get(name, pos = pos)
    
    type <- mode(obj)
    type <- ifelse(is.na(type), type, as.character(class(obj))[1L])
    size <- object.size(obj)
    
    data.frame(
      object = name, type = type, size = as.numeric(size),
      sizef = format(size, units = 'auto'),
      nrow = ifelse('function' %in% type, NA, NROW(obj)),
      ncol = ncol(obj) %||% NA, stringsAsFactors = FALSE
    )
  })
  
  res <- do.call('rbind', res)
  res$` ` <- symnum(
    unlist(res$size), corr = FALSE, na = FALSE,
    cutpoints = c(-Inf, 0.001, 0.1, 0.5, 1, Inf) * 1024 ^ 3,
    symbols = c(' ', '.', '*', '**', '***')
  )
  
  if ((mb <- sum(res$size) / (1024 ^ 2)) > 100)
    on.exit(message(sprintf('Total size: %.0f Mb', mb)))
  
  if (!is.null(by))
    res <- res[order(res[, by], decreasing = decreasing), ]
  
  head(res, n)
}

#' @rdname rawr_ls
#' @export
lsf <- function(package, file = 'DESCRIPTION') {
  ## DESCRIPTION, INDEX, NEWS, NAMESPACE
  package <- as.character(substitute(package))
  
  fp <- do.call('lsp', list(package = package, what = 'path'))
  lf <- list.files(fp)
  ff <- lf[grepl(sprintf('(?i)^%s.*$', file), lf, perl = TRUE)]
  
  if (!length(ff)) {
    message(sprintf('File \'%s\' not found', file))
    return(invisible(NULL))
  }
  
  f <- readLines(con <- file(fp <- file.path(fp, ff)))
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

#' Pairwise binary functions
#' 
#' Perform binary operations on vectors, matrices, or data frames.
#' 
#' @param ... numeric vectors, matrices, or data frames with equal dimensions
#' @param na.rm logical; if \code{TRUE}, omits missing values (including
#' \code{\link{NaN}}) from calculations
#' @param FUN a binary function
#' 
#' @return
#' An object similar to input objects after successively combining elements
#' with \code{FUN}.
#' 
#' @seealso
#' \code{\link{pmin}}; \code{\link{pmax}}
#' 
#' @examples
#' x <- c(-1, NA, 4, 15)
#' y <- c(NA, NA, 6, -1)
#' 
#' x + y
#' psum(x, y)
#' psum(x, y, na.rm = TRUE)
#' 
#' x <- matrix(x, 4, 4)
#' y <- matrix(y, 4, 4)
#' x + y
#' psum(x, y)
#' psum(x, y, na.rm = TRUE)
#' 
#' x - y - x
#' pfun(x, y, x, FUN = `-`)
#' pfun(x, y, x, FUN = `-`, na.rm = TRUE)
#' 
#' @export

pfun <- function(..., na.rm = FALSE, FUN = `+`) {
  l <- list(...)
  na <- lapply(l, is.na)
  na <- Reduce(`+`, na) == length(l)
  
  if (na.rm)
    l <- lapply(l, function(x) {x[is.na(x)] <- 0; x})
  
  res <- Reduce(FUN, l)
  res[na] <- NA
  
  res
}

#' @rdname pfun
#' @export
psum <- function(..., na.rm = FALSE) {
  pfun(..., na.rm = na.rm, FUN = `+`)
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
#' rescaler(1:5, to = c(0, 1))
#' rescaler(1:5, to = c(0, 100))
#' rescaler(5, to = c(0, 100), from = c(0, 25))
#' 
#' @export

rescaler <- function (x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  zero_range <- function(x, tol = .Machine$double.eps * 100) {
    if (length(x) == 1L)
      return(TRUE)
    if (length(x) != 2L)
      stop('\'x\' must be length one or two')
    if (any(is.na(x)))
      return(NA)
    if (x[1L] == x[2L])
      return(TRUE)
    if (all(is.infinite(x)))
      return(FALSE)
    m <- min(abs(x))
    if (m == 0)
      return(FALSE)
    abs((x[1L] - x[2L]) / m) < tol
  }
  
  if (zero_range(from) || zero_range(to))
    rep_len(mean(to), length(x))
  else (x - from[1L]) / diff(from) * diff(to) + to[1L]
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
#' \code{\link[rawr2]{clist}}; \pkg{qpcR}
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
  
  res <- do.call('rbind', res)
  colnames(res) <- un
  
  res
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
  
  if (!use.rownames)
    rownames(res) <- NULL
  
  res
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
  
  if (!use.rownames)
    rownames(res) <- NULL
  
  res
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
  l <- rbindlist(l, use.rownames = FALSE, use.names = FALSE)
  
  data <- data[l[, 1L], ]
  data[, column] <- as.character(data[, column])
  data[, column] <- l[, 2L]
  
  if (!use.rownames)
    rownames(data) <- NULL
  
  data
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
  
  which <- match.arg(which, c('rbind', 'cbind', 'rbindx', 'cbindx'))
  
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
  res <- apply(args, seq.int(length(dim(args))), function(x)
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
#' x <- c('', '', 'a', '', 'b', '', '', '', 'c')
#' locf(x)
#' locf(x, c(FALSE, TRUE))
#' locf(x, TRUE)
#' 
#' dd <- data.frame(
#'   V1 = c('Bob', NA, NA, 'Joe', NA, NA),
#'   V2 = c(NA, 1, NA, NA, 2, NA),
#'   stringsAsFactors = FALSE
#' )
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
#' dat <- data.frame(x = c(1, 1, 2, 2, 2, 3, 4, 5, 5, 5),
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
    l[seq.int(n)] <- lapply(seq.int(n), function(x) l[[n]])
  
  sapply(if (fromLast) rev(l) else l, FUN, ...)
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
#' overwritten by simply passing arguments to \code{...} (names must match
#' exactly and no partial matching is allowed or they will be ignored).
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
  rownames(res) <- NULL
  
  res
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
  rownames(res) <- NULL
  
  res
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
#' @param skip.null logical; if \code{TRUE} (default), \code{NULL} elements
#' of \code{l} will be skipped before \code{FUN} can be applied
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
#' 
#' ## use skip.null = FALSE to apply FUN to NULL list elements
#' ll <- list(NULL, 1, list(2, NULL, list(3, NULL)))
#' rapply2(ll, function(x) if (is.null(x)) NA else x)
#' rapply2(ll, function(x) if (is.null(x)) NA else x, skip.null = FALSE)
#' 
#' @export

rapply2 <- function(l, FUN, classes = 'ANY', ...,
                    check.nested = 'list' %in% classes, skip.null = TRUE) {
  stopifnot(islist(l))
  FUN <- match.fun(FUN)
  
  is.nested <- if (check.nested)
    function(l) any(vapply(l, islist, NA)) else function(l) FALSE
  
  for (ii in seq_along(l)) {
    if (skip.null && is.null(l[[ii]]))
      next
    l[[ii]] <- if (is.nested(l[[ii]]) ||
                   (islist(l[[ii]]) & ('list' %ni% classes)))
      Recall(l[[ii]], FUN, classes, ..., check.nested = check.nested,
             skip.null = skip.null)
    else if (any(toupper(classes) == 'ANY') || inherits(l[[ii]], classes))
      FUN(l[[ii]], ...)
    else l[[ii]]
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
  
  stopifnot(length(order) == length(unique(c(m))))
  
  dd <- data.frame(m)
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
#' For a vector, \code{x}, \code{rleid} creates a run-length group identifier
#' for by grouping sequential identical elements of \code{x}.
#' 
#' \code{rleid2} identifies the nth sequence of each unique value of \code{x}.
#' 
#' @param x a logical, character, or numeric vector
#' @param na.rm for \code{rleid2}, logical; if \code{FALSE} (default),
#' \code{NA}s are kept in-place
#' @param ignore.na for \code{rleid2}, logical; if \code{FALSE} (default),
#' \code{NA}s are treated as a distinct group; if \code{TRUE}, \code{NA}s are
#' ignored and will not separate runs, e.g., \code{1, 1, NA, 1} would be
#' treated as a single run
#' 
#' @return
#' An integer vector having the same length as \code{x}.
#' 
#' @seealso
#' \code{\link{rle}}; \code{data.table::rleid}
#' 
#' @examples
#' x <- LETTERS[c(1, 1, 2, 1, 1, 2, 3, NA, 3, 3, 2, 3, 1, NA)]
#' data.frame(
#'   id = x,
#'   rleid = rleid(x),
#'   rleid2 = rleid2(x, ignore.na = FALSE),
#'   rleid2 = rleid2(x, ignore.na = TRUE)
#' )
#' 
#' @export

rleid <- function(x) {
  with(rle(x), rep(seq.int(length(lengths)), lengths))
}

#' @rdname rleid
#' @export
rleid2 <- function(x, na.rm = FALSE, ignore.na = FALSE) {
  res <- rep_len(1L, length(x))
  unx <- unique(x)
  
  if (ignore.na)
    unx <- na.omit(unx)
  
  for (ux in unx) {
    idx <- x %in% c(ux, if (ignore.na) NULL else NA)
    ids <- rleid(idx)
    res[idx] <- rleid(ids[idx])
  }
  
  if (na.rm)
    res else replace(res, is.na(x), NA)
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
#' ## levels between used levels are kept
#' droplevels2(x)
#' 
#' ## keep first level thru last level used
#' droplevels2(x, min_level = 1)
#' 
#' @export

droplevels2 <- function(x, min_level = min(as.integer(x), na.rm = TRUE),
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
#' @param ordered logical; if \code{TRUE}, returns an ordered factor
#' @param regex logical; if \code{TRUE}, \code{levels} is assumed to be
#' regular expressions, and inputs are passed to \code{combine_regex}
#' @param ... additional arguments passed to \code{combine_regex} or further
#' to \code{\link{grep}}
#' @param keep.original deprecated, will be ignored
#' 
#' @return
#' \code{combine_levels} will always return a factor. The levels will be
#' in the order of \code{x} plus any new levels (i.e., \code{labels}).
#' 
#' \code{combine_regex} returns an integer vector if character \code{labels}
#' are not given; otherwise, a character vector is returned.
#' 
#' @seealso
#' \code{\link{factor2}}; \code{\link[rawr2]{recoder}}
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
#' ## use NULL list to combine others
#' combine_levels(x, list(b = 3, others = NULL))
#' 
#' ## levels may be swapped without losing original distinction
#' combine_levels(x, list('3' = 1:2, '1' = 3))
#' combine_levels(x, list('1' = 3, '3' = 1:2))
#' 
#' 
#' ## combine by regular expression
#' x <- letters[1:5]
#' combine_regex(x, 'a')
#' combine_regex(x, c('a', 'b'))
#' combine_regex(x, c('a', 'b|c|e'))
#' 
#' ## character labels return a labeled factor
#' combine_regex(x, 'a', c('a', 'b'))            ## a -> a; else -> b
#' combine_regex(x, '[a-c]', c('ABC', 'Others')) ## a,b,c -> ABC; else Others
#' 
#' ## levels passed as a list
#' combine_regex(x, list(ABC = c('a', 'b', 'c')))       ## d,e are unchanged
#' combine_regex(x, list(ABC = '[a-c]', Others = NULL)) ## d,e are changed
#' 
#' ## combine_levels(..., regex = TRUE) returns the same as above
#' combine_levels(x, '[a-c]', c('ABC', 'Others'), regex = TRUE)
#' 
#' @export

combine_levels <- function(x, levels, labels = NULL, ordered = is.ordered(x),
                           regex = FALSE, ...) {
  if (regex) {
    levels <- unlist(sapply(levels, paste0, collapse = '|'))
    labels <- unlist(labels %||% seq.int(length(levels) + 1L))
    return(combine_regex(x, levels, labels, ordered, ...))
  }
  
  levels <- if (islist(levels))
    levels else list(levels)
  levels <- lapply(levels, function(y)
    if (is.null(y))
      setdiff(x, unlist(levels)) else y)
  
  if (anyDuplicated(dup <- unlist(levels))) {
    dup <- unique(dup[duplicated(dup)])
    warning(sprintf('duplicated levels: %s', toString(dup)))
  }
  
  ## create unique labels to allow for swapping
  labels <- lapply(as.list(labels %||% names(levels)), as.character)
  set.seed(1)
  ul <- lapply(labels, function(x)
    format(runif(length(x)), digits = 22L))
  labels <- unlist(labels)
  
  stopifnot(length(levels) == length(labels))
  
  xf <- as.factor(x)
  xc <- as.character(xf)
  xl <- ol <- levels(xf)
  
  ## new levels, ie, original minus replaced plus new
  nl <- c(setdiff(ol, c(unlist(levels), labels)), labels)
  nl <- as.character(sort(factor(nl, unique(c(ol, nl)))))
  nl <- na.omit(nl)

  for (ii in seq_along(levels)) {
    xl[xl %in% unlist(levels[[ii]])] <- unlist(ul[[ii]])
    xc[xc %in% levels[[ii]]]         <- ul[[ii]]
  }
  
  ## convert unique label back to desired
  xc <- c(ol, labels)[match(xc, c(ol, unlist(ul)))]
  
  factor(xc, nl, nl, ordered = ordered)
}

#' @rdname combine_levels
#' @export
combine_regex <- function(x, levels, labels = NULL, ordered = is.ordered(x),
                          keep.original = TRUE, ...) {
  if (length(labels) == (length(levels) + 1L))
    levels <- c(as.list(levels), list(NULL))
  
  names(levels) <- labels %||% names(levels) %||% seq_along(levels)
  levels <- lapply(levels, function(p) {
    if (!is.null(p))
      unique(grep(paste0(p, collapse = '|'), x, value = TRUE, ...))
  })
  
  combine_levels(x, levels, ordered = ordered, regex = FALSE)
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

#' Response data
#' 
#' @description
#' Get response data (dates, progression, best-so-far, best overall,
#' confirmed, etc.) for vectors of dates and response assessments.
#' 
#' \code{response} performs the work for a single ID--data with multiple
#' IDs should be split before proceeding; see examples. This function also
#' requires that dates are ordered.
#' 
#' \code{response2} is a convenience function that calls \code{response}
#' for each unique \code{id}. Data are ordered and split by \code{id},
#' and the results are aggregated into a single data frame.
#' 
#' @param date a vector of assessment dates for a single ID; identical
#' \code{date}/\code{response} visits will be removed with a warning
#' @param response a factor variable of responses for each \code{date} with
#' levels ordered from best response to worst (e.g.,
#' \code{factor(., levels = c('CR', 'PR', 'SD', 'PD'))}
#' @param include integer vector corresponding to the levels of \code{response}
#' or a single regular expression to match levels of \code{response}; defines
#' assessments that will be treated as a meaningful response, e.g., if
#' \code{include} does not match stable disease and/or minimal response,
#' these assessments will be ignored in calculating best-so-far and best
#' overall responses
#' @param default (optional) the default response if no assessments have been
#' confirmed, usually stable disease; must be a level of \code{response}
#' @param no_confirm responses that do not require confirmation assessments,
#' usually stable disease (default); must match a level of \code{response}
#' @param progression a character string or regular expression to identify
#' progression events in \code{response}; must match a level of \code{response}
#' @param n_confirm to confirm a response, the number of subsequent responses
#' equal to or better than previous; e.g., if \code{n_confirm = 1} (default),
#' to confirm a response, the next assessment must be at least as good as the
#' current; note that this only affects \code{.$confirmed} and
#' \code{.$bsf_confirmed} in the return object; if \code{n_confirm = 2}, to
#' confirm a response, the next two assessments must be at least as good, etc
#' @param n_prog similar to \code{n_confirm} but for progression; if
#' \code{nprog = 1}, then a progression must be followed by at least one
#' progression to confirm; note that this only affects \code{.$confirmed} and
#' \code{.$bsf_confirmed} in the return object
#' @param strict logical; if \code{TRUE}, only the first uninterrupted
#' sequence of confirmed responses will be evaluated for best response; e.g.,
#' an unconfirmed CR between two confirmed PR sequences will result in the
#' later PR sequence being ignored
#' @param dr (optional) difference in level required to confirm responses; if
#' \code{dr = 0} (default), the next response must be equal to or better to
#' confirm; if \code{dr = -1}, the next response must be at least one level
#' better; \code{dr = Inf} will be equivalent to unconfirmed responses since
#' any subsequent response can confirm the one previous
#' @param dp (optional) difference in level required to show progression; if
#' \code{dp = NULL} (default), only responses matching \code{progression}
#' pattern are progression; if \code{dp = 0}, any worse response will be
#' considered progression (e.g., CR followed by PR); \code{dp = 1},
#' progression is defined as a greater than one level drop in response (e.g.,
#' CR to PR is not progression but CR to SD or MR is progression)
#' 
#' @return
#' A list with the following elements:
#' \item{$unconfirmed}{a \code{1 x 10} data frame with dates of first and last
#' response, first and last best response, the response for each, date last
#' free from progression, and date of progression}
#' \item{$confirmed}{similar to \code{$unconfirmed} but only for responses
#' that have been confirmed}
#' \item{$bsf_unconfirmed}{an \code{n x 2} data frame with the best-so-far
#' responses matching \code{include} with corresponding dates}
#' \item{$bsf_confirmed}{similar to \code{$bsf_unconfirmed} but only for
#' responses that have been confirmed}
#' 
#' @examples
#' set.seed(1)
#' rsp <- c('CR', 'PR', 'SD', 'PD')
#' rsp <- c('sCR', 'CR', 'VGPR', 'PR', 'MR', 'SD', 'PD')
#' dat <- data.frame(
#'   id = rep(1:5, each = 10),
#'   date = Sys.Date() + 1:10,
#'   response = factor(
#'     # rsp[sample(seq_along(rsp), 50, TRUE, c(0.2, 0.5, 0.2, 0.1))], rsp
#'     rsp[sample(seq_along(rsp), 50, TRUE)], rsp
#'   )
#' )
#' 
#' sp <- split(dat[, -1L], dat$id)
#' ii <- 2
#' response(sp[[ii]]$date, sp[[ii]]$response)
#' 
#' unconf <- do.call(
#'   'rbind',
#'   lapply(sp, function(x) response(x$date, x$response)$unconfirmed)
#' )
#' conf <- do.call(
#'   'rbind',
#'   lapply(sp, function(x) response(x$date, x$response, default = 'SD')$confirmed)
#' )
#' rownames(unconf) <- rownames(conf) <- NULL
#' 
#' data.frame(
#'   id = names(sp),
#'   unconf = unconf$response_best_first,
#'   conf = conf$response_best_first
#' )
#' ## compare
#' do.call('cbind', sp)
#' 
#' 
#' ## or simply with response2
#' unconf2 <- response2(dat$id, dat$date, dat$response)
#' conf2 <- response2(dat$id, dat$date, dat$response, type = 'confirmed')
#' 
#' identical(unconf, unconf2[-1])
#' identical(conf, conf2[-1])
#' 
#' @export

response <- function(date, response, include = '(resp|stable)|([cpm]r|sd)$',
                     default = NA, no_confirm = 'stable|sd',
                     progression = 'prog|pd|relapse', n_confirm = 1L,
                     n_prog = n_confirm, strict = FALSE, dr = 0, dp = NULL) {
  stopifnot(
    is.factor(response),
    !anyNA(date),
    all(diff(date) >= 0)
  )
  
  best_seq <- function(x) {
    ## get first continuous sequence of best response
    x[x > min(x)] <- NA
    rl <- rle(x)
    rl$values[duplicated(rl$values)] <- NA
    res <- inverse.rle(rl)
    replace(res, is.na(res), Inf)
  }
  
  confirm <- function(x, n = 2L, d = 0, dni = NA) {
    ## find confirmed responses requiring n in a row of at least d
    ## with dni not requiring a confirmation
    ##   d = 0 (default) as-good or better response needed
    ##   d = -1 better response (by at least one level) needed
    x <- as.integer(x)
    n <- as.integer(n)
    
    if (length(x) < n) {
      res <- rep_len(FALSE, length(x))
      return(replace(res, x %in% dni, TRUE))
    }
    if (n < 2L)
      return(rep_len(TRUE, length(x)))
    
    res <- sapply(seq.int(n), function(ii) {
      ii <- ii - 1L
      if (ii == 0L)
        x else c(tail(x, -ii), rep_len(Inf, ii))
    })
    res <- apply(res, 1L, function(x) all(diff(x) <= d))
    
    if (is.na(dni))
      res else replace(res, x %in% dni, TRUE)
  }
  
  fix_levels <- function(data, levels = lvls, ordered = ord, def = default) {
    r <- grep('^resp',  names(data))
    
    data[r] <- lapply(data[r], function(x) {
      x <- if (is.factor(x))
        x else factor(x, seq_along(levels), levels, ordered = ordered)
      x[is.na(x)] <- def
      x
    })
        
    data
  }
  
  data <- data.frame(date = date, response = response)[order(date), ]
  data <- within(data, {
    responsei <- as.integer(response)
  })
  
  if (any(idx <- duplicated(data))) {
    warning('removing duplicated date-response observations')
    data <- data[!idx, ]
  }
  
  lvls <- levels(response)
  ord  <- is.ordered(response)
  include <- if (is.numeric(include))
    lvls[include]
  else grep(include[1L], lvls, value = TRUE, ignore.case = TRUE)
  
  default <- grep(default[1L], lvls, value = TRUE, ignore.case = TRUE)[1L]
  
  no_confirmi <- grep(no_confirm[1L], lvls, ignore.case = TRUE)
  no_confirm <- lvls[no_confirm]
  
  na  <- as.Date(NA)
  
  
  ## unconfirmed pd
  pd <- grep(progression, data$response, ignore.case = TRUE)
  if (!is.null(dp))
    pd <- sort(c(pd, which(diff(data$responsei) > dp) + 1L))
  dt_prog <- if (length(pd))
    data$date[min(pd)] else na
  
  ## confirmed pd
  pd_conf <- grep(progression, data$response, ignore.case = TRUE)
  if (!is.null(dp))
    pd_conf <- sort(c(pd_conf, which(diff(data$responsei) > dp) + 1L))
  pd_conf <- pd_conf[which(diff(pd_conf) == 1L)[1L]]
  dt_prog_conf <- if (!is.na(pd_conf))
    data$date[pd_conf] else na
  
  
  ## select rows up until first progression
  data_conf <- if (!is.na(pd_conf))
    data[seq(1L, pd_conf - 1L), ] else data
  dt_progfree_conf <- max(data_conf$date)
  
  data <- if (length(pd))
    data[seq(1L, min(pd) - 1L), ] else data
  dt_progfree <- max(data$date)
  
  
  bsf <- data.frame(dt_bsf = na, response_bsf = NA)
  rsp_na <- data.frame(
    dt_first = na, response_first = NA, dt_last = na, response_last = NA,
    dt_best_first = na, response_best_first = NA, dt_best_last = na,
    response_best_last = NA, dt_lastprogfree = dt_progfree, dt_prog = dt_prog
  )
  
  ## make sure response levels are correct
  rsp_na <- fix_levels(rsp_na)
  bsf    <- fix_levels(bsf)
  
  if (!any(data$response %in% c(include, no_confirm)) ||
      length(pd) && min(pd) == 1L)
    return(
      list(
        unconfirmed = rsp_na,
        confirmed = within(rsp_na, dt_prog <- dt_prog_conf),
        bsf_unconfirmed = bsf,
        bsf_confirmed = bsf
      )
    )
  
  ## confirmed
  data_conf <- within(data_conf, {
    bsfi <- locf(cummin_na(responsei))
    bsf  <- factor(bsfi, seq_along(lvls), lvls)
    
    confirm <- responsei
    # confirm <- c(confirm[-1L] <= confirm[-length(confirm)], FALSE)
    confirm <- confirm(responsei, n_confirm + 1L, dr, no_confirmi)
    confirm[grepl(progression, response)] <- NA
    confirm_best <- replace(confirm, responsei > min(bsfi), FALSE)
    
    confirm_date <- c(date[-1L], NA)
    confirm_date[!confirm %in% TRUE] <- NA
    
    responsei_confirm <- ifelse(confirm %in% TRUE, responsei, NA)
    bsfi_confirm <- locf(cummin_na(responsei_confirm))
    bsf_confirm <- factor(bsfi_confirm, seq_along(lvls), lvls)
    
    best_confirm <- if (all(is.na(responsei_confirm[confirm])))
      responsei_confirm[confirm]
    else lvls[min(responsei_confirm[confirm],
                  na.rm = sum(sort(responsei_confirm[confirm])) > 0)]
  })
  
  ## select only rows that meet minimal response criteria or require no conf
  if (length(include))
    dd <- data_conf[data_conf$response %in% c(include, no_confirm), ]
  
  ## first/last response of any type, first/last of best response
  bes <- dd$responsei
  bes <- best_seq(bes)
  
  idx <- c(
    first = 1L, last = nrow(dd),
    best_first = max.col(-t(bes), 'first'),
    best_last  = max.col(-t(bes), 'last')
  )
  
  rsp <- lapply(seq_along(idx), function(ii) {
    x <- idx[ii]
    setNames(
      data.frame(date = dd$date[x], response = dd$response[x]),
      paste0(c('dt_', 'response_'), names(idx)[ii])
    )
  })
  rsp <- do.call('cbind', rsp)
  
  ## best so far response
  bsf <- data.frame(dt_bsf = locf(dd$date), response_bsf = dd$bsf)
  
  ## repeat but only for confirmed responses
  id <- rleid(dd$confirm_best)
  dd <- if (strict)
    dd[id %in% id[dd$confirm_best %in% TRUE][1L], ]
  else dd[dd$confirm %in% TRUE, ]
  
  if (nrow(dd) == 0L) {
    bsf_na <- data.frame(dt_bsf = na, response_bsf = NA)
    rsp$dt_prog <- rsp_na$dt_prog
    rsp$dt_lastprogfree <- rsp_na$dt_lastprogfree <- dt_progfree
    
    rsp    <- fix_levels(rsp)
    bsf    <- fix_levels(bsf)
    bsf_na <- fix_levels(bsf_na)
    
    return(
      list(
        unconfirmed = rsp, confirmed = within(rsp_na, dt_prog <- dt_prog_conf),
        bsf_unconfirmed = bsf, bsf_confirmed = bsf_na
      )
    )
  }
  
  bes <- dd$responsei
  bes <- best_seq(bes)
  
  idx_confirm <- c(
    first = 1L, last = nrow(dd),
    best_first = max.col(-t(bes), 'first'),
    best_last  = max.col(-t(bes), 'last')
  )
  
  rsp_confirm <- lapply(seq_along(idx_confirm), function(ii) {
    x <- idx_confirm[ii]
    setNames(
      data.frame(date = dd$date[x], response = dd$response[x]),
      paste0(c('dt_', 'response_'), names(idx_confirm)[ii])
    )
  })
  rsp_confirm <- do.call('cbind', rsp_confirm)
  
  
  ## first progression if any
  rsp$dt_prog <- dt_prog
  rsp_confirm$dt_prog <- dt_prog_conf
  rsp$dt_lastprogfree <- dt_progfree
  rsp_confirm$dt_lastprogfree <- dt_progfree_conf
  
  
  ## best so far response
  bsf_confirm <- data.frame(
    dt_bsf = locf(dd$date),
    response_bsf = dd$bsf_confirm
  )
  
  rsp <- fix_levels(rsp)
  bsf <- fix_levels(bsf)
  rsp_confirm <- fix_levels(rsp_confirm)
  bsf_confirm <- fix_levels(bsf_confirm)
  
  list(
    unconfirmed = rsp,
    confirmed = rsp_confirm,
    bsf_unconfirmed = bsf,
    bsf_confirmed = bsf_confirm
  )
}

#' @param id for \code{response2}, a vector of IDs
#' @param ... additional arguments passed to \code{response}
#' @param type see "Value" section
#' @rdname response
#' @export
response2 <- function(id, date, response, ...,
                      type = c('unconfirmed', 'confirmed',
                               'bsf_unconfirmed', 'bsf_confirmed')) {
  type <- match.arg(type)
  data <- data.frame(id, date, response)[order(id, date), ]
  data <- split(data, data$id)
  
  resp <- lapply(data, function(x)
    cbind(id = x$id[1L], response(x$date, x$response, ...)[[type]]))
  
  res <- do.call('rbind', resp)
  rownames(res) <- NULL
  
  res
}

#' Cross table
#' 
#' Create a contingency table with totals, percentages, and statistical test.
#' 
#' @param x,by row and column variables, respectively; should be factor-like
#' and will be coerced; missing values in \code{by} will be removed with the
#' corresponding values in \code{x}
#' @param digits for percentages, number of digits past the decimal to keep
#' @param total logical; if \code{TRUE}, the row totals will be added as a
#' separate column
#' @param pct.sign logical; if \code{FALSE} (default), no percentage signs
#' will be shown
#' @param test logical; if \code{TRUE}, a test and p-value will be added as
#' a separate column; see \code{rawr:::guess_test}
#' 
#' alternatively, a user-defined test function which takes two arguments,
#' \code{x} and \code{by}, and returns a numeric p-value with an optional
#' attribute, \code{"name"}, which will be used as a test label; see
#' examples
#' @param ... additional arguments passed to \code{\link[Gmisc]{getDescriptionStatsBy}}
#' 
#' @seealso
#' \code{\link[Gmisc]{getDescriptionStatsBy}}; \code{\link{tabler_stat2}};
#' \code{rawr:::guess_test}
#' 
#' @examples
#' x <- mtcars$gear
#' y <- mtcars$cyl
#' z <- mtcars$vs
#' 
#' table(x, y)
#' xtable(x, y)
#' 
#' xtable(ordered(x), z)
#' xtable(ordered(x), y)
#' xtable(ordered(x), ordered(y))
#' 
#' 
#' ## user-defined test function
#' test <- function(x, by) {
#'   structure(chisq.test(table(x, by))$p.value, name = '<i>p-value</i>')
#' }
#' res <- xtable(x, y, test = test)
#' res
#' 
#' names(dimnames(res)) <- c('Gears', 'Cylinders')
#' htmlTable::htmlTable(
#'   res, n.cgroup = c(1, 3, 1), cgroup = c('', 'Cylinders', '')
#' )
#' 
#' @export

xtable <- function(x, by, digits = 0L, total = TRUE, pct.sign = FALSE,
                   test = TRUE, ...) {
  xn <- deparse(substitute(x))
  bn <- deparse(substitute(by))
  
  x <- as.factor(x)
  by <- as.factor(by)
  
  res <- Gmisc::getDescriptionStatsBy(
    x, by, digits = digits, add_total_col = total, ...,
    percentage_sign = pct.sign, show_all_values = TRUE
  )
  class(res) <- attr(res, 'label') <- NULL
  
  if (!isFALSE(test)) {
    test <- if (isTRUE(test))
      guess_test(x, by) else match.fun(test)(x, by)
    res <- cbind(res, c(pvalr(test), rep_len('', nrow(res) - 1L)))
    colnames(res)[ncol(res)] <- attr(test, 'name') %||% 'test'
  }
  names(dimnames(res)) <- c(xn, bn)
  
  res
}

#' Combine values to factor levels
#' 
#' @description
#' Convenience function to combine level(s) of a vector into a new or existing
#' factor level(s) by unique value and/or regular expression.
#' 
#' Requires \code{R >= 3.5.0} for duplicated labels in \code{\link{factor}}s.
#' 
#' @param x a vector of data, usually taking a small number of distinct values
#' @param levels a \emph{named} list of unique values or regular expressions
#' to group elements of \code{x}
#' 
#' a \code{NULL} group will collapse all non matches to a single category;
#' however, if there is no \code{NULL} group, non matches will be unchanged
#' 
#' if \code{regex = TRUE} or flagged with \code{(?r)}, matching elements of
#' \code{x} will be combined
#' @param exclude,ordered passed to \code{\link{factor}}
#' @param regex logical; if \code{TRUE}, \code{levels} is treated as a list
#' of regular expressions or otherwise matched exactly (unless \code{levels}
#' is flagged with \code{(?r)})
#' @param ... additional arguments passed to \code{\link{grep}} when using
#' regular expressions
#' 
#' @seealso
#' \code{\link{combine_levels}}; \code{\link{combine_regex}}
#' 
#' @examples
#' ## combine numeric, character, or factor
#' x <- rep(1:5, each = 2)
#' factor2(x, list('1' = 1:2))
#' factor2(x, list(a = 1:2, b = 3))    ## all non matches unchanged
#' factor2(x, list(a = 1:2, b = NULL)) ## all non matches grouped to b
#' 
#' ## levels may be swapped without losing original distinction
#' factor2(x, list('3' = 1:2, '1' = 3))
#' factor2(x, list('1' = 3, '3' = 1:2))
#' 
#' ## combine by regular expression
#' x <- letters[1:5]
#' factor2(x, list(a = c('a', 'b')), regex = TRUE)
#' factor2(x, list(a = c('a|b')), regex = TRUE) ## same
#' factor2(x, list(a = 'a', b = 'b|c|e'), regex = TRUE)
#' 
#' ## use regex only when flagged with (?r)
#' ## note: must use regex = FALSE, otherwise all are treated as regex
#' x <- c('a', 'aaa', 'abc', 'def')
#' factor2(x, list(a = 'a', b = '(?r)a.', d = '(?r)d'))
#' factor2(x, list(a = 'a', b = '(?r)a.', d = 'd'))
#' 
#' @export

factor2 <- function(x = character(), levels = NULL, exclude = NA,
                    ordered = is.ordered(x), regex = FALSE, ...) {
  if (is.null(levels))
    return(as.factor(x))
  
  stopifnot(
    islist(levels),
    !is.null(names(levels))
  )
  
  levels <- lapply(levels, function(p) {
    if (!is.null(p) && (regex | any(grepl('(?r)', p, fixed = TRUE)))) {
      p <- gsub('(?r)', '', p, fixed = TRUE)
      unique(grep(paste0(p, collapse = '|'), x, value = TRUE, ...))
    } else p
  })
  
  ## if a NULL level is present, group all others into one category
  levels <- lapply(levels, function(y)
    if (is.null(y))
      setdiff(x, unlist(levels)) else y)
  
  if (anyDuplicated(dup <- unlist(levels))) {
    dup <- unique(dup[duplicated(dup)])
    warning(sprintf('duplicated levels: %s', toString(dup)))
  }
  
  key <- stack(levels)
  key$ind <- as.character(key$ind)
  ext <- setdiff(x, key$values)
  key <- rbind(key, data.frame(values = ext, ind = ext))
  
  factor(x, key$values, key$ind, exclude = exclude, ordered = ordered)
}
