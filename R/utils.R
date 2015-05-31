### utilities
# rawrops: %ni%, %==%, %||%, %inside%, %:%
# misc: lss, lsp, lsf, ht, progress, recoder, psum, ident, search_df,
# search_hist, fapply, rescaler, try_require, list2file, Restart, clc, clear,
# helpExtract, Round, bind_all, cbindx, rbindx, rbindfill, interleave, outer2,
# merge2, locf, roll_fun, round_to, updateR, read_clip, fcols, classMethods,
# regcaptures, path_extract, fname, file_name, file_ext, cast, melt,
# install_temp, nestedMerge
###

#' rawr operators
#' 
#' Some useful binary operators.
#' 
#' \code{\%ni\%} is the negation of \code{\link[base]{\%in\%}}.
#' 
#' \code{\%inside\%} returns a logical vector indicating if \code{x} is inside
#' the \code{interval} (inclusive).
#' 
#'
#' \code{\%=\%} is an operator combining the qualities of \code{\link{==}} and
#' \code{\link{\%in\%}} to compare vectors in a pairwise manner which may
#' include \code{\link{NA}}s.
#' 
#' \code{\%||\%} is useful for a function, \code{f}, that may return a value
#' or \code{NULL}, but if \code{NULL} is the result of \code{f}, it is
#' desirable to return some other default value without errors.
#' 
#' \code{\%:\%} is useful for obtaining a range of \code{colnames} or 
#' \code{names} by literal character strings rather than by index.
#' 
#' @param a,b raw, logical, "number-like" vectors or objects
#' @param x vector or \code{NULL}; the values to be matched
#' @param table vector or \code{NULL}; the values to be matched against
#' @param interval numeric vector of length two representing the interval
#' @param object a \emph{named} vector or list, a matrix or data frame
#' @param range a numeric or character vector of length two with the indices
#' or names from \code{object}, generally of the structure \code{c(from, to)}
#' 
#' @aliases oror %||% notin %ni% inside %inside% %==%
#' @seealso \code{\link{==}}, \code{\link{\%in\%}}, \code{\link{||}}
#' @name rawrops
#' 
#' @examples
#' \dontrun{
#' 1:5 %ni% 3:5
#' 
#' c(0,4) %inside% c(0, 4)
#' -5:5 %inside% c(0,5)
#' -5:5 %inside% c(5,0)
#' 
#' a <- c(1, NA, 2)
#' b <- c(2, NA, 1)
#' ## not desired
#' a == b     # FALSE NA   FALSE
#' a %in% b   # TRUE  TRUE TRUE
#' ## desired results
#' a %==% b   # FALSE TRUE FALSE
#' 
#' f <- function(x0 = TRUE) NULL || x0
#' f() # error
#' f <- function(x0 = TRUE) NULL %||% x0
#' f() # TRUE
#' 
#' ## these are equivalent
#' mtcars %:% c('hp','vs')
#' mtcars %:% c(4, 8)
#' names(mtcars[, 4:8])
#' }
#' 
#' @export

#' @rdname rawrops
#' @export
`%ni%` <- function(x, table) !(match(x, table, nomatch = 0) > 0)

#' @rdname rawrops
#' @export
`%inside%` <- function(x, interval) {
  interval <- sort(interval)
  x >= interval[1] & x <= interval[2]
}

#' @rdname rawrops
#' @export
`%==%` <- function(a, b)
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)

#' @rdname rawrops
#' @export
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @rdname rawrops
#' @export
`%:%` <- function(object, range) {
  FUN <- if (is.matrix(object)) colnames else names
  wh <- if (is.numeric(range)) range else which(FUN(object) %in% range)
  FUN(object)[seq(wh[1], wh[2])]
}

#' Improved list of objects
#' 
#' Provides more details of objects in workspace.
#' 
#' @param pos argument specifying the environment as a position in search list
#' @param pattern optional \code{\link{regex}}; only names matching 
#' \code{pattern} are returned; \code{\link{glob2rx}} can be used to convert
#' wildcard patterns to regular expressions
#' @param by variable to order output ('type', 'size' (default), 'sizef', 
#' 'nrow', or 'ncol')
#' @param decreasing logical; if \code{TRUE}, displays output in decreasing 
#' order
#' @param all.names logical; if \code{TRUE}, all object names are returned; if
#' \code{FALSE}, names which begin with a \code{.} are omitted
#' @param n number of objects to displace if \code{head} is \code{TRUE}
#' 
#' @seealso \code{\link{ls}}, \code{\link{ls.str}}, \code{\link{objects}}
#' 
#' @examples
#' lss()
#' a <- rnorm(100000)
#' b <- matrix(1, 1000, 100)
#' lss()
#' 
#' @export

lss <- function (pos = 1, pattern, by = NULL, all.names = FALSE,
                 decreasing = TRUE, n = 15) {
  
  if (length(ls(envir = as.environment(pos))) < 1L)
    stop(return(character(0)))
  
  napply <- function(names, fn) 
    sapply(names, function(x) fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern, all.names = all.names)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x)
    capture.output(print(object.size(x), units = 'auto')))
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[ , 1] & (obj.type != 'function')
  obj.dim[vec, 1] <- napply(names, length)[vec]
  
  out <- setNames(data.frame(obj.type, obj.size, obj.prettysize, obj.dim),
                  c('type', 'size', 'sizef', 'nrow', 'ncol'))
  if (!is.null(by))
    out <- out[order(out[[by]], decreasing = decreasing), ]
  head(out, n)
}

#' List package
#' 
#' List all exported and/or non exported objects in a package.
#' 
#' This is a helper/wrapper function to list exported (\code{?'::'}) and
#' non exported (\code{?':::'}) functions (and other features from a package's 
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
#' @param package package name, as \code{\link{name}} or literal character 
#' string
#' @param what what to get; \code{'all'} is default which returns all exported
#' and non exported functions in \code{package}; see details for more
#' @param pattern text pattern or regular expression passed to 
#' \code{\link{grep}} to filter results
#' 
#' @seealso \code{\link{lsf}}, \code{\link{ls}}, \code{\link{search}}
#' 
#' @examples
#' ## see the contents of this package
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
#' lsp('rawr', NULL)
#' 
#' ## data sets
#' lsp('ggplot2', 'lazydata')
#' 
#' @export

lsp <- function(package, what, pattern) {
  
  if (!is.character(substitute(package)))
    package <- deparse(substitute(package))
  ns <- asNamespace(package)
  if (missing(what))
    what <- 'all'
  if (missing(pattern))
    pattern <- '.*'
  
  ## base package does not have NAMESPACE
  if (isBaseNamespace(ns)) {
    res <- ls(.BaseNamespaceEnv, all.names = TRUE)
    return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
  } else {
    ## for non base packages
    if (exists('.__NAMESPACE__.', envir = ns, inherits = FALSE)) {
      wh <- get('.__NAMESPACE__.', envir = asNamespace(package, base.OK = FALSE),
                inherits = FALSE)
      if ('?' %in% what) 
        return(ls(wh))
      if (!is.null(what) && !any(what %in% c('all', ls(wh))))
        stop('what is invalid; see ?rawr::lsp \'details\'')
      res <- sapply(ls(wh), function(x) getNamespaceInfo(ns, x))
      res <- rapply(res, ls, classes = 'environment', 
                    how = 'replace', all.names = TRUE)
      if (is.null(what)) 
        return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
      if (what %in% 'all') {
        res <- ls(getNamespace(package), all.names = TRUE)
        return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
      }
      if (any(what %in% ls(wh))) {
        res <- res[what]
        return(res[[grep(pattern, res, perl = TRUE, ignore.case = TRUE)]])
      }
    } else stop(sprintf('no NAMESPACE file found for package %s', package))
  }
}

#' List package files
#' 
#' Prints package files (\code{DESCRIPTION}, \code{NEWS}, \code{INDEX},
#' \code{NAMESPACE}, etc.) to console.
#' 
#' @param package package name, as a \code{\link{name}} or literal character
#' string
#' @param file file to return as a character string; usual options are
#' \code{'DESCRIPTION'}, \code{'NEWS'}, \code{'INDEX'}, \code{'NAMESPACE'};
#' partial matching is supported and case is ignored
#' 
#' @seealso \code{\link{lsp}}
#' 
#' @examples
#' lsf(rawr, 'DESCRIPTION')
#' lsf('rawr', 'des')
#' 
#' @export

lsf <- function(package, file = 'DESCRIPTION') {
  ## DESCRIPTION, INDEX, NEWS, NAMESPACE
  package <- as.character(substitute(package))
  p <- do.call('lsp', list(package = package, what = 'path'))
  lf <- list.files(p)
  ff <- lf[grepl(sprintf('(?i)^%s.*$', file), lf, perl = TRUE)]
  if (!length(ff)) {
    message(sprintf('File \'%s\' not found', file))
    return(invisible())
  }
  f <- readLines(con <- file(fp <- file.path(p, ff)))
  on.exit(close(con))
  message(sprintf('Showing file:\n%s\n', fp))
  cat(f, sep = '\n')
}

#' head/tail
#' 
#' @param x an object
#' @param ... other parameters, such as \code{n}, passed to \code{\link{head}} 
#' or other methods
#' @param sep separator 
#' @export

ht <- function(x, ..., sep = NULL) rbind(head(x, ...), sep, tail(x, ...))

#' Progress function
#' 
#' Displays the percent (or iterations) completed during some loop.
#' 
#' @param value numeric; i-th iteration or percent completed (values 0-100)
#' @param max.value numeric; n-th iteration; if missing, will assume percent
#' completion is desired
#' @param textbar logical; if \code{TRUE}, uses text progress bar which will
#' span across the console width; see \code{\link{options}}
#' 
#' @examples
#' \dontrun{
#' iterations <- 77
#' ## percent completed:
#' for (ii in 1:iterations) {
#'    progress(ii / iterations * 100)
#'    Sys.sleep(.01)
#' }
#' 
#' ## iterations completed
#' for (ii in 1:iterations) {
#'    progress(ii, iterations)
#'    Sys.sleep(.01)
#' }
#'
#' ## text progress bar
#' for (ii in 1:iterations) {
#'    progress(ii, iterations, textbar = TRUE)
#'    Sys.sleep(.01)
#' }
#' }
#' 
#' @export

progress <- function (value, max.value, textbar = FALSE) {
  oo <- options()
  on.exit(options(oo))
  options(scipen = 10)
  if (!is.numeric(value)) 
    stop("\'value\' must be numeric")
  if (missing(max.value)) {
    max.value <- 100
    percent <- TRUE
  } else percent <- FALSE
  
  f <- function(...) paste0(..., collapse = '')
  erase.only <- value > max.value
  max.value <- as.character(round(max.value))
  l <- nchar(max.value)
  # value <- formatC(round(value), width = l, format = 'd')
  # max.value <- formatC(max.value, width = l, format = 'd')
  
  if (textbar) {
    # m <- getOption('width')
    # r <- floor(as.numeric(value) / as.numeric(max.value) * m)
    # backspaces <- f(rep('\b', m * 2))
    #
    # if (erase.only) message <- ''
    # else {
    #   message <- f('|', f(rep('=', max(0, r - 1))),
    #                f(rep(' ', max(0, m - r))), '|')
    #   cat(backspaces, message, sep = '')
    # }
    m <- getOption('width') - 5
    pct <- as.numeric(value) / as.numeric(max.value)
    r <- floor(pct * m)
    backspaces <- f(rep('\b', m * 2))
    if (erase.only) message <- ''
    else {
      message <- f('|', f(rep('=', max(0, r - 1))), 
                   f(rep(' ', max(0, m - r))), '|')
      cat(backspaces, message, sprintf('  %s%%', round(pct * 100)), sep = '')
    }
  } else {
    if (percent) {
      backspaces <- f(rep('\b', l + 14))
      if (erase.only) message <- ''
      else message <- paste0('Progress: ', value, '%  ')
      cat(backspaces, message, sep = '')
    } else {
      backspaces <- f(rep('\b', 2 * l + 17))
      if (erase.only) message <- ''
      else message <- f('Progress: ', value, ' of ', max.value, '  ')
      cat(backspaces, message, sep = '')
    }
  }
  if (.Platform$OS.type == 'windows') 
    flush.console()
  cat('\n')
}

#' Recode a variable
#' 
#' A function for recoding numeric, character, and factor values in a vector, 
#' list, matrix, or data frame.
#' 
#' @param object object to recode
#' @param pattern what to replace
#' @param replacement what to replace \code{pattern} with
#' @param ... for future use
#' 
#' @details When recoding a factor variable with a new level, \code{recoder} 
#' automatically adds the corresponding level to \code{levels(object)} to avoid
#' errors.
#' @note The function currently recursively replaces \code{pattern[i]} with 
#' \code{replacement[i]} in sequential order, so if you intend to swap values,
#' say \code{a} and \code{b}, in an \code{object}, \code{recoder} will instead
#' first replace all occurrences of \code{a} with \code{b} and then all 
#' occurrences of \code{b} with \code{a} resulting in the \code{object} with no
#' \code{b} occurrences. See examples. I will fix this eventually.
#' @return Returns an object with the same length (or dimensions) and class as 
#' \code{object}
#' @seealso \code{\link[car]{recode}}
#' 
#' @examples
#' data(mtcars)
#' recoder(mtcars$carb, c(1, 2), c('A','B'))
#' recoder(mtcars, c(1, 2), c('A', 'B'))
#' mtcars <- within(mtcars, carb1 <- factor(carb))
#' recoder(mtcars$carb1, 1, 999)
#' 
#' tmp <- c(list(1:5), list(5), list(NA))
#' recoder(tmp, 5, NA)
#' 
#' # example from note
#' tmp <- 1:10
#' recoder(tmp, c(1, 2), c(2, 1))
#' # [1]  1  1  3  4  5  6  7  8  9 10    # actual return
#' # [1]  2  1  3  4  5  6  7  8  9 10    # intended return
#' 
#' @export

recoder <- function(object, pattern, replacement, ...) {
  
  ## to do:
  # add swapping option
  # add expression option, eg, if object[i, j] > 0, use replacement
  # fix level printing: DONE
  # allow NA for input: DONE
  # need to recode factor and numeric NAs simultaneously?
  
  m <- match.call()
  op <- options()
  on.exit(options(op))
  options(stringsAsFactors = FALSE)
  
  if (is.factor(object)) {
    cat('level(s)', 
        levels(factor(levels = setdiff(replacement, levels(object)))),
        'added to factor variable', deparse(m$object),'\n')
    levels(object) <- c(levels(object), replacement)
    #     object <- droplevels(object)
  }
  if (length(replacement) == 1)
    replacement <- rep(replacement, length(pattern))
  
  ## helper functions
  splitter <- function(df){
    LIST <- split(t(df), 1:ncol(df))
    names(LIST) <- names(df)
    return(LIST)
  }
  switcher <- function(f, g, h){ 
    if (is.na(g)) {
      f[is.na(f)] <- h
    } else {
      f[f == g] <- h }
    return(f)
  } 
  superswitcher <- function(x, y, z){
    DF <- data.frame(y, z, stringsAsFactors = FALSE)
    z <- x
    if (class(DF[ , 2]) %in% c('character', 'factor')) {
      lapply(1:nrow(DF), function(i) {
        if (sum(z %in% DF[i, 1]) == 0) {
          z <<- z
        } else {
          z <<- switcher(z, DF[i, 1], as.character(DF[i, 2])) 
        }
      })
    } else {
      lapply(1:nrow(DF), function(i) {
        z <<- switcher(z, DF[i, 1], DF[i, 2]) 
      })
    }
    return(z)
  }
  ## end helper functions
  
  # treat certain object classes differently
  if (is.vector(object) & !is.list(object)) {
    sapply(object, superswitcher, pattern, replacement)
  } else {
    if (is.data.frame(object)) {
      tmp <- do.call(data.frame, lapply(unclass(object)[1:ncol(object)], 
                                        superswitcher, pattern, replacement))
      rownames(tmp) <- attr(object, 'row.names')
      return(tmp)
    }
    if (is.matrix(object)) {
      nrow <- nrow(object)
      tmp <- do.call(rbind, 
                     lapply(object, superswitcher, pattern, replacement))
      tmp <- matrix(tmp, nrow = nrow, byrow = FALSE)
      return(tmp)
    } else {
      if (is.factor(object)) 
        unlist(lapply(object, superswitcher, pattern, replacement))
      else lapply(object, superswitcher, pattern, replacement)
    }
  }
}

#' Pairwise sum
#' 
#' Compute the pairwise sum of two or more vectors.
#' 
#' Each vector passed in \code{...} must be equal in length. The function
#' coerces the vectors into a matrix and \code{\link{rowSums}} the rows.
#' 
#' @param ... numeric vectors
#' @param na.rm logical; if \code{TRUE}, omits missing values (including
#' \code{\link{NaN}}) from calculations
#' 
#' @return A single vector of element-wise sums.
#' @seealso \code{\link{pmin}}, \code{\link{pmax}}
#' 
#' @examples
#' x <- c(-1, NA, 4, 5)
#' y <- c(NA, NA, 6, -1)
#' psum(x, y)
#' psum(x, y, na.rm = TRUE)
#' 
#' @export

psum <- function(..., na.rm = FALSE) {
  dat <- do.call(cbind, list(...))
  res <- rowSums(dat, na.rm = na.rm) 
  idx_na <- !rowSums(!is.na(dat))
  res[idx_na] <- NA
  res 
}

#' Test two or more objects for exact equality
#' 
#' The safe and reliable way to test two or more objects for being exactly 
#' equal; returns \code{TRUE} in this case, \code{FALSE} in every other case.
#'    
#' @param ... any \code{R} objects
#' @param num.eq logical indicating if (\code{\link{double}} and 
#' \code{\link{complex}} non-\code{\link{NA}}) numbers should be compared using
#' \code{\link{==}} ("equal"), or by bitwise comparison. The latter 
#' (non-default) differentiates between -0 and +0.
#' @param single.NA logical indicating if there is conceptually just one 
#' numeric \code{NA} and one \code{\link{NaN}}; \code{single.NA = FALSE} 
#' differentiates bit patterns.
#' @param attrib.as.set logical indicating if \code{\link{attributes}} of 
#' \code{...} should be treated as \emph{unordered} tagged pairlists ("sets"); 
#' this currently also applies to \code{\link{slot}}s of S4 objects. It may 
#' well be too strict to set \code{attrib.as.set = FALSE}.
#' @param ignore.bytecode logical indicating if byte code should be ignored 
#' when comparing \code{\link{closure}}s.
#' @param ignore.environment logical indicating if their environments should be
#' ignored when comparing \code{closure}s.
#' 
#' @return
#' A single logical value, \code{TRUE} or \code{FALSE}, never \code{NA}
#' and never anything other than a single value.
#' 
#' @seealso \code{\link{identical}}; \code{\link{all.equal}} for 
#' descriptions of how two objects differ; \code{\link{Comparison}} for 
#' operators that generate elementwise comparisons; \code{\link{isTRUE}} is a 
#' simple wrapper based on \code{identical}.
#' 
#' @examples
#' ident(1, 1.)
#' ident(1, 1., 1L)
#' 
#' # for unusual R objects:
#' ident(.GlobalEnv, environment(), globalenv(), as.environment(1))
#' 
#' ident(0., 0, -0.) # not differentiated
#' ident(0., 0, -0., num.eq = FALSE)
#' 
#' ident(NaN, -NaN)
#' ident(NaN, -NaN, single.NA = FALSE) # differ on bit-level
#' 
#' # for functions
#' f <- function(x) x
#' g <- compiler::cmpfun(f)
#' ident(f, g)
#' ident(f, g, ignore.bytecode = FALSE)
#' 
#' @export

ident <- function(..., num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
                  ignore.bytecode = TRUE, ignore.environment = FALSE) {
  lst <- list(...)
  if (length(lst) < 2L)
    stop('must provide at least two objects')
  zzz <- sapply(1:(length(lst) - 1), 
                function(x) 
                  identical(lst[x], lst[x + 1], 
                            num.eq = num.eq, 
                            single.NA = single.NA, 
                            attrib.as.set = attrib.as.set,
                            ignore.bytecode = ignore.bytecode, 
                            ignore.environment = ignore.environment))
  all(zzz)
}

#' Search function for data frames
#' 
#' Searches a data frame column for matches.
#' 
#' @param pattern string to find
#' @param data data frame to search
#' @param col.name column name in \code{data} to search
#' @param var variation; maximum distance allowed for a match; see 
#' \code{\link{agrep}}
#' @param ignore.case logical; if \code{FALSE}, the pattern matching is 
#' \emph{case-sensitive}, and if \code{TRUE}, case is ignored during matching
#' @param ... additional arguments passed to \code{\link{agrep}}
#' 
#' @return
#' Subset of the original \code{data} where the \code{pattern} was found in 
#' the specified \code{col.name}.
#' 
#' @examples
#' df <- data.frame(islands = names(islands)[1:32], mtcars)
#' search_df(New, df, islands)
#' search_df(ho, df, islands, var = 0.2) # too much variation
#' search_df(ho, df, islands, var = 0)
#' search_df('Axel Hieberg', df, islands) # misspelled, not enough variation
#' search_df('Axel Hieberg', df, islands, var = 2)
#' search_df(19, df, mpg)
#' 
#' @export

search_df <- function(pattern, data, col.name, var = 0,
                      ignore.case = TRUE, ...) {
  p <- as.character(substitute(pattern))
  x <- as.character(substitute(col.name))
  zzz <- agrep(p, data[, x], ignore.case = ignore.case,
               max.distance = var, ...)
  df[zzz, ]
}

#' Search history
#' 
#' Searches Rhistory file for pattern matches.
#' 
#' @param x numeric or character; if numeric, shows the most recent \code{n} 
#' lines in \code{.Rhistory}; if character, searches for pattern matches
#' @param ... additional arguments passed to \code{\link{grep}}
#' 
#' @return
#' A list of recent commands that match \code{pattern}.
#' 
#' @examples
#' search_hist()
#' search_hist(25)
#' search_hist('?')
#' search_hist('?', fixed = TRUE)
#' search_hist('\\?')
#' 
#' @export

search_hist <- function (x, ...) {
  hist <- tryCatch(readLines('.Rhistory'),
                   warning = function(w) message("No history found"),
                   finally = return(invisible()))
  lhist <- length(hist)
  if (is.numeric(x))
    return(hist[lhist:(lhist - x + 1)])
  if (is.character(x))
    return(grep(x, readLines(".Rhistory"), value = TRUE, ...))
}

#' Apply list of functions over list or vector
#' 
#' A simple modification to the \code{*apply} functions which allows a list of 
#' functions to be passed simultaneously.
#' 
#' @param X a vector (atomic or list) or an \code{\link{expression}} object; 
#' other objects (including classed objects) will be coerced by 
#' \code{base::\link{as.list}}.
#' @param FUN a list of functions to be applied to each element of \code{X}; 
#' see \code{\link{lapply}}:"Details"
#' @param ... additional arguments passed to \code{FUN}; note that these must
#' be defined arguments for each function in \code{FUN}; otherwise, you will
#' need to define a function before passing to \code{FUN}; see examples
#' 
#' @return A data frame where \code{nrow} equals the length of \code{X} and 
#' \code{ncol} equals the length of \code{FUN}.
#' 
#' @examples
#' tmp <- recoder(mtcars, 6, NA)
#' fapply(tmp, list(mean, median))
#' fapply(tmp, list(mean, median), na.rm = TRUE)
#' 
#' ## define a new function
#' `95% CI` <- function(x)
#'   sprintf('(%s)', paste0(quantile(x, c(.025, .975)), collapse = ', '))
#' fapply(mtcars, list(median, `95% CI`))
#' 
#' ## compare: 
#' t(fapply(mtcars, list(min, mean, max, length)))
#' summary(mtcars)
#' 
#' @export

fapply <- function(X, FUN, ...) {
  fn <- as.character(match.call()$FUN)[-1]
  out <- sapply(FUN, mapply, X, ...)
  setNames(as.data.frame(out), fn)
}

#' Rescale numeric vector
#' 
#' Rescale a numeric vector to have specified maximum and minimum; shamelessly
#' stolen from hadley's \code{scales} package.
#' 
#' @param x numeric vector of values
#' @param to output range (numeric vector of length two)
#' @param from input range (numeric vector of length two); if not given, 
#' \code{from} is calculated from the range of \code{x}
#' 
#' @seealso \code{\link[scales]{rescale}}; \code{\link[scales]{zero_range}}
#' 
#' @examples
#' rescaler(1:100)
#' rescaler(runif(10))
#' rescaler(1)
#' 
#' @export

rescaler <- function (x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  zero_range <- function (x, tol = .Machine$double.eps * 100) {
    if (length(x) == 1) return(TRUE)
    if (length(x) != 2) stop('x must be length one or two')
    if (any(is.na(x))) return(NA)
    if (x[1] == x[2]) return(TRUE)
    if (all(is.infinite(x))) return(FALSE)
    m <- min(abs(x))
    if (m == 0) return(FALSE)
    abs((x[1] - x[2]) / m) < tol
  }
  if (zero_range(from) || zero_range(to))
    return(rep(mean(to), length(x)))
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

#' Quietly try to require a package
#' 
#' Quietly require a package, returning an error message if not installed.
#' 
#' @param package name of package as name or character string
#' 
#' @export

try_require <- function(package) {
  package <- ifelse(!is.character(substitute(package)), 
                    as.character(substitute(package)),
                    package)
  available <- suppressMessages(
    suppressWarnings(
      sapply(package, require, quietly = TRUE, 
             character.only = TRUE, warn.conflicts=FALSE)))
  missing <- package[!available]
  if (length(missing) > 0)
    stop(paste(package, collapse = ', '), ' package not found.',
         call. = FALSE)
}

#' List to file
#' 
#' Save a \emph{named} list of data frames or matrices into \code{R} data files
#' \code{.rda}, \code{.csv}, or \code{.txt} files.
#' 
#' @param l a list of data frames or matrices
#' @param targetdir target directory (created if doesn't exist)
#' @param sep field separator string; default is none which results in 
#' \code{.rda} data files; "\code{,}" creates \code{.csv} files; any other
#' separator will create \code{.dat} files
#' @param ... additional arguments passed to \code{\link{save}} if \code{sep}
#' is not given or to \code{\link{write.table}} if \code{sep} is given
#' 
#' @return
#' \code{list2file} will create \code{length(l)} files in the \code{targetdir}.
#' 
#' @examples
#' \dontrun{
#' dfl <- setNames(list(mtcars, iris), c('mtcars','iris'))
#' 
#' ## .csv files
#' list2file(dfl, '~/desktop/tmp', sep = ',')
#' 
#' ## r data files
#' list2file(dfl, '~/desktop/tmp')
#' }
#' 
#' @export

list2file <- function(l, targetdir = getwd(), sep, ...) {
  if (!is.list(l))
    stop('\'l\' must be a list')
  if (is.null(names(l)) || any(is.na(names(l))))
    stop('all elements of \'l\' must be named')
  if (any(sapply(l, class) %ni% c('data.frame','matrix')))
    stop('all elements of \'l\' should be class \'matrix\' or \'data.frame\'')
  if (!file.exists(targetdir)) {
    message(sprintf('creating directory:\n%s', targetdir))
    dir.create(targetdir)
  }
  e <- new.env()
  list2env(l, envir = e)
  
  if (missing(sep)) {
    sapply(names(l), function(x)
      save(x, file = sprintf('%s/%s.rda', targetdir, x), ...))
  } else
    sapply(names(l), function(x)
      write.table(get(x, envir = e), 
                  file = sprintf('%s/%s.%s', targetdir, x, 
                                 ifelse(sep == ',','csv','dat')),
                  row.names = FALSE, quote = FALSE, ...))
  message(sprintf('NOTE: %s written to %s', iprint(names(l)), targetdir))
  invisible()
}

#' Restart
#' 
#' Ends current and restarts a clean \code{R} session.
#' 
#' @param afterRestartCommand character string of command(s) to be 
#' executed after restarting
#' 
#' @examples
#' \donttest{
#' Restart("clear(); cat('Here is a clean session just for you')")
#' }
#' 
#' @export

Restart <- function(afterRestartCommand = '')
  (getOption('restart'))(afterRestartCommand)

# Reload <- function(...) {
#   ## clean (rstudio) r session packages:
#   pkgs <- c(".GlobalEnv", "tools:rstudio", "package:stats", "package:graphics",
#             "package:grDevices", "package:utils", "package:datasets",
#             "package:methods", "Autoloads", "package:base")
#   to_unload <- setdiff(search(), pkgs)
#   for (pkg in to_unload)
#     try(detach(pkg, unload = TRUE, character.only = TRUE), silent = TRUE)
#   rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
#   cat('\014')
#   invisible(NULL)
# }

#' clc
#' 
#' Clear the workspace by removing all objects in \code{\link{ls}}.
#' 
#' @param all logical; if \code{TRUE}, also removes hidden files
#' 
#' @export

clc <- function(all = FALSE)
  rm(list = ls(.GlobalEnv, all.names = all), envir = .GlobalEnv)

#' clear
#' 
#' Clear the console window.
#' 
#' @param ... ignored
#' 
#' @export

clear <- function(...) cat('\014')

#' Extract R help files
#' 
#' Extracts specified portions of R help files (from \emph{loaded} libraries)
#' for use in Sweave or R-markdown documents.
#' 
#' The \code{type} argument accepts:
#' \itemize{ 
#' \item \code{"m_code"}: Markdown code chunks; for use with markdown documents
#' when highlighted code is expected.
#' \item \code{"m_text"}: Markdown plain text; for use with markdown documents 
#' where regular text is expected.
#' \item \code{"s_code"}: Sweave code chunks; for use with Sweave documents 
#' where highlighted code is expected.
#' \item \code{"s_text"}: Sweave plain text; for use with Sweave documents 
#' where regular text is expected.
#' }
#' 
#' To see the results in the console:
#' 
#' \code{cat(helpExtract(print, type = 'm_text'))}
#' 
#' To insert a (highlighted) chunk into a markdown document:
#' 
#' \verb{
#' ```{r, results='asis'}
#' cat(helpExtract(print), sep ='\n')
#' ```
#' }
#' 
#' To insert a (highlighted) chunk into a Sweave document:
#' 
#' \verb{
#' \\Sexpr{knit_child(textConnection(helpExtract(print, type = 's_code')),
#'      options = list(tidy = FALSE, eval = FALSE))}
#' }
#' 
#' @param FUN a function
#' @param show.sections logical; if \code{TRUE}, returns \code{section} options
#' for \code{FUN}
#' @param section section to extract (default is \code{"Usage"}
#' @param type type of character vector you want returned; default is 
#' \code{"m_code"}, see details
#' @param ... additional arguments passed to \code{utils:::.getHelpFile}
#' 
#' @return 
#' A character vector to be used in a Sweave or R-markdown document.
#' 
#' @examples
#' cat(helpExtract(print), sep = "\n")
#' 
#' cat(helpExtract(print, type = 'm_text'))
#' 
#' cat(helpExtract(print, type = 'm_text', section = 'description'))
#' 
#' @export

helpExtract <- function(FUN, show.sections = FALSE, section = 'Usage', 
                        type = 'm_code', ...) {
  ## helpers 
  # tools:::fetchRdDB
  fetchRdDB <- function (filebase, key = NULL) {
    fun <- function(db) {
      vals <- db$vals
      vars <- db$vars
      datafile <- db$datafile
      compressed <- db$compressed
      envhook <- db$envhook
      fetch <- function(key) 
        lazyLoadDBfetch(vals[key][[1L]], datafile, compressed, envhook)
      if (length(key)) {
        if (!key %in% vars) 
          stop(gettextf("No help on %s found in RdDB %s", 
                        sQuote(key), sQuote(filebase)), domain = NA)
        fetch(key)
      } else {
        res <- lapply(vars, fetch)
        names(res) <- vars
        res
      }
    }
    res <- lazyLoadDBexec(filebase, fun)
    if (length(key)) 
      res
    else invisible(res)
  }
  # utils:::.getHelpFile
  getHelpFile <- function (file) {
    path <- dirname(file)
    dirpath <- dirname(path)
    if (!file.exists(dirpath)) 
      stop(gettextf("invalid %s argument", sQuote("file")), domain = NA)
    pkgname <- basename(dirpath)
    RdDB <- file.path(path, pkgname)
    if (!file.exists(paste(RdDB, "rdx", sep = "."))) 
      stop(gettextf("package %s exists but was not installed under R >= 2.10.0 so help cannot be accessed", 
                    sQuote(pkgname)), domain = NA)
    fetchRdDB(RdDB, basename(file))
  }
  
  A <- deparse(substitute(FUN))
  x <- capture.output(tools::Rd2txt(getHelpFile(utils::help(A, ...)),
                                     options = list(sectionIndent = 0)))
  ## section start lines
  B <- grep('^_', x)
  x <- gsub('_\b', '', x, fixed = TRUE)
  if (show.sections)
    return(gsub(':','', x[B]))
  X <- rep(FALSE, length(x))
  X[B] <- 1
  out <- split(x, cumsum(X))
  out <- out[[which(sapply(out, function(x) 
    grepl(section, x[1], fixed = FALSE, ignore.case = TRUE)))]][-c(1, 2)]
  while (TRUE) {
    out <- out[-length(out)]
    if (out[length(out)] != '')
      break
  }
  
  switch(type,
         m_code = c('```r', out, '```'),
         s_code = c('<<>>=', out, '@'),
         m_text = paste('    ', out, collapse = '\n'),
         s_text = c('\\begin{verbatim}', out, '\\end{verbatim}'),
         stop('\"type\" must be either \"m_code\", \"s_code\", \"m_text\", ',
              'or \"s_text\"')
  )
}

#' Round vector to target sum
#' 
#' Rounds a numeric vector constrained to sum to a \code{target} value.
#' 
#' @param x numeric values
#' @param target desired sum of \code{x} after rounding
#' 
#' @examples
#' pcts <- data.frame(pct1 = c(33.3, 21.5, 45.51),
#'                    pct2 = c(33.3,33.3,33.3))
#' 
#' ## base round                                     
#' Map(round, pcts); sapply(.Last.value, sum)
#' 
#' ## round to target
#' Map(Round, pcts, 100); sapply(.Last.value, sum)
#' 
#' @export

Round <- function(x, target) {
  r.x <- round(x)
  diff.x <- r.x - x
  if ((s <- sum(r.x)) == target) {
    return(r.x)
  } else if (s > target) {
    select <- seq_along(x)[diff.x != 0]
    wh <- which.max(diff.x[select])
    x[select[wh]] <- r.x[select[wh]] - 1
    Recall(x, target)
  } else {
    select <- seq_along(x)[diff.x != 0]
    wh <- which.min(diff.x[select])
    x[select[wh]] <- r.x[select[wh]] + 1
    Recall(x, target)
  }
}

#' Bind objects
#' 
#' Utilities for binding objects with inconsistent dimensions.
#' 
#' \code{bind_all} and \code{rbindfill} are used for binding vectors,
#' the latter specifically for \code{\link{rbind}}ing \emph{named} vectors
#' a la a "stacking" merge.
#' 
#' \code{cbindx} and \code{rbindx} take vector-, matrix-, and data frame-like
#' objects and bind normally, filling with \code{NA}s where dimensions are
#' not equal.
#' 
#' \code{rbindfill2} row-binds data frames with zero or more common column
#' names. \code{rbindfill2} starts with the first data frame given and
#' \code{rbind}s subsequent data frames adding new columns of \code{NA} as
#' needed to bind. Any columns with matching names will be aggregated;
#' otherwise, data frames without a matching column of data will be filled
#' with \code{NA}.
#' 
#' @param ... for \code{bind_all} and \code{rbindfill}, vectors;
#' \code{cbindx} and \code{rbindx} will accept vectors, matrices, data frames
#' @param which joining method; \code{'rbind'} or \code{'cbind'}
#' @param deparse.level integer controlling the construction of labels in
#' the case of non-matrix-like arguments (for the default method):\cr
#' \code{deparse.level = 0} constructs no labels; the default; \cr
#' \code{deparse.level = 1} or \code{2} constructs labels from the argument
#' names \cr see \code{\link{cbind}}
#' @param use.rownames logical; if \code{TRUE}, data frames in a \emph{named}
#' list will retain corresponding rownames; the default is to remove rownames
#' (note that this parameter is ignored if \dots is not a named list)
#' 
#' @seealso \code{\link{interleave}}, \pkg{qpcR}, \code{\link{cbind}},
#' \code{\link{rbind}}
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
#' f <- function(x) setNames(letters[x], LETTERS[x])
#' x <- lapply(list(1:5, 3:6, 2:7, 26), f)
#' do.call('rbindfill', x)
#' 
#' set.seed(1)
#' dd <- matrix(NA, nrow = 1, ncol = 10)
#' dd <- as.data.frame(col(dd))
#' l <- setNames(lapply(1:5, function(x) dd[, sample(x), drop = FALSE]),
#'               letters[1:5])
#' 
#' Reduce(rbindfill2, l) ## or do.call('rbindfill2', l)
#' do.call('rbindfill2', c(l, use.rownames = TRUE))
#' rbindfill2(l$c, l$e)
#' 
#' rbindfill2(mtcars, cars)
#' 
#' @name bindx
NULL

#' @rdname bindx
#' @export
bind_all <- function(..., which) {
  if (missing(which))
    stop('specify which: \'rbind\' or \'cbind\'')
  l <- list(...)
  if (any(sapply(l, function(x) !is.null(dim(x)))))
    warning('This function is intended for vector inputs. ',
            'Use ?cbindx or ?rbindx instead.')
  l <- lapply(l, `length<-`, max(sapply(l, length)))
  do.call(which, l)
}

#' @rdname bindx
#' @export
cbindx <- function (..., deparse.level = 1) {
  na <- nargs() - (!missing(deparse.level))    
  deparse.level <- as.integer(deparse.level)
  stopifnot(0 <= deparse.level, deparse.level <= 2)
  argl <- list(...)   
  while (na > 0 && is.null(argl[[na]])) {
    argl <- argl[-na]
    na <- na - 1
  }
  if (na == 0)
    return(NULL)
  if (na == 1) {
    if (isS4(..1))
      return(cbind2(..1))
    else return(matrix(...))  ##.Internal(cbind(deparse.level, ...)))
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
  if (na == 0) {
    r <- argl[[2]]
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
        2:(na - 1) else 2:na
      if (any(iV[ii])) {
        for (i in ii[iV[ii]]) 
          if (!is.null(nmi <- Nms(i)))
            names(argl)[i] <- nmi
      }
    }
    
    ## filling with NA's to maximum occuring nrows
    nRow <- as.numeric(sapply(argl, function(x) NROW(x)))
    maxRow <- max(nRow, na.rm = TRUE)
    argl <- lapply(argl, function(x) 
      if (is.null(nrow(x))) {
        c(x, rep(NA, maxRow - length(x)))
      } else rbindx(x, matrix(, maxRow - nrow(x), ncol(x))))
    r <- do.call('cbind', c(argl[-1L], list(deparse.level = deparse.level)))
  }
  d2 <- dim(r)
  r <- cbind2(argl[[1]], r)
  if (deparse.level == 0)
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
  nn1 <- !is.null(N1 <- if ((l1 <- Ncol(..1)) && !ism1) Nms(1))
  nn2 <- !is.null(N2 <- if (na == 2 && Ncol(..2) && !ism2) Nms(2))
  if (nn1 || nn2 || fix.na) {
    if (is.null(colnames(r)))
      colnames(r) <- rep.int('', ncol(r))
    setN <- function(i, nams) colnames(r)[i] <<- if (is.null(nams)) ''
    else nams
    if (nn1)
      setN(1, N1)
    if (nn2)
      setN(1 + l1, N2)
    if (fix.na)
      setN(ncol(r), Nna)
  }
  r
}

#' @rdname bindx
#' @export
rbindx <- function (..., deparse.level = 1) {
  na <- nargs() - (!missing(deparse.level))
  deparse.level <- as.integer(deparse.level)
  stopifnot(0 <= deparse.level, deparse.level <= 2)
  argl <- list(...)
  while (na > 0 && is.null(argl[[na]])) {
    argl <- argl[-na]
    na <- na - 1
  }
  if (na == 0)
    return(NULL)
  if (na == 1) {
    if (isS4(..1))
      return(rbind2(..1))
    else return(matrix(..., nrow = 1)) ##.Internal(rbind(deparse.level, ...)))
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
  if (na == 0) {
    r <- argl[[2]]
    fix.na <- FALSE
  } else {
    nrs <- unname(lapply(argl, ncol))
    iV <- sapply(nrs, is.null)
    fix.na <- identical(nrs[(na - 1):na], list(NULL, NULL))
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
        2:(na - 1) else 2:na
      if (any(iV[ii])) {
        for (i in ii[iV[ii]])
          if (!is.null(nmi <- Nms(i)))
            names(argl)[i] <- nmi
      }
    }
    
    ## filling with NAs to maximum occuring ncols
    nCol <- as.numeric(sapply(argl, function(x)
      if (is.null(ncol(x)))
        length(x) else ncol(x)))
    maxCol <- max(nCol, na.rm = TRUE)
    argl <- lapply(argl, function(x)
      if (is.null(ncol(x))) {
        c(x, rep(NA, maxCol - length(x)))
      } else cbind(x, matrix(, nrow(x), maxCol - ncol(x))))
    
    ## create a common name vector from the
    ## column names of all 'argl' items
    namesVEC <- rep(NA, maxCol)
    for (i in 1:length(argl)) {
      CN <- colnames(argl[[i]])
      m <- !(CN %in% namesVEC)
      namesVEC[m] <- CN[m]
    }
    
    ## make all column names from common 'namesVEC'
    for (j in 1:length(argl)) {
      if (!is.null(ncol(argl[[j]]))) colnames(argl[[j]]) <- namesVEC
    } 
    r <- do.call('rbind', c(argl[-1L], list(deparse.level = deparse.level)))
  }  
  d2 <- dim(r)
  
  ## make all column names from common 'namesVEC'
  colnames(r) <- colnames(argl[[1]])
  r <- rbind2(argl[[1]], r)
  
  if (deparse.level == 0)
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
  nn1 <- !is.null(N1 <- if ((l1 <- Nrow(..1)) && !ism1) Nms(1))
  nn2 <- !is.null(N2 <- if (na == 2 && Nrow(..2) && !ism2) Nms(2))
  if (nn1 || nn2 || fix.na) {
    if (is.null(rownames(r)))
      rownames(r) <- rep.int('', nrow(r))
    setN <- function(i, nams) rownames(r)[i] <<- if (is.null(nams)) ''
    else nams
    if (nn1)
      setN(1, N1)
    if (nn2)
      setN(1 + l1, N2)
    if (fix.na)
      setN(nrow(r), Nna)
  }
  r
}

#' @rdname bindx
#' @export
rbindfill <- function(...) {
  l <- list(...)
  nn <- sapply(l, names)
  un <- unique(unlist(nn))
  len <- sapply(l, length)
  out <- vector('list', length(len))
  for (ii in seq_along(len)) {
    out[[ii]] <- unname(l[[ii]])[match(un, nn[[ii]])]
  }
  `colnames<-`(do.call('rbind', out), un)
}

#' @rdname bindx
#' @export
rbindfill2 <- function(..., use.rownames = FALSE) {
  l <- list(...)
  nn <- sapply(l, names)
  un <- unique(unlist(nn))
  out <- lapply(l, function(x) {
    if (!all(wh <- un %in% names(x))) {
      tmp <- as.data.frame(matrix(NA, nrow = nrow(x), ncol = sum(!wh),
                                  dimnames = list(NULL, un[!wh])))
      res <- do.call('cbind.data.frame', list(x, tmp))
      res[, un]
    } else x[, un]
  })
  res <- do.call('rbind.data.frame', out)
  if (use.rownames)
    res else `rownames<-`(res, NULL)
}

#' Interleave rows or columns
#' 
#' Interleave rows (or columns) of vectors, matrices, or data frames.
#' 
#' @param ... vectors, matrices, or data frames
#' @param which joining method to use (\code{'rbind'} or \code{'cbind'}) when
#' \code{...} are matrices or data frames
#' @seealso \code{\link{bindx}}
#' 
#' @examples
#' interleave(letters[1:3],
#'            LETTERS[3:1],
#'            letters[26:24])
#' interleave(t(matrix(1:9, 3, 3)),
#'            t(matrix(1:9 * 10, 3, 3)),
#'            which = 'rbind')
#' interleave(matrix(1:9, 3, 3),
#'            matrix(1:9 * 10, 3, 3),
#'            which = 'cbind')
#' 
#' @export

interleave <- function(..., which) {
  l <- list(...)
  if (all(sapply(l, function(x) is.null(dim(x)))))
    return(c(do.call('rbind', l)))
  else {
    if (missing(which))
      stop('specify which: \'rbind\' or \'cbind\'')
    if (which == 'rbind')
      return(do.call('rbind', l)[order(sequence(sapply(l, nrow))), ])
    else if (which == 'cbind')
      return(do.call('cbind', l)[ , order(sequence(sapply(l, ncol)))])
  }
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
  vf <- Vectorize(function(x, y) c(as.list(x), as.list(y)), SIMPLIFY = FALSE)
  f <- function(l) Reduce(function(x, y) outer(x, y, vf), l)
  args <- f(list(...))
  res <- apply(args, 1:length(dim(args)), function(x) do.call(FUN, x[[1]]))
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

merge2 <- function(l, ...) Reduce(function(x, y) merge(x, y, ...), l)

#' Last observation carried forward
#' 
#' Replaces \code{NA} in vectors, data frames, or matrices with most recent
#' non-\code{NA} value
#' 
#' @param x a vector, matrix, or data frame
#' @param fromLast logical; if \code{TRUE}, starts from end
#' 
#' @examples
#' df <- data.frame(V1 = c('Bob', NA, NA, 'Joe', NA, NA),
#'                  V2 = c(NA, 1, NA, NA, 2, NA))
#' 
#' within(df, {
#'   V1 <- locf(V1)
#'   V2 <- locf(V2, fromLast = TRUE)
#' })
#' 
#' locf(df)
#' 
#' @export

locf <- function(x, fromLast = FALSE) {
  if (!(ok <- !is.null(nrow(x))))
    x <- data.frame(x)
  indx <- !is.na(x)
  #   if (!missing(ch.strings))
  #     indx <- do.call('cbind', lapply(x, `%in%`, ch.strings))
  x[] <- lapply(seq_along(x), function(ii) {
    if (fromLast) {
      idx <- rev(cumsum(rev(indx[, ii])))
      idx[idx == 0] <- NA
      return(rev(x[, ii])[rev(indx[, ii])][idx])
    } else {
      idx <- cumsum(indx[, ii])
      idx[idx == 0] <- NA
      return(x[, ii][indx[, ii]][idx])
    }
  })
  if (ok)
    return(x)
  else return(x[, 1])
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

roll_fun <- function(x, n = 5, FUN = mean, ..., fromLast = FALSE, keep = FALSE) {
  l <- lapply(seq_along(x), function(ii) {
    if (fromLast) 
      x[length(x) + 1 - tail(sequence(ii), n)]
    else x[tail(sequence(ii), n)]
  })
  if (keep)
    l[1:n] <- lapply(1:n, function(x) l[[n]])
  sapply(if (fromLast) rev(l) else l, FUN, ...)
}

#' Round to
#' 
#' Round numerics to nearest multiple of \code{to}.
#' 
#' @param x a numeric vector
#' @param to nearest fraction or integer
#' 
#' @examples
#' round_to(mtcars$mpg, 5)
#' round_to(mtcars$mpg, .5)
#' 
#' @export

round_to <- function(x, to = 1) round(x / to) * to

#' Update R
#' 
#' Copies and updates \code{R} libraries from most recent installed version
#' into the current \code{\link{.libPaths}} directory. This assumes that the
#' user has installed a new \code{X.x} version of \code{R} but will not copy
#' any libraries from previous frameworks into the new library.
#' 
#' @param update logical; if \code{TRUE}, checks for available packages
#' updates, downloads, and installs
#' 
#' @seealso \code{\link{update.packages}}
#' 
#' @export

updateR <- function(update = TRUE) {
  path <- file.path(R.home(), '..', '..')
  v <- tail(sort(list.files(path, pattern = '^\\d{1}.\\d{1}$')), 2)
  if (!grepl(v[2], .libPaths()))
    stop("A more recent version of R was found on your system\n")
  if (file.exists(v_last <- sub(v[2], v[1], .libPaths()))) {
    pkg <- list.files(.libPaths())
    pkg <- setdiff(list.files(v_last), pkg)
    if (length(pkg) > 0) {
      cat(sprintf("Copying %s package%s to %s\n", length(pkg),
                  ifelse(length(pkg) > 1, 's', ''), .libPaths()))
      file.copy(file.path(v_last, pkg), .libPaths(), recursive = TRUE)
    } else cat("No packages to copy\n")
  }
  if (update) {
    if ((up <- table(packageStatus()$inst$Status)['upgrade']) > 0) {
      cat(sprintf("Updating %s package%s\n", up, ifelse(up > 1, 's', '')))
      update.packages(ask = FALSE)
    } else cat("All packages are up-to-date\n")
  }
}

#' Read data from clipboard
#' 
#' Reads data (comma-, tab-, or fixed-width separated) data from clipboard and
#' returns as a data frame.
#' 
#' @param header logical; indicates if variable names are in first line
#' @param ... additional arguments passed to \code{\link{read.table}}
#' 
#' @seealso \code{\link[psych]{read.clipboard}}, \code{\link{read.table}},
#' \code{\link{read.fwf}}
#' 
#' @export

read_clip <- function(header = TRUE, ...) {
  if (Sys.info()['sysname'] %ni% 'Darwin')
    read.table(file = 'clipboard', header = header, ...)
  else read.table(file = pipe('pbpaste'), header = header, ...)
}

#' @rdname read_clip
#' @param sep separator as a character string
#' @export
read_clip.csv <- function(header = TRUE, sep = ',', ...)
  read_clip(header = header, sep = sep, ...)

#' @rdname read_clip
#' @export
read_clip.tab <- function(header = TRUE, sep = '\t', ...)
  read_clip(header = header, sep = sep, ...)

#' @rdname read_clip
#' @param widths a vector of widths of the fixed-width fields or a list of
#' vectors giving the widths for multiple lines
#' @export
read_clip.fwf <- function(header = TRUE, widths, ...) {
  if (Sys.info()['sysname'] %ni% 'Darwin')
    read.fwf(file = 'clipboard', header = header, widths = widths, ...)
  else read.fwf(file = pipe('pbpaste'), header = header, widths = widths, ...)
}

#' Find columns by pattern
#' 
#' Quickly selects and returns columns from a matrix or data frame by
#' \code{\link{grep}}'ing for a desired \code{pattern}.
#' 
#' @param x a matrix or data frame
#' @param pattern pattern to match
#' @param keep optional vector of names of other columns to keep
#' @param ... additional parameters passed to \code{\link{grep}}
#' 
#' @examples
#' fcols(iris, 'Petal')
#' fcols(iris, '\\.')
#' fcols(mtcars, '^[\\w]{2}$')
#' 
#' @export

fcols <- function(x, pattern, keep, ...) {
  keep <- if (missing(keep)) NULL else which(colnames(x) %in% keep)
  x[, c(keep, grep(pattern, colnames(x), perl = TRUE, ...)), drop = FALSE]
}

#' Show class methods
#' 
#' Lists available methods for a given class.
#' 
#' @param class an object or classes as a vector of character strings
#' 
#' @seealso \code{\link{methods}}, \code{\link{S3Methods}}, \code{\link{class}}
#' @references \url{https://gist.github.com/MrFlick/55ed854eb935e5c21f71}
#' 
#' @examples
#' fit <- glm(vs ~ mpg, data = mtcars)
#' classMethods(fit)
#' classMethods(c('glm', 'lm'))
#' 
#' @export

classMethods <- function(class) {
  if (!is.character(class)) 
    class <- class(class)
  cat(sprintf('class methods for %s:\n\n', paste0(class, collapse = ', ')))
  
  ml <- lapply(class, function(x) {
    sname <- gsub('([.[])', '\\\\\\1', paste0('.', x, '$'))
    m <- methods(class = x)
    if (length(m)) {
      data.frame(m = as.vector(m), c = x, n = sub(sname, '', as.vector(m)),
                 attr(m, 'info'), stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })
  df <- do.call('rbind', ml)
  df <- df[!duplicated(df$n), ]
  structure(df$m, byclass = FALSE, class = 'MethodsFunction',
            info = data.frame(visible = df$visible, from = df$from,
                              generic = df$generic, isS4 = df$isS4,
                              row.names = df$m))
}

#' Extract captured substrings
#' 
#' Extract the captured substrings from match data obtained by 
#' \code{\link{regexpr}}, \code{\link{gregexpr}}, or \code{\link{regexec}}.
#' 
#' @param x a character vector
#' @param m an object with match data
#' 
#' @return
#' A list with captures for each string in \code{x}.
#' 
#' @seealso \code{\link{regmatches}}
#' @references \url{https://gist.github.com/MrFlick/10413321}
#' 
#' @examples
#' x <- c('larry:35,M', 'alison:22,F', 'dave', 'lily:55,F')
#' m <- regexpr('(.*):(\\d+),([MF])', x, perl = TRUE)
#' regcaptures(x, m)
#' 
#' x <- 'ACCACCACCAC'
#' m <- gregexpr('(?=([AC]C))', x, perl = TRUE)
#' regcaptures(x, m)[[1]]
#' 
#' ## compare:
#' mapply(function(xx) substr(x, xx, xx + 1), m[[1]])
#' 
#' @export

regcaptures <- function(x, m) {
  if (length(x) != length(m))
    stop('\'x\' and \'m\' must have the same length')
  msg <- 'No capture data found'
  ili <- is.list(m)
  useBytes <- if (ili)
    any(unlist(lapply(m, attr, 'useBytes')))
  else
    any(attr(m, 'useBytes'))
  if (useBytes) {
    asc <- iconv(x, 'latin1', 'ASCII')
    ind <- is.na(asc) | (asc != x)
    if (any(ind))
      Encoding(x[ind]) <- 'bytes'
  }
  if (ili) {
    if (any(sapply(m, function(x) is.null(attr(x, 'capture.start')))))
      stop(msg)
    starts <- lapply(m, function(x) attr(x, 'capture.start'))
    lengths <- lapply(m, function(x) attr(x, 'capture.length'))
  } else {
    if (is.null(attr(m, 'capture.start')))
      stop(msg)
    starts <- data.frame(t(attr(m, 'capture.start')))
    lengths <- data.frame(t(attr(m, 'capture.length')))
  }
  
  Substring <- function(x, starts, lens) {
    if (all(starts < 0)) {
      return(character())
    } else {
      return(t(mapply(function(x, st, ln)
        substring(x, st, st + ln - 1), x, data.frame(t(starts)),
        data.frame(t(lens)), USE.NAMES = FALSE)
      ))
    }
  }
  
  Map(function(x, sos, mls) Substring(x, sos, mls), 
      x, starts, lengths, USE.NAMES = FALSE)
}

#' Extract parts of file path
#' 
#' These functions will extract the directory, file name, and file extension
#' of some common types of files. Additionally, \code{path_extract} will
#' check its results by recreating \code{path} and will give warnings if
#' the results fail to match the input.
#' 
#' \code{fname} and \code{path_extract} do the text processing; 
#' \code{file_name} and \code{file_ext} are convenience functions that only
#' return the file name or file extension, respectively.
#' 
#' Known examples where this function fails:
#' \itemize{
#'  \item{\code{.tar.gz} }{files with compound file extensions}
#' }
#' 
#' @param path file path as character string
#' 
#' @seealso \code{\link[rawr]{regcaptures}}, \code{\link{basename}},
#' \code{\link{dirname}}
#' 
#' @examples
#' l <- list('~/desktop/tmp.csv',               ## normal file with directory
#'           '.dotfile.txt',                    ## dotfile with extension
#'           '.vimrc',                          ## dotfile with no extension
#'           '~/file.',                         ## file name ending in .
#'           '~/DESCRIPTION',                   ## no extension
#'           '~/desktop/tmp/a.filename.tar.gz') ## compound extension fails
#' 
#' setNames(lapply(l, fname), l)
#' setNames(lapply(l, path_extract), l)
#' setNames(lapply(l, file_name), l)
#' setNames(lapply(l, file_ext), l)
#' 
#' @export

path_extract <- function(path) {
  p <- normalizePath(path, mustWork = FALSE)
  m <- cbind(dirname = dirname(p), basename = basename(p), fname(p))
  mm <- file.path(m[, 'dirname'],
                  paste(m[, 'filename'], m[, 'extension'],
                        sep = ifelse(nzchar(m[, 'extension']), '.', '')))
  if (gsub('\\./', '', mm) != p || !nzchar(m[, 'filename']))
    warning('Results could not be validated')
  m
}

#' @rdname path_extract
#' @export
fname <- function(path) {
  xx <- basename(path)
  pattern <- '(^\\.[^ .]+$|[^:\\/]*?[.$]?)(?:\\.([^ :\\/.]*))?$'
  m <- gregexpr(pattern, xx, perl = TRUE)
  `colnames<-`(regcaptures(xx, m)[[1]], c('filename','extension'))
}

#' @rdname path_extract
#' @export
file_name <- function(path) path_extract(path)[, 'filename']

#' @rdname path_extract
#' @export
file_ext <- function(path) path_extract(path)[, 'extension']

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
#' @seealso \code{\link{reshape}}
#' 
#' @examples
#' ## default of melt is no id variable so all are melted
#' melt(mtcars)
#' 
#' dat <- data.frame(id = rep(1:4, rep(2, 4)),
#'                   visit = I(rep(c('Before', 'After'), 4)),
#'                   x = abs(rpois(4, 10)), y = -runif(4))
#' 
#' ## equivalent ways to give `varying`
#' melt(dat, list(3:4))
#' melt(dat, 3:4)
#' (l <- melt(dat, c('x','y')))
#' 
#' ## equivalent ways to give `idvar`, `timevar`, `v.names`
#' cast(l, c('id','visit'), 'variable', 'value')
#' cast(l, list('id','visit'), 3, 4)
#' cast(l, 1:2, 3, list('value'))
#' 
#' ## cast assumes columns are id, time, followed by all varying:
#' cast(dat)
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
  l$idvar <- idvar
  l$v.names <- v.names
  l$data <- data
  l$timevar <- timevar
  l$times <- l$ids <- NULL
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
  l$varying <- varying
  l$times <- if (vl) n[varying[[1L]]] else seq_along(varying[[1L]])
  l$data <- data
  l$idvar <- '_id_'
  l$timevar <- if (vl) 'variable' else 'time'
  l$v.names <- if (vl) 'value' else paste0('value', 1:length(varying))
  ## also allow any melt defaults to be overridden, ie, direction = 'wide'
  ## but names must match exactly, ie, dir = 'wide' will not work
  # l <- l[!duplicated(names(l), fromLast = TRUE)]
  l <- modifyList(l, list(...))
  res <- do.call('reshape', l)
  res$'_id_' <- NULL
  `rownames<-`(res, NULL)
}

#' Install packages temporarily
#' 
#' This function will create a temporary \code{.libPath}, install, and load
#' packages for use in a single \code{R} session. \cr \cr To install a repo
#' from github temporarily, use \code{\link[devtools]{with_libpaths}}.
#' 
#' @param pkgs character vector of the names of packages whose current
#' versions should be downloaded from the repositories
#' @param lib character vector giving the library directories where to install
#' \code{pkgs}; recycled as needed; if missing (default), a 
#' \code{\link{tempdir}} will be created
#' @param ... additional arguments passed to 
#' \code{\link[utils]{install.packages}}
#' 
#' @examples
#' \dontrun{
#' install_temp(c('devtools','testthat'))
#' }
#' 
#' @export

install_temp <- function(pkgs, lib, ...) {
  if (missing(lib))
    lib <- tempdir()
  lp <- .libPaths()
  ## reset libPaths before restarting r session may not be desired
  # on.exit(.libPaths(lp))
  .libPaths(c(lp, lib))
  utils::install.packages(pkgs = pkgs, lib = lib, ...)
  for (ii in pkgs)
    require(ii, character.only = TRUE)
  invisible()
}

#' Merge nested lists
#' 
#' Recursive functions to merge nested lists.
#' 
#' \code{nestedmerge} recursively calls itself to merge similarly-structured
#' named \emph{or} unnamed lists. Unnamed lists results in a "horizontal" 
#' merge; named lists will be matched based on names. In either case, the
#' matching element (or list(s) of elements(s)) should also have the same
#' structure.
#' 
#' \code{nestedMerge} is a convenience wrapper for \code{nestedmerge} in cases
#' where list \code{a} contains elements not in list \code{b}. If using 
#' \code{nestedmerge} in this case, only elements of list \code{a} will be
#' merged and returned.
#' 
#' @param a,b lists
#' @seealso
#' Adapted from \url{http://stackoverflow.com/questions/23483421/combine-merge-lists-by-elements-names-list-in-list}
#' 
#' @examples
#' ## `l1` and `l2` have similar structures
#' l1 <- list(a = list(1:2, NULL), b = list(1:3, NULL), c = list(1:5))
#' l2 <- list(a = list(NULL, 0:1), b = list(NULL, 4:6))
#' l3 <- list(a = list(NULL, 0:1), b = list(4:6))
#' 
#' nestedMerge(l1, l2)
#' 
#' ## "fails" for `b` since `l1$b` and `l3$b` are not structured similarly
#' nestedMerge(l1, l3)
#' 
#' l1 <- list(integers = 1:3, letters = letters[1:3],
#'            words = c('two','strings'), rand = rnorm(5))
#' l2 <- list(letters = letters[24:26], booleans = c(TRUE, TRUE, FALSE),
#'            words = 'another', floating = c(1.2, 2.4),
#'            integers = 1:3 * 10)
#'            
#' nestedMerge(l1, l2)
#' 
#' ## compare to
#' nestedmerge(l1, l2)
#' 
#' @export

nestedMerge <- function(a, b) {
  if (is.list(a) & is.list(b)) {
    nn <- setdiff(names(b), names(a))
    a <- c(a, setNames(vector('list', length(nn)), nn))
  }
  nestedmerge(a, b)
}

#' @rdname nestedMerge
#' @export
nestedmerge <- function(a, b) {
  if (is.list(a) & is.list(b)) {
    out <- list()
    if (!is.null(names(a))) {
      for (n in names(a)) {
        if (n %in% names(b) && !is.null(b[[n]])) {
          out <- append(out, c(Recall(a[[n]], b[[n]])))
        } else {
          out <- append(out, list(a[[n]]))
        }
        names(out)[length(out)] <- n
      }
    } else {
      for (i in seq_along(a))
        if (i <= length(b) && !is.null(b[[i]])) {
          out <- append(out, Recall(a[[i]], b[[i]]))
        } else {
          out <- append(out, list(a[[i]]))
        }
    }
    return(out)
  } else return(list(c(a, b)))
}
