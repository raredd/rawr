### utilities
# lsp, %ni%, ht, oror, progress, recoder, psum, ident, search.df, search.hist, 
# ggcols, grcols, tcol, fapply, lss, rescaler, html.test, roundr, pvalr, intr, 
# show.colors, show.pch, %inside%, try_require, clist, binconr, num2char, 
# iprint, list2file, match_ctc, Reload, clc, clear, writeftable, helpExtract,
# Round, bind_all, interleave, outer2, merge2, locf, roll_fun, round2, .updateR
###


#' List package
#' 
#' List all exported and/or non exported objects in a package.
#' 
#' @param package package name, as \code{\link{name}} or literal character 
#' string
#' @param what what to get; \code{'all'} is default which returns all exported
#' and non exported functions in \code{package}; see details for more
#' @param pattern text pattern or regular expression passed to 
#' \code{\link{grep}} to filter results
#' 
#' @details
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

#' not in
#' 
#' Negation of \code{\link[base]{\%in\%}}
#' @name notin
#' @usage x \%ni\% table
#' @param x vector or \code{NULL}; the values to be matched
#' @param table vector or \code{NULL}; the values to be matched against
#' @aliases %ni%
#' @examples
#' 1:5 %ni% 3:5
#' @export

`%ni%` <- function(x, table) !(match(x, table, nomatch = 0) > 0)

#' head/tail
#' 
#' @param x an object
#' @param ... other parameters, such as \code{n}, passed to \code{\link{head}} 
#' or other methods
#' @param sep separator 
#' @export

ht <- function(x, ..., sep = NULL) rbind(head(x, ...), sep, tail(x, ...))

#' oror
#' 
#' \code{function_that_may_return_null()} \code{oror} default value
#' @name oror
#' @param e1,e2 raw or logical or "number-like" vectors or objects
#' @aliases %||%
#' @examples
#' \dontrun{
#' NULL || TRUE    # error
#' NULL %||% TRUE  # no error
#' }
#' @export

`%||%` <- function(e1, e2) if (!is.null(e1)) e1 else e2

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
  
  if (!is.numeric(value)) 
    stop("\'value\' must be numeric")
  if (missing(max.value)) {
    max.value <- 100
    percent <- TRUE
  } else percent <- FALSE
  
  erase.only <- value > max.value
  max.value <- as.character(round(max.value))
  l <- nchar(max.value)
  value <- formatC(round(value), width = l)
  f <- function(...) paste0(..., collapse = '')
  
  if (textbar) {
#     m <- getOption('width')
#     r <- floor(as.numeric(value) / as.numeric(max.value) * m)
#     backspaces <- f(rep('\b', m * 2))
#     if (erase.only) message <- ''
#     else {
#       message <- f('|', f(rep('=', max(0, r - 1))), 
#                    f(rep(' ', max(0, m - r))), '|')
#       cat(backspaces, message, sep = '')
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
  invisible(NULL)
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
#' Compute the pairwise sum of two or more vectors
#' 
#' @param ... string of vectors
#' @param na.rm logical; should missing values (including \code{NaN}) be 
#' omitted from calculations
#' 
#' @details Each vector passed in \code{...} must be equal in length. The 
#' function coerces the vectors into a matrix and performs 
#' \code{\link{rowSums}} on the resulting rows.
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
#' @return A single logical value, \code{TRUE} or \code{FALSE}, never \code{NA}
#' and never anything other than a single value.
#' 
#' @seealso \code{\link{identical}}; \code{\link{all.equal}} for 
#' descriptions of how two objects differ; \code{\link{Comparison}} for 
#' operators that generate elementwise comparisons; \code{\link{isTRUE}} is a 
#' simple wrapper based on \code{identical}.
#' 
#' @examples
#' ident(1, 1.)
#' ident(1, 1., as.integer(1))
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
  return(all(zzz))
}

#' Search function for data frames
#' 
#' Searches a data frame column for matches
#' 
#' @param pattern string to find
#' @param df data frame to search
#' @param col.name column name in \code{df} to search
#' @param var variation; maximum distance allowed for a match; see 
#' \code{\link{agrep}}
#' @param ignore.case logical; if \code{FALSE}, the pattern matching is 
#' \emph{case-sensitive}, and if \code{TRUE}, case is ignored during matching
#' @param ... other arguments passed to \code{\link{agrep}}
#' @return Subset of the original \code{df} where the \code{pattern} was
#' found in the specificed \code{col.name}
#' 
#' @examples
#' df <- data.frame(islands = names(islands)[1:32], mtcars)
#' search.df(New, df, islands)
#' search.df(ho, df, islands, var = 0.2) # too much variation
#' search.df(ho, df, islands, var = 0)
#' search.df('Axel Hieberg', df, islands) # misspelled, not enough variation
#' search.df('Axel Hieberg', df, islands, var = 2)
#' search.df(19, df, mpg)
#' 
#' @export

search.df <- function(pattern, df, col.name, var = 0, ignore.case = TRUE, ...) {
  
  tmp1 <- as.character(substitute(pattern))
  tmp2 <- as.character(substitute(col.name))
  FIND <- agrep(tmp1, df[ , tmp2],
                ignore.case = ignore.case, max.distance = var, ...)
  df[c(FIND), ]
}

#' Search history
#' 
#' Searches Rhistory file for pattern matches
#' 
#' @param x numeric or character; if numeric, shows the most recent \code{n} 
#' lines in \code{.Rhistory}; if character, searches for pattern matches
#' @param ... additional arguments passed to \code{\link{grep}}
#' 
#' @return Returns a list of recent commands that match \code{pattern}
#' 
#' @examples
#' \dontrun{
#' search.hist()
#' search.hist(25)
#' search.hist('?')
#' search.hist('?', fixed = TRUE)
#' search.hist('\\?')
#' }
#' 
#' @export

search.hist <- function (x, ...) {
  
  hist <- tryCatch(readLines('.Rhistory'),
                   warning = function(w) message("No history found"),
                   finally = return(invisible()))
  lhist <- length(hist)
  
  if (is.numeric(x))
    return(hist[lhist:(lhist - x + 1)])
  if (is.character(x))
    return(grep(x, readLines(".Rhistory"), value = TRUE, ...))
}

#' ggplot colors
#' 
#' A function to replicate default \code{\link[ggplot2]{ggplot}} colors
#' 
#' @param n number of colors
#' @param c the chroma of the color; the upper bound for chroma depends on hue
#' and luminance
#' @param l a value in the range \code{[0, 100]} giving the luminance of the 
#' color; for a given combination of hue and chroma, only a subset of this 
#' range is possible
#' @seealso \code{\link{hcl}}
#' 
#' @examples
#' plot(rnorm(1000), col = ggcols(1000), pch = 19)
#' 
#' @export

ggcols <- function(n, l = 65, c = 100) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = l, c = c)[1:n]
}

#' Choose n colors using the golden ratio
#'
#' This chooses \code{n} colour hues using a sequence generated by the Golden
#' Ratio.
#'
#' @param n number of colors
#' @param s,v numeric vectors of values in the range \code{[0, 1]} for 
#' "saturation" and "value," respectively, to be combined to form a vector of
#' colors; values in shorter arguments are recycled
#' @param alpha  numeric vector of values in the range \code{[0, 1]} for alpha 
#' transparency channel (0 is transparent and 1 is opaque)
#' @seealso \code{\link{hsv}}
#' 
#' @examples
#' plot(1:5, 1:5, col = grcols(5), pch = 20, cex = 3)
#' 
#' plot(c(1, 6), c(0, 1), type = 'n', axes = FALSE, 
#'      bty = 'n', xlab = '', ylab = '')
#' rect(1:5, 0, 2:6, 1, col = grcols(5), border = NA)
#' 
#' @export

grcols <- function(n, s = .5, v = 1, alpha = 1) {
  GR <- 2 / (1 + sqrt(5))
  hues <- (seq(0, n - 1) * GR) %% 1
  hsv(hues, s = s, v = v, alpha = alpha)
}

#' Transparent colors
#' 
#' Add transparency to colors.
#' 
#' @param color single or string of color names (or hexadecimal format) 
#' @param trans transparency defined as an integer in the range 
#' \code{[0, 255]} where \code{0} is fully transparent and \code{255} is fully
#' visible; see details
#' 
#' @details This is a vectorized function to add transparency to colors. 
#' \code{color} and \code{trans} must either be the same length or one of the 
#' two must have length one. 
#' 
#' The function adds integers (in hex) between 0 (fully transparent) and 255
#' (fully visible) to the color(s) given. \code{color} values are converted to
#' RGB with transparency.
#' 
#' @seealso \code{\link{as.hexmode}}, \code{\link{col2rgb}}, 
#' \code{\link{adjustcolor}}
#' 
#' @examples
#' cols <- c('red','green','blue')
#' 
#' # a normal plot
#' plot(rnorm(100), col = tcol(cols), pch = 16, cex = 4)
#' 
#' # more transparent
#' plot(rnorm(100), col = tcol(cols, 100), pch = 16, cex = 4)
#' 
#' # hexadecimal colors also work
#' cols <- c('#FF0000','#00FF00','#0000FF')
#' plot(rnorm(100), col = tcol(cols, c(50, 100, 255)), pch= 16, cex = 4)
#' 
#' @export

tcol <- function(color, trans = 255) {
  
  if (length(color) != length(trans) & 
        !any(c(length(color), length(trans)) == 1)) 
    stop('Vector lengths not correct')
  if (length(color) == 1 & length(trans) > 1) 
    color <- rep(color, length(trans))
  if (length(trans) == 1 & length(color) > 1) 
    trans <- rep(trans, length(color))
  
  res <- paste0('#', apply(apply(rbind(col2rgb(color)), 2, function(x) 
    format(as.hexmode(x), 2)), 2, paste, collapse = ''))
  res <- unlist(unname(Map(paste0, res, as.character(as.hexmode(trans)))))
  res[is.na(color)] <- NA
  return(res)
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
#'      paste0('(', paste0(quantile(x, c(.025, .975)), collapse = ', '), ')')
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
#' @param head logical; if \code{TRUE}, only shows first \code{n} objects of 
#' output
#' @param n number of objects to displace if \code{head} is \code{TRUE}
#' 
#' @seealso \code{\link{ls}}, \code{\link{ls.str}}, \code{\link{objects}}
#' 
#' @examples
#' lss()
#' \dontrun{
#' a <- rnorm(100000)
#' b <- matrix(1, 1000, 100)
#' lss()
#' }
#' 
#' @export

lss <- function (pos = 1, pattern, by = NULL, all.names = FALSE,
                 decreasing = TRUE, head = TRUE, n = 15) {
  
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
  if (head)
    out <- head(out, n)
  out
}

#' Rescale numeric vector
#' 
#' Rescale a numeric vector to have specified maximum and minimum; shamelessly
#' stolen from hadley's \code{scales} package
#' 
#' @param x numeric vector of values
#' @param to output range (numeric vector of length two)
#' @param from input range (numeric vector of length two); if not given, 
#' \code{from} is calculated from the range of \code{x}
#' 
#' @seealso \code{\link[scales]{rescale}}; \code{\link[scales]{zero_range}}
#' @examples
#' rescaler(1:100)
#' rescaler(runif(10))
#' rescaler(1)
#' 
#' @export

rescaler <- function (x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
  
  zero_range <- function (x, tol = .Machine$double.eps * 100) {
    if (length(x) == 1) 
      return(TRUE)
    if (length(x) != 2) 
      stop('x must be length one or two')
    if (any(is.na(x))) 
      return(NA)
    if (x[1] == x[2]) 
      return(TRUE)
    if (all(is.infinite(x))) 
      return(FALSE)
    m <- min(abs(x))
    if (m == 0) 
      return(FALSE)
    abs((x[1] - x[2]) / m) < tol
  }
  
  if (zero_range(from) || zero_range(to)) 
    return(rep(mean(to), length(x)))
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

#' html test
#' 
#' Render html in rstudio viewer
#' 
#' @param x character string of html code
#' 
#' @examples
#' \dontrun{
#' html.test("
#' <div align = center><h1>A heading<sup>&dagger;</sup><h1></div>
#' <font size = 1><sup>&dagger;</sup>That was the heading</font>
#' ")
#' 
#' library(Gmisc)
#' html.test(htmlTable(mtcars, output = FALSE))
#' }
#' 
#' @export

html.test <- function(x) {
  htmlFile <- tempfile(fileext = '.html')
  writeLines(x, con = htmlFile)
  if ((Sys.getenv('RSTUDIO') != '') && ('rstudio' %in% .packages(TRUE)))
      rstudio::viewer(htmlFile)
  else browseURL(htmlFile)
}

#  roundr
#'
#' Improved rounding formatter
#'
#' @param x numeric value, vector, matrix, or data frame
#' @param digits number of digits past the decimal point to keep
#'
#' @details 
#' Uses \code{\link[base]{sprintf}} to round numeric value, retaining extra 0s.
#' @return
#' A vector of character strings.
#' @seealso \code{\link[base]{round}}; \code{\link[base]{sprintf}}
#'
#' @examples
#' roundr(51.01, 3)
#' roundr(0.199, 2)
#' 
#' ## useful for dropping the negative in case 1:
#' roundr(c(-0.0002, 0.0002, 0.5, -0.5, -0.002), digits = 3)
#' 
#' roundr(matrix(1:9, 3), 2)
#' 
#' @export

roundr <- function(x, digits = 1) UseMethod('roundr')

#' @rdname roundr
#' @export

roundr.default <- function(x, digits = 1) {
  mode.ok <- vapply(x, function(x) is.numeric(x) || is.complex(x), NA)
  if (!all(mode.ok))
    stop('non-numeric argument to mathematical function')
  res <- sprintf(paste0('%.', digits, 'f'), x)
  zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
  res[res == paste0('-', zzz)] <- zzz
  res
}

#' @rdname roundr
#' @export

roundr.data.frame <- function(x, digits = 1) {
  mode.ok <- vapply(x, function(x) is.numeric(x) || is.complex(x), NA)
  if (!all(mode.ok))
    stop(paste0('non-numeric variable in data frame, ', deparse(substitute(x))))
  else
    do.call(data.frame, lapply(x, roundr, digits))
}

#' @rdname roundr
#' @export

roundr.matrix <- function(x, digits = 1) {
  mode.ok <- vapply(x, function(x) is.numeric(x) || is.complex(x), NA)
  if (!all(mode.ok))
    stop(paste0('non-numeric variable in matrix, ', deparse(substitute(x))))
  else
    apply(x, 2, roundr, digits)
}

#' Confidence interval formatter
#' 
#' Automatically calculate summary statistic and interval for numeric vectors
#' 
#' @param ... numeric vector or string of numeric vectors
#' @param fun summary stat function, usually \code{\link{mean}} or 
#' \code{\link{median}}
#' @param conf width of confidence interval in \code{[0,1]}; if \code{NULL} 
#' (default), returns min and max of \code{...}
#' @param digits number of digits (includes trailing 0s)
#' @param na.rm logical; if \code{TRUE}, any \code{\link{NA}} and \code{NaN}
#' are removed from \code{...} before \code{fun} and \code{\link{quantile}} are
#' computed
#' 
#' @seealso \code{\link[rawr]{roundr}}
#' 
#' @examples
#' intr(1:10)
#' intr(1:10, conf = .95)
#' # inner quartile range
#' `colnames<-`(cbind(lapply(mtcars, intr, conf = .5), 
#'                    lapply(mtcars, intr, fun = mean)),
#'              c('median (IQR)','mean (range)'))
#' # compare to
#' summary(mtcars)
#' 
#' @export

intr <- function(..., fun = median, conf = NULL, digits = 2, na.rm = FALSE) {
  
  lst <- list(...)
  if (is.null(conf) || conf == 0 || 
        findInterval(conf, c(0, 1), rightmost.closed = FALSE) != 1)
    conf <- 1
  
  sapply(lst, function(x) {
    bounds <- quantile(x, c((1 - conf) / 2 * c(1, -1) + c(0, 1)), na.rm = na.rm)
    bounds <- roundr(bounds, digits = digits)
    val <- roundr(fun(x, na.rm = na.rm), digits = digits)
    
    if (! conf %in% c(0, 1))
      sprintf('%s (%s%% CI: %s - %s)', val, conf * 100, bounds[1], bounds[2])
    else
      sprintf('%s (%s - %s)', val, bounds[1], bounds[2])
  })
}

#' p-value formatter
#' 
#' Formats several cases of p-values; see details.
#' 
#' @param pvals numeric value or vector of p-values
#' @param sig.limit lower bound for precision
#' @param digits integer; number of decimal places; see also 
#' \code{\link[rawr]{roundr}}
#' @param html logical; if \code{TRUE}, uses \code{&lt;} instead of \code{<}
#' @param show.p logical; if \code{TRUE}, inserts \code{p = } or \code{p < }
#' where appropriate
#' 
#' @details
#' This function will deal with several cases of common p-values: 1) p-values
#' which are > 0.1 will be rounded to two decimal places (and keep any trailing
#' 0s) since we are not usually interested in precision of insignificant
#' values; 2) p-values which are less than the specified \code{sig.level} will
#' be formatted as \code{< 0.001}, for example; 3) p-values that are less than
#' 0.01 but greater than \code{sig.level} will be precise to \code{digits} and
#' keep any trailing 0s.
#' 
#' @seealso \code{\link[rawr]{roundr}}
#' 
#' @examples
#' pvals <- c(.13354, .060123, .004233, .00000016223)
#' pvalr(pvals, digits = 3)
#' pvalr(pvals, digits = 3, show.p = TRUE)
#' 
#' @export

pvalr <- function(pvals, sig.limit = .001, digits = 3, html = FALSE, 
                  show.p = FALSE) {
  sapply(pvals, function(x, sig.limit) {
    if (is.na(x))
      return(NA)
    if (x >= 1)
      return('1')
    if (x < sig.limit) {
      if (show.p) p <- 'p ' else p <- ''
      if (html)
        return(sprintf('%s&lt; %s', p, format(sig.limit))) else
          return(sprintf('%s< %s', p, format(sig.limit)))
    } else {
      if (show.p) p <- 'p = ' else p <- ''
      if (x > .1)
        return(sprintf('%s%s', p, roundr(x, digits = 2))) else
          return(sprintf('%s%s', p, roundr(x, digits = digits)))
    }
  }, sig.limit = sig.limit)
}

#' Show colors
#' 
#' In \code{R}, there are 657 named colors. This function shows these colors 
#' and their respective numbers. Find a color by number in the plot or find the
#' name of the color with \code{colors()[n]}
#' 
#' @seealso \code{\link{show.pch}}
#' 
#' @examples
#' show.colors()
#' colors()[81]
#' # [1] "darkgreen"
#' @export

show.colors <- function() {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(1,1),
      mai=c(.4,.4,.4,.4),
      oma=c(.2,0,0,.2))
  x <- 22
  y <- 30
  plot(c(-1, x), c(-1, y), xlab = '', ylab = '', type = 'n', xaxt = 'n', 
       yaxt = 'n', bty = 'n')
  sapply(1:x, function(i) {
    sapply(1:y, function(j) {
      k <- y * (i - 1) + j
      co <- colors()[k]
      rect(i - 1, j - 1, i, j, col = co, border = grey(.5))
    })
  })
  text(rep(-.5, y), (1:y) - .5, 1:y, cex = 1.2 - .016 * y)
  text((1:x) - .5, rep(-.5, x), y * (0:(x - 1)), cex = 1.2 - .022 * x)
  title('col = colors()[n]')
}

#' Show plotting characters
#' 
#' In \code{R}, there are 26 numeric plotting characters. This function shows 
#' these options and their respective numbers. Note that \code{col} specifies
#' both the border and fill color (if applicable) for \code{0:20}; \code{pch}s
#' \code{21:25} can be filled with \code{bg}.
#' 
#' @seealso \code{\link{show.colors}}
#' 
#' @examples
#' show.pch()
#' @export

show.pch <- function() {
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(xpd = TRUE,
      mfrow = c(1, 1),
      mai = c(.4,.4,.4,.4),
      oma = c(.2,0,0,.2))
  x <- rep(1:5, 6)[1:26]
  y <- c(rep(5:1, each = 5)[1:25], 0)
  plot(x, y, pch = 0:25, axes = FALSE, bg = 'gray', cex = 2, col = 'red')
  text(x = x, y = y, labels = 0:25, pos = 4, cex = 1.5, offset = 1)
  text(x = 4, y = 0, labels = 'plotting characters 0:25', cex = 1.5)
}

#' Inside
#' 
#' Return logical vector indicating if \code{x} is inside the \code{interval}
#' @name inside
#' @usage x \%inside\% interval
#' @param x numeric
#' @param interval numeric interval
#' @aliases %inside%
#' @examples
#' -5:5 %inside% c(0,5)
#' @export

`%inside%` <- function(x, interval) x >= interval[1] & x <= interval[2]

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
    stop(paste(package, collapse = ', '), ' package not found.', call. = FALSE)
}

#' Concatenate a named list for output
#' 
#' Print a \code{list(a = 1, b = 2)} as \code{(a = 1, b = 2)}
#'
#' @param l list to concatenate
#' 
#' @examples
#' clist(list(a = 1, b = 2))
#' clist(par()[1:5])
#' 
#' @export

clist <- function(l) 
  paste(paste(names(l), l, sep = ' = ', collapse = ', '), sep = '')

#' binconr
#' 
#' Binomial confidence interval formatter.
#' 
#' @param r number of responses (successes)
#' @param n number of observations (trials)
#' @param conf level of confidence
#' @param digits number of digits
#' @param est logical; if \code{TRUE}, includes the point estimate
#' @param method method to use; see \code{\link{bincon}}
#' 
#' @seealso \code{\link{bincon}}; \code{\link[Hmisc]{binconf}}
#' 
#' @examples
#' binconr(5, 10, .90, est = FALSE)
#' binconr(45, 53, .95, digits = 1)
#' 
#' @export

binconr <- function(r, n, conf = 0.95, digits = 2, 
                    est = TRUE, method = 'exact') {
  res <- roundr(bincon(r, n, alpha = 1 - conf, method = method) * 100, 
                digits = digits)
  zzz <- paste0('(', conf * 100, '% CI: ', res[4], ' - ', res[5], ')')
  if (est) 
    zzz <- paste0(res[3], ' ', zzz)
  return(zzz)
}

#' Numeric to character string
#' 
#' Convert a number to a character string.
#' 
#' @param num number; integer value in \code{(-1e08, 1e08)}
#' @param informal logical; if \code{TRUE}, adds "and" before tens or ones
#' @param cap logical; if \code{TRUE}, capitalizes the first word
#' 
#' @details
#' Whole numbers twenty-one through ninety-nine are hyphenated when they are 
#' written out whether used alone or as part of a larger number; for example: 
#' twenty-one. Whole numbers are also hyphenated when they are part of larger 
#' numbers that are written out - but not other parts of large numbers; for 
#' example, 5,264 is written "five thousand two hundred sixty-four." The rule 
#' applies only to two-word numbers; for example, 603 is written out, e.g., 
#' "six hundred three" (formal) or "six hundred and three" (informal). A whole 
#' number followed by hundred, thousand, etc., would be written as, for 
#' example, "one hundred," and not hyphenated. In a phrase like "one hundred 
#' and ten years," no hyphenation should be added. 
#' 
#' @references \url{http://dictionary.reference.com/help/faq/language/g80.html}
#' 
#' @examples
#' num2char(19401, informal = TRUE)
#' 
#' v_num2char <- Vectorize(num2char)
#' nums <- c(-1000, 100, 10000, 3922, 3012, 201, -152, 1002, 90765432)
#' v_num2char(nums)
#' 
#' @export

num2char <- function(num, informal = FALSE, cap = TRUE) {
  
  if (num == 0) {if (cap) return('Zero') else return('zero')}
  neg <- FALSE
  if (num < 0) {neg <- TRUE; num <- abs(num)}
  if (!num %inside% c(1, 99999999)) 
    stop("I can't count that high")
  #   `%inside%` <- function(x, interval) # don't need as.numeric
  #     as.numeric(x) >= interval[1] & as.numeric(x) <= interval[2]
  ## helpers
  key <- c('0'='','1'='one','2'='two','3'='three','4'='four','5'='five',
           '6'='six','7'='seven','8'='eight','9'='nine','10'='ten',
           '11'='eleven','12'='twelve','13'='thirteen','14'='fourteen',
           '15'='fifteen','16'='sixteen','17'='seventeen','18'='eighteen',
           '19'='nineteen','20'='twenty','30'='thirty','40'='forty','50'='fifty',
           '60'='sixty','70'='seventy','80'='eighty','90'='ninety',
           '100'='hundred','1000'='thousand','1000000'='million')
  upcase <- function(x)
    paste(toupper(substr(x, 1, 1)), substring(x, 2), sep = '', collapse = ' ')
  f1 <- function(x, informal = informal) { # for 1-99
    x <- as.numeric(x) # if string with leading 0s is passed
    z <- paste0(' and ',
                if (x %inside% c(21, 99) && (x %ni% seq(30, 100, 10)))
                  paste(key[as.character(as.numeric(substr(x, 1, 1)) * 10)], 
                        key[substr(x, 2, 2)], sep = '-')
                else 
                  key[as.character(as.numeric(x))])
    if (!informal) gsub(' and ', '', z) else z
  }
  f2 <- function(x, informal = informal) {
    x <- as.numeric(x) # if string with leading 0s is passed
    if (x %inside% c(100, 999))
      paste0(key[substr(x, 1, 1)], ' hundred ', f1(substr(x, 2, 3), informal))
    else f1(x, informal = informal)
  }
  f3 <- function(x, informal = informal) {
    x <- as.numeric(x) # if string with leading 0s is passed
    if (x %inside% c(1000, 9999))
      paste0(key[substr(x, 1, 1)], ' thousand ', f2(substr(x, 2, 4), informal))
    else f2(x, informal = informal)
  }
  f4 <- function(x, informal = informal) {
    x <- as.numeric(x) # if string with leading 0s is passed
    if (x %inside% c(10000, 99999))
      paste0(f1(substr(x, 1, 2), FALSE), ' thousand ', 
             f2(substr(x, 3, 5), informal)) 
    else f3(x, informal = informal)
  }
  f5 <- function(x, informal = informal) {
    x <- as.numeric(x) # if string with leading 0s is passed
    if (x %inside% c(100000, 999999))
      paste0(f2(substr(x, 1, 3), FALSE), ' thousand ', 
             f2(substr(x, 4, 6), informal)) 
    else f4(x, informal = informal)
  }
  f6 <- function(x, informal = informal) {
    x <- as.numeric(x) # if string with leading 0s is passed
    if (x %inside% c(1000000, 9999999))
      paste0(key[substr(x, 1, 1)], ' million ', f5(substr(x, 2, 7), informal))
    else f5(x, informal = informal)
  }
  f <- function(x, informal = informal) {
    x <- as.numeric(x) # if string with leading 0s is passed
    if (x %inside% c(10000000, 99999999))
      paste0(f1(substr(x, 1, 2), FALSE), ' million ',
             f5(substr(x, 3, 8), informal))
    else f6(x, informal = informal)
  }
  ## trim leading/trailing whitespace
  zzz <- gsub('^\\s+|\\s+$', '', f(num, informal = informal))
  ## trim double whitespace
  zzz <- upcase(gsub('\\s{2,}', ' ', zzz))
  ## trim ands in special cases
  zzz <- ifelse(cap, upcase(gsub('And\ |\ and*$', '', zzz)),
                gsub('And\ |\ and*$', '', zzz))
  zzz <- ifelse(neg, paste0('negative ', tolower(zzz)), tolower(zzz))
  return(ifelse(cap, upcase(zzz), zzz))
}

#' In-line printing
#' 
#' Modified \code{\link[pander]{p}} function from the \code{pander} package.
#' 
#' @param ... one or more numeric or character elements to be converted into
#' character vectors
#' @param wrap character string to wrap each term
#' @param sep character string to separate the terms
#' @param copula character string to separate last two terms
#' @param digits number of digits past the decimal point to keep; see 
#' \code{\link{roundr}}
#' 
#' @seealso \code{\link{roundr}}, \code{\link[pander]{p}}
#' 
#' @examples
#' iprint('fee','fi','fo','fum')
#' iprint(rnorm(2))
#' iprint(-0.000, 0.100)
#' iprint(LETTERS[1:5], copula = ", and the letter ")
#' iprint("Thelma", "Louise", copula = " & ")
#' 
#' @export

iprint <- function (..., wrap, sep, copula, digits = 2) {
  
  x <- c(...)
  len <- length(x)
  f <- function(x, wrap = '"') 
    sprintf('%s%s%s', wrap, x, wrap)
  
  if (len == 0) 
    return('')
  if (missing(wrap))
    wrap <- ''
  if (missing(sep))
    sep <- ', '
  if (missing(copula))
    copula <- ifelse(len == 2, ' and ', ', and ')
  
  if (is.numeric(x))
    x <- roundr(x, digits = digits)
  
  if (len == 1) 
    f(x, wrap)
  else if (len == 2) 
    paste(f(x, wrap), collapse = copula)
  else paste0(paste(f(head(x, -1), wrap = wrap), collapse = sep), 
              copula, f(tail(x, 1), wrap = wrap))
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
  return(invisible())
}

#' Match CTCAE codes
#' 
#' Convenience function to convert CTCAE (version 3 or 4) toxicity codes or
#' descriptions with their appropriate matches. Especially useful in data from
#' paper trials where only the toxicity codes are reported excluding (the more
#' meaningful) descriptions.
#' 
#' @param ... character string(s) of toxicity codes (usually of the form 
#' \code{AB123} but can handle \code{AB-123} or \code{AB 123}) or keyword(s)
#' to be matched in the toxicity description
#' @param version version number; default is 
#' \href{http://www.dfhcc.harvard.edu/fileadmin/DFHCC_Admin/Clinical_Trials/QACT/Policies_and_Procedures/CTCToxVersion4.pdf}{CTCAE v4}
#' 
#' @return
#' A list containing:
#' 
#' \item{\code{matches}}{a data frame of matches with toxicity codes and their
#' respective descriptions and categories}
#' \item{\code{version}}{CTCAE version used}
#' 
#' @examples
#' codes <- sample(ctcae_v4$tox_code, 10)
#' match_ctc(codes)
#' 
#' match_ctc('injury', version = 3)
#' match_ctc('aortic','arterial')
#' 
#' @export

match_ctc <- function(..., version = 4) {
  
  x <- c(...)
  if (version %ni% 3:4)
    stop('CTCAE version should be 3 or 4')
  else if (version == 3)
    dat <- ctcae_v3
  else dat <- ctcae_v4
  
  if (any(grepl('([A-Za-z -])([0-9])', x))) {
    idx <- 'tox_code'
    x <- gsub('\\s*|-', '', x)
  } else idx <- 'tox_desc'
  
  idx <- grep(paste(x, collapse = '|'), dat[ , idx], ignore.case = TRUE)
  
  return(list(matches = dat[idx, ],
              version = sprintf('CTCAE v%s', version)))
}

#' Reload
#' 
#' Emulate starting a fresh \code{R} session.
#' 
#' @param ... ignored
#' 
#' @export

Reload <- function(...) {
  ## clean (rstudio) r session packages:
  pkgs <- c(".GlobalEnv", "tools:rstudio", "package:stats", "package:graphics",
            "package:grDevices", "package:utils", "package:datasets",
            "package:methods", "Autoloads", "package:base")
  to_unload <- setdiff(search(), pkgs)
  for (pkg in to_unload)
    try(detach(pkg, unload = TRUE, character.only = TRUE), silent = TRUE)
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  cat('\014')
  invisible(NULL)
}

#' clc
#' 
#' Clear the workspace
#' 
#' @param all logical; if \code{TRUE}, also removes hidden files
#' 
#' @export

clc <- function(all = FALSE)
  rm(list = ls(.GlobalEnv, all.names = all), envir = .GlobalEnv)

#' clear
#' 
#' Clear the console window
#' 
#' @param ... ignored
#' 
#' @export

clear <- function(...) cat('\014')

#' Write ftable
#' 
#' \code{\link{ftable}}s can look nice, but are only \code{\link{cat}}'d to 
#' the console and, thus, not easily used (or manipulated). 
#' \code{\link{write.ftable}} does not write \code{ftable}s as they print,
#' so here we are.
#' 
#' @param x an object of class \code{ftable}
#' @param quote logical; if \code{TRUE}, strings will be surrounded by double
#' quotes
#' @param digits integer giving the number of significant digits to use for
#' the cell entries of \code{x}
#' @param ... additional parameters passed to \code{\link{format.ftable}}
#' 
#' @return
#' A matrix formatted as \code{ftable} would print.
#' 
#' @examples
#' x <- ftable(Titanic, row.vars = 1:3)
#' writeftable(x)
#' 
#' @export

writeftable <- function (x, quote = FALSE, digits = getOption('digits'), ...) {
  if (!inherits(x, 'ftable'))
    stop('x must be an ftable object')
  x <- format(x, quote = quote, digits = digits, ...)
  as.matrix(x)
}

#' Extract R help files
#' 
#' Extracts specified portions of R help files (from \emph{loaded} libraries)
#' for use in Sweave or R-markdown documents.
#' 
#' @param f a function
#' @param show.sections logical; if \code{TRUE}, returns \code{section} options
#' for \code{f}
#' @param section section to extract (default is \code{"Usage"}
#' @param type type of character vector you want returned; deefaults is 
#' \code{"m_code"}, see details
#' @param ... additional arguments passed to \code{utils:::.getHelpFile}
#' 
#' @details
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
#' @return 
#' A character vector to be used in a Sweave or R-markdown document.
#' 
#' @examples
#' 
#' cat(helpExtract(print), sep = "\n")
#' 
#' cat(helpExtract(print, type = 'm_text'))
#' 
#' cat(helpExtract(print, type = 'm_text', section = 'description'))
#' 
#' @export

helpExtract <- function(f, show.sections = FALSE, section = 'Usage', 
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
  
  A <- deparse(substitute(f))
  x <- capture.output(tools::Rd2txt(getHelpFile(utils::help(A, ...)),
                                     options = list(sectionIndent = 0)))
  ## section start lines
  B <- grep('^_', x)
  ## remove "_\b"
  x <- gsub('_\b', '', x, fixed = TRUE)
  if (show.sections)
    return(gsub(':','', x[B]))
  X <- rep(FALSE, length(x))
  X[B] <- 1
  out <- split(x, cumsum(X))
  out <- out[[which(sapply(out, function(x) 
    grepl(section, x[1], fixed = F, ignore.case = TRUE)))]][-c(1, 2)]
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

#' Round to specified target
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
    Round(x, target)
  } else {
    select <- seq_along(x)[diff.x != 0]
    wh <- which.min(diff.x[select])
    x[select[wh]] <- r.x[select[wh]] + 1
    Round(x, target)
  }
}

#' Bind objects with unequal number of rows or columns
#' 
#' Bind objects with unequal number of rows or columns.
#' 
#' @param ... vectors
#' @param which joining method; \code{'rbind'} or \code{'cbind'}
#' 
#' @examples
#' bind_all(1:5, 1:3, which = 'cbind')
#' bind_all(1:5, 1:3, which = 'rbind')
#' 
#' @export

bind_all <- function(..., which) {
  if (missing(which))
    stop('specify which: \'rbind\' or \'cbind\'')
  l <- list(...)
  l <- lapply(l, `length<-`, max(sapply(l, length)))
  return(do.call(which, l))
}

#' Interleave rows or columns
#' 
#' Interleave rows (or columns) of vectors, matrices, or data frames.
#' 
#' @param ... vectors, matrices, or data frames
#' @param which joining method to use (\code{'rbind'} or \code{'cbind'}) when
#' \code{...} are matrices or data frames
#' 
#' @examples
#' interleave(letters[1:3], LETTERS[3:1], letters[26:24])
#' interleave(t(matrix(1:9, 3, 3)), t(matrix(letters[1:9], 3, 3)), which = 'rbind')
#' interleave(matrix(1:9, 3, 3), matrix(letters[1:9], 3, 3), which = 'cbind')
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
#' Use \code{\link{merge}} to join \code{n} data frames
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
#' d <- matrix(c(1:5, rnorm(5)), nrow = 5, dimnames = list(NULL, c('id', 'd')))
#' 
#' merge2(list(a, b, c))
#' merge2(list(a, b, c), all = TRUE)
#' merge2(list(a, b, c, d), all = TRUE)
#' 
#' @export

merge2 <- function(l, ...) Reduce(function(x, y) merge(x, y, ...), l)

#' Tabler
#' 
#' Extracts coefficients, standard errors, odds ratios, confidence intervals, 
#' p-values, etc. from model fits.
#' 
#' @param x an object of class \code{\link{lm}}, \code{\link{glm}},
#' \code{\link{survfit}}
#' @param digits number of digits printed
#' @param level confidence level; default is \code{0.95}
#' @param type use \code{"or"} for odds ratios; others may be added later
#' @param ... additional parameters passed to other methods
#' 
#' @examples
#' lmfit <- lm(mpg ~ hp + disp + wt, data = mtcars)
#' tabler(lmfit)
#' 
#' glmfit <- glm(vs ~ drat + factor(gear), data = mtcars, family = 'binomial')
#' tabler(glmfit, type = 'or')
#' 
#' \donttest{
#' library(survival)
#' sfit <- survfit(Surv(time, status) ~ 1, data = cancer, conf.int = 0.9)
#' tabler(sfit)
#' }
#' 
#' @export

tabler <- function(x, ...) UseMethod('tabler')

#' @rdname tabler
#' @export

tabler.default <- function(x, ...) summary(x, ...)

#' @rdname tabler
#' @export

tabler.lm <- function(x, digits = 3, ...) {
  res <- round(summary(x, ...)$coefficients, digits = digits)
  return(res)
}

#' @rdname tabler
#' @export

tabler.glm <- function(x, digits = 3, level = 0.95, type = '', ...) {
  res <- summary(x, ...)$coefficients
  if (tolower(type) == 'or') {
    res <- round(cbind(exp(cbind(coef(x), confint(x, level = level))),
                       res[, 4]), digits)
    res <- setNames(as.data.frame(res), 
                    c('Odds Ratio', sprintf('L%s%% CI', level * 100), 
                      sprintf('U%s%% CI', level * 100), 'Pr(>|z)'))
  }
  return(res)
}

#' @rdname tabler
#' @export

tabler.survfit <- function(x, ...) {
  surv_table(x, ...)
}

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

#' Round to nearest
#' 
#' Round numerics to nearest multiple of \code{to}.
#' 
#' @param x a numeric vector
#' @param to nearest fraction or integer
#' 
#' @examples
#' round2(mtcars$mpg, 5)
#' round2(mtcars$mpg, .5)
#' 
#' @export

round2 <- function(x, to = 1) round(x / to) * to

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
#' @aliases updateR

updateR <- function(update = TRUE) {
  path <- file.path(R.home(), '..', '..')
  v <- tail(sort(list.files(path, pattern = '^\\d{1}.\\d{1}$')), 2)
  if (!grepl(v[2], .libPaths()))
    stop("A more recent version of R was found on your system")
  if (file.exists(v_last <- sub(v[2], v[1], .libPaths()))) {
    pkg <- list.files(.libPaths())
    pkg <- setdiff(list.files(v_last), pkg)
    if (length(pkg) > 0) {
      cat(sprintf("Copying %s package%s to %s", length(pkg),
                  ifelse(length(pkg) > 1, 's', ''), .libPaths()))
      file.copy(file.path(v_last, pkg), .libPaths(), recursive = TRUE)
    } else cat("No packages to copy")
  }
  if (update) {
    if ((up <- table(packageStatus()$inst$Status)['upgrade']) > 0) {
      cat(sprintf("%s packages are being updated", up))
      update.packages(ask = FALSE)
    } else cat("All packages are up-to-date")
  }
}
