### utilities
# lsp, %ni%, ht, oror, progress, recoder, psum, ident, search.df, search.hist, 
# ggcols, grcols, tcol, fapply, lss, rescaler, html.test, roundr, pvalr, intr, 
# show.colors, show.pch, %inside%, try_require, clist, binconr, num2char, 
# iprint, list2file, match_ctc, clc, clear, writeftable, helpExtract, Round,
# bind_all, interleave
###

#' List package
#' 
#' List all exported and/or non exported objects in a package.
#' 
#' @usage
#' lsp(package, what = 'all')
#' 
#' @param package package name
#' @param what what to get; \code{'all'} is default which returns all exported
#' and non exported functions in \code{package}; see details for more
#' 
#' @details
#' This is a helper/wrapper function to easily list exported (\code{?'::'}) and
#' non exported (\code{?':::'}) functions (and other features from a package's 
#' \code{NAMESPACE} file). Note that \code{base} and older packages do not have
#' a \code{'NAMESPACE'} file in which case, for \code{base} packages, 
#' \code{lsp} returns \code{ls(.BaseNamespaceEnv, all.names = TRUE)}, and for
#' older packages with no \code{NAMESPACE} will throw an error.
#' 
#' Possible values for \code{what} are \code{'all'} (default), \code{NULL},
#' \code{'exports'}, \code{'imports'}, \code{'dynlibs'}, \code{'lazydata'}, 
#' \code{'path'}, \code{'S3methods'}, \code{'spec'}, and others depending on 
#' the package.
#' 
#' \code{lsp(packagename, '?')} to see options for a specific package.
#' 
#' \code{lsp(packagename, NULL)} to return all information.
#' 
#' @examples
#' lsp(base)
#' 
#' lsp(rawr)
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

lsp <- function(package, what = 'all') {
  
  if (!is.character(substitute(package)))
    package <- deparse(substitute(package))
  ns <- asNamespace(package)
  
  ## base package does not have NAMESPACE
  if (isBaseNamespace(ns))
    return(ls(.BaseNamespaceEnv, all.names = TRUE))
  else {
    ## for non base packages
    if (exists('.__NAMESPACE__.', envir = ns, inherits = FALSE)) {
      wh <- get('.__NAMESPACE__.', envir = asNamespace(package, base.OK = FALSE),
                inherits = FALSE)
      if ('?' %in% what) 
        return(ls(wh))
      if (!is.null(what) && !any(what %in% c('all', ls(wh))))
        stop('what is invalid; see ?rawr::lsp \'details\'')
      tmp <- sapply(ls(wh), function(x) getNamespaceInfo(ns, x))
      tmp <- rapply(tmp, ls, classes = 'environment', 
                    how = 'replace', all.names = TRUE)
      if (is.null(what))
        return(tmp)
      if (what %in% 'all')
        return(ls(getNamespace(package), all.names = TRUE))
      if (any(what %in% ls(wh)))
        return(tmp[what])
    } else 
      stop(sprintf('no NAMESPACE file found for package %s', package))
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
#' @usage ht(x, ..., sep = NULL)
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
#' Displays the percent completed during an iteration
#' 
#' @usage progress(value, max.value = NULL)
#' @param value numeric; i-th iteration
#' @param max.value numeric; n-th iteration
#' 
#' @examples
#' \dontrun{
#' for (ii in 1:1000) {
#'    progress(ii / 1000 * 100)
#'    Sys.sleep(.01)
#' }
#' }
#' @export

progress <- function (value, max.value = NULL) {
  
  if (!is.numeric(value)) 
    stop("'value' must be numeric")
  if (is.null(max.value)) {
    max.value <- 100
    percent <- TRUE
  } else 
    percent <- FALSE
  
  erase.only <- value > max.value
  max.value <- as.character(round(max.value))
  l <- nchar(max.value)
  value <- formatC(round(value), width = l)
  
  if (percent) {
    backspaces <- paste(rep('\b', l + 14), collapse = '')
    
    if (erase.only) 
      message <- ''
    else 
      message <- paste('Progress: ', value, '%  ', sep = '')
    cat(backspaces, message, sep = '')
    
  } else {
    backspaces <- paste(rep('\b', 2 * l + 16), collapse = '')
    if (erase.only) 
      message <- ''
    else 
      message <- paste('Progress: ', value, ' on ', max.value, '  ', sep = '')
    cat(backspaces, message, sep = '')
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
#' @usage recoder(object, pattern, replacement, ...)
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
#' @usage psum(..., na.rm = FALSE)
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
#' @usage ident(..., num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE, 
#'    ignore.bytecode = TRUE, ignore.environment = FALSE)
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
#' @details 
#' Generalized, recursive \code{\link{identical}} function for testing equality
#' of two or more \code{R} objects. 
#' 
#' A call to \code{identical} is the way to test exact equality in \code{if} 
#' and \code{while} statements, as well as in logical expressions that use 
#' \code{&&} or \code{||}. In all these applications you need to be assured of 
#' getting a single logical value. 
#' 
#' Users often use the comparison operators, such as \code{==} or \code{!=}, 
#' in these situations. It looks natural, but it is not what these operators 
#' are designed to do in \code{R}. They return an object like the arguments. 
#' If you expected \code{x} and \code{y} to be of length 1, but it happened 
#' that one of them was not, you will \emph{not} get a single \code{FALSE}. 
#' Similarly, if one of the arguments is \code{NA}, the result is also 
#' \code{NA}. In either case, the expression \code{if(x == y)...} won't work 
#' as expected. 
#' 
#' The function \code{all.equal} is also sometimes used to test equality this 
#' way, but was intended for something different: it allows for small 
#' differences in numeric results. 
#' 
#' The computations in \code{identical} are also reliable and usually fast. 
#' There should never be an error. The only known way to kill \code{identical} 
#' is by having an invalid pointer at the C level, generating a memory fault. 
#' It will usually find inequality quickly. Checking equality for two large, 
#' complicated objects can take longer if the objects are identical or nearly 
#' so, but represent completely independent copies. For most applications, 
#' however, the computational cost should be negligible. 
#' 
#' If \code{single.NA} is true, as by default, identical sees \code{NaN} as 
#' different from \code{\link{NA_real_}}, but all \code{NaN}s are equal (and 
#' all \code{NA} of the same type are equal). 
#' 
#' Character strings are regarded as identical if they are in different marked 
#' encodings but would agree when translated to UTF-8. 
#' 
#' If \code{attrib.as.set} is \code{TRUE}, as by default, comparison of 
#' attributes view them as a set (and not a vector, so order is not tested). 
#' 
#' If \code{ignore.bytecode} is \code{TRUE} (the default), the compiled 
#' bytecode of a function (see \code{\link{cmpfun}}) will be ignored in the 
#' comparison. If it is \code{FALSE}, functions will compare equal only if they
#' are copies of the same compiled object (or both are uncompiled). To check 
#' whether two different compiles are equal, you should compare the results of 
#' \code{\link{disassemble}}(). 
#' 
#' Note that \code{identical(x, y, FALSE, FALSE, FALSE, FALSE)} and 
#' \code{ident(..., FALSE, FALSE, FALSE, FALSE)} \emph{pickily} tests for 
#' exact equality. 
#' 
#' @return A single logical value, \code{TRUE} or \code{FALSE}, never \code{NA}
#' and never anything other than a single value.
#' @author John Chambers and R Core, \code{\link{identical}}; Robert Redd, 
#' additions
#' @references Chambers, J.M. (1998) \emph{Programming with Data. A guide to 
#' the S Language}. Springer.
#' @seealso \code{\link{identical}} for original; \code{\link{all.equal}} for 
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
#' @usage search.df(pattern, df, col.name, var = 0, ignore.case = TRUE, ...)
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
#' @usage search.hist(..., fixed = FALSE)
#' 
#' @param ... numeric or character; if numeric, shows the most recent \code{n} 
#' lines in \code{.Rhistory}; if character, searches for pattern matches
#' @param fixed logical; if \code{TRUE}, \code{pattern} is a string to be 
#' matched as is; overrides all conflicting arguments.
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
#' @export

search.hist <- function (..., fixed = FALSE) {
  
  hist <- readLines('.Rhistory')
  lhist <- length(hist)
  
  if (is.numeric(...))
    return(hist[lhist:(lhist - ... + 1)])
  if (is.character(...))
    return(grep(..., readLines(".Rhistory"), fixed = fixed, value = TRUE))
}

#' ggplot colors
#' 
#' A function to replicate default \code{\link[ggplot2]{ggplot}} colors
#' 
#' @usage ggcols(n, l = 65, c = 100)
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
#' @export

ggcols <- function(n, l = 65, c = 100) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = l, c = c)[1:n]
}

#' Choose n colors using the golden ratio
#'
#' This chooses \code{n} colour hues using a sequence generated by the Golden
#' Ratio
#' 
#' @usage grcols(n, s = .5, v = 1, alpha = 1)
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
#' @export

grcols <- function(n, s = .5, v = 1, alpha = 1) {
  GR <- 2 / (1 + sqrt(5))
  hues <- (seq(0, n - 1) * GR) %% 1
  hsv(hues, s = s, v = v, alpha = alpha)
}

#' Transparent colors
#' 
#' Add transparency to colors
#' 
#' @usage tcol(color, trans = 255)
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
#' The funtion converts a adds two more hex numbers coding a transparency 
#' between 0 (fully transparent) and 255 (fully visible). \code{color} values 
#' are converted to RGB, transparency is added, and converted 
#' 
#' @seealso \code{\link{as.hexmode}}, \code{\link{col2rgb}}
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
#' @usage fapply(X, FUN, ...)
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
#' @seealso \code{\link{sapply}}
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
#' @export

fapply <- function(X, FUN, ...) {
  fcts <- unlist(sapply(match.call()$FUN, as.character))[-1]
  out <- t(sapply(FUN, mapply, X, ...))
  setNames(as.data.frame(t(out)), fcts)
}

fapply_1 <- function(X, FUN, ...) {
  fcts <- unlist(sapply(match.call()$FUN, as.character))[-1]
  out <- outer(FUN, X, Vectorize(function(a, b) a(b, ...)))
  setNames(as.data.frame(t(out)), fcts)
}

fapply_2 <- function(X, FUN, trans = FALSE, ...) {
  fcts <- unlist(sapply(match.call()$FUN, as.character))[-1]
  out <- as.data.frame(sapply(FUN, function(x) lapply(X, x)))
  names(out) <- fcts
  if (trans) 
    out <- as.data.frame(t(out))
  out
}

#' Improved list of objects
#' 
#' Provides more details of objects in workspace
#' 
#' @usage 
#' lss(pos = 1, pattern, by = NULL, all.names = FALSE,
#'     decreasing = TRUE, head = TRUE, n = 15)
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
#' @usage rescaler(x, to = c(0, 1), from = range(x, na.rm = TRUE))
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
#' @usage html.test(...)
#' 
#' @param ... character string of html code
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
#' @export

html.test <- function(...) {
  htmlFile <- tempfile(fileext = '.html')
  writeLines(..., con = htmlFile,)
  if ((Sys.getenv('RSTUDIO') != '') && ('rstudio' %in% .packages(TRUE)))
      rstudio::viewer(htmlFile)
  else browseURL(htmlFile)
}

#  roundr
#'
#' Improved rounding formatter
#' 
#' @usage roundr(x, digits = 1)
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

#' @export
roundr.data.frame <- function(x, digits = 1) {
  mode.ok <- vapply(x, function(x) is.numeric(x) || is.complex(x), NA)
  if (!all(mode.ok))
    stop(paste0('non-numeric variable in data frame, ', deparse(substitute(x))))
  else
    do.call(data.frame, lapply(x, roundr, digits))
}

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
#' @usage intr(..., fun = median, conf = NULL, digits = 2, na.rm = FALSE)
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
#' @usage
#' pvalr(pvals, sig.limit = .001, digits = 3, html = FALSE, show.p = FALSE)
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
#' @usage show.colors()
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
#' @usage show.pch()
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
#' Binomial confidence interval formatter
#' 
#' @usage binconr(r, n, conf = 0.95, digits = 2, est = TRUE, method = 'exact')
#' 
#' @param r number of responses (successes)
#' @param n number of observations (trials)
#' @param conf level of confidence
#' @param digits number of digits
#' @param est logical; if \code{TRUE}, includes the point estimate
#' @param method method to use; see \code{\link{bincon}}
#' 
#' @examples
#' binconr(5, 10, .90, est = FALSE)
#' binconr(45, 53, .95, digits = 1)
#' 
#' @seealso \code{\link{bincon}}; \code{\link[Hmisc]{binconf}}
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
#' Convert a number to a character string
#' 
#' @usage num2char(num, informal = FALSE, cap = TRUE)
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
#' @usage
#' iprint(..., wrap, sep, copula, digits = 2)
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
#' @usage
#' list2file(l, targetdir = getwd(), sep, ...)
#' 
#' @param l a list of data frames or matrices
#' @param targetdir target directory (created if doesn't exist)
#' @param sep field separator string; default is none which results in \code{R}
#' data files; \code{,} creates \code{.csv} files; any other separator will
#' create \code{.txt} files
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
                                 ifelse(sep == ',','csv','txt')),
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
#' @usage
#' match_ctc(..., version = 4)
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
    dat <- rawr::ctcae_v3
  else dat <- rawr::ctcae_v4
  
  if (any(grepl('([A-Za-z -])([0-9])', x))) {
    idx <- 'tox_code'
    x <- gsub('\\s*|-', '', x)
  } else idx <- 'tox_desc'
  
  idx <- grep(paste(x, collapse = '|'), dat[ , idx], ignore.case = TRUE)
  
  return(list(matches = dat[idx, ],
              version = sprintf('CTCAE v%s', version)))
}

#' clc
#' 
#' Clear the workspace
#' 
#' @usage clc(all = FALSE)
#' @param all logical; if \code{TRUE}, also removes hidden files
#' 
#' @export

clc <- function(all = FALSE)
  rm(list = ls(.GlobalEnv, all.names = all), envir = .GlobalEnv)

#' clear
#' 
#' Clear the console window
#' 
#' @usage clear()
#' 
#' @export

clear <- function() cat('\014')

#' Write ftable
#' 
#' \code{\link{ftable}}s can look nice, but are only \code{\link{cat}}'d to 
#' the console and, thus, not easily used (or manipulated). 
#' \code{\link{write.ftable}} does not write \code{ftable}s as they print,
#' so here we are.
#' 
#' @usage
#' writeftable(x, quote = FALSE, digits = getOption('digits'), ...)
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
  x <- stats:::format.ftable(x, quote = quote, digits = digits, ...)
  as.matrix(x)
}

#' Extract R help files
#' 
#' Extracts specified portions of R help files (from \emph{loaded} libraries)
#' for use in Sweave or R-markdown documents.
#' 
#' @usage
#' helpExtract(f, show.sections = FALSE, section = 'Usage', 
#'             type = 'm_code', ...)
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
#' \verb{```{r, results='asis'}} \verb{cat(helpExtract(print), sep ='\n')} 
#' \verb{```}
#' 
#' To insert a (highlighted) chunk into a Sweave document:
#' 
#' \verb{\Sexpr{knit_child(textConnection(helpExtract(print, type = 's_code')),
#' options = list(tidy = FALSE, eval = FALSE))}}
#' 
#' @return 
#' A character vector to be used in a Sweave or R-markdown document.
#' @author Ananda Mahto
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
  
  A <- deparse(substitute(f))
  x <- capture.output(tools:::Rd2txt(utils:::.getHelpFile(utils::help(A, ...)),
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
#' @usage Round(x, target)
#' 
#' @param x numeric values
#' @param target desired sum of code{x} after rounding
#' 
#' @examples
#' pcts <- data.frame(pct1 = c(33.3, 21.5, 45.51),
#'                    pct2 = c(33.3,33.3,33.3))
#' 
#' Map(round, pcts); sapply(.Last.value, sum)
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
#' @usage bind_all(..., which)
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
#' @usage interleave(..., which)
#' 
#' @param ... vectors, matrices, or data frames
#' @param which joining method; \code{'rbind'} or \code{'cbind'}
#' 
#' @examples
#' interleave(matrix(1:9, 3, 3), matrix(letters[1:9], 3, 3), which = 'rbind')
#' interleave(matrix(1:9, 3, 3), matrix(letters[1:9], 3, 3), which = 'cbind')
#' 
#' @export

interleave <- function(..., which) {
  if (missing(which))
    stop('specify which: \'rbind\' or \'cbind\'')
  l <- list(...)
  if (which == 'rbind')
    return(do.call('rbind', l)[order(sequence(sapply(l, nrow))), ])
  else if (which == 'cbind')
    return(do.call('cbind', l)[ , order(sequence(sapply(l, ncol)))])
}

#' Outer product of n-dimensional arrays
#' 
#' The outer product of the arrays \code{X}, \code{Y}, ... with dimensions
#' \code{c(dim(X), dim(Y), ...)}; see \code{\link{outer}}.
#' 
#' @usage outer2(..., FUN)
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
