### utilities
# progress bar, data recoder, pairwise sum, ident, search functions, ggcols, 
# grcols, tcol, fapply, lss, rescaler, html.test, roundr, pvalr, intr, 
# show.colors
###

#' @export
`%ni%` <- Negate(`%in%`)

#' headtail
#' 
#' @usage ht(x, ..., sep = NULL)
#' @param x an object
#' @param ... other parameters, such as \code{n}, passed to \code{\link{head}} 
#' or other methods
#' @param sep separater 
#' @export
ht <- function(x, ..., sep = NULL) rbind(head(x, ...), sep, tail(x, ...))

#' oror
#' 
#' function_that_may_return_null() or default value
#' @name oror
#' @aliases %||%
#' @examples
#' NULL || TRUE    # error
#' NULL %||% TRUE  # no error
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
  } else percent <- FALSE
  
  if (!is.numeric(max.value) || !is.null(max.value)) 
    stop("'max.value' must be numeric or NULL")
  
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
#' A function for recoding numeric, character, and factor values 
#' in a vector, list, matrix, or data frame.
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
  
  if (is.factor(object)) {
    cat('level(s)', 
        levels(factor(levels = setdiff(replacement, levels(object)))),
        'added to factor variable,', deparse(substitute(object)),'\n')
    levels(object) <- c(levels(object), replacement)
  }
  if (length(replacement) == 1) {
    replacement <- rep(replacement, length(pattern))
  }
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
#' @param ... numeric or character; if numeric, shows the most recent \code{n} 
#' lines in \code{.Rhistory}; if character, searches for pattern matches
#' @return Returns a list of recent commands that match \code{pattern}
#' 
#' @examples
#' search.hist()
#' search.hist(25)
#' search.hist('?')
#' search.hist('?', fixed = TRUE)
#' search.hist('\\?')
#' @export

search.hist <- function (..., fixed = FALSE) {
  
  hist <- readLines('.Rhistory')
  lhist <- length(hist)
  
  if (is.numeric(...))
    return(hist[lhist:(lhist - ... + 1)])
  if (is.character(...))
    return(grep(..., readLines(".Rhistory"), fixed = fixed, value = TRUE))
}

#' ggplot colours
#' 
#' A function to replicate default \code{\link{ggplot}} colours
#' 
#' @usage ggcols(n, c = 100, l = 65)
#' 
#' @param n number of colours
#' @param c the chroma of the colour; the upper bound for chroma depends on hue
#' and luminance
#' @param l a value in the range \code{[0, 100]} giving the luminance of the 
#' colour; for a given combination of hue and chroma, only a subset of this 
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
#' @param alpha Numeric. The alpha blending value that is input into hsv.
#' @seealso \code{\link{hsv}}
#' 
#' @examples
#' plot(1:5, 1:5, col = grcols(5), pch = 20, cex = 3)
#' 
#' plot(c(1, 5), c(0, 1), type = 'n')
#' rect(1:5, 0, 2:6, 1, col = grcols(5))
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
#' @seealso \code{\link{num2hex}}, \code{\link{col2rgb}}
#' 
#' @examples
#' cols <- c('red','green','pink')
#' 
#' # a normal plot
#' plot(rnorm(100), col = tcol(cols), pch = 16, cex = 4)
#' 
#' # more transparent
#' plot(rnorm(100), col = tcol(cols, 100), pch = 16, cex = 4)
#' 
#' # hexadecimal colors also work
#' cols <- c('#FF0000','#00FF00','#FFC0CB')
#' plot(rnorm(100), col = tcol(cols, 200), pch= 16, cex = 4)
#' @export

tcol <- function(color, trans = 255) {
  if (length(color) != length(trans) & 
        !any(c(length(color), length(trans)) == 1)) 
    stop('Vector lengths not correct')
  if (length(color) == 1 & length(trans) > 1) 
    color <- rep(color, length(trans))
  if (length(trans) == 1 & length(color) > 1) 
    trans <- rep(trans, length(color))
  
  num2hex <- function(x) {
    hex <- unlist(strsplit('0123456789ABCDEF', split = ''))
    return(paste0(hex[(x - x %% 16) / 16 + 1], hex[x %% 16 + 1]))
  }
  
  rgb <- rbind(col2rgb(color), trans)
  res <- paste0('#', apply(apply(rgb, 2, num2hex), 2, paste, collapse = ''))
  return(res)
}

#' Apply list of functions over list or vector
#' 
#' A simple modification to the \code{*apply} functions which allows a list of 
#' functions to be passed simultaneously.
#' 
#' @usage fapply(X, FUN, trans = FALSE, ...)
#' 
#' @param X a vector (atomic or list) or an \code{\link{expression}} object; 
#' other objects (including classed objects) will be coerced by 
#' \code{base::\link{as.list}}.
#' @param FUN a list of functions to be applied to each element of \code{X}; 
#' see \code{\link{lapply}}:"Details"
#' @param trans logical; should output be transposed
#' @param ... for future options
#' 
#' @return A data frame where \code{nrow} equals the length of \code{X} and 
#' \code{ncol} equals the length of \code{FUN}.
#' @seealso \code{\link{sapply}}
#' 
#' @examples
#' fct <- function(x) quantile(x, c(.05, .95))
#' fapply(mtcars, list(median, fct))
#' fapply(mtcars, list(min, mean, max, length), trans = TRUE)
#' # compare to 
#' summary(mtcars)
#' @export
#' 
#' 

fapply <- function(X, FUN, trans = FALSE, ...) {
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
#' lss(pos = 1L, pattern, by = 'size', decreasing = TRUE, all.names = FALSE, 
#'     head = TRUE, n = 10)
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
                 decreasing = TRUE, head = TRUE, n = 15, ...) {
  
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
#' View html in rstudio viewer
#' 
#' @usage html.test(...)
#' 
#' @param ... character string of html code
#' 
#' @examples
#' html.test("
#' <div align = center><h1>A heading<sup>&dagger;</sup><h1></div>
#' <font size = 1><sup>&dagger;</sup>That was the heading</font>
#' ")
#' 
#' library(Gmisc)
#' html.test(htmlTable(mtcars, output = FALSE))
#' @export

html.test <- function(...) {
  htmlFile <- tempfile(fileext = '.html')
  write(..., file = htmlFile)
  rstudio::viewer(htmlFile)
}

#  roundr
#'
#' Round a number to the specified number of decimal places (default is 1), 
#' preserving trailing 0s
#' 
#' @usage roundr(..., digits  = 1)
#'
#' @param x numeric value or vector
#' @param digits number of digits past the decimal point to keep
#'
#' @details 
#' Uses \code{\link[base]{sprintf}} to round numeric value, retaining extra 0s.
#' @return
#' A vector of character strings.
#'
#' @examples
#' roundr(51.01, 3)
#' roundr(0.199, 2)
#' 
#' # useful for dropping the negative in case 1:
#' roundr(c(-0.0002, 0.0002, 0.5, -0.5, -0.002), digits = 3)
#'
#' @seealso \code{\link[base]{round}}; \code{\link[base]{sprintf}}
#' @export

roundr <- function(x, digits = 1) {
  res <- sprintf(paste0('%.', digits, 'f'), x)
  zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
  res[res == paste0('-', zzz)] <- zzz
  res
}

#' Interval formatter
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
#' cbind(lapply(mtcars, intr, conf = .5), lapply(mtcars, intr, fun = mean))
#' # compare to
#' summary(mtcars)
#' 
#' @export

intr <- function(..., fun = median, conf = NULL, digits = 2, na.rm = FALSE) {
  
  lst <- list(...)
  if (is.null(conf)) 
    conf <- 1 else
      if (findInterval(conf, c(0, 1), rightmost.closed = TRUE) !=1)
        stop('invalid confidence interval')
  
  # see ?rawr::roundr
  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x)
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    res
  }
  
  sapply(lst, function(x) {
    bounds <- quantile(x, c((1 - conf) / 2 * c(1, -1) + c(0, 1)), na.rm = na.rm)
    bounds <- roundr(bounds, digits = digits)
    val <- roundr(fun(x, na.rm = na.rm), digits = digits)
    
    if (! conf %in% 1)
      paste0(val, ' (', paste0(conf * 100, '% CI: '), 
             bounds[1], ' - ', bounds[2],')') else
               paste0(val, ' (min ', bounds[1], '; max ', bounds[2],')')
  })
}

#' p-value formatter
#' 
#' Formats several cases of p-values; see details
#'  
#' @usage pvalr(pvals, sig.limit = .001, digits = 3, html = FALSE)
#' 
#' @param pvals numeric value or vector of p-values
#' @param sig.limit lower bound for precision
#' @param digits integer; number of decimal places; see also 
#' \code{\link[rawr]{roundr}}
#' @param html logical; if \code{TRUE}, uses \code{&lt;} instead of \code{<}
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
#' 
#' @export

pvalr <- function(pvals, sig.limit = .001, digits = 3, html = FALSE) {
  
  # see ?rawr::roundr
  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x)
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    res
  }
  
  sapply(pvals, function(x, sig.limit) {
    if (x < sig.limit)
      if (html)
        return(sprintf('&lt; %s', format(sig.limit))) else
          return(sprintf('< %s', format(sig.limit)))
    if (x > .1)
      return(roundr(x, digits = 2)) else
        return(roundr(x, digits = digits))
  }, sig.limit = sig.limit)
}

#' Show colors
#' 
#' In \code{R}, there are 657 named colors. This funciton shows these colors 
#' and their respective numbers. Find a color by number in the plot or find the
#' name of the color with \code{colors()[n]}
#' 
#' @usage show.colors()
#' 
#' @examples
#' show.colors()
#' colors()[81]
#' # [1] "darkgreen"
#' @export

show.colors <- function() {
  par(mfrow = c(1,1))
  par(mai=c(.4,.4,.4,.4), oma=c(.2,0,0,.2))
  x <- 22
  y <- 30
  plot(c(-1, x), c(-1, y), xlab = '', ylab = '', type = 'n', xaxt = 'n', 
       yaxt = 'n', bty = 'n')
  sapply(1:x, function(i) {
    sapply(1:y, function(j) {
      k <- y*(i-1) + j
      co <- colors()[k]
      rect(i - 1, j - 1, i, j, col = co, border = grey(.5))
    })
  })
  text(rep(-.5, y), (1:y) - .5, 1:y, cex = 1.2 - .016 * y)
  text((1:x) - .5, rep(-.5, x), y * (0:(x - 1)), cex = 1.2 - .022 * x)
  title('col = colors()[n]')
}