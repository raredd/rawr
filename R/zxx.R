### some random things
# identical2, all_equal2, fapply, fapply_by, flatten, rm_null, kinda_sort,
# sym_sort, sample_each, Round, round_to, pickcol, lunique, rm_nonascii
# 
# cumfuns
# cum_reset, cum_na, cumsum_na, cumprod_na, cummax_na, cummin_na, cum_mid
###


#' Test two or more objects for exact equality
#' 
#' The safe and reliable way to test two or more objects for being exactly
#' equal; returns \code{TRUE} in this case, \code{FALSE} in every other case.
#'    
#' @param ... any \code{R} objects
#' @param num.eq logical indicating if (\code{\link{double}} and
#' \code{\link{complex}} non-\code{\link{NA}}) numbers should be compared
#' using \code{\link{==}} ("equal"), or by bitwise comparison. The latter
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
#' @param ignore.environment logical indicating if their environments should
#' be ignored when comparing \code{closure}s.
#' 
#' @return
#' A single logical value, \code{TRUE} or \code{FALSE}, never \code{NA}
#' and never anything other than a single value.
#' 
#' @seealso
#' \code{\link{identical}}; \code{\link{all.equal}} for descriptions of how
#' two objects differ; \code{\link{Comparison}} for operators that generate
#' elementwise comparisons; \code{\link{isTRUE}} is a simple wrapper based
#' on \code{identical}; \code{\link{all_equal2}}
#' 
#' @examples
#' identical2(1, 1.)
#' identical2(1, 1., 1L)
#' 
#' ## for unusual R objects:
#' identical2(.GlobalEnv, environment(), globalenv(), as.environment(1))
#' 
#' identical2(0., 0, -0.) ## not differentiated
#' identical2(0., 0, -0., num.eq = FALSE)
#' 
#' identical2(NaN, -NaN)
#' identical2(NaN, -NaN, single.NA = FALSE) ## differ on bit-level
#' 
#' ## for functions
#' f <- function(x) x
#' g <- compiler::cmpfun(f)
#' identical2(f, g)
#' identical2(f, g, ignore.bytecode = FALSE)
#' 
#' @export

identical2 <- function(..., num.eq = TRUE, single.NA = TRUE,
                       attrib.as.set = TRUE, ignore.bytecode = TRUE,
                       ignore.environment = FALSE) {
  if (length(l <- list(...)) < 2L)
    stop('must provide at least two objects')
  
  l <- sapply(seq.int((length(l) - 1L)), function(ii)
    identical(l[ii], l[ii + 1L], num.eq = num.eq, single.NA = single.NA,
              attrib.as.set = attrib.as.set, ignore.bytecode = ignore.bytecode,
              ignore.environment = ignore.environment))
  
  all(l)
}

#' Test if two or more objects are (nearly) equal
#' 
#' A generalization of \code{\link{all.equal}} that allows more than two
#' objects to be tested for near-equality.
#' 
#' @param ... any \code{R} objects
#' @param tolerance numeric >= 0; differences smaller than \code{tolerance}
#' are not reported (default value is close to 1.5e-8)
#' @param scale numeric scalar > 0 (or \code{NULL}), see details in
#' \code{\link{all.equal}}
#' @param check.attributes logical indicating if the \code{\link{attributes}}
#' should be compared
#' @param use.names logical indicating if \code{\link{list}} comparison should
#' report differing components by name (if matching) instead of integer index
#' @param all.names logical passed to \code{\link{ls}} indicating if "hidden"
#' objects should also be considered in the environments
#' @param check.names logical indicating if the \code{\link{names}}\code{(.)}
#' should be compared
#' 
#' @return
#' If all \code{...} are nearly equal, \code{TRUE} otherwise returns a list
#' with the objects that failed.
#' 
#' @seealso
#' \code{\link{all.equal}}; \code{\link{identical2}}; \code{\link{identical}}
#' 
#' @examples
#' all_equal2(pi, 355/113, 22/7)
#' all_equal2(pi, 355/113, 22/7, tolerance = 0.01)
#' 
#' all_equal2(cars[1], cars[, 1, drop = FALSE], cars[, -2, drop = TRUE])
#' 
#' @export all_equal2

all_equal2 <- function(..., tolerance = .Machine$double.eps ^ 0.5,
                       scale = NULL, check.attributes = TRUE,
                       use.names = TRUE, all.names = TRUE,
                       check.names = TRUE) {
  dots <- substitute(...())
  l <- setNames(list(...), dots)
  
  if (length(l <- list(...)) < 2L)
    stop('must provide at least two objects')
  
  l <- lapply(seq.int((length(l) - 1L)), function(x)
    do.call('all.equal', list(
      target = l[[x]], current = l[[x + 1L]],
      tolerance = tolerance, check.attributes = check.attributes,
      scale = scale, use.names = use.names, all.names = all.names))
  )
  trues <- c(TRUE, sapply(l, isTRUE))
  trues[1L] <- trues[2L]
  
  if (all(trues))
    TRUE else dots[!trues]
}

#' Apply summary functions over list or vector
#' 
#' \code{fapply} applies summary function(s) over a vector, list, or data
#' frame, and \code{fapply_by} applies summary function(s) over subsets of
#' a data frame.
#' 
#' @param data for \code{fapply}, a vector, list, or data frame to operate on;
#' for \code{fapply_by}, a data frame containing the variables in \code{formula}
#' @param ... summary function(s) such as \code{length(.)} or
#' \code{mean(., na.rm = TRUE)} to apply; names are not required but strongly
#' recommended
#' @param formula a formula such as \code{y ~ x} or \code{cbind(y1, y2) ~ x1 + x2}
#' where the \code{y} variables are numeric data to be split into groups
#' according to the grouping \code{x} variables (usually factors)
#' 
#' @examples
#' tmp <- replace(mtcars, mtcars == 6, NA)
#' fapply(tmp, mean = mean(.), median = median(., na.rm = TRUE))
#' fapply(mtcars$mpg, mean = mean(.))
#' 
#' ## define a new function
#' ci <- function(x) {
#'   q <- quantile(x, c(0.025, 0.975), na.rm = TRUE)
#'   sprintf('%.0f (%.2f, %.2f)', median(x), q[1], q[2] )
#' }
#' fapply(mtcars, median(.), '95% CI' = ci(.))
#' 
#' ## compare: 
#' t(fapply(mtcars, min(.), mean(.), max(.), length(.)))
#' summary(mtcars)
#' 
#' 
#' fapply_by(mpg ~ vs + am, mtcars, mean(.), median(.), length(.))
#' fapply_by(as.matrix(mtcars) ~ vs, mtcars, mean = mean(.))
#' 
#' ## one ~ one, one ~ many, many ~ one, and many ~ many
#' fapply_by(disp ~ cyl, mtcars, mean = mean(.))
#' fapply_by(disp ~ cyl + vs, mtcars, mean = mean(.))
#' fapply_by(cbind(disp, wt) ~ cyl, mtcars, mean = mean(.))
#' fapply_by(cbind(disp, wt) ~ cyl + vs, mtcars, mean = mean(.), n = length(.))
#' 
#' ## compare
#' aggregate(cbind(disp, wt) ~ cyl + vs, mtcars, function(x)
#'   c(mean(x), length(x)))
#' 
#' @export

fapply <- function(data, ...) {
  cl <- match.call(expand.dots = FALSE)$`...`
  if (is.null(cl))
    stop('no methods given')
  cl <- c(alist(i = NULL), cl)
  if (any(nn <- !nzchar(names(cl))))
    names(cl)[nn] <- sapply(cl, deparse)[nn]
  
  if (!is.list(data))
    data <- list(data)
  
  res <- lapply(cl[-1L], function(fn)
    mapply(function(.) eval(fn, NULL), data))
  
  setNames(data.frame(res, stringsAsFactors = FALSE), names(cl)[-1L])
}

#' @rdname fapply
#' @export
fapply_by <- function(formula, data, ...) {
  cl <- match.call(expand.dots = FALSE)$`...`
  if (is.null(cl))
    stop('no methods given')
  cl <- c(alist(i = NULL), cl)
  if (any(nn <- !nzchar(names(cl))))
    names(cl)[nn] <- sapply(cl, deparse)[nn]
  
  nt <- length(all.vars(formula[[3L]]))
  ag <- aggregate(formula, data, function(.)
    lapply(cl, function(fn) eval(fn, NULL)))
  ag <- unclass(ag)
  
  ll <- lapply(tail(ag, -nt), function(x) {
    x <- data.frame(x, check.names = FALSE)[, -1L, drop = FALSE]
    data.frame(lapply(x, unlist), check.names = FALSE)
  })
  
  ## useful names if >1 lhs variable
  # if (length(all.vars(formula[[2L]])) > 1L)
  ll <- if (length(ag) > (nt + 1L))
    lapply(seq_along(ll), function(ii) {
      names(ll[[ii]]) <- paste(names(ll)[ii], names(ll[[ii]]), sep = '.')
      ll[[ii]]
    }) else ll[[length(ll)]]
  
  cbind(data.frame(head(ag, nt), check.names = FALSE), ll)
}

#' Flatten lists
#' 
#' Flattens lists and nested lists of vectors, matrices, and/or data frames.
#' 
#' @param l a list
#' 
#' @references
#' \url{https://stackoverflow.com/q/8139677/2994949}
#' 
#' @examples
#' l <- list(matrix(1:3), list(1:3, 'foo'), TRUE, 'hi',
#'            list(mtcars[1:5, 1:5], list(mtcars[1:5, 1:5])))
#' str(l)
#' str(flatten(l))
#' 
#' @export

flatten <- function(l) {
  while (any(vapply(l, islist, logical(1L)))) {
    l <- lapply(l, function(x)
      if (islist(x))
        x else list(x))
    l <- unlist(l, recursive = FALSE)
  }
  
  l
}

#' Recursive \code{rm} for lists
#' 
#' Remove \code{NULL} or \code{list(NULL)} objects recursively from a list.
#' 
#' @param l a list
#' @param rm_list logical; if \code{FALSE}, lists with only the \code{NULL}
#' object will not be removed
#' 
#' @references
#' \url{https://stackoverflow.com/q/26539441/2994949}
#' 
#' @examples
#' str(l <- list(list(NULL),list(1),list('a', NULL)))
#' str(rm_null(l))
#' str(rm_null(l, FALSE))
#' 
#' @export

rm_null <- function(l, rm_list = TRUE) {
  isnull <- if (rm_list)
    function(x) is.null(x) | all(vapply(x, is.null, logical(1L)))
  else function(x) is.null(x)
  
  x <- Filter(Negate(isnull), l)
  
  lapply(x, function(x)
    if (is.list(x))
      rm_null(x, rm_list) else x)
}

#' Cumulative functions
#' 
#' @description
#' \code{cum_reset} will reset a cumulative function, \code{FUN}, when
#' \code{value} is encountered.
#' 
#' \code{*_na} functions offer alternatives to the \pkg{base}
#' \link[=cumsum]{cumulative functions} that can handle \code{NA}s.
#' 
#' \code{cum_mid} finds the mid-points between "stacked" numeric values.
#' 
#' @param x a vector (or numeric matrix for \code{cum_mid})
#' @param value a value of \code{x} which signals the end of a group and
#' resets \code{FUN}
#' @param FUN function to apply to each group, usually one of
#' \code{\link{cumsum}}, \code{\link{cumprod}}, \code{\link{cummax}}, or
#' \code{\link{cummin}} but can be any function that returns a vector the
#' same length and type as the input (\emph{a la} \code{\link{ave}})
#' @param useNA logical; if \code{TRUE}, indices with \code{NA} will be
#' unchanged; if \code{FALSE}, the previous value is carried forward
#' @param adj for \code{cum_mid}, an adjustment parameter, usually in
#' \code{[0, 1]}, giving the relative position between each value (default
#' is centered, \code{adj = 0.5})
#' 
#' @return
#' A vector having the same length as \code{x} with \code{FUN} applied to
#' each group defined by positions of \code{value}.
#' 
#' @seealso
#' \code{\link{cumsum}}; \code{\link{ave}}; \code{locf}
#' 
#' @examples
#' x <- 1:10
#' cum_reset(x, 5, cummin)
#' cum_reset(x, c(5, 8), cummin)
#' 
#' x[x %% 4 == 0] <- 0
#' cum_reset(x, FUN = cumsum)
#' cum_reset(x, FUN = sum)
#' 
#' set.seed(1)
#' data.frame(x = x <- rpois(15, 1),
#'            y = cum_reset(x, FUN = cumsum),
#'            z = cum_reset(x, 0, function(x) ave(x, FUN = sum)))
#' 
#' 
#' ## x need not be numeric if FUN returns an appropriate type and length
#' cum_reset(letters[1:10], c('d','g'), function(x)
#'   letters[as.numeric(factor(x))])
#' 
#' 
#' ## cum* functions to handle NA values
#' x <- 1:10
#' x[x %% 4 == 0] <- 0
#' na <- ifelse(x == 0, NA, x)
#' 
#' cumsum(x)
#' cum_na(x, cumsum)
#' 
#' cumsum(na)
#' cum_na(na, cumsum)
#' 
#' ## shorthand
#' cumsum_na(na)
#' cumsum_na(na)
#' 
#' 
#' ## like cum_reset, cum_na's FUN argument can be generalized if FUN
#' ## returns the correct class and length of the input
#' FUN <- function(x) vector(class(x), length(x))
#' cum_na(na, FUN)
#' 
#' cumdiff <- function(x) Reduce(`-`, x, accumulate = TRUE)
#' cumdiff(x)
#' cumsum(c(x[1L], -x[-1L]))
#' 
#' cumdiff(na)
#' cumsum(c(na[1L], -na[-1L]))
#' cum_na(na, cumdiff)
#' 
#' 
#' ## "stacked" numeric values, eg, from a barplot
#' set.seed(1)
#' x <- matrix(runif(12), ncol = 3L)
#' bp <- barplot(x, names.arg = paste('adj = ', c(0, 1, 0.5)))
#' 
#' for (ii in seq.int(ncol(x))) {
#'   xii <- x[, ii, drop = FALSE]
#'   text(bp[ii], cum_mid(xii, c(0, 1, 0.5)[ii]), xii, xpd = NA)
#' }
#' 
#' @name cumfuns
NULL

#' @rdname cumfuns
#' @export
cum_reset <- function(x, value = 0L, FUN) {
  FUN <- match.fun(FUN)
  idx <- c(0L, head(cumsum(x %in% value), -1L))
  unname(unlist(lapply(split(x, idx), FUN)))
}

#' @rdname cumfuns
#' @export
cum_na <- function(x, FUN, useNA = TRUE) {
  FUN <- match.fun(FUN)
  x[!is.na(x)] <- FUN(x[!is.na(x)])
  if (useNA)
    x else locf(x)
}

#' @rdname cumfuns
#' @export
cumsum_na <- function(x, useNA = TRUE) {
  cum_na(x, cumsum, useNA)
}

#' @rdname cumfuns
#' @export
cumprod_na <- function(x, useNA = TRUE) {
  cum_na(x, cumprod, useNA)
}

#' @rdname cumfuns
#' @export
cummax_na <- function(x, useNA = TRUE) {
  cum_na(x, cummax, useNA)
}

#' @rdname cumfuns
#' @export
cummin_na <- function(x, useNA = TRUE) {
  cum_na(x, cummin, useNA)
}

#' @rdname cumfuns
#' @export
cum_mid <- function(x, adj = 0.5) {
  stopifnot(adj %inside% 0:1)
  
  mat <- as.matrix(x)
  res <- rbind(0, mat[-nrow(mat), , drop = FALSE])
  res <- mat / (1 / adj) + apply(res, 2L, cumsum)
  
  if (is.null(dim(x)))
    drop(res) else res
}

#' Kinda sort
#' 
#' @description
#' \code{\link{sort}} a vector but not very well.
#' 
#' For a vector, \code{x}, \code{n} elements will be randomly selected, and
#' their positions will remain unchanged as all other elements are sorted.
#' Alternatively, a vector of \code{indices} of \code{x} can be given and
#' will remain unsorted.
#' 
#' @param x a numeric, complex, character, or logical vector
#' @param n number of elements of x to remain unsorted (the default is
#' approximately 10\% of \code{x}), ignored if \code{indices} is given
#' @param decreasing logical; if \code{FALSE} (default), \code{x} is sorted
#' in increasing order
#' @param indices a vector of indices specifying which elements of \code{x}
#' should \emph{not} be sorted
#' @param index.return logical; if \code{TRUE}, the ordering index vector is
#' returned
#' 
#' @return
#' \code{x} sorted approximately \code{(length(x) - n)/length(x)*100} percent.
#' 
#' @seealso
#' \code{\link{sort2}}; \code{\link{sym_sort}}
#' 
#' @examples
#' set.seed(1)
#' x <- sample(1:10)
#' 
#' rbind(
#'   unsorted   = x,
#'   '50% sort' = kinda_sort(x, n = 5),
#'   'fix 2:5'  = kinda_sort(x, indices = 2:5)
#' )
#' 
#' #          [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#' # unsorted    3    4    5    7    2    8    9    6   10     1
#' # 50% sort    3    4    5    6    2    8    7    9   10     1
#' # fix 2:5     1    4    5    7    2    3    6    8    9    10
#' 
#' 
#' ## use index.return = TRUE for indices instead of values
#' set.seed(1)
#' x  <- runif(100)
#' o1 <- kinda_sort(x, n = 50, index.return = TRUE)
#' 
#' set.seed(1)
#' x  <- runif(100)
#' o2 <- kinda_sort(x, n = 50)
#' 
#' stopifnot(
#'   identical(x[o1], o2)
#' )
#' 
#' @export

kinda_sort <- function(x, n, decreasing = FALSE, indices = NULL,
                       index.return = FALSE) {
  l <- length(x)
  n <- if (missing(n))
    ceiling(0.1 * l) else if (n > l) l else n
  
  if ((n <- as.integer(n)[1L]) == 0L)
    return(x)
  
  k <- sort(indices %||% sample(seq.int(l), n))
  s <- replace(x, k, NA)
  o <- sort2(s, decreasing, TRUE)
  
  if (index.return)
    o else x[o]
}

#' Symmetrical sort
#' 
#' Sort a vector symmetrically, i.e., the two most extreme values are put
#' at opposite ends and repeated until the median value(s) is(are) put in
#' the middle of the sorted vector.
#' 
#' @param x a numeric, complex, character, or logical vector
#' @param rev logical; if \code{TRUE}, vectors are sorted in reverse
#' @param index.return logical; if \code{TRUE}, the ordering index vector
#' is returned
#' 
#' @seealso
#' \code{\link{kinda_sort}}
#' 
#' @examples
#' sym_sort(letters)
#' sym_sort(letters, rev = TRUE)
#' 
#' x <- runif(50)
#' plot(sym_sort(x))
#' plot(x[sym_sort(x, index.return = TRUE)])
#' 
#' plot(sym_sort(x, rev = TRUE))
#' plot(-sym_sort(-x, rev = TRUE))
#' 
#' @export

sym_sort <- function(x, rev = FALSE, index.return = FALSE) {
  if (length(x) <= 1L)
    return(x)
  if (index.return)
    names(x) <- seq_along(x)
  rev <- if (rev)
    0:1 else 1:0
  
  s <- sort(x)
  f <- rep_len(1:2, length(s))
  
  sp <- split(s, f)
  sp <- Vectorize(sort, SIMPLIFY = FALSE)(sp, decreasing = !!rev)
  nn <- unlist(sapply(sp, names))
  
  res <- unlist(c(sp))
  names(res) <- nn
  
  if (index.return)
    as.integer(nn) else res
}

#' Sample each
#' 
#' Returns a logical vector where \code{n} items are randomly sampled from
#' each unique value of a vector, \code{x}.
#' 
#' @param x a character, factor, or numeric vector
#' @param n number to sample from each unique group in order; if \code{x} is
#' a factor, \code{n} should correspond to \code{levels(x)}; otherwise,
#' \code{n} will be matched with the sorted unique groups
#' 
#' @return
#' A logical vector the same length as \code{x} identifying selected indices.
#' 
#' @seealso
#' \code{\link{sample}}; \code{\link{kinda_sort}}
#' 
#' @examples
#' x <- mtcars$gear
#' 
#' sample_each(x)
#' mtcars[sample_each(x), ]
#' 
#' ## compare numeric vs factor vectors (see description above)
#' mtcars[sample_each(x, 3:5), ]
#' X <- factor(x, 5:3)
#' mtcars[sample_each(X, 3:5), ]
#' 
#' @export

sample_each <- function(x, n = 1L) {
  x  <- setNames(x, x)
  lx <- table(x)
  nT <- setNames(rep_len(n, length(lx)), names(lx))
  nF <- lx - nT
  x  <- as.character(x)
  
  idx <- ave(x, x, FUN = function(xx)
    sample(rep(0:1, c(nF[xx[1L]], nT[xx[1L]]))))
  
  !!as.numeric(idx)
}

#' Round vector to target sum
#' 
#' Rounds a numeric vector constrained to sum to a \code{target} value.
#' 
#' @param x numeric values
#' @param target desired sum of \code{x} after rounding
#' 
#' @seealso
#' \code{\link{roundr}}; \code{\link{round_to}}
#' 
#' @examples
#' pcts <- data.frame(
#'   pct1 = c(33.3, 21.5, 45.51),
#'   pct2 = c(33.3, 33.3, 33.3)
#' )
#' 
#' ## base round                                     
#' colSums(mapply(round, pcts))
#' 
#' ## round to target
#' colSums(mapply(Round, pcts, 100))
#' 
#' @export

Round <- function(x, target = NULL) {
  r.x <- round(x)
  diff.x <- r.x - x
  
  if (is.null(target) || (s <- sum(r.x)) == target)
    return(r.x)
  
  if (s > target) {
    select <- seq_along(x)[diff.x != 0]
    wh <- which.max(diff.x[select])
    x[select[wh]] <- r.x[select[wh]] - 1
  } else {
    select <- seq_along(x)[diff.x != 0]
    wh <- which.min(diff.x[select])
    x[select[wh]] <- r.x[select[wh]] + 1
  }
  
  Recall(x, target)
}

#' Round to
#' 
#' Round numerics to nearest multiple of \code{to}.
#' 
#' @param x a numeric vector
#' @param to nearest fraction or integer
#' 
#' \code{\link{roundr}}; \code{\link{Round}}
#' 
#' @examples
#' x <- 1:20 / 10
#' round_to(x, 1)
#' round_to(x, 0.5)
#' 
#' @export

round_to <- function(x, to = 1) {
  to <- abs(to)
  round(x / to) * to
}

#' Pick elements from columns
#' 
#' This function will return \code{\link{colnames}} or column values (if
#' \code{value = TRUE}) for "indicator-like" matrices or data frames.
#' 
#' @param data a data frame or matrix
#' @param ind if \code{value = FALSE} (default), a vector (usually a single
#' value) to match and return column name(s) of \code{data} where \code{ind}
#' is found; if \code{value = TRUE}, a vector of values to be \emph{ignored},
#' e.g., \code{NA}s or empty strings
#' @param value logical; if \code{TRUE}, returns column value(s); otherwise,
#' returns column name(s) (default)
#' @param default for \code{value = FALSE}, the default value returned if
#' a row of \code{data} contains no \code{ind} in any column
#' 
#' @return
#' If \code{value} is \code{FALSE} (default), the column names of \code{data}
#' for which each row of \code{data} contained \code{ind}.
#' 
#' If \code{value} is \code{TRUE}, the column values of \code{data} which are
#' \emph{not} values of \code{ind}.
#' 
#' @examples
#' set.seed(1)
#' ss <- sample(10)
#' dd <- as.matrix(ftable(1:10, ss))
#' 
#' all(pickcol(dd) == ss)
#' 
#' rn <- rnorm(10)
#' dd[dd == 1] <- rn
#' all(pickcol(dd, value = TRUE, ind = 0) == rowSums(dd))
#' 
#' 
#' dd <- data.frame(
#'   x = c(1, 0, 0),
#'   y = c(0, 0, 1),
#'   z = c(0, 1, 0),
#'   a = c('one', '', ''),
#'   b = c('', '', 'three'),
#'   c = c('', 'two', '')
#' )
#' 
#' pickcol(dd[1:2])
#' pickcol(dd[1:2], 0)
#' pickcol(dd[1:3] + 1, ind = 2)
#' 
#' pickcol(dd[4:6], value = TRUE, ind = '')
#' pickcol(dd, value = TRUE, ind = c('', 0:1))
#' 
#' dd[dd == ''] <- NA
#' pickcol(dd[4:6], value = TRUE)
#' 
#' @export

pickcol <- function(data, ind = 1L, value = FALSE, default = NA) {
  res <- apply(data, 1L, function(x) {
    if (value) {
      x[x %in% ind] <- NA
      if (length(x <- x[!is.na(x)]) > 1L)
        toString(x) else x
    } else {
      idx <- x %in% ind
      if (sum(idx))
        toString(names(x[idx])) else default
    }
  })
  
  unname(res)
}

#' Number of unique values
#' 
#' @param x a vector
#' @param na.rm logical; if \code{TRUE}, \code{NA} will not be counted as a
#' unique level; default is to include
#' 
#' @examples
#' x <- c(1:5, NA)
#' lunique(factor(x))
#' lunique(x, TRUE)
#' 
#' @export

lunique <- function(x, na.rm = FALSE) {
  length(unique(if (na.rm) sort(x) else x))
}

#' Remove non ASCII characters
#' 
#' @param x a character vector
#' 
#' @export

rm_nonascii <- function(x) {
  gsub('[^\x20-\x7E]', '', x)
}
