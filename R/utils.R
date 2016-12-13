### utilities
# rawr_ops: %ni%, %==%, %||%, %inside%, %:%
# rawr_ls: lss, lsf, lsp
# rawr_parse: parse_yaml, parse_index, parse_news, parse_namespace
#
# psum, rescaler, clc, clear, bind_all, cbindx, rbindx, rbindfill, interleave,
# outer2, merge2, locf, roll_fun, classMethods, regcaptures, cast, melt, view,
# view2, clist, rapply2, sort_matrix, insert_matrix, tryCatch2, rleid,
# droplevels2, combine_levels
#
# unexported: islist, done, where
###


## is.list(data.frame()); islist(data.frame)
islist <- function(x) inherits(x, 'list')

done <- function() while (TRUE) {beepr::beep(3); Sys.sleep(3)}

## recursively find env where x is defined
where <- function(x, env = parent.frame()) {
  stopifnot(is.character(x), length(x) == 1L)
  if (identical(env, emptyenv())) {
    stop(x, ' not found', call. = FALSE)
  }
  if (exists(x, env, inherits = FALSE))
    env else Recall(x, parent.env(env))
}

#' rawr operators
#' 
#' Some useful binary operators.
#' 
#' \code{\%ni\%} is the negation of \code{\link[base]{\%in\%}}.
#' 
#' \code{\%inside\%} returns a logical vector indicating if \code{x} is inside
#' the \code{interval} (inclusive).
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
#' NULL || TRUE   # error
#' NULL %||% TRUE # TRUE
#' 
#' 1:5 %:% c(3,5)
#' letters %:% c('e', 'n')
#' 
#' ## these are equivalent
#' mtcars %:% c('hp','vs')
#' mtcars %:% c(4, 8)
#' names(mtcars[, 4:8])
#' }
#' 
#' @aliases oror %||% notin %ni% inside %inside% %==%
#' @name rawr_ops
NULL

#' @rdname rawr_ops
#' @export
'%ni%' <- function(x, table) !(match(x, table, nomatch = 0L) > 0L)

#' @rdname rawr_ops
#' @export
'%inside%' <- function(x, interval) {
  interval <- sort(interval)
  x >= interval[1L] & x <= interval[2L]
}

#' @rdname rawr_ops
#' @export
'%==%' <- function(a, b)
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)

#' @rdname rawr_ops
#' @export
'%||%' <- function(a, b) if (!is.null(a)) a else b

#' @rdname rawr_ops
#' @export
'%:%' <- function(object, range) {
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
#' the workspace such as size and dimensions; \code{lsp} lists the
#' \code{p}ackage contents, i.e., exported and/or non exported obejcts (see
#' details); and \code{lsf} lists package \code{f}iles (see details).
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
#' @seealso
#' \code{\link{parse_yaml}}, \code{\link{parse_index}},
#' \code{\link{parse_news}}, \code{\link{parse_namespace}}, \code{\link{ls}},
#' \code{\link{search}}
#' 
#' @return
#' \code{lss} (invisibly) returns a data frame of the printed output.
#' \code{lsf} (invisibly) returns the contents of \code{file}. \code{lsp}
#' returns a vector of character strings.
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
    return(character(0))
  napply <- function(names, fn)
    sapply(names, function(x) fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern, all.names = all.names)
  
  oclass <- napply(names, function(x) as.character(class(x))[1])
  omode <- napply(names, mode)
  otype <- ifelse(is.na(oclass), omode, oclass)
  oprettysize <- napply(names, function(x)
    capture.output(print(object.size(x), units = 'auto')))
  osize <- napply(names, object.size)
  odim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
  vec <- is.na(odim)[ , 1] & (otype != 'function')
  odim[vec, 1] <- napply(names, length)[vec]
  
  out <- setNames(data.frame(otype, osize, oprettysize, odim),
                  c('type', 'size', 'sizef', 'nrow', 'ncol'))
  mb <- sum(napply(names, function(x) object.size(x))) / (1024 * 1024)
  if (mb > 1)
    on.exit(message(sprintf('Total size: %s Mb', roundr(mb, 1))))
  
  if (!is.null(by))
    out <- out[order(out[[by]], decreasing = decreasing), ]
  
  head(out, n)
}

#' @rdname rawr_ls
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
    return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
  } else {
    ## for non base packages
    if (exists('.__NAMESPACE__.', envir = ns, inherits = FALSE)) {
      wh <- get('.__NAMESPACE__.', inherits = FALSE,
                envir = asNamespace(package, base.OK = FALSE))
      what <- if (missing(what)) 'all'
      else if ('?' %in% what) return(ls(wh)) 
      else ls(wh)[pmatch(what[1], ls(wh))]
      if (!is.null(what) && !any(what %in% c('all', ls(wh))))
        stop('\'what\' should be one of ',
             paste0(shQuote(ls(wh)), collapse = ', '),
             ', or \'all\'', domain = NA)
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
        res <- res[[what]]
        return(res[grep(pattern, res, perl = TRUE, ignore.case = TRUE)])
      }
    } else stop(sprintf('no NAMESPACE file found for package %s', package))
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
#' @seealso
#' \code{\link{parseNamespaceFile}}, \code{\link{lss}}, \code{\link{lsf}},
#' \code{\link{lsp}}, \code{\link{parse_sci}}
#' 
#' @return
#' A named list for each section type.
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
                split = '\\$\\$')[[1]]
  x <- strsplit(x, '\\s{2,}')
  as.list(sapply(x, function(xx) `names<-`(xx[2], xx[1])))
}

#' @rdname rawr_parse
#' @export
parse_news <- function(x) {
  ## assume some separator
  x <- Filter(nzchar, gsub('[-=_]{2,}', '', x))
  ## assume version updates state with letter, not -, *, etc
  nn <- x[idx <- grepl('^\\w+', x)]
  sp <- split(x, cumsum(idx))
  setNames(lapply(sp, '[', -1), nn)
}

#' @rdname rawr_parse
#' @export
parse_namespace <- function(x,
    wh = c("import", "export", "exportPattern", "importClass",
           "importMethod", "exportClass", "exportMethod",
           "exportClassPattern", "useDynLib", "nativeRoutine", "S3method")) {
  ## remove comments and collapse
  x <- paste0(gsub('#.*$', '', x), collapse = '')
  mm <- lapply(wh, function(xx)
    gregexpr(sprintf('(?i)%s\\((.*?)\\)', xx), x, perl = TRUE))
  setNames(lapply(mm, function(xx)
    gsub('^\\s+|\\s+$|\\s{2,}', '',
         unlist(strsplit(unlist(regcaptures(x, xx)), ',')))), wh)
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
#' @return
#' A single vector of element-wise sums.
#' 
#' @seealso
#' \code{\link{pmin}}, \code{\link{pmax}}
#' 
#' @references
#' \href{http://stackoverflow.com/questions/13123638/there-is-pmin-and-pmax-each-taking-na-rm-why-no-psum}{SO question}
#' 
#' @examples
#' x <- c(-1, NA, 4, 5)
#' y <- c(NA, NA, 6, -1)
#' psum(x, y)
#' psum(x, y, na.rm = TRUE)
#' 
#' @export

psum <- function(..., na.rm = FALSE) {
  dat <- do.call('cbind', list(...))
  res <- rowSums(dat, na.rm = na.rm)
  idx_na <- !rowSums(!is.na(dat))
  res[idx_na] <- NA
  res
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
#' @seealso
#' \code{\link[scales]{rescale}}; \code{\link[scales]{zero_range}}
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
    if (length(x) != 2) stop('\'x\' must be length one or two')
    if (any(is.na(x)))  return(NA)
    if (x[1] == x[2])   return(TRUE)
    if (all(is.infinite(x))) return(FALSE)
    m <- min(abs(x))
    if (m == 0) return(FALSE)
    abs((x[1] - x[2]) / m) < tol
  }
  if (zero_range(from) || zero_range(to))
    return(rep(mean(to), length(x)))
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

#' Clear workspace
#' 
#' Clear the workspace by removing all objects in \code{\link{ls}}.
#' 
#' @param all.names logical; if \code{TRUE}, also removes hidden (dot) objects
#' 
#' @export

clc <- function(all.names = FALSE)
  rm(list = ls(.GlobalEnv, all.names = all.names), envir = .GlobalEnv)

#' Clear console
#' 
#' Clear the console window.
#' 
#' @param ... ignored
#' 
#' @export

clear <- function(...) cat('\014')

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
    stop('specify combining method: \'rbind\' or \'cbind\'')
  l <- list(...)
  if (any(sapply(l, function(x) !is.null(dim(x)))))
    warning('bind_all is intended for vectors: use cbindx or rbindx instead.')
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
      } else rbindx(x, matrix(nrow = maxRow - nrow(x), ncol = ncol(x))))
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
      } else cbind(x, matrix(nrow = nrow(x), ncol = maxCol - ncol(x))))
    
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
#' \dontrun{
#' interleave(d1, m2, which = 'rbind')  ## error
#' interleave(d1, m2, which = 'rbindx') ## works
#' }
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

merge2 <- function(l, ...)
  Reduce(function(x, y) merge(x, y, ...), l)

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
        ## this throws an error for special cases like dates
        x[x == unlist(na.strings[ii])] <- NA
      }, silent = TRUE)
  indx <- !is.na(x)
  
  x[] <- lapply(seq_along(x), function(ii) {
    if (fromLast[ii]) {
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
    x else x[, 1]
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

#' Show class methods
#' 
#' Lists available methods for a given class.
#' 
#' @param class an object or classes as a vector of character strings
#' 
#' @seealso
#' \code{\link{methods}}, \code{\link{S3Methods}}, \code{\link{class}}
#' 
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
#' @param pattern a character string containing a regular expression
#' @param use.names logical; if \code{FALSE}, all names (capture names and
#' list names) will be stripped; if \code{TRUE} (default) and capture groups
#' have names, these will be used; otherwise, match start positions will be
#' used
#' 
#' @return
#' A list with a matrix of captures for each string in \code{x}. Note that the
#' column names of each matrix will be the starting positions of the captures.
#' 
#' @seealso \code{\link{regmatches}}; \code{\link{grep}}; \code{\link{regex}}
#' 
#' @references \url{https://gist.github.com/MrFlick/10413321}
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
#' ## regcaptures2 is a convenience function for the two-step
#' regcaptures2(x, p1)
#' 
#' ## both will use named captures (if perl = TRUE)
#' regcaptures(x, gregexpr(p2, x, perl = TRUE))
#' do.call('rbind.data.frame', regcaptures2(x, p2))
#' 
#' 
#' x <- 'ACCACCACCAC'
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
  on.exit(options(stringsAsFactors = getOption('stringsAsFactors')))
  options(stringsAsFactors = FALSE)
  
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
    cn <- data.frame(attr(m, 'capture.names'))
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
  
  Map(function(x, s, l, n)
    Substring(x, s, l, n), x, cs, cl, cn, USE.NAMES = use.names)
}

#' @rdname regcaptures
#' @export

regcaptures2 <- function(x, pattern, use.names = TRUE)
  regcaptures(x, gregexpr(pattern, x, perl = TRUE), use.names)

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

#' View data
#' 
#' Convenience functons to use the base \code{R} data viewer (\code{view}
#' always invokes \code{\link[utils]{View}} instead of the rstudio viewer)
#' or the default browser (\code{view2} which can open html and widgets in
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
#' view2(mtcars)
#' view2(htmlTable::htmlTable(mtcars))
#' 
#' \dontrun{
#' view2(qtlcharts::iplot(1:5, 1:5))
#' }
#' 
#' @name rawr_view
#' @export

#' @rdname rawr_view
#' @export
view <- function(x, title, ...)
  utils::View(x, title)

#' @rdname rawr_view
#' @export
view2 <- function(x, use_viewer = FALSE, ...) {
  if (is.data.frame(x) | is.matrix(x))
    x <- DT::datatable(x)
  htmlFile <- tempfile(fileext = '.html')
  if (inherits(x, 'htmlwidget'))
    htmlwidgets::saveWidget(x, htmlFile, selfcontained = TRUE) else
      writeLines(x, con = htmlFile)
  if (use_viewer)
    tryCatch(rstudioapi::viewer(htmlFile),
             error = function(e) {
               message('Viewer not available - opening in browser.\n',
                       'In RStudio, try installing the \'rstudioapi\' package.',
                       domain = NA)
               browseURL(htmlFile)
             }) else browseURL(htmlFile)
}

#' Concatenate lists
#' 
#' Combine lists with mixed data types "horizontally."
#' 
#' @param x,y \emph{uniquely-named} lists or nested lists with each pair
#' of non-\code{NULL} elements having identical classes
#' 
#' @return
#' A lists with all elements from \code{x} and \code{y} joined using
#' \code{\link{cbind}} for matrices, \code{\link{cbind.data.frame}} for
#' data frames, lists for factors, and \code{\link{c}} otherwise.
#' 
#' @seealso
#' \code{\link{nestedMerge}}, \code{\link{modifyList}}
#' 
#' @examples
#' f <- function(x) boxplot(mpg ~ vs, data = x, plot = FALSE)
#' bp1 <- f(mtcars[mtcars$vs == 0, ])
#' bp2 <- f(mtcars[mtcars$vs == 1, ])
#' bp  <- f(mtcars)
#' stopifnot(identical(clist(bp1, bp2), bp))
#' 
#' l1 <- list(x = factor(1:5), y = matrix(1:4, 2),
#'            z = head(cars), l = list(zz = 1:5))
#' l2 <- list(z = head(cbind(cars, cars)), x = factor('a'),
#'            l = list(zz = 6:10))
#' 
#' clist(l1, l2)
#' clist(l1, list(zzz = data.frame(1)))
#' 
#' @export

clist <- function (x, y) {
  get_fun <- function(x, y)
    switch(class(x %||% y),
           matrix = cbind,
           data.frame = function(x, y)
             do.call('cbind.data.frame', Filter(Negate(is.null), list(x, y))),
           factor = function(...) unlist(list(...)), c)
  
  stopifnot(islist(x), islist(y))
  nn <- names(rapply(c(x, y), names, how = 'list'))
  if (is.null(nn) || any(!nzchar(nn)))
    stop('All non-NULL list elements should have unique names', domain = NA)
  
  nn <- unique(c(names(x), names(y)))
  z <- setNames(vector('list', length(nn)), nn)
  
  for (ii in nn)
    z[[ii]] <- if (islist(x[[ii]]) && islist(y[[ii]]))
      Recall(x[[ii]], y[[ii]]) else
        (get_fun(x[[ii]], y[[ii]]))(x[[ii]], y[[ii]])
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
#' \code{"any"} to apply to every non-\code{\link{list}} element of \code{l}
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


rapply2 <- function(l, FUN, classes = 'any', ...,
                    check.nested = 'list' %in% classes) {
  stopifnot(islist(l))
  FUN <- match.fun(FUN)
  is.nested <- if (check.nested)
    function(l) any(vapply(l, islist, NA)) else function(l) FALSE
  for (ii in seq_along(l))
    l[[ii]] <- if (is.nested(l[[ii]]) ||
                   (islist(l[[ii]]) & ('list' %ni% classes)))
      Recall(l[[ii]], FUN, classes, ..., check.nested = check.nested) else
        if (any(classes == 'any') || inherits(l[[ii]], classes))
          FUN(l[[ii]], ...) else l[[ii]]
  if ('list' %in% classes & !identical(FUN, unlist))
    FUN(l, ...) else l
}

# depth <- function(this, thisdepth = 0L) {
#   # http://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r
#   if (!islist(this))
#     return(thisdepth)
#   lapply(this, depth, thisdepth + 1L)
# }

#' Sort matrix
#' 
#' Sort a matrix (or an object that can be coerced) by values in rows or
#' columns.
#' 
#' @param m a matrix
#' @param margin margin to sort by; default is to sort based on row values
#' (\code{margin = 1}) but values by column may be used (\code{margin = 2})
#' @param order vector specifying all unique values of \code{m} in the
#' desired order; if missing, the order will be the sorted unique values
#' of \code{m}
#' 
#' @examples
#' set.seed(1)
#' (m <- matrix(rpois(5 * 10, 1), 5))
#' 
#' ## sort columns by decreasing row values
#' sort_matrix(m)
#' 
#' ## sort rows by decreasing column values
#' sort_matrix(m, 2)
#' 
#' ## sort 0s first followed by 3, 1, 2
#' sort_matrix(m, order = c(0,3,1,2))
#' 
#' @export

sort_matrix <- function(m, margin = 1L, order) {
  stopifnot(sum(1:2 == margin) == 1L)
  m <- if (margin == 1L)
    as.matrix(m) else t(as.matrix(m))
  if (missing(order))
    order <- sort(unique(c(m)), decreasing = TRUE)
  stopifnot(length(order) != length(unique(m)))
  dd <- data.frame(t(m))
  dd[] <- lapply(dd, factor, levels = order)
  m <- m[, do.call('order', dd)]
  if (margin == 1L)
    m else t(m)
}

#' Insert rows or columns
#' 
#' Insert rows and/or columns into a matrix.
#' 
#' @param m a matrix
#' @param rowsep,colsep index of row or column to shift right or down,
#' respectively
#' @param rowrep,colrep row and column replacement values, recycled if needed;
#' note that rows are replaced first followed by columns, so for the opposite
#' behavior, transpose \code{m} before and after using \code{insert_matrix}
#' and swap \code{rowrep} and \code{colrep}; see examples
#' 
#' @examples
#' m <- col(matrix(0, 5, 5))
#' insert_matrix(m, 2, c(2,4,4,4))
#' 
#' ## anticipate number of values needed for replacement(s)
#' insert_matrix(m, 4, 4:5, colrep = 1:6)
#' 
#' ## these are _almost_ identical
#' insert_matrix(m, 5, 5, 0, 1) == t(insert_matrix(t(m), 5, 5, 1, 0))
#' 
#' @export

insert_matrix <- function(m, rowsep, colsep, rowrep = NA, colrep = rowrep) {
  # insert_matrix(m, 4, c(5, 7))
  # insert_matrix(m, 4, c(5, 7), sample(22), 0)
  im_ <- function(m, idx, repl = NA) {
    nr <- nrow(m)
    ii <- sort(c(1:ncol(m), idx))
    m <- m[, ii]
    ri <- idx + seq_along(idx) - 1
    ii <- cbind(rep(1:nr, length(idx)), rep(ri, each = nr))
    m[ii] <- repl
    if (!is.null(colnames(m)))
      colnames(m)[ri] <- ''
    m
  }
  if (!missing(rowsep))
    m <- t(im_(t(m), rowsep, rowrep))
  if (!missing(colsep))
    m <- im_(m, colsep, colrep)
  m
}

#' tryCatch2
#' 
#' Modification of \code{\link{tryCatch}} which allows recovery from errors,
#' warnings, messages, and interrupts while returning the value from
#' \code{expr}.
#' 
#' @param expr an expression
#' 
#' @return
#' A list of at least \code{$value}. If \code{expr} is evaluated with no error,
#' warning, messages, or interrupts, then a list with a single element,
#' \code{$value}, is returned with the value of \code{expr}. If any other
#' condition is triggered, a corresponding element will be appended to the
#' list with \code{$value}. For errors, \code{$value} will be \code{NULL}.
#' 
#' @author
#' Martin Morgan, Jan Gorecki, Robert Redd (modifications)
#' 
#' @seealso
#' \href{http://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function}{SO question};
#' \code{\link[logR]{tryCatch2}}; \href{https://github.com/jangorecki/logR}{
#' \pkg{logR} github repo}
#' 
#' @examples
#' tryCatch2(1)
#' tryCatch2(stop('halt at once!'))
#' 
#' l <- alist(1, simpleError('oops'), stop('halt!'),
#'            warning('hmm'), message('hey'))
#' lapply(l, function(x) tryCatch2(eval(x)))
#' 
#' l <- lapply(list(1,-1,'a'), function(x) tryCatch2(log(x)))
#' rm_null(l)
#' 
#' @export

tryCatch2 <- function(expr) {
  E <- W <- M <- I <- V <- NULL
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
  V <- suppressMessages(withCallingHandlers(
    tryCatch(expr, error = e.handler, interrupt = i.handler),
    warning = w.handler, message = m.handler))
  l <- list(error = E, warning = W, message = M, interrupt = I)
  c(list(value = V), Filter(Negate(is.null), l))
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

rleid <- function(x)
  cumsum(c(1L, x[-length(x)] != x[-1L]))

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
#' x <- factor(c('b','d'), levels = letters[1:5])
#' droplevels(x)
#' droplevels2(x)
#' droplevels2(x, min_level = 2, max_level = 5)
#' 
#' @export

droplevels2 <- function(x, min_level = 1, max_level = max(as.numeric(x))) {
  stopifnot(is.factor(x))
  factor(x, levels = levels(x)[min_level:max_level], ordered = is.ordered(x))
}

#' Combine factor levels
#' 
#' Convenience function to combine multiple levels of a factor into a new or
#' existing level(s).
#' 
#' @param x a vector
#' @param levels a vector of unique values of \code{x} to combine; to combine
#' values into multiple groups, use a list
#' @param labels a vector of new labels; if \code{levels} is a vector,
#' \code{labels} should be length 1; if \code{levels} is a list, \code{labels}
#' (need not be a list but) should have one value for each list element of
#' \code{levels}
#' 
#' @seealso
#' \code{\link{recoder}}
#' 
#' @examples
#' x <- mtcars$gear
#' combine_levels(factor(x, ordered = TRUE), 3:4, 6)
#' combine_levels(x, list(3:4, 5), c(3, 6))
#' 
#' combine_levels(iris$Species, c('setosa', 'virginica'), 'setosa')
#' combine_levels(iris$Species, list(c('setosa', 'virginica'), 'versicolor'),
#'                c('Setosa/Virginica','Versicolor'))
#' 
#' @export

combine_levels <- function(x, levels, labels) {
  levels <- if (is.list(levels))
    levels else list(levels)
  labels <- as.list(labels)
  stopifnot(length(levels) == length(labels))
  
  ## less code but requires rawr::recoder
  # for (ii in seq_along(levels))
  #   x <- recoder(x, levels[[ii]], labels[[ii]])
  # x
  
  xf <- as.factor(x)
  xc <- as.character(xf)
  xl <- levels(xf)

  for (ii in seq_along(levels)) {
    xl[xl %in% unlist(levels[[ii]])] <- unlist(labels[[ii]])
    xc[xc %in% levels[[ii]]] <- labels[[ii]]
  }

  factor(xc, unique(xl), ordered = is.ordered(x))
}
