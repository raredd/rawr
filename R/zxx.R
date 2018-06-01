### some random shit
# ht, progress, recoder, identical2, all_equal2, search_df, search_hist,
# fapply, try_require, list2file, Restart, helpExtract, Round, round_to,
# updateR, read_clip, read_clip.csv, read_clip.tab, read_clip.fwf, icols,
# fill_df, kinda_sort, sym_sort, rgene, install_temp, nestedMerge, nestedmerge,
# path_extract, fname, file_name, file_ext, rm_ext, mgrepl, mgrep, msub, mgsub,
# flatten, tree, rm_null, cum_reset, cum_na, cumsum_na, cumprod_na, cummax_na,
# cummin_na, cum_mid, vgrep, vgrepl, justify, factors, sample_each, pickcol,
# lunique, rm_nonascii
# 
# unexported:
# helpExtract_, mgrep_, msub_, fill_spaces_
###


#' \code{head}/\code{tail}
#' 
#' \code{\link{rbind}} the \code{\link{head}} and \code{\link{tail}} of an
#' object.
#' 
#' @param x an object
#' @param n an integer giving the first and last \code{n / 2} elements if
#' positive or the middle \code{n} elements if negative
#' @param sep separator
#' 
#' @examples
#' ht(letters, 6, '...')
#' ht(letters, -6)
#' 
#' 
#' mt <- cbind(mtcars, n = seq.int(nrow(mtcars)))
#' 
#' ## ends
#' ht(mt)
#' ht(mt, sep = '...')
#' 
#' ## middle
#' ht(as.matrix(mt), -6)
#' ht(mt, -6)
#' ht(mt, -6, sep = '...')
#' 
#' @export

ht <- function(x, n = 6L, sep = NULL) {
  pn <- abs(n) / 2
  FUN <- if (is.null(dim(x)))
    function(...) setNames(c(...), NULL) else 'rbind'
  FUN <- match.fun(FUN)
  
  if (n < 0L) {
    idx <- cut(seq.int(NROW(x)), breaks = 2L, labels = 1:2)
    x <- if (is.null(dim(x)))
      list(x[idx %in% '1'], x[idx %in% '2'])
    else
      list(
        x[idx %in% '1', , drop = FALSE],
        x[idx %in% '2', , drop = FALSE]
    )
    
    FUN(' ' = sep, tail(x[[1L]], pn), head(x[[2L]], pn), '  ' = sep)
  } else FUN(head(x, pn), ' ' = sep, tail(x, pn))
}

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

progress <- function(value, max.value, textbar = FALSE) {
  if (!is.numeric(value))
    stop('\'value\' must be numeric')
  
  oo <- options(scipen = 10)
  on.exit(options(oo))
  
  percent <- if (missing(max.value)) {
    max.value <- 100
    TRUE
  } else FALSE
  
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
    m <- getOption('width') - 5L
    pct <- as.numeric(value) / as.numeric(max.value)
    r <- floor(pct * m)
    backspaces <- f(rep('\b', m * 2))
    
    message <- if (erase.only)
      '' else {
        message <- f('|', f(rep('=', max(0, r - 1))),
                     f(rep(' ', max(0, m - r))), '|')
        cat(backspaces, message, sprintf('  %s%%', round(pct * 100)), sep = '')
      }
  } else {
    if (percent) {
      backspaces <- f(rep('\b', l + 14L))
      message <- if (erase.only)
        '' else sprintf('Progress: %s%%', round(value))
      cat(backspaces, message, sep = '')
    } else {
      backspaces <- f(rep('\b', 2 * l + 17L))
      message <- if (erase.only)
        '' else sprintf('Progress: %s of %s  ', value, max.value)
      cat(backspaces, message, sep = '')
    }
  }
  if (.Platform$OS.type == 'windows')
    flush.console()
  cat('\n')
}

#' Recode variables
#' 
#' Recodes numeric, character, and factor values in a vector, list, matrix,
#' or data frame.
#' 
#' When recoding a factor variable with a new level, \code{recoder}
#' automatically adds the corresponding level to \code{levels(object)} to
#' avoid errors.
#' 
#' The function currently recursively replaces \code{pattern[i]} with
#' \code{replacement[i]} in sequential order, so if you intend to swap values,
#' say \code{a} and \code{b}, in an \code{object}, \code{recoder} will instead
#' first replace all occurrences of \code{a} with \code{b} and then all
#' occurrences of \code{b} with \code{a} resulting in the \code{object} with
#' no \code{b} occurrences; see examples. I will (may) fix this eventually.
#' 
#' @param object object to recode
#' @param pattern what to replace
#' @param replacement what to replace \code{pattern} with
#' @param ... ignored
#' 
#' @return
#' An object with the same length (or dimensions) and class as \code{object}
#' with the recoded variables.
#' 
#' @seealso
#' \code{\link{fill_df}}; \code{\link[car]{recode}};
#' \code{\link{combine_levels}}
#' 
#' @examples
#' recoder(mtcars$carb, c(1, 2), c('A','B'))
#' recoder(mtcars, c(1, 2), c('A', 'B'))
#' 
#' mtcars <- within(mtcars, carb1 <- factor(carb))
#' recoder(mtcars$carb1, 1, 999)
#' 
#' tmp <- c(list(1:5), list(5), list(NA))
#' recoder(tmp, 5, NA)
#' 
#' ## example from note
#' tmp <- 1:10
#' recoder(tmp, c(1, 2), c(2, 1))
#' # [1]  1  1  3  4  5  6  7  8  9 10    ## actual return
#' # [1]  2  1  3  4  5  6  7  8  9 10    ## desired return
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
  op <- options(stringsAsFactors = FALSE)
  on.exit(options(op))
  
  if (is.factor(object)) {
    lvl <- setdiff(replacement, levels(object))
    if (length(lvl))
      cat('level(s)', levels(factor(levels = lvl)),
          'added to factor variable', deparse(m$object),'\n')
    levels(object) <- c(levels(object), replacement)
    # object <- droplevels(object)
  }
  if (length(replacement) == 1L)
    replacement <- rep(replacement, length(pattern))
  
  ## helper functions
  splitter <- function(df) setNames(split(t(df), 1:ncol(df)), names(df))
  switcher <- function(f, g, h) {
    if (is.na(g))
      f[is.na(f)] <- h else f[f == g] <- h
    f
  }
  superswitcher <- function(x, y, z){
    DF <- data.frame(y, z, stringsAsFactors = FALSE)
    z <- x
    if (class(DF[, 2]) %in% c('character', 'factor')) {
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
    z
  }
  
  # treat certain object classes differently
  if (is.vector(object) & !is.list(object)) {
    sapply(object, superswitcher, pattern, replacement)
  } else {
    if (is.data.frame(object)) {
      tmp <- do.call('data.frame',
                     lapply(unclass(object)[1:ncol(object)],
                            superswitcher, pattern, replacement))
      rownames(tmp) <- attr(object, 'row.names')
      return(tmp)
    }
    if (is.matrix(object)) {
      nrow <- nrow(object)
      tmp <- do.call('rbind',
                     lapply(object, superswitcher, pattern, replacement))
      tmp <- matrix(tmp, nrow = nrow, byrow = FALSE)
      return(tmp)
    } else {
      if (is.factor(object))
        factor(unlist(lapply(object, superswitcher, pattern, replacement)),
               levels(object), ordered = is.ordered(object))
      else lapply(object, superswitcher, pattern, replacement)
    }
  }
}

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
  
  l <- sapply(1:(length(l) - 1L), function(ii)
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
  
  l <- lapply(1:(length(l) - 1L), function(x)
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
#' dd <- data.frame(islands = names(islands)[1:32], mtcars)
#' search_df(New, dd, islands)
#' search_df(ho, dd, islands, var = 0.2) # too much variation
#' search_df(ho, dd, islands, var = 0)
#' search_df('Axel Hieberg', dd, islands) # misspelled, not enough variation
#' search_df('Axel Hieberg', dd, islands, var = 2)
#' search_df(19, dd, mpg)
#' 
#' @export

search_df <- function(pattern, data, col.name, var = 0,
                      ignore.case = TRUE, ...) {
  p <- as.character(substitute(pattern))
  x <- as.character(substitute(col.name))
  idx <- agrep(p, data[, x], ignore.case = ignore.case,
               max.distance = var, ...)
  data[idx, ]
}

#' Search history
#' 
#' Searches \code{.Rhistory} file for pattern matches.
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
                   warning = function(w) message('No history found'),
                   finally = return(invisible(NULL)))
  lhist <- length(hist)
  if (is.numeric(x))
    hist[lhist:(lhist - x + 1L)]
  else if (is.character(x))
    grep(x, readLines('.Rhistory'), value = TRUE, ...)
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
  fn <- as.character(match.call()$FUN)[-1L]
  res <- sapply(FUN, mapply, X, ...)
  setNames(as.data.frame(res), fn)
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
                    as.character(substitute(package)), package)
  available <- suppressMessages(
    suppressWarnings(
      sapply(package, require, quietly = TRUE,
             character.only = TRUE, warn.conflicts = FALSE)
    ))
  missing <- package[!available]
  
  if (length(missing) > 0L)
    stop(paste(package, collapse = ', '), ' package not found.')
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
  if (!islist(l))
    stop('\'l\' must be a list')
  if (is.null(names(l)) || any(is.na(names(l))))
    stop('all elements of \'l\' must be named')
  if (any(sapply(l, class) %ni% c('data.frame', 'matrix')))
    stop('all elements of \'l\' should be class \'matrix\' or \'data.frame\'')
  if (!file.exists(targetdir)) {
    message(sprintf('creating directory:\n%s', targetdir))
    dir.create(targetdir)
  }
  
  e <- new.env()
  list2env(l, envir = e)
  
  if (missing(sep))
    sapply(names(l), function(x)
      save(x, file = sprintf('%s/%s.rda', targetdir, x), ...))
  else sapply(names(l), function(x)
    write.table(get(x, envir = e), sep = sep, ...,
                file = sprintf('%s/%s.%s', targetdir, x,
                               ifelse(sep == ',', 'csv', 'dat'))))
  
  message(sprintf('NOTE: %s written to %s', iprint(names(l)), targetdir))
  
  invisible(NULL)
}

#' Restart \code{R} session
#' 
#' Ends current and restarts a clean \code{R} session.
#' 
#' @param afterRestartCommand character string of command(s) to be
#' executed after restarting
#' 
#' @examples
#' \dontrun{
#' Restart("clear(); cat('Here is a clean session just for you')")
#' }
#' 
#' @export

Restart <- function(afterRestartCommand = '') {
  (getOption('restart'))(afterRestartCommand)
}

# Reload <- function(...) {
#   ## clean (rstudio) r session packages:
#   pkgs <- c(".GlobalEnv", "tools:rstudio", "package:stats", "package:graphics",
#             "package:grDevices", "package:utils", "package:datasets",
#             "package:methods", "Autoloads", "package:base")
#   to_unload <- setdiff(search(), pkgs)
#   
#   for (pkg in to_unload)
#     try(detach(pkg, unload = TRUE, character.only = TRUE), silent = TRUE)
#   rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
#   cat('\014')
#   
#   invisible(NULL)
# }

#' Extract \code{R} help files
#' 
#' Extracts specified portions of R help files (from \emph{loaded} libraries)
#' for use in Sweave or R-markdown documents.
#' 
#' The \code{type} argument accepts:
#' 
#' \tabular{llllll}{
#' \tab \code{text}    \tab \tab \tab \tab plain text \cr
#' \tab \code{md_code} \tab \tab \tab \tab markdown code chunks; for use with
#' markdown documents when highlighted code is expected \cr
#' \tab \code{md_text} \tab \tab \tab \tab markdown plain text; for use with
#' markdown documents where regular text is expected \cr
#' \tab \code{sw_code} \tab \tab \tab \tab sweave code chunks; for use with
#' Sweave documents where highlighted code is expected \cr
#' \tab \code{sw_text} \tab \tab \tab \tab sweave plain text; for use with
#' Sweave documents where regular text is expected \cr
#' }
#' 
#' To see the results in the console:
#' 
#' \code{cat(helpExtract(print, type = 'md_text'))}
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
#' @param FUN a function as name or character string
#' @param show.sections logical; if \code{TRUE}, returns \code{section} options
#' for \code{FUN}
#' @param section section to extract (default is \code{"Usage"}
#' @param type type of character vector you want returned; default is
#' \code{"m_code"}, see details
#' @param ... additional arguments passed to \code{\link[utils]{help}}
#' 
#' @return
#' A character vector to be used in a Sweave or Rmarkdown document.
#' 
#' @examples
#' helpExtract(print)
#' cat(helpExtract(print, section = 'ex'), sep = '\n')
#' cat(helpExtract(print, type = 'md_text', section = 'description'))
#' 
#' ## selecting multiple sections prints section names
#' cat(helpExtract(print, section = c('references', 'see also')), sep = '\n')
#' 
#' @export

helpExtract <- function(FUN, show.sections = FALSE, section = 'Usage',
                        type = c('text','md_code','md_text',
                                 'sw_code','sw_text'), ...) {
  type <- match.arg(type)
  FUN  <- if (is.function(FUN))
    deparse(substitute(FUN)) else as.character(FUN)
  x <- helpExtract_(FUN, ...)
  
  ## section start lines
  B <- grep('^_\b._\b._', x)
  x <- gsub('_\b', '', x, fixed = TRUE)
  if (show.sections)
    return(gsub(':','', x[B]))
  
  X <- rep_len(0L, length(x))
  X[B] <- 1L
  res <- split(x, cumsum(X))
  
  res <- res[which(sapply(res, function(x)
    any(Vectorize(grepl)(section, x[1L], ignore.case = TRUE))))]
  # res <- unlist(sapply(res, '[', -(1:2)))
  res <- if (length(section) > 1L)
    unname(unlist(res)) else res[[1L]][-(1:2)]
  
  while (TRUE) {
    res <- res[-length(res)]
    if (nzchar(res[length(res)]))
      break
  }
  
  switch(type,
         text = res,
         md_code = c('```r', res, '```'),
         sw_code = c('<<>>=', res, '@'),
         md_text = paste('    ', res, collapse = '\n'),
         sw_text = c('\\begin{verbatim}', res, '\\end{verbatim}')
  )
}

helpExtract_ <- function(FUN, ...) {
  # (helpExtract_('print'))
  stopifnot(is.character(FUN))
  
  ## tools:::fetchRdDB
  fetchRdDB <- function(filebase, key = NULL) {
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
      res else invisible(res)
  }
  
  ## utils:::.getHelpFile
  getHelpFile <- function(file) {
    path <- dirname(file)
    dirpath <- dirname(path)
    if (!file.exists(dirpath))
      stop(gettextf("invalid %s argument", sQuote("file")), domain = NA)
    pkgname <- basename(dirpath)
    RdDB <- file.path(path, pkgname)
    if (!file.exists(paste(RdDB, "rdx", sep = ".")))
      stop(gettextf(paste("package %s exists but was not installed under R',
                          '>= 2.10.0 so help cannot be accessed"),
                    sQuote(pkgname)), domain = NA)
    fetchRdDB(RdDB, basename(file))
  }
  
  x <- capture.output(
    tools::Rd2txt(getHelpFile(utils::help(FUN, ...)),
                  options = list(sectionIndent = 0))
  )
  
  invisible(x)
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
  
  if ((s <- sum(r.x)) == target)
    return(r.x)
  else if (s > target) {
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

#' Round to
#' 
#' Round numerics to nearest multiple of \code{to}.
#' 
#' @param x a numeric vector
#' @param to nearest fraction or integer
#' 
#' @examples
#' x <- 1:20 / 10
#' round_to(x, 1)
#' round_to(x, 0.5)
#' 
#' @export

round_to <- function(x, to = 1) {
  round(x / to) * to
}

#' Update \code{R}
#' 
#' Copies and updates \code{R} libraries from most recent installed version
#' into the current \code{\link{.libPaths}} directory. This assumes that the
#' user has installed a new \code{X.x} version of \code{R} but will not copy
#' any libraries from previous frameworks into the new library.
#' 
#' @param update logical; if \code{TRUE}, checks for available packages
#' updates, downloads, and installs
#' 
#' @seealso
#' \code{\link{update.packages}}
#' 
#' @export

updateR <- function(update = TRUE) {
  path <- file.path(R.home(), '..', '..')
  
  v <- tail(sort(list.files(path, pattern = '^\\d{1}.\\d{1}$')), 2L)
  if (!grepl(v[2L], .libPaths()))
    stop('A more recent version of R was found on your system\n')
  
  if (file.exists(v_last <- sub(v[2L], v[1L], .libPaths()))) {
    pkg <- list.files(.libPaths())
    pkg <- setdiff(list.files(v_last), pkg)
    if (length(pkg) > 0L) {
      cat(sprintf("Copying %s package%s to %s\n", length(pkg),
                  ifelse(length(pkg) > 1L, 's', ''), .libPaths()))
      file.copy(file.path(v_last, pkg), .libPaths(), recursive = TRUE)
    } else cat('No packages to copy\n')
  }
  
  if (update) {
    if ((up <- table(packageStatus()$inst$Status)['upgrade']) > 0L) {
      cat(sprintf('Updating %s package%s\n', up, ifelse(up > 1L, 's', '')))
      update.packages(ask = FALSE)
    } else cat('All packages are up-to-date\n')
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
#' @seealso
#' \code{\link[psych]{read.clipboard}}; \code{\link{read.table}};
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
read_clip.csv <- function(header = TRUE, sep = ',', ...) {
  read_clip(header = header, sep = sep, ...)
}

#' @rdname read_clip
#' @export
read_clip.tab <- function(header = TRUE, sep = '\t', ...) {
  read_clip(header = header, sep = sep, ...)
}

#' @rdname read_clip
#' @param widths a vector of widths of the fixed-width fields or a list of
#' vectors giving the widths for multiple lines
#' @export
read_clip.fwf <- function(header = TRUE, widths, ...) {
  if (Sys.info()['sysname'] %ni% 'Darwin')
    read.fwf(file = 'clipboard', header = header, widths = widths, ...)
  else read.fwf(file = pipe('pbpaste'), header = header, widths = widths, ...)
}

#' Index columns by pattern
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
#' icols(iris, 'Petal')
#' icols(iris, '\\.')
#' icols(mtcars, '^[\\w]{2}$')
#' 
#' @export

icols <- function(x, pattern, keep, ...) {
  keep <- if (missing(keep)) NULL else which(colnames(x) %in% keep)
  x[, c(keep, grep(pattern, colnames(x), perl = TRUE, ...)), drop = FALSE]
}

#' Fill data frame
#' 
#' Fills data frame, \code{data}, containing \code{NA} values using a look-up
#' table, \code{key}. \code{ids} and \code{fill} columns must be in both
#' \code{data} and \code{key}. If neither are given, \code{fill_df} will
#' smartly try to guess which columns need to be filled with the values from
#' the look-up table.
#' 
#' @param data a data frame to recode
#' @param key a look-up table data frame
#' @param ids columns treated as id variables, as character strings or indices
#' @param fill columns to recode, as character strings or column indices
#' @param values optional vector of values to recode with \code{fill}; if
#' missing (default), \code{fill_df} only looks for \code{NA}s in \code{data};
#' otherwise, all occurrences of \code{values} will be replaced with
#' \code{NA}, and \code{fill_df} will procede normally
#' 
#' @return
#' A data frame with \code{NA}s from \code{fill}-columns recoded to match
#' the values from \code{key}.
#' 
#' @seealso
#' \code{\link{recoder}}; \code{\link{locf}}
#' 
#' @examples
#' dd <- mtcars
#' dd[matrix(sample(c(TRUE, FALSE), 32 * 11, replace = TRUE), 32)] <- NA
#' identical(mtcars, fill_df(dd, mtcars))  ## TRUE
#' 
#' ## recode other variables instead of NAs
#' nn <- sum(is.na(dd))
#' dd[is.na(dd)] <- sample(-10:-1, nn, replace = TRUE)
#' identical(mtcars, fill_df(dd, mtcars, values = -1:-10)) ## TRUE
#' 
#' f <- function(x, n = 20) sample(x, size = n, replace = TRUE)
#' set.seed(1)
#' key_df <- data.frame(id = c(1,2,1,2), group = c(3,3,4,4),
#'                      x = c(100, 200, 300, 400), y = I(LETTERS[1:4]))
#' na_df <- data.frame(id = f(1:2), group = f(3:4),
#'                     x = f(c(0, NA)), y = I(f(c('', NA))), z = 1)
#' 
#' ## auto: all cols with no NAs == ids; cols with any NAs = fill
#' fill_df(na_df, key_df)
#' 
#' ## select which to be filled and returned
#' fill_df(na_df, key_df, ids = 1:2, fill = 'x')
#' fill_df(na_df, key_df, ids = 1:2, fill = 4)
#' 
#' @export

fill_df <- function(data, key, ids, fill, values) {
  nn <- names(data)
  ## if given replace "values" with NAs
  if (!missing(values)) {
    idx <- data
    idx[] <- lapply(data, function(x) x %in% values)
    data[as.matrix(idx)] <- NA
  }
  
  ## get columns names not defined as ids or fill
  if (length(whk <- which(nn %ni% names(key)))) {
    whk <- nn[whk]
    keep <- data[, whk, drop = FALSE]
    data[, whk] <- NULL
  } else keep <- NULL
  
  ## error checks
  nd <- names(data)
  nad <- vapply(data, anyNA, logical(1L))
  if (all(!nad))
    return(data)
  
  ## try to guess columns to use for ids/fill
  ids <- if (missing(ids)) {
    ids <- nd[which(!nad)]
    message('\'ids\' : ', paste(ids, collapse = ', '), domain = NA)
    ids
  } else if (is.numeric(ids)) nd[ids] else ids
  fill <- if (missing(fill)) {
    fill <- nd[which(nad)]
    message('\'fill\': ', paste(fill, collapse = ', '), domain = NA)
    fill
  } else if (is.numeric(fill))
    nd[fill] else fill
  
  ## match current data rows with rows in key and fill NAs
  ok <- all(nad)
  nak <- if (ok)
    seq.int(nrow(data)) else do.call('paste0', c(key[, ids, drop = FALSE]))
  
  dfk <- if (ok)
    seq.int(nrow(data)) else do.call('paste0', c(data[, ids, drop = FALSE]))
  mm <- match(dfk, nak)
  
  for (col in fill) {
    nnr <- which(is.na(data[, col]))
    data[nnr, col] <- key[mm[nnr], col]
  }
  
  # data[do.call('order', as.list(data[, c(nnk, nnf)])), ]
  if (!is.null(keep))
    cbind.data.frame(data, keep)[, nn, drop = FALSE]
  else data[, nn, drop = FALSE]
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
    as.integer(nn)
  else res
}


#' Generate random gene names
#' 
#' Generate random character strings from pools of letters and digits.
#' 
#' @param n number of gene names to return
#' @param alpha vector of letters to select from
#' @param nalpha range of possible number of \code{alpha} to select
#' @param num numerics to select from
#' @param nnum range of possible number of \code{num} to select
#' @param sep character to separate \code{alpha} and \code{num}
#' @param seed seed; integer or \code{NULL}
#' 
#' @examples
#' rgene()
#' rgene(5, alpha = 'ABCD', nalpha = 1, nnum = 5:6)
#' rgene(5, alpha = c('A','T','C','G'), num = '', sep = '')
#' 
#' @export

rgene <- function(n = 1L, alpha = LETTERS[1:5], nalpha = 2:5,
                  num = 0:9, nnum = 1:5, sep = '-', seed = NULL) {
  ## helpers
  p0 <- function(...) paste0(..., collapse = '')
  alphas   <- function() sample(alpha, sample(nalpha, 1), TRUE)
  numerics <- function() sample(num, sample(nnum, 1), TRUE)
  
  set.seed(seed)
  replicate(n, p0(p0(alphas()), sep, p0(numerics())))
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
  ## resetting libPaths before restarting r session may not be desired
  # lp <- .libPaths()
  # on.exit(.libPaths(lp))
  .libPaths(lib)
  utils::install.packages(pkgs = pkgs, lib = lib, ...)
  for (ii in pkgs)
    require(ii, character.only = TRUE)
  
  invisible(NULL)
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
#' @param x,y lists
#' 
#' @seealso
#' \code{\link{clist}}; adapted from
#' \url{http://stackoverflow.com/questions/23483421/combine-
#' merge-lists-by-elements-names-list-in-list}
#' 
#' @examples
#' ## l1 and l2 have similar structures
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

nestedMerge <- function(x, y) {
  if (missing(y))
    return(x)
  if (islist(x) & islist(y)) {
    nn <- setdiff(names(y), names(x))
    x <- c(x, setNames(vector('list', length(nn)), nn))
  }
  
  nestedmerge(x, y)
}

#' @rdname nestedMerge
#' @export
nestedmerge <- function(x, y) {
  if (missing(y))
    return(x)
  if (islist(x) & islist(y)) {
    res <- list()
    if (!is.null(names(x))) {
      for (nn in names(x)) {
        res <- if (nn %in% names(y) && !is.null(y[[nn]]))
          append(res, c(Recall(x[[nn]], y[[nn]]))) else
            append(res, list(x[[nn]]))
        names(res)[length(res)] <- nn
      }
    } else {
      for (ii in seq_along(x))
        res <- if (ii <= length(y) && !is.null(y[[ii]]))
          append(res, Recall(x[[ii]], y[[ii]])) else
            append(res, list(x[[ii]]))
    }
    res
  } else list(c(x, y))
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
#' @note
#' Known examples where this function fails:
#' \itemize{
#'  \item{\code{.tar.gz} }{files with compound file extensions}
#' }
#' 
#' @param path file path as character string
#' 
#' @seealso \code{\link[rawr]{regcaptures}}; \code{\link{basename}};
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
#' setNames(lapply(l, path_extract), l)
#' setNames(lapply(l, fname), l)
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
    warning('Results could not be validated', domain = NA)
  
  m
}

#' @rdname path_extract
#' @export
fname <- function(path) {
  xx <- basename(path)
  pp <- '(^\\.[^ .]+$|[^:\\/]*?[.$]?)(?:\\.([^ :\\/.]*))?$'
  
  `colnames<-`(regcaptures2(xx, pp)[[1L]], c('filename', 'extension'))
}

#' @rdname path_extract
#' @export
file_name <- function(path) {
  path_extract(path)[, 'filename']
}

#' @rdname path_extract
#' @export
file_ext <- function(path) {
  path_extract(path)[, 'extension']
}

#' @rdname path_extract
#' @export
rm_ext <- function(path) {
  gsub('(^\\.[^ .]+$|[^:\\/]*?[.$]?)(?:\\.([^ :\\/.]*))?$',
       '\\1', path, perl = TRUE)
}

#' Multiple pattern matching and replacement
#' 
#' Perform multiple pattern matching and replacement.
#' 
#' @param pattern for substituting, a vector of length two for a single
#' replacement or a \emph{list} of length two vectors for multiple
#' replacements where each vector is \code{c(pattern,replacement)}; or for
#' grepping, a vector of character strings containing regular expressions
#' to be matched in \code{x}
#' @param x a character vector where matches are sought
#' @param ... additional parameters passed onto other methods
#' @param parallel logical; if \code{TRUE}, grepping will be performed in
#' \pkg{\link{parallel}}; also, if \code{pattern} is a vector greater than
#' \code{1e4} elements in length, \code{parallel} defaults to \code{TRUE}
#' @param replacement optional; if given, both \code{pattern} and
#' \code{replacement} should be character vectors of equal length
#' (\code{replacement} will be recycled if needed)
#' 
#' @seealso
#' \code{\link[base]{grep}}; \code{\link{vgrep}}
#' 
#' @examples
#' ## grepping
#' mgrep(letters[1:5], letters[1:5])
#' mgrepl(letters[1:5], letters[1:5])
#' 
#' ## subbing
#' s1 <- 'thiS iS SooD'
#' 
#' ## if replacement is given, acts like gsub
#' mgsub(c('hi', 'oo'), c('HI', '00'), s1)
#' mgsub(c('\\bS','$','i'), '_', rep(s1, 3))
#' 
#' ## pattern can also be a list of c(pattern, replacement)
#' r1 <- c('hi','HI')
#' r2 <- c(list(r1), list(c('oo', '00')))
#' r3 <- c(r2, list(c('i', '1'), c('\\b(\\w)', '\\U\\1')))
#' 
#' mgsub(r1, x = s1, ignore.case = TRUE)
#' mgsub(r2, x = s1)
#' mgsub(r3, x = s1, perl = TRUE)
#' 
#' @name mgrep
NULL

mgrep_ <- function(parallel, FUN, vlist, ...) {
  pattern <- vlist$pattern
  x <- vlist$x
  
  if (parallel) {
    ## if parallel = TRUE or long vector x (>1e4), run in parallel
    requireNamespace('parallel')
    cl <- makeCluster(nc <- getOption('cl.cores', detectCores()))
    on.exit(stopCluster(cl))
    clusterExport(cl = cl, varlist = c('x', 'pattern'), envir = environment())
    parLapply(cl, seq_along(pattern),
              function(ii) FUN(pattern = pattern[ii], x = x, ...))
  } else {
    ## slow version
    lapply(seq_along(pattern), function(ii)
      FUN(pattern = pattern[ii], x = x, ...))
  }
}

#' @rdname mgrep
#' @export
mgrepl <- function(pattern, x, ..., parallel = length(pattern) > 1e4) {
  mgrep_(parallel = parallel, FUN = base::grepl, ...,
         vlist = list(pattern = pattern, x = x))
}

#' @rdname mgrep
#' @export
mgrep <- function(pattern, x, ..., parallel = length(pattern) > 1e4) {
  mgrep_(parallel = parallel, FUN = base::grep, ...,
         vlist = list(pattern = pattern, x = x))
}

msub_ <- function(pattern, replacement, x, ..., FUN) {
  dots <- match.call(expand.dots = FALSE)$...
  FUN  <- match.fun(FUN)
  
  if (!missing(replacement))
    pattern <- as.list(data.frame(
      t(cbind(I(pattern), I(rep_len(replacement, length(pattern)))))))
  if (!is.list(pattern))
    pattern <- list(pattern)
  
  sub2 <- function(l, x)
    do.call(FUN, c(list(x = x, pattern = l[1L], replacement = l[2L]), dots))
  
  Reduce('sub2', pattern, x, right = TRUE)
}

#' @rdname mgrep
#' @export
msub <- function(pattern, replacement, x, ...) {
  msub_(pattern, replacement, x, ..., FUN = 'sub')
}

#' @rdname mgrep
#' @export
mgsub <- function(pattern, replacement, x, ...) {
  msub_(pattern, replacement, x, ..., FUN = 'gsub')
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

#' tree
#' 
#' List contents of directories in a tree-like format.
#' 
#' @param path file name path as character string
#' @param full.names logical; if \code{TRUE}, the full file path will be
#' returned; otherwise, only the \code{\link{basename}} is returned (default)
#' @param ndirs,nfiles maximum number of directories and files per directory
#' to print
#' 
#' @references
#' \url{https://stackoverflow.com/q/14188197/2994949}
#' 
#' @examples
#' str(tree(system.file(package = 'rawr'), FALSE))
#' 
#' @export

tree <- function(path = '.', full.names = FALSE, ndirs = 5L, nfiles = 5L) {
  ## helper
  tree_ <- function(path = '.', full.names, n) {
    isdir <- file.info(path)$isdir
    n <- as.integer(n)
    
    res <- if (!isdir) {
      if (full.names)
        path else basename(path)
    } else {
      files <- list.files(path, full.names = TRUE, include.dirs = TRUE)
      isdir <- file.info(files)$isdir
      files <- files[isdir | cumsum(!isdir) <= n]
      res <- lapply(files, tree_, full.names, n)
      names(res) <- basename(files)
      res
    }
    
    res
  }
  
  path <- normalizePath(path, mustWork = TRUE)
  head(tree_(path, full.names, nfiles), ndirs)
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
  stopifnot(
    adj %inside% 0:1
  )
  
  mat <- as.matrix(x)
  res <- rbind(0, mat[-nrow(mat), , drop = FALSE])
  res <- mat / (1 / adj) + apply(res, 2L, cumsum)
  
  if (is.null(dim(x)))
    drop(res) else res
}

#' \code{grep} for vectors
#' 
#' \code{grep} vectors for patterns given by other vectors.
#' 
#' @param pattern a vector to be matched
#' @param x vector having the same type as \code{pattern} where matches are
#' sought
#' 
#' @return
#' For \code{vgrep}, a vector of indices indicating the start of the matches
#' found in \code{x}. For \code{vgrepl}, a list of logical vetors of
#' \code{length(x)} for each match found in \code{x}.
#' 
#' @references
#' Adapted from \url{https://stackoverflow.com/q/33027611/2994949}
#' 
#' @seealso
#' \code{\link{grep}}; \code{\link[rawr]{mgrep}}; \code{\link[rawr]{\%==\%}}
#' 
#' @examples
#' x <- c(0,1,1,0,1,1,NA,1,1,0,1,1,NA,1,0,0,1,
#'        0,1,1,1,NA,1,0,1,NA,1,NA,1,0,1,0,NA,1)
#' vgrep(c(1, NA, 1), x)
#' vgrepl(c(1, NA, 1), x)
#' 
#' vgrep(c(1, 0, 1, NA), x)
#' which(vgrepl(c(1, 0, 1, NA), x)[[1]])
#' 
#' @export

vgrep <- function(pattern, x) {
  vgrep_ <- function(pp, xx, acc = if (length(pp))
    seq_along(xx) else integer(0L)) {
    if (!length(pp))
      return(acc)
    Recall(pp[-1L], xx, acc[which(pp[[1L]] %==% xx[acc])] + 1L)
  }
  vgrep_(pattern, x) - length(pattern)
}

#' @rdname vgrep
#' @export

vgrepl <- function(pattern, x) {
  m  <- vgrep(pattern, x)
  lp <- length(pattern)
  pp <- rep(FALSE, length(x))
  
  if (!length(m))
    integer(0L) else lapply(m, function(y) {
      pp[y:(y + lp - 1L)] <- TRUE
      pp
    })
}

#' Justify text
#' 
#' Add whitespace to (monospaced) text for justified or block-style spacing.
#' 
#' @param string a character string
#' @param width desired width text in characters given as a positive integer
#' @param fill method of adding whitespace
#' 
#' @seealso
#' \code{\link{strwrap}}
#' 
#' @references
#' Adapted from \url{https://stackoverflow.com/q/34710597/2994949}
#' 
#' @examples
#' x <- paste(rownames(mtcars), collapse = ' ')
#' cat(justify(x))
#' 
#' ## slight differences in whitespace for fill methods
#' op <- par(no.readonly = TRUE)
#' par(cex = .8, xpd = NA, family = 'mono', mar = c(2,2,2,2))
#' plot(0, ann = FALSE, axes = FALSE, type = 'n')
#' text(1, 0, justify(x, fill = 'random'))
#' text(1, 0, justify(x, fill = 'right'), col = 2)
#' text(1, 0, justify(x, fill = 'left'), col = 3)
#' par(op)
#' 
#' @export

justify <- function(string, width = getOption('width') - 10L,
                    fill = c('random', 'right', 'left')) {
  fill <- match.arg(fill)
  string <- gsub('\n', '\n\n', string, fixed = TRUE)
  strs   <- strwrap(string, width = width)
  
  paste(fill_spaces_(strs, width, fill), collapse = '\n')
}

fill_spaces_ <- function(lines, width, fill) {
  tokens <- strsplit(lines, '\\s+')
  res <- lapply(head(tokens, -1L), function(x) {
    nspace <- max(length(x) - 1L, 1L)
    extra <- width - sum(nchar(x)) - nspace
    reps  <- extra %/% nspace
    extra <- extra %% nspace
    times <- rep.int(if (reps > 0L) reps + 1L else 1L, nspace)
    
    if (extra > 0L) {
      if (fill == 'right')
        times[1:extra] <- times[1:extra] + 1L
      else if (fill == 'left')
        times[(nspace - extra + 1L):nspace] <-
          times[(nspace - extra + 1L):nspace] + 1L
      else times[inds] <- times[(inds <- sample(nspace, extra))] + 1L
    }
    
    spaces <- c('', unlist(lapply(times, formatC, x = ' ', digits = NULL)))
    res <- paste(c(rbind(spaces, x)), collapse = '')
    
    if (sum(c(nchar(x), length(x), extra)) < width / 2)
      gsub('\\s{1,}', ' ', res) else res
  })
  
  c(res, paste(tail(tokens, 1L)[[1L]], collapse = ' '))
}

#' Find factors
#' 
#' Find common factors of two or more integers.
#' 
#' @param ... integers
#' 
#' @examples
#' factors(21)
#' factors(3 * 2 ^ 20)
#' factors(64, 128, 58)
#' 
#' @export

factors <- function(...) {
  factors_ <- function(x) {
    x <- as.integer(x)
    y <- seq_len(abs(x))
    y[x %% y == 0L]
  }
  
  l <- lapply(list(...), factors_)
  Reduce(intersect, l)
}

#' Sample each
#' 
#' Sample each unique value of a vector.
#' 
#' @param x a vector
#' @param n number to sample from each unique group in order; if \code{x} is
#' a factor, \code{n} should correspond to the order of \code{levels(x)};
#' otherwise, \code{n} will be matched with the sorted unique groups
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
#' X <- factor(x, 5:3)
#' mtcars[sample_each(x, 3:5), ]
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

#' Pick elements from columns
#' 
#' This function will return \code{\link{colnames}} or column values (if
#' \code{value = TRUE}) for "indicator-like" matrices or data frames.
#' 
#' @param data a data frame or matrix
#' @param ind if \code{value = FALSE}, a vector (usually a single value)
#' which, for each row, will return the corresponding column name; if
#' \code{value = FALSE} (default), a vector of values to be \emph{ignored}
#' @param value logical; if \code{TRUE}, returns column value; otherwise,
#' returns column names (default)
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
#' dd <- data.frame(x = c(1,0,0), y = c(0,0,1), z = c(0,1,0),
#'                  a = c('one','',''), b = c('','','three'), c = c('','two',''))
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

pickcol <- function(data, ind = 1L, value = FALSE) {
  res <- apply(data, 1L, function(x) {
    if (value) {
      x[x %in% ind] <- NA
      if  (length(x <- x[!is.na(x)]) > 1L)
        toString(x) else x
    } else {
      idx <- x %in% ind
      if (sum(idx))
        toString(names(x[idx])) else NA
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
