### formatting and miscellaneous utilities
# html_test, roundr, intr, pvalr, pvalr2, clist, binconr, num2char, iprint,
# match_ctc, writeftable, surv_summary, surv_table, tabler, tabler_by,
# countr, tox_worst, gcd, dmy
###


#' html test
#' 
#' Render html in rstudio viewer
#' 
#' @param x character string of html code
#' 
#' @examples
#' \dontrun{
#' html_test("
#' <div align = center><h1>A heading<sup>&dagger;</sup><h1></div>
#' <font size = 1><sup>&dagger;</sup>That was the heading</font>
#' ")
#' 
#' library(htmlTable)
#' html_test(htmlTable(mtcars, output = FALSE))
#' }
#' 
#' @export

html_test <- function(x) {
  htmlFile <- tempfile(fileext = '.html')
  writeLines(x, con = htmlFile)
  if ((Sys.getenv('RSTUDIO') != '') && ('rstudio' %in% .packages(TRUE)))
      rstudio::viewer(htmlFile)
  else browseURL(htmlFile)
}

#' roundr
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
# useful for dropping the negative in case 1:
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

#' Interval formatter
#' 
#' Calculate summary statistic with range or confidence interval.
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

intr <- function(..., fun = median, conf = NULL, digits = 0, na.rm = FALSE) {
  
  lst <- list(...)
  if (is.null(conf) || conf == 0 || 
        findInterval(conf, c(0, 1), rightmost.closed = FALSE) != 1)
    conf <- 1
  
  sapply(lst, function(x) {
    bounds <- quantile(x, c((1 - conf) / 2 * c(1, -1) + c(0, 1)), na.rm = na.rm)
    bounds <- roundr(bounds, digits = digits)
    val <- roundr(fun(x, na.rm = na.rm), digits = digits)
    
    if (!conf %in% c(0, 1))
      sprintf('%s (%s%% CI: %s - %s)', val, conf * 100, bounds[1], bounds[2])
    else
      sprintf('%s (range: %s - %s)', val, bounds[1], bounds[2])
  })
}

#' p-value formatter
#' 
#' Formats several cases of p-values; see details.
#' 
#' \code{pvalr} will deal with several cases of common p-values: 1) p-values
#' which are > 0.1 will be rounded to two decimal places (and keep any trailing
#' 0s) since we are not usually interested in precision of insignificant
#' values; 2) p-values which are less than the specified \code{sig.level} will
#' be formatted as \code{< 0.001}, for example; 3) p-values that are less than
#' 0.01 but greater than \code{sig.level} will be precise to \code{digits} and
#' keep any trailing 0s.
#' 
#' \code{pvalr2} deals with common cases of character string p-values which are
#' not ideal (0.000, 1.00, etc) and will leave others unchanged.
#' 
#' @param pvals for \code{pvalr}, a numeric value or vector of p-values; for
#' \code{pvalr2}, a vector of p-values as character strings
#' @param sig.limit lower bound for precision
#' @param digits integer; number of decimal places; see also 
#' \code{\link[rawr]{roundr}}
#' @param html logical; if \code{TRUE}, uses \code{&lt;} instead of \code{<}
#' @param show.p logical; if \code{TRUE}, inserts \code{p = } or \code{p < }
#' where appropriate
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

#' @rdname pvalr
#' @examples
#' pvals <- c('1.000','1.0','1','0.00000','0.123','0.6','0.0', '< 0.001')
#' pvalr2(pvals)
#' 
#' @export

pvalr2 <- function(pvals, html = FALSE, show.p = FALSE) {
  x <- gsub('^1$', '1.0', pvals)
  x <- gsub('(?:^1\\.|\\G)\\K0(?=0*$)', '9', x, perl = TRUE)
  x <- gsub('^1\\.', '> 0.', x)
  x <- gsub('^(0\\.0*)0$', '< \\11', x)
  if (html) {
    x <- gsub('>', '&gt;', x)
    x <- gsub('<', '&lt;', x)
  }
  if (show.p)
    ifelse(grepl('[<>]', pvalr2(pvals)), paste0('p ', x), paste0('p = ', x))
  else x
}

#' Concatenate a named list for output
#' 
#' Print a \emph{named} \code{list} as a character string with values.
#'
#' @param l list to concatenate
#' 
#' @examples
#' (l <- list(a = 1, b = 2))
#' 
#' # $a
#' # [1] 1
#' #
#' # $b
#' # [1] 2
#' 
#' clist(l) # [1] "a = 1, b = 2"
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
#' binconr(45, 53, digits = 1)
#' 
#' @export

binconr <- function(r, n, conf = 0.95, digits = 0,
                    est = TRUE, method = 'exact') {
  res <- roundr(bincon(r, n, alpha = 1 - conf, method = method) * 100, 
                digits = digits)
  zzz <- sprintf('%s%% CI: %s - %s%%', conf * 100, res[4], res[5])
  if (est)
    zzz <- sprintf('%s%% (%s)', res[3], zzz)
  zzz
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
#' match_ctc('aortic', 'arterial')
#' 
#' @export

match_ctc <- function(..., version = 4) {
  
  x <- c(...)
  if (version %ni% 3:4)
    stop('CTCAE version should be 3 or 4')
  else {
    if (version == 3)
      dat <- rawr::ctcae_v3
    else dat <- rawr::ctcae_v4
  }
  
  if (any(grepl('([A-Za-z -])([0-9])', x)))
    idx <- match(gsub('\\s*|-', '', x, perl = TRUE), dat[, 'tox_code'])
  else idx <- grep(paste(x, collapse = '|'), dat[, 'tox_desc'],
                   ignore.case = TRUE)
  
  return(list(matches = `rownames<-`(dat[idx, ], NULL),
              version = sprintf('CTCAE v%s', version)))
}

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

#' Summary of a survival curve
#' 
#' Prints and returns a list containing the survival curve, confidence limits 
#' for the curve, and other information.
#' 
#' @param s \code{\link[survival]{survfit}} object
#' @param digits number of digits to use in printing numbers
#' @param ... additional arguments passed to 
#' \code{\link[survival]{summary.survfit}}
#' 
#' @return
#' A list with summaries for each strata; see 
#' \code{\link[survival]{summary.survfit}}
#' @seealso
#' \code{\link[survival]{survfit}}, 
#' \code{\link[survival]{print.summary.survfit}}
#' 
#' @examples
#' library(survival)
#' data(cancer)
#' fit1 <- survfit(coxph(Surv(time, status) ~ strata(I(age > 60)), 
#'                       data = cancer),
#'                 conf.type = 'log-log')
#' surv_summary(fit1, times = c(0, 100, 200))
#' 
#' @export

surv_summary <- function(s, digits = max(getOption('digits') - 4, 3), ...) {
  
  ## error checks
  if (!inherits(s, 'survfit')) 
    stop('s must be a survfit object')
  
  x <- summary(s, ...)
  
  savedig <- options(digits = digits)
  on.exit(options(savedig))
  if (!is.null(cl <- x$call)) {
    cat("Call: ")
    dput(cl)
  }
  omit <- x$na.action
  if (length(omit)) 
    cat(naprint(omit), "\n")
  if (x$type == "right" || is.null(x$n.enter)) {
    mat <- cbind(x$time, x$n.risk, x$n.event, x$surv)
    cnames <- c("time", "n.risk", "n.event")
  } else 
    if (x$type == "counting") {
      mat <- cbind(x$time, x$n.risk, x$n.event, x$n.enter, x$n.censor, x$surv)
      cnames <- c("time", "n.risk", "n.event", "entered", "censored")
    }
  if (is.matrix(x$surv)) 
    ncurve <- ncol(x$surv)
  else ncurve <- 1
  if (ncurve == 1) {
    cnames <- c(cnames, "survival")
    if (!is.null(x$std.err)) {
      if (is.null(x$lower)) {
        mat <- cbind(mat, x$std.err)
        cnames <- c(cnames, "std.err")
      } else {
        mat <- cbind(mat, x$std.err, x$lower, x$upper)
        cnames <- c(cnames, "std.err", 
                    paste("lower ", x$conf.int * 100, "% CI", sep = ""),
                    paste("upper ", x$conf.int * 100, "% CI", sep = ""))
      }
    }
  } else 
    cnames <- c(cnames, paste("survival", seq(ncurve), sep = ""))
  if (!is.null(x$start.time)) {
    mat.keep <- mat[ , 1] >= x$start.time
    mat <- mat[mat.keep, , drop = FALSE]
    if (is.null(dim(mat))) 
      stop(paste("No information available using start.time =", 
                 x$start.time, "."))
  }
  if (!is.matrix(mat)) 
    mat <- matrix(mat, nrow = 1)
  if (!is.null(mat)) {
    dimnames(mat) <- list(NULL, cnames)
    if (is.null(x$strata)) {
      cat("\n")
      invisible(prmatrix(mat, rowlab = rep("", nrow(mat))))
    } else {
      strata <- x$strata
      if (!is.null(x$start.time))
        strata <- strata[mat.keep]
      invisible(setNames(lapply(levels(strata), function(i) {
        who <- (strata == i)
        cat("\n               ", i, "\n")
        if (sum(who) == 1)
          prmatrix(mat[who, ])
        else prmatrix(mat[who, ], rowlab = rep("", sum(who)))
      }), levels(strata)))
    }
  } else 
    stop("There are no events to print. Use the option censored = TRUE ",
         "with the summary function to see the censored observations.")
}

#' Summary table
#' 
#' Prints a formatted summary table for \code{\link[survival]{survfit}} objects
#' 
#' @param s \code{\link[survival]{survfit}} object
#' @param digits number of digits to use in printing numbers
#' @param times vector of times
#' @param ... additional arguments passed to 
#' \code{\link[survival]{summary.survfit}}
#' 
#' @return
#' A matrix (or list of matrices) with formatted summaries for each strata; see 
#' \code{\link[survival]{summary.survfit}}
#' @seealso
#' \code{\link[survival]{survfit}}, 
#' \code{\link[survival]{print.summary.survfit}}
#' 
#' @examples
#' library(survival)
#' data(cancer)
#' 
#' fit0 <- survfit(coxph(Surv(time, status) ~ 1, 
#'                       data = cancer),
#'                 conf.type = 'log-log')
#' surv_table(fit0, times = c(0, 100, 200))
#' 
#' ## also works for list of tables
#' fit1 <- survfit(coxph(Surv(time, status) ~ strata(I(age > 60)), 
#'                       data = cancer),
#'                 conf.type = 'log-log', conf.int = 0.9)
#' surv_table(fit1)
#' 
#' \dontrun{
#' library(htmlTable)
#' s <- `colnames<-`(surv_table(fit0, times = c(0, 200, 400, 600, 800), 
#'                              digits = 2)[ , -4], 
#'                   c('Time','No. at risk','No. of events','OS (95% CI)'))
#' htmlTable(s)
#' }
#' 
#' @export

surv_table <- function(s, digits = 3, times = pretty(range(s$time)), ...) {
  tmp <- capture.output(summ <- surv_summary(s, digits = digits, 
                                             times = times, ...))
  f <- function(x, d = digits, vars = vars) {
    vars = colnames(x)
    tmpvar <- colnames(x)[grep('survival|std.err|lower|upper', 
                               colnames(x))]
    x[ , tmpvar] <- roundr(x[ , tmpvar], digits = d)
    
    surv <- sprintf('%s (%s, %s)', 
                    x[, colnames(x)[grepl('survival', colnames(x))]],
                    x[, colnames(x)[grepl('lower', colnames(x))]],
                    x[, colnames(x)[grepl('upper', colnames(x))]])
    `colnames<-`(cbind(x[, c(setdiff(vars, tmpvar), 'std.err')], surv),
                 c('Time','No. at risk','No. event','Std.Error',
                   sprintf('OR (%s%% CI)', s$conf.int * 100)))
  }
  if (is.list(summ))
    Map(f = f, summ)
  else f(summ)
}

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
  res
}

#' @rdname tabler
#' @export
tabler.glm <- function(x, digits = 3, level = 0.95, type = '', ...) {
  res <- summary(x, ...)$coefficients
  if (tolower(type) == 'or') {
    res <- round(cbind(exp(cbind(coef(x), confint(x, level = level))),
                       res[, 4]), digits = digits)
    res <- cbind(res, sprintf('%s (%s, %s)', res[, 1], res[, 2], res[, 3]))
    ci <- sprintf('%s%% CI', level * 100)
    res <- setNames(as.data.frame(res), 
                    c('Odds Ratio', paste0('L ', ci),
                      paste0('U ', ci), 'Pr(>|z)', sprintf('OR (%s)', ci)))
  } else res <- round(res, digits = digits)
  res
}

#' @rdname tabler
#' @export
tabler.survfit <- function(x, ...) surv_table(x, ...)

#' tabler_by
#' 
#' This function is helpful to make simple stratified tables, faster and 
#' easier to use for simple tables than \code{\link[tables]{tabular}}.
#' 
#' \code{varname} and \code{byvar} should be factors, and the levels will
#' appear in the output as they occur in \code{levels(x)}.
#' 
#' \code{n} is used to calculate the percentages. If missing, the output will
#' only show counts in the table. If given, \code{length(n)} should be one or
#' equal to the number of levels of \code{byvar}.
#' 
#' If one \code{n} is given, \code{tabler_by} assumes that this is the total
#' population for a subgroup, i.e., if creating a table for a subset of the 
#' data, it is only necessary to provide the total \code{n} for that group.
#' 
#' If more than one \code{n} is given, \code{tabler_by} assumes that the
#' entire data set is given to \code{dat} and will use the corresponding 
#' \code{n} to show percentages out of each respective subgroup.
#' 
#' @param dat a data frame; variables \code{varname} and \code{byvar} should
#' be factors
#' @param varname variable with subgroups to count
#' @param byvar stratification variable
#' @param n number in each group; see details
#' @param order logical; order the result by decreasing frequency
#' @param zeros optional character string replacement for cells which have
#' zero counts; will appear as \code{0 (0\%)} if not given
#' @param pct.col logical; if \code{TRUE}, percents are separated into new
#' columns
#' 
#' @examples
#' set.seed(1618)
#' 
#' f <- function(x, ...) sample(x, 100, replace = TRUE, ...)
#' tox <- data.frame(casenum = rep(1:10, 10), phase = 1:2,
#'                   tox_code = f(rawr::ctcae_v4$tox_code[1:25]),
#'                   tox_grade = f(1:3, prob = c(.6, .3, .1)),
#'                   stringsAsFactors = FALSE)
#' 
#' n <- table(tox[1:10, ]$phase)
#' tox <- cbind(tox, match_ctc(tox$tox_code)$matches[, c('tox_cat', 'tox_desc')])
#' 
#' tox <- within(tox, {
#'   phase <- factor(phase)
#'   tox_grade <- factor(tox_grade)
#'   tox_cat <- factor(tox_cat)
#'   tox_desc <- factor(tox_desc)
#' })
#' 
#' ## get worst toxicities by casenum by grade
#' tox <- tox_worst(tox)$tox_worst
#' 
#' out <- cbind(tabler_by(tox, 'tox_desc',
#'                        'phase', n = n, zeros = '-')[, 1, drop = FALSE],
#'              tabler_by(tox[tox$phase == '1', ], 'tox_desc',
#'                        'tox_grade', n = n[1], zeros = '-'),
#'              tabler_by(tox[tox$phase == '2', ], 'tox_desc',
#'                        'tox_grade', n = n[2], zeros = '-'))
#' out <- out[order(as.numeric(out[, 1]), decreasing = TRUE), ]
#' 
#' library('htmlTable')
#' cgroup <- c(sprintf('Total<br /><font size=1>n = %s</font>', sum(n)),
#'             sprintf('Phase I<br /><font size=1>n = %s</font>', n[1]),
#'             sprintf('Phase II<br /><font size=1>n = %s</font>', n[2]))
#'             
#' htmlTable(out, ctable = TRUE, cgroup = cgroup, n.cgroup = c(1, 4, 4),
#'     caption = 'Table 1: Toxicities<sup>&dagger;</sup> by phase and grade.',
#'     col.columns = rep(c('grey97','none','grey97'), times = c(1, 4, 4)),
#'     col.rgroup = rep(rep(c('none', 'grey97'), each = 5), 10),
#'     tfoot = paste0('<font size=1><sup>&dagger;</sup>Percentages represent ',
#'             'proportion of patients out of respective phase total.</font>'))
#'             
#' @export

tabler_by <- function(dat, varname, byvar, n, order = FALSE, zeros,
                      pct.col = FALSE) {
  if (!all(sapply(dat[, c(varname, byvar)], is.factor)))
    stop('\'varname\' and \'byvar\' must be factors')
  
  ## split data by varname, get totals overall and for each level of byvar
  l <- split(dat, dat[, varname])
  res <- do.call('rbind', lapply(l, function(x)
    c(Total = length(x[, varname]), tapply(x[, varname], x[, byvar], length))))
  res1 <- res[, -1]
  res1[is.na(res1)] <- 0
  
  ## this will add percents: N (x%) to each column where n for each group
  if (!missing(n)) {
    nr <- nrow(res1)
    ## if one n is given, assume this is the total of a subgroup
    ## if > 1, assume that these correspond to the size of each level of byvar
    ## i.e., if calculating overall totals, give the n for each group
    ## if calculating totals for a subgroup, give one n since this group
    ## will have the same overall n
    if (length(n) == 1L)
      n <- rep(n, ncol(res1))
    if (length(n) != nlevels(dat[, byvar]))
      stop('\'n\' should be 1 or equal to nlevels(byvar)')
    res1 <- matrix(res1)
    mat <- round(res1 / matrix(rep(n, each = nr)) * 100)
    res1 <- matrix(sprintf('%s (%s%%)', res1, mat),
                   nrow = nr, ncol = length(n))
    # if (!missing(zeros))
    #   res1 <- gsub('0 \\(0%\\)', zeros, res1)
  }
  
  zzz <- if (pct.col) {
    res2 <- apply(res1, 1, paste0, collapse = ' ')
    res2 <- as.matrix(setNames(read.table(text = gsub('\\(|\\)|%', '', res2),
                                          colClasses = 'character'),
                       interleave(colnames(res)[-1],
                                  rep('%', ncol(res1)))))
    cbind(Total = res[, 1, drop = FALSE], res2)
  } else `colnames<-`(cbind(Total = res[, 1], res1), colnames(res))

  if (order) {
    zzz[, 1] <- as.numeric(zzz[, 1])
    zzz <- zzz[order(zzz[, 1], decreasing = TRUE), ]
  }
   if (!missing(zeros))
     zzz <- gsub('0 \\(0%\\)|^0$', zeros, zzz)
  zzz
}

#' Count formatter
#' 
#' Formats and prints a \emph{named} vector of counts with percentages.
#' 
#' @param top named vector of counts (a summary or table) or a long vector of
#' character strings or factors
#' @param n total number of observations; if not given, the length of
#' \code{top} is used
#' @param lowcase logical; names will be lowercase if \code{TRUE}, upper
#' case if \code{FALSE}, and unchanged otherwise (do not use \code{\link{NA}})
#' 
#' @examples
#' top <- setNames(3:1, c('gold','silver','bronze'))
#' countr(names(top))
#' countr(top, 10)
#' 
#' @export

countr <- function(top, n, lowcase = TRUE) {
  ## if top is a vector of names, get table
  if (class(top) %in% c('character','factor')) {
    n <- length(top)
    top <- table(top)
  }
  iprint(sprintf('%s (n = %s, %s%%)', 
                 if (is.logical(lowcase))
                   if (lowcase) tolower(names(top)) else toupper(names(top))
                 else names(top),
                 top, round(as.numeric(top) / n * 100)))
}

#' Find most severe toxicities
#' 
#' Returns a subset of input data with worst toxicity grade per toxicity
#' code per patient, ie, sorts and removes duplicates.
#' 
#' @param dat toxicity data frame
#' @param id column name with identifier
#' @param tox_desc column name with toxicity descriptions (or codes)
#' @param tox_grade column name with toxiticity grades; should be a factor
#' with the desired order to be properly sorted
#' 
#' @return
#' A list of length three with \code{tox_worst}, the filtered data frame;
#' \code{dat}, the input data frame sorted by \code{id}, \code{tox_desc},
#' and \code{tox_grade}; and \code{duplicates}, the rows which correspond to
#' duplicate \code{tox_desc} per \code{id} but of an equal or lesser 
#' \code{tox_grade}.
#' 
#' @examples
#' options(stringsAsFactors = FALSE)
#' set.seed(1618)
#' 
#' f <- function(x, ...) sample(x, 100, replace = TRUE, ...)
#' tox <- data.frame(casenum = rep(1:10, 10), phase = 1:2,
#'                   tox_code = f(rawr::ctcae_v4$tox_code[1:10]),
#'                   tox_grade = f(1:3, prob = c(.3, .4, .3)))
#' 
#' tox <- cbind(tox, match_ctc(tox$tox_code)$matches[, c('tox_cat', 'tox_desc')])
#' 
#' tox <- within(tox, {
#'   tox_grade <- factor(tox_grade, levels = 1:3, 
#'                       labels = c('Mild','Moderate','Severe'))
#'   tox_cat <- factor(tox_cat)
#'   tox_desc <- factor(tox_desc)
#' })
#' 
#' (out <- tox_worst(tox))
#' 
#' ## use tabler_by to summarize:
#' tabler_by(out$tox_worst, 'tox_desc', 'tox_grade')
#' 
#' @export

tox_worst <- function(dat, id = 'casenum', tox_desc = 'tox_desc', tox_grade = 'tox_grade') {
  ## make sure grades are properly ordered
  if (!is.factor(dat[, tox_grade]))
    stop('\'tox_grade\' should be a factor')
  ## sort by id, toxicity, and grade
  dat <- dat[order(dat[, id], dat[, tox_desc], -xtfrm(dat[, tox_grade])), ]
  idx <- which(duplicated(dat[, c(id, tox_desc)]))
  list(tox_worst = dat[-idx, ], dat = dat, duplicates = idx)
}

#' GCD
#' 
#' Find greatest common divisor of two integers.
#' 
#' @param x,y integers
#' @examples
#' gcd(99, 2048)
#' gcd(2 ** (1:12), 2048)
#' @export
gcd <- function(x, y) ifelse(r <- x%%y, Recall(y, r), y)

#' Date parse
#' 
#' Parses day, month, year columns to the standard date format.
#' 
#' For two-digit years, the \code{origin} year should be specified; otherwise,
#' the default of 1900 will be used. For \code{NA} year, month, or day,
#' \code{origin} is used for defaults, i.e., \code{origin = c(15, 6, 2000)}
#' will convert missing days to day 15, missing months to June, and missing
#' years to 2000.
#' 
#' @param d,m,y day, month, year as single integers or vectors
#' @param origin a vector of length three giving the origins for \code{d},
#' \code{m}, and \code{y}, respectively; see details
#' 
#' @return
#' A vector of \code{\link{Date}}-formatted strings.
#' 
#' @examples
#' dmy(25, 7, 13)
#' 
#' ## this function is vectorized:
#' dmy(NA, NA, 2000:2009)
#' 
#' set.seed(1)
#' dd <- data.frame(id = 1:10,
#'                  day = sample(1:31, 10),
#'                  month = sample(1:12, 10),
#'                  year = sample(1000:2500, 10))
#' 
#' cbind(dd, dt = with(dd, dmy(day, month, year)))
#' 
#' dd1 <- within(dd, {
#'   day[sample(1:10, 5)] <- NA
#'   month[sample(1:10, 2)] <- NA
#'   year[sample(1:10, 2)] <- NA
#' })
#' 
#' cbind(dd1,
#'       new_date = with(dd1, dmy(day, month, year, origin = c(15, 6, 2000))))
#' 
#' @export

dmy <- function(d, m, y, origin = c(1, 1, 1900)) {
  f <- function(a, b) {
    suppressWarnings(a <- as.numeric(a))
    ifelse(is.na(a), b, a)
  }
  y <- ifelse(nchar(y) <= 2, f(y, 0) + origin[3], f(y, 0))
  as.Date(sprintf('%04s-%02s-%02s', y, f(m, origin[2]), f(d, origin[1])))
}
