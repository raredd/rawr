### formatting, knitr, misc utils
# show_html, show_markdown, show_math, roundr, intr, pvalr, pvalr2, catlist,
# binconr, num2char, iprint, writeftable, tabler, tabler_by, tabler_by2,
# match_ctc, tox_worst, countr, dmy, combine_table
###


#' html render
#' 
#' Render html in rstudio viewer or default browser.
#' 
#' @param ... one or more character strings
#' @param use_viewer logical; if \code{TRUE}, attempts to use
#' \code{rstudioapi::viewer} or opens in default browser on error
#' 
#' @return
#' The html code (invisibly) as a character string.
#' 
#' @seealso
#' \code{\link{show_math}}, \code{\link{show_markdown}}
#' 
#' @examples
#' \dontrun{
#' show_html("
#' <div align = center><h1>A heading<sup>&dagger;</sup><h1></div>
#' <font size = 1><sup>&dagger;</sup>That was the heading</font>
#' ")
#' 
#' library('htmlTable')
#' show_html(htmlTable(mtcars, output = FALSE), use_viewer = FALSE)
#' }
#' 
#' @export

show_html <- function(..., use_viewer = !is.null(getOption('viewer'))) {
  htmlFile <- tempfile(fileext = '.html')
  x <- c(...)
  if (is.null(x)) return(invisible())
  writeLines(x, con = htmlFile)
  if (use_viewer)
    tryCatch(rstudioapi::viewer(htmlFile),
             error = function(e) {
               message('Viewer not available - opening in browser.\n',
                       'In RStudio, try installing the \'rstudio\' package.',
                       domain = NA)
               browseURL(htmlFile)
             })
  else browseURL(htmlFile)
  invisible(x)
}

#' Show markdown
#' 
#' Render markdown to html in rstudio viewer or default browser.
#' 
#' @param ... one or more character strings
#' @param use_viewer logical; if \code{TRUE}, attempts to use
#' \code{rstudioapi::viewer} or opens in default browser on error
#' @param markArgs a list of addition arguments passed to
#' \code{\link[markdown]{markdownToHTML}}
#' 
#' @seealso
#' \code{\link{show_html}}, \code{\link{show_math}}
#' 
#' @examples
#' \dontrun{
#' library('markdown')
#' 
#' ## separate strings of markdown
#' show_markdown('## this is a header','here is some *plain* text  ',
#'               '<font color=red>ahhh</font>')
#' 
#' 
#' ## or as a single character string
#' mkd <- "
#' 1. Here is some markdown I want to test
#' 2. This is a list
#'   + a sub-point
#'   + another sub-point
#' 
#' ```
#' This text is displayed verbatim / preformatted
#' ```
#' 
#' and this text will be formatted using r syntax
#' 
#' ```r
#' mean(1:5)
#' ```
#' 
#' And now I will make a table:
#' 
#' First Header  | Second Header
#' ------------- | -------------
#' Content Cell  | Content Cell
#' Content Cell  | Content Cell
#' 
#' ## ta-daaa!
#' 
#' > here's a picture with a [hyperlink](www.google.com):
#' 
#' <div align=center>
#'     <a href='http://xkcd.com/710/'>
#'         <img src='http://imgs.xkcd.com/comics/collatz_conjecture.png'
#'              style='display: block; margin: auto;' />
#'     </a>
#' </div>
#' "
#' 
#' show_markdown(mkd)
#' 
#' 
#' ## the default stylesheet is
#' getOption('markdown.HTML.stylesheet')
#' 
#' ## apply another style sheet by setting the option or passing
#' ## arguments directly to markdown::markdownToHTML
#' 
#' ## here are four css files in this package
#' list.files(system.file(package = 'rawr', 'styles'))
#' kcss <- system.file(package = 'rawr', 'styles', 'knitr.css')
#' gcss <- system.file(package = 'rawr', 'styles', 'github.css')
#' ## file.show(gcss)
#' 
#' 
#' show_markdown(mkd, markArgs = list(stylesheet = gcss))
#' show_markdown(mkd, markArgs = list(stylesheet = kcss),
#'               use_viewer = FALSE)
#' }
#' 
#' @export

show_markdown <- function(..., use_viewer = !is.null(getOption('viewer')),
                          markArgs = list()) {
  text <- list(text = c(...))
  mdk <- do.call('markdownToHTML', c(text, markArgs))
  show_html(mdk, use_viewer = use_viewer)
}

#' Show math equations
#' 
#' Displays math equations in \code{rstudioapi::viewer} or browser using the
#' \href{http://www.mathjax.org}{MathJax} javascript engine.
#' 
#' @param ... one or more character strings
#' @param css optional css formatting
#' @param use_viewer logical; if \code{TRUE}, attempts to use
#' \code{rstudioapi::viewer} or opens in default browser on error
#' 
#' @seealso
#' \code{\link{show_html}}, \code{\link{show_markdown}},
#' \href{http://detexify.kirelabs.org/classify.html}{draw math symbols},
#' \href{http://stackoverflow.com/questions/31193843/display-r-formula-
#' elegantly-as-in-latex}{SO question}
#' 
#' @examples
#' \dontrun{
#' form1 <- '$$A=\\frac{B}{C}$$'
#' form2 <- '$$
#'   \\frac{1}{\\displaystyle 1+
#'   \\frac{1}{\\displaystyle 2+
#'   \\frac{1}{\\displaystyle 3+x}}} +
#'   \\frac{1}{1+\\frac{1}{2+\\frac{1}{3+x}}}
#' $$'
#' form3 <- '\\frac{d}{dx}\\left( \\int_{0}^{x} f(u)\\,du\\right)=f(x)'
#' 
#' show_math(form1)
#' show_math(form2, use_viewer = FALSE)
#' show_math(form1, form2, form3, css = 'color: red; font-size: 15px;')
#' 
#' form4 <- "
#' \\forall a,b,c \\in \\mathbb{R} \\\\
#' \\begin{align}
#'                       a + b &= c \\\\
#'              (a + b)(a - b) &= c(a - b) \\\\
#'                   a^2 - b^2 &= ca - cb \\\\
#'                    a^2 - ca &= b^2 - cb \\\\
#'   a^2 - ca + \\frac{c^2}{4} &= b^2 - cb + \\frac{c^2}{4} \\\\
#'        (a - \\frac{c}{2})^2 &= (b - \\frac{c}{2})^2 \\\\
#'            a - \\frac{c}{2} &= b - \\frac{c}{2} \\\\
#'                           a &= b \\qquad \\qquad \\blacksquare \\\\
#'  \\end{align}
#' "
#' 
#' cat(show_math(form4))
#' }
#' 
#' @export

show_math <- function(..., css, use_viewer = !is.null(getOption('viewer'))) {
  mj <- "
  <script>
  (function () {
  var script = document.createElement('script');
  script.type = 'text/javascript';
  script.src  = 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML';
  document.getElementsByTagName('head')[0].appendChild(script);
  })();
  </script>
  "
  check_expr <- function(x)
    ## use \[ expr \] instead of $$ expr $$
    sprintf('\\[ %s \\]',
            gsub('\\\\\\[|\\\\]', '', gsub('^\\$+|\\$+$', '', x)))
  x <- paste(sapply(c(...), check_expr), collapse = '<br />')
  if (!nzchar(x))
    return(invisible())
  if (missing(css))
    css <- ''
  show_html(sprintf('<span class="math" style="font-size: 24px; %s;">\n', css),
            x, '\n</span>\n', mj, use_viewer = use_viewer)
}

#' roundr
#'
#' Improved rounding formatter.
#' 
#' Uses \code{\link[base]{sprintf}} to round numeric value, retaining extra 0s.
#'
#' @param x numeric value, vector, matrix, or data frame
#' @param digits number of digits past the decimal point to keep
#'
#' @return
#' An object having the same class as \code{x}.
#' 
#' @seealso
#' \code{\link[base]{round}}; \code{\link[base]{sprintf}}
#'
#' @examples
#' round(0.199, 2)
#' 
#' ## compare to
#' roundr(0.199, 2)
#' 
#' ## useful for dropping the negative sign in case 1:
#' roundr(c(-0.0002, 0.0002, 0.5, -0.5, -0.002), digits = 3)
#' 
#' ## for matrices or data frames (including factors and/or characters)
#' roundr(matrix(1:9, 3))
#' 
#' dd <- within(head(mtcars), {
#'   mpg <- as.character(mpg)
#'   cyl <- factor(cyl, labels = LETTERS[1:3])
#' })
#' 
#' roundr(dd)
#' 
#' @export

roundr <- function(x, digits = 1) UseMethod('roundr')

#' @rdname roundr
#' @export
roundr.default <- function(x, digits = 1) {
  mode.ok <- vapply(x, function(x) is.numeric(x) || is.complex(x), NA)
  if (!all(mode.ok))
    stop('non-numeric argument to mathematical function')
  res <- sprintf(paste0('%.', digits = digits, 'f'), x)
  zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
  res[res == paste0('-', zzz)] <- zzz
  'names<-'(res, names(x))
}

#' @rdname roundr
#' @export
roundr.data.frame <- function(x, digits = 1) {
  mode.ok <- vapply(x, function(x) is.numeric(x) || is.complex(x), NA)
  if (sum(mode.ok) < 1)
    stop(paste0('non-numeric variables in data frame, ',
                deparse(substitute(x))))
  else x[]  <- lapply(seq_along(mode.ok), function(ii)
    if (mode.ok[ii]) roundr.default(x[, ii], digits = digits) else x[, ii])
  x
}

#' @rdname roundr
#' @export
roundr.matrix <- function(x, digits = 1) {
  if (!is.numeric(x) || is.complex(x))
    stop(paste0('non-numeric variable in matrix, ', deparse(substitute(x))))
  else x[] <- roundr.default(x, digits)
  x
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
#' are removed from \code{...} before \code{fun} and \code{\link{quantile}}
#' are computed
#' 
#' @seealso
#' \code{\link[rawr]{roundr}}
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
#' @seealso
#' \code{\link[rawr]{roundr}}
#' 
#' @examples
#' pvals <- c(-1, .00001, .004233, .060123, .13354, .4999, .51, .89, .9, 1)
#' pvalr(pvals)
#' pvalr(pvals, show.p = TRUE, html = TRUE)
#' 
#' @export

pvalr <- function(pvals, sig.limit = 0.001, digits = 3, html = FALSE,
                  show.p = FALSE) {
  show.p <- show.p + 1L
  html   <- html + 1L
  sapply(pvals, function(x, sig.limit) {
    if (is.na(x))
      return(NA)
    if (x >= 0.99)
      return(paste0(c('','p ')[show.p], c('> ','&gt; ')[html], '0.99'))
    if (x >= 0.9)
      return(paste0(c('','p ')[show.p], c('> ','&gt; ')[html], '0.9'))
    if (x < sig.limit) {
      paste0(c('', 'p ')[show.p], c('< ', '&lt; ')[html], format(sig.limit))
    } else {
      nd <- c(digits, 2, 1)[findInterval(x, c(-Inf, .1, .5, Inf))]
      paste0(c('','p = ')[show.p], roundr(x, nd))
    }
  }, sig.limit)
}

#' @rdname pvalr
#' @examples
#' pvals <- c('0.00000', '< 0.001', '0.0', '0.123', '0.6', '1', '1.0', '1.000')
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
  if (!show.p)
    x else
      ifelse(grepl('[<>]', pvalr2(pvals)), paste0('p ', x), paste0('p = ', x))
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
#' catlist(l) # [1] "a = 1, b = 2"
#' catlist(par()[1:5])
#' 
#' @export

catlist <- function(l)
  paste(paste(names(l), l, sep = ' = ', collapse = ', '), sep = '')

#' bincon formatter
#' 
#' Binomial confidence interval (\code{\link{bincon}}) formatter.
#' 
#' @param r number of responses (successes)
#' @param n number of observations (trials)
#' @param conf level of confidence
#' @param digits number of digits
#' @param est logical; if \code{TRUE}, includes the point estimate
#' @param method method to use; see \code{\link{bincon}}
#' 
#' @seealso
#' \code{\link{bincon}}; \code{\link[Hmisc]{binconf}}
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
    sprintf('%s%% (%s)', res[3], zzz)
  else zzz
}

#' Numeric to character string
#' 
#' Convert a number to a character string representation.
#' 
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
#' @param num number; integer value in \code{(-1e08, 1e08)}
#' @param informal logical; if \code{TRUE}, adds "and" before tens or ones
#' @param cap logical; if \code{TRUE}, capitalizes the first word
#' 
#' @references
#' \url{http://dictionary.reference.com/help/faq/language/g80.html}
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
  scipen <- getOption('scipen')
  options(scipen = 999)
  on.exit(options(scipen = scipen))
  
  ## helpers
  key <- c('0'='','1'='one','2'='two','3'='three','4'='four','5'='five',
           '6'='six','7'='seven','8'='eight','9'='nine','10'='ten',
           '11'='eleven','12'='twelve','13'='thirteen','14'='fourteen',
           '15'='fifteen','16'='sixteen','17'='seventeen','18'='eighteen',
           '19'='nineteen','20'='twenty','30'='thirty','40'='forty',
           '50'='fifty','60'='sixty','70'='seventy','80'='eighty',
           '90'='ninety','100'='hundred','1000'='thousand','1000000'='million')
  upcase <- function(x) gsub('(^.)', '\\U\\1', x, perl = TRUE)
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
  
  if (isTRUE(all.equal(num, 0)))
    return(ifelse(cap, 'Zero', 'zero'))
  num <- if (num < 0) {
    neg <- TRUE
    abs(num)
  } else {
    neg <- FALSE
    num
  }
  if (!num %inside% c(1, 99999999))
    stop("I can\'t count that high")
  
  zzz <- trimws(f(num, informal = informal))
  ## trim double whitespace and "and"s in special cases
  zzz <- upcase(gsub('\\s{2,}', ' ', zzz))
  zzz <- tolower(gsub('And\ |\ and*$', '', zzz))
  if (neg)
    zzz <- paste('negative', zzz)
  ifelse(cap, upcase(zzz), zzz)
}

#' In-line printing
#' 
#' Modified \code{\link[pander]{p}} function from the \pkg{pander} package.
#' 
#' @param ... one or more numeric or character elements to be converted into
#' character vectors
#' @param wrap character string to wrap each term
#' @param sep character string to separate the terms
#' @param copula character string to separate last two terms
#' @param digits number of digits past the decimal point to keep; see
#' \code{\link{roundr}}
#' 
#' @seealso
#' \code{\link[pander]{p}}; \code{\link{roundr}}; \code{\link{countr}}
#' 
#' @examples
#' iprint('fee','fi','fo','fum')
#' iprint(rnorm(2))
#' iprint(-0.000, 0.100)
#' iprint(LETTERS[1:5], copula = ", and the letter ")
#' iprint("Thelma", "Louise", copula = " & ")
#' 
#' @export

iprint <- function (..., wrap = '', sep = ', ', copula, digits = 2) {
  # x <- substitute(...())
  x <- c(...)
  if (!(len <- length(x))) return('')
  f <- function(x, wrap = '"') sprintf('%s%s%s', wrap, x, wrap)
  if (missing(copula))
    copula <- ifelse(len == 2L, ' and ', ', and ')
  if (is.numeric(x))
    x <- roundr(x, digits = digits)
  if (len == 1L) f(x, wrap) else
    if (len == 2L) paste(f(x, wrap), collapse = copula) else
      paste0(paste(f(head(x, -1), wrap = wrap), collapse = sep),
             copula, f(tail(x, 1), wrap = wrap))
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
#' writeftable(ftable(mtcars$vs, mtcars$gear))
#' 
#' @export

writeftable <- function (x, quote = FALSE, digits = getOption('digits'), ...) {
  if (!inherits(x, 'ftable'))
    stop('x must be an ftable object')
  
  ## add row/col names if blank (ie, if vectors used in ftable)
  rn <- names(attr(x, 'row.vars'))
  names(attr(x, 'row.vars')) <- ifelse(!nzchar(rn), '$$$', rn)
  cn <- names(attr(x, 'col.vars'))
  names(attr(x, 'col.vars')) <- ifelse(!nzchar(cn), '$$$', cn)
  
  mat <- as.matrix(format(x, quote = quote, digits = digits, ...))
  mat[] <- trimws(mat)
  `colnames<-`(mat[-(1:2), ],
               gsub('$$$', '', Filter(nzchar, mat[1:2, ]), fixed = TRUE))
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
#' library('survival')
#' sfit <- survfit(Surv(time, status) ~ 1, data = cancer, conf.int = 0.9)
#' tabler(sfit)
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
#' entire data set is given to \code{data} and will use the corresponding
#' \code{n} to show percentages out of each respective subgroup.
#' 
#' @param data a data frame; variables \code{varname} and \code{byvar} should
#' be factors
#' @param varname subgroup variable name (rows)
#' @param byvar stratification variable name (columns)
#' @param n number in each group; see details
#' @param order logical; order the result by decreasing frequency
#' @param zeros optional character string replacement for cells which have
#' zero counts; will appear as \code{0 (0\%)} if \code{TRUE}
#' @param pct logical; if \code{TRUE} (and \code{n} is not missing), percents
#' are shown
#' @param pct.column logical; if \code{TRUE}, percents are separated into new
#' columns
#' @param pct.total logical; if \code{TRUE}, adds percents for total column
#' @param pct.sign logical; if \code{TRUE}, percent sign is shown
#' @param drop logical; if \code{TRUE}, rows with zero total counts
#' will be removed (default); \code{FALSE} case is useful when merging multiple
#' \code{tabler_by} tables (eg, this is what \code{tabler_by2} does)
#' @param stratvar for \code{tabler_by2}, a factor-like variable used to
#' stratify observations into mutually exclusive groups for which
#' \code{tabler_by} will be performed on each subset
#' 
#' @seealso
#' \code{\link{tox_worst}}; \code{\link{match_ctc}}
#' 
#' @examples
#' mt <- within(mtcars, {
#'   am <- factor(am)
#'   gear <- factor(gear)
#'   vs <- factor(vs, levels = 0:2)
#'   carb <- factor(carb)
#' })
#' 
#' tabler_by(mt, 'vs', 'gear')
#' tabler_by(mt, c('vs', 'carb'), 'gear', order = TRUE)
#' tabler_by(mt, 'vs', 'gear', n = table(mt$gear))
#' tabler_by(mt, 'vs', 'gear', n = table(mt$gear), pct.column = TRUE, zeros = '-')
#' 
#' 
#' ## example workflow
#' set.seed(1)
#' 
#' f <- function(x, ...) sample(x, 100, replace = TRUE, ...)
#' tox <- data.frame(id = rep(1:10, 10), phase = 1:2,
#'                   code = f(rawr::ctcae_v4$tox_code[1:100]),
#'                   grade = f(1:3, prob = c(.6, .3, .1)),
#'                   stringsAsFactors = FALSE)
#' 
#' tox <- cbind(tox, match_ctc(tox$code)$matches[, c('tox_cat', 'tox_desc')])
#' 
#' ## get worst toxicities by id, by grade
#' n <- colSums(table(tox$id, tox$phase) > 0)
#' tox[] <- lapply(tox, factor)
#' tox <- tox_worst(tox, desc = 'tox_desc')$tox_worst
#' 
#' out <- tabler_by2(tox, 'tox_desc', 'grade', stratvar = 'phase', zeros = '.')
#' colnames(out)[1] <- sprintf('Total<br /><font size=1>n = %s</font>', sum(n))
#' cgroup <- c('',
#'             sprintf('Phase I<br /><font size=1>n = %s</font>', n[1]),
#'             sprintf('Phase II<br /><font size=1>n = %s</font>', n[2]))
#' 
#' library('htmlTable')
#' htmlTable(out, ctable = TRUE, cgroup = cgroup, n.cgroup = c(1, 4, 4),
#'     caption = 'Table 1: Toxicities<sup>&dagger;</sup> by phase and grade,
#'                sorted by total.',
#'     col.columns = rep(c('grey97','none','grey97'), times = c(1,4,4)),
#'     col.rgroup = rep(rep(c('none', 'grey97'), each = 5), 10),
#'     tfoot = paste0('<font size=1><sup>&dagger;</sup>Percentages represent ',
#'             'proportion of patients out of respective phase total.</font>'))
#' 
#' 
#' ## same as above but add level of stratification, sort by total within group
#' out2 <- tabler_by2(tox, c('tox_cat', 'tox_desc'), 'grade',
#'                    stratvar = 'phase', zeros = '-', pct = TRUE)
#' stopifnot(
#'   identical(
#'     sort(unname(out[, grep('Total', colnames(out))[1]])),
#'     sort(unname(out2[, grep('Total', colnames(out2))[1]]))
#' ))
#' 
#' colnames(out2)[1:2] <- c(
#'   'Description', sprintf('Total<br /><font size=1>n = %s</font>', sum(n)))
#' 
#' cgroup <- c('', '',
#'             sprintf('Phase I<br /><font size=1>n = %s</font>', n[1]),
#'             sprintf('Phase II<br /><font size=1>n = %s</font>', n[2]))
#'             
#' htmlTable(out2, align = 'lccccccccc', cgroup = cgroup, n.cgroup = c(1,1,4,4),
#'     caption = 'Table 1: Toxicities<sup>&dagger;</sup> by category, phase,
#'                grade.')
#'             
#' @export

tabler_by <- function(data, varname, byvar, n, order = FALSE, zeros = TRUE,
                      pct = TRUE, pct.column = FALSE, pct.total = FALSE,
                      pct.sign = TRUE, drop = TRUE) {
  
  rm_p <- function(x) gsub(' \\(.*\\)$', '', x)
  ord <- function(...) order(..., decreasing = TRUE)
  
  if (!all(wh <- sapply(data[, c(varname, byvar)], is.factor))) {
    warning(sprintf('coercing %s to factor',
                    iprint(shQuote(idx <- c(varname, byvar)[!wh]))),
            domain = NA)
    data[, idx] <- lapply(data[, idx, drop = FALSE], as.factor)
  }
  if (pct.column & missing(n))
    warning('\'n\' must be given when \'pct.column = TRUE\'', domain = NA)
  
  ## use ftbl format later, ttbl for counts, ptbl for percents
  ftbl <- ftable(data[, c(varname, byvar)])
  ttbl <- res <- cbind(Total = rowSums(ftbl), ftbl)
  cn <- c('Total', unlist(attr(ftbl, 'col.vars')))
  nr <- nrow(ttbl)
  nc <- ncol(ttbl)
  
  ## add percents, eg "N (x%)", to each column
  if (missing(n) & any(pct, pct.column, pct.total))
    message('To show percents \'n\' should be given', domain = NA)
  if (pct & !missing(n)) {
    ## if length(n) == 1L, use same n for all strat levels (assume subgroup)
    ## else, map each n to each strat level (assume total)
    if (length(n) == 1L)
      n <- rep(n, ncol(ttbl) - 1)
    if (length(n) != nlevels(data[, byvar]))
      stop('\'n\' should be 1 or equal to nlevels(byvar)')
    
    ## add percents, make them sum to 100 by column
    ## if recursive error in Round, skip to regular round
    ptbl <- ttbl / matrix(rep(c(sum(ttbl[, 1]), n), each = nr), nr) * 100
    ptbl <- tryCatch(apply(ptbl, 2, Round, 100),
                     error = function(e) apply(ptbl, 2, round, 0))
    res <- matrix(sprintf('%s (%s%%)', ttbl, ptbl), nrow = nr, ncol = nc)
    res[] <- gsub('0 (NaN%)', '0 (0%)', res, fixed = TRUE)
    if (!pct.sign)
      res[] <- gsub('%', '', res, fixed = TRUE)
    
    ## split percents into individual columns
    if (pct.column) {
      res <- gsub('[^0-9 ]', '', apply(res, 1, paste0, collapse = ' '))
      res <- as.matrix(read.table(text = res, colClasses = 'character'))
      cn <- interleave(cn, rep('%', nc))
      if (!pct.total) {
        cn <- cn[-2]
        res <- res[, -2]
      }
    } else {
      if (!pct.total)
        res[, 1] <- rm_p(res[, 1])
    }
  }
  
  ## use ftable formatting, replace table with new one
  ftbl <- writeftable(ftbl)
  ftbl <- as.matrix(locf(as.data.frame(ftbl)))
  
  ## column that separates labels from counts
  ftbl[is.na(ftbl)] <- ''
  idx  <- which(colSums(apply(ftbl, 2, Negate(nzchar))) == nr)
  
  res  <- cbind(ftbl[, -(idx:ncol(ftbl)), drop = FALSE], `colnames<-`(res, cn))
  
  ## order by group variable (if given) and total
  o <- if (order) {
    o <- data.frame(res[, c(varname, 'Total')], stringsAsFactors = FALSE)
    o <- within(locf(o), Total <- as.numeric(Total))
    if (length(varname) == 1L)
      ord(o[, 'Total']) else ord(-xtfrm(o[, 1]), o[, 'Total'])
  } else seq(nrow(res))
  res <- res[o, ]
  
  ## remove rows with 0 total since not dropped in ftable
  if (drop)
    res <- res[res[, 'Total'] %ni% '0', ]
  if (length(varname) != 1)
    res[, 1] <- ifelse(duplicated(res[, 1]), '', res[, 1])
  
  if (!isTRUE(zeros)) {
    if (identical(FALSE, zeros))
      zeros = ''
    idx <- idx:ncol(res)
    res[, idx] <- `[<-`(res[, idx], gsub('^0.*', zeros, res[, idx]))
  }
  `rownames<-`(res[, -1], res[, 1])
}

#' @rdname tabler_by
#' @export
tabler_by2 <- function(data, varname, byvar, n, stratvar, zeros = TRUE,
                       pct = FALSE, pct.column = FALSE, pct.total = FALSE,
                       pct.sign = TRUE) {
  
  rm_p <- function(x) gsub(' \\(.*\\)$', '', x)
  ord <- function(...) order(..., decreasing = TRUE)
  
  stopifnot(length(byvar) == 1L &
              (missing(stratvar) || length(stratvar) == 1L) &
              (ln <- length(varname)) <= 2L)
  
  data[] <- lapply(data, as.factor)
  data$`_strat_var_` <- if (missing(stratvar))
    factor(1) else data[, stratvar]
  bylvl <- levels(data[, '_strat_var_'])
  
  ## try to guess n, N if missing
  n <- if (missing(n))
    setNames(colSums(table(data[, 1], data[, '_strat_var_']) > 1), bylvl) else n
  N <- sum(n)
  
  ## get (second varname if ln == 2L and) overall total column(s)
  o1 <- tabler_by(data, varname, '_strat_var_', n, FALSE, zeros,
                  pct, pct.column, pct.total, drop = FALSE)
  o1 <- o1[, 1:(ln + (pct.column & pct.total)), drop = FALSE]
  
  ## get groups of columns for each level of byvar
  o2 <- lapply(bylvl, function(x)
    tabler_by(data[data[, '_strat_var_'] == x, ], varname, byvar, n[x],
              FALSE, zeros, pct, pct.column, pct.total, pct.sign, drop = FALSE))
  
  res <- do.call('cbind', c(list(o1), o2))
  rownames(res) <- locf(rownames(res))
  
  ## remove duplicate columns, rows with 0 total, order using varname input
  res <- t(t(res)[!duplicated(t(res)), ])
  res <- res[!res[, ln] %in% c('0', as.character(zeros)), ]
  res <- res[if (ln == 1L)
    ord(as.numeric(rm_p(res[, 1]))) else
      ord(-xtfrm(rownames(res)), as.numeric(rm_p(res[, ln]))), ]
  
  rownames(res)[duplicated(rownames(res))] <- ''
  res
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
#' @param version version number; default is CTCAE v4
#' 
#' @return
#' A list containing:
#' \item{\code{matches}}{a data frame of matches with toxicity codes and their
#' respective descriptions and categories}
#' \item{\code{version}}{CTCAE version used}
#' 
#' @examples
#' codes <- sample(rawr::ctcae_v4$tox_code, 10)
#' match_ctc(codes)
#' 
#' match_ctc('injury', version = 3L)
#' match_ctc('aortic', 'arterial')
#' 
#' @export

match_ctc <- function(..., version = 4L) {
  x <- unlist(list(...))
  ctc <- if (version %ni% 3:4)
    stop('\'version\' should be 3 or 4') else {
      if (version == 3L)
        rawr::ctcae_v3 else rawr::ctcae_v4
    }
  ## guess if input is code or description
  idx <- if (any(grepl('([A-Za-z -])([0-9])', x)))
    match(gsub('\\s*|-', '', x, perl = TRUE), ctc[, 'tox_code']) else
      grep(paste(x, collapse = '|'), ctc[, 'tox_desc'], ignore.case = TRUE)
  list(matches = `rownames<-`(ctc[idx, ], NULL),
       version = sprintf('CTCAE v%s', version))
}

#' Find most severe toxicities
#' 
#' Returns a subset of input data with worst toxicity grade per toxicity
#' code per patient, ie, sorts and removes duplicates.
#' 
#' @param data toxicity data frame
#' @param id column name with identifier
#' @param desc column name with toxicity descriptions (or codes)
#' @param grade column name with toxiticity grades; should be a factor
#' with the desired order to be properly sorted, i.e., least to most severe
#' @param code,version if \code{code} is given, \code{\link{match_ctc}} will
#' match this column from \code{data} with the CTCAE \code{version} given and
#' return toxicity descriptions rather than codes; if showing toxicity codes
#' is desired, use \code{desc} instead
#' 
#' @return
#' A list of length three with \code{tox_worst}, the filtered data frame;
#' \code{data}, the input data frame sorted by \code{id}, \code{desc},
#' and \code{grade}; and \code{duplicates}, the rows which correspond to
#' duplicate \code{desc} per \code{id} but of an equal or lesser \code{grade}.
#' 
#' @seealso
#' \code{\link{match_ctc}}, \code{\link{tabler_by}}; \code{\link{tabler_by2}}
#' 
#' @examples
#' set.seed(1)
#' f <- function(x, ...) sample(x, 100, replace = TRUE, ...)
#' 
#' tox <- data.frame(id = rep(1:10, 10), phase = 1:2,
#'                   code = f(rawr::ctcae_v4$tox_code[1:10]),
#'                   grade = factor(f(1:3, prob = c(.3, .4, .3))))
#' 
#' (tox1 <- tox_worst(tox, code = 'code'))
#' 
#' tox$desc <- factor(match_ctc(tox$code)$matches$tox_desc)
#' (tox2 <- tox_worst(tox))
#' 
#' stopifnot(identical(tox1, tox2))
#' 
#' ## use tabler_by/tabler_by2 to summarize
#' tabler_by(tox1$tox_worst, 'desc', 'grade', n = 10, pct.sign = FALSE)
#' tabler_by2(tox1$tox_worst, 'desc', 'grade', stratvar = 'phase')
#' 
#' @export

tox_worst <- function(data, id = 'id', desc = 'desc', grade = 'grade',
                      code, version = 4L) {
  if (!is.factor(data[, grade]))
    stop('\'grade\' should be a factor with proper order')
  if (!missing(code)) {
    ctc <- match_ctc(data[, code], version = version)
    data$desc <- factor(ctc$matches$tox_desc)
    desc <- 'desc'
  }
  data <- data[order(data[, id], data[, desc], -xtfrm(data[, grade])), ]
  idx <- which(duplicated(data[, c(id, desc)]))
  list(tox_worst = if (length(idx)) data[-idx, ] else data,
       data = data, duplicates = idx)
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
  iprint(sprintf('%s (n = %s, %s%%)', if (is.logical(lowcase))
    if (lowcase) tolower(names(top)) else toupper(names(top)) else
      names(top), top, round(as.numeric(top) / n * 100)))
}

#' Date parse
#' 
#' Parses day, month, and year columns to the standard date format.
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
#' dmy(25, 7, 2013)
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
#' 
#' ## NAs will be filled with corresponding values of origin
#' dd[, -1] <- lapply(dd[, -1], function(x) {x[sample(nrow(dd), 5)] <- NA; x})
#' cbind(dd, dt = with(dd, dmy(day, month, year, origin = c(15, 6, 2000))))
#' 
#' @export

dmy <- function(d, m, y, origin = c(1, 1, 1900)) {
  f <- function(a, b) {
    suppressWarnings(a <- as.numeric(a))
    ifelse(is.na(a), b, a)
  }
  d[is.na(d)] <- origin[1]
  m[is.na(m)] <- origin[2]
  y[is.na(y)] <- origin[3]
  y <- ifelse(nchar(y) <= 2, f(y, 0) + origin[3], f(y, 0))
  as.Date(sprintf('%04s-%02s-%02s', y, f(m, origin[2]), f(d, origin[1])))
}

#' Combine html tables
#' 
#' Wrapper to easily combine a list of data frames or matrices into html
#' tables using the \pkg{htmlTable} package.
#' 
#' @param l a list of matrices (preferred) or data frames
#' @param ... additional arguments passed to \code{\link[htmlTable]{htmlTable}}
#' 
#' @examples
#' library('htmlTable')
#' sp <- lapply(split(mtcars, rep(1:3, c(1,11,20))), as.matrix)
#' 
#' ## basic table
#' combine_table(sp)
#' 
#' ## adding more options
#' combine_table(sp, tspanner = c('one','eleven','twenty'),
#'   css.cell = 'padding: 0 10 5px;',
#'   css.tspanner = 'text-align: center; color: red; font-style: italic;')
#' 
#' @export

combine_table <- function(l, ...) {
  m <- match.call(expand.dots = FALSE)$`...`
  lx <- sapply(l, function(x) if (is.null(nr <- nrow(x))) 1 else nr)
  ts <- m$tspanner %||% names(l) %||% rep(' ', each = length(lx))
  m$tspanner <- NULL
  do.call('htmlTable', c(list(x = do.call('rbind', l), tspanner = ts,
                              n.tspanner = lx), m))
}
