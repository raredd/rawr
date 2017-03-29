### formatting, knitr, misc utils
# show_html, show_markdown, show_math, roundr, intr, pvalr, pvalr2, catlist,
# binconr, num2char, iprint, writeftable, tabler, tabler_by, tabler_by2,
# tabler_stat, tabler_resp, resp_, r_or_better_, match_ctc, tox_worst, countr,
# dmy, combine_table, inject_div, sparKDT, render_sparkDT, case, write_htmlTable
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
#' \href{http://stackoverflow.com/questions/31193843/display-r-formula-elegantly-as-in-latex}{SO question}
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
#' Uses \code{\link[base]{sprintf}} to round numeric value while keeping
#' trailing zeros.
#'
#' @param x numeric vector, matrix, or data frame
#' @param digits number of digits past the decimal point to keep
#'
#' @return
#' An object having the same class as \code{x}.
#' 
#' @seealso
#' \code{\link[base]{round}}; \code{\link[base]{sprintf}};
#' \code{\link{round_to}}; \code{\link{pvalr}}
#'
#' @examples
#' ## compare
#' round(0.199, 2)
#' roundr(0.199, 2)
#' 
#' ## drops negative when x rounds to 0, eg, case 1:
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

roundr <- function(x, digits = 1L) UseMethod('roundr')

#' @rdname roundr
#' @export
roundr.default <- function(x, digits = 1L) {
  if (!is.numeric(x) || is.complex(x))
    stop('non-numeric argument to mathematical function')
  
  fmt <- paste0('%.', digits, 'f')
  res <- sprintf(fmt, x)
  
  ## drop leading - if value is 0.00...
  zero <- sprintf(fmt, 0)
  res[res == paste0('-', zero)] <- zero
  
  setNames(res, names(x))
}

#' @rdname roundr
#' @export
roundr.data.frame <- function(x, digits = 1L) {
  x[] <- lapply(x, function(ii)
    if (is.numeric(ii) || is.complex(ii))
      roundr.default(ii, digits = digits) else ii)
  x
}

#' @rdname roundr
#' @export
roundr.matrix <- function(x, digits = 1L) {
  if (!is.numeric(x) || is.complex(x))
    stop(deparse(substitute(x)), ' is not numeric')
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
#' are removed from \code{...} before \code{FUN} and \code{\link{quantile}}
#' are computed
#' 
#' @seealso
#' \code{\link[rawr]{roundr}}
#' 
#' @examples
#' intr(1:10)
#' intr(1:10, conf = .95)
#' 
#' # inner quartile range
#' `colnames<-`(cbind(lapply(mtcars, intr, conf = .5),
#'                    lapply(mtcars, intr, fun = mean)),
#'              c('median (IQR)','mean (range)'))
#' # compare to
#' summary(mtcars)
#' 
#' @export

intr <- function(..., fun = median, conf = NULL,
                 digits = 0L, na.rm = FALSE) {
  lst <- list(...)
  if (is.null(conf) || conf == 0 || 
      findInterval(conf, 0:1, rightmost.closed = FALSE) != 1L)
    conf <- 1L
  
  sapply(lst, function(x) {
    bounds <- quantile(x, c((1 - conf) / 2 * c(1,-1) + 0:1),
                       na.rm = na.rm)
    bounds <- roundr(bounds, digits)
    val <- roundr(fun(x, na.rm = na.rm), digits)
    
    if (!conf %in% 0:1)
      sprintf('%s (%s%% CI: %s - %s)', val, conf * 100,
              bounds[1L], bounds[2L])
    else
      sprintf('%s (range: %s - %s)', val,
              bounds[1L], bounds[2L])
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
#' \code{\link[rawr]{roundr}}; \code{\link[base]{format.pval}}
#' 
#' @examples
#' pvals <- c(-1, .00001, .004233, .060123, .13354, .4999, .51, .89, .9, 1)
#' pvalr(pvals)
#' pvalr(pvals, show.p = TRUE, html = TRUE)
#' 
#' @export

pvalr <- function(pvals, sig.limit = 0.001, digits = 3L,
                  html = FALSE, show.p = FALSE) {
  stopifnot(sig.limit > 0, sig.limit < 1)
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
      nd <- c(digits, 2L, 1L)[findInterval(x, c(-Inf, .1, .5, Inf))]
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

#' @rdname pvalr
#' 
#' @param breaks,cols a numeric vector defining breaks in \code{(0,1)} (passed
#' to \code{\link{findInterval}}) and the corresponding colors
#' @param format_pval logical; if \code{TRUE} (default), p-values will first
#' be passed to \code{pvalr} to be formatted
#' 
#' @examples
#' pvals <- c(0.00001, 0.03, .06, .11, .49, .51, .89, .9, 1)
#' show_html(color_pval(pvals))
#' show_html(iprint(color_pval(pvals, show.p = TRUE)))
#' 
#' @export

color_pval <- function(pvals, breaks = c(0, .01, .05, .1, .5, 1),
                       cols = colorRampPalette(2:1)(length(breaks)),
                       sig.limit = 0.001, digits = 3L, show.p = FALSE,
                       format_pval = TRUE) {
  stopifnot(length(breaks) == length(cols))
  
  pvn <- if (!is.numeric(pvals)) {
    warning('p-values are not numeric')
    as.numeric(gsub('[^0-9.]', '', pvals))
  } else pvals
  
  if (format_pval)
    pvals <- rawr::pvalr(pvn, sig.limit, digits, TRUE, show.p)
  
  pvc <- cols[findInterval(pvn, breaks)]
  
  sprintf('<font color=\"%s\">%s</font>', pvc, pvals)
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

#' \code{bincon} formatter
#' 
#' @description
#' Binomial confidence interval (\code{\link{bincon}}) formatter.
#' 
#' Note that this function will also calculate confidence intervals for
#' two-stage designs by \code{method="two-stage"}. If both \code{r} and
#' \code{n} are length 2, a two-stage design is assumed. If \code{r} and
#' \code{n} are not both length 1 or 2, the function will fail. For vector
#' inputs, see \code{\link{bincon}}.
#' 
#' @param r number of responses (successes)
#' @param n number of observations (trials)
#' @param conf level of confidence
#' @param digits number of digits
#' @param est logical; if \code{TRUE}, includes the point estimate
#' @param frac logical; if \code{TRUE}, includes the fraction \code{r/n}
#' @param show_conf logical; if \code{TRUE} includes the confidence level
#' @param pct.sign logical; if \code{TRUE}, percent sign is shown; otherwise,
#' percents are shown without sign (this does not affect the confidence text)
#' @param method method to use (default is exact); see \code{\link{bincon}}
#' 
#' @seealso
#' \code{\link{bincon}}; \code{\link[Hmisc]{binconf}}
#' 
#' @examples
#' binconr(5, 10, .90, est = FALSE)
#' binconr(45, 53, digits = 1, conf = .975)
#' binconr(45, 53, show_conf = FALSE, frac = TRUE)
#' 
#' ## length 2 vectors assume two-stage confidence intervals
#' binconr(c(15, 45), c(20, 33), show_conf = FALSE, frac = TRUE)
#' 
#' @export

binconr <- function(r, n, conf = 0.95, digits = 0, est = TRUE, frac = FALSE,
                    show_conf = TRUE, pct.sign = TRUE, method = 'exact') {
  lr <- length(r)
  ln <- length(n)
  
  method <- if (lr == 2L & ln == 2L)
    'two-stage' else {
      stopifnot(lr == 1L, ln == 1L)
      match.arg(method, c('exact', 'wilson', 'asymptotic'), FALSE)
    }
  
  bc <- bincon(r, n, alpha = 1 - conf, method = method)
  stopifnot(nrow(bc) == 1L)
  
  res <- roundr(bc * 100, digits)
  zzz <- sprintf('%s%% CI: %s - %s%%', conf * 100, res[4], res[5])
  
  if (!show_conf)
    zzz <- gsub('.*% CI: ', '', zzz)
  if (est)
    zzz <- sprintf('%s%% (%s)', res[3], zzz)
  if (!pct.sign)
    zzz <- gsub('%(?= \\()|%(?=\\))', '', zzz, perl = TRUE)
  if (frac)
    sprintf('%s/%s, %s', tail(r, 1L), sum(n), zzz) else zzz
}

#' Numeric to character string
#' 
#' Convert a number to its word equivalent.
#' 
#' Whole numbers twenty-one through ninety-nine are hyphenated when they are
#' written out whether used alone or as part of a larger number; for example: 
#' "twenty-one" or "one million twenty-one."
#' 
#' Whole numbers in this range are \emph{not} hyphenated for other orders of
#' magnitude; for example, 52,052 is written "\emph{fifty two} thousand fifty-
#' two" and not "\emph{fifty-two} thousand fifty-two." This rule applies only
#' to two-word numbers 21-99.
#' 
#' Informal and formal case differ only by the use of "and" to separate
#' 1-99: "one hundred one" is the formal case, and "one hundred and one" is
#' the informal case.
#' 
#' @param x an integer to convert to words; can be negative or positive but
#' decimals will be rounded first
#' @param informal logical; if \code{TRUE}, adds "and" before tens or ones
#' @param cap logical; if \code{TRUE}, capitalizes the first word
#' 
#' @references
#' \url{http://dictionary.reference.com/help/faq/language/g80.html}
#' 
#' @seealso
#' \code{\link{case}}; adapted from
#' \url{github.com/ateucher/useful_code/blob/master/R/numbers2words.r}
#' 
#' @examples
#' num2char(19401, TRUE, TRUE)
#' num2char(19401, FALSE, FALSE)
#' 
#' v <- Vectorize(num2char)
#' x <- c(-1000, 100, 10000, 3922, 3012, 201, -152, 1002, 91765432)
#' setNames(x, v(x))
#' 
#' @export

num2char <- function(x, informal = FALSE, cap = TRUE) {
  neg <- x < 0
  x <- round(abs(x))
  if (isTRUE(all.equal(x, 0)))
    return(ifelse(cap, 'Zero', 'zero'))
  
  scipen <- getOption('scipen')
  options(scipen = 999)
  on.exit(options(scipen = scipen))
  
  ## helpers
  num2char_ <- function(x) {
    digits <- rev(strsplit(as.character(x), '')[[1L]])
    nDigits <- length(digits)
    
    if (nDigits == 1L) as.vector(ones[digits])
    else if (nDigits == 2L)
      if (x <= 19) as.vector(teens[digits[1L]])
    else trim(paste(tens[digits[2L]],
                    Recall(as.numeric(digits[1L]))))
    else if (nDigits == 3L)
      trim(paste(ones[digits[3L]], 'hundred and',
                 Recall(as.num(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1L
      if (nSuffix > length(suffixes))
        stop(x, ' is too large!')
      trim(paste(Recall(as.num(digits[
        nDigits:(3 * nSuffix + 1L)])),
        suffixes[nSuffix], ',' ,
        Recall(as.num(digits[(3 * nSuffix):1]))))
    }
  }
  as.num <- function(...) as.numeric(paste(..., collapse = ''))
  trim <- function(x)
    gsub('\\s*,|,\\s*$|\\s*and\\s*$', '', trimws(x))
  
  ## definitions
  ones <- setNames(
    c('','one','two','three','four','five','six','seven','eight','nine'),
    0:9)
  teens <- setNames(
    c('ten','eleven','twelve',
      paste0(c('thir', 'four','fif','six','seven','eigh','nine'), 'teen')),
    0:9)
  tens <- setNames(
    c('twenty','thirty','forty','fifty','sixty','seventy','eighty','ninety'),
    2:9)
  suffixes <- c('thousand', 'million', 'billion', 'trillion')
  
  ## actual work
  x <- if (length(x) > 1L)
    trim(sapply(x, num2char_)) else num2char_(x)
  if (neg)
    x <- paste('negative', x)
  if (!informal)
    x <- gsub(' and ', ' ', x)
  x <- gsub(sprintf('(.*%s) (%s)$',
                    paste(tens, collapse = '|'),
                    paste(ones, collapse = '|')),
            '\\1-\\2', x)
  if (cap)
    case(x) else x
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
      paste0(paste(f(head(x, -1L), wrap = wrap), collapse = sep),
             copula, f(tail(x, 1L), wrap = wrap))
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
#' @seealso
#' \code{\link{tabler_by}}; \code{\link{tabler_stat}}
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
tabler.lm <- function(x, digits = 3L, ...) {
  res <- data.frame(summary(x, ...)$coefficients, check.names = FALSE)
  res[, ncol(res)] <- pvalr(res[, ncol(res)], ...)
  res[, -ncol(res)] <- lapply(res[, -ncol(res)], round, digits = digits)
  res
}

#' @rdname tabler
#' @export
tabler.glm <- function(x, digits = 3L, level = 0.95, type = '', ...) {
  res <- data.frame(summary(x, ...)$coefficients, check.names = FALSE)
  res[, ncol(res)] <- pvalr(res[, ncol(res)], ...)
  
  if (tolower(type) == 'or') {
    suppressMessages(
      res <- cbind.data.frame(
        exp(cbind(coef(x), confint(x, level = level))), res[, 4L]
      )
    )
    fmt <- '%.df (%.df, %.df)'
    res <- cbind(res, sprintf(chartr('d', as.character(digits), fmt),
                              res[, 1L], res[, 2L], res[, 3L]))
    
    level <- level * 100
    res <- setNames(res, c('OR', paste0(c('L', 'U'), level), 'Pr(>|z|)',
                           sprintf('OR (%s%% CI)', level)))
  }
  
  res[, 1:3] <- lapply(res[, 1:3], round, digits = digits)
  res
}

#' @rdname tabler
#' @export
tabler.survfit <- function(x, ...) surv_table(x, ...)

#' tabler_by
#' 
#' @description
#' This function is helpful for making simple, formatted tables similar to
#' the functionality of \code{\link[tables]{tabular}}.
#' 
#' \code{tabler_by} creates simple tables, and \code{tabler_by2} is a wrapper
#' which can create stratified tables (\code{tabler_by} can also achieve this
#' but requires additional steps).
#' 
#' @details
#' \code{varname} and \code{byvar} should be factors, and the levels will
#' appear in the output as they occur in \code{levels(x)}.
#' 
#' \code{n} is used to calculate the percentages. If missing, the output will
#' only show counts in the table. If given, \code{length(n)} should be one or
#' equal to the number of levels of \code{byvar}.
#' 
#' If one \code{n} is given, \code{tabler_by} assumes that this is the total
#' population for a subgroup, e.g., if creating a table for a subset of the
#' data, it is only necessary to provide the total \code{n} for that group.
#' 
#' If more than one \code{n} is given, \code{tabler_by} assumes that the
#' entire data set is given as \code{data} and will use the corresponding
#' \code{n} for percentages.
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
#' @param pct.sign logical; if \code{TRUE}, percent sign is shown; otherwise,
#' percents are shown in parens without sign
#' @param drop logical; for \code{tabler_by} if \code{TRUE}, rows or columns
#' with zero total counts will be removed (default); the \code{FALSE} case is
#' useful when merging multiple \code{tabler_by} tables (e.g., this is how
#' \code{tabler_by2} aligns stratified tables)
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
#' 
#' 
#' ## when length(n) > 1, each column uses a different n for percents
#' tabler_by(mt, 'vs', 'gear', n = table(mt$gear), pct = TRUE)
#' tabler_by(mt, 'vs', 'gear', n = table(mt$gear), zeros = '-',
#'           pct = TRUE, pct.column = TRUE, pct.total = TRUE)
#' 
#' 
#' ## use tabler_by2 to create a stratified table
#' t1 <- tabler_by2(mt, c('vs', 'carb'), 'gear', stratvar = 'am', order = TRUE)
#' 
#' ## or tabler_by to do this in several steps
#' t2 <- cbind(
#'   tabler_by(mt, varname = c('vs', 'carb'), byvar = 'gear',
#'             drop = FALSE)[, c('carb', 'Total')],
#'   tabler_by(mt[mt$am == 0, ], varname = c('vs', 'carb'), byvar = 'gear',
#'             drop = FALSE)[, -1],
#'   tabler_by(mt[mt$am == 1, ], varname = c('vs', 'carb'), byvar = 'gear',
#'             drop = FALSE)[, -1]
#' )
#' 
#' ## order, drop extra rows/columns, set rownames
#' rownames(t2) <- locf(rownames(t2))
#' t2 <- t2[order(locf(rownames(t2)), -xtfrm(t2[, 2])), ]
#' t2 <- t2[!t2[, 'Total'] %in% '0', ]
#' t2 <- t2[, apply(t2, 2, function(x) !all(x %in% '0'))]
#' rownames(t2)[duplicated(rownames(t2))] <- ''
#' 
#' identical(t1, t2) # [1] TRUE
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
#' out <- tabler_by2(tox, 'tox_desc', 'grade', stratvar = 'phase', zeros = '-')
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
#' out2 <- tabler_by2(tox, c('tox_cat', 'tox_desc'), 'grade', order = TRUE,
#'                    stratvar = 'phase', zeros = '-', pct = TRUE)
#' stopifnot(
#'   identical(
#'     sort(unname(out[, grep('Total', colnames(out))[1]])),
#'     sort(unname(out2[, grep('Total', colnames(out2))[1]]))
#' ))
#' 
#' colnames(out2)[1:2] <- c(
#'   'Description', sprintf('Total<br /><font size=1>n = %s</font>', sum(n))
#' )
#' 
#' cgroup <- c('', '',
#'             sprintf('Phase I<br /><font size=1>n = %s</font>', n[1]),
#'             sprintf('Phase II<br /><font size=1>n = %s</font>', n[2])
#' )
#'             
#' htmlTable(out2, align = 'lccccccccc', cgroup = cgroup, n.cgroup = c(1,1,4,4),
#'     caption = 'Table 1: Toxicities<sup>&dagger;</sup> by category, phase,
#'                grade.')
#'             
#' @export

tabler_by <- function(data, varname, byvar, n, order = FALSE, zeros = TRUE,
                      pct = FALSE, pct.column = FALSE, pct.total = FALSE,
                      pct.sign = FALSE, drop = TRUE) {
  
  rm_p <- function(x) gsub(' \\(.*\\)$', '', x)
  ord  <- function(...) order(..., decreasing = TRUE)
  
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
    warning('\'n\' must be given to show percents', domain = NA)
  if (pct & !missing(n)) {
    ## if length(n) == 1L, use same n for all strat levels (assume subgroup)
    ## else, map each n to each strat level (assume total)
    if ((ln <- length(n)) == 1L)
      n <- rep.int(n, ncol(ttbl) - 1L)
    if (length(n) != nlevels(data[, byvar]))
      stop('\'n\' should be length 1 or nlevels(data[, byvar])')
    
    ## add percents, make them sum to 100 by column
    ## if recursive error in rawr::Round, skip to regular round
    
    ## pct based on counts
    ptbl <- ttbl / matrix(rep(c(sum(ttbl[, 1]), n), each = nr), nr) * 100
    ## pct based on n
    ptbl <- ttbl / matrix(rep(c(sum(n[seq.int(ln)]), n), each = nr), nr) * 100
    
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
      cn <- interleave(cn, rep_len('%', nc))
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
  } else seq.int(nrow(res))
  res <- res[o, ]
  
  ## remove rows with 0 total since not dropped in ftable
  if (drop)
    res <- res[!grepl('^0', res[, 'Total']), ]
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
tabler_by2 <- function(data, varname, byvar, n, order = FALSE, stratvar,
                       zeros = TRUE, pct = FALSE, pct.column = FALSE,
                       pct.total = FALSE, pct.sign = TRUE, drop = TRUE) {
  
  rm_p <- function(x) gsub(' \\(.*\\)$', '', x)
  ord  <- function(...) order(..., decreasing = TRUE)
  
  stopifnot(length(byvar) == 1L,
            (missing(stratvar) || length(stratvar) == 1L),
            (ln <- length(varname)) <= 2L)
  
  data[] <- lapply(data, as.factor)
  data$`_strat_var_` <- if (missing(stratvar))
    factor(1) else data[, stratvar]
  bylvl <- levels(data[, '_strat_var_'])
  
  ## try to guess n, N if missing
  n <- if (missing(n))
    setNames(colSums(table(data[, 1], data[, '_strat_var_']) > 1), bylvl)
  else setNames(n, '1')
  N <- sum(n)
  
  ## get (second varname if ln == 2L and) overall total column(s)
  o1 <- tabler_by(data, varname, '_strat_var_', n, FALSE, zeros,
                  pct, pct.column, pct.total, pct.sign, FALSE)
  o1 <- o1[, 1:(ln + (pct.column & pct.total)), drop = FALSE]
  
  ## get groups of columns for each level of byvar
  o2 <- lapply(bylvl, function(x)
    tabler_by(data[data[, '_strat_var_'] == x, ], varname, byvar, n[x],
              FALSE, zeros, pct, pct.column, pct.total, pct.sign, FALSE))
  
  res <- do.call('cbind', c(if (length(bylvl) == 1L) NULL else list(o1), o2))
  rownames(res) <- locf(rownames(res))
  
  ## remove duplicate columns, rows with 0 total, order using varname input
  # res <- t(t(res)[!duplicated(t(res)), ])
  res <- res[, !(duplicated(colnames(res)) & colnames(res) %in% varname)]
  if (drop) {
    res <- res[, apply(res, 2, function(x)
      !(all(grepl('^\\s*0', x)) | all(x %in% as.character(zeros))))]
    # res <- res[!res[, ln] %in% c('0', as.character(zeros)), ]
    res <- res[!(grepl('^\\s*0', res[, ln]) |
                 res[, ln] %in% as.character(zeros)), ]
  }
  res <- res[if (!order)
    seq.int(nrow(res)) else {
      if (ln == 1L)
        ord(as.numeric(rm_p(res[, 1]))) else
          ord(-xtfrm(rownames(res)), as.numeric(rm_p(res[, ln])))
    }, ]
  
  rownames(res)[duplicated(rownames(res))] <- ''
  res
}

#' Description statistics tabler
#' 
#' Wrapper function to create table of description statistics with tests
#' of association.
#' 
#' If \code{FUN} is \code{FALSE}, no test will be performed. If \code{FUN}
#' is a function, it must take two vector arguments: the row variable vector,
#' \code{data$varname}, and the column variable vector, \code{data$byvar},
#' and return a numeric p-value. Otherwise, \code{FUN} will be selected
#' based on the input vectors:
#' 
#' For a continuous \code{varname}, \code{\link{wilcox.test}} or
#' \code{\link{kruskal.test}} if \code{byvar} has two or more than two levels,
#' respectively. If \code{varname} is not numeric (or has few unique values),
#' \code{fisher.test} is used.
#' 
#' @param data a matrix or data frame with variables \code{varname} and
#' \code{byvar}
#' @param varname,byvar the row and column variable, respectively
#' @param digits number of digits past the decimal point to keep
#' @param FUN a function performing the test of association between
#' \code{varname} and \code{byvar}; \code{FALSE} will suppress the test but
#' keep a column for p-values; \code{NA} will suppress the test and drop the
#' column for p-values; see details
#' @param color_pval logical; if \code{TRUE}, p-values will be colored
#' by significance; see \code{\link{color_pval}}
#' @param color_missing logical; if \code{TRUE}, rows summarizing missing
#' values will be shown in light grey; a color string can be used for a
#' custom color
#' @param dagger logical or a character string giving the character to
#' associate with \code{FUN}; if \code{FALSE}, none are used; if \code{TRUE},
#' the defaults are used (\code{"*"} is used if \code{FUN} is given)
#' @param ... additional arguments passed to
#' \code{\link[Gmisc]{getDescriptionStatsBy}}
#' 
#' @return
#' A matrix with additional attributes:
#' 
#' \item{\code{attr(,"FUN")}}{the test passed to \code{FUN} or the test
#' selected based on \code{varname} and \code{byvar} if \code{FUN = NULL}}
#' \item{\code{attr(,"p.value")}}{the numeric p-value returned by \code{FUN}}
#' \item{\code{attr(,"fnames")}}{a vector of the default \code{FUN} options
#' with names to match the appropriate \code{dagger} character; see examples;
#' if \code{FUN} is given, the function name will be added with a new dagger
#' symbol (\code{"*"} by default or \code{dagger} if given)}
#' \item{\code{attr(,"tfoot")}}{a footnote for the table using each dagger
#' and corresponding test name}
#' 
#' @seealso
#' \code{\link{tabler}}, \code{\link{tabler_by}}
#' 
#' @examples
#' tabler_stat(mtcars, 'mpg', 'cyl')
#' tabler_stat(mtcars, 'mpg', 'cyl', FUN = NA)
#' tabler_stat(mtcars, 'mpg', 'cyl', FUN = FALSE)
#' 
#' tabler_stat(mtcars, 'mpg', 'cyl',
#'   FUN = function(x, y)
#'     cuzick.test(x ~ y, data.frame(x, y))$p.value)
#' 
#' 
#' mt <- within(mtcars, {
#'   mpg[1:5] <- carb[1:5] <- drat[1:20] <- NA
#'   carb <- factor(carb)
#'   cyl  <- factor(cyl)
#' })
#' 
#' tbl <- lapply(names(mt)[-10L], function(x)
#'   tabler_stat(mt, x, 'gear', percentage_sign = FALSE))
#' 
#' htmlTable::htmlTable(
#'   do.call('rbind', tbl),
#'   cgroup = c('', 'Gear', ''), n.cgroup = c(1, 3, 1),
#'   rgroup = names(mt)[-10L], n.rgroup = sapply(tbl, nrow),
#'   tfoot = attr(tbl[[1L]], 'tfoot')
#' )
#' 
#' @export

tabler_stat <- function(data, varname, byvar, digits = 0L, FUN = NULL,
                        color_pval = TRUE, color_missing = TRUE,
                        dagger = TRUE, ...) {
  color_missing <- if (isTRUE(color_missing))
    'lightgrey'
  else if (identical(color_missing, FALSE))
    NULL else color_missing
  
  x <- data[, varname]
  y <- data[, byvar]
  n <- length(unique(na.omit(y)))
  
  res <- Gmisc::getDescriptionStatsBy(
    x, y, digits = digits, html = TRUE, add_total_col = TRUE,
    show_all_values = TRUE, statistics = FALSE, useNA.digits = 0L, ...,
    continuous_fn = function(...) Gmisc::describeMedian(..., iqr = FALSE)
  )
  
  ## recolor missing
  if (is.character(color_missing)) {
    wh <- rownames(res) %in% 'Missing'
    rownames(res)[wh] <-  sprintf('<font color=%s><i>%s</i></font>',
                                  color_missing, rownames(res)[wh])
    res[wh, ] <- sprintf('<font color=%s><i>%s</i></font>',
                         color_missing, res[wh, ])
  }
  
  if (identical(NA, FUN))
    return(structure(res, FUN = FALSE, p.value = NULL, fnames = NULL))
  
  pvn <- if (identical(FUN, FALSE)) {
    color_pval <- FALSE
    structure(NULL, FUN = NULL)
  } else if (is.function(FUN))
    structure(FUN(x, y), FUN = gsub('\\(.*', '', deparse(FUN)[2L]))
  else {
    if (is.null(FUN))
      if (is.numeric(x) & length(unique(x)) / length(x) > 0.5) {
        if (n > 2L)
          structure(Gmisc::getPvalKruskal(x, y), FUN = 'kruskal.test')
        else structure(Gmisc::getPvalWilcox(x, y), FUN = 'wilcox.test')
      } else structure(Gmisc::getPvalFisher(x, y), FUN = 'fisher.test')
  }
  
  fname  <- attr(pvn, 'FUN')
  fnames <- setNames(paste0(c('wilcox', 'kruskal', 'fisher'), '.test'),
                     c('&dagger;','&dagger;','&Dagger;'))
  
  if (!is.null(FUN) & !identical(FUN, FALSE)) {
    if (!is.character(dagger))
      dagger <- '*'
    fnames <- c(fnames, setNames(fname, dagger))
  }
  
  dagger <- if (isTRUE(dagger))
    names(fnames)[match(fname, fnames, 3L)] else
      if (is.character(dagger)) dagger else ''
  fnames[1:3] <- setNames(c('Wilcoxon rank-sum test',
                            'Kruskal-Wallis rank-sum test',
                            'Fisher\'s exact test'),
                          names(fnames)[1:3])
  
  ## only return one of wilcox/kruskal based on byvar
  fnames <- fnames[-((n == 2L) + 1L)]
  
  pvc <- if (is.null(pvn))
    pvn else {
      if (color_pval)
        color_pval(pvn)
      else sprintf('<i>%s</i>', pvalr(pvn, html = TRUE))
    }
  
  m <- matrix('', nrow(res))
  m[1L, 1L] <- if (!length(pvc))
    '-' else sprintf('<i>%s</i><sup>%s</sup>', pvc, dagger)
  
  structure(
    cbind(res, m), FUN = fname, p.value = pvn, fnames = fnames,
    tfoot = toString(sprintf('<sup>%s</sup>%s', names(fnames), fnames))
  )
}

#' Response table
#' 
#' Convenience function to calculate proportions and confidence invervals and
#' format for easy display.
#' 
#' @param x a factor variable of responses; responses should be ordered as
#' CR, PR, SD, PD, NE or similar
#' @param r_or_better if an integer, the first \code{r_or_better} levels of
#' \code{x} will be combined and proportions and confidence intervals will be
#' calculated for the aggregate; if \code{FALSE}; this is not included
#' @param conf,frac,show_conf,pct.sign additional arguments passed to
#' \code{\link{binconr}}
#' @param digits number of digits past the decimal point to keep
#' @param total logical or numeric; if \code{TRUE}, a column with the total,
#' i.e., \code{length(x)} is added; if numeric, \code{length(x)} and,
#' optionally, fracton and percent out of \code{total} is added
#' 
#' @seealso
#' \code{\link{bincon}}; \code{\link{binconr}}
#' 
#' @examples
#' set.seed(1)
#' r <- c('CR','PR','SD','PD','NE')
#' x <- factor(sample(r, 30, replace = TRUE), r)
#' tabler_resp(x, 3)
#' tabler_resp(x, 'PR')
#' tabler_resp(x, 'PR', total = 50)
#' 
#' ## NAs are removed
#' y <- `[<-`(x, 1:10, value = NA)
#' tabler_resp(x, FALSE)
#' tabler_resp(y, FALSE)
#' 
#' library('htmlTable')
#' htmlTable(
#'   rbind(
#'     tabler_resp(x),
#'     tabler_resp(x, conf = 0.9),
#'     tabler_resp(x, frac = FALSE, pct.sign = FALSE,
#'                 show_conf = FALSE, digits = 1)
#'   ),
#'   caption = 'Table of responses with confidence intervals.',
#'   css.cell = 'padding: 0 10 0px; white-space: nowrap;',
#'   cgroup = c('Evaluation', 'Outcome (95% CI)'),
#'   n.cgroup = c(nlevels(x), 3L)
#' )
#' 
#' @export

tabler_resp <- function(x, r_or_better = 3L, conf = 0.95, digits = 0L,
                        frac = TRUE, show_conf = TRUE, pct.sign = TRUE,
                        total = FALSE) {
  r  <- names(table(x))
  lx <- length(x)
  
  if (is.character(r_or_better))
    r_or_better <- if (length(wh <- which(r %in% r_or_better)))
      wh else {
        warning('Failed to guess \'r_or_better\'')
        3L
      }
  
  out <- c(resp_(x, r, conf, digits, frac, show_conf, pct.sign),
           if (!is.numeric(r_or_better))
             NULL else r_or_better_(x, rev(r[seq.int(r_or_better)]),
                                    conf, digits, frac, show_conf, pct.sign))
  
  tot <- if (is.numeric(total))
    sprintf('%s/%s (%s%%)', lx, total, roundr(lx / total * 100, digits)) else lx
  tot <- c(Total = if (!pct.sign) gsub('%', '', tot, fixed = TRUE) else tot)
  if (!frac)
    tot <- gsub('/\\S+', '', tot)
  
  c(if (total) tot else NULL, out)
}

resp_ <- function(x, r, conf, digits, frac, show_conf, pct.sign) {
  # rawr:::resp_(x, levels(x),    .9, 0L, TRUE, TRUE, TRUE)
  # rawr:::resp_(x, c('CR','PR'), .9, 0L, TRUE, TRUE, TRUE)
  FUN <- if ('CR' %ni% r || which(r %in% 'CR') == 1L) identity else rev
  tbl <- table(x)[FUN(r)]
  out <- if (all(is.na(x)))
    rep('-', length(r)) else sapply(tbl, function(X)
      binconr(X, sum(tbl), conf, digits, TRUE, frac,
              show_conf, pct.sign, 'exact'))
  setNames(out, FUN(r))
}

r_or_better_ <- function(x, r, conf, digits, frac, show_conf, pct.sign) {
  # rawr:::r_or_better_(x, c('CR','PR'), .9, 0L, TRUE, TRUE, TRUE)
  x[x %ni% r] <- NA
  out <- if (all(is.na(x)))
    rep('-', length(r)) else
      sapply(seq_along(r), function(X)
        binconr(sum(x %in% r[X:length(r)]), length(x), conf,
                digits, TRUE, frac, show_conf, pct.sign, 'exact'))
  setNames(out, paste(r, 'or better'))
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
#' case if \code{FALSE}, and unchanged for any other value
#' @param frac logical; if \code{TRUE}, the numbers are shown as fractions
#' 
#' @examples
#' top <- setNames(3:1, c('Gold','Silver','Bronze'))
#' countr(names(top))
#' countr(names(top), lowcase = NA)
#' countr(names(top), frac = TRUE)
#' 
#' 
#' countr(top, 10)
#' 
#' @export

countr <- function(top, n, lowcase = TRUE, frac = FALSE) {
  if (inherits(top, 'table')) {
    ## if top is a table, get n
    n <- if (missing(n)) sum(top) else n
  } else {
    ## if top is a vector, get table
    n <- if (missing(n)) length(top) else n
    top <- table(top)
  }
  
  if (is.na(lowcase) || !is.logical(lowcase))
    lowcase <- NULL
  
  iprint(sprintf('%s (n = %s%s, %s%%)', if (isTRUE(lowcase))
    tolower(names(top)) else if (identical(lowcase, FALSE))
      toupper(names(top)) else names(top),
    top, if (frac) paste0('/', n) else '', round(as.numeric(top) / n * 100)))
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
#' @param l a list of matrices or data frames
#' @param tspanner,n.tspanner table spanner labels and number of rows,
#' respectively, passed to \code{\link[htmlTable]{htmlTable}}; if missing,
#' \code{names(l)} and \code{sapply(l, nrow)} is used
#' @param ... additional arguments passed to \code{\link[htmlTable]{htmlTable}}
#' 
#' @examples
#' library('htmlTable')
#' sp <- lapply(split(mtcars, rep(1:3, c(1,11,20))), as.matrix)
#' 
#' ## basic table
#' combine_table(sp)
#' combine_table(sp, letters[1:3], c(2, 5, 25))
#' 
#' ## adding more options
#' combine_table(
#'   caption = 'Table 1: <code>mtcars</code> data set',
#'   sp, tspanner = sapply(sp, function(x) num2char(nrow(x))),
#'   css.cell = 'padding: 0 10 5px;',
#'   css.tspanner = 'text-align: center; color: red; font-style: italic;'
#' )
#' 
#' @export

combine_table <- function(l, tspanner, n.tspanner, ...) {
  l <- if (!islist(l))
    list(l) else l
  n.tspanner <- if (missing(n.tspanner))
    sapply(l, function(x) nrow(x) %||% 1L) else n.tspanner
  tspanner <- if (missing(tspanner))
    names(l) %||% rep(' ', each = length(n.tspanner)) else tspanner
  htmlTable::htmlTable(
    do.call('rbind', l), tspanner = tspanner, n.tspanner = n.tspanner, ...
  )
}

#' Inject div
#' 
#' Inject an html division tag with style attribute.
#' 
#' @param x a matrix or data frame
#' @param where an \code{nx2} matrix of row and column indices or vector (of
#' the form c(row, col, row, col, ...)) specifying which cells to select; if
#' \code{where} is missing, \code{style} is recycled for all cells
#' @param style vector of character string(s) applied to each cell, recycled
#' if necessary
#' 
#' @seealso
#' \code{\link[htmlTable]{htmlTable}}
#' 
#' @examples 
#' library('htmlTable')
#' htmlTable(
#'   inject_div(head(cars), c(2,2), style = 'border: dashed 1px;')
#' )
#' 
#' ## if where is missing, style is recycled over all cells
#' htmlTable(
#'   inject_div(mtcars,
#'              style = c('color: red;', 'color: blue', 'border: dashed 1px;')
#'   )
#' )
#' 
#' htmlTable(
#'   inject_div(head(cars),
#'              rbind(c(2,2), c(2,1), c(5,2)),
#'              'background-color: yellow;')
#' )
#' 
#' htmlTable(
#'   inject_div(head(cars),
#'              c(2,2,2,1,5,2),
#'              c('background-color: red; color: white;',
#'                'border: solid 1px;',
#'                'font-weight: 900; color: blue;')
#'   )
#' )
#' 
#' @export

inject_div <- function(x, where, style) {
  if (missing(style))
    return(x)
  style <- inject_(x, where, style)
  where <- style != ''
  x[where] <- sprintf('<div style=\'%s\'>%s</div>',
                      gsub(';*$', ';', style[where]), x[where])
  x
}

inject_ <- function(x, where, what) {
  where <- if (missing(where) & !missing(what) || !length(where))
    which(row(x) > 0L, arr.ind = TRUE) else
      matrix(where, ncol = 2L, byrow = !is.matrix(where))
  what <- rep_len(what, nrow(where))
  mat  <- matrix('', nrow(x), ncol(x))
  mat[where] <- what
  mat
}

#' \code{datatable}s with sparklines
#' 
#' Create an HTML table widget using the JavaScript library DataTables
#' (\code{\link[DT]{datatable}}) with \code{\link[sparkline]{sparkline}}
#' columns.
#' 
#' @param data a data frame or matrix
#' @param spark a \emph{named} list of lists for each column of \code{data}
#' for which an interactive sparkline will replace each row cell
#' 
#' each named list of \code{spark} should have length \code{nrow(data)} and
#' contain at least one numeric value
#' @param type the type of sparkline, one or more of "line", "bar", "box1",
#' or "box2", recycled as needed; the only difference between "box1" and
#' "box2" is the use of \code{spark_range}
#' @param spark_range an optional list or vector (recycled as needed) giving
#' the overall range for each list of \code{spark}; if missing, the ranges
#' will be calculated; note this is only applicable for \code{type = "line"}
#' or \code{type = "box1"}
#' @param options,... \code{options} or additional arguments passed to
#' \code{\link[DT]{datatable}}
#' 
#' @seealso
#' Adapted from \url{leonawicz.github.io/HtmlWidgetExamples/ex_dt_sparkline.html}
#' 
#' @examples
#' \dontrun{
#' library('DT')
#' 
#' ## strings of data separated by commas should be passed to each row
#' ## this data will be used to generate the sparkline
#' dd <- aggregate(cbind(wt, mpg) ~ gear, mtcars, function(x)
#'   toString(fivenum(x)))
#' sparkDT(dd)
#' 
#' 
#' ## for each column, create a list of vectors for each row to be plotted
#' l <- sapply(c('wt', 'mpg'), function(x)
#'   split(mtcars[, x], mtcars$gear), simplify = FALSE, USE.NAMES = TRUE)
#' 
#' sparkDT(dd, l, type = 'box1')
#' 
#' 
#' set.seed(1)
#' spark <- replicate(nrow(mtcars), round(rnorm(sample(20:100, 1)), 2), FALSE)
#' 
#' sparkDT(mtcars, list(mpg = spark, wt = spark, disp = spark, qsec = spark))
#' 
#' sparkDT(mtcars, list(mpg = spark, wt = spark, disp = spark, qsec = spark),
#'         spark_range = list(disp = c(-5, 5), mpg = c(0, 10)))
#' 
#' ## note difference between box1 (boxes aligned) and box2 (max size)
#' sparkDT(mtcars[, c('mpg', 'wt')],
#'         list(mpg = spark, wt = spark),
#'         type = c('box1', 'box2'),
#'         # range = c(-2, 2),
#'         rownames = FALSE,
#'         colnames = c('box1', 'box2')
#' )
#' }
#' 
#' @export

sparkDT <- function(data, spark, type = c('line', 'bar', 'box1', 'box2'),
                    spark_range, options = list(), ...) {
  data <- as.data.frame(data)
  if (missing(spark))
    return(DT::datatable(data = data, ..., options = options))
  
  srange <- lapply(spark, function(x) range(unlist(x), na.rm = TRUE))
  
  spark_range <- if (missing(spark_range))
    srange else if (!is.list(spark_range))
      setNames(list(spark_range)[rep_len(1L, length(spark))], names(spark))
  else if (length(names(spark_range)))
    modifyList(srange, spark_range) else setNames(spark_range, names(spark))
  
  spark_range <- spark_range[names(spark)]
  
  type <- match.arg(type, several.ok = TRUE)
  type <- rep_len(type, length(spark))
  
  stopifnot(all(names(spark) %in% names(data)))
  
  spark <- rapply(spark, paste, collapse = ', ', how = 'list')
  data[, names(spark)] <- lapply(spark, unlist)
  
  render_sparkDT(data, names(spark), type, spark_range, options, ...)
}

render_sparkDT <- function(data, variables, type, range, options, ...) {
  ## catch case of rownames = FALSE - first spark col does not render
  dots <- lapply(substitute(alist(...))[-1L], eval)
  if (identical(dots$rownames, FALSE))
    dots$rownames <- rep_len('', nrow(data))
  
  targets <- match(variables, names(data))
  idx <- seq_along(targets)
  
  ## each column definition and type need a distinct class with variable name
  columnDefs <- lapply(idx, function(ii)
    list(targets = targets[ii],
         render = DT::JS(
           sprintf("function(data, type, full){ return '<span class=spark%s>' + data + '</span>' }", variables[ii])
         )
    )
  )
  
  type <- lapply(idx, function(ii) {
    bar  <- "type: 'bar' ,  barColor: 'orange', negBarColor: 'purple',      highlightColor: 'black'"
    line <- "type: 'line', lineColor: 'black',    fillColor: '#cccccc', highlightLineColor: 'orange', highlightSpotColor: 'orange'"
    box  <- "type: 'box' , lineColor: 'black', whiskerColor: 'black' ,    outlierFillColor: 'black' ,   outlierLineColor: 'black',  medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"
    
    r <- range[[ii]]
    line_range <- sprintf("%s , chartRangeMin: %s , chartRangeMax: %s",
                          line, r[1], r[2])
    box_range  <- sprintf("%s , chartRangeMin: %s , chartRangeMax: %s",
                          box, r[1], r[2])
    
    types <- list(bar = bar, line = line_range, box1 = box_range, box2 = box)
    
    types[match(type[ii], names(types))]
  })
  
  js <- sapply(idx, function(ii) sprintf(
    "$('.spark%s:not(:has(canvas))').sparkline('html', { %s }); \n",
    variables[ii], type[[ii]])
  )
  js <- sprintf("function (oSettings, json) {\n %s }\n", paste(js, collapse = '\n'))
  
  oo <- list(columnDefs = columnDefs, fnDrawCallback = DT::JS(js))
  dt <- do.call(DT::datatable, c(
    list(data = data, options = modifyList(options, oo)), dots)
  )
  dt$dependencies <- c(dt$dependencies, htmlwidgets::getDependency('sparkline'))
  dt
}

#' Letter case
#' 
#' Convert a text string to several common types of letter case.
#' 
#' This function supports the following letter cases:
#' 
#' \tabular{llllll}{
#' \tab \code{first} \tab \tab \tab \tab
#' \code{Only the first letter is uppercase} \cr
#' \tab \code{upcase} \tab \tab \tab \tab
#' \code{Each Word Is Uppercase} \cr
#' \tab \code{downcase} \tab \tab \tab \tab
#' \code{tHE oPPOSITE oF uPCASE} \cr
#' \tab \code{camelcase} \tab \tab \tab \tab
#' \code{TheStringWillBeInCamelCase} \cr
#' \tab \code{upper} \tab \tab \tab \tab
#' \code{EQUIVALENT TO }\code{\link{toupper}} \cr
#' \tab \code{lower} \tab \tab \tab \tab
#' \code{equivalent to }\code{\link{tolower}} \cr
#' \tab \code{lowup} \tab \tab \tab \tab
#' \code{aLtErNaTiNg cAsEs} \cr
#' \tab \code{uplow} \tab \tab \tab \tab
#' \code{ThE OpPoSiTe oF LoWuP} \cr
#' }
#' 
#' @param x a text string
#' @param case a case to use; can be (unambiguously) abbreviated; see details
#' @param translate logical; if \code{TRUE}, strings will be translated to
#' upper- or lowercase \emph{before} \code{case} is applied to ensure that
#' the case of all characters of \code{x} is uniform
#' 
#' @examples
#' cases <- eval(formals(case)$case)
#' x <- 'the quick brown fox'
#' sapply(cases, case, x = x)
#' 
#' ## all cases are vectorized
#' sapply(cases, case, x = strsplit(x, ' ')[[1]])
#' 
#' case('upCASE', 'upcase', translate = FALSE) ## default
#' case('upCASE', 'upcase', translate = TRUE)
#' 
#' @export

case <- function(x, case = c('first', 'upcase', 'downcase', 'camelcase',
                             'upper', 'lower', 'lowup', 'uplow'),
                 translate = FALSE) {
  alternating <- function(x, seq) {
    x <- strsplit(x, '')
    x <- lapply(x, function(y) {
      substring(y, seq, seq) <-
        toupper(substring(y, seq, seq))
      paste(y, collapse = '')
    })
    unlist(x)
  }
  
  case <- match.arg(case)
  
  if (translate) {
    x <- if (case %in% c('upper', 'uplow'))
      toupper(x) else tolower(x)
  }
  
  case <- switch(case,
    first = '(^.)',
    upcase = '(\\b.)',
    downcase = '(?<=[a-z])(.)',
    camelcase = 'camelcase',
    upper = {x <- toupper(x); TRUE},
    lower = {x <- tolower(x); TRUE},
    lowup = {x <- alternating(x, 0:1); TRUE},
    uplow = {x <- alternating(x, 1:0); TRUE}
  )
  
  if (isTRUE(case))
    x
  else if (case == 'camelcase')
    gsub(' ', '', Recall(x, 'upcase'))
  else gsub(case, '\\U\\1', x, perl = TRUE)
}

#' Write an \code{htmlTable} to file
#' 
#' Write an \code{\link[htmlTable]{htmlTable}} object to a file with optional
#' html attributes.
#' 
#' @param x a string, e.g., the return of \code{\link[htmlTable]{htmlTable}}
#' @param file a character string naming the file to print to; \code{""},
#' the default, prints to the console; other values (\code{NULL}, logicals,
#' etc.) return \code{x}
#' @param attributes logical; if \code{TRUE}, html attributes are added and
#' default border color names are replaced with hexadecimal values
#' 
#' @examples
#' library('htmlTable')
#' x <- htmlTable(head(cars))
#' 
#' write_htmlTable(x)
#' write_htmlTable(x, attributes = FALSE)
#' 
#' @export

write_htmlTable <- function(x, file = '', attributes = TRUE) {
  if (attributes) {
    x <- gsub('gr[ea]y\\s*(?=;)', '#bebebe', x, perl = TRUE)
    x <- paste(
      '<!DOCTYPE html>\n<html>\n<body>',
      x,
      '</body>\n</html>',
      sep = '\n'
    )
    attr(x, 'html') <- TRUE
    class(x) <- c('html', 'htmlTable', 'character')
  }
  if (!is.character(file))
    x else cat(x, file = file)
}
