### formatting, knitr, html-related, misc utils
# show_html, show_markdown, show_math, roundr, roundr.default, roundr.matrix,
# roundr.data.frame, intr, pvalr, pvalr2, color_pval, catlist, binconr,
# num2char, iprint, writeftable, tabler, tabler.default, tabler.lm, tabler.glm,
# tabler.survfit, tabler_by, tabler_by2, tabler_stat, tabler_stat2, tabler_resp,
# match_ctc, tox_worst, countr, dmy, combine_table, combine_table2, inject_div,
# case, write_htmlTable, html_align
#
# S3 methods:
# roundr, tabler
#
## in-line stats
# anova, chisq, cuzick, fisher, jt, kruskal, ca, kw, t, wilcox, logrank
#
# unexported:
# js, getPvalCAtest, getPvalCuzick, getPvalJTtest, getPvalKruskal, getPvalKWtest,
# getPvalTtest, getPval_, guess_test, tabler_stat_list, tabler_stat_html,
# guess_digits, get_tabler_stat_n, resp1, r_or_better1, inject_
###


js <- function() {
  # <div class="fold o">
  # <div class="fold s">
  # <div class="fold s o">
  # https://stackoverflow.com/a/37839683/2994949
  system.file('scripts', 'toggleFold.js', package = 'rawr')
}

#' In-line stats
#'
#' Convenience functions to summarize statistical tests.
#'
#' @param x numeric or factor-like variable, table, or a list depending on
#' the statistic
#' @param y (optional) group or stratification variable
#' @param ... additional arguments passed to the stat function
#' @param details logical; if \code{FALSE}, only the p-value is printed; if
#' \code{TRUE}, additional details, such as the test statistic, degrees of
#' freedom depending on the test, are printed
#' @param digits number of digits past the decimal point to keep
#' @param object for \code{inl_logrank}, a \code{\link[survival]{survdiff}}
#' object or formula to be passed to \code{survdiff}
#'
#' @examples
#' x <- mtcars$vs
#' y <- mtcars$am
#'
#' t.test(x ~ y)
#' inl_t(split(x, y))
#' inl_t(x, y)
#'
#' fisher.test(x, y)
#' inl_fisher(x, y)
#' inl_fisher(table(x, y))
#'
#' chisq.test(table(x, y))
#' inl_chisq(x, y)
#' inl_chisq(table(x, y))
#'
#' wilcox.test(mtcars$mpg ~ y)
#' inl_wilcox(mtcars$mpg, y)
#' inl_wilcox(split(mtcars$mpg, y))
#'
#' cuzick.test(mpg ~ gear, mtcars)
#' inl_cuzick(mtcars$mpg, mtcars$gear)
#' inl_cuzick(split(mtcars$mpg, mtcars$gear))
#'
#' jt.test(table(mtcars$gear, mtcars$cyl))
#' inl_jt(mtcars$gear, mtcars$cyl)
#' inl_jt(table(mtcars$gear, mtcars$cyl))
#' 
#' ca.test(table(mtcars$vs, mtcars$cyl))
#' inl_ca(mtcars$vs, mtcars$cyl)
#' inl_ca(table(mtcars$vs, mtcars$cyl))
#' 
#' kw.test(table(mtcars$gear, mtcars$cyl))
#' inl_kw(mtcars$gear, mtcars$cyl)
#' inl_kw(table(mtcars$gear, mtcars$cyl))
#'
#' library('survival')
#' s1 <- survdiff(Surv(time, status) ~ sex, colon)
#' s2 <- survdiff(Surv(time, status) ~ sex + strata(age), colon)
#' inl_logrank(s1)
#' inl_logrank(s2)
#' inl_logrank(Surv(time, status) ~ sex, data = colon)
#'
#' @name inline_stats
NULL

#' @rdname inline_stats
#' @export
inl_anova <- function(x, y = NULL, ..., details = TRUE, digits = 2L) {
  res <- anova(lm(y ~ x, ...))

  if (!details)
    pvalr(res[['Pr(>F)']][[1L]], show.p = TRUE)
  else {
    sprintf(
      'F: %s (%s df), one-way ANOVA p-value: %s',
      roundr(res[['F value']][[1L]], digits), res$Df[[1L]],
      pvalr(res[['Pr(>F)']][[1L]])
    )
  }
}

#' @rdname inline_stats
#' @export
inl_chisq <- function(x, y = NULL, ..., details = TRUE, digits = 2L) {
  tbl <- if (!is.null(dim(x)))
    x else table(x, y)

  suppressWarnings({
    res <- chisq.test(tbl, ...)
  })

  if (!details)
    pvalr(res$p.value, show.p = TRUE)
  else {
    sprintf(
      '&chi;<sup>2</sup>: %s (%s df), chi-squared p-value: %s',
      roundr(res$statistic, digits), res$parameter, pvalr(res$p.value)
    )
  }
}

#' @rdname inline_stats
#' @export
inl_cuzick <- function(x, y = NULL, ..., details = TRUE, digits = 2L) {
  if (inherits(x, 'list')) {
    x <- rbindlist(x)
    y <- x[, 1L]
    x <- x[, 2L]
  }
  res <- cuzick.test(x ~ as.factor(y), ...)

  if (!details)
    pvalr(res$p.value, show.p = TRUE)
  else {
    sprintf(
      'z: %s (%s ordered groups), Cuzick trend p-value: %s',
      roundr(res$statistic, digits), length(res$estimate), pvalr(res$p.value)
    )
  }
}

#' @rdname inline_stats
#' @export
inl_fisher <- function(x, y = NULL, ..., details = TRUE, digits = 2L) {
  tbl <- if (!is.null(dim(x)))
    x else table(x, y)
  res <- fisher.test(tbl, ...)

  if (!details || any(dim(tbl) > 2L))
    pvalr(res$p.value, show.p = TRUE)
  else {
    sprintf(
      'OR: %s (%s%% CI: %s - %s), Fisher\'s exact p-value: %s',
      roundr(res$estimate, digits), attr(res$conf.int, 'conf.level') * 100,
      roundr(res$conf.int[1L], digits), roundr(res$conf.int[2L], digits),
      pvalr(res$p.value, show.p = FALSE)
    )
  }
}

#' @rdname inline_stats
#' @export
inl_jt <- function(x, y = NULL, ..., details = TRUE, digits = 2L) {
  res <- jt.test(x, y, ...)

  if (!details)
    pvalr(res$p.value, show.p = TRUE)
  else {
    sprintf(
      'z: %s, Jonckheere-Terpstra p-value: %s',
      roundr(res$statistic, digits), pvalr(res$p.value)
    )
  }
}

#' @rdname inline_stats
#' @export
inl_kruskal <- function(x, y = NULL, ..., details = TRUE, digits = 2L) {
  if (inherits(x, 'list')) {
    x <- rbindlist(x)
    y <- x[, 1L]
    x <- x[, 2L]
  }
  res <- kruskal.test(x ~ as.factor(y), ...)

  if (!details)
    pvalr(res$p.value, show.p = TRUE)
  else {
    sprintf(
      '&chi;<sup>2</sup>: %s (%s df), Kruskal-Wallis rank-sum p-value: %s',
      roundr(res$statistic, digits), res$parameter, pvalr(res$p.value)
    )
  }
}

#' @rdname inline_stats
#' @export
inl_ca <- function(x, y = NULL, ..., details = TRUE, digits = 2L) {
  tbl <- if (!is.null(dim(x)))
    x else table(x, y)
  
  suppressWarnings({
    res <- ca.test(tbl, ...)
  })
  
  if (!details)
    pvalr(res$p.value, show.p = TRUE)
  else {
    sprintf(
      '&chi;<sup>2</sup>: %s (%s df), Cochran-Armitage p-value: %s',
      roundr(res$statistic, digits), res$parameter, pvalr(res$p.value)
    )
  }
}

#' @rdname inline_stats
#' @export
inl_kw <- function(x, y = NULL, ..., details = TRUE, digits = 2L) {
  tbl <- if (!is.null(dim(x)))
    x else table(x, y)

  suppressWarnings({
    res <- kw.test(tbl, ...)
  })

  if (!details)
    pvalr(res$p.value, show.p = TRUE)
  else {
    sprintf(
      '&chi;<sup>2</sup>: %s (%s df), test for trend in proportions p-value: %s',
      roundr(res$statistic, digits), res$parameter, pvalr(res$p.value)
    )
  }
}

#' @rdname inline_stats
#' @export
inl_logrank <- function(object, ..., details = TRUE, digits = 2L) {
  res <- lr_pval(object, ..., details = TRUE)
  str <- !is.null(object$strata)

  if (!details)
    pvalr(res$p.value, show.p = TRUE)
  else {
    sprintf(
      '%slog-rank test p-value: %s',
      ifelse(str, 'stratified ', ''), pvalr(res$p.value)
    )
  }
}

#' @rdname inline_stats
#' @export
inl_t <- function(x, y = NULL, ..., details = TRUE, digits = 2L) {
  if (inherits(x, 'list')) {
    x <- rbindlist(x)
    y <- x[, 1L]
    x <- x[, 2L]
  }

  res <- t.test(x ~ as.factor(y), ...)

  if (!details)
    pvalr(res$p.value, show.p = TRUE)
  else {
    sprintf(
      't: %s, %s p-value: %s',
      roundr(res$statistic, digits), res$method, pvalr(res$p.value)
    )
  }
}

#' @rdname inline_stats
#' @export
inl_wilcox <- function(x, y = NULL, ..., details = TRUE, digits = 2L) {
  if (inherits(x, 'list')) {
    x <- rbindlist(x)
    y <- x[, 1L]
    x <- x[, 2L]
  }

  suppressWarnings({
    res <- wilcox.test(x ~ as.factor(y), ...)
  })

  if (!details)
    pvalr(res$p.value, show.p = TRUE)
  else {
    sprintf(
      'w: %s, Wilcoxon rank-sum p-value: %s',
      roundr(res$statistic, digits), pvalr(res$p.value)
    )
  }
}

#' Show HTML
#'
#' Render html in rstudio viewer or browser.
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
  x <- c(...)
  if (is.null(x))
    return(invisible(NULL))

  htmlFile <- tempfile(fileext = '.html')
  writeLines(x, con = htmlFile)

  if (use_viewer)
    tryCatch(
      rstudioapi::viewer(htmlFile),
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
#' @return
#' The html code (invisibly) as a character string.
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
  txt <- list(text = c(...))
  mdk <- do.call(markdown::markdownToHTML, c(txt, markArgs))

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
#' @return
#' The html code (invisibly) as a character string.
#'
#' @seealso
#' \code{\link{show_html}}, \code{\link{show_markdown}},
#' \href{http://detexify.kirelabs.org/classify.html}{draw math symbols},
#' \url{https://stackoverflow.com/q/31193843/2994949}
#'
#' @examples
#' \dontrun{
#' form1 <- '$$A=\\frac{B}{C}$$'
#'
#' form2 <- '$$
#'   \\frac{1}{\\displaystyle 1+
#'   \\frac{1}{\\displaystyle 2+
#'   \\frac{1}{\\displaystyle 3+x}}} +
#'   \\frac{1}{1+\\frac{1}{2+\\frac{1}{3+x}}}
#' $$'
#'
#' form3 <- '\\frac{d}{dx}\\left( \\int_{0}^{x} f(u)\\,du\\right)=f(x)'
#'
#' form4 <- "
#'   \\forall a,b,c \\in \\mathbb{R} \\\\
#'   \\begin{align}
#'                         a + b &= c \\\\
#'                (a + b)(a - b) &= c(a - b) \\\\
#'                     a^2 - b^2 &= ca - cb \\\\
#'                      a^2 - ca &= b^2 - cb \\\\
#'     a^2 - ca + \\frac{c^2}{4} &= b^2 - cb + \\frac{c^2}{4} \\\\
#'          (a - \\frac{c}{2})^2 &= (b - \\frac{c}{2})^2 \\\\
#'              a - \\frac{c}{2} &= b - \\frac{c}{2} \\\\
#'                             a &= b \\qquad \\qquad \\blacksquare \\\\
#'    \\end{align}
#' "
#'
#' show_math(form1)
#' cat(show_math(form4))
#'
#' ## use default browser
#' show_math(form2, use_viewer = FALSE)
#'
#' ## concatenate multiple expressions
#' show_math(form1, form2, form3, css = 'color: red; font-size: 15px;')
#' }
#'
#' @export

show_math <- function(..., css = '', use_viewer = !is.null(getOption('viewer'))) {
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

  ## use \[ expr \] instead of $$ expr $$
  check_expr <- function(x) {
    sprintf('\\[ %s \\]', gsub('\\\\\\[|\\\\]', '', gsub('^\\$+|\\$+$', '', x)))
  }

  x <- paste(sapply(c(...), check_expr), collapse = '<br />')

  if (!nzchar(x))
    return(invisible(NULL))

  show_html(
    sprintf('<span class="math" style="font-size: 24px; %s;">\n', css),
    x, '\n</span>\n', mj, use_viewer = use_viewer
  )
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

roundr <- function(x, digits = 1L) {
  UseMethod('roundr')
}

#' @rdname roundr
#' @export
roundr.default <- function(x, digits = 1L) {
  if (!is.numeric(x) || is.complex(x))
    stop('non-numeric argument to mathematical function')

  fmt <- paste0('%.', digits, 'f')
  res <- sprintf(fmt, x)

  ## drop leading '-' if value is 0.00...
  zero <- sprintf(fmt, 0)
  res[res == paste0('-', zero)] <- zero

  res[is.na(x)] <- NA

  setNames(res, names(x))
}

#' @rdname roundr
#' @export
roundr.matrix <- function(x, digits = 1L) {
  if (!is.numeric(x) || is.complex(x))
    stop(deparse(substitute(x)), ' is not numeric')
  else x[] <- roundr.default(x, digits)

  x
}

#' @rdname roundr
#' @export
roundr.data.frame <- function(x, digits = 1L) {
  x[] <- lapply(x, function(xx)
    if (is.numeric(xx) || is.complex(xx))
      roundr.default(xx, digits = digits) else xx)

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
#' intr(1:10, conf = 0.95)
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
#' @param pv for \code{pvalr}, a numeric value or vector of p-values; for
#' \code{pvalr2}, a vector of p-values as character strings
#' @param sig.limit lower bound for precision; smaller values will be shown as
#' \code{< sig.limit}
#' @param digits number of digits past the decimal point to keep
#' @param html logical; if \code{TRUE}, uses HTML entities for \code{<}
#' and \code{>}
#' @param show.p logical; if \code{TRUE}, inserts \code{p = }, \code{p < }, or
#' \code{p > } where appropriate
#' @param journal logical; if \code{TRUE}, p-values greater than
#' \code{sig.limit} are rounded to \code{digits}
#' @param ... additional arguments passed to \code{\link{format.pval}} or
#' further to \code{\link{format}}
#'
#' @seealso
#' \code{\link[rawr]{roundr}}; \code{\link[base]{format.pval}}
#'
#' @examples
#' pv <- c(-1, .00001, .004233, .060123, .13354, .4999, .51, .89, .9, 1)
#'
#' pvalr(pv)
#' pvalr(pv, journal = TRUE)
#' pvalr(pv, show.p = TRUE, html = TRUE)
#'
#' @export

pvalr <- function(pv, sig.limit = 0.001, digits = 3L, html = FALSE,
                  show.p = FALSE, journal = TRUE, ...) {
  stopifnot(
    sig.limit > 0,
    sig.limit < 1
  )

  show.p <- show.p + 1L
  html   <- html + 1L

  sapply(pv, function(x, sig.limit) {
    if (is.na(x) | !nzchar(x))
      return(NA)
    if (x >= 0.99)
      return(paste0(c('', 'p ')[show.p], c('> ', '&gt; ')[html], '0.99'))
    if (x >= 0.9 && !journal)
      return(paste0(c('', 'p ')[show.p], c('> ', '&gt; ')[html], '0.9'))
    if (x < sig.limit) {
      paste0(c('', 'p ')[show.p], c('< ', '&lt; ')[html],
             format.pval(sig.limit, ...))
    } else {
      nd <- if (journal)
        c(digits, 2L)[findInterval(x, c(-Inf, .1, Inf))]
      else c(digits, 2L, 1L)[findInterval(x, c(-Inf, .1, .5, Inf))]
      paste0(c('', 'p = ')[show.p], roundr(x, nd))
    }
  }, sig.limit)
}

#' @rdname pvalr
#' @examples
#' pv <- c('0.00000', '< 0.001', '0.0', '0.123', '0.6', '1', '1.0', '1.000')
#' pvalr2(pv)
#'
#' @export

pvalr2 <- function(pv, html = FALSE, show.p = FALSE) {
  x <- gsub('^1$', '1.0', pv)
  x <- gsub('(?:^1\\.|\\G)\\K0(?=0*$)', '9', x, perl = TRUE)
  x <- gsub('^1\\.', '> 0.', x)
  x <- gsub('^(0\\.0*)0$', '< \\11', x)

  if (html) {
    x <- gsub('>', '&gt;', x)
    x <- gsub('<', '&lt;', x)
  }

  if (show.p)
    ifelse(grepl('[<>]', pvalr2(pv)), paste0('p ', x), paste0('p = ', x))
  else x
}

#' @rdname pvalr
#'
#' @param breaks,cols a numeric vector defining breaks in \code{(0,1)} (passed
#' to \code{\link{findInterval}}) and the corresponding colors
#' @param format_pval logical; if \code{TRUE}, p-values will be formatted
#' using \code{\link{pvalr}}; alternatively, a function may by used which will
#' be applied to each p-value
#'
#' @examples
#' pv <- c(0, 0.00001, 0.03, .06, .11, .49, .51, .89, .9, 1)
#'
#' show_html(color_pval(pv))
#' show_html(color_pval(pv, format_pval = format.pval))
#' show_html(iprint(color_pval(pv, show.p = TRUE)))
#'
#' @export

color_pval <- function(pv, breaks = c(0, .01, .05, .1, .5, 1),
                       cols = colorRampPalette(2:1)(length(breaks)),
                       sig.limit = 0.001, digits = 3L, show.p = FALSE,
                       format_pval = TRUE, journal = TRUE, ...) {
  if (!is.numeric(pv))
    return(pv)
  pvn <- pv

  stopifnot(
    length(breaks) == length(cols)
  )

  pv <- if (isTRUE(format_pval))
    pvalr(pvn, sig.limit, digits, TRUE, show.p, journal, ...)
  else if (identical(format_pval, FALSE))
    pv
  else format_pval(pv)

  pvc <- cols[findInterval(pvn, breaks)]
  res <- sprintf('<font color=\"%s\">%s</font>', pvc, pv)

  replace(res, grepl('>NA<', res, fixed = TRUE), '-')
}

#' Concatenate list for output
#'
#' Print a \code{list} (usually named) as a character vector or string.
#'
#' @param l a list to concatenate
#' @param sep,collapse passed to \code{\link{paste}} controlling the string
#' to separate name from value and list elements, respectively; if
#' \code{collapse} is a non character string, the result will be a vector
#' of strings
#'
#' @seealso
#' \code{\link{iprint}}
#'
#' @examples
#' l <- list(a = 1:3, b = 2, '4')
#' catlist(l)
#' catlist(l, collapse = FALSE)
#' cat(catlist(par()[1:5], sep = ':\n  ', collapse = '\n'))
#'
#' @export

catlist <- function(l, sep = ' = ', collapse = ', ') {
  res <- paste(names(l), l, sep = sep)

  idx <- !nzchar(names(l))
  res[idx] <- gsub(sprintf('^%s', sep), '', res)[idx]

  if (is.character(collapse))
    paste(res, collapse = collapse) else res
}

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
#' @param percent logical; if \code{TRUE} (default), estimates and intervals
#' are returned as percentages
#'
#' @seealso
#' \code{\link{bincon}}; \code{\link[Hmisc]{binconf}}
#'
#' @examples
#' binconr(5, 10)
#' binconr(5, 10, percent = FALSE)
#'
#' binconr(5, 10, .90, est = FALSE)
#' binconr(45, 53, digits = 1, conf = .975)
#' binconr(45, 53, show_conf = FALSE, frac = TRUE)
#'
#' ## length 2 vectors assume two-stage confidence intervals
#' binconr(c(15, 45), c(20, 33), show_conf = FALSE, frac = TRUE)
#'
#' @export

binconr <- function(r, n, conf = 0.95, digits = 0L, est = TRUE, frac = FALSE,
                    show_conf = TRUE, pct.sign = TRUE, method = 'exact',
                    percent = TRUE) {
  lr <- length(r)
  ln <- length(n)
  xx <- if (!percent) {
    pct.sign <- FALSE
    digits <- ifelse(digits == 0, 2L, pmax(1L, digits))
    1
  } else 100

  method <- if (lr == 2L & ln == 2L)
    'two-stage' else {
      stopifnot(lr == 1L, ln == 1L)
      match.arg(method, c('exact', 'wilson', 'asymptotic'))
    }

  bc <- bincon(r, n, alpha = 1 - conf, method = method)
  stopifnot(
    nrow(bc) == 1L
  )

  tmp <- roundr(bc * xx, digits)
  res <- sprintf('%s%% CI: %s - %s%%', conf * 100, tmp[4L], tmp[5L])

  if (!show_conf)
    res <- gsub('.*% CI: ', '', res)
  if (est)
    res <- sprintf('%s%% (%s)', tmp[3L], res)
  if (!pct.sign)
    res <- gsub('%(?= \\()|%(?=\\))', '', res, perl = TRUE)
  if (frac)
    res <- sprintf('%s/%s, %s', tail(r, 1L), sum(n), res)

  structure(res, method = method)
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

  oo <- options(scipen = 999)
  on.exit(options(oo))

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
        return(x)
      trim(paste(Recall(as.num(digits[
        nDigits:(3 * nSuffix + 1L)])),
        suffixes[nSuffix], ',' ,
        Recall(as.num(digits[(3 * nSuffix):1]))))
    }
  }
  as.num <- function(...) {
    as.numeric(paste(..., collapse = ''))
  }
  trim <- function(x) {
    gsub('\\s*,|,\\s*$|\\s*and\\s*$', '', trimws(x))
  }

  ## definitions
  ones <- setNames(
    c('', 'one', 'two', 'three', 'four', 'five',
      'six', 'seven', 'eight', 'nine'),
    0:9
  )
  teens <- setNames(
    c('ten', 'eleven', 'twelve',
      paste0(c('thir', 'four', 'fif', 'six',
               'seven', 'eigh', 'nine'), 'teen')),
    0:9
  )
  tens <- setNames(
    c('twenty', 'thirty', 'forty', 'fifty',
      'sixty', 'seventy', 'eighty', 'ninety'),
    2:9
  )
  suffixes <- c('thousand', 'million', 'billion', 'trillion')

  ## actual work
  x <- if (length(x) > 1L)
    trim(sapply(x, num2char_)) else num2char_(x)
  if (neg)
    x <- paste('negative', x)
  if (!informal)
    x <- gsub(' and ', ' ', x)

  ## add hyphen between tens and ones
  x <- gsub(
    sprintf('(.*%s) (%s)$', paste(tens, collapse = '|'),
            paste(ones, collapse = '|')), '\\1-\\2', x
  )

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
#' iprint(rnorm(2))
#' iprint(-0.000, 0.100)
#'
#' ## compare numeric and integer default printing
#' iprint(1, 2, 3)
#' iprint(1:3)
#'
#' iprint('fee', 'fi', 'fo', 'fum')
#' iprint(LETTERS[1:5], copula = ', and the letter ')
#' iprint('Thelma', 'Louise', copula = ' & ')
#'
#' @export

iprint <- function (..., wrap = '', sep = ', ', copula = ', and ',
                    digits = if (is.integer(x)) 0L else 2L) {
  x <- c(...)
  if (!(len <- length(x)))
    return('')

  f <- function(x, wrap = '"') sprintf('%s%s%s', wrap, x, wrap)

  if (len == 2L)
    copula <- sub(',', '', copula)
  if (is.numeric(x))
    x <- roundr(x, digits = digits)

  if (len == 1L)
    f(x, wrap)
  else if (len == 2L)
    paste(f(x, wrap), collapse = copula)
  else
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
  stopifnot(
    inherits(x, 'ftable')
  )

  ## add row/col names if blank (ie, if vectors used in ftable)
  rn <- names(attr(x, 'row.vars'))
  names(attr(x, 'row.vars')) <- ifelse(!nzchar(rn), '$$$', rn)
  cn <- names(attr(x, 'col.vars'))
  names(attr(x, 'col.vars')) <- ifelse(!nzchar(cn), '$$$', cn)

  mat <- as.matrix(format(x, quote = quote, digits = digits, ...))
  mat[] <- trimws(mat)

  `colnames<-`(
    mat[-(1:2), , drop = FALSE],
    gsub('$$$', '', Filter(nzchar, mat[1:2, ]), fixed = TRUE)
  )
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
#' @param exp logical; if \code{TRUE}, estimates and confidence intervals are
#' exponentiated (for \code{glm} or \code{coxph} methods only)
#' @param ... additional arguments passed to or from other methods
#'
#' @family tabler
#'
#' @seealso
#' \code{\link{surv_table}}
#'
#' @examples
#' lmfit <- lm(mpg ~ hp + disp + wt, data = mtcars)
#' tabler(lmfit)
#'
#' glmfit <- glm(vs ~ drat + factor(gear), data = mtcars, family = 'binomial')
#' tabler(glmfit)
#' tabler(glmfit, exp = FALSE)
#'
#' library('survival')
#' sfit <- survfit(Surv(time, status) ~ 1, data = cancer, conf.int = 0.9)
#' tabler(sfit)
#'
#' cphfit <- coxph(Surv(time, status) ~ factor(sex) + age, cancer)
#' tabler(cphfit)
#'
#' @export

tabler <- function(x, ...) {
  UseMethod('tabler')
}

#' @rdname tabler
#' @export
tabler.default <- function(x, ...) {
  summary(x, ...)
}

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
tabler.glm <- function(x, digits = 3L, level = 0.95, exp = TRUE, ...) {
  res <- data.frame(summary(x, ...)$coefficients, check.names = FALSE)
  res[, ncol(res)] <- pvalr(res[, ncol(res)], ...)

  suppressMessages(
    res <-
      data.frame(
        exp(cbind(coef(x), confint(x, level = level))), res[, 4L],
        stringsAsFactors = FALSE
      )
  )

  if (!exp)
    res[, 1:3] <- log(res[, 1:3])

  level <- level * 100

  fmt <- sprintf('%%.0%1$sf (%%.0%1$sf, %%.0%1$sf)', digits)
  res <- data.frame(
    res,
    sprintf(fmt, res[, 1L], res[, 2L], res[, 3L]),
    stringsAsFactors = FALSE
  )

  res <- setNames(
    res,
    c(if (exp) 'OR' else 'Est',
      paste0(c('L', 'U'), level), 'Pr(>|z|)',
      sprintf('OR (%s%% CI)', level))
  )
  res[, 1:3] <- lapply(res[, 1:3], round, digits = digits)

  res
}

#' @rdname tabler
#' @export
tabler.survfit <- function(x, ...) {
  surv_table(x, ...)
}

#' @rdname tabler
#' @export
tabler.coxph <- function(x, digits = 3L, level = 0.95, exp = TRUE, ...) {
  res <- data.frame(summary(x, ...)$coefficients, check.names = FALSE)
  res[, ncol(res)] <- pvalr(res[, ncol(res)], ...)

  suppressMessages(
    res <-
      data.frame(
        exp(cbind(coef(x), confint(x, level = level))), res[, 4L],
        stringsAsFactors = FALSE
      )
  )

  if (!exp)
    res[, 1:3] <- log(res[, 1:3])

  level <- level * 100

  fmt <- sprintf('%%.0%1$sf (%%.0%1$sf, %%.0%1$sf)', digits)
  res <- data.frame(
    res,
    sprintf(fmt, res[, 1L], res[, 2L], res[, 3L]),
    stringsAsFactors = FALSE
  )

  res <- setNames(
    res,
    c(ifelse(exp, 'HR', 'Est'),
      paste0(c('L', 'U'), level), 'Pr(>|z|)',
      sprintf('%s (%s%% CI)', ifelse(exp, 'HR', 'Est'), level))
  )
  res[, 1:3] <- lapply(res[, 1:3], round, digits = digits)

  res
}

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
#' \code{n} is used to calculate the percent. If missing, the output will
#' only show counts in the table. If given, \code{length(n)} should be one or
#' equal to the number of levels of \code{byvar}.
#'
#' If one \code{n} is given, \code{tabler_by} assumes that this is the total
#' population for a subgroup, e.g., if creating a table for a subset of the
#' data, it is only necessary to provide the total \code{n} for that group.
#'
#' If more than one \code{n} is given, \code{tabler_by} assumes that the
#' entire data set is given as \code{data} and will use the corresponding
#' \code{n} for percents.
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
#' @family tabler
#'
#' @seealso
#' \code{\link{tox_worst}}; \code{\link{match_ctc}}
#'
#' @examples
#' mt <- within(mtcars, {
#'   am   <- factor(am)
#'   gear <- factor(gear)
#'   vs   <- factor(vs, levels = 0:2)
#'   carb <- factor(carb)
#' })
#'
#' tabler_by(mt, 'vs', 'gear')
#' tabler_by(mt, c('vs', 'carb'), 'gear', order = TRUE)
#'
#'
#' ## when length(n) > 1, each column uses a different n for percents
#' tabler_by(mt, 'vs', 'gear', n = 32, pct = TRUE)
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
#' stopifnot(identical(t1, t2))
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
#' tox <- cbind(tox, match_ctc(tox$code)[, c('tox_cat', 'tox_desc')])
#'
#' ## get worst toxicities by id, by grade
#' n <- colSums(table(tox$id, tox$phase) > 0)
#' tox[] <- lapply(tox, factor)
#' tox <- tox_worst(tox, desc = 'tox_desc')
#'
#' out <- tabler_by2(tox, 'tox_desc', 'grade', stratvar = 'phase', zeros = '-')
#' colnames(out)[1] <- sprintf('Total<br /><font size=1>n = %s</font>', sum(n))
#' cgroup <- c('',
#'             sprintf('Phase I<br /><font size=1>n = %s</font>', n[1]),
#'             sprintf('Phase II<br /><font size=1>n = %s</font>', n[2]))
#'
#' ht <- htmlTable::htmlTable(
#'   out, ctable = TRUE, cgroup = cgroup, n.cgroup = c(1, 4, 4),
#'   caption = 'Table 1: Toxicities<sup>&dagger;</sup> by phase and grade,
#'             sorted by total.',
#'   col.columns = rep(c('grey97','none','grey97'), times = c(1,4,4)),
#'   col.rgroup = rep(rep(c('none', 'grey97'), each = 5), 10),
#'   tfoot = paste0('<font size=1><sup>&dagger;</sup>Percents represent ',
#'            'proportion of patients out of respective phase total.</font>')
#' )
#' structure(ht, clas = 'htmlTable')
#'
#' ## same as above but add level of stratification, sort by total within group
#' out2 <- tabler_by2(tox, c('tox_cat', 'tox_desc'), 'grade', order = TRUE,
#'                    stratvar = 'phase', zeros = '-', n = c(5, 5), pct = TRUE)
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
#' ht <- htmlTable::htmlTable(
#'   out2, align = 'lc', cgroup = cgroup, n.cgroup = c(1,1,4,4),
#'   caption = 'Table 1: Toxicities<sup>&dagger;</sup> by category, phase,
#'   grade.'
#' )
#' structure(ht, clas = 'htmlTable')
#'
#' @export

tabler_by <- function(data, varname, byvar, n, order = FALSE, zeros = TRUE,
                      pct = FALSE, pct.column = FALSE, pct.total = FALSE,
                      pct.sign = FALSE, drop = TRUE) {
  stopifnot(
    length(byvar) == 1L,
    length(varname) <= 2L
  )

  ## helpers
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
  if (missing(n) & length(varname) == 1L)
    n <- c(table(data[, byvar]))
  if (missing(n) & any(pct, pct.column, pct.total))
    warning('\'n\' must be given if pct = TRUE', domain = NA)

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

    ptbl <- tryCatch(apply(ptbl, 2L, Round, 100),
                     error = function(e) apply(ptbl, 2L, round, 0))
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
        cn <- cn[-2L]
        res <- res[, -2L]
      }
    } else {
      if (!pct.total)
        res[, 1L] <- rm_p(res[, 1L])
    }
  }

  ## use ftable formatting, replace table with new one
  ftbl <- writeftable(ftbl)
  ftbl <- as.matrix(locf(as.data.frame(ftbl)))

  ## column that separates labels from counts
  ftbl[is.na(ftbl)] <- ''
  idx  <- which(colSums(apply(ftbl, 2L, Negate(nzchar))) == nr)

  res  <- cbind(ftbl[, -(idx:ncol(ftbl)), drop = FALSE], `colnames<-`(res, cn))

  ## order by group variable (if given) and total
  o <- if (order) {
    o <- data.frame(res[, c(varname, 'Total')], stringsAsFactors = FALSE)
    o <- within(locf(o), Total <- as.numeric(Total))
    if (length(varname) == 1L)
      ord(o[, 'Total']) else ord(-xtfrm(o[, 1L]), o[, 'Total'])
  } else seq.int(nrow(res))
  res <- res[o, ]

  ## remove rows with 0 total since not dropped in ftable
  if (drop)
    res <- res[!grepl('^0', res[, 'Total']), ]
  if (length(varname) != 1L)
    res[, 1L] <- ifelse(duplicated(res[, 1]), '', res[, 1])

  if (!isTRUE(zeros)) {
    if (identical(FALSE, zeros))
      zeros = ''
    idx <- idx:ncol(res)
    res[, idx] <- `[<-`(res[, idx], gsub('^0.*', zeros, res[, idx]))
  }

  `rownames<-`(res[, -1L], res[, 1L])
}

#' @rdname tabler_by
#' @export
tabler_by2 <- function(data, varname, byvar, n, order = FALSE, stratvar,
                       zeros = TRUE, pct = FALSE, pct.column = FALSE,
                       pct.total = FALSE, pct.sign = TRUE, drop = TRUE) {
  ## helpers
  rm_p <- function(x) gsub(' \\(.*\\)$', '', x)
  ord  <- function(...) order(..., decreasing = TRUE)

  stopifnot(
    length(byvar) == 1L,
    (missing(stratvar) || length(stratvar) == 1L),
    (ln <- length(varname)) <= 2L
  )

  data[] <- lapply(data, as.factor)
  data$`_strat_var_` <- if (missing(stratvar))
    factor(1) else data[, stratvar]
  bylvl <- levels(data[, '_strat_var_'])

  if (pct && missing(n)) {
    warning('\'n\' must be given if pct = TRUE', domain = NA)
    pct <- FALSE
  } else if (!missing(n))
    names(n) <- bylvl

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
    res <- res[, apply(res, 2L, function(x)
      !(all(grepl('^\\s*0', x)) | all(x %in% as.character(zeros)))),
      drop = FALSE]
    # res <- res[!res[, ln] %in% c('0', as.character(zeros)), , drop = FALSE]
    res <- res[!(grepl('^\\s*0', res[, ln]) |
                   res[, ln] %in% as.character(zeros)), , drop = FALSE]
  }
  res <- res[if (!order)
    seq.int(nrow(res)) else {
      if (ln == 1L)
        ord(as.numeric(rm_p(res[, 1L]))) else
          ord(-xtfrm(rownames(res)), as.numeric(rm_p(res[, ln])))
    }, , drop = FALSE]
  rownames(res)[duplicated(rownames(res))] <- ''

  res
}

#' Description statistics \code{tabler}
#'
#' Wrapper function to create table of description statistics with optional
#' tests of association.
#'
#' If \code{FUN} is \code{FALSE}, no test will be performed but a column for
#' p-values will still be added; \code{FUN = NA} will prevent both the test
#' from being performed and the column from being added to the output.
#'
#' For \code{FUN = NULL} (default), the correct test will be guessed based on
#' the row and column data. The current options are \code{\link{fisher.test}},
#' \code{\link{wilcox.test}}, and \code{\link{kruskal.test}}. If the row data
#' is continuous, one of the latter two tests will be used based on the number
#' of unique values in the column data.
#'
#' For special cases, the function is not always guessed correctly (e.g., if
#' the row data contains few unique values, a Fisher test may be used where
#' not appropriate). One of the default tests can be given explicitly with a
#' character string, one of \code{"fisher"}, \code{"wilcoxon"}, \code{"ttest"},
#' \code{"kruskal"}, \code{"chisq"}, \code{"anova"}, \code{"cuzick"},
#' \code{"jt"}, \code{"ca"}, or \code{"kw"} (can be abbreviated).
#'
#' If \code{FUN} is a function, it must take two vector arguments: the row
#' variable vector, \code{data$varname}, and the column variable vector,
#' \code{data$byvar}, in this order and return a numeric p-value only.
#'
#' @param data a matrix or data frame with variables \code{varname} and
#' \code{byvar}
#' @param varname,byvar the row and column variable, respectively
#' @param digits number of digits past the decimal point to keep
#' @param FUN a function performing the test of association between
#' \code{varname} and \code{byvar}; \code{FALSE} will suppress the test but
#' keep a column for p-values; \code{NA} will suppress the test and drop the
#' column for p-values; or a character string; see details
#' @param format_pval logical; if \code{TRUE}, p-values will be formatted
#' using \code{\link{pvalr}}; alternatively, a function may by used which will
#' be applied to each p-value
#' @param color_pval logical; if \code{TRUE}, p-values will be colored
#' by significance; see \code{\link{color_pval}}; alternatively, a vector of
#' colors passed to \code{\link{color_pval}})
#' @param color_missing logical; if \code{TRUE}, rows summarizing missing
#' values will be shown in light grey; alternatively, a color string can be
#' used for a custom color
#' @param dagger logical or a character string giving the character to
#' associate with \code{FUN}; if \code{FALSE}, none are used; if \code{TRUE},
#' the defaults are used (\code{"*"} is used if \code{FUN} is given)
#' @param color_cell_by apply a color gradient to each cell (for html output);
#' one of \code{"none"} for no coloring, \code{"value"} to color by numeric
#' summary (e.g., for continuous variables), or \code{"pct"} to color by
#' proportions (e.g., for factors)
#' @param cell_color a vector of colors used for \code{color_cell_by}
#' @param confint logical or \code{varname}; if \code{TRUE} (or \code{varname})
#' rows will be formatted as confidence intervals; see \code{\link{binconr}}
#' @param continuous_fn a function to describe continuous variables (default
#' is to show median and range); see \code{\link[Gmisc]{getDescriptionStatsBy}}
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
#' @family tabler
#'
#' @seealso
#' \code{\link{fisher.test}}; \code{\link{wilcox.test}}; \code{\link{t.test}};
#' \code{\link{kruskal.test}}; \code{\link{chisq.test}}; \code{\link{anova}};
#' \code{\link{cuzick.test}}; \code{\link{jt.test}}; \code{\link{kw.test}};
#' \code{\link{ca.test}}
#'
#' @examples
#' tabler_stat(mtcars, 'mpg', 'cyl') ## picks kruskal-wallis
#' tabler_stat(mtcars, 'mpg', 'cyl', FUN = NA)    ## no test, no p-value column
#' tabler_stat(mtcars, 'mpg', 'cyl', FUN = FALSE) ## no test, p-value column
#' tabler_stat(mtcars, 'mpg', 'cyl', FUN = 'fisher') ## force fisher test
#' tabler_stat(mtcars, 'mpg', 'cyl', FUN = 'anova')  ## force anova test
#'
#' ## use of a custom function - see ?rawr::cuzick.test
#' tabler_stat(mtcars, 'mpg', 'cyl',
#'   continuous_fn = Gmisc::describeMean,
#'   FUN = function(x, y)
#'     cuzick.test(x ~ y, data.frame(x, y))$p.value)
#'
#' ## "cuzick" is also an option for FUN
#' tabler_stat(mtcars, 'mpg', 'cyl', FUN = 'cuzick')
#'
#'
#' ## typical usage
#' mt <- within(mtcars, {
#'   mpg[1:5] <- carb[1:5] <- drat[1:20] <- NA
#'   carb <- factor(carb, ordered = TRUE)
#'   cyl  <- factor(cyl)
#' })
#'
#' tbl <- lapply(names(mt)[-10L], function(x)
#'   tabler_stat(mt, x, 'gear', percentage_sign = FALSE,
#'               color_cell_by = ifelse(is.factor(mt[, x]), 'pct', 'none'),
#'               continuous_fn = Gmisc::describeMean))
#'
#' ht <- htmlTable::htmlTable(
#'   do.call('rbind', tbl),
#'   cgroup = c('', 'Gear', ''), n.cgroup = c(1, 3, 1),
#'   rgroup = names(mt)[-10L], n.rgroup = sapply(tbl, nrow),
#'   tfoot = toString(unique(unlist(strsplit(sapply(seq_along(tbl), function(ii)
#'     attr(tbl[[ii]], 'tfoot')), ', '))))
#' )
#' structure(ht, class = 'htmlTable')
#'
#'
#' ## use the tabler_stat2 wrapper for convenience
#' tabler_stat2(mt, names(mt)[-10L], 'gear')
#'
#' tabler_stat2(mt, names(mt)[-10L], 'gear', FUN = c(cyl = 'jt'))
#'
#' mt$gear <- factor(mt$gear, ordered = TRUE)
#' tabler_stat2(mt, names(mt)[-10L], 'gear',
#'   format_pval = function(x) format.pval(x, digits = 3))
#'
#' @export

tabler_stat <- function(data, varname, byvar = NULL, digits = 0L, FUN = NULL,
                        format_pval = TRUE, color_pval = TRUE,
                        color_missing = TRUE, dagger = TRUE,
                        color_cell_by = c('none', 'value', 'pct'),
                        cell_color = palette()[1:2], confint = FALSE,
                        continuous_fn = function(...)
                          Gmisc::describeMedian(..., iqr = FALSE), ...) {
  fun <- deparse(substitute(FUN))
  nof <- identical(FUN, FALSE)
  color_missing <- if (isTRUE(color_missing))
    'lightgrey'
  else if (identical(color_missing, FALSE))
    NULL else color_missing
  pcol <- if (isTRUE(color_pval))
    palette()[2:1]
  else if (!identical(color_pval, FALSE)) {
    pcol <- color_pval
    color_pval <- TRUE
    pcol
  } else NULL

  if (is.null(byvar)) {
    FUN   <- NA
    byvar <- '_by_var_'
    data[, byvar] <- factor(1L, 1:2)
  }

  x <- if (is.character(x <- data[, varname]))
    as.factor(x) else x
  y <- as.factor(data[, byvar])
  n <- length(unique(na.omit(y)))

  oy <- is.ordered(y)
  if (oy & nlevels(y) < 3L) {
    warning(sprintf('%s is ordered has < 3 unique values', shQuote(byvar)))
    y <- factor(y, ordered = FALSE)
    oy <- FALSE
  }
  ox <- is.ordered(x)
  if (ox & nlevels(x) < 3L) {
    warning(sprintf('%s is ordered has < 3 unique values', shQuote(varname)))
    x <- factor(x, ordered = FALSE)
    ox <- FALSE
  }

  confint <- if (isTRUE(confint))
    varname else if (identical(confint, FALSE)) NULL else confint


  res <- if (inherits(x, c('Date', 'POSIXct', 'POSIXt'))) {
    color_cell_by <- 'none'
    FUN <- if (length(FUN) && is.na(FUN))
      NA else FALSE
    describeDateBy(x, y, ...)
  } else if (varname %in% confint) {
    describeConfInt(x, y, ...)
  } else {
    Gmisc::getDescriptionStatsBy(
      x, y, digits = digits, html = TRUE, add_total_col = TRUE,
      show_all_values = TRUE, statistics = FALSE, useNA.digits = 0L,
      continuous_fn = continuous_fn, ...
    )
  }
  class(res) <- 'matrix'
  ## sub out ' ( - )' pattern (all NA in continuous var)
  res[] <- gsub('\\s*\\(\\s*\\-\\s*\\)', '-', res)

  ## recolor missing
  if (is.character(color_missing)) {
    wh <- rownames(res) %in% 'Missing'
    rownames(res)[wh] <- sprintf('<font color=%s><em>%s</em></font>',
                                 color_missing, rownames(res)[wh])
    res[wh, ] <- sprintf('<font color=%s><em>%s</em></font>',
                         color_missing, res[wh, ])
  }

  ## color cells of by variable by proportion or value
  ## assume prop is in parens and value starts string
  ## eg, -value (proportion%)
  color_cell_by <- match.arg(color_cell_by)
  if (color_cell_by %ni% 'none') {
    wh <- !wh
    pp <- switch(color_cell_by,
                 value = '(^[^(]+)|.',
                 pct   = '\\(.*?([0-9.]+).*?\\)|.')
    pp <- gsub(pp, '\\1', res[wh, -1L], perl = TRUE)

    res[wh, -1L] <-
      sprintf('<font color="%s">%s</font>',
              col_scaler(as.numeric(pp), cell_color), res[wh, -1L])
  }

  ## no stat fn, no pvalue column
  if (identical(NA, FUN))
    return(
      structure(if (byvar == '_by_var_')
        res[, 1L, drop = FALSE] else res, FUN = FALSE, tfoot = '')
    )

  ## add pvalue column using stat fn
  pvn <- tryCatch(
    getPval_(x, y, FUN),
    error = function(e) {
      message(sprintf(
        '\nAn error occurred for %s\n\t%s\nSkipping test for %s\n',
        shQuote(varname), e$message, shQuote(varname))
      )
      NULL
    }
  )
  if (is.null(pvn))
    nof <- TRUE

  dags  <- c('&dagger;', '&Dagger;', '&#94;', '&sect;', 'v')
  dags1 <- dags[c(1, 3, 2)]
  dags2 <- dags[c(3, 4, 1)]
  dags3 <- dags[c(1, 1, 5)]
  fname <- attr(pvn, 'FUN') %||% fun
  attr(fname, 'tnames') <- attr(pvn, 'name')

  fnamel <- list(
    unordered = structure(
      setNames(paste0(c('wilcox', 'kruskal', 'fisher'), '.test'), dags1),
      tnames = c('Wilcoxon rank-sum test', 'Kruskal-Wallis rank-sum test',
                 'Fisher\'s exact test')
    ),
    ordered1 = structure(
      setNames(paste0(c('ca', 'kw', 'cuzick'), '.test'), dags2),
      tnames = c('Cochran-Armitage test', 'Test for trend in proportions',
                 'Cuzick\'s trend test')
    ),
    ordered2 = structure(
      setNames(paste0(c('cuzick', 'cuzick', 'jt'), '.test'), dags3),
      tnames = c('Cuzick\'s trend test', 'Cuzick\'s trend test',
                 'Jonckheere-Terpstra test')
    )
  )
  fnames <- fnamel[[1L + oy + ox]]

  ## no fun - null out return
  ## user input fns get * identifier
  ## else guess/construct footnote string
  if (nof) {
    fname   <- FALSE
    p.value <- NULL
    fnames  <- NULL
  } else if (!is.null(FUN)) {
    fname <- tryCatch({
      # FUN <- match.arg(FUN, unique(unlist(fnamel)))
      fn <- lapply(fnamel, function(x) {
        idx <- match(fname, x)[1L]
        # idx <- tail(match(fname, x), 1L)
        if (!is.na(idx))
          structure(x[idx], tnames = attr(x, 'tnames')[idx])
        else NULL
      })
      Filter(Negate(is.null), fn)[[1L]]
    }, error = function(e) fname
    )
    if (!is.character(dagger))
      dagger <- names(fname) %||% '&#42;'
    fnames <- setNames(attr(fname, 'tnames'), dagger)
  } else {
    dagger <- if (isTRUE(dagger))
      names(fnames)[match(fname, fnames, 3L)]
    else if (is.character(dagger))
      dagger else ''
    fnames <- setNames(attr(fnames, 'tnames'), names(fnames))
    fnames <- fnames[!duplicated(fnames)]

    ## for fnamel$unordered, only return one of wilcox/kruskal based on byvar
    if (!oy & !ox) {
      idx <- gsub('^kw\\.$', 'krus', substr(tolower(fname), 1L, 3L))
      idx <- pmatch(c(idx, 'fish'), unique(tolower(fnames)))
      if (anyNA(idx))
        idx <- c((n > 2L) + 1L, na.omit(idx))
      fnames <- fnames[sort(idx)]
      fnames <- fnames[!duplicated(fnames)]
    }
  }

  pvc <- if (is.null(pvn) || is.na(pvn))
    NULL else {
      if (color_pval)
        color_pval(pvn, cols = colorRampPalette(pcol)(6L),
                   format_pval = format_pval)
      else if (isTRUE(format_pval))
        sprintf('<em>%s</em>', pvalr(pvn, html = TRUE))
      else if (identical(format_pval, FALSE))
        pvn else format_pval(pvn)
    }

  m <- matrix('', nrow(res))
  m[1L, 1L] <- if (!length(pvc))
    '-' else sprintf('<em>%s</em><sup>%s</sup>', pvc, dagger)

  structure(
    cbind(res, m), FUN = unname(fname), p.value = pvn, fnames = fnames,
    tfoot = if (nof)
      '' else toString(sprintf('<sup>%s</sup>%s', names(fnames), fnames))
  )
}

getPvalCAtest <- function(x, by) {
  ca.test(x, by)$p.value
}

getPvalCuzick <- function(x, by) {
  cuzick.test(x ~ by)$p.value
}

getPvalJTtest <- function(x, by) {
  jt.test(x, by)$p.value
}

getPvalKruskal <- function(x, by) {
  ## Gmisc:::getPvalKruskal fails if by is a char
  kruskal.test(x ~ as.factor(by))$p.value
}

getPvalKWtest <- function(x, by) {
  kw.test(x, by)$p.value
}

getPvalTtest <- function(x, by) {
  t.test(x ~ by, alternative = 'two.sided')$p.value
}

getPval_ <- function(x, y, FUN, n_unique_x = 10L) {
  if (identical(FUN, FALSE))
    return(NULL)

  if (is.numeric(FUN))
    return(FUN)

  if (is.function(FUN))
    return(
      structure(FUN(x, y), FUN = gsub('\\(.*', '', deparse(FUN)[2L]),
                name = deparse(FUN)[2L])
    )

  sFUN <- tryCatch(
    match.arg(FUN, c('anova', 'ca', 'chisq', 'cuzick', 'fisher',
                     'jt', 'kruskal', 'kw', 'ttest', 'wilcoxon')),
    error = function(e)
      if (grepl('one of', e$message))
        TRUE else stop(e$message)
  )

  if (isTRUE(sFUN))
    return(FUN)

  if (is.character(FUN))
    return(
      switch(
        sFUN,
        anova    = structure(
          Gmisc::getPvalAnova(x, y),  FUN = 'anova',
          name = 'ANOVA F-test'),
        ca       = structure(
          getPvalCAtest(x, y),        FUN = 'ca.test',
          name = 'Cochran-Armitage test for trend'),
        chisq    = structure(
          Gmisc::getPvalChiSq(x, y),  FUN = 'chisq.test',
          name = 'Pearson\'s chi-squared test'),
        cuzick   = structure(
          getPvalCuzick(x, y),        FUN = 'cuzick.test',
          name = 'Cuzick\'s trend test'),
        fisher   = structure(
          Gmisc::getPvalFisher(x, y), FUN = 'fisher.test',
          name = 'Fisher\'s exact test'),
        jt       = structure(
          getPvalJTtest(x, y),        FUN = 'jt.test',
          name = 'Jonckheere-Terpstra test'),
        kruskal  = structure(
          getPvalKruskal(x, y),       FUN = 'kruskal.test',
          name = 'Kruskal-Wallis rank-sum test'),
        kw       = structure(
          getPvalKWtest(x, y),        FUN = 'kw.test',
          name = 'Chi-squared test for trend in proportions'),
        ttest    = structure(
          getPvalTtest(x, y),         FUN = 't.test',
          name = 'Unpaired T-test'),
        wilcoxon = structure(
          Gmisc::getPvalWilcox(x, y), FUN = 'wilcox.test',
          name = 'Wilcoxon rank-sum test')
      )
    )

  if (is.null(FUN))
    guess_test(x, y, n_unique_x)
  else NULL
}

guess_test <- function(x, y, n_unique_x = 10L) {
  ## guess stat test for table var (x) using stratification var (y)
  ## x with >= n_unique_x unique values is assumed continuous
  ## dbl/int with many (?) unique values uses rank-sum tests
  ## otherwise assume contingency table
  ox <- is.ordered(x)
  oy <- is.ordered(y)
  ny <- lunique(y, na.rm = TRUE)
  nx <- lunique(x, na.rm = TRUE)

  if (!is.character(x) && !is.factor(x) && nx >= n_unique_x) {
    if (ny > 2L) {
      if (oy)
        structure(getPvalCuzick(x, y), FUN = 'cuzick.test',
                  name = 'Cuzick\'s trend test')
      else
        structure(getPvalKruskal(x, y),  FUN = 'kruskal.test',
                  name = 'Kruskal-Wallis rank-sum test')
    } else {
      structure(Gmisc::getPvalWilcox(x, y), FUN = 'wilcox.test',
                name = 'Wilcoxon rank-sum test')
    }
  } else {
    if (ox & oy & nx > 2L & ny > 2L)
      structure(getPvalJTtest(x, y), FUN = 'jt.test',
                name = 'Jonckheere-Terpstra test')
    else if ((nx == 2L & oy & ny > 2L) || (ny == 2L & ox & nx > 2L)) {
      if (ny > 2L)
        structure(getPvalCAtest(x, y), FUN = 'ca.test',
                  name = 'Cochran-Armitage Test for Trend')
      else
        structure(getPvalCAtest(x, y), FUN = 'ca.test',
                  name = 'Cochran-Armitage Test for Trend')
    } else if ((nx > 2L & !ox & oy & ny > 2L) || (ny > 2L & !oy & ox & nx > 2L)) {
      if (ny > 2L)
        structure(getPvalKWtest(x, y), FUN = 'kw.test',
                  name = 'Chi-squared test for trend in proportions')
      else
        structure(getPvalKWtest(y, x), FUN = 'kw.test',
                  name = 'Chi-squared test for trend in proportions')
    }
    else
      structure(Gmisc::getPvalFisher(x, y), FUN = 'fisher.test',
                name = 'Fisher\'s exact test')
  }
}

#' Describe a date
#'
#' A function that returns the range of a date object.
#'
#' @param x,by vectors of dates and the variable to split \code{x} by
#' @param format the date format; see \code{\link{strptime}}
#' @param copula character string to separate the range
#' @param FUN a function to summarize \code{x}, usually \code{range} or
#' \code{min}
#' @param add_total_col logical, \code{"first"}, or \code{"last"}; adds
#' the total column to the output
#' @param useNA how to handle missing values, one of \code{"ifany"} (default),
#' \code{"no"}, or \code{"always"}
#' @param useNA.digits number of digits to use for missing percentages
#' @param percentage_sign logical; if \code{TRUE}, percent signs are added
#'
#' @seealso
#' \code{\link[Gmisc]{getDescriptionStatsBy}}
#'
#' @examples
#' x <- c(1:4, NA, 5)
#' d <- as.Date(x, origin = Sys.Date())
#' y <- c(1, 1, 2, 2, 1, 3)
#' describeDate(x)
#' describeDateBy(d, y, copula = ' - ', format = '%d %B')
#' describeDateBy(d, y, percentage_sign = TRUE, useNA.digits = 2, FUN = min)
#'
#' dd <- data.frame(x, y, d)
#' tabler_stat(dd, 'd', 'y')
#' tabler_stat2(dd, c(Integer = 'x', Date = 'd'), c(Byvar = 'y'))
#'
#' @export

describeDate <- function(x, format = '%b %d, %Y', copula = ' to ', FUN = range) {
  x <- x[!is.na(x)]
  FUN <- match.fun(FUN)
  res <- format(FUN(x), format = format)

  if (length(res) == 1L)
    res else sprintf('%s%s%s', res[1L], copula, res[2L])
}

#' @rdname describeDate
#' @export
describeDateBy <- function(x, by, format = '%b %d, %Y', copula = ' to ',
                           FUN = range, add_total_col = TRUE,
                           useNA = c('ifany', 'no', 'always'),
                           useNA.digits = 0L, percentage_sign = FALSE) {
  nax <- is.na(x)
  nay <- is.na(by)

  if (anyNA(by)) {
    warning(
      '\n Your \'by\' variable has ', sum(nay), ' missing values\n',
      '  The corresponding \'x\' and \'by\' variables are automatically removed'
    )
    x <- x[!nay]
    by <- by[!nay]
  }

  by_y <- by(x, by, function(x)
    describeDate(x, format = format, copula = copula, FUN = FUN))
  by_y <- matrix(unlist(by_y), ncol = length(by_y),
                 dimnames = list('Date', names(by_y)))
  by_t <- describeDate(x, format = format, copula = copula, FUN = FUN)
  by_t <- matrix(by_t, dimnames = list('Date', 'Total'))

  res <- if (isTRUE(add_total_col) || add_total_col %in% 'first')
    cbind(by_t, by_y)
  else if (add_total_col %in% 'last')
    cbind(by_y, by_t)
  else by_y

  useNA <- match.arg(useNA)

  if (useNA %in% 'always' || (anyNA(x) & !useNA %in% 'no')) {
    mis <- c(list(x), split(x, by))
    mis <- sapply(mis, function(m) {
      s <- sum(is.na(m))
      sprintf('%s (%s%%)', s, if (s == 0)
        '0' else roundr(s / length(m) * 100, useNA.digits))
    })
    if (!percentage_sign)
      mis <- gsub('%', '', mis)

    res <- rbind(res, Missing = mis)
  }

  res
}

describeConfInt <- function(x, y, include_NA = TRUE, percent = TRUE,
                            digits = ifelse(percent, 0L, 2L),
                            add_total_col = TRUE, useNA.digits = 0L,
                            conf = 0.95, frac = TRUE, ...) {
  if (!include_NA) {
    na <- is.na(x) | is.na(y)
    x <- x[!na]
    y <- y[!na]
  }

  x <- as.factor(x)
  sp <- split(x, y)
  sp <- c(Total = list(x), sp)

  res <- lapply(sp, function(xx) {
    n <- table(xx)
    x <- sapply(n, function(r)
      binconr(r, sum(n), show_conf = FALSE, frac = frac, conf = conf,
              percent = percent, digits = digits))
    matrix(x, dimnames = list(names(n), NULL))
  })

  res <- do.call('cbind', res)
  colnames(res) <- names(sp)

  if (add_total_col)
    res else res[, -1L, drop = FALSE]
}

#' \code{tabler_stat} wrappers
#'
#' Helper functions for using \code{\link{tabler_stat}}.
#'
#' @param data a matrix or data frame with variables \code{varname} and
#' \code{byvar}
#' @param varname one or more variables in \code{data} to calculate
#' statistics by \code{byvar}; the rows variable(s) of the table
#' @param byvar a stratification variable; the column variable of the table
#' @param varname_label,byvar_label optional labels for each \code{varname}
#' and \code{byvar}
#'
#' note that duplicated \code{varname_label}s will be processed individually
#' then grouped; this is useful if one variable is shown as continuous and
#' categorical, for example; grouped variables should be consecutive, see
#' examples
#' @param digits \code{NULL} or a vector of digits past the decimal point to
#' keep for each \code{varname}; if \code{NULL}, these will be guessed using
#' \code{rawr:::guess_digits}; if length 1, all will be rounded to
#' \code{digits}
#'
#' alternatively, to set \code{digits} for a single \code{varname} manually,
#' a \emph{named} vector of \code{digits} corresponding to one or more
#' \code{varname} variables can be used; see examples
#' @param FUN \code{NULL} or a list of functions performing the test of
#' association between each \code{varname} and \code{byvar}; see
#' \code{\link{tabler_stat}}
#' @param confint optional vector of \code{varname}(s) to summarize as
#' confidence intervals
#' @param format_pval logical; if \code{TRUE}, p-values will be formatted
#' using \code{\link{pvalr}}; alternatively, a function may by used which
#' will be applied to each p-value
#' @param color_pval,color_missing,dagger \code{NULL} or vectors, recycled as
#' needed for each \code{varname}; see \code{\link{tabler_stat}}
#' @param correct logical or one of \code{\link{p.adjust.methods}}; if p-value
#' correction is desired, a column is added to the table with the corrected
#' p-values
#' @param group a \emph{named} vector of variables (either index of or a
#' value in \code{varname}) to begin a new group (see \code{tspanner} in
#' \code{\link[htmlTable]{htmlTable}})
#' @param color_cell_by,cell_color apply a color gradient (\code{cell_color})
#' to each cell (for html output); one of \code{"none"}, \code{"value"}, or
#' \code{"pct"}; see \code{\link{tabler_stat}}
#' @param statArgs a named list of additional arguments passed to
#' \code{\link[Gmisc]{getDescriptionStatsBy}}
#' @param align,rgroup,cgroup,tfoot optional arguments passed to
#' \code{\link[htmlTable]{htmlTable}}
#' @param tfoot2 optional footnote(s) appended to \code{tfoot}
#' @param htmlArgs a named list of additional arguments passed to
#' \code{\link[htmlTable]{htmlTable}}
#' @param zeros a character string used in place of zero cells (non-character
#' value keeps cells as-is)
#' @param clean_daggers logical or one of \code{"letters"}, \code{"numbers"};
#' values other than \code{FALSE} will replace default dagger characters
#'
#' @family tabler
#'
#' @seealso
#' \code{\link{tabler_stat}}; \code{rawr:::get_tabler_stat_n}
#'
#' \code{rawr:::guess_digits}; \code{rawr:::name_or_index}
#'
#' \code{rawr:::tabler_stat_list}; \code{rawr:::tabler_stat_html}
#'
#' @examples
#' sapply(mtcars[1:6], rawr:::guess_digits)
#' Map(rawr::roundr, mtcars[1:6], sapply(mtcars[1:6], rawr:::guess_digits))
#'
#' rawr:::get_tabler_stat_n(mtcars$gear)
#'
#'
#' ## typical usage
#' mt <- within(mtcars, {
#'   cyl  <- factor(cyl)
#'   mpg2 <- factor(+(mpg > 20), 0:1, c('&le; 20', '&gt; 20'))
#' })
#'
#' tabler_stat2(mt, c('mpg', 'cyl', 'wt'))
#' tabler_stat2(mt, c('mpg', 'cyl', 'wt'), 'vs')
#' tabler_stat2(mt, c('mpg', 'cyl', 'wt'), 'vs', group = 'wt')
#' tabler_stat2(mt, c('mpg', 'cyl', 'wt'), 'vs',
#'   group = c(group1 = 1, 'Weight' = 3))
#' tabler_stat2(mt, c('mpg', 'cyl', 'wt'), 'vs',
#'   FUN = list('kruskal', 'fisher', FALSE))
#'
#' tabler_stat2(
#'  mt, c('mpg', 'cyl', 'wt'), 'vs',
#'  cgroup = c('', 'V/S engine', ''),
#'  rgroup = c('Miles/gallon', 'No. cylinders', 'Weight (1000 lbs)')
#' )
#'
#' tabler_stat2(
#'   mt,
#'   varname = c(MPG = 'mpg', MPG = 'mpg2', Cylinders = 'cyl', Weight = 'wt'),
#'   byvar = c('V/S engine' = 'vs'),
#'   confint = c('mpg2', 'cyl'),
#'   zeros = NULL,
#'   digits = c(wt = 2)
#'   # digits = c('3' = 2) ## equivalently
#' )
#' 
#' \dontrun{
#' h1 <- tabler_stat2(mt, c('mpg', 'cyl', 'wt'), 'vs')
#' h2 <- tabler_stat2(mt, c('mpg', 'cyl', 'wt'), 'am')
#' h3 <- tabler_stat2(mt, c('mpg', 'cyl', 'wt'), 'gear')
#' 
#' rawr:::combine_tabler_stat2(list(h1, h2), how = 'rbind')
#' rawr:::combine_tabler_stat2(list(table1 = h1, table2 = h2), how = 'rbind')
#' 
#' rawr:::combine_tabler_stat2(list(h1, h2, h3), how = 'cbind')
#' rawr:::combine_tabler_stat2(list(t1 = h1, t2 = h2, t3 = h3), how = 'cbind')
#' }
#'
#' @export

tabler_stat2 <- function(data, varname, byvar = NULL,
                         varname_label = names(varname), byvar_label = names(byvar),
                         digits = NULL, FUN = NULL, confint = FALSE,
                         format_pval = TRUE, color_pval = TRUE, correct = FALSE,
                         color_missing = TRUE, dagger = TRUE,
                         group = NULL, color_cell_by = 'none',
                         cell_color = palette()[1:2], statArgs = NULL,
                         align = NULL, rgroup = NULL, cgroup = NULL,
                         tfoot = NULL, tfoot2 = NULL, htmlArgs = NULL,
                         zeros = '-', clean_daggers = FALSE) {
  data <- as.data.frame(data)
  nv   <- length(varname)

  varname_label <- varname_label %||% varname
  byvar_label   <- byvar_label %||% byvar

  if (!all(c(varname, byvar) %in% names(data))) {
    stop(
      sprintf('%s not found in data',
              toString(shQuote(setdiff(c(varname, byvar), names(data)))))
    )
  }

  stopifnot(
    length(byvar) %in% 0:1,
    byvar %in% names(data),
    nv == length(varname_label),
    is.null(FUN) ||
      !is.null(names(FUN)) ||
      is.null(names(FUN)) & length(FUN) == 1L ||
      nv == length(FUN)
  )

  if (is.character(group)) {
    ## need to shift group indices up for duplicated/combined varnames
    dup <- which(duplicated(varname_label))
    grp <- match(group, varname, nomatch = NA)
    grp <- sapply(grp, function(x) x - sum(dup < x))
    group <- na.omit(setNames(grp, names(group)))
  }

  l <- tabler_stat_list(
    data, varname, byvar, varname_label, byvar_label, digits, FUN,
    format_pval, color_pval, color_missing, dagger, color_cell_by,
    cell_color, confint, statArgs
  )

  tabler_stat_html(
    l, align, rgroup, cgroup, tfoot, tfoot2, htmlArgs,
    zeros, group, correct, format_pval, clean_daggers
  )
}

tabler_stat_list <- function(data, varname, byvar, varname_label = varname,
                             byvar_label = byvar, digits = NULL, FUN = NULL,
                             format_pval = TRUE, color_pval = TRUE,
                             color_missing = TRUE, dagger = TRUE,
                             color_cell_by = 'none', cell_color = NULL,
                             confint = NULL, statArgs = NULL) {
  if (is.null(byvar)) {
    FUN   <- NA
    byvar <- '_by_var_'
    data[, byvar] <- factor(1, 1:2)
  }

  nv <- length(varname)
  byvar <- byvar[1L]

  data <- data[, c(varname, byvar)]
  data[, byvar] <- as.factor(data[, byvar])
  .data <- data

  dig <- sapply(data[, -ncol(data), drop = FALSE], guess_digits)
  digits <- if (is.null(digits))
    dig
  else {
    if (length(digits) == 1L & is.null(names(digits)))
      rep_len(digits, nv)
    else replace(dig, name_or_index(names(digits), varname), digits)
  }

  fun <- setNames(vector('list', nv), varname)
  FUN <- if (is.null(FUN))
    fun
  else if (is.null(names(FUN)) & length(FUN) == nv)
    FUN
  else {
    if (length(FUN) == 1L & is.null(names(FUN)))
      rep_len(list(FUN), nv)
    else replace(fun, name_or_index(names(FUN), varname), FUN)
  }

  cell_color <- if (is.null(cell_color))
    list(palette()[1:2])
  else if (islist(cell_color))
    cell_color else list(cell_color)
  cell_color <- rep_len(cell_color, nv)

  color_cell_by <- rep_len(color_cell_by, nv)

  data <- rep_len(list(data), nv)
  pval <- any(!is.na(FUN))

  l <- do.call('Map', c(list(
    f = tabler_stat, data, varname, byvar, digits, FUN,
    list(format_pval), color_pval, color_missing, dagger,
    color_cell_by, cell_color, list(confint %||% '')), statArgs)
  )

  tbl <- lapply(l, function(x)
    x[!(duplicated(x, fromLast = TRUE) &
          duplicated(rownames(x), fromLast = TRUE)), , drop = FALSE]
  )
  names(l) <- names(tbl) <- varname_label

  res <- do.call('rbind', tbl)

  dup <- table(names(tbl))
  if (length(dup <- dup[dup > 1L])) {
    for (dd in names(dup)) {
      ii <- names(tbl) %in% dd
      tbl[[which(ii)[1L]]] <- do.call('rbind', tbl[ii])
      tbl <- tbl[-which(ii)[-1L]]
    }
  }

  rgroup   <- names(tbl)
  n.rgroup <- unname(sapply(rgroup, function(x) nrow(tbl[[x]]) %||% 1L))
  cgroup   <- c('', byvar_label, '')
  n.cgroup <- c(1L, nlevels(.data[, byvar]), 1L)

  if (!pval) {
    cgroup   <- head(cgroup, -1L)
    n.cgroup <- head(n.cgroup, -1L)
  }

  structure(
    class = 'htmlStat',
    list(
      output_data = res, rgroup = rgroup, n.rgroup = n.rgroup,
      cgroup = cgroup, n.cgroup = n.cgroup, pval = pval,
      data = .data, byvar = byvar, l = l
    )
  )
}

tabler_stat_html <- function(l, align = NULL, rgroup = NULL, cgroup = NULL,
                             tfoot = NULL, tfoot2 = NULL, htmlArgs = NULL, zeros = NULL,
                             group = NULL, correct = FALSE, format_pval = TRUE,
                             clean_daggers = FALSE) {
  stopifnot(
    inherits(l, 'htmlStat')
  )

  tr <- function(x) {
    gsub('\\s{2,}', ' ', x)
  }

  if (noby <- l$byvar == '_by_var_') {
    l$cgroup <- l$cgroup[1L]
    l$n.cgroup <- 1L
  }

  cn <- c(get_tabler_stat_n(l$data[, l$byvar]), '<em>p-value</em>')
  colnames(l$output_data) <- if (noby)
    cn[1L] else if (l$pval) cn else head(cn, -1L)

  res <- gsub('%', '', l$output_data, fixed = TRUE)
  p <- c('^\\s*0\\s*\\(\\s*0\\s*\\)\\s*$', '^\\s*0\\s*$',
         '^\\s*NA\\s*\\(\\s*NA\\s*-\\s*NA\\s*\\)\\s*$')
  if (is.character(zeros))
    res <- gsub(paste(p, collapse = '|'), zeros, res)

  ## text/daggers used in footnotes
  tt <- strsplit(sapply(l$l, attr, 'tfoot'), ', (?=<sup>)', perl = TRUE)
  dg <- lapply(l$l, function(x)
    gsub('<sup>([^<]+)</sup>|.', '\\1', x[1L, ncol(x)]))
  tf <- Map(function(x, y)
    if (nchar(x) == 0L)
      NULL else grep(x, y, value = TRUE), dg, tt)
  lf <- unique(unlist(tf))
  tf <- toString(lf)


  if (!is.null(tfoot2))
    tf <- sprintf('%s<br />%s', tf, paste0(tfoot2, collapse = '<br />'))

  ## tspanner
  if (!is.null(group)) {
    group <- group[group <= length(l$n.rgroup)]
    names(group) <- names(group) %||% as.character(group)
    group <- sort(group)
    nt <- insert(l$n.rgroup, group)
    nt <- cum_reset(nt, NA, function(x) sum(x, na.rm = TRUE))
    ts <- names(group)
    ts <- if (1 %ni% group)
      c('', ts)
    else if (0 %in% nt) {
      nt <- nt[nt > 0]
      ts
    } else ts

    htmlArgs <- modifyList(
      htmlArgs %||% list(),
      list(n.tspanner = nt, tspanner = ts)
    )
  }

  ## extract p-values and use correction method
  method <- if (isTRUE(correct))
    'fdr' else if (is.character(correct)) correct else 'none'
  pvn <- sapply(seq_along(l$l), function(ii)
    attr(l$l[[ii]], 'p.value') %||% NA)
  if (!identical(correct, FALSE)) {
    nc  <- ncol(res)
    wh  <- nzchar(res[, nc])
    pvc <- p.adjust(pvn, method, length(sort(pvn))) ## only non na pvalues
    res <- cbind(res, res[, nc])
    res[, nc + 1L][wh] <- color_pval(pvc, format_pval = format_pval)
    colnames(res)[nc + 1L] <-
      gsub('(?=p)', paste(method, ''), colnames(res)[nc], perl = TRUE)
    l$n.cgroup[length(l$n.cgroup)] <- l$n.cgroup[length(l$n.cgroup)] + 1L
  }

  if (!identical(clean_daggers, FALSE)) {
    old <- gsub('</sup>.*', '</sup>', lf)
    dag <- if (isTRUE(clean_daggers) | clean_daggers %in% 'letters')
      letters else seq_along(old)
    new <- sprintf('<sup>%s</sup>', dag[seq_along(old)])

    m <- gregexpr('<sup>.+</sup>', res)
    r <- lapply(regmatches(res, m), function(x)
      as.character(factor(x, old, new)))

    res[] <- `regmatches<-`(res, m, FALSE, r)

    ## update footnote
    tf <- unique(gsub('.*>', '', unlist(tt)))
    tf <- toString(paste0(new, tf))
  }

  args <- list(
    x = res, align = align %||% strrep('c', ncol(res)),
    rgroup = rgroup %||% l$rgroup, n.rgroup = l$n.rgroup,
    cgroup = cgroup %||% l$cgroup, n.cgroup = l$n.cgroup,
    css.cell = 'padding: 0px 5px 0px; white-space: nowrap;',
    tfoot = tr(tfoot %||% sprintf('<font size=1>%s</font>', tf))
  )
  args <- c(args, htmlArgs)
  ht <- do.call(htmlTable::htmlTable, args)

  structure(ht, class = 'htmlTable', p.value = pvn, call = args)
}

combine_tabler_stat2 <- function(l, correct = FALSE, format_pval = TRUE,
                                 how = c('rbind', 'cbind'), headers = names(l),
                                 htmlArgs = list()) {
  lget <- function(l, what, attr = FALSE) {
    lapply(l, function(x) if (attr)
      attr(x, what) else x[[what]])
  }
  tr <- function(x) {
    gsub('\\s{2,}', ' ', x)
  }
  
  p <- unlist(lget(l, 'p.value', TRUE))
  l <- lget(l, 'call', TRUE)
  
  how <- match.arg(how)
  res <- do.call(how, lget(l, 'x'))

  method <- if (isTRUE(correct))
    'fdr' else if (is.character(correct)) correct else 'none'
  if (!identical(correct, FALSE)) {
    nc  <- ncol(res)
    wh  <- nzchar(res[, nc])
    pvc <- p.adjust(p, method, length(sort(p))) ## only non na pvalues
    res <- cbind(res, res[, nc])
    res[, nc + 1L][wh] <- color_pval(pvc, format_pval = format_pval)
    colnames(res)[nc + 1L] <-
      gsub('(?=p)', paste(method, ''), colnames(res)[nc], perl = TRUE)
    l[[1L]]$n.cgroup[length(l[[1L]]$n.cgroup)] <-
      l[[1L]]$n.cgroup[length(l[[1L]]$n.cgroup)] + 1L
  }

  tf <- gsub('</?font[^>]*?>', '', unlist(lget(l, 'tfoot')))
  tf <- unique(unlist(strsplit(tf, ', ')))

  args <- list(
    x = res, align = l[[1L]]$align,
    rgroup = do.call('c', lget(l, 'rgroup')),
    n.rgroup = do.call('c', lget(l, 'n.rgroup')),
    cgroup = l[[1L]]$cgroup, n.cgroup = l[[1L]]$n.cgroup,
    css.cell = 'padding: 0px 5px 0px; white-space: nowrap;',
    tfoot = sprintf('<font size=1>%s</font>', toString(tf))
  )
  
  if (how == 'cbind') {
    args$align <- paste(lget(l, 'align'), collapse = '')
    args$rgroup <- lget(l, 'rgroup')[[1L]]
    args$n.rgroup <- lget(l, 'n.rgroup')[[1L]]
    args$cgroup <- do.call('c', lget(l, 'cgroup'))
    args$n.cgroup <- do.call('c', lget(l, 'n.cgroup'))
    
    if (!is.null(headers)) {
      ncg <- sapply(lget(l, 'x'), ncol)
      lcg <- headers
      length(ncg) <- length(lcg) <- length(args$cgroup)
      args$n.cgroup <- rbind(ncg, args$n.cgroup)
      args$cgroup <- rbind(lcg, args$cgroup)
    }
  }
  
  if (how == 'rbind' && !is.null(headers)) {
    args$tspanner <- headers
    args$n.tspanner <- sapply(lget(l, 'x'), nrow)
  }
  
  ht <- do.call(htmlTable::htmlTable, c(args, htmlArgs))

  structure(ht, class = 'htmlTable', p.value = p, call = args)
}

guess_digits <- function(x, default = 0L) {
  if (!inherits(x, 'numeric'))
    return(default)

  co <- capture.output(cat(x))
  co <- strsplit(co, '\\s+')[[1L]]

  nch <- max(nchar(sub('.*?(?:\\.|$)', '', co)))
  dig <- if (nch >= 4L)
    1L else nch

  if (dig)
    dig else default
}

get_tabler_stat_n <- function(x, pct = TRUE, use_labels = TRUE) {
  fmt <- if (pct)
    '%s<br /><font weight=normal; size=1>n = %s (%s)</font>' else
      '%s<br /><font weight=normal; size=1>n = %s</font>'
  x <- as.factor(x)
  l <- if (use_labels)
    levels(x) else rep_len('Total', nlevels(x))
  t <- table(x)
  n <- format(c(sum(t), t), big.mark = ',', scientific = FALSE)
  p <- roundr(prop.table(t) * 100, 0)
  o <- Vectorize('sprintf')(c('Total', l), n, c('%', p), fmt = fmt)

  drop(o)
}

#' Response table
#'
#' Convenience function to calculate proportions and confidence invervals and
#' format for easy display.
#'
#' @param x a factor variable of responses; responses should be ordered as
#' CR, PR, SD, PD, NE or similar (i.e., best to worst)
#' @param r_or_better if integer(s), indicates the levels of \code{x} that
#' are to be combined with better responses; for example, if
#' \code{r_or_better = 3} (default), then any occurrence of level 1, 2, or 3
#' of \code{x} is treated as a response, and the proportion and confidence
#' interval are calculated for the aggregate
#'
#' if \code{FALSE}; levels of \code{x} are estimated independently
#' @param conf,frac,show_conf,pct.sign additional arguments passed to
#' \code{\link{binconr}}
#' @param digits number of digits past the decimal point to keep
#' @param total logical or numeric; if \code{TRUE}, a column with the total,
#' i.e., \code{length(x)} is added; if numeric, \code{length(x)} and,
#' optionally, fraction and percent out of \code{total} is added
#' @param two_stage \code{FALSE} (default, assumes exact binomial CIs are
#' desired) or a vector of length 3 with the 1) maximum number responses in
#' the first stage that can be observed \emph{without} continuing; 2) the
#' number entered in the first stage; and 3) the additional number entered
#' in the second stage
#'
#' if more than three integers are given, the remaining should indicate the
#' columns which should be calculated as two-stage CIs; usually this is only
#' one value but multiple are accepted
#'
#' @family tabler
#'
#' @seealso
#' \code{\link{bincon}}; \code{\link{binconr}}
#'
#' @examples
#' set.seed(1)
#' r <- c('CR','PR','SD','PD','NE')
#' x <- factor(sample(r, 30, replace = TRUE), r)
#'
#' tabler_resp(x, 3)
#' tabler_resp(x, 'PR')
#' tabler_resp(x, 'PR', total = 50)
#'
#' ## note NAs are removed
#' y <- `[<-`(x, 1:10, value = NA)
#' tabler_resp(x, FALSE)
#' tabler_resp(y, FALSE)
#'
#'
#' ## two-stage designs
#' ## use two-stage CI in "PR" column
#' tabler_resp(x)
#' two_idx <- 1:2
#' two_stage <- c(r1 = 2, n1 = 10, n2 = 20)
#' tabler_resp(x, two_stage = c(two_stage, two_idx))
#'
#' ## compare
#' bincon(c(2, 4),  c(10, 20), method = 'two-stage') ## CRs
#' bincon(c(2, 11), c(10, 20), method = 'two-stage') ## PRs
#' ## one-stage methods should not be used
#' bincon(c(4, 11), 30, method = 'exact')
#'
#'
#' ## typical usage
#' ht <- htmlTable::htmlTable(
#'   rbind(
#'     tabler_resp(x),
#'     tabler_resp(x, conf = 0.9),
#'     tabler_resp(x, frac = FALSE, pct.sign = FALSE,
#'                 show_conf = FALSE, digits = 1),
#'     tabler_resp(x, two_stage = c(2, 10, 20, 1))
#'   ),
#'   caption = 'Table of responses with exact binomial and
#'     two-stage<sup>&dagger;</sup>confidence intervals.',
#'   css.cell = 'padding: 0 10 0px; white-space: nowrap;',
#'   cgroup = c('Evaluation', 'Outcome (95% CI)'),
#'   n.cgroup = c(nlevels(x), 3L)
#' )
#' structure(ht, class = 'htmlTable')
#'
#' @export

tabler_resp <- function(x, r_or_better = levels(x)[3:1], conf = 0.95,
                        digits = 0L, frac = TRUE, show_conf = TRUE,
                        pct.sign = TRUE, total = FALSE, two_stage = FALSE) {
  x  <- as.factor(x)
  rs <- names(table(x))
  lx <- length(x)

  if (is.character(r_or_better))
    r_or_better <- if (length(wh <- match(r_or_better, rs)))
      wh else {
        warning('Failed to guess \'r_or_better\'')
        ## take first 3 levels
        3:1
      }

  res <- c(
    resp1(x, rs, conf, digits, frac, show_conf, pct.sign, FALSE),
    if (is.numeric(r_or_better))
      rev(
        r_or_better1(x, rev(rs), conf, digits, frac, show_conf, pct.sign, FALSE)
      )[r_or_better]
    else NULL
  )

  if (!identical(two_stage, FALSE)) {
    two_idx   <- tail(two_stage, -3L)
    two_stage <- two_stage[1:3]

    if (two_stage[1L] > two_stage[2L] || two_stage[1L] > two_stage[3L])
      stop(
        'For two-stage designs, \'two_stage\' should be a vector of ',
        'length 3 giving:\n\t1) the max number of successes in the first ',
        'stage that can be observed _without_ continuing; ',
        '\n\t2) the number entered in the first stage; and ',
        '\n\t3) additional entered in the second stage'
      )

    res2 <- c(
      resp1(x, rs, conf, digits, frac, show_conf, pct.sign, two_stage),
      if (is.numeric(r_or_better))
        rev(
          r_or_better1(x, rev(rs), conf, digits, frac,
                       show_conf, pct.sign, two_stage)
        )[r_or_better]
      else NULL
    )

    res[two_idx] <- res2[two_idx]
  } else two_idx <- NULL

  tot <- if (is.numeric(total))
    sprintf('%s/%s (%s%%)', lx, total, roundr(lx / total * 100, digits))
  else lx

  tot <- c(Total = if (!pct.sign)
    gsub('%', '', tot, fixed = TRUE) else tot)

  if (!frac)
    tot <- gsub('/\\S+', '', tot)

  res <- c(if (total)
    tot else NULL, res)

  attr(res, 'two_stage') <- if (length(two_idx)) {
    res[two_idx] <- paste0(res[two_idx], '<sup>&dagger;</sup>')
    setNames(two_idx, names(res)[two_idx])
  } else NULL

  res
}

resp1 <- function(x, r, conf, digits, frac, show_conf, pct.sign, two) {
  # rawr:::resp1(x, levels(x),    .9, 0L, TRUE, TRUE, TRUE, FALSE)
  # rawr:::resp1(x, c('CR','PR'), .9, 0L, TRUE, TRUE, TRUE, FALSE)
  FUN <- if ('CR' %ni% r || which(r %in% 'CR') == 1L)
    identity else rev
  tbl <- table(x)[FUN(r)]

  res <- if (all(is.na(x)))
    rep('-', length(r))
  else sapply(tbl, function(X)
    if (identical(two, FALSE))
      binconr(X, sum(tbl), conf, digits, TRUE, frac,
              show_conf, pct.sign, 'exact')
    else binconr(c(two[1L], X), two[2:3], conf, digits, TRUE, frac,
                 show_conf, pct.sign, 'two-stage')
  )

  setNames(res, FUN(r))
}

r_or_better1 <- function(x, r, conf, digits, frac, show_conf, pct.sign, two) {
  # rawr:::r_or_better1(x, unique(x), .9, 0L, TRUE, TRUE, TRUE, FALSE)
  x[x %ni% r] <- NA
  x <- na.omit(x)

  res <- if (all(is.na(x)))
    rep('-', length(r))
  else
    sapply(seq_along(r), function(X)
      if (identical(two, FALSE))
        binconr(sum(x %in% r[X:length(r)]), length(x), conf,
                digits, TRUE, frac, show_conf, pct.sign, 'exact')
      else binconr(c(two[1L], sum(x %in% r[X:length(r)])), two[2:3], conf,
                   digits, TRUE, frac, show_conf, pct.sign, 'two-stage')
    )

  setNames(res, paste(r, 'or better'))
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
#' A data frame of matches with toxicity codes, descriptions, and categories
#' corresponding to the CTCAE version used.
#'
#' The version is stored as an attribute, \code{attr(., "version")}.
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
    stop('\'version\' should be 3 or 4')
  else if (version == 3L)
    rawr::ctcae_v3
  else rawr::ctcae_v4

  ## guess if input is code or description
  idx <- if (any(grepl('([A-Za-z -])([0-9])', x)))
    match(gsub('\\s*|-', '', x, perl = TRUE), ctc[, 'tox_code'])
  else grep(paste(x, collapse = '|'), ctc[, 'tox_desc'], ignore.case = TRUE)

  if (anyNA(idx[nzchar(as.character(x)) & !is.na(x)]))
    warning(
      'CTCAE version may be incorrect - ',
      'try version = ', ifelse(version == 4, 3, 4),
      call. = FALSE
    )

  structure(
    `rownames<-`(ctc[idx, ], NULL),
    version = sprintf('CTCAE v%s', version)
  )
}

#' Find highest grade toxicities
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
#' note that \code{version} should be either \code{3} or \code{4} (default)
#' corresponding to \code{\link{ctcae_v3}} or \code{\link{ctcae_v4}},
#' respectively
#'
#' @return
#' A filtered data frame with attributes:
#'
#' \item{\code{attr(., "data")}}{the input data frame sorted by \code{id},
#' \code{desc}, and \code{grade}}
#' \item{\code{attr(., "duplicates"}}{the indices of rows removed from
#' \code{attr(., "data")} which correspond to duplicate \code{desc} per
#' \code{id} with equal or lesser \code{grade}s}
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
#' ## get worst tox by CTCAE code
#' ## this will convert the code to description strings
#' tox1 <- tox_worst(tox, id = 'id', grade = 'grade', code = 'code')
#'
#' ## or by formatted descriptions and grade
#' tox$desc <- factor(match_ctc(tox$code)$tox_desc)
#' tox2 <- tox_worst(tox, id = 'id', grade = 'grade', desc = 'desc')
#'
#' ## both methods are equivalent
#' stopifnot(identical(tox1, tox2))
#'
#'
#' ## these rows have been removed from attr(tox1, 'data')
#' attr(tox1, 'duplicates')
#'
#' stopifnot(
#'   all.equal(tox1, attr(tox1, 'data')[-attr(tox1, 'duplicates'), ],
#'             check.attributes = FALSE)
#' )
#'
#'
#' ## use tabler_by/tabler_by2 to summarize
#' tabler_by(tox1, 'desc', 'grade', n = 10, pct.sign = FALSE)
#' tabler_by2(tox1, 'desc', 'grade', stratvar = 'phase')
#'
#' @export

tox_worst <- function(data, id = 'id', desc = 'desc', grade = 'grade',
                      code, version = 4L) {
  if (!is.factor(data[, grade]))
    stop('\'grade\' should be a factor with proper order')

  if (!missing(code)) {
    ctc <- match_ctc(data[, code], version = version)
    data$desc <- factor(ctc$tox_desc)
    desc <- 'desc'
  }

  data <- data[order(data[, id], data[, desc], -xtfrm(data[, grade])), ]
  idx  <- which(duplicated(data[, c(id, desc)]))

  structure(
    if (length(idx))
      data[-idx, ] else data,
    data = data, duplicates = idx
  )
}

#' Count formatter
#'
#' Formats and prints a \emph{named} vector of counts with percents.
#'
#' @param x named vector of counts (a summary or table) or a long vector
#' of character strings or factors
#' @param n total number of observations; if not given, the length of
#' \code{x} is used
#' @param lowcase logical; names will be lowercase if \code{TRUE}, upper
#' case if \code{FALSE}, and unchanged for any other value
#' @param frac logical; if \code{TRUE}, counts are shown as fractions of
#' \code{n}
#' @param digits number of digits past the decimal point to keep
#' @param which optional integer or character vector to select or re-order
#' the output; note that this does not change the counts or percentages
#'
#' @examples
#' x <- setNames(3:1, c('Gold', 'Silver', 'Bronze'))
#' countr(x)
#' countr(x, n = 10, frac = TRUE)
#' countr(x, n = 10, frac = TRUE, which = 2)
#'
#' countr(names(x))
#' countr(names(x), which = 1)
#' countr(names(x), which = c(3, 1))
#' countr(names(x), which = 'Silver')
#'
#' countr(names(x), lowcase = TRUE)
#' countr(names(x), frac = TRUE)
#'
#' @export

countr <- function(x, n, lowcase = NA, frac = FALSE, digits = 0L,
                   which = seq_along(x)) {
  if (inherits(x, 'table') || (!is.null(names(x)) & is.numeric(x))) {
    ## if x is a table or a _named_ vector (of counts)
    n <- if (missing(n))
      sum(x) else n
    x <- as.table(x)
  } else {
    n <- if (missing(n))
      length(x) else n
    x <- table(x)
  }

  if (is.na(lowcase) || !is.logical(lowcase))
    lowcase <- NULL

  x <- x[which]

  iprint(
    sprintf(
      '%s (n = %s%s, %s%%)',
      if (isTRUE(lowcase))
        tolower(names(x))
      else if (identical(lowcase, FALSE))
        toupper(names(x)) else names(x),
      x,
      if (frac)
        paste0('/', n) else '',
      roundr(as.numeric(x) / n * 100, digits)
    )
  )
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

  d[is.na(d)] <- origin[1L]
  m[is.na(m)] <- origin[2L]
  y[is.na(y)] <- origin[3L]

  y <- ifelse(nchar(y) <= 2, f(y, 0) + origin[3L], f(y, 0))

  as.Date(sprintf('%04s-%02s-%02s', y, f(m, origin[2L]), f(d, origin[1L])))
}

#' Combine html tables
#'
#' Wrapper to easily combine a list of data frames or matrices into html
#' tables using the \pkg{htmlTable} package. \code{combine_table2} can join
#' tables vertically or horizontally (common column and row names are not
#' required).
#'
#' @param l a list of matrices or data frames
#' @param tspanner,n.tspanner table spanner labels and number of rows,
#' respectively, passed to \code{\link[htmlTable]{htmlTable}}; if missing,
#' \code{names(l)} and \code{sapply(l, nrow)} are used
#' @param cgroup,n.cgroup table column labels and number of columns for each,
#' respectively, passed to \code{\link[htmlTable]{htmlTable}}; if missing,
#' \code{names(l)} and \code{sapply(l, ncol)} are used
#' @param how method to join objects, by row (\code{"rbind"}) or column
#' (\code{"cbind"}) binding
#' @param ... additional arguments passed to \code{\link[htmlTable]{htmlTable}}
#'
#' @examples
#' sp <- lapply(split(mtcars, rep(1:3, c(1, 11, 20))), as.matrix)
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
#' combine_table2(sp, how = 'c', cgroup = LETTERS[1:3])
#'
#' @export

combine_table <- function(l, tspanner, n.tspanner, ...) {
  l <- if (!islist(l))
    list(l) else l

  n.tspanner <- if (missing(n.tspanner))
    sapply(l, function(x) nrow(x) %||% 1L) else n.tspanner
  tspanner   <- if (missing(tspanner))
    names(l) %||% rep(' ', each = length(n.tspanner)) else tspanner

  ht <- htmlTable::htmlTable(
    do.call('rbind', l), tspanner = tspanner, n.tspanner = n.tspanner, ...
  )

  structure(ht, class = 'htmlTable')
}

#' @rdname combine_table
#' @export
combine_table2 <- function(l, tspanner, n.tspanner, cgroup, n.cgroup,
                           how = c('rbind', 'cbind'), ...) {
  l <- if (!islist(l))
    list(l) else l
  how <- switch(match.arg(how), rbind = 'rbindx', cbind = 'cbindx')

  if (how %in% c('rbind', 'rbindx')) {
    n.tspanner <- if (missing(n.tspanner))
      sapply(l, function(x) nrow(x) %||% 1L) else n.tspanner
    tspanner   <- if (missing(tspanner))
      names(l) %||% rep(' ', each = length(n.tspanner)) else tspanner
    if (missing(cgroup))
      cgroup <- NULL
    if (missing(n.cgroup))
      n.cgroup <- NULL
  } else {
    n.cgroup <- if (missing(n.cgroup))
      sapply(l, function(x) ncol(x) %||% 1L) else n.cgroup
    cgroup   <- if (missing(cgroup))
      names(l) %||% rep(' ', each = length(n.cgroup)) else cgroup
    if (missing(tspanner))
      tspanner <- NULL
    if (missing(n.tspanner))
      n.tspanner <- NULL
  }

  ht <- htmlTable::htmlTable(
    do.call(how, l), ...,
    tspanner = tspanner, n.tspanner = n.tspanner,
    cgroup = cgroup, n.cgroup = n.cgroup
  )

  structure(ht, class = 'htmlTable')
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
#' ht <- htmlTable::htmlTable(
#'   inject_div(head(cars), c(2,2), style = 'border: dashed 1px;')
#' )
#' structure(ht, class = 'htmlTable')
#'
#' ## if where is missing, style is recycled over all cells
#' ht <- htmlTable::htmlTable(
#'   inject_div(head(mtcars),
#'              style = c('color: red;', 'color: blue', 'border: dashed 1px;')
#'   )
#' )
#' structure(ht, class = 'htmlTable')
#'
#' ht <- htmlTable::htmlTable(
#'   inject_div(head(cars),
#'              rbind(c(2,2), c(2,1), c(5,2)),
#'              'background-color: yellow;')
#' )
#' structure(ht, class = 'htmlTable')
#'
#' ht <- htmlTable::htmlTable(
#'   inject_div(head(cars),
#'              c(2,2,2,1,5,2),
#'              c('background-color: red; color: white;',
#'                'border: solid 1px;',
#'                'font-weight: 900; color: blue;')
#'   )
#' )
#' structure(ht, class = 'htmlTable')
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
    which(row(x) > 0L, arr.ind = TRUE)
  else matrix(where, ncol = 2L, byrow = !is.matrix(where))

  what <- rep_len(what, nrow(where))
  mat  <- matrix('', nrow(x), ncol(x))

  mat[where] <- what
  mat
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
      substring(y, seq, seq) <- toupper(substring(y, seq, seq))
      paste(y, collapse = '')
    })
    unlist(x)
  }

  case <- match.arg(case)

  if (translate) {
    x <- if (case %in% c('upper', 'uplow'))
      toupper(x) else tolower(x)
  }

  case <- switch(
    case,
    first     = '(^.)',
    upcase    = '(\\b.)',
    downcase  = '(?<=[a-z])(.)',
    camelcase = 'camelcase',
    upper     = {x <- toupper(x); TRUE},
    lower     = {x <- tolower(x); TRUE},
    lowup     = {x <- alternating(x, 0:1); TRUE},
    uplow     = {x <- alternating(x, 1:0); TRUE}
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
#' x <- htmlTable::htmlTable(head(cars))
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

    x <- structure(x, class = 'htmlTable', html = TRUE)
  }

  if (!is.character(file))
    x else cat(x, file = file)
}

#' Align html
#'
#' Align text in columns of an \code{\link[htmlTable]{htmlTable}} at a
#' specific location (similar to \code{align} or \code{eqnarray} environments
#' in latex).
#'
#' @param x an object of class \code{\link[htmlTable]{htmlTable}}
#' @param sep a character string used as the center of alignment
#' @param where a character string or regular expression (see examples)
#' defining where strings should be aligned; the easiest method is to
#' use \code{"&&"} at the desired alignment point
#' @param min_width minimum width of the span tag; too narrow will not
#' align strings but too wide adds whitespace
#'
#' @examples
#' tmp <- within(cars, {
#'   align2 <- sprintf('%s&&(%s)', speed, dist)
#'   align1 <- sprintf('%s (%s)', speed, dist)
#'   raw    <- sprintf('%s - (%s)', speed, dist)
#' })
#'
#' ht <- htmlTable::htmlTable(
#'   rawr::ht(tmp), n.cgroup = 2:3, cgroup = c('raw', 'align'),
#'   caption = 'caption', rnames = FALSE
#' )
#'
#' ## default
#' structure(ht, class = 'htmlTable')
#'
#' ## align at '&&'
#' structure(html_align(ht), class = 'htmlTable')
#' structure(html_align(ht, ' --- '), class = 'htmlTable')
#'
#' ## align at '&&' or ' '
#' ## the regex should capture the left text in group 1, use non-capture
#' ## for separating text, andn capture the right text in group 2
#' structure(
#'   html_align(ht, '&nbsp;', '(\\d+)(?: |&&)([()0-9]+)'),
#'   class = 'htmlTable'
#' )
#'
#' @export

html_align <- function(x, sep = '&nbsp;', where = '&&', min_width = '35px') {
  stopifnot(
    inherits(x, 'htmlTable')
  )
  ok <- identical(where, '&&')

  css <- sprintf(
    '
    .charalign {
      text-align: center;
      /* font-size: 5pt; */
    }
    .charalign span {
      /* font-size: 11pt; */
      min-width: %s;
      display: inline-block;
      text-align: left;
    }
    .charalign span:first-child {
      text-align: right;
    }', min_width)

  co <- capture.output(print(x, useViewer = FALSE))
  at <- grepl(where, co, perl = TRUE)

  pat  <- if (ok)
    sprintf('(?<=>)(.*?)%s(.*?)(?=<)', where)
  else where
  repl <- sprintf(
    '<div class="charalign"><span>\\1</span>%s<span>\\2</span></div>',
    sep
  )
  co[at] <- gsub(pat, repl, co[at], perl = TRUE)


  res <- paste0(co, collapse = '\n')
  res <- paste0(res, '\n<style>', css, '\n</style>', collapse = '\n')
  attributes(res) <- attributes(x)

  res
}
