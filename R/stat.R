### statistical functions
# bincon, bintest, dlt_table, pr_table, moods.test, cuzick.test,
# cuzick.test.default, cuzick.test.formula, jt.test, kw.test, kw.test.default,
# kw.test.formula, ca.test, ca.test.default, ca.test.formula, lspline,
# perm.t.test, perm.t.test.default, perm.t.test.formula, vs.rfsrc, vs.glmnet,
# ransch, ranschtbl, twostg.test, cor.n, cor.ci, bni, pickwin
#
# S3 methods:
# cuzick.test, kw.test, ca.test, perm.t.test
#
# rpart_utils:
# rpart_parent, rpart_subset, rpart_nodes
#
# unexported:
# cuzick.test.stat, cuzick.test.pvalue, jt.test.stat, sim.test.pvalue,
# formula0, ransch_
###


#' Binomial probability confidence intervals
#'
#' @description
#' Calculates confidence intervals for binomial probabilities for specified
#' type-I error (\code{alpha}) using exact, Wilson, asymptotic methods, or
#' two-stage methods.
#'
#' For exact, Wilson, and asymptotic methods, \code{r} and \code{n} can be
#' any length (the other argument is recycled as needed).
#'
#' For the two-stage method, \code{r} and \code{n} must each be length 2
#' where \code{r = c(max # responses in 1st stage that can be observed
#' without continuing, total number of responses observed)} and
#' \code{n = c(# of cases entered in 1st stage, # of additional cases
#' entered in 2nd stage)}.
#'
#' @details
#' If \code{method = 'all'}, \code{r} and \code{n} should each be length 1.
#' The "exact" method uses the \code{\link{df}} distribution to comupte exact
#' intervals (based on the binomial cdf); the "wilson" interval is score-test-
#' based; the "asymptotic" is the asymptotic normal interval.
#'
#' The "wilson" method has been preferred by Agresti and Coull.
#'
#' \code{method = "two-stage"} uses the \code{\link[desmon]{twocon}} function
#' from the \pkg{desmon} library:
#'
#' First \code{n[1]} patients are entered on the study. If more than
#' \code{r[1]} responses are observed, then an additional \code{n[2]} patients
#' are entered. This function assumes that if the observed number of responses
#' \code{r < r[1]}, then only \code{n[1]} patients were entered.
#'
#' The estimators computed are the MLE (the observed proportion of responses),
#' a bias corrected MLE, and an unbiased estimator, which is sometimes
#' incorrectly described as the UMVUE.
#'
#' The confidence interval is based on the exact sampling distribution.
#' However, there is not a universally accepted ordering on the sample space
#' in two-stage designs. The parameter \code{dp} can be used to modify the
#' ordering by weighting points within the sample space differently.
#'
#' \code{dp=0} will give the Atkinson and Brown procedure, and \code{dp=1}
#' will order outcomes base on the MLE. The Atkinson and Brown procedure
#' orders outcomes based solely on the number of responses, regardless of the
#' number cases sampled.
#'
#' The MLE ordering defines as more extreme those outcomes with a more extreme
#' value of the MLE (the proportion of responses). Other powers of \code{dp},
#' such as \code{dp=1/2}, could also be used.
#'
#' Let \code{R} be the number of responses and \code{N=n[1]} if
#' \code{R <= r[1]} and \code{N=n[1] + n[2]} if \code{R>r[1]}. In general, the
#' outcomes that are more extreme in the high response direction are those
#' with \code{R/(N^dp) >= r/(n^dp)}, where \code{r} and \code{n} are the
#' observed values of \code{R} and \code{N}, and the outcomes that are more
#' extreme in the low response direction are those with
#' \code{R/(N^dp) <= r/(n^dp)}.
#'
#' @param r number of responses (successes); if \code{method = 'two-stage'},
#'   a vector of length two giving the max number of responses that can be
#'   observed in the first stage without continuing and the total number of
#'   responses observed
#' @param n number of observations (trials); if \code{method = 'two-stage'},
#'   a vector of length two giving the number of cases entered during the first
#'   stage and the number of additional cases to be entered during the second
#'   stage
#' @param alpha type-I error probability
#' @param digits integer value specifying number of decimal places
#' @param method character strings specifying which method to use; can be
#'   (unambiguously) abbreviated; see details
#' @param dp numeric value affecting the ordering of sample space in two-stage
#'   designs when \code{method = "two-stage"}
#'
#'   \code{dp = 0} will give the Atkinson and Brown procedure, and
#'   \code{dp = 1} (default) will order based on MLE; values such as
#'   \code{dp = 0.5} can also be used; see details
#' @param max_width logical; if \code{TRUE}, the maximum width of each
#'   confidence interval is given for \code{n}
#'
#' @return
#' A matrix containing the computed interval(s) and their widths.
#'
#' @author
#' Rollin Brant, Frank Harrell, and Brad Biggerstaff; modifications by Robert
#' Redd including support for two-stage designs
#'
#' @references
#' Agresti, A. and B.A. Coull. Approximate is better than "exact" for interval
#' estimation of binomial proportions. \emph{American Statistician}.
#' \strong{59}:119-126, 1998.
#'
#' Brown, L.D., T.T. Cai, and A. Das Gupta. Inverval estimation for a binomial
#' proportion (with discussion). \emph{Statistical Science}.
#' \strong{16}:101-133, 2001.
#'
#' Newcombe, R.G. Logit confidence intervals and the inverse sinh
#' transformation, \emph{American Statistician}. \strong{55}:200-2, 2001.
#'
#' Atkinson E.N. and B.W. Brown. \emph{Biometrics}. \strong{41(3)}: 741-4,
#' 1985.
#'
#' @seealso
#' \code{\link{binconr}}; \code{\link[Hmisc]{binconf}};
#' \code{\link[desmon]{binci}}; \code{\link[desmon]{twocon}}
#'
#' @examples
#' bincon(0, 10)
#' bincon(0:10, 10)
#' bincon(5, 10, method = 'all')
#'
#'
#' ## two-stage confidence intervals
#' bincon(c(3, 4), c(14, 18), method = 'two-stage', dp = 0)
#' bincon(c(3, 4), c(14, 18), method = 'two-stage', dp = 1)
#'
#' ## ?desmon::twocon
#' ## equivalent to
#' rawr:::twocon(14, 18, 3, 4, dp = 0)
#' rawr:::twocon(14, 18, 3, 4, dp = 1)
#'
#' @export

bincon <- function(r, n, alpha = 0.05, digits = getOption('digits'),
                   method = c('exact', 'wilson', 'asymptotic', 'all', 'two-stage'),
                   dp = 1, max_width = TRUE) {
  if (alpha >= 1 | alpha <= 0)
    stop('\'alpha\' must be between 0 and 1')

  method <- match.arg(method)
  lr <- length(r)
  ln <- length(n)

  if (method == 'two-stage') {
    if (any(r < 0) | r[1L] > n[1L] | r[2L] > sum(n))
      stop('Invalid response value')
    if (lr != 2L | ln != 2L)
      stop('\'r\' and \'n\' should be length 2')

    mat <- twocon(n[1L], n[2L], r[1L], r[2L], 1 - alpha, dp)
    mat <- cbind(
      Responses = r[2L], Trials = sum(n), PointEst = r[2L] / sum(n),
      Lower = mat['lower'], Upper = mat['upper'],
      Width = diff(mat[c('lower', 'upper')])
    )
    rownames(mat) <- NULL
    return(mat)
  }

  if (any(r < 0) | any(r > n))
    stop('Invalid response value')

  bc <- function(r, n, alpha, method) {
    nu1 <- 2 * (n - r + 1)
    nu2 <- 2 * r
    ll <- if (r > 0)
      r / (r + qf(1 - alpha / 2, nu1, nu2) * (n - r + 1)) else 0
    nu1p <- nu2 + 2
    nu2p <- nu1 - 2
    pp <- if (r < n)
      qf(1 - alpha / 2, nu1p, nu2p) else 1
    ul <- ((r + 1) * pp) / (n - r + (r + 1) * pp)
    zcrit <- -qnorm(alpha / 2)
    z2 <- zcrit * zcrit
    p <- r / n
    cl <- (p + z2 / 2 / n + c(-1, 1) * zcrit *
             sqrt((p * (1 - p) + z2 / 4 / n) / n)) / (1 + z2 / n)
    if (r == 1)
      cl[1L] <- -log(1 - alpha) / n
    if (r == (n - 1))
      cl[2L] <- 1 + log(1 - alpha) / n
    asymp.lcl <- r / n - qnorm(1 - alpha / 2) *
      sqrt(((r / n) * (1 - r / n)) / n)
    asymp.ucl <- r / n + qnorm(1 - alpha / 2) *
      sqrt(((r / n) * (1 - r / n)) / n)
    res <- rbind(c(ll, ul), cl, c(asymp.lcl, asymp.ucl))
    res <- cbind(rep(r / n, 3L), res)
    res <- cbind(res, res[, 3L] - res[, 2L])
    dimnames(res) <- NULL

    switch(
      method,
      exact      = res[1L, ],
      wilson     = res[2L, ],
      asymptotic = res[3L, ],
      all        = res
    )
  }

  if ((lr != ln) & lr == 1L)
    r <- rep_len(r, ln)
  if ((lr != ln) & ln == 1L)
    n <- rep_len(n, lr)
  if ((lr > 1L | ln > 1L) & method == 'all') {
    method <- 'exact'
    warning('Multiple confidence intervals should use only one method, ',
            'defaulting to \'exact\' method', domain = NA)
  }

  maxwid <- function(n, alpha, method) {
    max(sapply(seq.int(n), function(nn)
      bc(nn, n[1L], alpha[1L], method[1L])[4L]))
  }

  rn <- c('exact', 'wilson', 'asymptotic')
  cn <- c('PointEst', 'Lower', 'Upper', 'Width')

  if (method == 'all' & lr == 1L & ln == 1L) {
    mat <- bc(r, n, alpha, method)
    dimnames(mat) <- list(rn, cn)

    if (max_width)
      mat <- cbind(mat, MaxWidth = sapply(rn, function(x) maxwid(n, alpha, x)))
    mat[, -1L] <- round(mat[, -1L], digits = digits)

    return(cbind(Responses = r, Trials = n, mat))
  }

  mat <- matrix(ncol = 3L + max_width, nrow = lr)
  for (i in seq.int(lr))
    mat[i, ] <- bc(r[i], n[i], alpha = alpha, method = method)
  colnames(mat) <- cn
  if (max_width)
    mat <- cbind(mat, MaxWidth = sapply(n, function(x) maxwid(x, alpha, method)))
  mat[, -1L] <- round(mat[, -1L], digits = digits)

  cbind(Responses = r, Trials = n, mat)
}

#' Single-stage designs
#'
#' @description
#' Function for sample sizes for single-stage designs in early clinical trial
#' phases (generally phases I and II) based on exact binomial method.
#'
#' Cycles through possible designs constrained by \code{alpha}, \code{beta},
#' and \code{n.max} arguments for specified ranges of \code{p0} and \code{p1}.
#'
#' @param p0low,p0high low and high values for p0
#' @param p1low,p1high low and high values for p1
#' @param n.max maximum sample size allowed (or feasible)
#' @param r cut-off value for responses expected; usually best to leave
#'   \code{r = n.max} (default); useful if the maximum number of responses
#'   that may occur is known
#' @param alpha,beta type-I and type-II errors
#'
#' @return
#' A list with the following elements:
#'
#' \item{\code{$designs}}{a matrix with a row giving a summary of each design
#' which meets the criteria. The columns are: \code{p0} and \code{p1}, the
#' null and alternative hypotheses, respectively; \code{n} and \code{r2}, the
#' overall sample size and minimum number of responses required to reject
#' \code{p0}; \code{type1} and \code{power}, the alpha and \code{1 - beta} for
#' the designs; and \code{signal}, the magnitude of difference between
#' \code{p0} and \code{p1}}
#' \item{\code{$call}}{the call to \code{bintest}}
#' \item{\code{$description}}{help text for \code{$designs}}
#'
#' @references Khan, Sarker, Hackshaw. Smaller sample sizes for phase II trials
#'   based on exact tests with actual error rates by trading-off their nominal
#'   levels of significance and power. \emph{British J of Cancer} (2012) 107,
#'   1801-9.
#'
#' @seealso
#' \code{\link[clinfun]{ph2single}}; \code{\link[desmon]{bin1samp}}; bintest,
#' \href{https://github.com/raredd/sascros/blob/master/bintest.sas}{SAS macro}
#'
#' @examples
#' bintest(0.2, 0.5, n.max = 25)
#' ## compare
#' rawr:::bin1samp(0.2, 0.5)
#'
#' ## example in sas macro
#' bintest(0.1, 0.2, 0.15, 0.2, n.max = 80, alpha = 0.08, beta = 0.24)
#'
#' @export

bintest <- function(p0low, p1low, p0high = p0low, p1high = p1low, n.max,
                    r = n.max, alpha = 0.1, beta = 0.1) {
  stopifnot(
    alpha %inside% c(0, 1),
    beta %inside% c(0, 1),
    r > 0
  )

  mat <- expand.grid(
    seq(p0low, p0high, by = 0.01),
    seq(p1low, p1high, 0.01),
    seq.int(n.max),
    seq.int(r)
  )

  ## require > r responders out of n and remove where p1 <= p0 or r >= n
  mat <- as.matrix(within(mat, r2 <- mat[, 4L] + 1L))
  colnames(mat) <- c('p0', 'p1', 'n', 'r', 'r2')
  mat <- mat[!(mat[, 2L] <= mat[, 1L] | mat[, 4L] >= mat[, 3L]), ]

  ## Pr(<r2 responders|p0 true): pbinom(r2, n, p0)
  ## Pr(>r2 responders for p0): type-I error = 1 - y
  y <- pbinom(mat[, 5L], mat[, 3L], mat[, 1L])

  ## Pr(<r2 responders | p1 true): pbinom(r2, n, p1): type-II error
  ## Pr(>r2 responders for p1): 1 - type-II error
  z <- pbinom(mat[, 5L], mat[, 3L], mat[, 2L])

  ## delta: p1 - p0
  delta <- mat[, 2L] - mat[, 1L]

  mat <- cbind(mat, y, type1 = 1 - y, z, power = 1 - z, delta)
  alpha <- rep(alpha, nrow(mat))
  beta  <- rep(beta, nrow(mat))

  ## keep ones that fit type-I/II error constraints
  mat <- mat[mat[, 9L] > (1 - beta) & mat[, 7L] < alpha, , drop = FALSE]
  mat <- unique(as.data.frame(mat[, c(1:3, 5L, 7L, 9:10), drop = FALSE]))
  mat <- as.matrix(mat[order(mat$n), ])
  rownames(mat) <- NULL

  list(
    designs = mat,
    call = match.call(),
    description = c(
      'n = overall sample size',
      'r2 = minimum number of responders required to reject p0',
      'delta = difference in null and alternative hypotheses')
  )
}

#' Probability tables
#'
#' Functions to generate standard 3+3 or generalized probability tables.
#'
#' \code{dlt_table} creates a standard 3+3 dose-limiting toxicity table with
#' probabilities of dose-escalation for true but unknown probabilities of
#' experiencing toxicity.
#'
#' \code{pr_table} is a general single-stage (as opposed to \code{dlt_table})
#' function for calculating the probabilities of \code{crit} or fewer (or
#' greater if \code{greater = TRUE}) events for true but unknown probabilities
#' of the event occurring.
#'
#' @param prob a vector of probabilities
#' @param digits number of digits past the decimal point to keep; if
#'   \code{NULL}, then no rounding is done
#' @param n,crit sample size and critical number of events, respectively; note
#'   that the probabilities will always be calculated as weakly less or greater
#'   than \code{crit}, i.e., \code{Pr(>= crit)} or \code{Pr(<= crit)} depending
#'   if \code{greater} is \code{TRUE} or \code{FALSE}
#' @param greater logical; the direction of \code{crit}: if \code{FALSE},
#'   the probabilities are calculated for \code{crit} \emph{or fewer} events;
#'   if \code{TRUE}, the probabilities are calculated for \code{crit}
#'   \emph{or greater} events
#'
#' @examples
#' prob <- c(1, 5, 1:5 * 10) / 100
#'
#' ## standard 3+3 escalation design for true but unknown
#' ## probabilities of experiencing event
#' dlt_table(prob)
#' t(dlt_table(prob, 3))
#'
#'
#' ## probabilities of crit or fewer events given true but unknown prob
#' pr_table(prob, n = 15, crit = 3, greater = FALSE)
#'
#'
#' ## probabilities of crit or more events given true but unknown prob
#' pr_table(prob, n = 15, crit = 3, greater = TRUE)
#'
#' ## compare
#' 1 - pr_table(prob, n = 15, crit = 2, greater = FALSE)[2L, ]
#'
#' @name pr_tables
NULL

#' @rdname pr_tables
#' @export
dlt_table <- function(prob, digits = NULL) {
  res <- sapply(prob, function(pr) {
    dbinom(0L, 3L, pr) + dbinom(0L, 3L, pr) * dbinom(1L, 3L, pr)
  })

  res <- rbind(
    'Pr(DLT)' = prob,
    'Pr(Escalation)' = res
  )

  if (is.null(digits))
    res else roundr(res, digits)
}

#' @rdname pr_tables
#' @export
pr_table <- function(prob, n, crit, greater = FALSE, digits = NULL) {
  n <- as.integer(n)
  crit <- c(0L, sequence(max(as.integer(crit - greater))))

  res <- sapply(prob, function(pr) {
    # x <- sum(mapply(function(x) dbinom(x, n, pr), crit))
    x <- sum(dbinom(crit, n, pr))
    if (greater)
      1 - x else x
  })

  nn  <- c(
    'Pr(Event)',
    sprintf('Pr(%s%s)', c('<=', '>=')[greater + 1L], max(crit) + greater)
  )
  res <- structure(
    rbind(prob, res),
    dimnames = list(nn, NULL)
  )

  if (is.null(digits))
    res else roundr(res, digits)
}

#' Mood's median test
#'
#' A nonparametric test used to test the equality of medians from two or more
#' populations.
#'
#' Mood's median test is an exact test which dichotomizes values in each group
#' above or below the median of the pooled data. The test is conservative in
#' small sample sizes.
#'
#' Note that this test is different than \code{\link{mood.test}} in the stats
#' package which tests for a difference in \emph{scale parameters} rather than
#' medians.
#'
#' @param X a list of two or more numeric vectors
#' @param exact logical; if \code{TRUE} (default), uses Fisher's exact test
#'   and Pearson's chi-squared test otherwise
#' @param ... additional parameters passed to \code{\link{fisher.test}} or
#'   \code{\link{chisq.test}}
#'
#' @seealso
#' \code{\link{mood.test}}; \code{\link{fisher.test}};
#' \code{\link{chisq.test}}; \code{\link{kruskal.test}};
#' \code{\link{wilcox.test}}
#'
#' @examples
#' set.seed(1)
#' X <- list(rnorm(10), rnorm(10, 1), rnorm(20, 2))
#' moods.test(X)
#' moods.test(X[1:2], exact = FALSE, correct = TRUE)
#'
#' plot(density(X[[1]]), xlim = range(unlist(X)), ylim = c(0, .5), main = '')
#' for (x in 2:3)
#'   lines(density(X[[x]]), col = x)
#'
#' @export

moods.test <- function(X, ..., exact = TRUE) {
  x <- unlist(X)
  g <- rep(ng <- seq_len(length(X)), times = sapply(X, length))
  m <- median(x)

  FUN <- if (exact)
    fisher.test else chisq.test
  res <- FUN(x < m, g, ...)

  res$method <- sprintf('Mood\'s median test of %s groups (%s)',
                        length(ng), res$method)
  res$data <- table(x < m, g, dnn = c(sprintf('< median (%s)', m), 'group'))

  res
}

#' Rank-sum test for trend of ordered groups
#'
#' An implementation of Cuzick's extension of the Wilcoxon rank-sum test to
#' assess trend in data with three or more \emph{ordinal} groups.
#'
#' Data are assumed to be from independent groups with a natural or
#' meaningful order. If \code{x} and \code{g} are given as vectors and
#' \code{g} is a factor, the order of \code{levels(g)} will be respected;
#' however, if \code{g} is \emph{not} a factor, it will be coerced, and the
#' levels will be the same as \code{levels(factor(g))}.
#'
#' For example, if \code{g} is (a character string of) \code{c("5mg", "10mg",
#' "15mg")}, then the groups will be ordered as \code{"10mg" "15mg" "5mg"}
#' which may not be desired.
#'
#' Pairwise comparisons between each pair of groups is performed using the
#' function given by \code{details} (default is \code{\link{wilcox.test}});
#' the overall will be compared by \code{\link{kruskal.test}}. Other functions
#' may be used (e.g., in cases with ties), but these need a formula method
#' similar to \code{cuzick.test}, \code{wilcox.test}, etc. Common methods such
#' as \code{\link[coin]{wilcox_test}} or \code{\link[coin]{kruskal_test}} from
#' the \pkg{\link[coin]{coin}} package will work; see examples.
#'
#' @param x a numeric vector of data values or a list of numeric data vectors;
#'   non-numeric elements of a list will be coerced with a warning; if \code{x}
#'   is a list, the list elements are assumed to be groups ordered as
#'   \code{x[[1]], x[[2]], ..., x[[n]]}
#' @param ... additional arguments passed to the function given by
#'   \code{details}
#' @param g a vector or factor object giving the group for the corresponding
#'   elements of \code{x}, ignored with a warning if \code{x} is a list; if
#'   \code{g} is \emph{not} a factor, it will be coerced, and groups will be
#'   ordered as \code{sort(unique(g))}; see \code{\link{factor}}
#' @param details \code{FALSE} or a function to compute comparisons between
#'   pairs of groups; see details
#' @param correct logical; if \code{TRUE}, a correction is applied to the
#'   standard error of the test statistic (default)
#' @param formula a formula of the form \code{response ~ group} where
#'   \code{response} is a numeric variable and \code{group} is a factor-like
#'   variable with three or more unique values (groups)
#' @param data an optional matrix or data frame (or similar: see
#'   \code{\link{model.frame}}) containing the variables in \code{formula}; by
#'   default, the variables are taken from \code{environment(formula)}
#' @param simulate.p.value logical; if \code{TRUE}, p-value is computed using
#'   by Monte Carlo simulation
#' @param B an integer specifying the number of replicates used in the Monte
#'   Carlo test
#'
#' @return
#' A list with class "\code{htest}" containing the following elements:
#'
#' \item{statistic}{the value of the test statistic with a name describing it}
#' \item{p.value}{the p-value for the test (two-sided, (un)corrected for ties)}
#' \item{estimate}{the medians by group}
#' \item{method}{a character string describing the test used}
#' \item{data.name}{a character string giving the names of the data}
#' \item{details}{a list of pairwise (\code{details}) and overall
#' (\code{\link{kruskal.test}}) comparisons}
#' \item{\code{conf.int}}{optionally, (if \code{simulate.p.value = TRUE})
#' the 99\% confidence interval of the Monte Carlo p-value}
#' \item{\code{summary}}{optionally (if \code{simulate.p.value = TRUE}),
#' a summary of the simulated test statistics}
#'
#' @references
#' Altman, D. G. 1991. \emph{Practical Statistics for Medical Research}.
#' London: Chapman & Hall/CRC.
#'
#' Cuzick, J. 1985. A Wilcoxon-type test for trend. \emph{Statistics in
#' Medicine} \strong{4}: 87-90.
#'
#' @seealso
#' \code{\link{kruskal.test}}; \code{\link{wilcox.test}}
#'
#' \code{\link[coin]{kruskal_test}}; \code{\link[coin]{wilcox_test}} from the
#' \pkg{\link[coin]{coin}} package
#'
#' @examples
#' ## Altman (1991), 217
#' ## ocular exposure to ultraviolet radiation for 32 pairs of sunglasses
#' ## classified into three groups according to the amount of visible light
#' ## transmitted
#'
#' x <- list(c(1.4, 1.4, 1.4, 1.6, 2.3, 2.5),
#'           c(0.9, 1.0, 1.1, 1.1, 1.2, 1.2,
#'             1.5, 1.9, 2.2, 2.6, 2.6, 2.6,
#'             2.8, 2.8, 3.2, 3.5, 4.3, 5.1),
#'           c(0.8, 1.7, 1.7, 1.7, 3.4, 7.1, 8.9, 13.5))
#' cuzick.test(x, correct = FALSE)
#'
#'
#' ## equivalent ways to call cuzick.test
#' cuzick.test(x)
#' cuzick.test(unlist(x), rep(seq_along(x), lengths(x)))
#' cuzick.test(x ~ g, data.frame(x = unlist(x), g = rep(1:3, lengths(x))))
#'
#'
#' ## all pairwise comparisons are returned by default in $details
#' cuzick.test(x)$details
#' cuzick.test(x, alternative = 'less', correct = FALSE)$details
#' cuzick.test(x, details = kruskal.test)$details
#' cuzick.test(x, details = t.test)$details
#' # cuzick.test(x, details = coin::wilcox_test)$details
#'
#'
#' ## Cuzick (1985), 87-90
#' ## mice inoculated with five cell lines which had been selected for their
#' ## increasing metastatic potential; number of lung metastases found in each
#' ## mouse after inoculation
#'
#' x <- list(
#'   'CMT 64'  = c(0, 0, 1, 1, 2, 2, 4, 9),
#'   'CMT 167' = c(0, 0, 5, 7, 8, 11, 13, 23, 25, 97),
#'   'CMT 170' = c(2, 3, 6, 9, 10, 11, 11, 12, 21),
#'   'CMT 175' = c(0, 3, 5, 6, 10, 19, 56, 100, 132),
#'   'CMT 181' = c(2, 4, 6, 6, 6, 7, 18, 39, 60)
#' )
#' cuzick.test(x)$p.value / 2 ## one-sided (corrected for ties)
#'
#'
#' ## coercing character group vector g to factor may have undesired order
#' set.seed(1)
#' x  <- sort(rnorm(20))
#' g1 <- sample(paste0(c(5, 10, 15), 'mg'), 20, replace = TRUE)
#' g2 <- factor(g1, levels = paste0(c(5, 10, 15), 'mg'))
#'
#' ## wrong order
#' tplot(x ~ g1, data.frame(x, g1), type = 'db', test = cuzick.test)
#'
#' ## correct order
#' tplot(x ~ g2, data.frame(x, g2), type = 'db', test = cuzick.test)
#'
#'
#' ## groups need not be equally-spaced but will affect statistic/p-value
#' set.seed(1)
#' x  <- sort(rnorm(20))
#' g1 <- sample(1:3, 20, replace = TRUE)
#' g2 <- g1 + (g1 == 3)
#'
#' tplot(x, g1, test = cuzick.test)
#' tplot(x, g2, test = cuzick.test)
#' ## compare
#' tplot(x, factor(g2), test = cuzick.test)
#'
#' @export

cuzick.test <- function(x, ...) {
  UseMethod('cuzick.test')
}

#' @rdname cuzick.test
#' @export
cuzick.test.default <- function(x, g, details = wilcox.test, correct = TRUE,
                                ..., simulate.p.value = FALSE, B = 2000L) {
  ## checks adapted from stats:::kruskal.test.default
  if (is.list(x)) {
    if (length(x) < 2L)
      stop('\'x\' must be a list with at least 2 elements')
    if (!missing(g))
      warning('\'x\' is a list - ignoring \'g\'')
    dname <- deparse(substitute(x))
    x <- lapply(x, function(u) u <- u[complete.cases(u)])
    if (!all(sapply(x, is.numeric)))
      warning('some elements of \'x\' are not numeric and will be coerced')
    l <- sapply(x, length)
    # if (any(l == 0L))
    #   stop('all groups must contain data')
    g <- factor(rep.int(seq_len(length(x)), l))
    x <- unlist(x)
  } else {
    if (length(x) != length(g))
      stop('\'x\' and \'g\' must have the same length')
    dname <- paste(deparse(substitute(x)), 'and', deparse(substitute(g)))
    OK <- complete.cases(x, g)
    x <- x[OK]
    g <- g[OK]
    if (is.numeric(g) && !all(is.finite(g)))
      stop('all group levels must be finite')
    if (length(unique(g)) < 2L)
      stop('all observations are in the same group')
  }

  fac <- if (is.factor(g) || is.character(g)) {
    g <- as.factor(g)
    # if (nlevels(g) != nlevels(g <- droplevels(g)))
    #   warning('unused factor level(s) dropped')
    TRUE
  } else FALSE

  ug <- length(unique(g))
  li <- if (fac)
    seq.int(levels(g)) else as.character(sort(unique(g)))
  ni <- table(g)[li]

  estimate <- c(tapply(x, g, median))
  npg <- c(table(g))
  names(estimate) <- sprintf('median of %s (n=%s)', names(estimate), npg)
  method <- sprintf(
    'Wilcoxon rank-sum test for trend in %s ordered groups (%scorrected for ties)',
    ug, c('un', '')[correct + 1L]
  )
  z <- cuzick.test.stat(x, g, correct)
  pval <- 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE))

  res <- list(
    statistic = c(z = z), p.value = pval, estimate = estimate,
    method = method, data.name = dname, npg = npg, details = NULL
  )

  if (simulate.p.value) {
    p <- cuzick.test.pvalue(x, g, correct, B, TRUE)
    res$p.value <- unname(p[1L])
    res$method <- sprintf(
      '%s with simulated p-value (based on %s replicates)', method, B
    )
    res$conf.int <- structure(p[2:3], conf.level = attr(p, 'conf.level'))
    res$simulate <- attr(p, 'simulate')
  }

  ## pairwise details
  if (!identical(details, FALSE)) {
    l2df <- function(l) {
      data.frame(unlist(l), factor(rep(seq_along(l), lengths(l))))
    }
    tidy <- function(l) {
      n <- lengths(l) == 1L
      data.frame(Filter(length, unclass(l)[n]), stringsAsFactors = FALSE)
    }

    sp  <- split(x, g)
    idx <- as.list(data.frame(combn(length(sp), 2L)))
    ids <- apply(combn(names(ni), 2L), 2L, paste, collapse = ' vs ')
    sp  <- lapply(idx, function(x) setNames(l2df(sp[x]), c('x', 'g')))
    names(sp) <- ids

    nul <- structure(
      list(statistic = NA, parameter = NA, p.value = NA, null.value = NA,
           stderr = NA, alternative = NA, method = NA, data.name = NA),
      class = 'htest'
    )

    pw <- lapply(sp, function(X) {
      suppressWarnings({
        tryCatch(details(x ~ g, X, ...), error = function(e) nul)
      })
    })
    PW <- tryCatch(lapply(pw, tidy), error = function(e) NULL)
    nn <- Reduce(intersect, lapply(PW, names))
    PW <- lapply(PW, function(x) x[, nn])

    pw <- list(
      pairs = if (is.null(PW))
        pw else {
          pww <- cbind(pairs = ids, do.call('rbind', PW))
          rownames(pww) <- NULL
          pww
        },
      overall = tidy(kruskal.test(x ~ g))
    )

    res$details <- pw
  }

  structure(res, class = 'htest')
}

#' @rdname cuzick.test
#' @export
cuzick.test.formula <- function (formula, data, ...) {
  ## adapted from stats:::kruskal.test.formula
  if (missing(formula) || (length(formula) != 3L))
    stop('\'formula\' missing or incorrect')

  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame(1L))))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame(1L))

  if (length(mf) > 2L)
    stop('\'formula\' should be of the form response ~ group')

  dname <- paste(names(mf), collapse = ' by ')
  names(mf) <- NULL
  y <- do.call('cuzick.test', list(x = mf[, 1L], g = mf[, 2L], ...))
  y$data.name <- dname

  y
}

cuzick.test.stat <- function(x, g, correct) {
  if (length(x) < 2L)
    stop('not enough observations')

  fac <- if (is.factor(g) || is.character(g)) {
    g <- as.factor(g)
    # if (nlevels(g) != nlevels(g <- droplevels(g)))
    #   warning('unused factor level(s) dropped')
    TRUE
  } else FALSE

  ux <- length(unique(x))
  # ug <- length(unique(g))
  ug <- max(length(unique(g)), nlevels(g))

  if (!is.numeric(x))
    stop('\'x\' values must be numeric')
  if (ug < 3L)
    stop('three or more unique groups are required')
  if (ug > ux)
    warning('more unique groups than unique response values')

  ## scores for each group
  ## if g is character or factor, use 1,2,...,k
  ## if g is numeric, dont assume equally-spaced, use sorted unique values
  li <- if (fac)
    seq.int(ug) else as.character(sort(unique(g)))
  ni <- table(g)[li]
  N  <- sum(ni)

  ## sum of ranks of x for ith group; sum of weighted sum of ith group scores
  li <- as.numeric(li)
  Ri <- c(tapply(rank(x), g, sum))
  L  <- sum(li * ni)

  ## T statistic, expected value, variance
  Ts <- sum(li * Ri, na.rm = TRUE)
  eT <- (N + 1) * L / 2
  vT <- (N + 1) / 12 * (N * sum(li ^ 2 * ni) - L ^ 2)

  ## correction for ties
  ## tj: times each value of x appears; a: correction for se
  tj <- c(tapply(x, x, length))
  a  <- sum(tj * (tj ^ 2 - 1)) / (N * (N ^ 2 - 1))
  a  <- c(1, sqrt(1 - a))[correct + 1L]

  ## (un)corrected test statistic: (T - expected) / se
  (Ts - eT) / (a * sqrt(vT))
}

cuzick.test.pvalue <- function(x, g, correct, B = 2000L,
                               ci = FALSE, alpha = 0.99) {
  stopifnot(
    is.logical(correct),
    alpha %inside% 0:1
  )

  B <- as.integer(B)
  f <- function() {
    g <- sample(g)
    cuzick.test.stat(x, g, correct)
  }

  Z <- cuzick.test.stat(x, g, correct)
  Z <- abs(Z)
  r <- unname(replicate(B, f()))
  z <- r > Z
  p <- mean(z, na.rm = TRUE) * 2

  if (!ci)
    return(p)

  s2 <- sqrt(1 / (B - 1) * sum((z - p) ^ 2))

  ci <- p + c(-1, 1) * 2.576 * s2 / sqrt(B)
  if (ci[1L] < 0)
    ci[1L] <- 0
  if (ci[2L] > 1)
    ci[1L] <- 1

  ci <- if (p == 0)
    c(0, 1 - (1 - alpha) ^ (1 / B))
  else if (p == 1)
    c((1 - alpha) ^ (1 / B), 1)
  else ci

  structure(
    setNames(c(p, ci), c('p.value', 'LCI', 'UCI')),
    conf.level = alpha,
    simulate = summary(r)
  )
}

#' Jonckheere-Terpstra test
#'
#' An implementation of Jonckheere's trend test for \emph{ordered} independent
#' samples.
#'
#' @param x either a two-dimensional contingency table in matrix form or a
#'   factor object
#' @param y a factor object; ignored if x is a matrix
#'
#' @return
#' A list with class "\code{htest}" containing the following elements:
#'
#' \item{statistic}{the value of the test statistic with a name describing it}
#' \item{p.value}{the p-value for the test (asymptotic, two-sided, corrected
#' for ties)}
#' \item{method}{a character string describing the test used}
#' \item{data.name}{a character string giving the names of the data}
#'
#' @references
#' Jonckheere, A. R. (1954). A distribution-free k-sample test again ordered
#' alternatives. \emph{Biometrika} \strong{41}:133-145.
#'
#' Terpstra, T. J. (1952). The asymptotic normality and consistency of
#' Kendall's test against trend, when ties are present in one ranking.
#' \emph{Indagationes Mathematicae} \strong{14}:327-333.
#'
#' @seealso
#' \code{\link[stats]{cor.test}}; \code{clinfin::jonckheere.test}
#'
#' @examples
#' ## example from Exact Test (Mehta), figure 12.2
#' dat <- matrix(
#'   c(100, 18, 50, 50, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 1, 1), 4,
#'   dimnames = list(dose = 1:4 * 100,
#'                   toxicity = c('Mild', 'Moderate', 'Severe', 'Death'))
#' )
#' jt.test(dat)
#'
#'
#' tbl <- table(mtcars$gear, mtcars$cyl)
#' jt.test(tbl)
#' ## compare
#' cor.test(mtcars$gear, mtcars$cyl, method = 'kendall', exact = FALSE)
#' # clinfun::jonckheere.test(mtcars$gear, mtcars$cyl)
#'
#'
#' ## from stats::cor.test
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)
#'
#' cor.test(x, y, method = 'kendall', exact = FALSE)
#' jt.test(x, y)
#'
#' @export

jt.test <- function(x, y = NULL) {
  ## checks adapted from stats::fisher.test
  dname  <- deparse(substitute(x))
  method <- 'Jonckheere-Terpstra Test'

  if (is.data.frame(x))
    x <- as.matrix(x)
  if (is.matrix(x)) {
    if (any(dim(x) < 2L))
      stop('\'x\' must have at least 2 rows and columns')
    if (!is.numeric(x) || any(x < 0) || anyNA(x))
      stop('all entries of \'x\' must be nonnegative and finite')
    if (!is.integer(x)) {
      xo <- x
      x <- round(x)
      if (any(x > .Machine$integer.max))
        stop('\'x\' has entries too large to be integer')
      if (!isTRUE(ax <- all.equal(xo, x)))
        warning(gettextf('\'x\' has been rounded to integer: %s', ax),
                domain = NA)
      storage.mode(x) <- 'integer'
    }
  } else {
    if (is.null(y))
      stop('if \'x\' is not a matrix, \'y\' must be given')
    if (length(x) != length(y))
      stop('\'x\' and \'y\' must have the same length')
    dname <- paste(dname, 'and', deparse(substitute(y)))
    ok <- complete.cases(x, y)
    x <- as.factor(x[ok])
    y <- as.factor(y[ok])
    if ((nlevels(x) < 2L) || (nlevels(y) < 2L))
      stop('\'x\' and \'y\' must have at least 2 levels')
    x <- table(x, y)
  }

  z    <- jt.test.stat(x, y)
  pval <- 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE))
  res  <- list(
    statistic = c(z = z), p.value = pval,
    method = method, data.name = dname
  )

  structure(res, class = 'htest')
}

jt.test.stat <- function(x, y) {
  get_PQ <- function(x, y) {
    ## calculates P,Q scores larger,smaller than current score
    x <- unlist(x)
    y <- unlist(y)
    as.integer(rowSums(vapply(x, function(z)
      c(sum(y > z), sum(y < z)), integer(2L))))
  }

  n  <- sum(x)
  ti <- rowSums(x)
  ui <- colSums(x)
  # if (any(rowSums(!!x) > 1))
  #   warning('Cannot compute exact p-value with ties')

  ## variance
  vS <- (2 * (n ^ 3 - sum(ti ^ 3) - sum(ui ^ 3)) +
           3 * (n ^ 2 - sum(ti ^ 2) - sum(ui ^ 2)) + 5 * n) / 18 +
    (sum(ti ^ 3) - 3 * sum(ti ^ 2) + 2 * n) *
    (sum(ui ^ 3) - 3 * sum(ui ^ 2) + 2 * n) / (9 * n * (n - 1) * (n - 2)) +
    (sum(ti ^ 2) - n) * (sum(ui ^ 2) - n) / (2 * n * (n - 1))

  ## statistic
  ns <- c(x)
  sp <- c(row(x) * !!ns)
  sp <- split(rep(sp, ns), rep(c(col(x)), ns))
  PQ <- rowSums(vapply(seq_len(length(sp)), function(x)
    get_PQ(sp[x], sp[-(1:x)]), integer(2L)))

  (PQ[1L] - PQ[2L]) / sqrt(vS)
}

#' \code{rpart} utilities
#'
#' @description
#' Utilities for the \pkg{\link{rpart}} package:
#'
#' \code{rpart_parent} returns all parent nodes of \code{node}, i.e., the
#' path from node \code{1} to \code{node}.
#'
#' \code{rpart_subset} and \code{rpart_subset2} (in examples) return a
#' subset of the data used in \code{rpart} for any intermediate or terminal
#' \code{node}.
#'
#' \code{rpart_nodes} returns the terminal node label for each observation
#' in the original data frame used for \code{tree}.
#'
#' @param node an integer representing the node number
#' @param tree an object returned from \code{rpart}
#' @param node_labels a vector of labels having the same length as the number
#'   of terminal nodes or total nodes
#' @param droplevels logical; if \code{TRUE}, only node labels with at least
#'   one observation are used (i.e., only terminal node labels are used)
#'
#' @return
#' \code{rpart_parent} returns a vector representing the path from the root to
#' \code{node}.
#'
#' \code{rpart_subset} returns the data frame of observations in \code{node}.
#' For any \code{tree}, the possibilities
#'
#' \code{rpart_nodes} returns a factor variable
#'
#' @seealso
#' \url{https://stackoverflow.com/q/36086990/2994949}
#'
#' \url{https://stackoverflow.com/q/36748531/2994949}
#'
#' @examples
#' rpart_parent(116)
#' rpart_parent(29)
#'
#' \dontrun{
#' library('rpart')
#' fit <- rpart(Kyphosis ~ Age + Number + Start, kyphosis, minsplit = 5)
#'
#' ## children nodes should have identical paths
#' identical(
#'   head(rpart_parent(28), -1L),
#'   head(rpart_parent(29), -1L)
#' )
#'
#' ## terminal nodes should combine to original data
#' nodes <- as.integer(rownames(fit$frame[fit$frame$var %in% '<leaf>', ]))
#' sum(sapply(nodes, function(x) nrow(rpart_subset(fit, x))))
#' nrow(kyphosis)
#'
#' ## all nodes
#' nodes <- as.integer(rownames(fit$frame))
#' sapply(nodes, function(x) nrow(rpart_subset(fit, x)))
#'
#'
#' rpart_subset2 <- function(tree, node = 1L) {
#'   require('partykit')
#'   ptree <- as.party(tree)
#'   ptree$data <- model.frame(eval(tree$call$data, parent.frame(1L)))
#'   ## retain transformed variables but drop those not in formula
#'   ## http://stackoverflow.com/a/36816883/2994949
#'   # ptree$data <- model.frame(tree)
#'   data_party(ptree, node)[, seq_along(ptree$data)]
#' }
#'
#' ## note differences in node labels in party vs rpart
#' dim(rpart_subset(fit, 4))
#' dim(rpart_subset2(fit, 3))
#'
#'
#' rpart_nodes(fit)
#' rpart_nodes(fit, TRUE)
#'
#' table(rpart_nodes(fit, letters[1:10]),
#'       rpart_nodes(fit, letters[1:19]))
#'
#' ## subset an rpart object by node id which should only include
#' ## observations found in children of the node id(s) selected
#' identical(kyphosis, rpart_subset(fit, unique(rpart_nodes(fit))))
#'
#' kyphosis$node <- rpart_nodes(fit)
#' rpart_subset(fit, 14:15)
#' }
#'
#' @name rpart_utils
NULL

#' @rdname rpart_utils
#' @export
rpart_parent <- function(node = 1L) {
  node <- as.integer(node)
  if (node[1L] == 1L)
    1L else sort(c(node, Recall(node %/% 2L)))
}

#' @rdname rpart_utils
#' @export
rpart_subset <- function(tree, node = 1L) {
  nodes <- sort(as.integer(rownames(tree$frame)))
  rn    <- rpart_nodes(tree, FALSE)

  if (length(nn <- node[node %ni% nodes]))
    stop('Node ', toString(nn), ' not found\n\nPossibilities are ',
         iprint(nodes, digits = 0L), '.\n')

  f <- function(n) {
    idx <- unique(unlist(idx[sapply(idx, function(x)
      sapply(n, function(y) y %in% x))]))
    rn %in% idx[idx >= n]
  }

  data <- eval(tree$call$data, parent.frame(1L))
  idx  <- sapply(nodes, rpart_parent)

  data[!!rowSums(sapply(node, f)), ]
}

#' @rdname rpart_utils
#' @export
rpart_nodes <- function(tree, node_labels = FALSE, droplevels = TRUE) {
  if (identical(node_labels, FALSE))
    return(rownames(tree$frame)[tree$where])

  labels <- if (!isTRUE(node_labels)) {
    if (length(node_labels) == length(table(tree$where)) ||
        length(node_labels) == nrow(tree$frame))
      node_labels else {
        warning('length(labels) != number of total or terminal nodes')
        labels(tree, minlength = 0L)
      }
  } else labels(tree, minlength = 0L)

  nl <- factor(labels[if (length(labels) == nrow(tree$frame))
    tree$where else as.integer(factor(tree$where))], labels)

  if (droplevels)
    droplevels(nl) else nl
}

#' Test for trend in proportions
#'
#' Performs a test for trend in an \code{r x c} contingency table with
#' one nominal (rows, r > 2) and one ordinal (columns, c > 2) variable.
#'
#' @param x a factor-like vector giving the (unordered) variable (equivalently
#'   the row variable of a contingency table); if \code{x} is also ordered,
#'   consider using \code{\link{jt.test}}
#'
#'   alternatively, \code{x} can be an \code{r x c} table or matrix with at
#'   least two rows (unordered) and three columns (ordered); \code{x} may also
#'   be a list of the row variable split by the ordered column variable in
#'   which case the list is assumed to be ordered, i.e., \code{x[[1]]} <
#'   \code{x[[2]] < ... < x[[c]]}; see examples
#' @param ... further arguments to be passed to or from methods
#' @param g a factor-like vector giving the \emph{ordered} group for each
#'   corresponding element of \code{x}, ignored with a warning if \code{x} is
#'   a list or table; if \code{g} is not a factor, it will be coerced, and
#'   groups will be ordered as sort(unique(g)); see \code{\link{factor}}
#' @param simulate.p.value logical; if \code{TRUE}, p-value is computed using
#'   by Monte Carlo simulation
#' @param B an integer specifying the number of replicates used in the Monte
#'   Carlo test
#' @param formula a formula of the form \code{row ~ column} where \code{row}
#'   gives the (unordered) row variable and \code{column} gives the
#'   \emph{ordered} column variable
#' @param data an optional matrix or data frame (or similar: see
#'   \code{\link{model.frame}}) containing the variables in \code{formula};
#'   by default the variables are taken from \code{environment(formula)}
#'
#' @return
#' A list with class "\code{htest}" containing the following elements:
#'
#' \item{\code{statistic}}{the chi-squared test statistic}
#' \item{\code{parameter}}{the degrees of freedom of the approximate chi-
#' squared distribution of the test statistic}
#' \item{\code{p.value}}{the p-value of the test (two-sided)}
#' \item{\code{method}}{a character string describing the test, and,
#' optionally, the number of Monte Carlo replications, if applicable}
#' \item{\code{data.name}}{a character string giving the names of the data}
#' \item{\code{conf.int}}{optionally (if \code{simulate.p.value = TRUE}),
#' the 99\% confidence interval of the Monte Carlo p-value}
#' \item{\code{summary}}{optionally (if \code{simulate.p.value = TRUE}),
#' a summary of the simulated test statistics}
#'
#' @seealso
#' \code{\link{jt.test}} for doubly-ordered tables; \code{\link{cuzick.test}};
#' \code{\link{ca.test}}; \code{DescTools::CochranArmitageTest};
#' \code{\link{prop.trend.test}}
#'
#' @examples
#' ## example from Exact Test (Mehta), figure 11.1
#' ## system.file('docs', 'PASW_Exact_Tests.pdf', package = 'rawr')
#' dat <- data.frame(
#'   regimen  = rep(c('CTMX', 'CCNU', 'MTX', 'CTX+CCNU', 'CTX+CCNU+MTX'),
#'                  times = c(2, 2, 3, 4, 6)),
#'   response = c('NR', 'NR', 'NR', 'PR', 'NR', 'NR', 'NR',
#'                'NR', 'NR', 'PR', 'PR', 'NR', 'PR', 'CR', 'CR', 'CR', 'CR')
#' )
#' dat$response2 <- factor(dat$response, c('NR', 'PR', 'CR'))
#'
#' kw.test(dat$regimen, dat$response)   ## incorrect
#' kw.test(dat$regimen, dat$response2)  ## correct
#'
#'
#' ## the following are equivalent to the above
#' kw.test(dat$regimen ~ dat$response2)
#' kw.test(regimen ~ response2, dat)
#' kw.test(split(dat$regimen, dat$response2))
#' kw.test(table(dat$regimen, dat$response2))
#'
#'
#' ## compare (note formula is reversed)
#' kruskal.test(response2 ~ regimen, dat)
#'
#' \dontrun{
#' ## simulate p-value with 5k replicates
#' set.seed(1)
#' kw.test(regimen ~ response2, dat, simulate.p.value = TRUE, B = 5000)
#' }
#'
#' @export

kw.test <- function(x, ...) {
  UseMethod('kw.test')
}

#' @rdname kw.test
#' @export
kw.test.default <- function(x, g, ..., simulate.p.value = FALSE, B = 2000L) {
  dname <- deparse(substitute(x))
  if (is.list(x)) {
    if (length(x) < 3L)
      stop('\'x\' must be a list with at least 3 elements')
    if (!missing(g))
      warning('\'x\' is a list - ignoring \'g\'')
    g <- as.factor(rep(seq_along(x), lengths(x)))
    x <- as.factor(unlist(x))
  } else if (inherits(x, 'table') || is.matrix(x)) {
    if (nrow(x) < 2L | ncol(x) < 3L)
      stop('\'x\' must have at least 2 rows and 3 (ordered) columns')
    if (!is.numeric(x) || any(x < 0) || anyNA(x))
      stop('all entries of \'x\' must be nonnegative and finite')
    if (!missing(g))
      warning('\'x\' is a matrix - ignoring \'g\'')
    names(dimnames(x)) <- NULL
    x <- data.frame(as.table(x))
    g <- rep(x$Var2, times = x$Freq)
    x <- rep(x$Var1, times = x$Freq)
  } else {
    dname <- paste(deparse(substitute(x)), 'and', deparse(substitute(g)))
    g <- as.factor(g)
    x <- as.factor(x)
  }

  ok <- complete.cases(x, g)
  x <- x[ok]
  g <- g[ok]

  if (!length(unique(g)) >= 3L) {
    warning('Fewer than 3 groups')
  }
  if (!length(unique(x)) >= 3L) {
    warning('Only 2 groups -- use rawr::ca.test or stats::prop.trend.test')
  }

  if (any(table(x, g) < 5L) & !simulate.p.value)
    warning(
      'Chi-squared approximation may be incorrect - ',
      'cells with < 5 observations\n',
      '\tConsider using simulate.p.value = TRUE for Monte Carlo p-value'
    )

  method <- 'Test for equality of proportions without continuity correction'

  res <- kruskal.test(g ~ x)

  if (simulate.p.value) {
    p <- sim.test.pvalue(x, g, kruskal.test, TRUE, B, TRUE, 0.99)
    res$p.value <- unname(p[1L])
    method <- sprintf('%s with simulated p-value (based on %s replicates)',
                      method, B)
    res$conf.int <- structure(p[2:3], conf.level = attr(p, 'conf.level'))
    res$simulate <- attr(p, 'simulate')
  }

  res$data.name <- dname
  res$method <- method

  res
}

#' @rdname kw.test
#' @export
kw.test.formula <- function (formula, data, ...) {
  ## adapted from stats:::kruskal.test.formula
  if (missing(formula) || (length(formula) != 3L))
    stop('\'formula\' missing or incorrect')

  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame(1L))))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame(1L))

  if (length(mf) > 2L)
    stop('\'formula\' should be of the form response ~ ordered group')

  dname <- paste(names(mf), collapse = ' by ')
  names(mf) <- NULL
  y <- do.call('kw.test', list(x = mf[, 1L], g = mf[, 2L], ...))
  y$data.name <- dname

  y
}

sim.test.pvalue <- function(x, g, FUN, ordered = FALSE, B = 2000L,
                            ci = FALSE, alpha = 0.99) {
  stopifnot(
    is.logical(ordered),
    alpha %inside% 0:1
  )

  B <- as.integer(B)
  f <- function() {
    g <- sample(g)
    k <- if (ordered)
      FUN(g ~ x) else FUN(x ~ g)
    k$statistic
  }

  Z <- if (ordered)
    FUN(g ~ x)$statistic else FUN(x ~ g)$statistic
  r <- suppressWarnings({
    unname(replicate(B, f()))
  })
  z <- r >= Z
  p <- mean(z, na.rm = TRUE)

  if (!ci)
    return(p)

  s2 <- sqrt(1 / (B - 1) * sum((z - p) ^ 2))

  ci <- p + c(-1, 1) * 2.576 * s2 / sqrt(B)
  if (ci[1L] < 0)
    ci[1L] <- 0
  if (ci[2L] > 1)
    ci[1L] <- 1

  ci <- if (p == 0)
    c(0, 1 - (1 - alpha) ^ (1 / B))
  else if (p == 1)
    c((1 - alpha) ^ (1 / B), 1)
  else ci

  structure(
    setNames(c(p, ci), c('p.value', 'LCI', 'UCI')),
    conf.level = alpha,
    simulate = summary(r)
  )
}

#' Cochran-Armitage test for trend
#'
#' Performs a Cochran-Armitage chi-squared test for trend in proportions for
#' a \code{2 x c} contingency table with a nominal row (r == 2) and ordinal
#' column (c > 2) variable.
#'
#' @param x a factor-like vector giving the (unordered) variable (equivalently
#'   the row variable of a contingency table)
#'
#'   alternatively, \code{x} can be a \code{2 x c} table or matrix with
#'   exactly two rows and at least three ordered columns; \code{x} may also
#'   be a list of the row variable split by the ordered column variable in
#'   which case the list is assumed to be ordered, i.e.,
#'   \code{x[[1]] < x[[2]] < ... < x[[c]]}; see examples
#' @param ... further arguments to be passed to or from methods
#' @param g a factor-like vector giving the \emph{ordered} group for each
#'   corresponding element of \code{x}, ignored with a warning if \code{x} is
#'   a list or table; if \code{g} is not a factor, it will be coerced, and
#'   groups will be ordered as sort(unique(g)); see \code{\link{factor}}
#' @param score group score for each column, default is \code{1:ncol}
#' @param simulate.p.value logical; if \code{TRUE}, p-value is computed using
#'   by Monte Carlo simulation
#' @param B an integer specifying the number of replicates used in the Monte
#'   Carlo test
#' @param formula a formula of the form \code{row ~ column} where \code{row}
#'   gives the row variable having two unique values and \code{column} gives
#'   the \emph{ordered} column variable
#' @param data an optional matrix or data frame (or similar: see
#'   \code{\link{model.frame}}) containing the variables in \code{formula};
#'   by default the variables are taken from \code{environment(formula)}
#'
#' @return
#' A list with class "\code{htest}" containing the following elements:
#'
#' \item{\code{statistic}}{the chi-squared test statistic}
#' \item{\code{parameter}}{the degrees of freedom of the approximate chi-
#' squared distribution of the test statistic}
#' \item{\code{p.value}}{the p-value of the test (two-sided)}
#' \item{\code{method}}{a character string describing the test, and,
#' optionally, the number of Monte Carlo replications, if applicable}
#' \item{\code{data.name}}{a character string giving the names of the data}
#' \item{\code{conf.int}}{optionally (if \code{simulate.p.value = TRUE}),
#' the 99\% confidence interval of the Monte Carlo p-value}
#' \item{\code{summary}}{optionally (if \code{simulate.p.value = TRUE}),
#' a summary of the simulated test statistics}
#'
#' @seealso
#' \code{\link{prop.trend.test}}; \code{\link{jt.test}} for doubly-ordered tables;
#' \code{\link{cuzick.test}}; \code{DescTools::CochranArmitageTest}
#'
#' @examples
#' ## example from stats::prop.trend.test
#' smokers  <- c(83, 90, 129, 70)
#' patients <- c(86, 93, 136, 82)
#' prop.test(smokers, patients)
#' prop.trend.test(smokers, patients)
#'
#' # DescTools::CochranArmitageTest(rbind(smokers, patients - smokers))
#' ca.test(rbind(smokers, patients - smokers))
#' ca.test(rbind(smokers, patients - smokers), score = c(0, 0, 1, 2))
#'
#' ## equivalent ways to call ca.test
#' dat <- data.frame(x = mtcars$vs, y = mtcars$gear)
#' ca.test(dat$x, dat$y)
#' ca.test(x ~ y, dat)
#' ca.test(split(dat$x, dat$y))
#' ca.test(table(dat$x, dat$y))
#'
#'
#' \dontrun{
#' ## simulate p-value with 1k replicates
#' set.seed(1)
#' ca.test(rbind(smokers, patients - smokers), simulate.p.value = TRUE, B = 1000)
#' }
#'
#' @export

ca.test <- function(x, ...) {
  UseMethod('ca.test')
}

#' @rdname ca.test
#' @export
ca.test.default <- function(x, g, ..., score = NULL,
                            simulate.p.value = FALSE, B = 2000L) {
  dname <- deparse(substitute(x))
  if (is.list(x)) {
    if (length(x) < 3L)
      stop('\'x\' must be a list with at least 3 elements')
    if (!missing(g))
      warning('\'x\' is a list - ignoring \'g\'')
    g <- as.factor(rep(seq_along(x), lengths(x)))
    x <- as.factor(unlist(x))
  } else if (inherits(x, 'table') || is.matrix(x)) {
    if (nrow(x) != 2L | ncol(x) < 3L)
      stop('\'x\' must have 2 rows and at least 3 (ordered) columns')
    if (!is.numeric(x) || any(x < 0) || anyNA(x))
      stop('all entries of \'x\' must be nonnegative and finite')
    if (!missing(g))
      warning('\'x\' is a matrix - ignoring \'g\'')
    names(dimnames(x)) <- NULL
    x <- data.frame(as.table(x))
    g <- rep(x$Var2, times = x$Freq)
    x <- rep(x$Var1, times = x$Freq)
  } else {
    dname <- paste(deparse(substitute(x)), 'and', deparse(substitute(g)))
    g <- as.factor(g)
    x <- as.factor(x)
  }

  ok <- complete.cases(x, g)
  x <- x[ok]
  g <- g[ok]
  tbl <- table(x, g)

  if (ncol(tbl) < 2L)
    stop('Fewer than 2 groups - row variable is not binary')
  if (ncol(tbl) < 3L)
    stop('Fewer than 3 groups - column variable is not ordinal')

  if (any(tbl < 5L) & !simulate.p.value)
    warning(
      'Chi-squared approximation may be incorrect - ',
      'cells with < 5 observations\n',
      '\tConsider using simulate.p.value = TRUE for Monte Carlo p-value'
    )

  method <- sprintf('Cochran-Armitage test for trend in %sx%s table',
                    nrow(tbl), ncol(tbl))
  score <- score %||% seq.int(ncol(tbl))
  dname <- sprintf('%s\n\tusing scores: %s', dname, toString(score))
  res <- prop.trend.test(tbl[1L, ], colSums(tbl), score)

  if (simulate.p.value) {
    suppressWarnings({
      p <- sim.test.pvalue(x, g, ca.test, FALSE, B, TRUE, 0.99)
    })
    res$p.value <- unname(p[1L])
    method <- sprintf('%s with simulated p-value (based on %s replicates)',
                      method, B)
    res$conf.int <- structure(p[2:3], conf.level = attr(p, 'conf.level'))
    res$simulate <- attr(p, 'simulate')
  }

  res$data.name <- dname
  res$method <- method

  res
}

#' @rdname ca.test
#' @export
ca.test.formula <- function (formula, data, ...) {
  ## adapted from stats:::kruskal.test.formula
  if (missing(formula) || (length(formula) != 3L))
    stop('\'formula\' missing or incorrect')

  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame(1L))))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame(1L))

  if (length(mf) > 2L)
    stop('\'formula\' should be of the form response ~ ordered group')

  dname <- paste(names(mf), collapse = ' by ')
  names(mf) <- NULL
  y <- do.call('ca.test', list(x = mf[, 1L], g = mf[, 2L], ...))
  y$data.name <- dname

  y
}

#' Linear spline
#'
#' Fit a linear spline regression model with pre-specified knots.
#'
#' \code{rawr:::lsdata} creates a data set to fit \code{\link{lm}} for
#' the \code{knots}. Adjusted values may be extracted with
#' \code{\link{predict.lm}}.
#'
#' The plotting method draws one or two figures: if \code{which = 1}, the
#' original \code{x} and \code{y} values are plotted with fit lines for
#' each spline, and \code{{x, y}} coordinates are projected onto the line.
#' If \code{which = 2}, the \code{x} values are plotted with the fitted
#' \code{y} values and colored by spline.
#'
#' @param x,y the x- and y-axis variables
#' @param knots value(s) of \code{x} where knots will be fixed
#' @param col a vector of colors for each spline
#' @param which an integer vector specifying the plot(s); see details
#' @param ... additional arguments passed to \code{\link{plot.default}} or
#'   further to \code{\link{par}}
#' @param type character indicating the type of plotting; see
#'   \code{\link{plot.default}}
#'
#' @return
#' \code{lspline} returns an \code{\link{lm}} object with class
#' \code{"lspline"} and attribute \code{attr(., "knots")}. The data used to
#' fit the model can be accessed with \code{$model} or created using
#' \code{rawr:::lsdata}.
#'
#' @examples
#' x <- cars$speed
#' y <- cars$dist
#'
#' lspline(x, y, NULL)
#' lm(y ~ x)
#'
#' ls <- lspline(x, y, c(10, 20))
#' predict(ls)
#' plot(ls)
#'
#' plot(y ~ x)
#' points(ls, col = 3:5, pch = 16)
#' lines(ls, col = 3:5, lwd = 2)
#'
#' ## compare
#' plot(lspline(x, y, knots = 15), col = 2:3, which = 1L)
#'
#' knot <- 15
#' fit <- lm(dist ~ speed + I((speed - knot) * (speed >= knot)), cars)
#' plot(y ~ x)
#' xx <- seq(min(x), knot, length.out = 1000L)
#' co <- coef(fit)
#' lines(xx, co[1] + co[2] * xx, col = 2)
#' xx <- seq(knot, max(x), length.out = 1000L)
#' lines(xx, co[1] + co[2] * xx + co[3] * (xx - knot), col = 3)
#'
#' @export

lspline <- function(x, y, knots = NULL) {
  data <- data.frame(y, lsdata(x, knots))

  structure(
    lm(y ~ ., data), knots = knots,
    class = c('lspline', 'lm')
  )
}

lsdata <- function(x, knots = NULL) {
  data <- data.frame(x)
  if (is.null(knots))
    return(data)
  data[, paste0('x', knots)] <- lapply(knots, function(k)
    (x >= k) * (x - k))
  data
}

#' @rdname lspline
#' @export
plot.lspline <- function(x, col = NULL, which = 1:2, ...) {
  knots <- attr(x, 'knots')
  data  <- x$model
  col   <- rep_len(col %||% seq_along(c(1L, knots)), length(knots) + 1L)

  xx <- if (!is.null(knots))
    Map(function(x, y) seq(x, y, length.out = 1000L),
        c(min(sort(data$x)), knots), c(knots, max(sort(data$x))))
  else seq(min(sort(data$x)), max(sort(data$x)), length.out = 1000L)

  op <- par(mfrow = c(1L, length(which)))
  on.exit(par(op))

  col <- (col %||% palette())[findInterval(data$x, knots) + 1L]
  if (1L %in% which) {
    plot(y ~ x, data, ...)

    pr <- lapply(seq_along(xx), function(ii) {
      pr <- predict(x, lsdata(xx[[ii]], knots))
      lines(xx[[ii]], pr, col = (col %||% palette())[ii])
    })
    points(data$x, predict(x), col = col)
  }

  if (2L %in% which) {
    data$y <- data$y - predict(x)
    plot(y ~ x, data, col = col, ...)
  }

  invisible(NULL)
}

#' @rdname lspline
#' @export
points.lspline <- function(x, y = NULL, type = 'p', col = NULL, ...) {
  knots <- attr(x, 'knots')
  data  <- x$model
  col   <- rep_len(col %||% seq_along(c(1L, knots)), length(knots) + 1L)
  col   <- (col %||% palette())[findInterval(data$x, knots) + 1L]

  points(data$x, predict(x), col = col, type = type, ...)

  invisible(NULL)
}

#' @rdname lspline
#' @export
lines.lspline <- function(x, y = NULL, type = 'l', col = NULL, ...) {
  knots <- attr(x, 'knots')
  data  <- x$model
  col   <- rep_len(col %||% seq_along(c(1L, knots)), length(knots) + 1L)

  xx <- if (!is.null(knots))
    Map(function(x, y) seq(x, y, length.out = 1000L),
        c(min(sort(data$x)), knots), c(knots, max(sort(data$x))))
  else seq(min(sort(data$x)), max(sort(data$x)), length.out = 1000L)

  pr <- lapply(seq_along(xx), function(ii) {
    pr <- predict(x, lsdata(xx[[ii]], knots))
    lines(xx[[ii]], pr, col = (col %||% palette())[ii], type = type, ...)
  })

  invisible(NULL)
}

#' Permutation t-test
#'
#' Performs paired and two-sample t-tests based on Monte Carlo permutation.
#'
#' @param x,y vectors of numeric data
#' @param alternative a character string specifying the alternative hypothesis,
#'   one of \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}
#' @param paired logical; if \code{TRUE}, \code{x} and \code{y} are assumed
#'   to be paired (\code{NA}s will remove data in pairs); if \code{FALSE},
#'   \code{x} and \code{y} are assumed to be independent samples (\code{NA}s
#'   are removed from each sample independently)
#' @param var.equal logical; if \code{TRUE}, two-sample variances are treated
#'   equally; otherwise, variances are estimated separately for both samples
#'   (default)
#' @param conf.level confidence level in \code{(0, 1)} for the p-value
#' @param midp logical; if \code{TRUE} (default), the mid p-value is used,
#'   i.e., half the conditional probability of the observed statistic plus the
#'   conditional probability of more extreme values
#' @param B an integer specifying the number of permutations
#' @param ... additional arguments passed to or from methods
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} is
#'   a numeric variable giving the data values and \code{rhs} a factor-like
#'   variable with two levels giving the corresponding groups
#' @param data an optional matrix or data frame (or similar: see
#'   \code{\link{model.frame}}) containing the variables in \code{formula};
#'   by default the variables are taken from \code{environment(formula)}
#' 
#' @return
#' An object of class \code{power.htest} with the following elements:
#' 
#' \item{statistic}{the value of the t-statistic}
#' \item{p.value}{the p-value for the test}
#' \item{method}{a character string indicating what type of t-test was performed}
#' \item{data.name}{a character string giving the name(s) of the data}
#' \item{alternative}{a character string describing the alternative hypothesis}
#' \item{B}{the number of permutations}
#' \item{conf.int}{the confidence interval for the p-value}
#' 
#' @seealso
#' \code{\link{t.test}}
#'
#' \code{perm.t.test} from the \pkg{Deducer} package
#'
#' \code{independence_test} and \code{symmetry_test} from the \pkg{coin} package
#'
#' \code{paired.perm.test} and \code{perm.test} from the \pkg{broman} package
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(10, 0, 0.5)
#' y <- rnorm(10, 0.5, 1)
#' dat <- data.frame(value = c(x, y), group = rep(1:2, each = 10))
#'
#' ## two-sample data
#' t.test(x, y)
#'
#' ## equivalent ways to call perm.t.test for two-sample data
#' perm.t.test(x, y)
#' perm.t.test(value ~ group, dat)
#'
#' ## paired data
#' t.test(x - y)
#' t.test(x, y, paired = TRUE)
#'
#' ## equivalent ways to call perm.t.test for paired data
#' perm.t.test(x, y, paired = TRUE)
#' perm.t.test(value ~ group, dat, paired = TRUE)
#' perm.t.test(x - y)
#'
#' @export

perm.t.test <- function(x, ...) {
  UseMethod('perm.t.test')
}

#' @rdname perm.t.test
#' @export
perm.t.test.default <- function(x, y = NULL,
                                alternative = c('two.sided', 'less', 'greater'),
                                paired = FALSE, var.equal = FALSE,
                                conf.level = 0.95, midp = TRUE, B = 10000L, ...) {
  y0 <- y
  if (is.null(y)) {
    y0 <- x
    paired <- TRUE
  }

  data.name <- paste(deparse(substitute(x)), 'and', deparse(substitute(y)))
  alternative <- match.arg(alternative)

  method <- t.test(x, y0, alternative = alternative,
                   var.equal = var.equal, paired = paired)$method
  method <- gsub('(?=t-test)', 'permutation ', method, perl = TRUE)
  method <- sprintf('%s (based on %s replicates)', trimws(method), B)

  if (is.null(y)) {
    method <- gsub('Paired', 'One-sample', method)
    paired <- TRUE
  }

  lx <- length(x)
  ly <- length(y)
  nn <- length(c(x, y))

  if (paired) {
    d0 <- sort(if (is.null(y)) x else x - y)
    m0 <- st <- c(t = mean(d0))
    m1 <- replicate(B, mean(d0 * sign(runif(length(d0), -1, 1))))
  } else {
    d0 <- c(x, y)

    if (var.equal) {
      m0 <- sum(y)
      st <- c('Mean difference' = mean(x) - mean(y))
      m1 <- replicate(B, sum(sample(d0, ly)))
    } else {
      m0 <- st <- c(t = (mean(x) - mean(y)) / sqrt(var(x) / lx + var(y) / ly))
      m1 <- replicate(B, sample(d0, nn))
      sx <- apply(head(m1, lx), 2L, function(x) c(sum(x), var(x)))
      sy <- apply(tail(m1, ly), 2L, function(x) c(sum(x), var(x)))
      m1 <- (sx[1L, ] / lx - sy[1L, ] / ly) / sqrt(sx[2L, ] / lx + sy[2L,] / ly)
    }
  }

  lo <- sum(m1 < m0) / (B + 1)
  up <- sum(m1 > m0) / (B + 1)
  eq <- sum(m1 == m0) / (B + 1)
  mp <- if (isTRUE(midp)) 0.5 else 1

  pv <- switch(
    alternative,
    two.sided = 2 * min(lo, up) + 2 * mp * eq,
    less = lo + mp * eq,
    greater = up + mp * eq
  )

  pv <- pmin(pv, 1)
  ci <- qnorm(conf.level) * c(-1, 1) * sqrt(pv * (1 - pv) / B)
  ci <- structure(sort(pmin(1, pmax(0, pv + ci))), conf.level = conf.level)

  res <- list(
    statistic = st, p.value = pv, method = method, data.name = data.name,
    alternative = alternative, B = B, conf.int = ci
  )

  structure(res, class = 'htest')
}

#' @rdname perm.t.test
#' @export
perm.t.test.formula <- function(formula, data, ...) {
  ## adapted from stats:::t.test.formula
  if (missing(formula) || (length(formula) != 3L) ||
      (length(attr(terms(formula[-2L]), 'term.labels')) != 1L))
    stop('\'formula\' missing or incorrect')

  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())

  data.name <- paste(names(mf), collapse = ' by ')
  names(mf) <- NULL
  response <- attr(attr(mf, 'terms'), 'response')

  g <- factor(mf[[-response]])
  if (nlevels(g) != 2L)
    stop('grouping factor must have exactly 2 levels')

  y <- do.call(
    'perm.t.test',
    c(setNames(split(mf[[response]], g), c('x', 'y')), list(...))
  )
  y$data.name <- data.name

  y
}

#' rfsrc variable selection
#'
#' Drop least relevant variables from a \code{\link[randomForestSRC]{rfsrc}}
#' model with optional diagnostics.
#'
#' @param formula,data a formula and data frame containing the response and
#'   all potential predictor variables
#' @param nvar number of variables desired in final model (if positive) or
#'   the number of variables to drop (if negative)
#' @param depth logical or a numeric value (if \code{NULL}, variables will
#'   be selected via \code{nvar}); if \code{TRUE}, variables will be selected
#'   using first-order depths and the \code{threshold} value returned by
#'   \code{\link[randomForestSRC]{max.subtree}}; alternatively, a numeric
#'   value to be used as the threshold
#' @param verbose,plot logical; if \code{TRUE}, an updated \code{formula}
#'   and/or variable importance figures are printed after each step
#' @param refit logical; if \code{TRUE} (default), model is re-fit after each
#'   variable is dropped; otherwise, all variables to be dropped will be done
#'   in one step which could give significant performance gains for some models
#'   such as survival models (note the final model may be different than if
#'   \code{refit = TRUE})
#' @param ... additional parameters passed to
#'   \code{\link[randomForestSRC]{rfsrc}}
#'
#' @seealso
#' \code{\link{vs.glmnet}}
#'
#' @return
#' The \code{formula} of the final model.
#'
#' @examples
#' set.seed(1)
#' vs.rfsrc(iris)
#' vs.rfsrc(I(Species == 'setosa') ~ ., iris)
#'
#'
#' ## select variables based on first-order depth
#' f <- formula(rev(iris))
#' vs.rfsrc(f, iris, depth = TRUE, verbose = TRUE, plot = FALSE, ntree = 10)
#'
#' ## keep only most relevant 2 variables
#' vs.rfsrc(f, iris, nvar = 2)
#'
#' ## drop 2 least relevant variables
#' vs.rfsrc(f, iris, nvar = -2)
#' vs.rfsrc(f, iris, nvar = -2, refit = FALSE)
#'
#'
#' library('survival')
#' f <- Surv(time, status == 0) ~ rx + sex + age + obstruct + adhere + nodes
#' vs.rfsrc(f, colon, nvar = 4, ntree = 5)
#' # vs.rfsrc(f, colon, nvar = -2, ntree = 5) ## same
#'
#' ## for slower models, refit = FALSE may improve performance
#' vs.rfsrc(f, colon, nvar = 4, ntree = 5, refit = FALSE, plot = TRUE)
#'
#' @export

vs.rfsrc <- function(formula, data, nvar = -1L, depth = NULL,
                     verbose = FALSE, plot = verbose, refit = TRUE, ...) {
  if (is.data.frame(formula)) {
    data <- formula
    formula <- formula(data)
  }

  mf <- model.frame(formula, data)
  yy <- colnames(mf)[1L]
  xx <- colnames(mf)[-1L]

  co <- complete.cases(data[, all.vars(mf)])
  if (any(!co)) {
    message(sum(!co), ' observations removed due to missingness')
    data <- data[co, ]
  }

  if (nvar < 0)
    nvar <- pmax(1L, length(xx) + nvar)

  if (nvar == 0 || length(xx) == nvar)
    return(formula0(formula))

  rf <- randomForestSRC::rfsrc(formula, data = data, importance = TRUE, ...)

  if (verbose)
    print(rf$call)
  if (plot)
    randomForestSRC::plot.rfsrc(rf, verbose = FALSE)

  if (!is.null(depth) & !isFALSE(depth)) {
    ## first and second order depths
    vm <- randomForestSRC::max.subtree(rf, conservative = FALSE)
    vo <- vm$order[order(vm$order[, 1L]), ]
    ## variables with min depth <= threshold
    threshold <- if (isTRUE(depth))
      vm$threshold else depth
    vo <- rownames(vo[vo[, 1L] <= threshold, ])
    if (verbose)
      message('threshold used: ', threshold)
    return(formula0(reformulate(vo, yy)))
  }

  ## remove least relevant variable(s)
  ri <- rf$importance
  if (length(dim(ri)))
    ri <- ri[, 'all']
  ex <- if (refit)
    names(which.min(ri)) else head(names(ri)[order(ri)], -nvar)
  formula <- reformulate(setdiff(xx, ex), yy)

  formula0(Recall(formula, data, nvar, depth, verbose, plot, refit, ...))
}

#' glmnet variable selection
#'
#' Drop least relevant variables from a \code{\link[glmnet]{glmnet}}
#' model with optional diagnostics.
#'
#' @param formula,data a formula and data frame containing the response and
#'   all potential predictor variables
#' @param family a character string or family function for the error
#'   distribution and link, e.g., \code{"gaussian"}, \code{"binomial"},
#'   or \code{"cox"}; see \code{\link[glmnet]{glmnet}}
#' @param alpha elastic net mixing parameter; default penalty is 1 for lasso,
#'   or any value between 0 (ridge penalty) and 1
#' @param ... additional arguments passed to \code{\link[glmnet]{cv.glmnet}}
#'   or further to \code{\link[glmnet]{glmnet}}
#'
#' @seealso
#' \code{\link{vs.rfsrc}}
#'
#' @examples
#' set.seed(1)
#' vs.glmnet(iris, family = 'gaussian', alpha = 1) ## lasso - default
#' vs.glmnet(iris, family = 'gaussian', alpha = 0) ## ridge
#'
#' vs.glmnet(I(Species == 'setosa') ~ ., iris, family = 'binomial')
#'
#' library('survival')
#' f <- Surv(time, status == 0) ~ rx + sex + age + obstruct + adhere + nodes
#' vs.glmnet(f, colon, family = 'cox')
#'
#' @export

vs.glmnet <- function(formula, data, family, alpha = 1, ...) {
  if (is.data.frame(formula)) {
    data <- formula
    formula <- formula(data)
  }

  mf <- model.frame(formula, data)
  mm <- model.matrix(formula, data)
  # colnames(mm) <- make.names(colnames(mm))

  gn <- glmnet::cv.glmnet(
    x = mm[, -1L], y = as.matrix(mf[, 1L]), alpha = alpha, family = family, ...
  )

  co <- coef(gn, s = 'lambda.1se')
  ii <- which(as.numeric(co) != 0)
  rn <- row.names(co)[ii]
  rn <- rn[!grepl('\\(Intercept\\)', rn)]
  if (!length(rn))
    rn <- '1'

  formula0(reformulate(rn, colnames(mf)[1L]))
}

formula0 <- function(x) {
  attr(x, '.Environment') <- .GlobalEnv
  x
}

#' ransch
#'
#' Generate block randomization schedule tables with fixed or random block
#' size.
#'
#' @param n sample size of study or each stratum
#' @param block block size; note if \code{block} is not a factor of \code{n},
#'   \code{n} will be increased to accommodate a full block
#'
#'   for randomly-sized blocks, a vector of potential block sizes; note that
#'   a block size must be a multiple of \code{sum(r)}
#' @param arms names of the treatment arms
#' @param r randomization ratio; see examples
#' @param strata an optional named list of vectors for each stratum
#' @param write a file path to write a directory with csv files for each
#'   randomization table
#'
#' @examples
#' ## no strata, 2-3 treatments with varying randomization ratios
#' ransch(24, 4, 1:2) ## 1:1
#' ransch(24, 6, 1:3) ## 1:1:1
#' ransch(24, 8, 1:3, c(1, 2, 1)) ## 1:2:1
#'
#'
#' ## randomly-sized blocks
#' ransch(24, c(2, 4, 6), 1:2)
#'
#' set.seed(1)
#' r1 <- ransch(24, c(3, 6, 9), 1:3)
#' set.seed(1)
#' r2 <- ransch(24, 1:10, 1:3)
#'
#' ## note that these two are the same since only blocks sized 3, 6, 9
#' ## work for 1:1:1 randomization
#' identical(r1, r2)
#' addmargins(table(r1[[1]][, -1]))
#'
#'
#' ## one two-level stratum
#' ransch(24, 4, 1:2, strata = list(Age = c('<65', '>=65')))
#'
#' ## multiple strata
#' strata <- list(Site = LETTERS[1:3], Age = c('<65', '>=65'))
#' ransch(24, 4, 1:2, strata = strata)
#'
#'
#' ## tables for printing
#' ranschtbl(24, 4, c('Pbo', 'Trt'))
#' ranschtbl(24, 4, c('Pbo', 'Trt'), c(1, 3), strata)
#'
#' \dontrun{
#' ranschtbl(24, 4, c('Pbo', 'Trt'), strata = strata, write = '~/desktop')
#' }
#'
#' @export

ransch <- function(n, block, arms, r = rep_len(1L, length(arms)),
                   strata = NULL) {
  if (!is.null(strata)) {
    if (is.null(names(strata)))
      names(strata) <- paste0('Stratum', seq_along(strata))

    strata <- Map(paste, names(strata), strata)
    strata <- apply(expand.grid(strata), 1L, toString)
  }

  res <- replicate(pmax(1L, length(strata)), simplify = FALSE, {
    ransch_(n, block, arms, rep_len(r, length(arms)))
  })

  setNames(res, strata %||% 'ransch')
}

#' @rdname ransch
#' @export
ranschtbl <- function(n, block, arms, r = rep_len(1L, length(arms)),
                      strata = NULL, write = NULL) {
  res <- ransch(n, block, arms, r, strata)

  res[] <- lapply(seq_along(res), function(ii) {
    within(res[[ii]], {
      Stratum <- names(res)[ii]
      Name <- ID <- Date <- NA
    })
  })

  if (is.null(names(res)))
    names(res) <- 'ransch'

  if (is.character(write) && dir.exists(write)) {
    path <- sprintf('%s/ransch-%s', write, format(Sys.time(), '%Y%m%d%H%M'))
    dir.create(path, showWarnings = FALSE, recursive = TRUE)

    for (ii in seq_along(res))
      write.csv(res[[ii]], sprintf('%s/%s.csv', path, names(res)[ii]),
                na = '', row.names = FALSE)

    invisible(res)
  } else res
}

ransch_ <- function(n, block, arms, r) {
  ## table(ransch_(12, 6, c('Pbo', 'Trt'), c(1, 1))[, -1])
  ## table(ransch_(12, 1:4, c('Pbo', 'Trt'), c(1, 1))[, -1])
  stopifnot(length(arms) == length(r))

  sample <- function(x, ...) {
    x[sample.int(length(x), ...)]
  }
  rblock <- function(b, arms, r) {
    arms <- rep_len(rep(arms, r), b)
    sample(arms)
  }

  block <- block[block %% sum(r) == 0L]
  block <- sample(block, n, replace = TRUE)
  idx <- cumsum(block) < n
  block <- block[c(which(idx), sum(idx) + 1L)]


  data.frame(
    Number = seq.int(sum(block)),
    Block = rep(seq_along(block), block),
    Assignment = unlist(lapply(block, rblock, arms = arms, r = r))
  )
}

#' Two-stage trials
#'
#' Calculate confidence intervals, estimators, and p-value from a two-stage
#' trial.
#'
#' @param r,r1 total observed responses and maximum number of responses in
#'   first stage to declare treatment inactive
#' @param n1,n2 sample size of the first and second stage
#' @param p0 (optional) null hypothesis
#' @param conf confidence level
#' @param dp affects the ordering of outcomes within the sample space, see
#'   \code{\link[desmon]{twocon}}
#'
#' @seealso
#' \code{\link[desmon]{twocon}}; \code{\link[clinfun]{twostage.inference}}
#'
#' @examples
#' rawr:::simon(0.1, 0.3)
#'
#' # $designs$`p0 = 0.1, pa = 0.3`
#' #      n1 r1 n2 r2 Pstop1.H0       size      type2 E.tot.n.H0
#' # [1,] 12  1 23  5 0.6590023 0.09771828 0.09855051   19.84295
#' #
#' # ...
#' #
#' # $description
#' # [1] "n1, n2 = cases 1st stage and additional # in 2nd"
#' # [2] "r1, r2 = max # responses 1st stage and total to declare trt inactive"
#'
#' twostg.test(5, 1, 12, 23)
#' twostg.test(5, 1, 12, 23, 0.1)
#' twostg.test(8, 1, 12, 23, 0.1)$p.value
#'
#' @export

twostg.test <- function(r, r1, n1, n2, p0 = NULL, conf = 0.95, dp = 1) {
  tc <- twocon(n1, n2, r1, r, conf, dp)
  ti <- if (!is.null(p0))
    twostage.inference(r, r1, n1, n1 + n2, p0, 1 - conf) else NULL
  list(confint = tc, inference = ti, p.value = ti[[2L]])
}

twostage.inference <- function(x, r1, n1, n, pu, alpha = 0.05) {
  # clinfun::twostage.inference
  out <- list()
  n2 <- n - n1
  xlo <- max(r1 + 1, x - n2)
  xhi <- min(x, n1)
  x1 <- xlo:xhi
  pumvue <- if (x <= r1)
    x/n1
  else sum(choose(n1 - 1, x1 - 1) *
             choose(n2, x - x1)) / sum(choose(n1, x1) * choose(n2, x - x1))
  x1 <- (r1 + 1):n1
  p.value <- if (x <= r1)
    1 - pbinom(x - 1, n1, pu)
  else sum(dbinom(x1, n1, pu) * (1 - pbinom(x - x1 - 1, n2, pu)))
  pp <- seq(0, 1, by = 0.01)
  pval <- rep(0, 101)
  if (x <= r1) {
    pval <- 1 - pbinom(x - 1, n1, pp)
  }
  else {
    x2 <- x - x1 - 1
    pval <- sapply(pp, function(p, x1, x2, n1, n2) {
      sum(dbinom(x1, n1, p) * (1 - pbinom(x2, n2, p)))
    }, x1, x2, n1, n2)
  }
  ii <- which(pval > alpha)[1] - 1
  jj <- which(pval >= 1 - alpha)[1] - 1
  pp0 <- pp[ii] + pp/100
  pval <- rep(0, 101)
  if (x <= r1) {
    pval <- 1 - pbinom(x - 1, n1, pp0)
  }
  else {
    x2 <- x - x1 - 1
    pval <- sapply(pp0, function(p, x1, x2, n1, n2) {
      sum(dbinom(x1, n1, p) * (1 - pbinom(x2, n2, p)))
    }, x1, x2, n1, n2)
  }
  LCL <- pp0[which(pval > alpha)[1]]
  pp0 <- pp[jj] + pp/100
  pval <- rep(0, 101)
  if (x <= r1) {
    pval <- 1 - pbinom(x - 1, n1, pp0)
  }
  else {
    x2 <- x - x1 - 1
    pval <- sapply(pp0, function(p, x1, x2, n1, n2) {
      sum(dbinom(x1, n1, p) * (1 - pbinom(x2, n2, p)))
    }, x1, x2, n1, n2)
  }
  UCL <- pp0[which(pval >= 1 - alpha)[1] - 1]
  out <- c(pumvue, p.value, LCL, UCL)
  names(out) <- c(
    'pumvue', 'p.value', paste(100 * alpha, '% LCL', sep = ''),
    paste(100 * (1 - alpha), '% UCL', sep = '')
  )
  out
}

#' Sample size calculation for correlation
#'
#' Compute the sample size for a given correlation coefficient, alpha level,
#' and confidence interval width. Alternatively, solve for the coefficient,
#' interval width, or alpha level given fixed parameters. \code{cor.ci}
#' calculated the confidence interval for a fixed correlation coefficient
#' and sample size.
#'
#' @param rho correlation coefficient
#' @param n sample size
#' @param width the width of the \code{1 - alpha} confidence interval
#' @param alpha type I error probability
#' @param two.sided logical; if \code{TRUE}, a two-sided \code{alpha} is used
#' @param method the method used to calculate the standard deviation, one of
#'   \code{"bonett"} or \code{"fieller"} (can be (unambiguously) abbreviated)
#' @param tol numerical tolerance used in root finding, the default providing
#'   (at least) four significant digits
#'
#' @references
#' \url{https://www.researchgate.net/profile/Douglas_Bonett/publication/279926406_Sample_size_requirements_for_estimating_Pearson_Kendall_and_Spearman_correlations/links/575de23908aed88462166f2e/Sample-size-requirements-for-estimating-Pearson-Kendall-and-Spearman-correlations.pdf}
#'
#' \url{http://solarmuri.ssl.berkeley.edu/~schuck/public/manuscripts/Fieller1.pdf}
#'
#' \url{https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/PASS/Confidence_Intervals_for_Spearmans_Rank_Correlation.pdf}
#'
#' \url{https://stats.stackexchange.com/a/18904}
#'
#' @examples
#' ## table 1, bonett (spearman) -- solving for n
#' args <- data.frame(
#'   rho = 0.9,
#'   width = rep(1:3, each = 2) / 10,
#'   alpha = c(0.05, 0.01)
#' )
#'
#' apply(args, 1L, function(x)
#'   ceiling(cor.n(x['rho'], NULL, x['width'], x['alpha'])))
#'
#'
#' ## table 1, bonett (spearman)
#' cor.ci(0.1, 290, 0.01)
#' ceiling(cor.n(0.1, NULL, 0.3, 0.01))
#'
#' cor.ci(0.5, 111, 0.05)
#' ceiling(cor.n(0.5, NULL, 0.3, 0.05))
#'
#' @export

cor.n <- function(rho = NULL, n = NULL, width = NULL, alpha = 0.05,
                  two.sided = TRUE, method = c('bonett', 'fieller'),
                  tol = .Machine$double.eps ^ 0.25) {
  if (sum(sapply(list(rho, n, width, alpha), is.null)) != 1)
    stop('exactly one of \'rho\', \'n\', \'width\', or \'alpha\' must be NULL')

  b <- quote(cor.ci(rho, n, alpha, two.sided, 'bonett')$width)
  f <- quote(cor.ci(rho, n, alpha, two.sided, 'fieller')$width)

  if (is.null(width)) {
    b <- eval(b)
    f <- eval(f)
  } else if (is.null(n)) {
    b <- uniroot(function(n) eval(b) - width, c(5, 1e7), tol = tol)$root
    f <- uniroot(function(n) eval(f) - width, c(5, 1e7), tol = tol)$root
  } else if (is.null(rho)) {
    b <- uniroot(function(rho) eval(b) - width, c(0, 1), tol = tol)$root
    f <- uniroot(function(rho) eval(f) - width, c(0, 1), tol = tol)$root
  } else if (is.null(alpha)) {
    b <- uniroot(function(alpha) eval(b) - width, c(0, 1), tol = tol)$root
    f <- uniroot(function(alpha) eval(f) - width, c(0, 1), tol = tol)$root
  }

  c(bonett = b, fieller = f)[match.arg(method, several.ok = TRUE)]
}

#' @rdname cor.n
#' @export
cor.ci <- function(rho, n, alpha = 0.05, two.sided = TRUE,
                   method = c('bonett', 'fieller')) {
  method <- match.arg(method)
  sd <- switch(
    method,
    bonett = sqrt((1 + rho ^ 2 / 2) / (n - 3)),
    fieller = sqrt(1.06 / (n - 3))
  )
  pr <- 1 - alpha / (two.sided + 1L)
  ci <- tanh(atanh(rho) + c(-1, 1) * qnorm(pr) * sd)

  data.frame(
    rho = rho, lower = ci[1L], upper = ci[2L], width = diff(ci),
    conf.int = 1 - alpha, sides = two.sided + 1L, method = method
  )
}

#' Two-arm binomial non-inferiority trials
#'
#' Calculate the required sample size or power for a two-arm non-inferiority
#' design with binomial outcome. Exactly one of \code{power} or \code{n}
#' should be \code{NULL} to solve for this value given the other parameters.
#'
#' @param margin the largest acceptable difference in success rates between
#'   the standard (\code{p1}) and experimental (\code{p2}) arms that would be
#'   consistent with non-inferiority
#' @param p1,p2 true success rates for the standard and experimental arms,
#'   respectively
#' @param alpha type I error (one-sided), i.e., the \code{1 - 2 * alpha}%
#'   confidence interval around the difference between the rates
#' @param power the desired power level to rule out the null of inferiority
#' @param n the total sample size of both arms
#' @param p the proportion of \code{n} on the experimental arm, \code{p2}
#'
#' @seealso
#' \url{https://stattools.crab.org/R/Binomial_Non_Inferiority.html}
#'
#' \url{http://www.powerandsamplesize.com/Calculators/Compare-2-Proportions/2-Sample-Non-Inferiority-or-Superiority}
#'
#' @references
#' Kopecky K and Green S (2012). Noninferiority trials. In: Handbook of
#' Statistics in Clinical Oncology. Crowley J and Hoering A, eds. CRC Press,
#' Boca Raton, FL USA.
#'
#' @return
#' The a numeric vector with the total sample size (\code{n}), sample size
#' for the standard (\code{n1}) and experimental (\code{n2}) arms, and power
#' (\code{power}).
#'
#' @examples
#' bni(0.1, 0.65, 0.85, power = 0.8)
#' bni(0.1, 0.65, 0.85, n = 50)
#' bni(0.1, 0.65, 0.85, n = 50, p = 2 / 3) ## 1:2 randomization
#'
#' @export

bni <- function(margin, p1, p2 = p1, alpha = 0.05, power = NULL,
                n = NULL, p = 0.5) {
  stopifnot(margin > 0)
  if ((is.null(power) + is.null(n)) != 1)
    stop('exactly one of \'power\' and \'n\' should be NULL')

  if (is.null(n)) {
    za <- qnorm(alpha, lower.tail = FALSE)
    zb <- qnorm(power)
    n <- ((za + zb) / (margin + p2 - p1)) ^ 2 *
      (((p2 * (1 - p2)) / p) + ((p1 * (1 - p1)) / (1 - p)))
  } else {
    k <- (1 - p) / p
    p2_n <- n * p
    z <- (p1 - p2 - margin) /
      sqrt(p1 * (1 - p1) / p2_n / k + p2 * (1 - p2) / p2_n)
    power <- pnorm(z - qnorm(1 - alpha)) + pnorm(-z - qnorm(1 - alpha))
  }

  c(n = n, n1 = n * (1 - p), n2 = n * p, power = power)
}

#' Pick-the-winner design
#' 
#' Calculate sample size of Simon's pick winner design or determine the
#' probability of alternative hypothesis to obtain target power.
#' 
#' These calculations use method proposed by R. Simon for pick-the-winner design
#' for randomized phase II clinical trials.
#' 
#' Exactly one of the parameters \code{n}, \code{power}, and \code{p1} must
#' be \code{NULL}.
#' 
#' This design is not a hypothesis test, nor is there a formal comparison
#' between arms. Therefore, the type I error rate does not apply.
#' 
#' @param n sample size per arm
#' @param k number of arms
#' @param p0,p1 null and alternative response rates, respectively
#' @param power probability of choosing the winning arm
#' @param tol the desired accuracy (convergence tolerance); see
#'   \code{\link[stats]{uniroot}}
#' 
#' @return
#' An object of class \code{power.htest} with the following elements:
#' 
#' \item{n}{sample size per arm}
#' \item{arm}{number of arms}
#' \item{n.total}{total sample size for the study}
#' 
#' @references
#' Adapted from \code{power.ctepd::n.pick.winner}, Bingshu E. Chen
#' 
#' Simon, R., R.E. Wittes, S.S. Ellenberg. Randomized phase II clinical
#' trials. \emph{Cancer Treat Rep}. \strong{69}:1375-1381, 1985.
#' 
#' @seealso
#' \code{power.ctepd::n.pick.winner}; \code{\link[clinfun]{pselect}};
#' \code{\link[desmon]{pickwin}};
#' 
#' @examples
#' pickwin(20, 2, 0.25, 0.5)
#' pickwin(NULL, 2, 0.25, 0.5, 0.95)
#' 
#' pickwin(20, 3, 0.25, 0.5)$power
#' pickwin(20, 4, 0.25, 0.5)$power
#' 
#' @export

pickwin <- function(n = NULL, k = 2L, p0 = NULL, p1 = NULL, power = NULL,
                    tol = .Machine$double.eps ^ 0.25) {
  stopifnot(
    'exactly one of n, p1, and power must be NULL' =
      (is.null(n) + is.null(p1) + is.null(power)) == 1L,
    k >= 2L,
    p0 > 0,
    p0 < 1,
    is.null(p0) | p0 < p1,
    is.null(p0) | p1 < 1
  )
  
  ## prob of picking the winning arm
  p.body <- quote({
    n <- ceiling(n)
    x <- seq(0, n)
    ## cdf - cumulative prob of x or fewer responses
    Bi0 <- pbinom(x, n, p0)
    Bi1 <- pbinom(x, n, p1)
    ## pdf - prob of exactly x responses
    bi0 <- dbinom(x, n, p0)
    bi1 <- dbinom(x, n, p1)
    
    ## prob that max response is i in (k - 1) null arms
    Bix <- c(0, Bi0)[seq.int(n + 1L)]
    fx  <- Bi0 ^ (k - 1L) - Bix ^ (k - 1L)
    
    ## for ties, best arm was selected when being tied with 1 or more null arms
    gj <- 0
    for (j in seq.int(k - 1L)) {
      ckj <- choose(k - 1L, j)
      gj  <- gj + ckj * bi0 ^ j * Bix ^ (k - 1L - j) / (j + 1L)
    }  
    pb <- sum(fx * (1 - Bi1)) + sum(bi1 * gj)
  })
  
  if (is.null(n)) {
    n <- uniroot(function(n) eval(p.body) - power, c(5, 500), tol = tol)$root
    n <- n + 1
  }
  if (is.null(p1)) {
    p1 <- uniroot(function(p1) eval(p.body) - power, c(p0 - 0.001, 0.99), tol = tol)$root
  }
  
  power <- eval(p.body)
  
  structure(
    list(
      n = n, arms = k, n.total = n * k, p0 = p0, p1 = p1, power = power,
      method = 'Sample size for Simon\'s pick-the-winner'
    ), class = 'power.htest'
  )
}
