### statistical functions
# bincon, bintest, dlt_table, pr_table, power_cv, simon2, moods_test, fakeglm,
# gcd, install.bioc, lm.beta, cuzick.test, cuzick.test.default,
# cuzick.test.formula, jt.test, hl_est, rcor, rsum, kw.test, kw.test.default,
# kw.test.formula, ca.test, ca.test.default, ca.test.formula, lspline,
# winsorize, perm.t.test, perm.t.test.default, perm.t.test.formula
# 
# S3 methods:
# cuzick.test, kw.test, ca.test, perm.t.test
# 
# desmon (unexported):
# twocon, simon, bin1samp
# 
# rpart_utils:
# rpart_parent, rpart_subset, rpart_nodes
# 
# unexported:
# cuzick.test.stat, cuzick.test.pvalue, jt.test.stat, combn_fun, rcor1, rcorn,
# sim.test.pvalue
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
#' a vector of length two giving the max number of responses that can be
#' observed in the first stage without continuing and the total number of
#' responses observed
#' @param n number of observations (trials); if \code{method = 'two-stage'},
#' a vector of length two giving the number of cases entered during the first
#' stage and the number of additional cases to be entered during the second
#' stage
#' @param alpha type-I error probability
#' @param digits integer value specifying number of decimal places
#' @param method character strings specifying which method to use; can be
#' (unambiguously) abbreviated; see details
#' @param dp numeric value affecting the ordering of sample space in two-stage
#' designs when \code{method = "two-stage"}
#' 
#' \code{dp = 0} will give the Atkinson and Brown procedure, and \code{dp = 1}
#' (default) will order based on MLE; values such as \code{dp = 0.5} can
#' also be used; see details
#' 
#' @return
#' A matrix containing the computed interval(s) and their widths.
#' 
#' @author
#' Rollin Brant, Frank Harrell, and Brad Biggerstaff; modifications by Robert
#' Redd including support for two-stage designs
#' 
#' @references Agresti, A. and B.A. Coull. Approximate is better than "exact"
#' for interval extimation of binomial proportions. \emph{American
#' Statistician}. \strong{59}:119-126, 1998.
#' @references Brown, L.D., T.T. Cai, and A. Das Gupta. Inverval estimation
#' for a binomial proportion (with discussion). \emph{Statistical Science}. 
#' \strong{16}:101-133, 2001.
#' @references Newcombe, R.G. Logit confidence intervals and the inverse sinh
#' transformation, \emph{American Statistician}. \strong{55}:200-2, 2001.
#' @references Atkinson E.N. and B.W. Brown. \emph{Biometrics}.
#' \strong{41(3)}: 741-4, 1985.
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
#' \dontrun{
#' ## ?desmon::twocon
#' ## equivalent to
#' desmon::twocon(14, 18, 3, 4, dp = 0)
#' desmon::twocon(14, 18, 3, 4, dp = 1)
#' }
#' 
#' @export

bincon <- function(r, n, alpha = 0.05, digits = getOption('digits'),
                   method = c('exact', 'wilson', 'asymptotic', 'all',
                              'two-stage'),
                   dp = 1) {
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
    return(`rownames<-`(mat, NULL))
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
      cl[1] <- -log(1 - alpha) / n
    if (r == (n - 1))
      cl[2] <- 1 + log(1 - alpha) / n
    asymp.lcl <- r / n - qnorm(1 - alpha / 2) *
      sqrt(((r / n) * (1 - r / n)) / n)
    asymp.ucl <- r / n + qnorm(1 - alpha / 2) *
      sqrt(((r / n) * (1 - r / n)) / n)
    res <- rbind(c(ll, ul), cl, c(asymp.lcl, asymp.ucl))
    res <- cbind(rep(r / n, 3), res)
    
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
  
  if (method == 'all' & lr == 1L & ln == 1L) {
    mat <- bc(r, n, alpha, method)
    mat <- cbind(mat, mat[, 3L] - mat[, 2L])
    dimnames(mat) <- list(c('Exact', 'Wilson', 'Asymptotic'),
                          c('PointEst', 'Lower', 'Upper','Width'))
    mat[, 2:4] <- round(mat[, 2:4], digits = digits)
    
    return(cbind(Responses = r, Trials = n, mat))
  }
  
  mat <- matrix(ncol = 3L, nrow = lr)
  for (i in seq.int(lr))
    mat[i, ] <- bc(r[i], n[i], alpha = alpha, method = method)
  mat <- `colnames<-`(cbind(mat, mat[, 3L] - mat[, 2L]),
                      c('PointEst', 'Lower', 'Upper', 'Width'))
  mat[, 2:4] <- round(mat[, 2:4], digits = digits)
  
  cbind(Responses = r, Trials = n, mat)
}

twocon <- function(n1, n2, r1, r, conf = 0.95, dp = 1) {
  ## desmon::twocon
  # 
  # n1	  Number of cases entered during the first stage
  # n2	  Number of additional cases to be entered during the second stage
  # r1	  max number of responses that can be observed in the first stage
  #       without continuing
  # r	    total number responses observed
  # conf	two-sided confidence level (proportion) for the confidence interval
  # dp	  Affects the ordering of outcomes within the sample space
  
  if (n1 < 1 | n2 < 1 | r1 < 0 | r1 > n1 | r < 0 | r > n2 + 
      n1 | conf <= 0 | conf >= 1) 
    stop('invalid arguments')
  alpha <- (1 - conf) / 2
  x1 <- 0:n1
  x2 <- 0:n2
  u1 <- c(outer(x1[-(1:(r1 + 1))], x2, `+`))
  dbin2 <- function(p1, x1, x2, u1, n1, n2, r1) {
    w1 <- dbinom(x1, n1, p1)
    w3 <- dbinom(x2, n2, p1)
    u2 <- c(outer(w1[-(1:(r1 + 1))], w3))
    c(w1[1:(r1 + 1)], tapply(u2, u1, sum))
  }
  n <- n1 + n2
  mle <- if (r > r1) 
    r / n else r / n1
  mm <- c((0:r1)/n1, (r1 + 1):n / n)
  ff <- function(p, x1, x2, u1, n1, n2, r1, mm, dbin2, mle)
    sum(dbin2(p, x1, x2, u1, n1, n2, r1) * mm) - mle
  pm <- if (r <= 0)
    0
  else if (r >= n) 
    1
  else uniroot(ff, c(1e-08, 1 - 1e-08), x1 = x1, x2 = x2, u1 = u1,
               n1 = n1, n2 = n2, r1 = r1, mm = mm, dbin2 = dbin2, mle = mle)$root
  if (r1 >= r) {
    ube <- r / n1
  } else {
    aa <- dhyper((r1 + 1):r, n1, n2, r)
    ube <- sum(((r1 + 1):r) * aa)/(n1 * sum(aa))
  }
  mm <- if (r > r1) 
    c((n / n1) ^ dp * (0:r1), (r1 + 1):n)
  else c(0:r1, (n1 / n) ^ dp * ((r1 + 1):n))
  s1 <- mm >= r
  s2 <- mm <= r
  ff2 <- function(p, x1, x2, u1, n1, n2, r1, s, dbin2, alpha)
    sum(dbin2(p, x1, x2, u1, n1, n2, r1)[s]) - alpha
  pl <- if (r <= 0) 
    0
  else uniroot(ff2, c(1e-08, 1 - 1e-08), x1 = x1, x2 = x2, 
               u1 = u1, n1 = n1, n2 = n2, r1 = r1, s = s1, dbin2 = dbin2, 
               alpha = alpha)$root
  pu <- if (r >= n) 
    1
  else uniroot(ff2, c(1e-08, 1 - 1e-08), x1 = x1, x2 = x2, 
               u1 = u1, n1 = n1, n2 = n2, r1 = r1, s = s2, dbin2 = dbin2, 
               alpha = alpha)$root
  
  c(lower = pl, upper = pu, bcmle = pm, mle = mle, unbiased = ube)
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
#' \code{r = n.max} (default); useful if the maximum number of responses that
#' may occur is known
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
#' based on exact tests with actual error rates by trading-off their nominal
#' levels of significance and power. \emph{British J of Cancer} (2012) 107,
#' 1801-9.
#' 
#' @seealso
#' \code{\link[clinfun]{ph2single}}; \code{desmon::bin1samp}; bintest,
#' \href{https://github.com/raredd/sascros/blob/master/bintest.sas}{SAS macro}
#' 
#' @examples
#' bintest(p0low = 0.2, p1low = 0.5, n.max = 25)
#' ## compare
#' rawr:::bin1samp(0.2, 0.5)
#' 
#' ## example in sas macro
#' bintest(0.1, 0.15, 0.2, 0.2, n.max = 80, alpha = 0.08, beta = 0.24)
#' 
#' @export

bintest <- function (p0low, p0high = p0low, p1low, p1high = p1low, n.max,
                     r = n.max, alpha = 0.1, beta = 0.1) {
  stopifnot(
    alpha %inside% c(0, 1),
    beta %inside% c(0, 1),
    r > 0
  )
  
  mat <- expand.grid(
    seq(p0low, p0high, by = .01),
    seq(p1low, p1high, .01),
    seq.int(n.max),
    seq.int(r)
  )
  
  ## require > r responders out of n and remove where p1 <= p0 or r >= n
  mat <- `colnames<-`(as.matrix(within(mat, r2 <- mat[, 4L] + 1L)),
                      c('p0', 'p1', 'n', 'r', 'r2'))
  mat <- mat[!(mat[, 2L] <= mat[, 1L] | mat[, 4L] >= mat[, 3L]), ]
  
  ## Pr(<r2 responders|p0 true): pbinom(r2, n, p0)
  ## Pr(>r2 responders for p0): type-I error = 1 - y
  y <- pbinom(mat[, 5L], mat[, 3L], mat[, 1L])
  
  ## Pr(<r2 responders | p1 true): pbinom(r2, n, p1): type-II error
  ## Pr(>r2 responders for p1): 1 - type-II error
  z <- pbinom(mat[, 5L], mat[, 3L], mat[, 2L])
  
  ## signal: p1 - p0
  signal <- mat[, 2L] - mat[, 1L]
  
  mat <- cbind(mat, y, type1 = 1 - y, z, power = 1 - z, signal)
  alpha <- rep(alpha, nrow(mat))
  beta  <- rep(beta, nrow(mat))
  
  ## keep ones that fit type-I/II error constraints
  mat <- mat[mat[, 9L] > (1 - beta) & mat[, 7L] < alpha, , drop = FALSE]
  mat <- unique(as.data.frame(mat[, c(1:3, 5L, 7L, 9:10), drop = FALSE]))
  mat <- mat[order(mat$n), ]
  
  list(
    designs = `rownames<-`(as.matrix(mat), NULL),
    call = match.call(),
    description = c(
      'n = overall sample size',
      'r2 = minimum number of responders required to reject p0',
      'signal = difference in null and alternative hypotheses')
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
#' \code{NULL}, then no rounding is done
#' @param n,crit sample size and critical number of events, respectively; note
#' that the probabilities will always be calculated as weakly less or greater
#' than \code{crit}, i.e., \code{Pr(>= crit)} or \code{Pr(<= crit)} depending
#' if \code{greater} is \code{TRUE} or \code{FALSE}
#' @param greater logical; the direction of \code{crit}: if \code{FALSE},
#' the probabilities are calculated for \code{crit} \emph{or fewer} events;
#' if \code{TRUE}, the probabilities are calculated for \code{crit} \emph{or
#' greater} events
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

#' Power calculations for coefficient of variation
#' 
#' Compute power of one- and two-sample t-test using ratio of means and
#' coefficient of variation. Alternatively, determine the other parameters
#' to obtain a target power.
#' 
#' Exactly one of \code{n}, \code{f}, \code{cv}, \code{sig.level}, and
#' \code{power} must be \code{NULL}.
#' 
#' @param n number of observations (per group)
#' @param f ratio of means (>1)
#' @param cv coefficient of variation
#' @param sig.level significance level (type-I error probability)
#' @param power power of test (1 - type-II error probability)
#' @param type type of t test, \code{"two-sample"} or \code{"one-sample"}/
#' \code{"paired"}
#' @param alternative one- or two-sided test
#' @param distribution underlying distribution assumption
#' 
#' @return
#' Object of class \code{power.htest}, a list of the arguments (including
#' the computed) augmented with method and note elements.
#' 
#' @note
#' \code{\link{uniroot}} is used to solve power equation for unknowns, so one
#' may see errors from it, notably about inability to bracket the root when
#' invalid arguments are given.
#' 
#' @references
#' Van Belle, G., Martin, D. Sample size as a function of coefficient of
#' variation and ratio of means. Am Statistician, Vol. 47, No. 3 (Aug., 1993),
#' pp. 165-7.
#' @references
#' Martin, D.C., and van Belle, G. Approximations for Power and Sample Size
#' for Student's t-Test. Technical Report 125 (1991), University of Washington,
#' Dept. of Biostatistics.
#' 
#' @examples
#' power_cv(n = NULL, 1.25, 0.2, 0.05, 0.8, distribution = 'normal')
#' power_cv(13, 1.25, 0.2, 0.05, power = NULL, distribution = 't')
#' power_cv(13, 1.25, 0.2, 0.05, power = NULL, distribution = 'log.normal')
#' 
#' @export

power_cv <- function(n = NULL, f = NULL, cv = NULL,
                     sig.level = NULL, power = NULL,
                     type = c('two.sample', 'one.sample', 'paired'),
                     alternative = c('two.sided', 'less', 'greater'),
                     distribution = c('t', 'normal', 'log.normal')) {
  if (sum(sapply(list(n, f, cv, sig.level, power), is.null)) != 1)
    stop('exactly one of n, f, cv, power, sig.level must be NULL')
  if (!is.null(sig.level) && !is.numeric(sig.level) || 
      any(0 > sig.level | sig.level > 1))
    stop('\'sig.level\' must be numeric in [0, 1]')
  if (!is.null(power) && !is.numeric(power) || any(0 > power | power > 1))
    stop('\'power\' must be numeric in [0, 1]')
  if (f < 1) {
    warning('ratio of means must be such that mu1/mu0 > 1: 1/f used')
    f <- 1 / f
  }
  
  type <- match.arg(type)
  alternative <- match.arg(alternative)
  dist <- match.arg(distribution)
  if (dist == 'log.normal' && !(sig.level %in% c(0.01, 0.05)))
    stop('cannot use desired significance level: ',
         'use 5% or 10% type-I error probability')
  
  tsample <- switch(type, one.sample = 1L, two.sample = 2L, paired = 1L)
  ttside  <- switch(alternative, less = 1L, two.sided = 2L, greater = 3L)
  tside   <- switch(alternative, less = 1L, two.sided = 2L, greater = 1L)
  
  ## assuming underlying t distribution
  # one-sided, less
  if (ttside == 1L) {
    p.body <- quote({
      df <- (n - 1) * tsample
      qt <- qt(sig.level/tside, df, lower.tail = TRUE)
      pt(-(-qt + (sqrt(n) * (f - 1)) / (cv * (f ^ 2 + 1) ^ 0.5)), df,
         lower.tail = TRUE)
    })
  }
  
  # two-sided
  if (ttside == 2L) {
    p.body <- quote({
      df <- (n - 1) * tsample
      qt <- qt(sig.level / tside, df, lower.tail = FALSE)
      pt(-qt + (sqrt(n) * (f - 1)) / (cv * (f ^ 2 + 1) ^ 0.5), df,
         lower.tail = TRUE) / 2 +
        pt(-(-qt + (sqrt(n) * (f - 1)) / (cv * (f ^ 2 + 1) ^ 0.5)), df,
           lower.tail = FALSE) / 2
    })
  }
  
  # one-sided, greater
  if (ttside == 3L) {
    p.body <- quote({
      df <- (n - 1) * tsample
      qt <- qt(sig.level / tside, df, lower.tail = FALSE)
      pt(-(-qt + (sqrt(n) * (f - 1)) / (cv * (f ^ 2 + 1) ^ 0.5)), df,
         lower.tail = FALSE)
    })
  }
  
  ## assuming underlying normal distribution
  if (dist == 'normal') {
    p.body <- quote({
      qt <- qnorm(sig.level / tside, lower.tail = FALSE)
      pnorm(-qt + (sqrt(n) * (f - 1)) / (cv * (f ^ 2 + 1) ^ 0.5),
            lower.tail = TRUE)
    })
  }
  
  ## assuming underlying lognormal distribution with unknown variance
  if (dist == 'log.normal') {
    mat <- matrix(
      c(0.005, -2.57583, -2.203837,  0.6699734, -0.0524065, -0.0059258,
        0.010, -2.32635, -1.821394,  0.5380802,  0.0181774, -0.0584748,
        0.025, -1.95996, -1.145521,  0.2370261,  0.0392020, -0.0670915,
        0.050, -1.64485, -0.8455414, 0.1745865,  0.0774911, -0.0865455),
      ncol = 6L, byrow = TRUE,
      dimnames = list(NULL, c('alpha', 'z', 'a', 'b', 'c', 'd'))
    )
    
    alpha <- which(mat[, 1L] %in% sig.level)
    
    p.body <- quote({
      delta <- ifelse(cv < 0.5, sqrt(n / 2) * log(f) / cv,
                      sqrt(n / 2) * log(f) / sqrt(log(cv ^ 2 + 1)))
      v <- 2 * n - 2
      pnorm(mat[alpha - (tside == 2L), 'z'] + delta *
              (1 + mat[alpha, 'a'] / v + mat[alpha, 'b'] / (v - 1) +
                 delta * (mat[alpha, 'c'] / v + mat[alpha, 'd'] / (v - 1))))
    })
  }
  
  ## calculate missing parameter
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(n))
    n <- ifelse(dist == 'log.normal', ceiling(uniroot(function(n)
      eval(p.body) - power, c(2, 1e+07))$root), uniroot(function(n)
        eval(p.body) - power, c(2, 1e+07))$root)
  else if (is.null(cv)) {
    cv <- if (ttside == 2L)
      uniroot(function(cv) eval(p.body) - power, f * c(1e-07, 10))$root
    else if (ttside == 1L)
      uniroot(function(cv) eval(p.body) - power, f * c(-10, 5))$root
    else if (ttside == 3L)
      uniroot(function(cv) eval(p.body) - power, f * c(-5, 10))$root
  } else if (is.null(f)) {
    f <- if (ttside == 2L)
      uniroot(function(f) eval(p.body) - power, cv * c(1e-07, 10))$root
    else if (ttside == 1L)
      uniroot(function(f) eval(p.body) - power, cv * c(-10, 5))$root
    else if (ttside == 3L)
      uniroot(function(f) eval(p.body) - power, cv * c(-5, 10))$root
  } else if (is.null(sig.level))
    sig.level <- uniroot(function(sig.level) eval(p.body) - power,
                         c(1e-10, 1 - 1e-10))$root
  else stop('internal error - solve for null value')
  
  NOTE <- switch(
    type,
    paired = paste('n is number of *pairs*, cv is coefficient of',
                   'variation of *differences* within pairs'),
    two.sample = 'n is number in *each* group',
    one.sample = NULL
  )
  METHOD <- paste(
    switch(type, one.sample = 'One-sample', two.sample = 'Two-sample',
           paired = 'Paired'),
    't test power calculation'
  )
  
  structure(
    list(n = n, f = f, cv = cv, sig.level = sig.level, power = power,
         alternative = alternative, distributon = dist, note = NOTE,
         method = METHOD),
    class = 'power.htest'
  )
}

#' Simon two-stage designs
#' 
#' Function for sample sizes for Simon optimal two-stage, single-arm designs.
#' 
#' For two-stage designs for studies with binary endpoints, searches over
#' possible two-stage sampling designs to find those that minimize the
#' expected number of subjects, subject to specified constraints.
#'    
#' @param p0,pa null and alternative hypothesis response probability
#' @param n1max maximum number of subjects entered during the first stage; 
#' ignored if <= 0
#' @param ntmax maximum total number of total subjects
#' @param alpha,beta type-I and type-II errors
#' @param del searches for designs where the expected number of subjects under
#' the null with within \code{del} of the minimum possible value
#' @param minimax logical; if \code{TRUE}, only searches for designs which will
#' minimize the maximum sample size
#' 
#' @return
#' A list with the following elements:
#' 
#' \item{\code{$designs}}{a matrix with a row giving a summary of each
#' design which meets the criteria. The columns are: \code{n1}, the number of
#' subjects entered in the first stage; \code{r1}, the cutoff for stopping at
#' the first stage (continue if the number of responses is > \code{r1});
#' \code{n2}, the additional number of subjects enrolled in the second stage; 
#' \code{r2}, the cutoff for inactivity after the second stage (reject the null
#' if the number of responses is > \code{r2}); \code{Pstop1.H0}, the
#' probability of stopping after the first stage under H0 (\code{p0});
#' \code{size}, the actual type-I error; \code{type2}, the actual type-II
#' error; \code{E.tot.n.H0}, the expected number of subjects under H0}
#' \item{\code{$call}}{the call to \code{simon2}}
#' \item{\code{$description}}{a text string giving a brief description of
#' the columns in \code{$designs}}
#' 
#' @seealso
#' \pkg{desmon}: \code{simon}, \code{twostg}, \code{bin1samp}, \code{pickwin},
#' \code{rp21}
#' 
#' @author
#' Robert Gray (\code{desmon::simon}); Robert Redd (\code{simon2})
#' 
#' @references
#' Simon R (1989). Optimal two-stage designs for phase II clinical trials.
#' \emph{Controlled Clinical Trials}, 10:1-10.
#' 
#' @examples
#' simon2(0.2, c(0.4, 0.5))
#' simon2(p0 = seq(0.55, 0.6, by = 0.01), pa = 0.75, ntmax = 60)
#' 
#' ## compare this function to results from desmon::simon
#' simon2(0.4, 0.6)
#' ## from desmon package
#' rawr:::simon(0.4, 0.6)
#' 
#' @export

simon2 <- function(p0, pa, n1max = 0, ntmax = 1e+05, alpha = 0.1, beta = 0.1,
                   del = 1, minimax = FALSE) {
  args <- expand.grid(p0 = p0, pa = pa)
  
  sim <- Map('simon', p0 = args[['p0']], pa = args[['pa']],
             n1max = n1max, alpha = alpha, beta = beta,
             del = del, minimax = minimax)
  sim <- lapply(sim, '[[', 1L)
  sim <- setNames(sim, sapply(seq_len(nrow(args)), function(x)
    catlist(args[x, ])))
  
  list(
    designs = sim,
    call = match.call(),
    description = c('n1, n2 = cases 1st stage and additional # in 2nd',
                    paste('r1, r2 = max # responses 1st stage and total to',
                          'declare trt inactive'))
  )
}

simon <- function(p0, pa, n1max = 0, ntmax = 1e5, alpha = 0.1, beta = 0.1,
                  del = 1, minimax = FALSE) {
  ## desmon::simon
  # 
  ## prob calculations for 2-stage phase II design
  ## optimal simon designs (minimize E(n|H0))
  # 
  # p0	    Null hypothesis response probability
  # pa	    Alternative hypothesis response probability
  # n1max	  The maximum number of subjects entered during the first stage.
  #         Ignored if <= 0.
  # ntmax	  The maximum total number of subjects.
  # alpha	  Type I error rate
  # beta	  Type II error rate
  # del	    Searches for designs where the expected number of subjects under
  #         the null is within del of the minimum possible value
  # minimax	If TRUE, only searches for designs with the total sample size equal
  #         to the minimum possible value
  
  if (alpha > 0.5 | alpha <= 0 | 1 - beta <= alpha |
      beta <= 0 | p0 <= 0 | p0 >= pa |
      pa >= 1 | n1max > ntmax)
    stop('invalid arguments')
  
  ## determine min sample size first stage
  n1min <- max(ceiling(log(beta) / log(1 - pa)), 2)
  if (n1min > ntmax)
    stop('no valid designs')
  
  ## optimal one-sample design
  u1 <- bin1samp(p0, pa, alpha, beta)
  
  ## include even if n > n1max
  z <- matrix(c(u1[1:2], 0, u1[2L], 1, u1[5:6], u1[1L]), nrow = 1L)
  if (n1min < u1[1L]) {
    if (n1max > 0)
      n1max <- min(n1max, u1[1L] - 1)
    else n1max <- u1[1L] - 1
  } else
    if (n1min == u1[1L]) {
      if (n1max > 0)
        n1max <- min(n1max, u1[1L])
      else n1max <- u1[1L]
    } else stop('no valid designs')
  
  n1max <- min(n1max, ntmax)
  n1max <- min(n1max, ntmax)
  
  ## determine min total sample size
  ## (use randomized decision rule on boundary for single sample)
  b <- 0
  n <- u1[1L]
  while (b <= beta) {
    n <- n - 1
    u <- c(1, 1 - pbinom(0:(u1[2L] + 1), n, p0))
    index <- min((1:(u1[2L] + 3))[u <= alpha])
    pi <- (alpha - u[index]) / (u[index - 1L] - u[index])
    u <- if (index > 2L)
      1 - pbinom((index - 3L):(index - 2L), n, pa)
    else c(1, 1 - pbinom(0, n, pa))
    b <- 1 - u[2L] - pi * (u[1L] - u[2L])
  }
  
  if (n > ntmax)
    stop('no valid designs')
  e0 <- u1[1L]
  
  ## cases 1st stage
  for (i in n1min:n1max) {
    # stage I
    # feasible stopping rules
    x1 <- 0:i
    w1 <- dbinom(x1, i, p0)
    w2 <- dbinom(x1, i, pa)
    sub <- cumsum(w2) <= beta
    ## feasible 1st stage stopping rules
    y1 <- x1[sub]
    
    for (r1 in y1) {
      ## additional cases at 2nd stage
      j <- n - i
      if (j > 0) {
        q <- 0L
        pi0 <- 1 - sum(w1[1:(r1 + 1L)])
        
        while (q == 0L) {
          x2 <- 0:j
          w3 <- dbinom(x2, j, p0)
          w4 <- dbinom(x2, j, pa)
          ## b0, ba = marginal dist of # responses H0, Ha
          u3 <- c(outer(x1[-(1:(r1 + 1L))], x2, '+'))
          u4 <- c(outer(w1[-(1:(r1 + 1L))], w3))
          b0 <- cumsum(c(w1[1:(r1 + 1L)], tapply(u4, u3, sum)))
          sub <- b0 < 1 - alpha
          r2 <- sum(sub)
          u4 <- c(outer(w2[-(1:(r1 + 1L))], w4))
          ba <- cumsum(c(w2[1:(r1 + 1L)], tapply(u4, u3, sum)))
          e1 <- i + pi0 * j
          if (ba[r2 + 1L] <= beta) {
            q <- 1L
            if (minimax) {
              if (i + j < ntmax) {
                ntmax <- i + j
                ## need to reset to min E.H0 at min # cases
                e0 <- e1
              } else
                if (i + j == ntmax)
                  e0 <- min(e0, e1)
            } else {
              e0 <- min(e0, e1)
            }
            z <- rbind(z, c(i, r1, j, r2, b0[r1 + 1L],
                            1 - b0[r2 + 1], ba[r2 + 1L], e1))
          } else {
            j <- j + 1L
            if (minimax) {
              if (i + j > ntmax)
                q <- 1L
            } else {
              if (e1 > e0 + del | i + j > ntmax)
                q <- 1L
            }
          }
        }
      }
    }
  }
  
  dimnames(z) <- list(NULL, c('n1', 'r1', 'n2', 'r2', 'Pstop1.H0',
                              'size', 'type2', 'E.tot.n.H0'))
  z <- z[z[, 1L] + z[, 3L] <= ntmax, , drop = FALSE]
  z <- z[z[, 8L] <= e0 + del, , drop = FALSE]
  
  list(
    designs = z[order(z[, 8L]), , drop = FALSE],
    call = match.call(),
    description = c(
      'n1, n2 = cases 1st stage and additional # in 2nd',
      'r1, r2 = max # responses 1st stage and total to declare trt inactive')
  )
}

bin1samp <- function (p0, pa, alpha = 0.1, beta = 0.1, n.min = 20L) {
  ## desmon::bin1samp
  # 
  # p0	   Null hypothesis response probability
  # pa	   Alternative hypothesis response probability
  # alpha	 Type I error rate
  # beta	 Type II error rate
  # n.min	 Minimum sample size considered
  
  if (p0 == pa)
    stop('p0 should not be equal to pa')
  b <- 1
  x <- round(p0 * n.min)
  n <- n.min - 1L
  if (pa > p0) {
    while (b > beta) {
      n <- n + 1L
      l <- x:n
      s <- 1 - pbinom(l, n, p0)
      sub <- s <= alpha
      x <- l[sub][1L]
      size <- s[sub][1L]
      b <- pbinom(x, n, pa)
    }
  } else
    if (pa < p0) {
      while (b > beta) {
        n <- n + 1L
        l <- x:0
        s <- pbinom(l, n, p0)
        sub <- s <= alpha
        x <- l[sub][1L]
        size <- s[sub][1L]
        b <- 1 - pbinom(x, n, pa)
        x <- x + 1L
      }
    }
  
  structure(
    c(n = n, r = x, p0 = p0, pa = pa, size = size, type2 = b),
    class = 'bin1samp'
  )
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
#' and Pearson's chi-squared test otherwise
#' @param ... additional parameters passed to \code{\link{fisher.test}} or
#' \code{\link{chisq.test}}
#' 
#' @seealso
#' \code{\link{mood.test}}; \code{\link{fisher.test}};
#' \code{\link{chisq.test}}; \code{\link{kruskal.test}};
#' \code{\link{wilcox.test}}
#' 
#' @examples
#' set.seed(1)
#' X <- list(rnorm(10), rnorm(10, 1), rnorm(20, 2))
#' moods_test(X)
#' moods_test(X[1:2], exact = FALSE, correct = TRUE)
#' 
#' plot(density(X[[1]]), xlim = range(unlist(X)), ylim = c(0, .5), main = '')
#' for (x in 2:3)
#'   lines(density(X[[x]]), col = x)
#' 
#' @export

moods_test <- function(X, ..., exact = TRUE) {
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

#' Fake GLM
#' 
#' Creates a "fake" \code{\link{glm}} object using specific coefficients
#' which can be used with \code{predict.glm} without fitting a model first.
#' 
#' Using \code{data} a \code{glm} object will be created with a series of the
#' desired coefficients passed in \dots in the order that they would appear
#' in \code{names(coef(glm(...)))}. An unnamed value will be treated as
#' the intercept and all other coefficients must be named in the order given
#' in \code{formula}, eg, \code{1, x1 = 1, x2 = 3}, etc.
#' 
#' Also note that factor variables must already be factors in \code{data},
#' that is, it is not possible to use \code{y ~ factor(x)} currently in
#' \code{fakeglm}.
#' 
#' @param formula a \code{\link{formula}}
#' @param ... coefficients for new model; see details
#' @param family a description of the error distribution and link to be used
#' in the model; this can be a character string naming a family function or
#' the result of a clall to a family function; see \code{\link{family}}
#' @param data a data frame
#' 
#' @references
#' \url{https://gist.github.com/MrFlick/ae299d8f3760f02de6bf}
#' 
#' @examples
#' f1 <- glm(vs ~ mpg + wt + disp, data = mtcars, family = 'binomial')
#' p1 <- predict(f1, type = 'response')
#' 
#' f2 <- fakeglm(vs ~ mpg + wt + disp, data = mtcars, family = 'binomial',
#'               -21.9020, mpg = 0.6470, wt = 5.3315, disp = -0.0403)
#' p2 <- predict(f2, newdata = mtcars, type = 'response')
#' 
#' all.equal(p1, p2, tolerance = .0001) ## TRUE
#' 
#' dat <- within(mtcars, gear <- factor(gear))
#' fakeglm(vs ~ mpg + gear, data = dat, family= 'binomial', 0, mpg = 1,
#'         gear4 = 3, gear5 = 1)
#'
#' @export

fakeglm <- function(formula, ..., family, data = NULL) {
  dots <- list(...)
  res  <- list()
  
  ## stats:::.MFclass
  .MFclass <- function (x) {
    if (is.logical(x))
      'logical'
    else if (is.ordered(x))
      'ordered'
    else if (is.factor(x))
      'factor'
    else if (is.character(x))
      'character'
    else if (is.matrix(x) && is.numeric(x))
      paste('nmatrix', ncol(x), sep = '.')
    else if (is.numeric(x))
      'numeric'
    else 'other'
  }
  
  tt <- terms(formula, data = data)
  if (!is.null(data)) {
    mf <- model.frame(tt, data)
    vn <- sapply(attr(tt, 'variables')[-1L], deparse)
    if ((yvar <- attr(tt, 'response')) > 0)
      vn <- vn[-yvar]
    xlvl <- lapply(data[vn], function(x)
      if (is.factor(x))
        levels(x)
      else if (is.character(x))
        levels(as.factor(x))
      else NULL)
    attr(res, 'xlevels') <- xlvl[!vapply(xlvl, is.null, NA)]
    attr(tt, 'dataClasses') <- sapply(data[vn], .MFclass)
  }
  res$terms <- tt
  coef <- numeric(0L)
  stopifnot(length(dots) > 1L, !is.null(names(dots)))
  
  for (ii in seq_along(dots)) {
    if ((n <- names(dots)[ii]) != '') {
      v <- dots[[ii]]
      if (!is.null(names(v))) {
        coef[paste0(n, names(v))] <- v
      } else {
        stopifnot(length(v) == 1L)
        coef[n] <- v
      }
    } else {
      coef['(Intercept)'] <- dots[[ii]]
    }
  }
  
  res$coefficients <- coef
  res$rank <- length(coef)
  if (!missing(family)) {
    res$family <- if (inherits(family, 'family'))
      family
    else if (class(family) == 'function')
      family()
    else if (is.character(family))
      get(family)()
    else stop('Invalid family class: ', class(family))
    
    res$qr <- list(pivot = seq_len(res$rank))
    res$deviance <- 1
    res$null.deviance <- 1
    res$aic <- 1
    class(res) <- c('glm', 'lm')
  } else {
    class(res) <- 'lm'
    res$fitted.values <- predict(res, newdata = data)
    res$residuals <- res$mf[attr(tt, 'response')] - res$fitted.values
    res$df.residual <- nrow(data) - res$rank
    res$model <- data
    ## qr doesn't work
  }
  
  res
}

#' GCD
#' 
#' Find greatest common divisor of two integers.
#' 
#' @param x,y integers
#' 
#' @examples
#' gcd(99, 2048)
#' gcd(2 ^ (1:12), 2048)
#' 
#' @export

gcd <- function(x, y) {
  ifelse(r <- x %% y, Recall(y, r), y)
}

#' Install bioconductor
#' 
#' Installs bioconductor base, packages, or upgrades.
#' 
#' @param pkgs character vector of package names to install
#' @param upgrade logical; if \code{TRUE} updates installed packages
#' 
#' @examples
#' \dontrun{
#' install.bioc()
#' install.bioc('Biostrings')
#' }
#' 
#' @export

install.bioc <- function(pkgs, upgrade = FALSE) {
  source(system.file('scripts', 'biocLite.R', package = 'rawr'))
  
  f <- function(...)
    biocLite(..., suppressAutoUpdate = TRUE, suppressUpdates = TRUE)
  if (upgrade) {
    biocLite(suppressAutoUpdate = TRUE, suppressUpdates = FALSE)
    return(invisible(NULL))
  }
  
  if (missing(pkgs))
    f() else f(pkgs)
  
  invisible(NULL)
}

#' Standardize regression coefficients
#' 
#' Computes standardized regression coefficients (beta) for linear models.
#' 
#' The optional \code{weights} argument can be used to scale the standard
#' deviation(s) of the coefficient(s). The default is \code{weights = 1}, but
#' \code{weights = 2} has also been suggested so that the generic comparison
#' is with inputs equal to the mean +/- 1 standard deviation [Gelman] (see
#' references). Additionally, \code{weights} can be a vector of weights for
#' each coefficient.
#' 
#' @param x an \code{\link{lm}} object
#' @param weights a vector of weights; see details
#' 
#' @seealso
#' \code{\link[QuantPsyc]{lm.beta}}
#' 
#' @references
#' \url{http://onlinelibrary.wiley.com/doi/10.1002/sim.3107/abstract}
#' 
#' @examples
#' cc <- with(mtcars, cor(mpg, wt))
#' lm.beta(lm(mpg ~ wt, data = mtcars))
#' 
#' cc ^ 2
#' summary(lm(mpg ~ wt, data = mtcars))$r.squared
#' 
#' lm.beta(lm(mpg ~ wt + disp + vs, data = mtcars), weights = 2)
#' 
#' @export

lm.beta <- function (x, weights = 1) {
  stopifnot(
    inherits(x, 'lm')
  )
  
  b  <- coef(x)[-1L]
  mf <- x$model
  
  sx <- vapply(mf[, -1L, drop = FALSE], sd, double(1L))
  sy <- vapply(mf[,  1L, drop = FALSE], sd, double(1L))
  
  b * sx / sy * weights
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
#' non-numeric elements of a list will be coerced with a warning; if \code{x}
#' is a list, the list elements are assumed to be groups ordered as
#' \code{x[[1]], x[[2]], ..., x[[n]]}
#' @param ... additional arguments passed to the function given by
#' \code{details}
#' @param g a vector or factor object giving the group for the corresponding
#' elements of \code{x}, ignored with a warning if \code{x} is a list; if
#' \code{g} is \emph{not} a factor, it will be coerced, and groups will be
#' ordered as \code{sort(unique(g))}; see \code{\link{factor}}
#' @param details \code{FALSE} or a function to compute comparisons between
#' pairs of groups; see details
#' @param correct logical; if \code{TRUE}, a correction is applied to the
#' standard error of the test statistic (default)
#' @param formula a formula of the form \code{response ~ group} where
#' \code{response} is a numeric variable and \code{group} is a factor-like
#' variable with three or more unique values (groups)
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in \code{formula}; by
#' default, the variables are taken from \code{environment(formula)}
#' @param simulate.p.value logical; if \code{TRUE}, p-value is computed using
#' by Monte Carlo simulation
#' @param B an integer specifying the number of replicates used in the Monte
#' Carlo test
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
#' cuzick.test(x, details = coin::wilcox_test)$details
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
    
    pw <- list(pairs = if (is.null(PW)) pw else
      `rownames<-`(cbind(pairs = ids, do.call('rbind', PW)), NULL),
      overall = tidy(kruskal.test(x ~ g)))
    
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
  m$`...` <- NULL
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
  T  <- sum(li * Ri, na.rm = TRUE)
  eT <- (N + 1) * L / 2
  vT <- (N + 1) / 12 * (N * sum(li ^ 2 * ni) - L ^ 2)
  
  ## correction for ties
  ## tj: times each value of x appears; a: correction for se
  tj <- c(tapply(x, x, length))
  a  <- sum(tj * (tj ^ 2 - 1)) / (N * (N ^ 2 - 1))
  a  <- c(1, sqrt(1 - a))[correct + 1L]
  
  ## (un)corrected test statistic: (T - expected) / se
  (T - eT) / (a * sqrt(vT))
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
#' factor object
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

#' Hodges-Lehmann estimator
#' 
#' A function to calculate a nonparametric estimation of a population's
#' location parameter for one sample described by Hodges and Lehmann (1963).
#' 
#' For a numeric vector \code{x} with length \code{n}, the median of the
#' means of \code{n(n+1)/2} pairs (i.e., including the vector itself)
#' are calculated.
#' 
#' For symmetric distributions, the Hodges-Lehmann statistic estimates the
#' population's median and has greater efficiency than the sample median.
#' For the normal distribution, the Hodges-Lehmann statistic is nearly as
#' efficient as the sample mean and better when estimating mixtures of
#' normal distributions.
#' 
#' For non-symmetric distributions, the Hodges-Lehmann statistic estimates
#' the population's "pseudo-median" (see \code{\link[stats]{wilcox.test}}), a
#' location parameter that is closely related to the median. The pseudo-median
#' is well defined for all distributions of random variables having dimension
#' two or greater, and like the median, the pseudo-median is defined for even
#' heavy-tailed distributions that lack any finite mean.
#' 
#' @param x a numeric or logical vector
#' @param na.rm logical; if \code{TRUE}, \code{NA} values are removed
#' 
#' @references
#' Hettmansperger, T. P.; McKean, J. W. (1998). \emph{Robust nonparametric
#' statistical methods}. Kendall's Library of Statistics. \strong{5}.
#' 
#' Hodges, J. L.; Lehmann, E. L. (1963). "Estimation of location based on
#' ranks." \emph{Annals of Mathematical Statistics}. \strong{34} (2): 598-611.
#' 
#' @seealso
#' \code{\link{wilcox.test}}; \code{ICSNP::hl.loc}
#' 
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' hl <- hl_est(x)
#' 
#' ## compare
#' hl2 <- wilcox.test(x, exact = TRUE, conf.int = TRUE)
#' identical(hl, unname(hl2$estimate))
#' 
#' @export

hl_est <- function(x, na.rm = FALSE) {
  stopifnot(is.numeric(x) | is.logical(x))
  if (na.rm)
    x <- x[!is.na(x)]
  pmeans <- combn_fun(x, mean.default, 2L)
  
  median(c(pmeans, x))
}

combn_fun <- function(x, FUN, n = 2L, ...) {
  FUN <- match.fun(FUN)
  n   <- as.integer(n)
  x   <- combn(x, n)
  
  apply(x, 2L, FUN, ...)
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
#' of terminal nodes or total nodes
#' @param droplevels logical; if \code{TRUE}, only node labels with at least
#' one observation are used (i.e., only terminal node labels are used)
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
    1L else sort(c(node, Recall(node %/% 2)))
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

#' Correlate variables
#' 
#' Generate a (random) vector having a desired correlation with one or more
#' variables.
#' 
#' @param y a numeric vector or matrix used to set correlation(s)
#' @param x a vector to be correlated with \code{y}; if not given, a random,
#' normally distributed vector is used
#' @param rho the desired correlation(s); should be equal in length to the
#' number of columns of \code{y}, recycled as needed (or length 1 if \code{y}
#' is a vector)
#' 
#' @seealso
#' Adapted from \url{https://stats.stackexchange.com/q/15011/66247}
#' 
#' @examples
#' set.seed(1)
#' y <- matrix(rnorm(150), 50)
#' x <- rnorm(50)
#' 
#' cor(y[, 1], rcor(y[, 1]))
#' cor(y[, 1], rcor(y[, 1], x, rho = 0.8))
#' 
#' cor(y, rcor(y))
#' cor(y, rcor(y, rho = c(.75, -.5, .25)))
#' 
#' pairs(
#'   cbind(rcor(y, x, rho = c(.75, -.5, .25)), y),
#'   upper.panel = function(x, y, ...) {
#'     points(x, y)
#'     abline(lm(y ~ x), col = 2)
#'     mtext(parse(text = sprintf('rho==%.2f', cor(x, y))), cex = 2, col = 2)
#'   }
#' )
#' 
#' @export

rcor <- function(y, x = NULL, rho = 0.5) {
  x <- if (is.null(x))
    rnorm(NROW(y)) else rep_len(x, NROW(y))
  
  if (is.null(dim(y)))
    rcor1(y, x, rho[1L])
  else rcorn(y, x, rep_len(rho, ncol(y)))
}

rcor1 <- function(y, x, rho) {
  re <- residuals(lm(x ~ y))
  rho * sd(re) * y + re * sd(y) * sqrt(1 - rho ^ 2)
}

rcorn <- function(y, x, rho) {
  y  <- as.matrix(y)
  sy <- scale(y)
  nr <- nrow(y)
  
  re  <- residuals(lm(x ~ y))
  rho <- rep_len(rho, ncol(y))
  
  ## get the coefficient sigma of re so that the cor of y with the
  ## linear combination yd %*% rho + sigma * re is the desired vector
  yd <- with(svd(sy), (nr - 1L) * u %*% diag(ifelse(d > 0, 1 / d, 0)) %*% t(v))
  sigma2 <- c((1 - rho %*% cov(yd) %*% rho) / var(re))
  
  ## linear combination
  if (sigma2 >= 0)
    c(yd %*% rho + sqrt(sigma2) * re)
  else {
    warning('Joint correlations not possible', call. = FALSE)
    rep(0, nr)
  }
}

#' Random sum
#' 
#' Returns a vector of length \code{n} from \code{a} to \code{b} which sum to
#' \code{k}.
#' 
#' To find the \code{n} integers in \code{[a, b]} which sum to \code{k}, the
#' range \code{1:k} is broken into \code{n} pieces requiring \code{n - 1}
#' cutpoints, and the distance between cutpoints is summed and shifted by
#' \code{a}.
#' 
#' Note that the solution may not contain a unique set of integers unless
#' \code{unique = TRUE}. Additionally, this algorithm is not guaranteed to
#' find a solution even if one exists regardless of many \code{iterations}.
#' Finally, depending on the input parameters, the algorithm may complete
#' after one or require thousands of iterations.
#' 
#' @param a,b,n,k \code{n} integers in the range \code{[a,b]} which sum to
#' \code{k}
#' @param unique logical; if \code{TRUE}, the returned vector of integers
#' will be unique if possible
#' @param iterations maximum number of iterations to repeat the algorithm
#' until a solution is found
#' 
#' @seealso
#' Adapted from \url{https://stackoverflow.com/a/49016614/2994949};
#' \code{Surrogate::RandVec}
#' 
#' @examples
#' set.seed(1)
#' rsum(1, 100, 5, 98)
#' rsum(1, 100, 5, 120, TRUE)
#' sum(rsum(1, 100, 10, 98))
#' 
#' @export

rsum <- function(a, b, n, k, unique = FALSE, iterations = 100L) {
  a <- as.integer(a)
  b <- as.integer(b)
  n <- as.integer(n)
  k <- as.integer(k)
  
  for (ii in seq.int(iterations + 1L)) {
    if (ii > iterations) {
      message(
        sprintf('Note: maximum of %s iterations attempted:\n\t', iterations),
        'Try increasing the number of iterations; otherwise\n\t',
        'n or k may be too large, b - a is too small, or no solution exists'
      )
      return(invisible(NULL))
    }
    
    r <- k - n * a
    p <- n - 1L
    x <- sample(seq.int(r), p, TRUE)
    x <- diff(c(0, sort(x), r))
    
    if (x[1L] < a || x[p] > b)
      next
    
    if (max(x) <= b - a) {
      res <- structure(sort(a + x), iterations = ii)
      break
    }
  }
  
  if (!unique || length(unique(res)) == n)
    res else Recall(a, b, n, k, TRUE, iterations)
}

#' Test for trend in proportions
#' 
#' Performs a test for trend in an \code{r x c} contingency table with
#' one nominal (rows, r > 2) and one ordinal (columns, c > 2) variable.
#' 
#' @param x a factor-like vector giving the (unordered) variable (equivalently
#' the row variable of a contingency table); if \code{x} is also ordered,
#' consider using \code{\link{jt.test}}
#' 
#' alternatively, \code{x} can be an \code{r x c} table or matrix with at
#' least two rows (unordered) and three columns (ordered); \code{x} may also
#' be a list of the row variable split by the ordered column variable in
#' which case the list is assumed to be ordered, i.e., \code{x[[1]]} <
#' \code{x[[2]] < ... < x[[c]]}; see examples
#' @param ... further arguments to be passed to or from methods
#' @param g a factor-like vector giving the \emph{ordered} group for each
#' corresponding element of \code{x}, ignored with a warning if \code{x} is a
#' list or table; if \code{g} is not a factor, it will be coerced, and groups
#' will be ordered as sort(unique(g)); see \code{\link{factor}}
#' @param simulate.p.value logical; if \code{TRUE}, p-value is computed using
#' by Monte Carlo simulation
#' @param B an integer specifying the number of replicates used in the Monte
#' Carlo test
#' @param formula a formula of the form \code{row ~ column} where \code{row}
#' gives the (unordered) row variable and \code{column} gives the
#' \emph{ordered} column variable
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in \code{formula}; by
#' default the variables are taken from \code{environment(formula)}
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
  
  # stopifnot(
  #   length(unique(g)) >= 3L
  # )
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
#' the row variable of a contingency table)
#' 
#' alternatively, \code{x} can be a \code{2 x c} table or matrix with exactly
#' two rows and at least three ordered columns; \code{x} may also be a list
#' of the row variable split by the ordered column variable in which case the
#' list is assumed to be ordered, i.e., \code{x[[1]] < x[[2]] < ... < x[[c]]};
#' see examples
#' @param ... further arguments to be passed to or from methods
#' @param g a factor-like vector giving the \emph{ordered} group for each
#' corresponding element of \code{x}, ignored with a warning if \code{x} is a
#' list or table; if \code{g} is not a factor, it will be coerced, and groups
#' will be ordered as sort(unique(g)); see \code{\link{factor}}
#' @param score group score for each column, default is \code{1:ncol}
#' @param simulate.p.value logical; if \code{TRUE}, p-value is computed using
#' by Monte Carlo simulation
#' @param B an integer specifying the number of replicates used in the Monte
#' Carlo test
#' @param formula a formula of the form \code{row ~ column} where \code{row}
#' gives the row variable having two unique values and \code{column} gives
#' the \emph{ordered} column variable
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in \code{formula}; by
#' default the variables are taken from \code{environment(formula)}
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
#' ## the following are equivalent to call ca.test
#' dat <- data.frame(x = mtcars$vs, y = mtcars$gear)
#' ca.test(dat$x, dat$y)
#' ca.test(x ~ y, dat)
#' ca.test(split(dat$x, dat$y))
#' ca.test(table(dat$x, dat$y))
#' 
#' 
#' \dontrun{
#' ## simulate p-value with 10k replicates
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
  
  if (!length(unique(g)) >= 3L) {
    warning('Fewer than 3 groups')
  }
  
  if (any(table(x, g) < 5L) & !simulate.p.value)
    warning(
      'Chi-squared approximation may be incorrect - ',
      'cells with < 5 observations\n',
      '\tConsider using simulate.p.value = TRUE for Monte Carlo p-value'
    )
  
  method <- 'Cochran-Armitage test for trend in %sx%s table'
  tbl <- table(x, g)
  score <- score %||% seq.int(ncol(tbl))
  dname <- sprintf('%s\n\tusing scores: %s', dname, toString(score))
  res <- prop.trend.test(tbl[1L, ], colSums(tbl), score)
  
  if (simulate.p.value) {
    p <- sim.test.pvalue(x, g, ca.test, FALSE, B, TRUE, 0.99)
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
#' further to \code{\link{par}}
#' @param type character indicating the type of plotting; see
#' \code{\link{plot.default}}
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

#' Winsorization
#' 
#' Winsorize extreme values in a numeric vector.
#' 
#' @param x a numeric vector
#' @param probs numeric vector of probability value(s) in \code{[0,1]}
#' @param type an integer between 1 and 9 selecting one of the nine quantile
#' algorithms; see \code{\link{quantile}}
#' 
#' @examples
#' x <- c(92, 19, 101, 58, 1053, 91, 26, 78, 10, 13,
#'        -40, 101, 86, 85, 15, 89, 89, 28, -5, 41)
#' summary(x)
#' 
#' y <- winsorize(x, 0.1, type = 1L)
#' summary(y)
#' 
#' @export

winsorize <- function(x, probs = 1e-3, type = 7L) {
  probs <- if (length(probs) == 1L)
    c(probs, 1 - probs) else sort(probs)[1:2]
  
  stopifnot(
    is.numeric(x),
    type %in% 1:9,
    probs[1L] > 0, probs[2L] < 1
  )
  
  qn <- sort(quantile(x, probs, na.rm = TRUE, type = type))
  nx <- !is.na(x)
  
  x[nx & x < qn[1L]] <- qn[1L]
  x[nx & x > qn[2L]] <- qn[2L]
  
  x
}

#' Permutation t-test
#' 
#' Performs paired and two-sample t-tests based on Monte Carlo permutation.
#' 
#' @param x,y vectors of numeric data
#' @param alternative a character string specifying the alternative hypothesis,
#' one of \code{"two.sided"} (default), \code{"greater"}, or \code{"less"}
#' @param paired logical; if \code{TRUE}, \code{x} and \code{y} are assumed
#' to be paired (\code{NA}s will remove data in pairs); if \code{FALSE},
#' \code{x} and \code{y} are assumed to be independent samples (\code{NA}s
#' are removed from each sample independently)
#' @param var.equal logical; if \code{TRUE}, two-sample variances are treated
#' equally; otherwise, variances are estimated separately for both samples
#' (default)
#' @param conf.level confidence level in \code{(0, 1)} for the p-value
#' @param midp logical; if \code{TRUE} (default), the mid p-value is used,
#' i.e., half the conditional probability of the observed statistic plus the
#' conditional probability of more extreme values
#' @param B an integer specifying the number of permutations
#' @param ... additional arguments passed to or from methods
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} is a
#' numeric variable giving the data values and \code{rhs} a factor-like
#' variable with two levels giving the corresponding groups
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in \code{formula}; by
#' default the variables are taken from \code{environment(formula)}
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
  ci <- structure(pmin(1, pmax(0, pv + ci)), conf.level = conf.level)
  
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
