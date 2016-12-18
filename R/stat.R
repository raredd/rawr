### statistical functions
# rpart_utils: rpart_parent, rpart_subset, rpart_nodes
#
# bincon, bintest, dlt_table, power_cv, simon2, moods_test, fakeglm, gcd,
# install.bioc, lm.beta, cuzick.test, jt.test, hl_est, combn_fun
###


#' Binomial probability confidence intervals
#' 
#' Calculates confidence intervals for binomial probabilities for specified
#' type-I error (\code{alpha}) using exact, Wilson, or asymptotic methods.
#' 
#' If \code{method = 'all'}, \code{r} and \code{n} should each be length 1.
#' The "exact" method uses the \code{\link{df}} distribution to comupte exact
#' intervals (based on the binomial cdf); the "wilson" interval is score-test-
#' based; the "asymptotic"  is the asymptotic normal interval.
#' 
#' The "wilson" method has been preferred by Agresti and Coull.
#' 
#' @param r number of responses (successes)
#' @param n number of observations (trials)
#' @param alpha type-I error probability
#' @param round integer value specifying number of decimal places to round
#' (default is no rounding); see \code{\link{round}}
#' @param method character strings specifying which method to use; see details
#' 
#' @return
#' A matrix containing the computed interval(s) and their widths.
#' 
#' @author
#' Rollin Brant, Frank Harrell, and Brad Biggerstaff; modifications by Robert
#' Redd
#' 
#' @references Agresti, A. and B.A. Coull. Approximate is better than "exact"
#' for interval extimation of binomial proportions. \emph{American
#' Statistician}. \strong{59}:119-126, 1998.
#' @references Brown, L.D., T.T. Cai, and A. Das Gupta. Inverval estimation for
#' a binomial proportion (with discussion). \emph{Statistical Science}. 
#' \strong{16}:101-133, 2001.
#' @references Newcombe, R.G. Logit confidence intervals and the inverse sinh
#' transformation, \emph{American Statistician}. \strong{55}:200-202, 2001.
#' 
#' @seealso
#' \code{\link[Hmisc]{binconf}}; \code{desmon::binci}
#' 
#' @examples
#' bincon(0:10, 10)
#' bincon(5, 10, method = 'all')
#' 
#' @export

bincon <- function(r, n, alpha = 0.05, round = NULL,
                   method = c('exact','wilson','asymptotic','all')) {
  
  # error checks
  if (any(r < 0) | any(r > n))
    stop('invalid response value')
  if (alpha >= 1 | alpha <= 0)
    stop('alpha must be between 0 and 1')
  
  method <- match.arg(method)
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
    switch(method,
           exact = res[1, ],
           wilson = res[2, ],
           asymptotic = res[3, ],
           all = res, res)
  }
  
  if ((length(r) != length(n)) & length(r) == 1)
    r <- rep(r, length(n))
  if ((length(r) != length(n)) & length(n) == 1)
    n <- rep(n, length(r))
  if ((length(r) > 1 | length(n) > 1) & method == 'all') {
    method <- 'exact'
    warning('Multiple confidence intervals should use one method. ',
            'Using \'exact\' method', domain = NA)
  }
  if (method == 'all' & length(r) == 1 & length(n) == 1) {
    mat <- bc(r, n, alpha, method)
    mat <- cbind(mat, mat[, 3] - mat[, 2])
    dimnames(mat) <- list(c('Exact', 'Wilson', 'Asymptotic'),
                          c('PointEst', 'Lower', 'Upper','Width'))
    if (!is.null(round))
      mat[, 2:4] <- round(mat[, 2:4], digits = round)
    
    return(cbind(Responses = r, Trials = n, mat))
  }
  
  mat <- matrix(ncol = 3, nrow = length(r))
  for (i in 1:length(r))
    mat[i, ] <- bc(r[i], n[i], alpha = alpha, method = method)
  
  mat <- `colnames<-`(cbind(mat, mat[, 3] - mat[, 2]),
                      c('PointEst','Lower','Upper','Width'))
  
  if (!is.null(round))
    mat[, 2:4] <- round(mat[, 2:4], digits = round)
  
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
#' \code{r = n.max} (default); useful if the maximum number of responses that
#' may occur is known
#' @param alpha,beta type-I and type-II errors
#' 
#' @return
#' Sample size, overall power, and overall type-I error rate
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
#' bintest(p0low = .2, p1low = .5, n.max  = 25)
#' 
#' ## example in sas macro
#' bintest(.1, .15, .2, .2, n.max = 80, alpha = .08, beta = .24)
#' 
#' @export

bintest <- function (p0low, p0high = p0low, p1low, p1high = p1low, n.max,
                     r = n.max, alpha = .1, beta = .1) {
  
  stopifnot(alpha %inside% c(0,1), beta %inside% c(0,1))
  if (r < 1)
    stop('Number of responders must be >1')
  
  mat <- expand.grid(seq(p0low, p0high, by = .01), seq(p1low, p1high, .01),
                     1:n.max, 1:r)
  
  ## require > r responders out of n and remove where p1 <= p0 or r >= n
  mat <- `colnames<-`(as.matrix(within(mat, r2 <- mat[, 4] + 1)),
                      c('p0','p1','n','r','r2'))
  mat <- mat[!(mat[, 2] <= mat[, 1] | mat[, 4] >= mat[, 3]), ]
  
  ## Pr(<r2 responders|p0 true): pbinom(r2, n, p0)
  ## Pr(>r2 responders for p0): type-I error = 1 - y
  y <- pbinom(mat[, 5], mat[, 3], mat[, 1])
  
  ## Pr(<r2 responders | p1 true): pbinom(r2, n, p1): type-II error
  ## Pr(>r2 responders for p1): 1 - type-II error
  z <- pbinom(mat[, 5], mat[, 3], mat[, 2])
  
  ## signal: p1 - p0
  signal <- mat[, 2] - mat[, 1]
  
  mat <- cbind(mat, y, type1 = 1 - y, z, power = 1 - z, signal)
  alpha <- rep(alpha, times = dim(mat)[1])
  beta  <- rep(beta, times = dim(mat)[1])
  
  ## keep ones that fit type-I/II error constraints
  mat <- mat[mat[, 9] > (1 - beta) & mat[, 7] < alpha, ]
  mat <- unique(as.data.frame(mat[, c(1:3,5,7,9:10)]))
  mat <- mat[order(mat$n), ]
  
  list(designs = as.matrix(mat), call = match.call(),
       description = c(
         'n = overall sample size',
         'r2 = minimum number of responders required to reject p0',
         'signal = difference in null and alternative hypotheses'))
}

#' DLT table
#' 
#' Creates a standard 3x3 dose-limiting toxicity table with probabilities of
#' dose-escalation.
#' 
#' @param low,high lowest and highest true DLT rate, percent
#' @param delta interval of DLT rate sequence; default is 10
#' 
#' @examples
#' dlt_table(10, 50)
#' dlt_table(10, 50, 5)
#' 
#' @export

dlt_table <- function(low, high, delta = 10) {
  
  int <- seq(low, high, by = delta)
  mat <- matrix(NA, nrow = length(int), ncol = 2)
  
  for (ii in 1:length(int)) {
    mat[ii, 1] <- round(int[ii])
    mat[ii, 2] <- dbinom(0, 3, int[ii] / 100) + 
      dbinom(1, 3, int[ii] / 100) * dbinom(0, 3, int[ii] / 100)
  }
  `colnames<-`(mat, c('DLT rate (%)', 'Pr(Escalation)'))
}

#' Power calculations for one- and two-sample t-tests using ratio of means and
#' coefficients of variation
#' 
#' Compute power of test using mean ratios and coefficients of variation or
#' determine other parameters to obtain target power.
#' 
#' Exactly one of \code{n}, \code{f}, \code{cv}, \code{sig.level}, and
#' \code{power} must be \code{NULL}.
#' 
#' @param n number of observations (per group)
#' @param f ratio of means (>1)
#' @param cv coefficient of variation
#' @param sig.level significance level (type-I error probability)
#' @param power power of test (1 - type-II error probability)
#' @param type type of t test
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
#' @references Van Belle, G., Martin, D. Sample size as a function of
#' coefficient of variation and ratio of means. Am Statistician, Vol. 47, No. 3
#' (Aug., 1993), pp. 165-7.
#' @references Martin, D.C., and van Belle, G. Approximations for Power and
#' Sample Size for Student's t-Test. Technical Report 125 (1991), University of
#' Washington, Dept. of Biostatistics.
#' 
#' @examples
#' power_cv(n = NULL, 1.25, .2, .05, .8, distribution = 'normal')
#' power_cv(13, 1.25, .2, .05, power = NULL, distribution = 't')
#' power_cv(13, 1.25, .2, .05, power = NULL, distribution = 'log.normal')
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
    stop("'sig.level' must be numeric in [0, 1]")
  if (!is.null(power) && !is.numeric(power) || any(0 > power | power > 1))
    stop("'power' must be numeric in [0, 1]")
  if (f < 1) {
    warning('ratio of means must be such that mu1/mu0 > 1: 1/f used')
    f <- 1 / f
  }
  
  type <- match.arg(type)
  alternative <- match.arg(alternative)
  dist <- match.arg(distribution)
  if (dist == 'log.normal' && !(sig.level %in% c(0.01, 0.05)))
    stop('cannot use desired significance level:
         use 5% or 10% type-I error probability')
  tsample <- switch(type,
                    one.sample = 1, two.sample = 2, paired = 1)
  ttside <- switch(alternative,
                   less = 1, two.sided = 2, greater = 3)
  tside <- switch(alternative,
                  less = 1, two.sided = 2, greater = 1)
  
  ## assuming underlying t distribution
  if (ttside == 1) { # one-sided, less
    p.body <- quote({
      df <- (n - 1) * tsample
      qt <- qt(p = sig.level/tside, df = df, lower.tail = TRUE)
      pt(q = -(-qt + (sqrt(n)*(f-1))/(cv*(f**2+1)**.5)), df = df,
         lower.tail = TRUE)
    })
  }
  if (ttside == 2) { # two-sided
    p.body <- quote({
      df <- (n - 1) * tsample
      qt <- qt(sig.level/tside, df, lower.tail = FALSE)
      pt(q = -qt + (sqrt(n)*(f-1))/(cv*(f**2+1)**.5), df = df,
         lower.tail = TRUE)/2 + 
        pt(q = -(-qt + (sqrt(n)*(f-1))/(cv*(f**2+1)**.5)), df = df,
           lower.tail = FALSE)/2
    })
  }
  if (ttside == 3) { # one-sided, greater
    p.body <- quote({
      df <- (n - 1) * tsample
      qt <- qt(sig.level/tside, df = df, lower.tail = FALSE)
      pt(q = -(-qt + (sqrt(n)*(f-1))/(cv*(f**2+1)**.5)), df = df,
         lower.tail = FALSE)
    })
  }
  
  ## assuming underlying normal distribution
  if (dist == 'normal') {
    p.body <- quote({
      qt <- qnorm(p = sig.level/tside, lower.tail = FALSE)
      pnorm(q = -qt + (sqrt(n)*(f-1))/(cv*(f**2+1)**.5), lower.tail = TRUE)
    })
  }
  
  ## assuming underlying lognormal distribution with unknown variance
  if (dist == 'log.normal') {
    mat <- matrix(c(.005, -2.57583, -2.203837, .6699734, -.0524065, -.0059258,
                    .010, -2.32635, -1.821394, .5380802, .0181774, -.0584748,
                    .025, -1.95996, -1.145521, .2370261, .0392020, -.0670915,
                    .050, -1.64485, -.8455414, .1745865, .0774911, -.0865455),
                  ncol = 6,byrow = TRUE)
    dimnames(mat)[[2]] <- c('alpha','z','a','b','c','d')
    alpha <- which(mat[,1] %in% sig.level)
    
    p.body <- quote({
      delta <- ifelse(cv < .5,sqrt(n/2)*log(f)/cv,sqrt(n/2)*log(f)/
                        (sqrt(log(cv**2+1))))
      v <- 2*n-2
      pnorm(q = mat[alpha-ifelse(tside==2,1,0),'z'] + 
              delta*(1+mat[alpha,'a']/v+mat[alpha,'b']/(v-1)+
                       delta*(mat[alpha,'c']/v+mat[alpha,'d']/(v-1))))
    })
  }
  
  ## calculate missing parameter
  if (is.null(power)) power <- eval(p.body)
  else if (is.null(n))
    n <- ifelse(dist=='log.normal',ceiling(uniroot(function(n)
      eval(p.body) - power, c(2, 1e+07))$root),uniroot(function(n)
        eval(p.body) - power, c(2, 1e+07))$root)
  else if (is.null(cv)) {
    cv <- if (ttside == 2)
      uniroot(function(cv) eval(p.body) - power, f * c(1e-07, 10))$root else
        if (ttside == 1)
          uniroot(function(cv) eval(p.body) - power, f * c(-10, 5))$root else
            if (ttside == 3)
              uniroot(function(cv) eval(p.body) - power, f * c(-5, 10))$root
  } else if (is.null(f)) {
    f <- if (ttside == 2)
      uniroot(function(f) eval(p.body) - power, cv * c(1e-07, 10))$root else
        if (ttside == 1)
          uniroot(function(f) eval(p.body) - power, cv * c(-10, 5))$root else
            if (ttside == 3)
              uniroot(function(f) eval(p.body) - power, cv * c(-5, 10))$root
  } else if (is.null(sig.level)) {
    sig.level <- uniroot(function(sig.level) eval(p.body) - power,
                         c(1e-10, 1 - 1e-10))$root
  }  else stop("internal error")
  
  NOTE <- switch(type,
                 paired = paste('n is number of *pairs*, cv is coefficient of',
                                'variation of *differences* within pairs'),
                 two.sample = 'n is number in *each* group',
                 one.sample = NULL)
  METHOD <- paste(switch(type,
                         one.sample = "One-sample",
                         two.sample = "Two-sample",
                         paired = "Paired"),
                  't test power calculation')
  structure(list(n = n, f = f, cv = cv, sig.level = sig.level, power = power,
                 alternative = alternative, distributon = dist, note = NOTE,
                 method = METHOD),
            class = 'power.htest')
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
#' A list with the following components:
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
#' \item{\code{$call}}{the call to \code{simon2}.}
#' \item{\code{$description}}{a text string giving a brief description of
#' the columns in \code{$designs}.}
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
#' simon2(.2, c(.4, .5))
#' simon2(p0 = seq(.55, .6, by = .01), pa = .75, ntmax = 60)
#' 
#' \dontrun{
#' ## compare this function to results from desmon::simon
#' simon2(.4, .6)
#' ## from desmon package
#' rawr:::simon(.4, .6)
#' }
#' 
#' @export

simon2 <- function(p0, pa, n1max = 0, ntmax = 1e+05, alpha = 0.1, beta = 0.1,
                   del = 1, minimax = FALSE) {
  
  args <- expand.grid(p0 = p0, pa = pa)
  tmp <- setNames(lapply(Map(simon, p0 = args[['p0']], pa = args[['pa']],
                             n1max = n1max, alpha = alpha, beta = beta,
                             del = del, minimax = minimax),
                         '[[', 1),
                  sapply(seq_len(nrow(args)), function(x) catlist(args[x, ])))
  
  list(designs = tmp, call = match.call(),
       description = c('n1, n2 = cases 1st stage and additional # in 2nd',
                       paste('r1, r2 = max # responses 1st stage and total to',
                             'declare trt inactive')))
}

simon <- function(p0, pa, n1max = 0, ntmax = 1e5, alpha = 0.1, beta = 0.1,
                  del = 1, minimax = FALSE) {
  
  ## prob calculations for 2-stage phase II design
  ## optimal simon designs (minimize E(n|H0))
  ## p0, pa, null & alt response probabilities
  ## n1max = max # subject in first stage
  
  if (alpha > .5 | alpha <= 0 | 1 - beta <= alpha |
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
  z <- matrix(c(u1[1:2], 0, u1[2], 1, u1[5:6], u1[1]), nrow = 1)
  if (n1min < u1[1]) {
    if (n1max > 0)
      n1max <- min(n1max, u1[1] - 1)
    else n1max <- u1[1] - 1
  } else
    if (n1min == u1[1]) {
      if (n1max > 0)
        n1max <- min(n1max, u1[1])
      else n1max <- u1[1]
    } else
      stop('no valid designs')
  n1max <- min(n1max,ntmax)
  
  n1max <- min(n1max,ntmax)
  
  ## determine min total sample size
  ## (use randomized decision rule on boundary for single sample)
  b <- 0
  n <- u1[1]
  while (b <= beta) {
    n <- n-1
    u <- c(1, 1 - pbinom(0:(u1[2] + 1), n, p0))
    index <- min((1:(u1[2] + 3))[u <= alpha])
    pi <- (alpha - u[index]) / (u[index - 1] - u[index])
    if (index > 2) {
      u <- 1 - pbinom((index - 3):(index - 2), n, pa)
    } else
      u <- c(1, 1 - pbinom(0, n, pa))
    b <- 1 - u[2] - pi * (u[1] - u[2])
  }
  
  if (n > ntmax)
    stop('no valid designs')
  e0 <- u1[1]
  
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
      j <- n-i
      if (j > 0) {
        q <- 0
        pi0 <- 1-sum(w1[1:(r1 + 1)])
        
        while (q == 0) {
          x2 <- 0:j
          w3 <- dbinom(x2, j, p0)
          w4 <- dbinom(x2, j, pa)
          ## b0, ba = marginal dist of # responses H0, Ha
          u3 <- c(outer(x1[-(1:(r1 + 1))], x2, '+'))
          u4 <- c(outer(w1[-(1:(r1 + 1))], w3))
          b0 <- cumsum(c(w1[1:(r1 + 1)], tapply(u4, u3, sum)))
          sub <- b0 < 1 - alpha
          r2 <- sum(sub)
          u4 <- c(outer(w2[-(1:(r1 + 1))], w4))
          ba <- cumsum(c(w2[1:(r1 + 1)], tapply(u4, u3, sum)))
          e1 <- i + pi0 * j
          if (ba[r2 + 1] <= beta) {
            q <- 1
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
            z <- rbind(z, c(i, r1, j, r2, b0[r1 + 1],
                            1 - b0[r2 + 1], ba[r2 + 1], e1))
          } else {
            j <- j + 1
            if (minimax) {
              if (i + j > ntmax)
                q <- 1 
            } else {
              if (e1 > e0 + del | i + j > ntmax)
                q <- 1
            }
          }
        }
      }
    }
  }
  
  dimnames(z) <- list(NULL, c('n1', 'r1', 'n2', 'r2', 'Pstop1.H0',
                              'size', 'type2', 'E.tot.n.H0'))
  z <- z[z[ , 1] + z[ , 3] <= ntmax, ,drop = FALSE]
  z <- z[z[ , 8] <= e0 + del, , drop = FALSE]
  
  list(designs = z[order(z[ , 8]), , drop = FALSE],
       call = match.call(),
       description = c(
         'n1, n2 = cases 1st stage and additional # in 2nd',
         'r1, r2 = max # responses 1st stage and total to declare trt inactive')
  )
}

bin1samp <- function (p0, pa, alpha = 0.1, beta = 0.1, n.min = 20) {
  if (p0 == pa)
    stop('p0 should not be equal to pa')
  b <- 1
  x <- round(p0 * n.min)
  n <- n.min - 1
  if (pa > p0) {
    while (b > beta) {
      n <- n + 1
      l <- x:n
      s <- 1 - pbinom(l, n, p0)
      sub <- s <= alpha
      x <- l[sub][1]
      size <- s[sub][1]
      b <- pbinom(x, n, pa)
    }
  } else
    if (pa < p0) {
      
      while (b > beta) {
        n <- n + 1
        l <- x:0
        s <- pbinom(l, n, p0)
        sub <- s <= alpha
        x <- l[sub][1]
        size <- s[sub][1]
        b <- 1 - pbinom(x, n, pa)
        x <- x + 1
      }
    }
  u <- c(n = n, r = x, p0 = p0, pa = pa, size = size, type2 = b)
  class(u) <- 'bin1samp'
  u
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
#' @param ... additional parameters passed to \code{\link{fisher.test}}
#' 
#' @seealso
#' \code{\link{mood.test}}; \code{\link{kruskal.test}};
#' \code{\link{wilcox.test}}
#' 
#' @examples
#' set.seed(1)
#' X <- list(rnorm(10), rnorm(10, 1), rnorm(20, 2))
#' moods_test(X)
#' 
#' plot(density(X[[1]]), xlim = range(unlist(X)), ylim = c(0, .5))
#' for (x in 2:3)
#'   lines(density(X[[x]]), col = x)
#' 
#' @export

moods_test <- function(X, ...) {
  x <- unlist(X)
  g <- rep(ng <- seq_len(length(X)), times = sapply(X, length))
  m <- median(x)
  zzz <- fisher.test(x < m, g, ...)
  zzz$method <- sprintf('Mood\'s median test of %s groups', length(ng))
  zzz$data <- table(x < m, g, dnn = c('< median', 'group'))
  zzz
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
  out <- list()
  ## stats:::.MFclass
  .MFclass <- function (x) {
    if (is.logical(x)) return('logical')
    if (is.ordered(x)) return('ordered')
    if (is.factor(x)) return('factor')
    if (is.character(x)) return('character')
    if (is.matrix(x) && is.numeric(x))
      return(paste('nmatrix', ncol(x), sep = '.'))
    if (is.numeric(x)) return('numeric')
    return('other')
  }
  tt <- terms(formula, data = data)
  if (!is.null(data)) {
    mf <- model.frame(tt, data)
    vn <- sapply(attr(tt, 'variables')[-1], deparse)
    if ((yvar <- attr(tt, 'response')) > 0)
      vn <- vn[-yvar]
    xlvl <- lapply(data[vn], function(x)
      if (is.factor(x))
        levels(x)
      else if (is.character(x))
        levels(as.factor(x))
      else NULL)
    attr(out, 'xlevels') <- xlvl[!vapply(xlvl, is.null, NA)]
    attr(tt, 'dataClasses') <- sapply(data[vn], .MFclass)
  }
  out$terms <- tt
  coef <- numeric(0)
  stopifnot(length(dots) > 1, !is.null(names(dots)))
  for (ii in seq_along(dots)) {
    if ((n <- names(dots)[ii]) != '') {
      v <- dots[[ii]]
      if (!is.null(names(v))) {
        coef[paste0(n, names(v))] <- v
      } else {
        stopifnot(length(v) == 1)
        coef[n] <- v
      }
    } else {
      coef['(Intercept)'] <- dots[[ii]]
    }
  }
  out$coefficients <- coef
  out$rank <- length(coef)
  if (!missing(family)) {
    out$family <- if (class(family) == 'family') {
      family
    } else if (class(family) == 'function') {
      family()
    } else if (class(family) == 'character') {
      get(family)()
    } else {
      stop(paste('Invalid family class:', class(family)))
    }
    out$qr <- list(pivot = seq_len(out$rank))
    out$deviance <- 1
    out$null.deviance <- 1
    out$aic <- 1
    class(out) <- c('glm','lm')
  } else {
    class(out) <- 'lm'
    out$fitted.values <- predict(out, newdata = data)
    out$residuals <- out$mf[attr(tt, 'response')] - out$fitted.values
    out$df.residual <- nrow(data) - out$rank
    out$model <- data
    ## qr doesn't work
  }
  out
}

#' GCD
#' 
#' Find greatest common divisor of two integers.
#' 
#' @param x,y integers
#' 
#' @examples
#' gcd(99, 2048)
#' gcd(2 ** (1:12), 2048)
#' 
#' @export

gcd <- function(x, y) ifelse(r <- x %% y, Recall(y, r), y)

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
  source('http://bioconductor.org/biocLite.R')
  f <- function(...)
    biocLite(..., suppressAutoUpdate = TRUE, suppressUpdates = TRUE)
  if (upgrade) {
    biocLite(suppressAutoUpdate = TRUE, suppressUpdates = FALSE)
    return(invisible())
  }
  if (missing(pkgs))
    f() else f(pkgs)
  invisible()
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
#' (cc <- with(mtcars, cor(mpg, wt)))
#' lm.beta(lm(mpg ~ wt, data = mtcars))
#' 
#' cc ** 2
#' summary(lm(mpg ~ wt, data = mtcars))$r.squared
#' 
#' lm.beta(lm(mpg ~ wt + disp + factor(vs), data = mtcars), weights = 2)
#' 
#' @export

lm.beta <- function (x, weights = 1) {
  stopifnot(inherits(x, 'lm'))
  b  <- coef(x)[-1]
  mf <- x$model
  sx <- vapply(mf[, -1, drop = FALSE], sd, double(1))
  sy <- vapply(mf[,  1, drop = FALSE], sd, double(1))
  b * sx / sy * weights
}

#' Wilcoxon rank sum test for trend of ordered groups
#' 
#' An implementation of Cuzick's extension of the Wilcoxon rank sum test to
#' assess trend in data with three or more \emph{ordinal} groups.
#' 
#' Data are assumed to be from independent groups with a natural or
#' meaningful order. If \code{x} and \code{g} are given as vectors and
#' \code{g} is a factor, the order of \code{levels(g)} will be respected;
#' however, if \code{g} is \emph{not} a factor, it will be coerced, and the
#' levels will be the same as \code{levels(factor(g))}.
#' 
#' For example, if \code{g} is \code{c("5mg", "10mg", "15mg")}, then the
#' groups will be ordered as \code{"10mg" "15mg" "5mg"} which may not be
#' desired.
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
#' @param g a vector or factor object giving the group for the corresponding
#' elements of \code{x}, ignored with a warning if \code{x} is a list; if
#' \code{g} is \emph{not} a factor, it will be coerced, and groups will be
#' ordered as \code{sort(unique(g))}; see \code{\link{factor}}
#' @param details \code{FALSE} or a function to compute comparisons between
#' pairs of groups; see details
#' @param formula a formula of the form \code{response ~ group} where
#' \code{response} gives the data values and \code{group} a vector or factor
#' of the corresponding groups
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in \code{formula}; by
#' default the variables are taken from \code{environment(formula)}
#' @param subset an optional vector specifying a subset of observations to be
#' used
#' @param na.action a function which indicates what should happen when the
#' data contain \code{NA}s; defaults to \code{getOption("na.action")}
#' @param ... additional arguments passed to the function given by
#' \code{details}
#' 
#' @return
#' A list with class "\code{htest}" containing the following components:
#' 
#' \item{statistic}{the value of the test statistic with a name describing it}
#' \item{p.value}{the p-value for the test (two-sided, corrected for ties)}
#' \item{estimate}{the medians by group}
#' \item{method}{a character string describing the test used}
#' \item{data.name}{a character string giving the names of the data}
#' \item{details}{a list of pairwise (\code{details}) and overall
#' (\code{\link{kruskal.test}}) comparisons}
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
#' x <- list(c(1.4, 1.4, 1.4, 1.6, 2.3, 2.3),
#'           c(0.9, 1.0, 1.1, 1.1, 1.2, 1.2,
#'             1.5, 1.9, 2.2, 2.6, 2.6, 2.6,
#'             2.8, 2.8, 3.2, 3.5, 4.3, 5.1),
#'           c(0.8, 1.7, 1.7, 1.7, 3.4, 7.1, 8.9, 13.5))
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
#' cuzick.test(x, details = coin::wilcox_test)$details
#' 
#' 
#' ## Cuzick (1985), 87-90
#' ## mice inoculated with five cell lines which had been selected for their
#' ## increasing metastatic potential; number of lung metastases found in each
#' ## mouse after inoculation
#' 
#' x <- c(0, 0, 1, 1, 2, 2, 4, 9, 0, 0, 5, 7, 8, 11, 13, 23,
#'        25, 97, 2, 3, 6, 9, 10, 11, 11, 12, 21, 0, 3, 5, 6,
#'        10, 19, 56, 100, 132, 2, 4, 6, 6, 6, 7, 18, 39, 60)
#' g <- rep(1:5, c(8,10,9,9,9))
#' cuzick.test(x, g)
#' 
#' 
#' ## coercing character group vector g to factor may have undesired order
#' set.seed(1)
#' x <- sort(rnorm(20))
#' g1 <- sample(paste0(c(5,10,15), 'mg'), 20, replace = TRUE)
#' g2 <- factor(g1, levels = paste0(c(5,10,15), 'mg'))
#' 
#' p1 <- cuzick.test(x, g1)$p.value
#' p2 <- cuzick.test(x, g2)$p.value
#' tplot(x ~ g1, data.frame(x, g1), type = 'db',
#'       panel.first = title(sub = pvalr(p1, show.p = TRUE)))
#' tplot(x ~ g2, data.frame(x, g2), type = 'db',
#'       panel.first = title(sub = pvalr(p2, show.p = TRUE)))
#' 
#' 
#' ## groups need not be equally-spaced but will affect statistic/pvalue
#' set.seed(1)
#' x <- sort(rnorm(20))
#' g1 <- sample(1:3, 20, replace = TRUE)
#' g2 <- g1 + (g1 == 3)
#' 
#' p1 <- cuzick.test(x, g1)$p.value
#' p2 <- cuzick.test(x, g2)$p.value
#' tplot(x ~ g1, dd <- data.frame(x, g1), type = 'db', at = sort(unique(g1)),
#'       panel.first = title(sub = pvalr(p1, show.p = TRUE)))
#' tplot(x ~ g2, dd <- data.frame(x, g2), type = 'db', at = sort(unique(g2)),
#'       panel.first = title(sub = pvalr(p2, show.p = TRUE)))
#' 
#' @export

cuzick.test <- function(x, ...) UseMethod('cuzick.test')

#' @rdname cuzick.test
#' @export
cuzick.test.default <- function(x, g, details = wilcox.test, ...) {
  ## checks adapted from stats:::kruskal.test.default
  if (is.list(x)) {
    if (length(x) < 2L)
      stop('\'x\' must be a list with at least 2 elements')
    if (!missing(g))
      warning('\'x\' is a list, so ignoring argument \'g\'')
    dname <- deparse(substitute(x))
    x <- lapply(x, function(u) u <- u[complete.cases(u)])
    if (!all(sapply(x, is.numeric)))
      warning('some elements of \'x\' are not numeric and will be coerced')
    l <- lengths(x)
    if (any(l == 0L))
      stop("all groups must contain data")
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
  if (length(x) < 2L)
    stop('not enough observations')
  
  if (is.factor(g) || is.character(g)) {
    fac <- TRUE
    g <- as.factor(g)
    if (nlevels(g) != nlevels(g <- droplevels(g)))
      warning('unused factor level(s) dropped')
  } else fac <- FALSE
  
  ux <- length(unique(x))
  ug <- length(unique(g))
  if (!is.numeric(x))
    stop('\'x\' values must be numeric')
  if (ug < 3L)
    stop('three or more unique groups are required')
  if (ug > ux)
    warning('more unique groups than unique response values')
  
  ## scores for each group
  ## if g is character or factor, use 1,2,...,k
  ## if g is numeric, dont assume equally-spaced, use sorted unique values
  li <- if (fac) seq.int(ug) else as.character(sort(unique(g)))
  ni <- table(g)[li]
  N  <- sum(ni)
  
  ## sum of ranks of x for ith group; sum of weighted sum of ith group scores
  li <- as.numeric(li)
  Ri <- tapply(rank(x), g, sum)
  L  <- sum(li * ni)
  
  ## T statistic, expected value, variance
  T  <- sum(li * Ri)
  eT <- (N + 1) * L / 2
  vT <- (N + 1) / 12 * (N * sum(li ** 2 * ni) - L ** 2)
  
  ## correction for ties
  ## tj: times each value of x appears; a: correction for se
  tj <- ave(x, x, FUN = length)
  a  <- sum(tj * (tj ** 2 - 1)) / (N * (N ** 2 - 1))
  se_corrected <- sqrt(1 - a) * sqrt(vT)
  se_uncorrected <- sqrt(vT)
  
  ## corrected test statistic: (T - expected) / se
  correct <- TRUE
  z <- (T - eT) / c(se_uncorrected, se_corrected)[correct + 1L]

  estimate <- tapply(x, g, median)
  names(estimate) <- paste('median of', names(estimate))
  method <- sprintf('Wilcoxon rank sum test for trend in %s ordered groups', ug)
  pval <- 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE))
  
  ## pairwise details
  pw <- if (!identical(details, FALSE)) {
    l2df <- function(l)
      data.frame(unlist(l), factor(rep(seq_along(l), lengths(l))))
    tidy <- function(l)
      data.frame(Filter(length, unclass(l)), stringsAsFactors = FALSE)
    
    sp  <- split(x, g)
    idx <- as.list(data.frame(combn(length(sp), 2)))
    ids <- apply(combn(names(ni), 2), 2, paste, collapse = ' vs ')
    sp  <- lapply(idx, function(x) setNames(l2df(sp[x]), c('x', 'g')))
    names(sp) <- ids
    
    pw <- lapply(sp, function(X) suppressWarnings(details(x ~ g, X, ...)))
    PW <- tryCatch(lapply(pw, tidy), error = function(e) NULL)
    
    list(pairs = if (is.null(PW)) pw else
      `rownames<-`(cbind(pairs = ids, do.call('rbind', PW)), NULL),
      overall = tidy(kruskal.test(x, g)))
  } else NULL
  
  structure(list(statistic = c(z = z), p.value = pval, estimate = estimate,
                 method = method, data.name = dname,
                 details = pw),
            class = 'htest')
}

#' @rdname cuzick.test
#' @export
cuzick.test.formula <- function (formula, data, subset, na.action, ...) {
  ## adapted from stats:::kruskal.test.formula
  if (missing(formula) || (length(formula) != 3L))
    stop('\'formula\' missing or incorrect')
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame())
  if (length(mf) > 2L)
    stop('\'formula\' should be of the form response ~ group')
  dname <- paste(names(mf), collapse = ' by ')
  names(mf) <- NULL
  y <- do.call('cuzick.test', as.list(mf))
  y$data.name <- dname
  y
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
#' A list with class "\code{htest}" containing the following components:
#' 
#' \item{statistic}{the value of the test statistic with a name describing it}
#' \item{p.value}{the p-value for the test (two-sided, corrected for ties)}
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
#' (tbl <- table(mtcars$gear, mtcars$cyl))
#' jt.test(tbl)
#' cor.test(mtcars$gear, mtcars$cyl, method = 'kendall')
#' # clinfun::jonckheere.test(mtcars$gear, mtcars$cyl)
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
  dname <- deparse(substitute(x))
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
  vS <- (2 * (n ** 3 - sum(ti ** 3) - sum(ui ** 3)) +
             3 * (n ** 2 - sum(ti ** 2) - sum(ui ** 2)) + 5 * n) / 18 +
    (sum(ti ** 3) - 3 * sum(ti ** 2) + 2 * n) *
    (sum(ui ** 3) - 3 * sum(ui ** 2) + 2 * n) / (9 * n * (n - 1) * (n - 2)) +
    (sum(ti ** 2) - n) * (sum(ui ** 2) - n) / (2 * n * (n - 1))
  
  ## statistic
  ns <- c(x)
  sp <- c(row(x) * !!ns)
  sp <- split(rep(sp, ns), rep(c(col(x)), ns))
  PQ <- rowSums(vapply(seq_len(length(sp)), function(x)
    get_PQ(sp[x], sp[-(1:x)]), integer(2)))
  z <- (PQ[1] - PQ[2]) / sqrt(vS)
  pval <- 2 * min(pnorm(z), pnorm(z, lower.tail = FALSE))
  
  structure(list(statistic = c(z = z), p.value = pval,
                 method = method, data.name = dname),
            class = 'htest')
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
#' (hl <- hl_est(x))
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
  n <- as.integer(n)
  x <- combn(x, n)
  apply(x, 2, FUN, ...)
}

#' \code{rpart} utilities
#' 
#' @description
#' Utilities for the \pkg{\link{rpart}} package:
#' 
#' \code{rpart_parent} returns all parent nodes of \code{node}.
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
#' \url{http://stackoverflow.com/questions/36086990/how-to-climb-the-tree-structure-of-rpart-object-using-path-in-order-to-purge-man}
#' 
#' \url{http://stackoverflow.com/questions/36748531/getting-the-observations-in-a-rparts-node-i-e-cart}
#' 
#' @examples
#' rpart_parent(116)
#' rpart_parent(29)
#' 
#' \dontrun{
#' library('rpart')
#' (fit <- rpart(Kyphosis ~ Age + Number + Start, kyphosis, minsplit = 5))
#' 
#' ## children nodes should have identical paths
#' identical(head(rpart_parent(28), -1L), head(rpart_parent(29), -1L))
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
#' ## note differences in nodes labels in party vs rpart
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

#' @rdname rpart_utils
#' @export
rpart_parent <- function(node = 1L) {
  if (node[1L] != 1L)
    c(Recall(if (node %% 2 == 0L) node / 2 else (node - 1) / 2), node)
  else node
}

#' @rdname rpart_utils
#' @export
rpart_subset <- function(tree, node = 1L) {
  nodes <- sort(as.integer(rownames(tree$frame)))
  rn    <- rpart_nodes(tree, FALSE)
  
  if (length(nn <- node[node %ni% nodes]))
    stop('Node ', toString(nn), ' not found\n\nPossibilities are ',
         iprint(nodes, digits = 0), '.\n')
  
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
