### statistical functions
# bincon, bintest, table, power_cv, simon2, moods_test, fakeglm
###


#' Binomial probability confidence intervals
#' 
#' Calculates confidence intervales for binomial probabilities for specified 
#' type-I error (\code{alpha}) using exact, Wilson, or asymptotic methods.
#'    
#' @param r number of responses (successes)
#' @param n number of observations (trials)
#' @param alpha type-I error probability
#' @param round integer value specifying number of decimal places to round 
#' (default is no rounding); see \code{\link{round}}
#' @param method character strings specifying which method to use; see details
#' @param inc.r logical; include \code{r} in return
#' @param inc.n logical; include \code{n} in return
#' @param df logical; coerce return matrix to data frame
#' 
#' @details
#' The "all" option, which shows results from each calculation method, only 
#' works where \code{r} and \code{n} are of length 1. The "exact" method uses 
#' the \code{\link{df}} distribution to comupte exact intervales (based on the 
#' binomial cdf); the "wilson" interval is score-test-based; the "asymptotic" 
#' is the asymptotic normal interval.
#' 
#' The "wilson" method has been preferred by Agresti and Coull.
#' 
#' @return A matrix or data frame containing the computed intervals, their 
#' widths, and, optionally, \code{r} and \code{n}.
#' @author Rollin Brant, Frank Harrell, and Brad Biggerstaff; modifications by 
#' Robert Redd
#' @references Agresti, A. and B.A. Coull. Approximate is better than "exact" 
#' for interval extimation of binomial proportions. \emph{American 
#' Statistician}. \strong{59}:119-126, 1998.
#' @references Brown, L.D., T.T. Cai, and A. Das Gupta. Inverval estimation for
#' a binomial proportion (with discussion). \emph{Statistical Science}. 
#' \strong{16}:101-133, 2001.
#' @references Newcombe, R.G. Logit confidence intervals and the inverse sinh 
#' transformation, \emph{American Statistician}. \strong{55}:200-202, 2001.
#' @seealso \code{\link[Hmisc]{binconf}}, \code{binci} (desmon package)
#' 
#' @examples
#' bincon(0:10, 10)
#' bincon(5, 10, method = 'all')
#' 
#' @export

bincon <- function(r, n, alpha = 0.05, round = NULL,
                   method = c('exact','wilson','asymptotic','all'), 
                   inc.r = TRUE, inc.n = TRUE, df = FALSE) {
  
  # error checks
  if (any(r < 0) | any(r > n)) stop('invalid response value')
  if (alpha >= 1 | alpha <= 0) stop('alpha must be between 0 and 1')
  
  method <- match.arg(method)
  bc <- function(r, n, alpha, method) {
    nu1 <- 2 * (n - r + 1)
    nu2 <- 2 * r
    ll <- if (r > 0) 
      r / (r + qf(1 - alpha / 2, nu1, nu2) * (n - r + 1))
    else 0
    nu1p <- nu2 + 2
    nu2p <- nu1 - 2
    pp <- if (r < n) 
      qf(1 - alpha / 2, nu1p, nu2p)
    else 1
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
    warning("method = 'all' will not work with vectors... 
            setting method = 'exact' ")
  }
  if (method == 'all' & length(r) == 1 & length(n) == 1) {
    mat <- bc(r, n, alpha, method)
    mat <- cbind(mat, mat[ ,3] - mat[ ,2])
    dimnames(mat) <- list(c('Exact', 'Wilson', 'Asymptotic'), 
                          c('PointEst', 'Lower', 'Upper','Width'))
    if (inc.n) 
      mat <- cbind(Trials = n, mat)
    if (inc.r) 
      mat <- cbind(Responses = r, mat)
    if (df) 
      mat <- as.data.frame(mat)
    return(mat)
  }
  
  mat <- matrix(ncol = 3, nrow = length(r))
  for (i in 1:length(r)) 
    mat[i, ] <- bc(r[i], n[i], alpha = alpha, method = method)
  
  mat <- cbind(mat, mat[ ,3] - mat[ ,2])
  
  colnames(mat) <- c('PointEst','Lower','Upper','Width')
  
  if (!is.null(round))
    mat[ ,2:4] <- round(mat[ ,2:4], digits = round)
  
  if (inc.n) 
    mat <- cbind(Trials = n, mat)
  if (inc.r) 
    mat <- cbind(Responses = r, mat)
  if (df) 
    mat <- as.data.frame(mat, row.names = NULL)
  
  mat
  }

#' Single-stage designs
#' 
#' Function for sample sizes for single-stage designs in early clinical trial 
#' phases (generally phases I and II) based on exact binomial method.
#' 
#' @param p0low start value of p0
#' @param p0high highest value of p0
#' @param p1low start value of p1
#' @param p1high highest value of p1
#' @param n.max maximum sample size allowed (or feasible)
#' @param r cut-off value for responses expected; usually best to leave 
#' \code{r = n.max} (default); useful if you know a maximum number of responses
#' that may occur
#' @param alpha type-I error rate
#' @param beta type-II error rate
#' 
#' @details Cycles through possible designs constrained by alpha, beta, and 
#' n.max arguments for specified ranges of p0 and p1.
#' @references Khan, Sarker, Hackshaw. Smaller sample sizes for phase II trials
#' based on exact tests with actual error rates by trading-off their nominal 
#' levels of significance and power. \emph{British J of Cancer} (2012) 107, 
#' 1801-9.
#' @return Sample size, overall power, and overall type-I error rate
#' @seealso \code{ph2single} (clinfun package); \code{bin1samp} (desmon 
#' package); bintest.sas (SAS macro)
#' 
#' @examples
#' bintest(p0low = .2, p1low = .5, n.max  = 25)
#' 
#' # example in sas macro
#' bintest(.1, .15, .2, .2, n.max = 80, alpha = .08, beta = .24)
#' 
#' @export

bintest <- function (p0low, p0high = p0low, p1low, p1high = p1low, n.max, 
                     r = n.max, alpha = .1, beta = .1) {
  
  if (r < 1) stop('ERROR: Number of responders must be >1')
  
  # find all combinations of null, alternative, sample size, and responses
  mat <- expand.grid(seq(p0low, p0high, by = .01), 
                     seq(p1low, p1high, .01), 
                     1:n.max, 
                     1:r)
  # we require > r responders out of n
  mat <- as.matrix(within(mat, r2 <- mat[ ,4] + 1))
  colnames(mat) <- c('p0','p1','n','r','r2')
  
  # remove instances where p1 <= p0 or r >= n
  mat <- mat[!(mat[ ,2] <= mat[ ,1] | mat[ ,4] >= mat[ ,3]), ]
  
  # Pr(<r2 responders|p0 true): pbinom(r2, n, p0)
  y <- pbinom(mat[ ,5], mat[ ,3], mat[ ,1])
  
  # Pr(>r2 responders for p0)
  type1 <- 1 - y
  
  # Pr(<r2 responders | p1 true): pbinom(r2, n, p1): type-II error
  z <- pbinom(mat[ ,5], mat[ ,3], mat[ ,2])
  
  # Pr(>r2 responders for p1): 1 - type-II error
  power <- 1 - z
  
  # signal: p1 - p0
  diff <- mat[ ,2] - mat[ ,1]
  
  mat <- cbind(mat, y, type1, z, power, diff)
  
  alpha <- rep(alpha, times = dim(mat)[1])
  beta  <- rep(beta, times = dim(mat)[1])
  
  # keep occurences that fit type-I/II error constraints
  mat <- mat[mat[ ,9] > (1 - beta) & mat[ ,7] < alpha, ]
  
  # select pertinent variables
  mat <- unique(as.data.frame(mat[ ,c(1:3, 5, 7, 9:10)]))
  mat <- mat[order(mat$n), ]
  
  list(designs = mat,
       call = match.call(),
       description = 
         c('n = overall sample size',
           'r2 = minimum number of responders required to reject p0',
           'diff = signal, difference in null and alternative hypotheses'))
}

#' DLT table
#' 
#' Creates a standard 3x3 dose-limiting toxicity table with probabilities of
#' dose-escalation.
#' 
#' @param low lowest true DLT rate, percent
#' @param high highest true DLT rate, percent
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
#' @param n number of observations (per group)
#' @param f ratio of means (>1)
#' @param cv coefficient of variation
#' @param sig.level significance level (type-I error probability)
#' @param power power of test (1 - type-II error probability)
#' @param type type of t test
#' @param alternative one- or two-sided test
#' @param distribution underlying distribution assumption
#' 
#' @details Exactly one of n, f, cv, sig.level, and power must be NULL
#' @return Object of class "\code{power.htest}," a list of the arguments
#' (including the computed one) augmented with method and note elements
#' @note \code{\link{uniroot}} is used to solve power equation for unknowns, 
#' so you may see errors from it, notably about inability to bracket the root 
#' when invalid arguments are given.
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
  if (f < 1 ) {
    f <- 1 / f
    warning('ratio of means must be such that mu1/mu0 > 1: 1/f used')
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
  
  # assuming underlying t distribution
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
  
  # assuming underlying normal distribution
  if (dist == 'normal') {
    p.body <- quote({
      qt <- qnorm(p = sig.level/tside, lower.tail = FALSE)
      pnorm(q = -qt + (sqrt(n)*(f-1))/(cv*(f**2+1)**.5), lower.tail = TRUE)
    })
  }
  
  # assuming underlying lognormal distribution with unknown variance
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
  
  # calculate missing parameter
  if (is.null(power)) power <- eval(p.body)
  else if (is.null(n)) 
    n <- ifelse(dist=='log.normal',ceiling(uniroot(function(n) 
      eval(p.body) - power, c(2, 1e+07))$root),uniroot(function(n) 
        eval(p.body) - power, c(2, 1e+07))$root)
  else if (is.null(cv)) 
  {
    if (ttside == 2)
      cv <- uniroot(function(cv) eval(p.body) - power, f * c(1e-07, 10))$root
    if (ttside == 1)
      cv <- uniroot(function(cv) eval(p.body) - power, f * c(-10, 5))$root
    if (ttside == 3)
      cv <- uniroot(function(cv) eval(p.body) - power, f * c(-5, 10))$root
  }
  else if (is.null(f)) 
  {
    if (ttside == 2)
      f <- uniroot(function(f) eval(p.body) - power, cv * c(1e-07, 10))$root
    if (ttside == 1)
      f <- uniroot(function(f) eval(p.body) - power, cv * c(-10, 5))$root
    if (ttside == 3)
      f <- uniroot(function(f) eval(p.body) - power, cv * c(-5, 10))$root 
  }
  else if (is.null(sig.level)) 
    sig.level <- uniroot(function(sig.level) eval(p.body) - power, 
                         c(1e-10, 1 - 1e-10))$root
  else stop("internal error")
  
  # return
  NOTE <- switch(type, 
                 paired = "n is number of *pairs*, cv is coefficient of variation of *differences* within pairs", 
                 two.sample = "n is number in *each* group", 
                 one.sample = NULL)
  METHOD <- paste(switch(type, 
                         one.sample = "One-sample", 
                         two.sample = "Two-sample", 
                         paired = "Paired"), 
                  "t test power calculation")
  structure(list(n = n, f = f, cv = cv, sig.level = sig.level, power = power, 
                 alternative = alternative, 
                 distributon = dist, 
                 note = NOTE, 
                 method = METHOD),
            class = "power.htest")
}

#' Simon two-stage designs
#' 
#' Function for sample sizes for Simon optimal two-stage, single-arm designs.
#'    
#' @param p0 null hypothesis response probability
#' @param pa alternative hypothesis response probability
#' @param n1max maximum number of subjects entered during the first stage; 
#' ignored if <= 0
#' @param ntmax maximum total number of total subjects
#' @param alpha type-I error rate
#' @param beta type-II error rate
#' @param del searches for designs where the expected number of subjects under 
#' the null with within \code{del} of the minimum possible value
#' @param minimax logical; if \code{TRUE}, only searches for designs which will
#' minimize the maximum sample size
#' 
#' @details  For two-stage designs for studies with binary endpoints, searches 
#' over possible two-stage sampling designs to find those that minimize the 
#' expected number of subjects, subject to specified constraints.
#' 
#' @return
#' Returns a list with components:
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
#' @author Robert Gray (\code{simon}); Robert Redd (\code{simon2})
#' @references Simon R (1989). Optimal two-stage designs for phase II clinical 
#' trials. \emph{Controlled Clinical Trials}, 10:1-10.
#' 
#' @seealso desmon package: \code{simon}, \code{twostg}, \code{bin1samp},
#' \code{pickwin}, \code{rp21}
#' 
#' @examples
#' \donttest{
#' simon2(.2, c(.4, .5))
#' simon2(p0 = seq(.55, .6, by = .01), pa = .75, ntmax = 60)
#' 
#' ## compare this function to results from desmon::simon
#' simon2(.4, .6)
#' ## requires desmon package
#' simon(.4, .6)
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
                  sapply(seq_len(nrow(args)), function(x) clist(args[x, ])))
  
  list(designs = tmp,
       call = match.call(),
       description = c('n1, n2 = cases 1st stage and additional # in 2nd',
                       'r1, r2 = max # responses 1st stage and total to declare trt inactive'))
}

simon <- function(p0, pa, n1max = 0, ntmax = 1e5, alpha = 0.1, beta = 0.1,
                  del = 1, minimax = FALSE) {
  
  ## prob calculations for 2-stage phase II design
  ## optimal simon designs (minimize E(n|H0))
  ## p0, pa, null & alt response probabilities
  ## n1max = max # subject in first stage
  
  if (alpha > .5 | alpha <= 0 | 1 - beta <= alpha | beta <= 0 | p0 <= 0 | p0 >= pa | 
        pa >= 1 | n1max > ntmax) stop('invalid arguments')
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
       description = c('n1, n2 = cases 1st stage and additional # in 2nd',
                       'r1, r2 = max # responses 1st stage and total to declare trt inactive'))
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
#' @seealso \code{\link{mood.test}}, \code{\link{kruskal.test}}, 
#' \code{\link{wilcox.test}}
#' 
#' @examples
#' set.seed(1618)
#' X <- list(rnorm(10), rnorm(10, 1), rnorm(20, 2))
#' moods_test(X)
#' 
#' plot(density(X[[1]]), xlim = range(unlist(X)), ylim = c(0, .5))
#' for (x in 2:3) lines(density(X[[x]]), col = x)
#' 
#' @export

moods_test <- function(X, ...) {
  x <- unlist(X)
  g <- rep(ng <- seq_len(length(X)), times = sapply(X, length))
  m <- median(x)
  zzz <- fisher.test(x < m, g, ...)
  zzz$method <- sprintf('Mood\'s median test of %s groups', length(ng))
  zzz$data <- table(x < m, g, dnn = c('< median', 'group'))
  return(zzz)
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
#' @references \url{https://gist.github.com/MrFlick/ae299d8f3760f02de6bf}
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
  stopifnot(length(dots) > 1 & !is.null(names(dots)))
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
