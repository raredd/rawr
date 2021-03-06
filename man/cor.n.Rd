% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat.R
\name{cor.n}
\alias{cor.n}
\alias{cor.ci}
\title{Sample size calculation for correlation}
\usage{
cor.n(
  rho = NULL,
  n = NULL,
  width = NULL,
  alpha = 0.05,
  two.sided = TRUE,
  method = c("bonett", "fieller"),
  tol = .Machine$double.eps^0.25
)

cor.ci(rho, n, alpha = 0.05, two.sided = TRUE, method = c("bonett", "fieller"))
}
\arguments{
\item{rho}{correlation coefficient}

\item{n}{sample size}

\item{width}{the width of the \code{1 - alpha} confidence interval}

\item{alpha}{type I error probability}

\item{two.sided}{logical; if \code{TRUE}, a two-sided \code{alpha} is used}

\item{method}{the method used to calculate the standard deviation, one of
\code{"bonett"} or \code{"fieller"} (can be (unambiguously) abbreviated)}

\item{tol}{numerical tolerance used in root finding, the default providing
(at least) four significant digits}
}
\description{
Compute the sample size for a given correlation coefficient, alpha level,
and confidence interval width. Alternatively, solve for the coefficient,
interval width, or alpha level given fixed parameters. \code{cor.ci}
calculated the confidence interval for a fixed correlation coefficient
and sample size.
}
\examples{
## table 1, bonett (spearman) -- solving for n
args <- data.frame(
  rho = 0.9,
  width = rep(1:3, each = 2) / 10,
  alpha = c(0.05, 0.01)
)

apply(args, 1L, function(x)
  ceiling(cor.n(x['rho'], NULL, x['width'], x['alpha'])))


## table 1, bonett (spearman)
cor.ci(0.1, 290, 0.01)
ceiling(cor.n(0.1, NULL, 0.3, 0.01))

cor.ci(0.5, 111, 0.05)
ceiling(cor.n(0.5, NULL, 0.3, 0.05))

}
\references{
\url{https://www.researchgate.net/profile/Douglas_Bonett/publication/279926406_Sample_size_requirements_for_estimating_Pearson_Kendall_and_Spearman_correlations/links/575de23908aed88462166f2e/Sample-size-requirements-for-estimating-Pearson-Kendall-and-Spearman-correlations.pdf}

\url{http://solarmuri.ssl.berkeley.edu/~schuck/public/manuscripts/Fieller1.pdf}

\url{https://ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/PASS/Confidence_Intervals_for_Spearmans_Rank_Correlation.pdf}

\url{https://stats.stackexchange.com/a/18904}
}
