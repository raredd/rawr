\name{bintest}
\alias{bintest}
\title{Single-stage designs}
\usage{
bintest(p0low, p0high = p0low, p1low, p1high = p1low, n.max,
   r = n.max, alpha = 0.1, beta = 0.1)
}
\arguments{
  \item{p0low}{start value of p0}

  \item{p0high}{highest value of p0}

  \item{p1low}{start value of p1}

  \item{p1high}{highest value of p1}

  \item{n.max}{maximum sample size allowed (or feasible)}

  \item{r}{cut-off value for responses expected; usually
  best to leave \code{r = n.max} (default); useful if you
  know a maximum number of responses that may occur}

  \item{alpha}{type I error rate}

  \item{beta}{type II error rate}
}
\value{
Sample size, overall power, and overall type I error rate
}
\description{
Function for sample sizes for single-stage designs in early
clinical trial phases (generally phases I and II) based on
exact binomial method.
}
\details{
Cycles through possible designs constrained by alpha, beta,
and n.max arguments for specified ranges of p0 and p1.
}
\examples{
bintest(p0low = .2, p1low = .5, n.max  = 25)

# example in sas macro
bintest(.1, .15, .2, .2, n.max = 80, alpha = .08, beta = .24)
}
\references{
Khan, Sarker, Hackshaw. Smaller sample sizes for phase II
trials based on exact tests with actual error rates by
trading-off their nominal levels of significance and power.
\emph{British J of Cancer} (2012) 107, 1801-9.
}
\seealso{
\code{\link[clinfun]{ph2single}} (clinfun package);
\code{\link{bin1samp}} (desmon package); bintest.sas (SAS
macro)
}

