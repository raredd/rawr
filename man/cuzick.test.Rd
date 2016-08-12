% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat.R
\name{cuzick.test}
\alias{cuzick.test}
\alias{cuzick.test.default}
\alias{cuzick.test.formula}
\title{Wilcoxon rank sum test for trend of ordered groups}
\usage{
cuzick.test(x, ...)

\method{cuzick.test}{default}(x, g, details = wilcox.test, ...)

\method{cuzick.test}{formula}(formula, data, subset, na.action, ...)
}
\arguments{
\item{x}{a numeric vector of data values or a list of numeric data vectors;
non-numeric elements of a list will be coerced with a warning; if \code{x}
is a list, the list elements are assumed to be groups ordered as
\code{x[[1]], x[[2]], ..., x[[n]]}}

\item{...}{additional arguments passed to the function given by
\code{details}}

\item{g}{a vector or factor object giving the group for the corresponding
elements of \code{x}, ignored with a warning if \code{x} is a list; if
\code{g} is \emph{not} a factor, it will be coerced, and groups will be
ordered as \code{sort(unique(g))}; see \code{\link{factor}}}

\item{details}{\code{FALSE} or a function to compute comparisons between
pairs of groups; see details}

\item{formula}{a formula of the form \code{response ~ group} where
\code{response} gives the data values and \code{group} a vector or factor
of the corresponding groups}

\item{data}{an optional matrix or data frame (or similar: see
\code{\link{model.frame}}) containing the variables in \code{formula}; by
default the variables are taken from \code{environment(formula)}}

\item{subset}{an optional vector specifying a subset of observations to be
used}

\item{na.action}{a function which indicates what should happen when the
data contain \code{NA}s; defaults to \code{getOption("na.action")}}
}
\value{
A list with class "\code{htest}" containing the following components:

\item{statistic}{the value of the test statistic with a name describing it}
\item{p.value}{the p-value for the test (two-sided, corrected for ties)}
\item{estimate}{the medians by group}
\item{method}{a character string describing the test used}
\item{data.name}{a character string giving the names of the data}
\item{details}{a list of pairwise (\code{details}) and overall
(\code{\link{kruskal.test}}) comparisons}
}
\description{
An implementation of Cuzick's extension of the Wilcoxon rank sum test to
assess trend in data with three or more \emph{ordinal} groups.
}
\details{
Data are assumed to be from independent groups with a natural or
meaningful order. If \code{x} and \code{g} are given as vectors and
\code{g} is a factor, the order of \code{levels(g)} will be respected;
however, if \code{g} is \emph{not} a factor, it will be coerced, and the
levels will be the same as \code{levels(factor(g))}.

For example, if \code{g} is \code{c("5mg", "10mg", "15mg")}, then the
groups will be ordered as \code{"10mg" "15mg" "5mg"} which may not be
desired.

Pairwise comparisons between each pair of groups is performed using the
function given by \code{details} (default is \code{\link{wilcox.test}});
the overall will be compared by \code{\link{kruskal.test}}. Other functions
may be used (e.g., in cases with ties), but these need a formula method
similar to \code{cuzick.test}, \code{wilcox.test}, etc. Common methods such
as \code{\link[coin]{wilcox_test}} or \code{\link[coin]{kruskal_test}} from
the \pkg{\link[coin]{coin}} package will work; see examples.
}
\examples{
## Altman (1991), 217
## ocular exposure to ultraviolet radiation for 32 pairs of sunglasses
## classified into three groups according to the amount of visible light
## transmitted

x <- list(c(1.4, 1.4, 1.4, 1.6, 2.3, 2.3),
          c(0.9, 1.0, 1.1, 1.1, 1.2, 1.2,
            1.5, 1.9, 2.2, 2.6, 2.6, 2.6,
            2.8, 2.8, 3.2, 3.5, 4.3, 5.1),
          c(0.8, 1.7, 1.7, 1.7, 3.4, 7.1, 8.9, 13.5))

## equivalent ways to call cuzick.test
cuzick.test(x)
cuzick.test(unlist(x), rep(seq(x), lengths(x)))
cuzick.test(x ~ g, data.frame(x = unlist(x), g = rep(1:3, lengths(x))))


## all pairwise comparisons are returned by default in $details
cuzick.test(x)$details
cuzick.test(x, alternative = 'less', correct = FALSE)$details
cuzick.test(x, details = kruskal.test)$details
cuzick.test(x, details = coin::wilcox_test)$details


## Cuzick (1985), 87-90
## mice inoculated with five cell lines which had been selected for their
## increasing metastatic potential; number of lung metastases found in each
## mouse after inoculation

x <- c(0, 0, 1, 1, 2, 2, 4, 9, 0, 0, 5, 7, 8, 11, 13, 23,
       25, 97, 2, 3, 6, 9, 10, 11, 11, 12, 21, 0, 3, 5, 6,
       10, 19, 56, 100, 132, 2, 4, 6, 6, 6, 7, 18, 39, 60)
g <- rep(1:5, c(8,10,9,9,9))
cuzick.test(x, g)


## coercing character group vector g to factor may have undesired order
set.seed(1)
x <- sort(rnorm(20))
g1 <- sample(paste0(c(5,10,15), 'mg'), 20, replace = TRUE)
g2 <- factor(g1, levels = paste0(c(5,10,15), 'mg'))

p1 <- cuzick.test(x, g1)$p.value
p2 <- cuzick.test(x, g2)$p.value
tplot(x ~ g1, data.frame(x, g1), type = 'db',
      panel.first = title(sub = pvalr(p1, show.p = TRUE)))
tplot(x ~ g2, data.frame(x, g2), type = 'db',
      panel.first = title(sub = pvalr(p2, show.p = TRUE)))


## groups need not be equally-spaced but will affect statistic/pvalue
set.seed(1)
x <- sort(rnorm(20))
g1 <- sample(1:3, 20, replace = TRUE)
g2 <- g1 + (g1 == 3)

p1 <- cuzick.test(x, g1)$p.value
p2 <- cuzick.test(x, g2)$p.value
tplot(x ~ g1, dd <- data.frame(x, g1), type = 'db', at = sort(unique(g1)),
      panel.first = title(sub = pvalr(p1, show.p = TRUE)))
tplot(x ~ g2, dd <- data.frame(x, g2), type = 'db', at = sort(unique(g2)),
      panel.first = title(sub = pvalr(p2, show.p = TRUE)))

}
\references{
Altman, D. G. 1991. \emph{Practical Statistics for Medical Research}.
London: Chapman & Hall/CRC.

Cuzick, J. 1985. A Wilcoxon-type test for trend. \emph{Statistics in
Medicine} \strong{4}: 87-90.
}
\seealso{
\code{\link{kruskal.test}}; \code{\link{wilcox.test}}

\code{\link[coin]{kruskal_test}}; \code{\link[coin]{wilcox_test}} from the
\pkg{\link[coin]{coin}} package
}
