\name{dlt.table}
\alias{dlt.table}
\title{DLT table}
\usage{
dlt.table(dlt.low, dlt.high. delta = 10)
}
\arguments{
  \item{dlt.low}{lowest true DLT rate, percent}

  \item{dlt.high}{highest true DLT rate, percent}

  \item{delta}{interval of DLT rate sequence; default is
  10}
}
\description{
Creates a standard dose-limiting toxicity table with
probabilities of dose-escalation
}
\examples{
dlt.table(10, 50)
dlt.table(10, 50, 5)
}

