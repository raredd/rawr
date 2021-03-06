% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ctcae_v3}
\alias{ctcae_v3}
\title{Common Terminology Criteria for Adverse Events, v3}
\format{
A data frame with 1059 rows and 4 variables:

\tabular{lll}{
\tab \code{tox_code} \tab alphanumeric toxicity code (\code{AB123}) \cr
\tab \code{tox_desc} \tab description \cr
\tab \code{tox_cat} \tab category \cr
\tab \code{ctc_version} \tab version number \cr
}
}
\source{
\url{http://ctep.cancer.gov/protocolDevelopment/electronic_applications/docs/ctcaev3.pdf}

\url{https://ctep.cancer.gov/protocoldevelopment/electronic_applications/ctc.htm}
}
\usage{
ctcae_v3
}
\description{
A data set containing the CTCAE (version 3) toxicity codes, descriptions,
and categories.
}
\seealso{
\code{\link{ctcae_v4}}; \code{\link{ctcae_v5}}; \code{\link{match_ctc}};
\code{\link{tox_worst}}
}
\keyword{datasets}
