\name{oror}
\alias{\%||\%}
\alias{oror}
\title{oror}
\usage{
e1 \%||\% e2
}
\description{
function_that_may_return_null() or default value
}
\examples{
NULL || TRUE    # error
NULL \%||\% TRUE  # no error
}

