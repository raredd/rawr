\name{search.hist}
\alias{search.hist}
\title{Search history}
\usage{
search.hist(..., fixed = FALSE)
}
\arguments{
  \item{...}{numeric or character; if numeric, shows the
  most recent \code{n} lines in \code{.Rhistory}; if
  character, searches for pattern matches}
}
\value{
Returns a list of recent commands that match \code{pattern}
}
\description{
Searches Rhistory file for pattern matches
}
\examples{
search.hist()
search.hist(25)
search.hist('?')
search.hist('?', fixed = TRUE)
search.hist('\\\\?')
}

