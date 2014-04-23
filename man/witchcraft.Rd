\name{witchcraft}
\alias{witchcraft}
\title{Simple memory game}
\usage{
witchcraft(level = 1, seed = NULL)
}
\arguments{
  \item{level}{difficulty; choose levels 0 (easy) to 3
  (difficult)}

  \item{seed}{seed; see \code{\link{set.seed}}}
}
\value{
Interactive game

A prize.
}
\description{
Pick boxes in the correct numerical order to win a prize.
}
\details{
Set the seed to play the same game.
}
\examples{
witchcraft()
witchcraft(level = 3)
}

