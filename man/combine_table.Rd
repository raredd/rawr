% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils2.R
\name{combine_table}
\alias{combine_table}
\alias{combine_table2}
\title{Combine html tables}
\usage{
combine_table(x, tspanner, n.tspanner, ...)

combine_table2(
  x,
  tspanner,
  n.tspanner,
  cgroup,
  n.cgroup,
  how = c("rbind", "cbind"),
  ...
)
}
\arguments{
\item{x}{a list of matrices or data frames}

\item{tspanner, n.tspanner}{table spanner labels and number of rows,
respectively, passed to \code{\link[htmlTable]{htmlTable}}; if missing,
\code{names(x)} and \code{sapply(x, nrow)} are used}

\item{...}{additional arguments passed to \code{\link[htmlTable]{htmlTable}}}

\item{cgroup, n.cgroup}{table column labels and number of columns for each,
respectively, passed to \code{\link[htmlTable]{htmlTable}}; if missing,
\code{names(x)} and \code{sapply(x, ncol)} are used}

\item{how}{method to join objects, by row (\code{"rbind"}) or column
(\code{"cbind"}) binding}
}
\description{
Wrapper to easily combine a list of data frames or matrices into html
tables using the \pkg{htmlTable} package. \code{combine_table2} can join
tables vertically or horizontally (common column and row names are not
required).
}
\examples{
sp <- lapply(split(mtcars, rep(1:3, c(1, 11, 20))), as.matrix)

## basic table
combine_table(sp)
combine_table(sp, letters[1:3], c(2, 5, 25))

## adding more options
combine_table(
  caption = 'Table 1: <code>mtcars</code> data set',
  sp, tspanner = sapply(sp, function(x) num2char(nrow(x))),
  css.cell = 'padding: 0 10 5px;',
  css.tspanner = 'text-align: center; color: red; font-style: italic;'
)

combine_table2(sp, how = 'c', cgroup = LETTERS[1:3])

}
