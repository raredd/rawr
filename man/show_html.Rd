% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils2.R
\name{show_html}
\alias{show_html}
\title{Show HTML}
\usage{
show_html(..., use_viewer = !is.null(getOption("viewer")))
}
\arguments{
\item{...}{one or more character strings}

\item{use_viewer}{logical; if \code{TRUE}, attempts to use
\code{rstudioapi::viewer} or opens in default browser on error}
}
\value{
The html code (invisibly) as a character string.
}
\description{
Render html in rstudio viewer or browser.
}
\examples{
\dontrun{
show_html("
<div align = center><h1>A heading<sup>&dagger;</sup><h1></div>
<font size = 1><sup>&dagger;</sup>That was the heading</font>
")

library('htmlTable')
show_html(htmlTable(mtcars, output = FALSE), use_viewer = FALSE)
}

}
\seealso{
\code{\link{show_math}}, \code{\link{show_markdown}}
}
