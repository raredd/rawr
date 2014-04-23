\name{html.test}
\alias{html.test}
\title{html test}
\usage{
html.test(...)
}
\arguments{
  \item{...}{character string of html code}
}
\description{
View html in rstudio viewer
}
\examples{
html.test("
<div align = center><h1>A heading<sup>&dagger;</sup><h1></div>
<font size = 1><sup>&dagger;</sup>That was the heading</font>
")

library(Gmisc)
html.test(htmlTable(mtcars, output = FALSE))
}

