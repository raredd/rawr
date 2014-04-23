\name{ident}
\alias{ident}
\title{Test two or more objects for exact equality}
\usage{
ident(..., num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
   ignore.bytecode = TRUE, ignore.environment = FALSE)
}
\arguments{
  \item{...}{any \code{R} objects}

  \item{num.eq}{logical indicating if (\code{\link{double}}
  and \code{\link{complex}} non-\code{\link{NA}}) numbers
  should be compared using \code{\link{==}} ("equal"), or
  by bitwise comparison. The latter (non-default)
  differentiates between -0 and +0.}

  \item{single.NA}{logical indicating if there is
  conceptually just one numeric \code{NA} and one
  \code{\link{NaN}}; \code{single.NA = FALSE}
  differentiates bit patterns.}

  \item{attrib.as.set}{logical indicating if
  \code{\link{attributes}} of \code{...} should be treated
  as \emph{unordered} tagged pairlists ("sets"); this
  currently also applies to \code{\link{slot}}s of S4
  objects. It may well be too strict to set
  \code{attrib.as.set = FALSE}.}

  \item{ignore.bytecode}{logical indicating if byte code
  should be ignored when comparing \code{\link{closure}}s.}

  \item{ignore.environment}{logical indicating if their
  environments should be ignored when comparing
  \code{closure}s.}
}
\value{
A single logical value, \code{TRUE} or \code{FALSE}, never
\code{NA} and never anything other than a single value.
}
\description{
The safe and reliable way to test two or more objects for
being exactly equal; returns \code{TRUE} in this case,
\code{FALSE} in every other case.
}
\details{
Generalized, recursive \code{\link{identical}} function for
testing equality of two or more \code{R} objects.

A call to \code{identical} is the way to test exact
equality in \code{if} and \code{while} statements, as well
as in logical expressions that use \code{&&} or \code{||}.
In all these applications you need to be assured of getting
a single logical value.

Users often use the comparison operators, such as \code{==}
or \code{!=}, in these situations. It looks natural, but it
is not what these operators are designed to do in \code{R}.
They return an object like the arguments. If you expected
\code{x} and \code{y} to be of length 1, but it happened
that one of them was not, you will \emph{not} get a single
\code{FALSE}. Similarly, if one of the arguments is
\code{NA}, the result is also \code{NA}. In either case,
the expression \code{if(x == y)...} won't work as expected.

The function \code{all.equal} is also sometimes used to
test equality this way, but was intended for something
different: it allows for small differences in numeric
results.

The computations in \code{identical} are also reliable and
usually fast. There should never be an error. The only
known way to kill \code{identical} is by having an invalid
pointer at the C level, generating a memory fault. It will
usually find inequality quickly. Checking equality for two
large, complicated objects can take longer if the objects
are identical or nearly so, but represent completely
independent copies. For most applications, however, the
computational cost should be negligible.

If \code{single.NA} is true, as by default, identical sees
\code{NaN} as different from \code{\link{NA_real_}}, but
all \code{NaN}s are equal (and all \code{NA} of the same
type are equal).

Character strings are regarded as identical if they are in
different marked encodings but would agree when translated
to UTF-8.

If \code{attrib.as.set} is \code{TRUE}, as by default,
comparison of attributes view them as a set (and not a
vector, so order is not tested).

If \code{ignore.bytecode} is \code{TRUE} (the default), the
compiled bytecode of a function (see \code{\link{cmpfun}})
will be ignored in the comparison. If it is \code{FALSE},
functions will compare equal only if they are copies of the
same compiled object (or both are uncompiled). To check
whether two different compiles are equal, you should
compare the results of \code{\link{disassemble}}().

Note that \code{identical(x, y, FALSE, FALSE, FALSE,
FALSE)} and \code{ident(..., FALSE, FALSE, FALSE, FALSE)}
\emph{pickily} tests for exact equality.
}
\examples{
ident(1, 1.)
ident(1, 1., as.integer(1))

# for unusual R objects:
ident(.GlobalEnv, environment(), globalenv(), as.environment(1))

ident(0., 0, -0.) # not differentiated
ident(0., 0, -0., num.eq = FALSE)

ident(NaN, -NaN)
ident(NaN, -NaN, single.NA = FALSE) # differ on bit-level

# for functions
f <- function(x) x
g <- compiler::cmpfun(f)
ident(f, g)
ident(f, g, ignore.bytecode = FALSE)
}
\author{
John Chambers and R Core, \code{\link{identical}}; Robert
Redd, additions
}
\references{
Chambers, J.M. (1998) \emph{Programming with Data. A guide
to the S Language}. Springer.
}
\seealso{
\code{\link{identical}} for original;
\code{\link{all.equal}} for descriptions of how two objects
differ; \code{\link{Comparison}} for operators that
generate elementwise comparisons; \code{\link{isTRUE}} is a
simple wrapper based on \code{identical}.
}

