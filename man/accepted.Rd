\name{accepted}
\alias{accepted}
\title{Which generations pass the tests with success?}
\usage{
  accepted(tests, significance = 0.05)
}
\arguments{
  \item{tests}{list of objects returned by
  \code{\link{wilcox.test}} and \code{\link{ks.test}}}

  \item{significance}{significance for statistical tests
  (maximum accepted \code{p-Value}). Default is 0.05.}
}
\value{
  Vector with names of successful realizations.
}
\description{
  This functions lists the realizations which pass
  successfully Ks or Wilcoxon test.
}
\examples{
# See the example of 'climdex.data.frame' function
}
\seealso{
  \code{\link{climdex.data.frame}},\code{\link{ks.test}},\code{\link{ks.test.climdex.data.frame}},\code{\link{wilcox.test}}
}

