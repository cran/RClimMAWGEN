\name{wilcox.test}
\alias{wilcox.test}
\alias{wilcox.test.climdex.data.frame}
\title{Wilcoxon Rank Sum and Signed Rank Tests a ClimDex Data Frame}
\usage{
  \method{wilcox.test}{climdex.data.frame} (x, observed,
    generated, ...)
}
\arguments{
  \item{x}{a \code{\link{climdex.data.frame}} object}

  \item{observed}{name (String) of the column of
  \code{data} containing the obseved climate indices}

  \item{generated}{names (String vector) of the columns of
  \code{data} containing the climate index realizations
  which will be tested.}

  \item{...}{further arguments}
}
\description{
  wilcox.test S3 method for 'climdex.data.frame'
}
\examples{
# See the example of 'climdex.data.frame' function
}
\author{
  Emanuele Cordano, Annalisa Di Piazza
}
\seealso{
  \code{\link{climdex.data.frame}},\code{\link{ks.test}},\code{\link{ks.test.climdex.data.frame}}
}

