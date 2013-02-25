\name{ks.test.climdex.data.frame}
\alias{ks.test.climdex.data.frame}
\title{Kolgomorov-Smirnov Tests for a ClimDex Data Frame}
\usage{
  ks.test.climdex.data.frame(data, observed, generated,
    ...)
}
\arguments{
  \item{data}{a \code{\link{climdex.data.frame}} object}

  \item{observed}{name (String) of the column of
  \code{data} containing the obseved climate indices}

  \item{generated}{names (String vector) of the columns of
  \code{data} containing the climate index realizations
  which will be tested.}

  \item{...}{further arguments}
}
\description{
  ks.test S3 method for 'climdex.data.frame'
}
\examples{
# See the example of 'climdex.data.frame' function
}
\author{
  Annalisa Di Piazza, Emanuele Cordano
}
\seealso{
  \code{\link{climdex.data.frame}},\code{\link{wilcox.test}},\code{\link{ks.test}}
}

