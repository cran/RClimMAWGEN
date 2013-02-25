\name{generation_p1}
\alias{generation_p1}
\docType{data}

\title{
RClimMAWGEN Example Dataset 
}
%
\usage{
data(generation_p1)
}
%
\description{
This dates contains \code{generation_p1}. It is a \code{\link{list}} object returned by \code{\link{ComprehensiveTemperatureGenerator}}. See \code{\link{ComprehensiveTemperatureGenerator}} for a detailed description.
Some list elements, irrelevant for RClimMAWGEN examples, were removed from the variable  \code{generation_p1} to save disk memory.
It contains the following variables: 
%\describe{
%\item{\code{TEMPERATURE_MIN}}{Data frame containing \code{year},\code{month} , \code{day} and daily minimum temperature in 59 stations in Trentino region}
%\cr
%}
}
\details{
This data set can be regenerated using the R script 'generations.R' in the 'examples' package directory. See the Examples paragraph.
}
\format{
list
}
\source{

This data set is obtained reducing the output of the function \code{\link{ComprehensiveTemperatureGenerator}} and can be reproduced through the R script ...
This data set is intended for research purposes only, being distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY.
}
\examples{
# See file 'generations.R' to see how this dataset is obtained.
f <- system.file("examples/generations.R",package="RClimMAWGEN")
## source(f) # Do not Run!! This line is now commented because the elapsed time is too long!! Please uncomment to run this line. 

}

%%\references{
%% ~put references to the literature/web site here ~
%%}
\keyword{dataset}
