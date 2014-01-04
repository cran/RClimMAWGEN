\name{climdex.data.frame}
\alias{climdex.data.frame}
\title{ClimDex Data Frame}
\usage{
  climdex.data.frame(data, station, realization_TN,
    realization_TX, realization_PREC, start_date =
    "1981-01-01", end_date = "2010-12-31", climate_index =
    "climdex.gsl", frequency = c("yearly", "monthly",
    "daily"), freq = c("default", "monthly", "annual"),
    date.series = seq(as.PCICt(start_date, cal =
    "gregorian"), as.PCICt(end_date, cal = "gregorian"), by
    = "days"), base.range = c(1990, 2002), n = 5, prefix =
    NULL, ...)
}
\arguments{
  \item{data}{data.frame containing realizations of weather
  variables, e.g. the one retured as \code{output} by
  \code{\link{ComprehensiveTemperatureGenerator}}}

  \item{station}{names of weather stations where to apply
  climate indices}

  \item{realization_TN}{realizations of daily minimum
  temperature (observed and simulated) time series on which
  climate index are calculated}

  \item{realization_TX}{realizations of daily maximum
  temperature (observed and simulated) time series on which
  climate index are calculated}

  \item{realization_PREC}{realizations of daily
  precipitation (observed and simulated) time series on
  which climate index are calculated. It is \code{NULL} if
  missing.}

  \item{start_date}{start date \code{yyyy-mm-dd} of weather
  time series}

  \item{end_date}{start date \code{yyyy-mm-dd} of weather
  time series}

  \item{climate_index}{climate indices to be calculated.
  The names must correspond to the name of the respective
  function contained in the \code{climdex.pcic} R package}

  \item{yearly}{logical voalue. If \code{TRUE} (Default)
  the index is calculeted yearly per each year, otherwise
  the index is calculated monthly, i.e. per each month}

  \item{base.range}{see \code{\link{climdexInput.raw}}}

  \item{n}{see \code{\link{climdexInput.raw}}}

  \item{prefix}{name for time series on which climate
  indices are calculated.}

  \item{date.series}{see \code{\link{climdexInput.raw}}. If
  missing, it is automatically calculated from
  \code{start_date} and \code{end_date}}

  \item{frequency}{string value. Default is
  \code{c("yearly","monthly","daily")}. Set one of these,
  if the climate indices are referred to each year, month
  or day respectively.}

  \item{freq}{string value. Default is
  \code{c("default","monthly", "annual")}. It has the same
  role of \code{"frequency"} and is used in several
  \code{cilmdex.pcic} indices. If it is omitted (Default)
  the frequency is obtaind by \code{frequency} argument.
  See
  \code{\link{climdex.tn90p}},\code{\link{climdex.tx90p}}
  .}

  \item{...}{further arguments}
}
\value{
  a \code{climdex.data.frame} object (see the variable
  \code{climdex} in the examples.)
}
\description{
  Create input object for clim,ete index analyis from
  RMAWGEN output.
}
\examples{
rm(list=ls())
library(RClimMAWGEN)
#  generated and observed daily temperature data for the considering period
#  (1981-2010)(RMAWGEN output data structure)
data (generation_p1)


#collected generated (realizations) and observed data (realizations$Tx_mes, realizations$Tn_mes)

realizations <- generation_p1$output

realizations$Tx_mes <- generation_p1$input$Tx_mes

realizations$Tn_mes <- generation_p1$input$Tn_mes

# realization scanarios used for 'climdex.data.frame'
realizations_TN <- c("Tn_mes","Tn_gen00002","Tn_gen00003","Tn_gen00004")
realizations_TX <- c("Tx_mes","Tx_gen00002","Tx_gen00003","Tx_gen00004")

stations <- names(realizations$Tn_mes)

start_date = "1981-01-01"
end_date = "2010-12-31"

# The indices \\link{climdex.tn90p},\\link{climdex.tx90p} are considered in this example
climate_indices = c("climdex.tn90p","climdex.tx90p")

frequency =  "monthly"

date.series = seq(as.PCICt(start_date, cal = "gregorian"),
 as.PCICt(end_date, cal = "gregorian"), by = "days")

base.range = c(1990, 2002)
n = 5
prefix = NULL


climdex <- climdex.data.frame(data=realizations, station=stations,
 realization_TN=realizations_TN,realization_TX=realizations_TX,realization_PREC=NULL,
 start_date= start_date, end_date = end_date ,climate_index = climate_indices,
 frequency = frequency,date.series = date.series,base.range = base.range,
 n = n, prefix = prefix)

str(climdex)

## Function 'climdex.data.frame' can be also used with annual frequency
## The following lines are now commented because the elapsed time is too long!!
## Please uncomment to run the following lines to run the function.
# climdex_annual <- climdex.data.frame(data=realizations, station=stations,
# realization_TN=realizations_TN,realization_TX=realizations_TX,realization_PREC=NULL,
# start_date= start_date, end_date = end_date ,climate_index = climate_indices,
# frequency = "yearly",date.series = date.series,base.range = base.range,
# n = n, prefix = prefix)
#
# str(climdex_annual)


# Wilcoxon test between observed and generated climate indices

 observed <- "T0129__Tn_mes__climdex.tx90p"
 generated <- c("T0129__Tn_gen00002__climdex.tx90p","T0129__Tn_gen00003__climdex.tx90p")
 wxt <- wilcox.test(x=climdex,observed=observed,generated=generated)
 wxt
#  Kolgomorov-Smirinov test between observed and generated climate indices

kst <- ks.test.climdex.data.frame(data=climdex,observed=observed,generated=generated)
kst

accepted(wxt)
accepted(kst)
}
\author{
  Emanuele Cordano, Annalisa Di Piazzaa
}
\references{
  \url{http://www.climdex.org}
}
\seealso{
  \code{\link{as.climdex.data.frame}},\code{\link{climdexInput.raw}},\code{\link{climdex.tn90p}},\code{\link{climdex.tx90p}}
}

