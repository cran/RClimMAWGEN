
NULL 
#'
#' Create input object for clim,ete index analyis from RMAWWGEN output.
#' 
#' @param data data.frame containing realizations of weather variables, e.g. the one retured as \code{output} by \code{\link{ComprehensiveTemperatureGenerator}} 
#' @param station names of weather stations where to apply climate indices
#' @param realization_TN realizations of daily minimum temperature (observed and simulated) time series on which climate index are calculated
#' @param realization_TX realizations of daily maximum temperature (observed and simulated) time series on which climate index are calculated
#' @param realization_PREC realizations of daily precipitation (observed and simulated) time series on which climate index are calculated. It is \code{NULL} if missing.
#' @param start_date start date \code{yyyy-mm-dd} of weather time series
#' @param end_date start date \code{yyyy-mm-dd} of weather time series
#' @param climate_index climate indices to be calculated. The names must correspond to the name of the respective function contained in the \code{climdex.pcic}   R package
#' @param yearly logical voalue. If \code{TRUE} (Default) the index is calculeted yearly per each year, otherwise the index is calculated monthly, i.e. per each month
#' @param base.range see \code{\link{climdexInput.raw}}
#' @param n see \code{\link{climdexInput.raw}}
#' @param prefix name for time series on which climate indices are calculated. 
#' @param date.series see \code{\link{climdexInput.raw}}. If missing, it is automatically calculated from \code{start_date} and \code{end_date}
#' @param frequency string value. Default is \code{c("yearly","monthly","daily")}. Set one of these, if the climate indices are referred to each year, month or day respectively.
#' 
#' @title ClimDex Data Frame
#' @return a \code{climdex.data.frame} object (see the variable \code{climdex} in the examples.)
#' @export 
#' @seealso \code{\link{as.climdex.data.frame}},\code{\link{climdexInput.raw}}
#' 
#' @references \url{http://www.climdex.org}
#' @author Emanuele Cordano, Annalisa Di Piazzaa
#'
#' 
#' @examples
#'rm(list=ls())
#'library(RClimMAWGEN)
#' #  generated and observed daily temperature data for the considering period (1981-2010)(RMAWGEN output data structure)
#' data (generation_p1)
#'
#'
#' #collected generated (realizations) and observed data (realizations$Tx_mes, realizations$Tn_mes)
#'
#' realizations <- generation_p1$output
#'
#' realizations$Tx_mes <- generation_p1$input$Tx_mes
#'
#' realizations$Tn_mes <- generation_p1$input$Tn_mes
#'
#' # realization scanarios used for 'climdex.data.frame'
#' realizations_TN <- c("Tn_mes","Tn_gen00002","Tn_gen00003","Tn_gen00004")
#' realizations_TX <- c("Tx_mes","Tx_gen00002","Tx_gen00003","Tx_gen00004")
#'
#' stations <- names(realizations$Tn_mes)
#'
#' start_date = "1981-01-01"
#' end_date = "2010-12-31"
#'
#' climate_indices = c("climdex.tn90p","climdex.tx90p")
#'
#' frequency =  "monthly"
#'
#' date.series = seq(as.PCICt(start_date, cal = "gregorian"), as.PCICt(end_date, cal = "gregorian"), by = "days")
#'
#' base.range = c(1990, 2002)
#' n = 5
#' prefix = NULL
#'
#' 
#' climdex <- climdex.data.frame(data=realizations, station=stations, realization_TN=realizations_TN,realization_TX=realizations_TX,realization_PREC=NULL, start_date= start_date, end_date = end_date ,climate_index = climate_indices,frequency = frequency,date.series = date.series,base.range = base.range, n = n, prefix = prefix)
#'
#' str(climdex)
#' # Wilcoxon test between observed and generated climate indices
#' 
#'  observed <- "T0129__Tn_mes__climdex.tx90p"
#'  generated <- c("T0129__Tn_gen00002__climdex.tx90p","T0129__Tn_gen00003__climdex.tx90p") 
#'  wxt <- wilcox.test(x=climdex,observed=observed,generated=generated)
#'  wxt
#' #  Kolgomorov-Smirinov test between observed and generated climate indices
#' 
#' kst <- ks.test.climdex.data.frame(data=climdex,observed=observed,generated=generated)
#' kst 
#' 
#' accepted(wxt)
#' accepted(kst)
#' 
#' 
#' 








climdex.data.frame <- function(data,
                                station,
                                realization_TN,
                                realization_TX,
                                realization_PREC,
                                start_date="1981-01-01",
                                end_date="2010-12-31",climate_index="climdex.gsl",frequency=c("yearly","monthly","daily"),
                                date.series=seq(as.PCICt(start_date, cal="gregorian"), as.PCICt(end_date, cal="gregorian"), by="days"),base.range=c(1990, 2002),n=5,prefix=NULL){

  out <- NULL
   
  daily=FALSE
  monthly=FALSE
  yearly=FALSE
  if (frequency=="daily") daily=TRUE
  if (frequency=="monthy") monthly=TRUE
  if (frequency=="yearly") yearly=TRUE
  
  nday <- length(date.series)
  
  tn <- array(0,nday)
  tx <- array(0,nday)
  prec <- array(0,nday)
  
 if ((length(station)==1) & (length(realization_TN)==1 | is.null(realization_TN)) & (length(realization_TX==1)| is.null(realization_TX)) & (length(realization_PREC==1)| is.null(realization_PREC))){
    
    if (!is.null(realization_TN))   tn <- data[[realization_TN[1]]][,station[1]]
    if (!is.null(realization_TX))   tx <- data[[realization_TX[1]]][,station[1]]
    if (!is.null(realization_PREC)) prec <- data[[realization_PREC[1]]][,station[1]]
	
	
    temp <- climdexInput.raw(tx,tn,prec, date.series, date.series, date.series,base.range,n)
	
	if(frequency=="yearly"){
		
		years <- unique(as.character(temp@annual.factor))
		
		out <- as.data.frame(array(NA,c(length(years),length(climate_index))))
	}
	else if (frequency=="monthly") {
		
		months <- unique(as.character(temp@monthly.factor))
		
		out <- as.data.frame(array(NA,c(length(months),length(climate_index))))
	} else if (frequency=="daily") {
		
		out <- array(NA,c(nrow(tx),length(climate_index)))
	}
	
   
    names(out) <- paste(prefix,climate_index,sep="__")

	
    for (f in climate_index){

     
	  call <- call(f,temp)
	
	  name <- names(out)[climate_index==f]
      out[,name] <- as.vector(eval(call))
    }

  } else {
    nr <- max(c(length(realization_TX),length(realization_TN),length(realization_PREC)))
    out <- NULL
    
    for (it in station) {
      for (r in 1:nr) {
      
        
        name <- paste(it,realization_TN[r],sep="__")
      
        temp0 <- climdex.data.frame(data,station=it,realization_TN=realization_TN[r],
                                        realization_TX=realization_TX[r],realization_PREC=realization_PREC[r],frequency=frequency,
                               date.series=date.series,base.range=base.range,n=n,prefix=name,climate_index=climate_index)
  
		
		
	     class(temp0) <- "data.frame"
		
		if (is.null(out)) {
          out <- temp0
        } else {
            out <- cbind(out,temp0)
        }
      }
      
    }     
      
      
  }
  

 
  out <- as.data.frame(do.call(cbind,out))
  #print(class(out))
return(as.climdex.data.frame(out))

}


                  