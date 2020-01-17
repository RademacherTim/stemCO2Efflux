plotDurationUncertainty <- function(
	### plot the increase of uncertainty with decreaseing measurement duration
	ds
	,colTime="TIMESTAMP"	##<< column name of time [s]
	,fRegress = c(exp=regressFluxExp, lin=regressFluxLinear, tanh=regressFluxTanh)	##<< list of functions to yield 
			##<< a single flux estimate, see details of \code{\link{calcClosedChamberFlux}}
	,...	##<< further arguments to \code{\link{calcClosedChamberFlux}}
	,durations = seq( max(65,resFit0$tLag), max(times0), length.out=nDur+1)		##<< durations to check. Default is equally spaced between tLag and maximum duration
	,nDur = 20		##<< number of durations to check
	,maxSdFlux = 1	##<< maxium allowed standard deviation of flux in [mumol / s]
){
	times <- ds[[colTime]]
	times0 <- as.numeric(times) - as.numeric(times[1])
	resFit0 <- calcClosedChamberFlux(ds, colTime=colTime, fRegress=fRegress, ...)
	#resFit0
	duration <- durations[1]
	nDur <- length(durations)
	#plot( CO2_dry ~ times0, ds)
	resFits0 <- suppressWarnings(bind_rows(map_df( durations[-c(nDur+1) ], function(duration){
				dss <- subset(ds, times0 <= duration )
				times0s <- times0[times0 <= duration]
				resFit <- calcClosedChamberFlux(dss, tLagFixed=resFit0$tLag
					,colTime=colTime, fRegress=fRegress[resFit0$iFRegress],...) 
				#plot( CO2_dry ~ times0s, dss)
				#lines( fitted(resFit$model) ~ times0s[times0s >= resFit0$tLag], col="red")
				#bind_cols(select_(resFit,~flux,~sdFlux), duration = max(times0s) )
				#select_(resFit,~flux,~sdFlux)
			})))
	resFits <- suppressWarnings(bind_rows(resFits0, resFit0) %>% mutate(duration=c(durations,max(times0))))
	iMinTime <- if( min(resFits$sdFlux, na.rm=TRUE) <= maxSdFlux ) min(which( resFits$sdFlux <= maxSdFlux )) else nrow(resFits)
	minDuration <- resFits[iMinTime,]
	##details<< 
	## Produces a plot with standard deviation of the flux estimate versus the duration of the measurment.
	## The lines correspond to the given maxium acceptable standard deviation
	## and the duration that matches this criterion.
	plot( sdFlux ~ duration, resFits, xlab="Duration of measurement (s)" , ylab="sd(fluxEstimate)")
	abline(h = maxSdFlux, col="grey", lty="dashed" )
	abline(v = minDuration["duration"], , col="grey", lty="dashed" )
	#plot( flux ~ duration, tmp2 )
	#
	##value<< tibble result of \code{\link{calcClosedChamberFlux}} for the minimum duration, with additional component 
	ans <- mutate( resFits[iMinTime,]
		, statAll= list(resFits)					##<< tibble: each row a fit for a given duration
	)
	ans
}
attr(plotDurationUncertainty,"ex") <- function(){
	#data(chamberLoggerEx2)
	ds <- subset(chamberLoggerEx2, iChunk==99)	# very strong (and therefore precise) uptake
	#plot( CO2_dry ~ TIMESTAMP, ds )
	resDur <- plotDurationUncertainty( ds, colTemp="AirTemp", volume = 0.6*0.6*0.6
		, maxSdFlux = 0.8
		, nDur=10
		, durations = c(100,120,150)
	)
	resDur$duration
	#plot( flux ~ duration, resDur$statAll[[1]] )
	#plot( sdFlux ~ duration, resDur$statAll[[1]] )
}
