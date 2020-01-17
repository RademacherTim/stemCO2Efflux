\name{calcClosedChamberFlux}
\alias{calcClosedChamberFlux}
\title{calcClosedChamberFlux}
\description{Calculate gas flux and its uncertainties for a non steady-state canopy chamber.}
\usage{calcClosedChamberFlux(ds, colConc = "CO2_dry", colTime = "TIMESTAMP", 
    colTemp = "TA_Avg", colPressure = "Pa", volume = 1, 
    area = 1, fRegress = c(lin = regressFluxLinear, 
        tanh = regressFluxTanh), fRegressSelect = regressSelectPref1, 
    concSensitivity = 1, maxLag = 50, minTLag = 0, 
    debugInfo = list(useOscarsLagDectect = FALSE, omitAutoCorrFit = FALSE, 
        omitEstimateLeverage = FALSE, tLagFixed = NA, 
        isStopOnError = FALSE), ...)}
\arguments{
  \item{ds}{data.frame with concentration and time column of a chamber measurement of one replicate}
  \item{colConc}{column name of CO2 concentration [ppm]}
  \item{colTime}{column name of time [s], must be of class POSIXct or numeric or integer}
  \item{colTemp}{column name of air temperature inside chamber [degC]}
  \item{colPressure}{column name of air pressure inside chamber [Pa]}
  \item{volume}{volume inside the chamber im [m3]}
  \item{area}{area of the exchange surface [m2]}
  \item{fRegress}{list of functions to yield a single flux estimate, see details}
  \item{fRegressSelect}{function to select the regression function based on fitting results. Signature and return must correspond to \code{\link{regressSelectPref1}}}
  \item{concSensitivity}{measurement sensitivity of concentration. With concentration change below this sensitivity, only a linear model is fit}
  \item{maxLag}{number of initial records to be screened for a breakpoint, i.e. the lag (higher for water vapour than for CO2)}
  \item{minTLag}{possibility to specify a minimum lag-time in seconds}
  \item{debugInfo}{rarely used controls, mostly for debugging\describe{
\item{useOscarsLagDectect}{using the changepoint method for lag detection}
\item{omitAutoCorrFit}{set to TRUE to omit trying to fit autocorrelation (faster but maybe wrong (low) uncertainty estimate)}
\item{omitEstimateLeverage}{set to TRUE to omit the time consuming bootstrap for uncertainty due to leverage}
\item{tLagFixed}{possibility to specify the lagTime instead of estimating them}
\item{isStopOnError}{set to TRUE to stop execution when no model could be fitted, instead of a warning}
}}
  \item{\dots}{further arguments to \code{\link{sigmaBootLeverage}}}
}
\details{The function \code{fRegress} must conform to \code{\link{regressFluxSquare}}, i.e.
return a vector of length 2: the flux estimate and its standard deviation.
Optionally, it may return the model fit object in attribute "model"
If several functions are given, then the best fit is selected according 
to function with argument \code{fRegressSelect}, by default to the AIC criterion.
Fit an expoenential curve by using function \code{\link{regressFluxExp}}.

There are two kinds of uncertainty associated with the flux.
The first comes from the uncertainty of the slope of concentration increase.
The second comes from the leverage of starting and end points of the regression (estimated by a bootstrap)
return value sdFlux is the maximum of those two components}
\value{data.frame (tibble) of one row with entries
\item{flux}{the estimate of the CO2 flux into the chamber [mumol / m2 / s]}
\item{fluxMedian}{the median of the flux bootsrap estimates [mumol / m2 / s]}
\item{sdFlux}{the standard deviation of the CO2 flux}
\item{tLag}{time of lag phase in seconds}
\item{lagIndex}{index of the row at the end of lag-time}
\item{autoCorr}{autocorrelation coefficient, NA if model with autocorrelation could not be fitted or had higher AIC than a model without autocorrelation}
\item{AIC}{AIC goodness of fit for the model}
\item{sdFluxRegression}{the standard deviation of the flux by a single regression of CO2 flux}
\item{sdFluxLeverage}{the standard deviation of the flux by leverage of starting or end values of the time series}
\item{iFRegress}{index of the best (lowest AIC) regression function}
\item{r2}{coefficient of determination}
\item{times}{integer vector of predictor times for the model fit (excluding and possibly first record)
}}

\author{Thomas Wutzler, Oscar Perez Priego}



\seealso{\code{\link{RespChamberProc}}}
\examples{
#data(chamberLoggerEx1s)
library(dplyr)
ds <- chamberLoggerEx1s
    ds$Pa <- chamberLoggerEx1s$Pa * 1000  # convert kPa to Pa
conc <- ds$CO2_dry <- corrConcDilution(ds)
#trace(calcClosedChamberFlux, recover)	#untrace(calcClosedChamberFlux)
resFit <- calcClosedChamberFlux(ds)
select(resFit, flux, sdFlux) \%>\% unlist() 
#plotResp(ds, resFit)
.tmp.compareFittingFunctions <- function(){
	resLin <- calcClosedChamberFlux(ds, fRegress=list(regressFluxLinear))
	resPoly <- calcClosedChamberFlux(ds, fRegress=list(regressFluxSquare))
	#trace(regressFluxExp, recover)	#untrace(regressFluxExp)
	resExp <- calcClosedChamberFlux(ds, fRegress=list(regressFluxExp) )
	#trace(regressFluxTanh, recover)	#untrace(regressFluxTanh)
	resTanh <- calcClosedChamberFlux(ds, fRegress=list(regressFluxTanh ))
	
	times <- ds$TIMESTAMP
	times0 <- as.numeric(times) - as.numeric(times[1])
	times0Fit <- times0[times0>=resLin$stat["tLag"] ]
	plot( resid(resTanh$model, type="normalized") ~  times0Fit )	# residual plots
	qqnorm(resid(resTanh$model, type="normalized")); abline(0,1)
	
	#length(times0Fit)
	plot( ds$CO2_dry ~ times0, xlab="time (s)", ylab="" ); mtext("CO2_dry (ppm)",2,2,las=0)
	abline(v=resLin$stat["tLag"], col="grey", lty="dotted")
	lines( fitted(resExp$model) ~ times0Fit , col="red" )
	lines( fitted(resLin$model) ~ times0Fit , col="grey" )
	lines( fitted(resTanh$model) ~ times0Fit , col="purple" )
	lines( fitted(resPoly$model) ~ times0Fit , col="blue" )
	legend("topright", inset=c(0.02,0.02), legend=c("exp","lin","tanh","poly")
		, col=c("red","grey","purple","blue"), lty="solid")
}
}
