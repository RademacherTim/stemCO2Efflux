\name{plotResp}
\alias{plotResp}
\title{plotResp}
\description{plot a single time series, and the fit }
\usage{plotResp(dsi, resFlux = NULL, colConc = "CO2_dry", 
    colTime = "TIMESTAMP", ylab = "CO2_dry (ppm)", 
    xlab = "Time (s)", label = "")}
\arguments{
  \item{dsi}{data.frame of a time series of a single concentration gradient measurement}
  \item{resFlux}{result of \code{\link{calcClosedChamberFlux}}: a one-row tibble with columns tLag, flux, sdFlux, and model}
  \item{colConc}{column name of measurment variable}
  \item{colTime}{column name of time [s], or POSIXct}
  \item{ylab}{label of y axis}
  \item{xlab}{label of x axis}
  \item{label}{label of the time series}
}



\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
#data(chamberLoggerEx1s)
dsi <- chamberLoggerEx1s
dsi$Pa <- chamberLoggerEx1s$Pa * 1000  # convert kPa to Pa
conc <- dsi$CO2_dry <- corrConcDilution(dsi)
#trace(regressFluxExp, recover)		#untrace(regressFluxExp)
#trace(calcClosedChamberFlux, recover)		#untrace(calcClosedChamberFlux)
resFlux <- calcClosedChamberFlux(dsi)
plotResp( dsi, label="Example 1" )			# without flux regression
plotResp( dsi, resFlux, label="Example 1" )
}
