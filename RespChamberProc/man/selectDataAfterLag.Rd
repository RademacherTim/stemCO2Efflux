\name{selectDataAfterLag}
\alias{selectDataAfterLag}
\title{selectDataAfterLag}
\description{Omit the data within lag-time and normalize times to start after lag}
\usage{selectDataAfterLag(ds, colConc = "CO2_dry", colTime = "TIMESTAMP", 
    tLagFixed = NA, maxLag = 50, tLagInitial = 10, 
    minTimeDataAfterBreak = 30, minTLag = 0)}
\arguments{
  \item{ds}{a tibble or data.frame with time and concentration columns}
  \item{colConc}{column name of CO2 concentration per dry air [ppm]}
  \item{colTime}{column name of time column [s]}
  \item{tLagFixed}{possibility to specify the lagTime (in seconds) instead of estimating them}
  \item{maxLag}{number of initial records to be screened for a breakpoint, i.e. the lag}
  \item{tLagInitial}{the initial estimate of the length of the lag-phase}
  \item{minTimeDataAfterBreak}{number of minimum time (in seconds) left after breakpoint}
  \item{minTLag}{possibility to specify a minimum lag-time in seconds}
}

\value{A list with entries
\item{lagIndex}{the index of the end of the lag period}
\item{ds}{the dataset ds without the lag-period (lagIndex included)
}}

\author{Thomas Wutzler, Oscar Perez Priego}



\seealso{\code{\link{RespChamberProc}}}
\examples{
#data(chamberLoggerEx1s)
ds <- chamberLoggerEx1s
ds$CO2_dry <- corrConcDilution(ds)
#trace(selectDataAfterLag,recover)	#untrace(selectDataAfterLag)
ret <- selectDataAfterLag(ds)
plot( ds$CO2_dry)
abline(v=ret$lagIndex, col="red")
}
