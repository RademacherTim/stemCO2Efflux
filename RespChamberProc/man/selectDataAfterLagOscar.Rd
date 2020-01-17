\name{selectDataAfterLagOscar}
\alias{selectDataAfterLagOscar}
\title{selectDataAfterLagOscar}
\description{Omit the data within lag-time and normalize times to start after lag}
\usage{selectDataAfterLagOscar(ds, colConc = "CO2_dry", colTime = "TIMESTAMP", 
    tLagFixed = NA, minTimeDataAfterBreak = 30)}
\arguments{
  \item{ds}{data.frame with time and concentration columns}
  \item{colConc}{column name of CO2 concentration per dry air [ppm]}
  \item{colTime}{column name of time column [s]}
  \item{tLagFixed}{possibility to specify the lagTime (in seconds) instead of estimating them}
  \item{minTimeDataAfterBreak}{number of minimum time (in seconds) left after breakpoint}
}

\value{A list with entries
\item{lagIndex}{the index of the end of the lag period}
\item{ds}{the dataset ds without the lag-period (lagIndex included)
}}

\author{Thomas Wutzler, Oscar Perez Priego}



\seealso{\code{\link{RespChamberProc}}}

