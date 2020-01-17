\name{subsetContiguous}
\alias{subsetContiguous}
\title{subsetContiguous}
\description{Get contiguous subsets }
\usage{subsetContiguous(ds, colTime = "TIMESTAMP", colIndex = "Collar", 
    colMeasure = "CO2_dry", gapLength = 12, minNRec = 20, 
    minTime = 60, indexNA = 0, fIsBadChunk = function(dsi) FALSE)}
\arguments{
  \item{ds}{data.frame or tibble of measurements}
  \item{colTime}{column name that of time (POSIXct)}
  \item{colIndex}{column name of index variable (factor or integer)}
  \item{colMeasure}{column name of the concentration measurement}
  \item{gapLength}{minimal length of a gap between subsets (seconds)}
  \item{minNRec}{minimum number of records within one contiguous subset}
  \item{minTime}{minimum length of time (in seconds) that a contiguous subsets covers}
  \item{indexNA}{value of the index column, that signifies records not to use}
  \item{fIsBadChunk}{additional function taking a subset of contiguous data and returning a boolean value whether it shoudl be omitted}
}
\details{The time series in logger data consists of several chunks of concentration measurments.
In order to determine these chunks, either a change in an index variable (input by between the
measurements) or a gap in time is used.

Between the actual series of measurements, the logger may record sparse data.
These chunks are indicated by value \code{indexNA} in the index column or
by shortness of the series. Only chunks with at least \code{minNRec} records and at least 
\code{minTime} seconds are reported. Others are neglected.}


\author{Thomas Wutzler, Oscar Perez Priego}





