\name{read81x}
\alias{read81x}
\title{read81x}
\description{Read a Licor generated .x81 file into a data-frame		}
\usage{read81x(fName, nRowsFileInfo = 23, sep = "\\t", ..., 
    colsTimeStamp = 3, formatTS = NULL, tz = "UTC", 
    na.strings = c("", "NA", "NAN", "\\"NAN\\""), labelRowOffset = -16)}
\arguments{
  \item{fName}{scalar string: file name}
  \item{nRowsFileInfo}{scalar integer: number of lines of initial file information}
  \item{sep}{column separator}
  \item{\dots}{further arguments to \code{\link{readDat}}}
  \item{colsTimeStamp}{integer vector: colums with time stamp column (will be set to POSIXct}
  \item{formatTS}{format of the timestamp columns, see \code{\link{strptime}}, e.g.}
  \item{tz}{specify a time zone when converting to POSIXct, default: current local e.g CET, UTC}
  \item{na.strings}{
}
  \item{labelRowOffset}{the row offset, usually before concentration measurements to generate column \code{label}}
}
\details{version of \code{\link{readDat}} with adjusted defaults}


\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
#fName <- "inst/genData/Flux2_140929_1700.81x"
#fName <- "inst/genData/Flux2_140929_1700.81x"
fName <- system.file("genData/Flux2_140929_1700.81x", package = "RespChamberProc")
if( nzchar(fName) ){
	ds <- read81x(fName)
	#plot( CO2 ~ Date, ds )
	#plot( CO2 ~ Date, ds[ds$iChunk==9,] )
}
}
