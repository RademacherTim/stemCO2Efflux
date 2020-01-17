     \name{chamberLoggerEx1s}
     \docType{data}
     \alias{chamberLoggerEx1s}
     \title{chamberLoggerEx1s}
     \description{Respiraction chamber measurement example.}
     \usage{chamberLoggerEx1s}
     \format{data.frame with columns \describe{
		\item{TIMESTAMP}{POSIXct time of measurement}
		\item{CO2_Avg}{molar CO2 concentration in ppm}
		\item{H20_Avg}{molar water vapour concentration in ppm}
		\item{TA_Avg}{air temperature in degC}
		\item{Pa}{air pressure in kPa}
		}
     }
     \keyword{datasets}
     \examples{
     	data(chamberLoggerEx1s)
     	plot( CO2_Avg ~ TIMESTAMP, data=chamberLoggerEx1s)
     }
      
