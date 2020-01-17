     \name{chamberLoggerEx2}
     \docType{data}
     \alias{chamberLoggerEx2}
     \title{chamberLoggerEx2}
     \description{Respiraction chamber measurement example.}
     \usage{chamberLoggerEx2}
     \format{data.frame with columns \describe{
		\item{iChunk}{Identifier for each continuous time series of one concentration gradient}
		\item{TIMESTAMP}{POSIXct time of measurement}
		\item{Chamber}{Number of the chamber used}
		\item{Collar}{Number of the collar that has been measured}
		\item{CO2_dry}{molar CO2 concentration in ppm, corrected for dilution of air with water vapour}
		\item{CO2_LI840}{molar CO2 concentration in ppm}
		\item{H20_LI840}{molar water vapour concentration in ppm}
		\item{AirTemp}{air temperature in degC}
		\item{Pa}{air pressure in kPa}
		}
     }
     \keyword{datasets}
     \examples{
     	data(chamberLoggerEx2)
     	plot( CO2_dry ~ TIMESTAMP, data=subset(chamberLoggerEx2, iChunk==99))
     }
      
