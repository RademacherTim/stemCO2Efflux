\name{corrConcDilution}
\alias{corrConcDilution}
\title{corrConcDilution}
\description{Calculate concentration corrected for dilution with water vapor		}
\usage{corrConcDilution(ds, colConc = "CO2_Avg", colVapour = "H20_Avg", 
    isUpdateOnNonFiniteVapour = TRUE)}
\arguments{
  \item{ds}{a data frame with each row being one observation, and respective columns.}
  \item{colConc}{the column name of CO2 concentration [ppm].}
  \item{colVapour}{the column name of water vapour concentration [ppt].}
  \item{isUpdateOnNonFiniteVapour}{set to FALSE to not correct concentrations for records where vapour is not finite
<< in the default, the concentration will be NA}
}
\details{If CO2 concentration is measured per moist air, this function will calculate the concentration per dry air.}
\value{numeric vector (nrow ds):  concentration of CO2 per dry air [ppm]}
\references{LI-COR, Application Note 129. The Importance of Water Vapor Measurements and Corrections. LI-COR, Inc., 4421 Superior Street, Lincoln, NE 68504, USA.}
\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
#data(chamberLoggerEx1s)
ds <- chamberLoggerEx1s
ds$H20_Avg[1L] <- NA
ds$CO2_dry <- corrConcDilution(ds); head(ds$CO2_dry)	
ds$CO2_dry <- corrConcDilution(ds, isUpdateOnNonFiniteVapour=FALSE); head(ds$CO2_dry)	
}
