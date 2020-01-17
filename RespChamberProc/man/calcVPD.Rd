\name{calcVPD}
\alias{calcVPD}
\title{calcVPD}
\description{calculate the water Vapour Pressure Deficit                   }
\usage{calcVPD(Ts, Pa, H2O)}
\arguments{
  \item{Ts}{Surface temperature (deg Centrigrade)}
  \item{Pa}{Atmospheric pressure (Pa)}
  \item{H2O}{water Vapour Pressure (ppt)}
}

\value{water Vapour Pressure Deficit in kPa}

\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
Ts <- 10
Pa <- 970*100 #hPa -> Pa
H2O <- 8 #ppt
calcVPD( Ts, Pa, H2O )
}
