\name{convert_mumolPers_to_gPerday}
\alias{convert_mumolPers_to_gPerday}
\title{convert mumolPers to gPerday}
\description{convert vom mumol CO2 / second to g C / day}
\usage{convert_mumolPers_to_gPerday(flux, molarMass = 12.011)}
\arguments{
  \item{flux}{numeric vector or array: flux}
  \item{molarMass}{numeric scalar: the molar mass of the element of interest (default carbon) in g/mol}
}
\details{Concentration measures are usually given by micromol CO2 across several seconds, and
the flux, i.e. its slope hence given in micromol CO2 per second.
To compare carbon balances, the units of gC per day are more convenient.  

mumol are converted to mol by /1e6
mol are converted to gC by *12.011
per second are converted to per day by *3600*24

In order to convert masses of other elements, specify the molarMass argument, 
e.g. for converting mol H2O per second to H2O per day, 
specify \code{molarMass=2*1.008+15.999}.}
\value{numeric vector of flux in other units}

\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
convert_mumolPers_to_gPerday(c(1,10))
}
