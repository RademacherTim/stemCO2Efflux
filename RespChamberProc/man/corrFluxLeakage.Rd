\name{corrFluxLeakage}
\alias{corrFluxLeakage}
\title{corrFluxLeakage}
\description{Calculate flux corrected for leakage of the chamber	}
\usage{corrFluxLeakage(conc, pLeakage = 1)}
\arguments{
  \item{conc}{numeric vector: concentration [Amount of substance]}
  \item{pLeakage}{numeric scalar: fraction of concentration lost by leakage}
}
\details{XX}
\value{numeric vector (length conc): corrected concentration [Amount of substance]}

\author{Thomas Wutzler, Oscar Perez Priego}




\examples{
#data(chamberLoggerEx1s)
ds <- chamberLoggerEx1s
ds$CO2_leakC <- corrFluxLeakage(ds$CO2_Avg)	
}
