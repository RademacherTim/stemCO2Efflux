\name{calcChamberGeometry}
\alias{calcChamberGeometry}
\title{calcChamberGeometry}
\description{Calculate the inner volume of the chamber and the enclosed respiring surface area for some typical chamber geometries. 	}
\usage{calcChamberGeometry(geometry, innerDimensions)}
\arguments{
  \item{geometry}{A character string describing the geometric form of the chamber. Implemented geometries are 'cylinder' and 'cuboid'.}
  \item{innerDimensions}{A vector with the inner dimensions of the chamber specific to the geometry of the chamber. For a cylinder, the first element is the cylinder's radius and the second is the cylinder's height. For a cuboid, the first element is the width, the second is the depth, and the third is the height. All dimensions are in meters.}
  \item {taper}{A form factor describing a linear taper function of the geometry along the z-axis (height). 1.0 means no taper, 1/3 turns a cylinder into a cone.}
}
\details{This function will calculate the inner volume of the respiration chamber in meters cubed and the enclosed respiring surface area in meteres squared given the geometry, dimensions and taper factor of the chamber in meters.}
\value{}
#\references{}
\author{Tim T. Rademacher}
\examples{
  #
}
