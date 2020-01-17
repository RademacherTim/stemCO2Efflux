# functions to compute volume and surface area of common chamber geometries

calcChamberGeometryCuboid <- function (
		###Calculate the inner volume of the chamber and the enclosed respiring surface area for a cuboid shaped chamber.
		width			    ##<< The inner length of one basal edge (m).
		,depth			  ##<< The inner length of the other basal edge (m).
		,height			  ##<< The inner height of the chamber (m).
		,taper = 1.0	##<< A form factor describing a linear taper of the chamber. 1.0 is equal to no taper. 
){
	##details<< The respiring surface is assumed to be the basal area of the chamber.
	##details<<
	## There are functions for other geometries
	## \begin{itemize}
	## \item Cylinder: \code{\link{calcChamberGeometryCylinder}}
  ##value<< A vector with components.
  respArea = width * depth
	c (chamberVolume = respArea * height * taper	##<< The volume inside the chamber (m^3^).
		, respArea = respArea)				              ##<< The basal aera of the chamber (m^2^).
}

calcChamberGeometryCylinder <- function (
		### Calculate the inner volume of the chamber and the enclosed respiring surface area for a cylinder-shaped chamber.
		radius			 ##<< The inner radius of the cylinder (m).
		,height			 ##<< The inner height of the cyclinder (m).
		,taper = 1.0 ##<< A form factor describing a linear taper of the chamber. 1.0 is equal to no taper. 
){
	##details<< The respiring surface is assumed to be the basal area of the chamber.
  ##details<<
  ## There are functions for other geometries
  ## \begin{itemize}
  ## \item Cuboid: \code{\link{calcChamberGeometryCuboid}}
	##value<< A vector with components.
	respArea = pi * radius^2.0
	c (chamberVolume = respArea * height * taper	##<< The volume inside the chamber (m^3^).
	   , respArea = respArea)				              ##<< The basal aera of the cylinder (m^2^).
} 
attr (calcChamberGeometryCylinder,"ex") <- function(){
  innerRadius <- 0.5008
  innerHeight <- 0.1016
  calcChamberGeometryCylinder(radius = innerRadius, height = innerHeight)
}
