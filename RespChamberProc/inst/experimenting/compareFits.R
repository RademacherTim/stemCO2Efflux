tmp <- read.csv("tmp/Flux_R.csv")
i <- 1
dsAll <- do.call( rbind, lapply( 1:24, function(i){
			data.frame( series=i, time=tmp[,1], CO2 = tmp[,i+1], water = tmp[,i+1+24] ) 
		}))
dsAll$CO2_dry <- corrConcDilution(dsAll, "CO2", "water")
dsAll$Pa <- 101*1000	# sea level pressure
dsAll$TA_Avg <- 21		# room temperature
dsAll$TIMESTAMP <- dsAll$time

.tmp.f <- function(){
	library(ggplot2)
	p1 <- ggplot( dsAll, aes(x=time, y=CO2_dry) ) + geom_point() + facet_wrap( ~ series, scales="free"); p1
}





dss <- subset(dsAll, series != 18)

ds <- subset(dss, series==17)		# saturating consumption
ds <- subset(dss, series==15)		# saturating production
res <- calcClosedChamberFlux(ds )
# plot( CO2~time, ds )

ds <- subset(dss, series==3)		# saturating production


library(plyr)
resFluxesL <- dlply( dss, .(series), function(ds){
			cat(ds$series[1],",")
			#trace(calcClosedChamberFlux, recover )	#untrace(calcClosedChamberFlux)
			#trace(regressFluxTanh, recover )	#untrace(regressFluxTanh)
			resi <- calcClosedChamberFlux(ds, fRegress=c(regressFluxLinear))
			c( series = ds$series[1], resi$stat )
		})
resFluxesLin <- do.call(rbind, resFluxesL)

resFluxesL <- dlply( dss, .(series), function(ds){
			cat(ds$series[1],",")
			#trace(calcClosedChamberFlux, recover )	#untrace(calcClosedChamberFlux)
			#trace(regressFluxTanh, recover )	#untrace(regressFluxTanh)
			resi <- calcClosedChamberFlux(ds, fRegress=c(regressFluxTanh))
			c( series = ds$series[1], resi$stat )
		})
resFluxesTanh <- do.call(rbind, resFluxesL)

resFluxesL <- dlply( dss, .(series), function(ds){
			cat(ds$series[1],",")
			resi <- calcClosedChamberFlux(ds, fRegress=c(regressFluxSquare))
			c( series = ds$series[1], resi$stat )
		})
resFluxesPoly <- do.call(rbind, resFluxesL)

resFluxesL <- dlply( dss, .(series), function(ds){
			cat(ds$series[1],",")
			#trace(regressFluxExp, recover )	#untrace(regressFluxExp)
			resi <- calcClosedChamberFlux(ds, fRegress=c(regressFluxExp))
			c( series = ds$series[1], resi$stat )
		})
resFluxesExp <- do.call(rbind, resFluxesL)

.tmp.f <- function(){
	resFluxesL <- dlply( dss, .(series), function(ds){
				cat(ds$series[1],",")
				resi <- calcClosedChamberFlux(ds, fRegress=c(regressFluxExp, regressFluxLinear,regressFluxSquare,regressFluxTanh))
				c( series = ds$series[1], resi$stat )
			})
	resFluxesAIC <- do.call(rbind, resFluxesL)
	
	resFluxesL <- dlply( dss, .(series), function(ds){
				cat(ds$series[1],",")
				resi <- calcClosedChamberFlux(ds
					, fRegress=c(regressFluxExp, regressFluxLinear,regressFluxTanh,regressFluxSquare)
					, fRegressSelect = regressSelectAIC
			)
				c( series = ds$series[1], resi$stat )
			})
	resFluxesAIC2 <- do.call(rbind, resFluxesL)
}

AICs <- cbind(resFluxesLin[,"AIC"], resFluxesPoly[,"AIC"], resFluxesExp[,"AIC"], resFluxesTanh[,"AIC"] )
apply( AICs, 1, which.min )



(resFluxesPoly - resFluxesTanh ) / resFluxesTanh
(resFluxesTanh -resFluxesExp) / resFluxesExp		# AIC essentially equal, only use tanh if exp was not fitted
(resFluxesTanh -resFluxesExp)

plot( abs(flux) ~ series, resFluxesAIC, pch="x" )
points( abs(flux) ~ series, resFluxesLin, col="grey" )
points( abs(flux) ~ series, resFluxesExp, col="red" )
points( abs(flux) ~ series, resFluxesTanh, col="blue" )
points( abs(flux) ~ series, resFluxesPoly, col="maroon" )

ds <- subset(dss, series==2)		# differences between tanh and poly
ds <- subset(dss, series==5)		# differences between tanh and poly
ds <- subset(dss, series==13)		# differences between tanh and poly
#ds <- subset(dss, series==16)		# differences between tanh and poly
ds <- subset(dss, series==23)		# differences between tanh and poly

ds <- subset(dss, series==17)		# differences between tanh and exp # but strange points after lag-time
ds <- subset(dss, series==21)		# differences between tanh and exp -> exp AIC actually better 

ds <- subset(dss, series==6)		# tanh fitted but not exp -> nearly linear only slight underestimation by linear model

ds <- subset(dss, series==17)		# differences between tanh and exp -> AIC significantly worse, but pattern better than tanh
ds <- subset(dss, series==24)		# differences between tanh and exp -> almost linear, same flux estimate between tanh and exp

ds <- subset(dss, series==15)		# differences between tanh and exp -> almost no difference in estimate tanh and exp

ds <- subset(dss, series==23)		# autocorrelation? indeed

ds <- subset(dss, series==2)		# linear?  case where poly goes wrong (concave)
ds <- subset(dss, series==5)		# linear?  case where poly goes wrong (concave)

# check poly best: none
# iPoly <- which(resFluxesAIC[,"iFRegress"]==4)
# iPoly <- which(resFluxesAIC2[,"iFRegress"]==4)

#trace(calcClosedChamberFlux, recover )	#untrace(calcClosedChamberFlux)
#trace(regressFluxExp, recover )	#untrace(regressFluxExp)
rExp <- calcClosedChamberFlux(ds, fRegress=c(regressFluxExp))
#rExp <- calcClosedChamberFlux(ds, fRegress=c(regressFluxExp), isStopOnError=TRUE)
#trace(regressFluxTanh, recover )	#untrace(regressFluxTanh)
rTanh <- calcClosedChamberFlux(ds, fRegress=c(regressFluxTanh))
rLin <- calcClosedChamberFlux(ds, fRegress=c(regressFluxLinear))
rPoly <- calcClosedChamberFlux(ds, fRegress=c(regressFluxSquare))
#trace(regressSelectPref1, recover)	#untrace(regressSelectPref1)
#rAIC <- calcClosedChamberFlux(ds ); mAIC <- attr(rAIC,"model")
#rAIC <- calcClosedChamberFlux(ds, fRegress=c(regressFluxExp, regressFluxLinear,regressFluxSquare,regressFluxTanh) )

do.call( rbind, lapply(list(exp=rExp, lin=rLin, tanh=rTanh, poly=rPoly),"[[","stat" ) )
.tmp.f <- function(){
	qqnorm(resid(rTanh$model, type="normalized")); abline(0,1)
	qqnorm(resid(rExp$model, type="normalized")); abline(0,1)
	qqnorm(resid(rPoly$model, type="pearson")); abline(0,1)
	qqnorm(resid(rLin$model, type="pearson")); abline(0,1)
}
plot( CO2_dry ~ time, ds)
tLag <- rLin$stat["tLag"]
points( CO2_dry ~ time, ds[ds$time <= tLag, ], col="lightgrey")
points( CO2_dry ~ time, ds[ds$time > 5*tLag, ], col="lightgrey")
abline(v=tLag)
lines(fitted(rExp$model) ~ I(ds$time[ds$time > tLag ]))
lines(fitted(rTanh$model) ~ I(ds$time[ds$time > tLag ]), col="blue")
#lines(fitted(rAIC$model) ~ I(ds$time[ds$time > tLag ]), col="maroon")
lines(fitted(rPoly$model) ~ I(ds$time[ds$time > tLag ]), col="green")
lines(fitted(rLin$model) ~ I(ds$time[ds$time > tLag ]), col="darkgrey")

plot( resid(rTanh$model) ~ I(ds$time[ds$time > tLag ]))
points( resid(rExp$model) ~ I(ds$time[ds$time > tLag ]), col="blue")
qqnorm(resid(rTanh$model)); abline(0,1)

# plot normality
windows()
qqnorm(resid(rLin$model, type="normalized")); abline(0,1)
plot(density(resid(rLin$model, type="normalized")))
x <- seq(-3,3,length.out=30); 
lines( dnorm( x, sd=sd(resid(rLin$model, type="normalized")) ) ~ x, col="red")

# check autocorrelation
acf(resid(rTanh$model))
acf(resid(rExp$model))
acf(resid(rLin$model))

