library(RespChamberProc)

fileName <- "tmp/Chamber1_ChamberData.dat"
#fileName <- "tmp/MANIP_Ch1_2.dat"
#fileName <- "tmp/MANIP_Ch3_0.dat"
#ds0 <- readDat(fileName, tz="CET")
ds0 <- readDat(fileName, tz="UTC")    # keep all dates in UTC format (still actuallzy CET) to avoid daylight savings complications

ds <- subset(ds0, as.numeric(TIMESTAMP) >= as.numeric(as.POSIXctUTC("2014-06-23 00:00:01")) )
ds$CO2_dry <- corrConcDilution(ds, colConc = "CO2_LI840", colVapour = "H2O_LI840")
ds$H2O_dry <- corrConcDilution(ds, colConc = "H2O_LI840", colVapour = "H2O_LI840")
ds$Pa <- ds$AirPres * 100   # convert hPa to Pa
ds$VPD <- calcVPD( ds$SurTemp, ds$Pa, ds$H2O_LI840)

#dsChunksClean <- subsetContiguous(ds)
dsChunksClean <- subsetContiguous(ds, fIsBadChunk=function(dsi){ var(dsi$CO2_dry)==0})
length(unique(dsChunksClean$iChunk))

# plot the time series
library(ggplot2)
p1 <- ggplot( dsChunksClean, aes(x=TIMESTAMP, y=CO2_dry) ) + geom_point() + facet_wrap( ~ iChunk, scales = "free")
p2 <- ggplot( dsChunksClean, aes(x=TIMESTAMP, y=H2O_dry) ) + geom_point() + facet_wrap( ~ iChunk, scales = "free")
p3 <- ggplot( dsChunksClean, aes(x=TIMESTAMP, y=H2O_LI840) ) + geom_point() + facet_wrap( ~ iChunk, scales = "free")
p1

#-- calculate flux and extract environmental conditions, may be parallelized
library(plyr)
library(doSNOW)
nNode = 8	# number of processors
cl = makeCluster(nNode)		
registerDoSNOW(cl)
clusterEvalQ(cl, library(RespChamberProc))		# functions need to be loaded on remote hosts

#dsi <- subset( dsChunksClean, iChunk==10 )
#dsi <- subset( dsChunksClean, iChunk==19 )
system.time(res <- ddply(   dsChunksClean, .(iChunk), function(dsi){
  collar <- dsi$Collar[1] 
  iChunk = dsi$iChunk[1]
  print( paste(iChunk, dsi$TIMESTAMP[1]," Collar: ",collar) )
  #timeSec <- as.numeric(dsi$TIMESTAMP) - as.numeric(dsi$TIMESTAMP)[1]
  #plot( CO2_dry ~ timeSec, dsi )
  #plot( H2O_dry ~ timeSec, dsi )
  res <- calcClosedChamberFlux(dsi, colConc="CO2_dry", colTemp = "AirTemp", colPressure = "Pa"
                               #, fRegress = c(regressFluxLinear, regressFluxTanh)
                               , volume = 0.6*0.6*0.6, isEstimateLeverage = TRUE, isStopOnError=FALSE)
  #lines(fitted(res$model) ~ timeSec[timeSec>=res$stat["tLag"]], col="red")
  resH20 <- calcClosedChamberFlux(dsi, colConc="H2O_dry", colTemp = "AirTemp", colPressure = "Pa"
                                  #, fRegress = c(regressFluxLinear, regressFluxTanh)
                                  , maxLag=100    # slower respons of water vapour
                                  #,debugInfo=list(useOscarsLagDectect=TRUE)
                                  , volume = 0.6*0.6*0.6, isEstimateLeverage = TRUE, isStopOnError=FALSE)
  #lines(fitted(resH20$model) ~ timeSec[timeSec>=resH20$stat["tLag"]], col="red")
  # get additional environmental variables at the initial time
  dsiInitial <- dsi[ 1, ,drop=FALSE]
  cbind( data.frame( time=dsiInitial[,"TIMESTAMP"], collar=collar
                     , CO2_flux=res$stat[1], CO2_flux_sd=res$stat[2], H2O_flux=resH20$stat[1], H2O_flux_sd=resH20$stat[2] )
         , dsiInitial[,c("Chamber","AirTemp","AirPres","PAR","SurTemp","SoilTemp","SoilMoist","VPD")] )
}
, .parallel=TRUE
))

#stopCluster(cl)

# relate the flux per chamber to flux per ground area (mumol /s / m2)
res$CO2_fluxA <-  res$CO2_flux / (0.6*0.6)
res$CO2_fluxA_sd <-  res$CO2_flux_sd / (0.6*0.6)
res

data(collarCodes)
res2  <- merge(res, collarCodes)

#write.csv(res2,paste(fileName,"_results.csv",sep=""))





