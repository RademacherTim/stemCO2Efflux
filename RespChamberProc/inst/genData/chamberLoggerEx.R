ds <- readDat("tmp/chamberLoggerEx1.dat")
ds$Pa <- 101		# 101 kPa = 1010 hPa = 1.01 bar
#plot( CO2_Avg ~ TIMESTAMP, ds)
min(ds$TIMESTAMP)
ds1 <- subset( ds, TIMESTAMP <= as.POSIXct("2013-03-12"))
#ds2 <- subset(ds1, as.integer(format(ds1$TIMESTAMP,"%H")) < 10)
ds2 <- ds1[1:100,]
plot( CO2_Avg ~ TIMESTAMP, ds2)

chamberLoggerEx1s <- ds2
save( chamberLoggerEx1s, file=file.path("data","chamberLoggerEx1s.RData") )


#------------------ chamberLoggerEx2

ds <- readDat("tmp/Chamber2_ChamberData.dat")
ds$Pa <- ds$AirPres*1000
conc <- ds$CO2_dry <- corrConcDilution(ds, colConc="CO2_LI840", colVapour="H2O_LI840")
dsc <- subsetContiguous(ds)
dss <- subset(dsc, iChunk %in% c(11,65,99,101,174,307))
#library(ggplot2)
p1 <- ggplot( dss, aes(x=TIMESTAMP, y=CO2_dry) ) + geom_point() + facet_wrap( ~ iChunk, scales="free")
p1
chamberLoggerEx2 <- dss
save( chamberLoggerEx2, file=file.path("data","chamberLoggerEx2.RData") )

#trace(calcClosedChamberFlux, recover)	#untrace(calcClosedChamberFlux)

dsi <- subset(dss, iChunk==11)	# very strong (and therefore precise) uptake
dsi <- subset(dss, iChunk==65)  # slightly convex, ok
dsi <- subset(dss, iChunk==99)  # saturating good case, big dependence on initial lagTime estimate compared to fitting uncertainty
dsi <- subset(dss, iChunk==101)  # no chance: convex
dsi <- subset(dss, iChunk==174)  # similar to 65
dsi <- subset(dss, iChunk==307)  # increasing conc in lag-time

resL <- by( dss, dss$iChunk, function(dsi){
			print(dsi$iChunk[1])
			#trace(selectDataAfterLag,recover)	#untrace(selectDataAfterLag)
			tmp <- calcClosedChamberFlux(dsi, isEstimateLeverage=FALSE, colTemp="AirTemp")
			#tmp <- calcClosedChamberFlux(dsi, isEstimateLeverage=TRUE, colTemp="AirTemp")
			#tmp <- calcClosedChamberFlux(dsi, isEstimateLeverage=FALSE, colTemp="AirTemp", tLagFixed=4)
			#tmp <- calcClosedChamberFlux(dsi, isEstimateLeverage=FALSE, colTemp="AirTemp", tLagFixed=11) # 307
			#tmp <- calcClosedChamberFlux(dsi, isEstimateLeverage=TRUE, colTemp="AirTemp", tLagFixed=11) # 307
			plotResp(dsi, tmp)
} )

lenWin <- 5
maxLag <- 30
i <- 1
times <- as.numeric(dss$TIMESTAMP[1:(maxLag+lenWin)])
obs <- dsi$CO2_dry[1:(maxLag+lenWin)]
times0 <- times - times[1]
slopes <- sapply( 1:maxLag, function(i){
	times0i <- times0[i+(1:lenWin)]
    obs <- dsi$CO2_dry[i+(1:lenWin)]
	slope <- cov(times0i,obs)/var(times0i)	# this formula should be faster than running lm
})
plot( slopes ~ times0[1:maxLag] )
bps <- breakpoints( slopes ~ times0[1:maxLag],  breaks = 1 )
lines( fitted(bps) ~ times0[1:maxLag], col="red")

maxLag <- 30
i <- 1
times <- as.numeric(dsi$TIMESTAMP[1:(maxLag)])
obs <- dsi$CO2_dry[1:(maxLag)]
times0 <- times - times[1]
#times0 <- times0*10
plot( obs ~ times0 )
lm0 <- lm(obs ~ times0)
o<-segmented(lm0, seg.Z= ~times0
		,psi=list(times0=c(5))
		,control=seg.control(display=FALSE)
)
lines(fitted(o) ~ times0)





# breakpoints from strucchange will fit discontinuous
bps2 <- breakpoints( obs ~ times0,  breaks = 1 )
lines( fitted(bps2) ~ times0, col="red")
fm0 <- lm(obs ~ times0)
fm1 <- lm(obs ~ breakfactor(bps2))





.tmp.f <- function(){
	require(segmented)
	set.seed(1234567)
	library("lubridate")
	N <- 60	
	df <- data.frame(id = 1:N)
	df$date <- seq(as.Date("2013-07-01"), by = "day", along = df$id)
	df$date2 <- difftime(df$date, ymd("2013-07-01"), units = "day")
	df$date3 <- difftime(df$date, ymd("2013-08-01"), units = "day")
	difftime(ymd("2013-08-01"), ymd("2013-07-01"), units = "day")
	df$u <- 1 + 10 * rnorm(N)
	alpha <- .5
	df$y <- ifelse(df$date > as.Date("2013-08-01"), alpha * difftime(ymd("2013-08-01"), ymd("2013-07-01"), units = "day") + - 2 * as.numeric(df$date3) + df$u, alpha * as.numeric(df$date2) + df$u)
	
	out.lm<-lm(y~date3,data=df)
	o<-segmented(out.lm, seg.Z= ~date3
		,psi=list(date3=c(-5))
		#, psi=list(date3=c(-10))
		#,	control=seg.control(display=FALSE)
	)
	slope(o)
#$date3
#          Est. St.Err. t value CI(95%).l CI(95%).u
#slope1  0.3964  0.1802   2.199   0.03531    0.7574
#slope2 -1.6970  0.1802  -9.418  -2.05800   -1.3360
	
	str(fitted(o))
# Named num [1:60] 1.94 2.34 2.74 3.13 3.53 ...
# - attr(*, "names")= chr [1:60] "1" "2" "3" "4" ...
	plot(y ~ date3, data=df)
	lines(fitted(o) ~ date3, data=df)	
}

