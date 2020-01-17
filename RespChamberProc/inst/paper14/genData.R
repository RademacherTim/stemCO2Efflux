projectDir = "inst/paper14"
library(stringr)
library(ggplot2)
library(plyr)


genSMANIEControl <- function(){
	#data(collarCodes)   # merge on conc takes too long
	rawDataPaths="M:/data/SMANIE/Fluxes/raw"
	campaignDirs = list.files( rawDataPaths, pattern="SMANIP_\\d", include.dirs=TRUE)
	campaigns <- as.integer(str_extract(campaignDirs, "\\d"))
	#iCamp <- 1
	dsl <- data.frame()
	#iCamp <- 2
	# logger data has in part records from previous measurement campaigns, but take only the recent ones
	campaignStarts <- as.POSIXctUTC(c("2014-03-17", "2014-04-14","2014-05-06","2014-05-27","2014-06-23"))
	# some logger data also appears in different files (chamber 3 in other chamber files
	# make sure to take only the recent ones of the correct logger (from file name)
	for( iCamp in seq_along(campaignDirs) ){
	#dsl <-  lapply( seq_along(campaignDirs), function(iCamp){
				cat(",",iCamp)
				fNames <- dir( file.path(rawDataPaths,campaignDirs[iCamp]), pattern="\\.dat$", full.names=TRUE )
				#fName <- fNames[1]
				for( fName in fNames ){
				#dsd <- do.call(rbind, lapply( fNames, function(fName){
							dsa <- readDat(fName)
							#gsub("Chamber(\\d+).*","\\1",c("Chamber1","Chamber12_"))
							iChamber <- gsub("(.*Chamber)(\\d+)(.*)","\\2",fName)
							dsp <- subset(dsa, TIMESTAMP >= campaignStarts[iCamp] )
							ds <- subset(dsp, Chamber == iChamber)
							if( nrow(ds)==0  ) stop("encountered zero data frame. Check file names and patterns.")
							#trace(subsetContiguous, recover)	#untrace(subsetContiguous)
							ds$CO2_dry <- corrConcDilution(ds, colConc = "CO2_LI840", colVapour = "H2O_LI840")
							dsc <- subsetContiguous(ds )		# CO2_dry must be defined
							dscc <- cbind( campaign=iCamp, dsc )
							dsl <- rbind( dsl, dscc)
							if( any(table(interaction(dsl$RECORD,dsl$Chamber, drop=TRUE)) != 1) ) stop("encountered double record")
							dsl
						}
						#))
				#if( any(table(dsd$RECORD) != 1) ) stop("encountered double record")
			}
			#)
	#ds <- do.call( rbind, dsl )
	ds <- dsl
	if( any(table(interaction(ds$RECORD,ds$Chamber, drop=TRUE)) != 1) ) stop("encountered double record")
	ds <- ds[order(ds$campaign, ds$Chamber, ds$iChunk), ]
	ds$id <- interaction( ds$campaign, ds$Chamber, ds$iChunk, drop=TRUE, lex.order = TRUE)
	ds$Pa <- ds$AirPres * 1000		# convert kPa to Pa
	SMANIEconc <- ds 
	# TAKE care overwrites
	save( SMANIEconc, file=file.path(projectDir,"SMANIEconc.Rd"))
}

.plotCampaignConcSeries <- function(
	### plot all the time series into a series of pdf-pages
	ds					##<< data frame to plot, with collumns "id", "TIMESTAMP" and \code{varName}
	,resL=NULL			##<< list with results of \code{\link{calcClosedChamberFlux}} for each id-subset in ds
	,varName="CO2_dry"	##<< variable to plot
	,plotsPerPage=64	##<< number of plots per page
	,fileName=paste0(varName,".pdf")	##<< fileName of the output pdf 
){
	#iCamp <- 1
	#dss <- subset(ds, campaign==1 & Chamber==1)
	N <- length(unique(ds$id))
	ds$id <- factor(ds$id)	# drop unused factor levels
	#(as.numeric(unique(ds$id))-1)%/%plotsPerPage+1
	dsp <- cbind(iPage= factor((as.numeric(ds$id)-1)%/%plotsPerPage+1), ds)
	print(paste("Number of pages (each ",plotsPerPage," plots): ", length(unique(dsp$iPage))), sep="" )
	dss <- subset(dsp, iPage==1)
	#ds$H2O_dry <- corrConcDilution(ds, colConc = "H2O_LI840", colVapour = "H2O_LI840"); varName <- "H2O_dry"
	pdf(width=11.7,height=8.3,file=fileName)
	d_ply(dsp, .(iPage), function(dss){
				idsPage <- unique(dss$id)
				print(paste(idsPage, collapse=","))
				# calculate times0
				dss <- dss[order(dss$id, dss$TIMESTAMP),]
				dssi <- subset(dss, id==dss$id[1] )
				dss$times0  <- do.call(c, dlply( dss, .(id), function(dssi){
						times <- as.numeric(dssi$TIMESTAMP)
						times0 <- times - times[1]
					}))
				#dss <- dsc
				p1 <- ggplot( dss, aes_string(x="times0", y=varName) ) + geom_point() + 
						facet_wrap( ~ id, scales="free") +
						theme_gray(base_size=9) + 
						theme()
				if( length(resL) ){
					resLi <- resL[ names(resL) %in% idsPage ]
					# resCalc <- resLi[[1]]
					dfLag <- data.frame( id= names(resLi), tLag=sapply( resLi, function(resCalc){
										if( !inherits(resCalc,"try-error") && length(resCalc$stat) )	resCalc$stat["tLag"] else NA_real_	
									}))
					#idi <- names(resLi)[1]
					dfFitted <- do.call( rbind, lapply( names(resLi), function(idi){
								if( !inherits(resLi[[idi]],"try-error") && length(resLi[[idi]]$model) ){
									dsr <- data.frame( id=idi, fitted=fitted(resLi[[idi]]$model))
									times0 <- subset(dss, id==idi)$times0
									dsr$times0 <- times0[times0 >= dfLag$tLag[dfLag$id==idi]][1:nrow(dsr)]
									dsr
								} else NULL
							}))
					p1 <- p1 + 
							geom_vline( aes(xintercept=tLag), col="darkgrey", linetype="dashed", data=dfLag ) +
							geom_line( aes(y=fitted), col="red", data=dfFitted ) +
							theme()
				}
				print(p1)
				#p1
			})
	dev.off()
}
attr( .plotCampaignConcSeries, "ex") <- function(){
	load(file.path(projectDir,"SMANIEconc.Rd"))
	ds0 <- ds <- SMANIEconc
	#tmp <- subset(ds0, campaign==5 )
	#ds0 <- ds <- subset(SMANIEconc, id %in% levels(SMANIEconc$id)[64+1:8] ) testing
	#ds0 <- ds <- subset(SMANIEconc, id %in% c(levels(SMANIEconc$id)[64+1:3],"1.2.19","1.3.2")  ) #testing for fit errors
	.tmp.f <- function(){
		ds <- subset(ds0,  !id %in% c(
						"1.2.19"	# nonunique times
						,"1.3.2"	# exactly zero concentrations all
							) )
	}
	#dss <- subset(ds, id == ds$id[1] )
	#dss <- subset(ds, id == "1.3.2" )
	# ds <- ds[-(1:33550), ]
	.createresCalcClosedChamberFlux <- function(){
		resCalcClosedChamberFlux <- resL <- dlply( ds, .(id), function(dss){
			cat( ",", levels(dss$id)[dss$id[1]] )
			res2 <- res <- try( 
				res <- calcClosedChamberFlux( dss, colTemp="AirTemp", volume = 0.6*0.6*0.6
				, debugInfo = list(useOscarsLagDectect = TRUE, omitAutoCorrFit=TRUE, omitEstimateLeverage = TRUE)
				)
				,silent=TRUE)
		})
		save( resCalcClosedChamberFlux, file=file.path(projectDir,"resCalcClosedChamberFlux_SMANIEconc.Rd"))
	}
	load( file.path(projectDir,"resCalcClosedChamberFlux_SMANIEconc.Rd"))
	resL <- resCalcClosedChamberFlux
	names(resL)[ sapply( resL, inherits,  "try-error") ]	# "1.2.19"  "1.3.2"   "1.3.23"  "2.2.270" "2.2.277"
	#
	fileName=file.path(projectDir,paste0("SMANIEconc_CO2_dry.pdf"))	##<< fileName
	.plotCampaignConcSeries(ds, resL=resL, fileName=fileName)
}

.inspectTimeSeries <- function(){
	# inspect the pdf of fits and truncate series
	load(file.path(projectDir,"SMANIEconc.Rd"))
	ds <- SMANIEconc
	ds <- SMANIEconcScreened
	goodPeriods <- list(	##<< list with entries of a chunk with two two values constraining the beginning and end times0
			"2.2.60" = c(NA,200)	# bad end (forget to switch off?)			
			,"5.1.17" = c(NA,180)	# bad end			
	)
	goodPeriods <- list(	##<< list with entries of a chunk with two two values constraining the beginning and end times0 
	"1.2.19" = c(0,0)		# double series
	,"1.2.82" = c(0,0)		# nonsensical
	,"2.1.1" = c(5,NA)		# bad gap est 
	,"2.2.40" = c(NA,300)	# bad end (forget to switch off?)			
	,"2.2.60" = c(NA,200)	# bad end (forget to switch off?)			
	,"2.2.90" = c(NA,240)	# bad end (forget to switch off?)			
	,"2.2.116" = c(52,NA)	# bad gap est
	,"3.1.6." = c(NA,200)	# bad end			
	,"3.1.56" = c(NA,130)	# bad end
	,"3.1.62" = c(NA,150) 	#bad end			
	,"3.2.2" = c(35,NA)		# bad gap est			
	,"3.3.17" = c(0,0)		# non sensical			
	,"3.3.19" = c(0,0)		# non sensical			
	,"3.3.23" = c(0,0)		# non sensical			
	,"3.3.15" = c(NA,150)	# bad end			
	,"4.2.94" = c(0,0)		# non sensical			
	,"5.1.13" = c(NA,100)	# bad end			
	,"5.1.17" = c(NA,180)	# bad end			
	,"5.2.13" = c(NA,130)	# bad end			
	,"5.2.17" = c(NA,130)	# bad end			
	,"5.3.17" = c(NA,130)	# bad end			
	,"3.3.18" = c(10,NA)		# bad gap est			
	,"5.3.26" = c(NA,450)	# bad end			
	,"5.3.42" = c(NA,200)	# bad end			
	,"5.3.54" = c(NA,200)	# bad end			
	,"5.3.56" = c(NA,200)	# bad end			
	,"5.3.74" = c(NA,200)	# bad end			
	,"5.3.76" = c(NA,200)	# bad end			
	,"1.3.55" =  c(0,0)		# convex
	,"2.1.85" =  c(0,0)		# convex
	,"2.2.156" =  c(0,0)		# convex
	)
	.tmp.f <- function(){
		#inspect single series
		data(collarCodes)
		idi <- "5.1.18"
		dss <- merge( subset(ds, id==idi), collarCodes )
		head(dss,5)
		plotResp(dss)
		grid(12)
	}
	idi <- names(goodPeriods)[3]
	dsL <- lapply( names(goodPeriods), function(idi){
				gPi <- goodPeriods[[idi]]
				dss <- subset(ds, id==idi)
				times <- as.numeric(dss$TIMESTAMP)
				times0 <- times - times[1]
				if( is.finite(gPi[1]) ) dss <- subset(dss, times0 >= gPi[1] )
				if( is.finite(gPi[2]) ) dss <- subset(dss, times0 <= gPi[2] )
				#plot( CO2_dry ~ TIMESTAMP, dss )
			})
	dsRep <- do.call( rbind, dsL )
	dsOmit <- subset(ds, !(id %in% names(goodPeriods)) )
	dsNew <- rbind( dsOmit, dsRep )
	# omit the records with just one row c(0,0)
	id1RowOmit <- ddply( dsNew, .(id), nrow )
	id1RowOmit <- id1RowOmit[ id1RowOmit[,2] == 1, ]
	SMANIEconcScreened <- dsNew[ !(dsNew$id %in% id1RowOmit$id), ]
	nrow(SMANIEconcScreened)
	save( SMANIEconcScreened, file=file.path(projectDir,"SMANIEconcScreened.Rd"))
}

.fitAndPlotScreened <- function(){
	# with the clean data set fit the functions including costly autocorrelation and bootstrap
	load(file.path(projectDir,"SMANIEconcScreened.Rd"))
	ds0 <- ds <- SMANIEconcScreened
	#ds0 <- ds <- subset(SMANIEconcScreened, id %in% names(goodPeriods) )
	#tmp <- subset(ds0, campaign==5 )
	#ds0 <- ds <- subset(SMANIEconc, id %in% levels(SMANIEconc$id)[64+1:8] ) testing
	#ds0 <- ds <- subset(SMANIEconc, id %in% c(levels(SMANIEconc$id)[64+1:3],"1.2.19","1.3.2")  ) #testing for fit errors
	.createresCalcClosedChamberFlux <- function(){
		resCalcClosedChamberFluxScreened <- 
				resL <- dlply( ds, .(id), function(dss){
					cat( ",", levels(dss$id)[dss$id[1]] )
					res2 <- res <- try( 
							res <- calcClosedChamberFlux( dss, colTemp="AirTemp", volume = 0.6*0.6*0.6
									, debugInfo = list(
										useOscarsLagDectect = TRUE
										#, omitAutoCorrFit=TRUE, omitEstimateLeverage = TRUE
									)
							)
							,silent=TRUE)
				})
		#resCalcClosedChamberFluxScreened[names(resCalcClosedChamberFluxScreened) %in% names(resL)] <- resL
		save( resCalcClosedChamberFluxScreened, file=file.path(projectDir,"resCalcClosedChamberFluxScreened_SMANIEconc.Rd"))
	}
	load( file.path(projectDir,"resCalcClosedChamberFluxScreened_SMANIEconc.Rd"))
	resL <- resCalcClosedChamberFluxScreened
	names(resL)[ sapply( resL, inherits,  "try-error") ]	# "1.2.19"  "1.3.2"   "1.3.23"  "2.2.270" "2.2.277"
	#
	fileName=file.path(projectDir,paste0("SMANIEconcScreened_CO2_dry.pdf"))	##<< fileName
	.plotCampaignConcSeries(ds, resL=resL, fileName=fileName)
	
}


