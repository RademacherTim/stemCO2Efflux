#require(testthat)
context("severalCycles")

fName <- system.file("genData/SMANIE_Chamber1_26032015.zip", package = "RespChamberProc")
if( nzchar(fName) ){ 
	ds <- ds0 <- readDat(unz(fName, file=unzip(fName, list=TRUE)[1,"Name"] ),tz="UTC")
	ds <- filter_(ds0, ~TIMESTAMP < as.POSIXct("2015-03-26 06:29:12", tz="UTC"))
	ds$Pa <- ds$AirPres * 100  # convert hPa to Pa
	ds$CO2_dry <- corrConcDilution(ds, colConc = "CO2_LI840", colVapour = "H2O_LI840")
	ds$H2O_dry <- corrConcDilution(ds, colConc = "H2O_LI840", colVapour = "H2O_LI840")
	ds$VPD <- calcVPD( ds$SurTemp, ds$Pa, ds$H2O_LI840)
	#
	dsChunked <- subsetContiguous(ds, colTime="TIMESTAMP", colIndex="Collar") 
	test_that("subsetContiguous",{
				expect_true( nrow(dsChunked) > 1 )		
				expect_true( nrow(dsChunked) < nrow(ds) )
				expect_true( all( names(ds) %in% names(dsChunked)) )
				expect_true( all( "iChunk" %in% names(dsChunked)) )
				expect_true( length(unique(dsChunked$iChunk)) > 1 )
				expect_true( all( is.finite(dsChunked$CO2_dry) ))
			})
	#
	chamberVol=0.6*0.6*0.6		# chamber was a cube of 0.6m length
	surfaceArea=0.6*0.6
	nChunk <- length(unique(dsChunked$iChunk))
	# pretend more chunks in volume dataset as in dsChunke
	chamberVolByChunk <- data.frame( iChunk=c(as.character(unique(dsChunked$iChunk)),"unknownChunk"), volume=chamberVol * c(0.9, 1.1,1))
	resChunks <- calcClosedChamberFluxForChunks(dsChunked, colTemp="T_LI840"
			,fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh)	# linear and saturating shape
			,debugInfo=list(omitEstimateLeverage=TRUE)	# faster
			,volumesByChunk=chamberVolByChunk
			,area=surfaceArea)
	test_that("calcClosedChamberFluxForChunks",{
				expect_true( nrow(resChunks) > 1 )		
				expect_true( all( c("flux","sdFlux") %in% names(resChunks)) )
				expect_true( all( "iChunk" %in% names(resChunks)) )
				expect_true( all( table(resChunks$iChunk) == 1) )
				resChunks1 <- calcClosedChamberFluxForChunks(dsChunked, colTemp="T_LI840"
						,fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh)	# linear and saturating shape
						,debugInfo=list(omitEstimateLeverage=TRUE)	# faster
						,volume=chamberVol
						,area=surfaceArea)
				expect_true( resChunks$flux[1] < resChunks1$flux[1] )	# using a smaller volume in chunk-varying volume
				expect_true( resChunks$flux[2] > resChunks1$flux[1] )	# using a larger volume in chunk-varying volume
			})
	test_that("calcClosedChamberFluxForChunks with error on volumes",{
				chamberVolByChunkNA1 <- chamberVolByChunk[2,,drop=FALSE];
				expect_error(
				resChunks2 <- calcClosedChamberFluxForChunks(dsChunked, colTemp="T_LI840"
						,fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh)	# linear and saturating shape
						,debugInfo=list(omitEstimateLeverage=TRUE)	# faster
						,volumesByChunk=chamberVolByChunkNA1
						,area=surfaceArea)
				)
			})
	
	#
	suppressMessages(dsPlots <- plotCampaignConcSeries(dsChunked, resChunks, isVerbose=FALSE))
	#dsPlots <- plotCampaignConcSeries(dsChunked, resChunks1, isVerbose=FALSE)
	#dsPlots$plot[[1]]
	test_that("plotCampaignConcSeries",{
				expect_true( nrow(dsPlots) > 0 )		
				expect_true( all( c("iPage","plot") %in% names(dsPlots)) )
				expect_true( inherits( dsPlots$plot[[1]],"ggplot") )
			})
	#twWin(); print(dsPlots$plot[[1]])
	#twWin(); dsPlots %>% rowwise() %>% do({print(.$plot); data.frame()})

} # if nzchar(fname)
