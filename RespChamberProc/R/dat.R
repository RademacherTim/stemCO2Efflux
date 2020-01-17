readDat <- function(
	### Read a data loggger .dat file into a data-frame		
	fName					##<< scalar string: file name or a connection, e.g. returned by unz
	,nRowsFileInfo = 1		##<< integer scalar: number of lines before column information
	,nRowsColInfo = 2		##<< integer vector: number of lines with column description
	,sep=","				##<< column separator
	,...					##<< further arguments to
	,colClasses = rep(NA, ncol(colInfo))	##<< see \code{link{read.table}}
	,colsTimeStamp=1		##<< integer vector: colums with time stamp column (will be set to POSIXct
	,formatTS=NULL		    ##<< format string of the timestamp columns, see \code{\link{strptime}}, e.g. 
	,tz="UTC"				##<< specify a time zone when converting to POSIXct, default: current local e.g CET, UTC
	,na.strings=c('','NA','NAN','"NAN"')
){
	if( !length(formatTS) ) formatTS <- "%Y-%m-%d %H:%M:%S"	# cannot put "%" in declaration with inlinedocs
	##details<< 
	## Assumes that there
	setClass("myDate", where=globalenv())
	setAs("character","myDate", function(from) as.POSIXct(from, format=formatTS, tz=tz), where=globalenv() )
	isConnection <- inherits(fName, "connection")
	con <- if( isConnection ) fName else file(fName,"r")
	if( !isOpen(con) ){
		on.exit(close(con))
		open(con)
	} 
	fileInfo <- readLines(con, n=nRowsFileInfo )	
	colInfo <- read.table(con, header=TRUE
		#, skip=nRowsFileInfo
		, nrows=max(1,nRowsColInfo), sep=sep, na.strings=na.strings, stringsAsFactors = FALSE)
	colClasses[colsTimeStamp] <- "myDate"
	rawData <- read.table(con, header=FALSE
		#, skip=nRowsFileInfo+1+nRowsColInfo
		, sep=sep, na.strings=na.strings, ...
					,colClasses=colClasses)
	colnames(rawData) <- colnames(colInfo)	
	#plot( CO2_Avg ~ TIMESTAMP, data=rawData )
	attr(rawData,"fileInfo") <- fileInfo
	attr(rawData,"colInfo") <- colInfo
	as_tibble(rawData)
}
attr(readDat,"ex") <- function(){
	fName <- system.file("genData/chamberLoggerEx1_short.dat", package = "RespChamberProc")
	if( nzchar(fName) ){
		ds <- readDat(fName)
	}
	# reading first from zipped file
	fName <- system.file("genData/SMANIE_Chamber1_26032015.zip", package = "RespChamberProc")
	if( nzchar(fName) ){
		ds <- readDat( unz(fName, file=unzip(fName, list=TRUE)[1,"Name"] )
				,tz="UTC")
	}
}

read81x <- function(
		### Read a Licor generated .x81 file into a data-frame		
		fName					##<< scalar string: file name
		,nRowsFileInfo=23		##<< scalar integer: number of lines of initial file information
		,sep="\t"				##<< column separator
		,...					##<< further arguments to \code{\link{readDat}}
		,colsTimeStamp=3		##<< integer vector: colums with time stamp column (will be set to POSIXct
		,formatTS=NULL			##<< format of the timestamp columns, see \code{\link{strptime}}, e.g. 
		,tz="UTC"				##<< specify a time zone when converting to POSIXct, default: current local e.g CET, UTC
		,na.strings=c('','NA','NAN','"NAN"')
		,labelRowOffset=-16		##<< the row offset, usually before concentration measurements to generate column \code{label}
){
	##details<< 
	## version of \code{\link{readDat}} with adjusted defaults
	#
	# find the beginning of data blocks and of summary information
	if( !length(formatTS) ) formatTS <- "%Y-%m-%d %H:%M:%S"
	lines <- readLines(fName)
	blockStarts <- grep("^Type", lines)				# starts of data blocks
	summaryStarts <- grep("^CrvFitStatus", lines)	# starts of summary blocks (after each data block) 
	setClass("myDate", where=globalenv())
	setAs("character","myDate", function(from) 
				as.POSIXct(from, format=formatTS, tz=tz), where=globalenv() )
	fileInfo <- readLines(fName, n=nRowsFileInfo )
	colNamesFile <- unlist(read.table(fName, header=FALSE, skip=nRowsFileInfo
		, nrows=1, sep=sep, na.strings=na.strings))
	#colNamesFile <- colNamesFile[1:(length(colNamesFile)-1)]	# skip annotation collumn, not in data
	colClasses = rep(NA, length(colNamesFile))	##<< see \code{link{read.table}}	
	colClasses[colsTimeStamp] <- "myDate"
	#colInfo <- read.table(fName, header=TRUE, skip=nRowsFileInfo, nrows=max(1,nRowsColInfo), sep=sep, na.strings=na.strings)
	iChunk <- 1
	resBlocks <- lapply( seq_along(summaryStarts), function(iChunk){
				# read the label from above the chunk
				tmp <- scan(fName, "character", skip=blockStarts[iChunk]+labelRowOffset, nlines=1, quiet=TRUE)
				label <- if( length(tmp) >= 2) tmp[2] else as.character(iChunk)
				rawData <- read.table(fName, header=FALSE
						, sep=sep, na.strings=na.strings
						, skip= blockStarts[iChunk]+1
						, col.names = colNamesFile, fill=TRUE
						, ...
						, nrows= summaryStarts[iChunk] - blockStarts[iChunk] -2
						,colClasses=colClasses
				)
				cbind( iChunk=iChunk, rawData[ rawData$Type==1,], label=label )
			})
	res <- suppressWarnings(bind_rows( resBlocks ))		# warning on unequal factor levels of id and label
#	# try using the label as chunk identifier
#	nChunkPerLabel <- res %>% group_by_(~label) %>% summarise_(nLabel=~(max(iChunk) - min(iChunk)))
#	if( all(nChunkPerLabel$nLabel == 1L)){
#		res$iChunk <- res$label
#	} else warning("non-unique labels per contiguous chunk of concentration measurements.") 
	attr(res,"fileInfo") <- fileInfo
	as_tibble(res)
}
attr(read81x,"ex") <- function(){
	#fName <- "inst/genData/Flux2_140929_1700.81x"
	#fName <- "inst/genData/Flux2_140929_1700.81x"
	fName <- system.file("genData/Flux2_140929_1700.81x", package = "RespChamberProc")
	if( nzchar(fName) ){
		ds <- read81x(fName)
		#plot( CO2 ~ Date, ds )
		#plot( CO2 ~ Date, ds[ds$iChunk==9,] )
	}
}



subsetContiguous <- function(
	### Get contiguous subsets 
	ds						##<< data.frame or tibble of measurements 
	,colTime="TIMESTAMP"	##<< column name that of time (POSIXct)
	,colIndex="Collar"		##<< column name of index variable (factor or integer)
	,colMeasure="CO2_dry"	##<< column name of the concentration measurement
	,gapLength=12			##<< minimal length of a gap between subsets (seconds)
	,minNRec=20				##<< minimum number of records within one contiguous subset
	,minTime=60				##<< minimum length of time (in seconds) that a contiguous subsets covers
	,indexNA=0				##<< value of the index column, that signifies records not to use
	,fIsBadChunk=function(dsi) FALSE	
	### additional function taking a subset of contiguous data and returning a boolean value whether it shoudl be omitted
){
	##details<< 
	## The time series in logger data consists of several chunks of concentration measurments.
	## In order to determine these chunks, either a change in an index variable (input by between the
	## measurements) or a gap in time is used.
	ds <- as_tibble(ds)
	reqCols <- c(colTime,colIndex, colMeasure)
	iMissingCols <- which(!(reqCols %in% names(ds)))
	if( length(iMissingCols) ) stop("subsetContiguous: missing columns: ", paste(reqCols[iMissingCols],collapse=","))
	timeDiffInSeconds <- diff(as.numeric(ds[[colTime]]))
	iGaps <-  which( timeDiffInSeconds > gapLength)	# records with missing records in time before (start a new chunk) 
	iCollarChanges <- which( diff(as.numeric(ds[[colIndex]])) != 0 )
	iChunks <- c( 1, sort(union(iCollarChanges, iGaps)), nrow(ds) ) # start, breaks, end 
	##details<<
	## Between the actual series of measurements, the logger may record sparse data.
	## These chunks are indicated by value \code{indexNA} in the index column or
	## by shortness of the series. Only chunks with at least \code{minNRec} records and at least 
	## \code{minTime} seconds are reported. Others are neglected.
	dsChunks <- as_tibble(map_df( 2:length(iChunks), function(i){
				dsia <- cbind( iChunk=i-1, filter( ds, row_number() %in% (iChunks[i-1]+1):(iChunks[i]) ))
				index <- dsia[[colIndex]][1] 
				dsi <- dsia[is.finite(dsia[[colMeasure]]),]
				timeSec <- as.numeric(dsi[[colTime]]) - as.numeric( dsi[[colTime]][1] )
				#if( collar == indexNA || nrow(dsi) < minNRec || max(timeSec) < minTime || var(dsi[[colMeasure]])==0  ){
				if( index == indexNA || nrow(dsi) < minNRec || max(timeSec) < minTime || fIsBadChunk(dsi)){
					return( NULL )
				}else return(dsi)
			}))
	dsChunks$iChunk <- as.factor(dsChunks$iChunk)		# convert to factor
	### Argument \code{ds} with between-Chunk rows omitted and an additional integer column \code{iChunk} that designates the chunk number.
	dsChunks
}
