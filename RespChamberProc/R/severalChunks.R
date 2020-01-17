calcClosedChamberFluxForChunks <- function(
	### apply \code{\link{calcClosedChamberFlux}} for each chunk in data
	ds						##<< tibble or data.frame
	,colChunk = "iChunk"	##<< column name (scalar string) holding a factor unique to each chunk
	,...
	,volume					##<< volume inside the chamber im [m3]
	,volumesByChunk			##<< tibble or data.frame with column <colChunk> and column  \code{volume} giving a volume for each chunk identifier
		### By default, value of argument volume is used for eah chunkg 
		### Allows for adjusting the chamber volume across different chunks (argument \code{volumne} in \code{\link{calcClosedChamberFlux}})
	,isVerbose=TRUE			##<< set to FALSE to avoid messages
){
	if( missing(volumesByChunk) ){
		#volumesByChunk <- setNames( data.frame( unique(ds[[colChunk]]), volume ), c(colChunk,"volume") )
		dsVol <- ds
		dsVol$volume <- volume
	} else {
		dsVol <- suppressWarnings(left_join(ds, volumesByChunk, colChunk))
		if( !all(is.finite(dsVol$volume)) ){
			chunksMissing <- unique(dsVol[[colChunk]][ !is.finite(dsVol$volume)])
			stop("need to provide a finite volume for each chunk. Check chunks ",paste(chunksMissing, collapse=","))
		}
	}
	#. <- filter_(dsVol, paste0(colChunk,"==",colChunk,"[1]"))
	ans <- dsVol %>% group_by_(.dots=as.symbol(colChunk) ) %>% #summarize(nRec=n())
			do_(~{
						iChunk <- as.character(.[[colChunk]][1])
						vol <- .$volume[1] 
						calcClosedChamberFlux(.,...,volume=vol)
					})
	names(ans)[names(ans)=="iChunk"] <- colChunk
	##value<< a tibble with a row for each measurement cycle and additonal column <colChunk> identifying the measurement cycle
	ans
}

# TTR: Why not give it different areas by chunk, as well?
