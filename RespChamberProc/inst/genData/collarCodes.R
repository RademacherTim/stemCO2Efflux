#trNutrient <- c("No","Po","NP","Ct")
trNutrient <- c("N","P","NP","C")
trNutrient <- factor(trNutrient, levels=trNutrient)
trBlock <- 1:4
repl <- 1:2


collarCodes <- data.frame(collar=1:32, trNutrient=rep(trNutrient,each=2), trBlock=rep(trBlock,each=8), repl=repl)
replId <- with(collarCodes, paste(trNutrient,trBlock,trRep,sep="_"))
collarCodes$replId <- factor(replId, levels=replId)
blockId <- with(collarCodes, paste(trNutrient,trBlock,sep="_"))
collarCodes$blockId <- factor(blockId, levels=unique(blockId))
collarCodes
save( collarCodes, file=file.path("data","collarCodes.RData") )


.tmp.f <- function(){
	# splitting the replicate Id into components
	do.call( rbind, strsplit( replId, "_" , TRUE))
}