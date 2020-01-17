#require(testthat)
context("regressFluxesLeverage")

tmp <- structure(list(conc = c(407.2, 426.5, 428.7, 428.4, 427.3, 429.5, 
						429.7, 432.5, 428.4, 433.3, 429.8, 428.7, 429.5, 429.2, 431.7, 
						432.8, 433.4, 430.9, 434.2, 432, 430.3, 432.3, 435, 433.3, 432.7, 
						432, 432.5, 432.3, 433.3, 435.3, 435, 433.3, 434, 435.3, 435.1, 
						434.4, 433.6, 433.3, 432.8, 433.3, 436.9), times = c(0, 2, 4, 
						6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 
						38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 
						70, 72, 74, 76, 78, 80)), .Names = c("conc", "times"), row.names = c(NA, 
				-41L), class = "data.frame")
conc <- tmp[[1]]
timesSec <- times0 <- tmp[[2]]
#timesSec <- times0 <- -tmp[[2]]


.tmp.f <- function(){
	plot(conc ~ times0)
	fluxLin <- coefficients(lm(conc ~ timesSec ))[2]
		concP <- if( fluxLin < 0 ) -conc else conc		# invert, do not forget to invert again when flux calculated
		# increasing concentration
		# set the saturation twice above the range
		cRange <- quantile( concP , probs=c(0.05,0.95))
		cSatFac=2		
		cSat0 <- cRange[1] + cSatFac*diff(cRange)
		#abline(h=cSat0)
		#plot( concP-cSat0 ~ timesSec )
		lm1 <- lm(head(concP,30)-cSat0 ~ head(timesSec,30) )
		#abline(coefficients(lm1))
		# c0 is the range to cover (= -intercept from the linear model)
		s0 <- coefficients(lm1)[2]	     # initial slope
		c00 <- -coefficients(lm1)[1]     # range to cover
	nlm1 <- suppressWarnings(gnls( conc ~   (tanh(timesSec*s/c0)-1)*c0 + cSat 
					,start = list(s=s0, cSat = cSat0, c0 = c00)
					,params= c(s+cSat+c0~1)
					,correlation=NULL
			))
	lines(fitted(nlm1)~timesSec)
	# i <- 1L
	resL <- lapply( 1:5, function(i){
				concOut <- conc[-i]
				timesSecOut <- timesSec[-i]
				nlmOut <- suppressWarnings(gnls( concOut ~   (tanh(timesSecOut*s/c0)-1)*c0 + cSat 
								,start = list(s=s0, cSat = cSat0, c0 = c00)
								,params= c(s+cSat+c0~1)
								,correlation=NULL
						))
			})
	nlmOut <- resL[[1]]
	lines(fitted(nlmOut)~timesSec[-1], col="red")
	
	
	ans <- regressFluxTanh(conc,times0)
	length(fitted(ans$model))
	lines( predict(ans$model, newdata=data.frame(times0)) ~ times0, col="blue")
}


test_that("regress with first rec outlier",{
	ans <- regressFluxTanh(conc,times0)
	expect_equal( length(fitted(ans$model)), length(times0)-1)
	.tmp.f <- function(){
		plot(conc ~ times0)
		lines( fitted(ans$model) ~ ans$times, col="blue")
	}
})

