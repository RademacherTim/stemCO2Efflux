#require(testthat)
context("regressFluxes")

data(chamberLoggerEx1s, package="RespChamberProc")		# does not work with RCMDCheck and testthat
ds <- chamberLoggerEx1s[-(1:16),]
conc <- ds$CO2_dry <- corrConcDilution(ds)
times <- ds$TIMESTAMP
times0 <- as.numeric(times) - as.numeric(times[1])


.tmp.f <- function(){
	plot(conc ~ times0)
	points(concA ~ times0, col="red")
	plot(concP ~ times0)
	acf(conc, type="partial")
	acf(concA, type="partial")
}

test_that("regressFlux good data decreasing",{
	expFlux=-0.1
	expSdFlux=0.05
	#		
	res <- regressFluxSquare( conc, times  )
	expect_equal( as.numeric(res$stat["flux"]), expFlux, tolerance =0.5)
	expect_equal( as.numeric(res$stat["sdFlux"]), expSdFlux, tolerance =0.5)
	expect_true( is.na(res$stat["autoCorr"]) )
	#
	res <- regressFluxExp( conc, times  )
	expect_equal( as.numeric(res$stat["flux"]), expFlux, tolerance =0.5)
	expect_equal( as.numeric(res$stat["sdFlux"]), expSdFlux, tolerance =0.5)
	expect_true( is.na(res$stat["autoCorr"]) )
	#
	res <- regressFluxTanh( conc, times  )
	expect_equal( as.numeric(res$stat["flux"]), expFlux, tolerance =0.5)
	expect_equal( as.numeric(res$stat["sdFlux"]), expSdFlux, tolerance =0.5)
	expect_true( is.na(res$stat["autoCorr"]) )
})

test_that("regressFlux good data increasing",{
			concP <- 800 - conc			# increasing data
			expFlux=0.1
			expSdFlux=0.05
			#		
			res <- regressFluxSquare( concP, times  )
			expect_equal( as.numeric(res$stat["flux"]), expFlux, tolerance =0.5)
			expect_equal( as.numeric(res$stat["flux"]), expSdFlux, tolerance =0.5)
			expect_true( is.na(res$stat["autoCorr"]) )
			#
			res <- regressFluxExp( concP, times  )
			expect_equal( as.numeric(res$stat["flux"]), expFlux, tolerance =0.5)
			expect_equal( as.numeric(res$stat["flux"]), expSdFlux, tolerance =0.5)
			expect_true( is.na(res$stat["autoCorr"]) )
			#
			res <- regressFluxTanh( concP, times  )
			expect_equal( as.numeric(res$stat["flux"]), expFlux, tolerance =0.5)
			expect_equal( as.numeric(res$stat["flux"]), expSdFlux, tolerance =0.5)
			expect_true( is.na(res$stat["autoCorr"]) )
			#
			.tmp.f <- function(){
				plot( concP ~ times )
				lines( fitted(res$model) ~ times)
			}
		})


test_that("regressFlux autocorr data decreasing",{
			expFlux=-0.1
			expSdFlux=0.05
			res <- regressFluxSquare( conc, times  )
			resid0 <- residA <- resid(res$model)
			for( i in seq_along(times)[-(1:2)] ){
				residA[i] <- resid0[i] + 0.8*residA[i-1] - 0.4*residA[i-2]
			}
			concA <- fitted(res$model) + residA
			#
			res <- regressFluxSquare( concA, times  )
			expect_equal( as.numeric(res$stat["flux"]), expFlux, tolerance =0.5)
			expect_equal( as.numeric(res$stat["sdFlux"]), expSdFlux, tolerance =0.5)
			expect_true( is.finite(res$stat["autoCorr"]) )
			res <- regressFluxSquare( concA, times, tryAutoCorr=FALSE  )		# tryAutoCorr=FALSE arg
			# usually much lower false flux uncertainty
			expect_true( is.na(res$stat["autoCorr"]) )
			#	
			# trace(regressFluxExp, recover)	#untrace(regressFluxExp)
			res <- regressFluxExp( concA, times  )
			expect_equal( as.numeric(res$stat["flux"]), expFlux, tolerance =0.5)
			expect_equal( as.numeric(res$stat["sdFlux"]), expSdFlux, tolerance =0.5)
			expect_true( is.finite(res$stat["autoCorr"]) )
			#
			res <- regressFluxTanh( concA, times  )
			expect_equal( as.numeric(res$stat["flux"]), expFlux, tolerance =0.5)
			expect_equal( as.numeric(res$stat["sdFlux"]), expSdFlux, tolerance =0.5)
			expect_true( is.finite(res$stat["autoCorr"]) )
		})

