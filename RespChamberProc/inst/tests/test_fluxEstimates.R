#require(testthat)
context("fluxEstimates")

data(chamberLoggerEx1s, package="RespChamberProc")		# does not work with RCMDCheck and testthat
ds <- chamberLoggerEx1s[-(1:16),]
conc <- ds$CO2_dry <- corrConcDilution(ds)
times <- ds$TIMESTAMP
times0 <- as.numeric(times) - as.numeric(times[1])


.tmp.f <- function(){
}

test_that("default example flux Estimates runs",{
			#trace(calcClosedChamberFlux, recover)		#untrace(calcClosedChamberFlux)
			resFit <- calcClosedChamberFlux(ds, tLagFixed=0)
			#plotResp(ds, resFit)
		})

test_that("no flux with random data",{
			set.seed(0815)
			concR <- 390 + rnorm(length(times), sd=0.3)
			#plot( concR ~ times )
			ds <- data.frame( CO2_dry=concR, TIMESTAMP=times, TA_Avg=20, Pa=101*1000)
			#trace(calcClosedChamberFlux, recover)		#untrace(calcClosedChamberFlux)
			res <- calcClosedChamberFlux(ds)
			expect_true( abs(res$flux)-res$sdFlux < 0 )	# not significantly different from 0
			expect_that( length(coefficients(res$model[[1]])), equals(2) )	# fitted a linear model
		})
			
			
			