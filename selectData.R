# Read in a function that will plot *truncated* data (allows you to visually get rid of the noise in plots)
# Choose the start and end time (allows you to truncate the data to eliminate noise)
selectData <- function (ds = alldata, colConc = 'CO2', colTime = 'RunTime', lowerBound = 0, upperBound = 1e6,
                        plotB = T) {
  
  # make sure upperBound is not higher than the length of the time series
  upperBound <- min (upperBound, max (ds [[colTime]]))

  # make sure lowerBound is not lower than the starting point of the time series
  lowerBound <- max (lowerBound, min (ds [[colTime]]))

  # select appropriate data
  dat <- ds [(ds [[colTime]] >= lowerBound) & 
               (ds [[colTime]] <= upperBound), ]
  cut <- ds [(ds [[colTime]] < lowerBound) | 
               (ds [[colTime]] > upperBound), ]

  if (plotB) {
    # plot data
    plot (x = ds [[colTime]], 
          y = ds [[colConc]],
          xlab = 'time [s]',
          ylab = 'CO2 concentration [ppm]')#,
          #xlim = c (0, max (ds [[colTime]])),
          #ylim = c (0, max (ds [[colConc]])))

    # plot selected data bounds
    points (x = dat [[colTime]],
            y = dat [[colConc]],
            col  = '#91b9a499',
            pch  = 19,
            cex  = 0.9)
    rect (xleft   = lowerBound, 
          xright  = upperBound,
          ybottom = 0,
          ytop    = 100000,
          col     = '#91b9a411',
          border  = '#901C3B')

    # plot highlight cut data
    points (x = cut [[colTime]],
            y = cut [[colConc]],
            col  = '#901C3B99',
            pch  = 19,
            cex  = 0.9)
    rect (xleft   = 0, 
          xright  = lowerBound,
          ybottom = 0,
          ytop    = 100000,
          col     = '#901C3B11',
          lty     = 0)
  }
  return (dat)
}
