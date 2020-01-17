#--------------------------------------------------------------------------------------#
# This script will plot upper/lower bounds & calculate the stem respiration rate of 
# tree and each chamber. 
#--------------------------------------------------------------------------------------#

# Function to calculate the session
#--------------------------------------------------------------------------------------#
calcSession <- function (study, date_time, verbose = T) {
  
  # Read stem boundaries in or use plotChooseBounds script to create them
  #------------------------------------------------------------------------------------#
  stemBounds <- read.csv (file = './data/StemResp2019-bounds.csv') 
  stemBounds <- stemBounds [stemBounds$TIMESTAMP == date_time, ]
  stemBounds <- stemBounds [!is.na (stemBounds)]
  stemLowerBound <- as.numeric (stemBounds [seq (2, length (stemBounds), 2)])
  stemUpperBound <- as.numeric (stemBounds [seq (3, length (stemBounds), 2)])
  
  # Choose just the data files
  #------------------------------------------------------------------------------------#
  myfiles <- list.files (sprintf ('./data/%s', date_time), "csv")
  myfiles <- myfiles [substring (myfiles, 1, 2) == "G-"]   # Filter out only the data files
  myfiles <- myfiles [substring (myfiles, 3, 2 + nchar (study)) == study] # Filter out only the relevant files for the study
  
  tmp <- unlist (strsplit (myfiles, '_'))
  datestr <- tmp [seq (3, length (tmp), 4)]
  timestr <- tmp [seq (4, length (tmp), 4)]
  timestr <- substring (timestr, 1, nchar (timestr) - 4)
  timestamp <- strptime (paste (datestr, timestr),"%Y%m%d %H%M%S")
  
  if (study == 'Exp2019') {
    tree <- as.numeric (substring (myfiles, 14, 14))
  } else if (study != 'Obs2018' & study != 'Obs2019') {
    tree <- as.numeric (substring (myfiles, 11, 12))
  } else {
    tree <- as.numeric (substring (myfiles, 12, 13))
  }  
  if (study == 'Exp2019') {
    chamber <- as.numeric (substring (myfiles, 16, 16))
  } else if (study != 'Obs2018' & study != 'Obs2019') {
    chamber <- as.numeric (substring (myfiles, 16, 16))
  } else {
    chamber <- rep (NA, length (tree))
  }
  
  # Correct tree numbers is they were still the old ones
  #------------------------------------------------------------------------------------#
  if (timestamp [1] < as.POSIXct ('20180620', format = '%Y%m%d') & study == 'Exp2018') {
    # Clean up data to make sure the trees are number correctly
    # N.B. the numbering was changed on the 2018/06/xx to reflect the imposed 
    # changes to the experimental design, aka adding additional trees to the 
    # treatments. 
    #---------------------------------------------------------------------------#
    tree [timestamp < as.POSIXct ('180620', format = '%y%m%d') & tree == 11] <- 14 # Original tree 11 became tree 14
    tree [timestamp < as.POSIXct ('180620', format = '%y%m%d') & tree == 12] <- 15 # Original tree 12 became tree 15
    
    tree [timestamp < as.POSIXct ('180620', format = '%y%m%d') & tree == 13] <- 11 # Original tree 13 became tree 11
    tree [timestamp < as.POSIXct ('180620', format = '%y%m%d') & tree == 9 ] <- 12 # Original tree 9 became tree 12
    
    tree [timestamp < as.POSIXct ('180620', format = '%y%m%d') & tree == 8 ] <- 9 # Original tree 8 became tree 9
    tree [timestamp < as.POSIXct ('180620', format = '%y%m%d') & tree == 10] <- 13 # Original tree 10 became tree 13
    
    tree [timestamp < as.POSIXct ('180620', format = '%y%m%d') & tree == 7 ] <- 10 # Original tree 7 became tree 10
    
    tree [timestamp < as.POSIXct ('180620', format = '%y%m%d') & tree == 6 ] <- 7  # Original tree 6 became tree 7
    
    tree [timestamp < as.POSIXct ('180620', format = '%y%m%d') & tree == 4 ] <- 6  # Original tree 4 became tree 6
    
    tree [timestamp < as.POSIXct ('180620', format = '%y%m%d') & tree == 5 ] <- 4  # Original tree 5 became tree 4
  }
  
  # geometry of chambers
  #------------------------------------------------------------------------------------#
  chamberGeometry <- calcChamberGeometryCylinder (radius = 0.0508,
                                                  height = 0.1016, 
                                                  taper  = 1.0)
  
  # Add treatment, now that the tree numbers are cleaned up
  #------------------------------------------------------------------------------------#
  if (study == 'Exp2018') {
    treatment <- rep ('control', length (tree))
    treatment [tree <= 10] <- 'compression'
    treatment [tree <= 5]  <- 'chilling'   
  } else if (study == 'Obs2018' | study == 'Obs2019') {
    treatment <- rep ('control', length (tree))
  } else if (study == 'Exp2019') {
    treatment [tree == 1] <- 'control'
    treatment [tree == 2] <- 'chilling'
    treatment [tree == 3] <- 'control'
    treatment [tree == 4] <- 'chilling'
    treatment [tree == 5] <- 'control'
    treatment [tree == 6] <- 'chilling'
    treatment [tree == 7] <- 'chilling'
    treatment [tree == 8] <- 'control'
  }
  
  # Put all of that together into a data frame, seperating all the elements into columns 
  #------------------------------------------------------------------------------------#
  sessiondata <- tibble (file      = myfiles,
                         treatment = treatment,
                         tree      = tree,
                         chamber   = chamber,
                         timestamp = as.POSIXct (timestamp),
                         session   = date_time,
                         flux      = NA,
                         sdFlux    = NA, 
                         ea.Pa     = NA, 
                         airt.C    = NA, 
                         pres.Pa   = NA, 
                         H2O.ppt   = NA)
  
  # Pull appropriate meterological data from the HF website to account for those factors
  #------------------------------------------------------------------------------------#
  weatherdate <- Sys.Date ()
  weatherdate <- paste (substring (weatherdate, 1, 7), '01', sep = '-')
  
  if (as.POSIXct (date_time,   format = '%Y%m%d_%H%M') < 
      as.POSIXct (weatherdate, format = '%Y-%m-%d')) {
    met_HF <- read_csv (file = url ("http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/data/p00/hf001/hf001-10-15min-m.csv","rb"))
  } else if (as.POSIXct (date_time,   format = '%Y%m%d_%H%M') >= 
             as.POSIXct (weatherdate, format = '%Y-%m-%d')) {
    met_HF <- read_csv (file = url ("http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/qfm.csv","rb"))
    #met_HF <- read_csv (file = '~/Desktop/qfm.csv')
  }
  met_HF$TIMESTAMP <- as.POSIXct (met_HF$datetime, 
                                  format = '%Y-%m-%dT%H:%M')
  attr (met_HF$TIMESTAMP, "tzone") <- "EST"
  
  # Loop through each measurement in the session 
  #------------------------------------------------------------------------------------#
  for (ifile in  1:nrow (sessiondata)) {
    
    # assign each of the files to a general variable "currentfile"
    currentfile <- sprintf ('./data/%s/%s',
                            date_time,
                            sessiondata$file [ifile])
    # read in the data file, and assign it to another generable variable "measurement"
    measurement <- read_csv (file = currentfile)
    
    dat <- selectData (ds = measurement,
                       lowerBound = stemLowerBound [ifile],
                       upperBound = stemUpperBound [ifile],
                       plotB = verbose)
    if (verbose) { 
      title (main = paste ('Stem Respiration:', 'tree', sessiondata$tree [ifile], 'chamber',
                           sessiondata$chamber [ifile], sessiondata$timestamp [ifile]))
    }
    # to determine which weather measurements to use, we'll find the next 15 minute interval 
    next_interval <- as.POSIXct (x = (round (as.numeric (median (sessiondata$timestamp [ifile]))/
                                               (15 * 60)) * (15 * 60) + (15 * 60)), format = '%Y-%m-%d %H:%M:%S',
                                 origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', tz = 'UTC'), 
                                 tz = 'EST')
    
    pres.Pa <- met_HF$bar  [met_HF$TIMESTAMP == next_interval & !is.na (met_HF$TIMESTAMP)] * 100.0 # Pa
    airt.C  <- met_HF$airt [met_HF$TIMESTAMP == next_interval & !is.na (met_HF$TIMESTAMP)]         # deg C
    rh.per  <- met_HF$rh   [met_HF$TIMESTAMP == next_interval & !is.na (met_HF$TIMESTAMP)]         # %
    
    # Calculate saturation water vapour pressure (esat) to convert relative humidity
    #------------------------------------------------------------------------------------#
    es.Pa <- 0.61078 * exp ((17.269 * airt.C) / (237.3 + airt.C)) * 1000 # saturated water pressure [Pa]
    ea.Pa <- es.Pa * rh.per / 100.0                                         # get actual water vapour pressure [Pa]
    dat$ea.Pa   <- rep (ea.Pa,   dim (dat [, 1]) [1])   # add actual water vapour pressure [Pa] to sessiondata data.frame
    dat$airt.C  <- rep (airt.C,  dim (dat [, 1]) [1])   # add air temperature [degC] to sessiondata data.frame
    dat$pres.Pa <- rep (pres.Pa, dim (dat [, 1]) [1])   # add atmospheric pressure [Pa] to aalldat data.frame
    dat$H2O.ppt <- dat$ea.Pa / (dat$pres.Pa - dat$ea.Pa) * 1.0e3   # add actual water vapour pressure [ppt] to sessiondata data.frame
    
    # Rename CO2 column
    names (dat) [which (names (dat) == "CO2")] <- "CO2.ppm"
    # Correct CO2 concentration for water vapour
    dat$CO2.dry <- corrConcDilution (dat, 
                                     colConc   = 'CO2.ppm',
                                     colVapour = 'H2O.ppt')
    
    # Calculate chamber flux for entire timeseries
    #------------------------------------------------------------------------------------#
    resFit <- calcClosedChamberFlux (dat,
                                     colConc     = 'CO2.dry',
                                     colTime     = 'RunTime', # redundant
                                     colTemp     = 'airt.C',
                                     colPressure = 'pres.Pa',
                                     volume      = chamberGeometry [1],
                                     area        = chamberGeometry [2])
    
    # Put all the data into the table 'sessiondata" you created earlier
    #------------------------------------------------------------------------------------#
    sessiondata$flux    [ifile] <- resFit$flux
    sessiondata$sdFlux  [ifile] <- resFit$sdFlux
    sessiondata$ea.Pa   [ifile] <- ea.Pa   # add actual water vapour pressure [Pa] to alldata data.frame
    sessiondata$airt.C  [ifile] <- airt.C   # add air temperature [degC] to alldata data.frame
    sessiondata$pres.Pa [ifile] <- pres.Pa   # add atmospheric pressure [Pa] to aalldat data.frame
    sessiondata$H2O.ppt [ifile] <- ea.Pa / (pres.Pa - ea.Pa) * 1.0e3   
    
    # Plot data, if so desired
    if (verbose) {
      par (mfrow = c (1, 1))
      plot (x = dat [['RunTime']], 
            y = dat [['CO2.dry']],
            xlab = 'time [s]',
            ylab = 'CO2 concentration [ppm]')
      title (main = paste ('Stem Respiration:', 'tree', sessiondata$tree [ifile], 'chamber',
                           sessiondata$chamber [ifile], sessiondata$timestamp [ifile]))
      
      # plot selected data bounds
      points (x = dat [['RunTime']],
              y = dat [['CO2.dry']],
              col  = '#91b9a499',
              pch  = 19,
              cex  = 0.9)
      
      # Add a line for the calculated slope # TTR something still wrong with the intercept
      #------------------------------------------------------------------------------------#
      abline (lm (dat [['CO2.dry']] ~ dat [['RunTime']]),
              lwd = 4,
              col = '#91b9a499')
    }
  }
  #flux units are micromol/sec. can use conver_mmol function to get grams/day
  
  return (sessiondata)
}

# Calculate the rates for the sessions
#--------------------------------------------------------------------------------------#
sessionData <- calcSession (study, date_time, verbose = T)

# Save session data
#--------------------------------------------------------------------------------------#
saveRDS (file = sprintf ('%s_sessionData.rds', date_time), sessionData)
#--------------------------------------------------------------------------------------#