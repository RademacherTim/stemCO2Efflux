#=========================================================================================#
# Working with RespChamberProc using the split data
#-----------------------------------------------------------------------------------------#

# Start from clean slate
#-----------------------------------------------------------------------------------------#
rm (list = ls ()) 

# Load the necessary libraries
#-----------------------------------------------------------------------------------------#
library (segmented)
library (tibble)

# Load the RespChamber package functions from local directory
#-----------------------------------------------------------------------------------------#
fileList = list.files (path = './R/', pattern = "*.R")
fileList <- paste ('./R/', fileList [-5], sep = '')
sapply (fileList, source, .GlobalEnv)

# Create data.frame with tree ID, 
#                        week, 
#                        flux (micromol m-2 s-1), 
#                        median flux (micromol m-2 s-1) 
#                        flux standard deviation (micromol m-2 s-1)
#                        flux (g m-2 d-1),
#                        median flux (g m-2 d-1), 
#                        flux standard deviation (g m-2 d-1)
#                        AIC
#                        r2
#-----------------------------------------------------------------------------------------#
if (NEW) {
  resp <- data.frame (tree         = integer (),
                      chamber      = integer (),
                      date         = character (),
                      flux         = double (),
                      fluxMedian   = double (),
                      fluxSD       = double (),
                      flux_g       = double (),
                      fluxMedian_g = double (),
                      fluxSD_g     = double (),
                      AIC          = double (),
                      r2           = double ())
} else {
  resp <- readRDS ('../../../Obs2018/stem_respiration/resp.dat')
  names (resp) <- c ('tree','chamber','date','flux','fluxMedian','fluxSD','flux_g',
                     'fluxMedian_g','fluxSD_g','AIC','r2')
}

# Get dataframe with tree data
#-----------------------------------------------------------------------------------------#
study <- 'Obs2018'
if (study == 'Exp2017') {  
  trees <- read.csv ('/home/ttrademacher/NSF-DB Plant Growth/Exp2017/selected_trees.csv')
  # Paste tree id and treatment for filename
  changeIDs <- function (x) {
    if (x < 10) {
      paste ('0', as.character (trees$X [x]), 'p', as.character (trees$treatment [x]), sep = '')
    } else {
      paste (     as.character (trees$X [x]), 'p', as.character (trees$treatment [x]), sep = '')
    }
  }
  treeIDs <- sapply (1:40, changeIDs)
  treeIDs <- c (treeIDs, '41p2')

  # Function to extract last character and transform in numeric
  lastCh <- function (x, n) {as.numeric (substr (x, nchar(x)-n+1, nchar(x)))}
  
  # Determine treatment and number of chamber on each tree
  #-------------------------------------------------------------------------------------#
  treatments  = lastCh (treeIDs, n = 1)
  chamberNums = treatments
  chamberNums [treatments == 3] = 2
  chamberNums [treatments == 4] = 3
  
} else if (study == 'Obs2018') {
  treeIDs <- c ('Q05','Q06','P07','A08','Q09','Q10','Q11','Q12','Q13','Q15','P16','A18',
                'A19','P20','P21','A23','P25','P26','A28','A29')
  chamberNums <- 1
}

# Calcualte chamberGeometry
#-----------------------------------------------------------------------------------------#
chamberGeometry <- calcChamberGeometryCylinder (radius = 0.0508, 
                                                height = 0.1016, 
                                                taper  = 1.0)

# Get atmospheric pressure and water vapour concentration from the Fisher meteorological station at Harvard Forest
#-----------------------------------------------------------------------------------------#
#met_HF <- read.csv (file = url ('http://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-10-15min-m.csv'))
met_HF <- read.csv (file = url ('http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/weather/qfm.csv'))
met_HF$TIMESTAMP <- as.POSIXct (met_HF$datetime, 
                                format = '%Y-%m-%dT%H:%M',
                                tz = 'EST') 

# Function to determine chamber flux
#-----------------------------------------------------------------------------------------#
calcChamber <- function (inSamplingDate = samplingDate, inTreeIDs = treeIDs, 
                         inTreeIndex = treeIndex, inChamberIndex = chamberIndex, 
                         inMet_HF = met_HF) {
  # Save tibble of the example data with correct date
  fileName = paste ('/home/ttrademacher/NSF-DB Plant Growth/',study,'/stem_respiration/processed/',
                    inSamplingDate,'/',inTreeIDs [inTreeIndex],'_',inChamberIndex,'_',inSamplingDate,'.txt', sep = '')
  ds <- readDat (fName         = fileName,
                 nRowsFileInfo = 1,
                 nRowsColInfo  = 1,
                 sep           = ' ',
                 colsTimeStamp = 1,
                 formatTS      = '%H:%M:%S',
                 tz            = 'EST')
  closeAllConnections ()
  date <- substr (readLines (fileName, n = 1), 2, 11)
  ds$Time.H.M.S. <- as.POSIXct (gsub (as.character (ds$Time.H.M.S.),
                                      pattern = substr (Sys.time (), 1, 10),
                                      replacement = date),
                                tz = 'EST')
  
  # Determine which measurement to use, N.B. data is named after the time at the end of the 15 minute interval
  upper_bound <- as.POSIXct (x      = round (as.numeric (median (ds$Time.H.M.S.)) / (15 * 60)) * (15 * 60) + (15 * 60), 
                             format = '%Y-%m-%d %H:%M:%S',
                             origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', tz = 'UTC'), 
                             tz = 'EST')
  
  # Select appropriate pressure and humidity
  presPa <- inMet_HF$bar  [met_HF$TIMESTAMP == upper_bound] * 100.0 # Pa
  airt   <- inMet_HF$airt [met_HF$TIMESTAMP == upper_bound]         # deg C
  rh     <- inMet_HF$rh   [met_HF$TIMESTAMP == upper_bound]         # %
  
  # Calculate saturation water vapour pressure (esat) to convert relative humidity
  es <- 0.61078 * exp ((17.269 * airt) / (237.3 + airt)) * 1000 # saturated water pressure [Pa]
  ea <- es * rh / 100.0                                         # get actual water vapour pressure [Pa]
  ds$ea      <- rep (ea,   length (ds [, 1]))
  ds$airt    <- rep (airt, length (ds [, 1]))
  ds$presPa  <- rep (presPa, length (ds [, 1]))
  ds$H2O.ppt <- ds$ea / (ds$presPa - ds$ea) * 1.0e3
  
  # Add a column with time in seconds
  ds$TIMESTAMP <- as.POSIXct (ds$Time.H.M.S., 
                              format = '%Y-%m-%d %H:%M:%S',
                              tz = 'EST') 
  
  # Correct CO2 concentration for wter vapour
  ds$CO2_dry <- corrConcDilution (ds, 
                                  colConc   = 'CO2.ppm.',
                                  colVapour = 'H2O.ppt')
  
  # Calculate chamber flux for entire timeseries
  resfit <- calcClosedChamberFlux (ds,
                                   colConc     = 'CO2_dry',
                                   # colTime     = 'TIMESTAMP', # redundant
                                   colTemp     = 'airt',
                                   colPressure = 'presPa',
                                   volume      = chamberGeometry [1],
                                   area        = chamberGeometry [2])
  
  # Plot the chunks for the entire timeseries
  png (filename = paste ('fig/',inTreeIDs [inTreeIndex],'_',inChamberIndex,
                        '_',inSamplingDate,'.png', sep = ''))
  plotResp (ds, 
            resFlux = resfit,
            colConc = 'CO2_dry', # redundant
            colTime = 'TIMESTAMP',
            label = paste (inTreeIDs [inTreeIndex], inSamplingDate))
  dev.off ()
  plotResp (ds, 
            resFlux = resfit,
            colConc = 'CO2_dry', # redundant
            colTime = 'TIMESTAMP',
            label = paste (inTreeIDs [inTreeIndex], inSamplingDate))
  
  return (resfit)
}

# Determine sampling date
#-----------------------------------------------------------------------------------------#
#samplingDates = c ('2017-06-19','2017-06-21','2017-06-23','2017-06-28','2017-07-06',
#                   '2017-07-12','2017-07-19','2017-07-26','2017-08-03','2017-08-09',
#                   '2017-08-13','2017-08-16','2017-08-23','2017-08-30','2017-09-06',
#                  '2017-09-13','2017-09-20','2017-09-27','2017-10-05','2017-10-11',
#                  '2017-10-18','2017-10-27')
#samplingDate  = samplingDates [22]
samplingDate <- '2018-04-14' #'2018-04-09'

# Set treeIndex and chamberIndex
#-----------------------------------------------------------------------------------------#
treeIndex    = 15
chamberIndex = 1

# Calculate chamber fluxes
#-----------------------------------------------------------------------------------------#
resfit <- calcChamber ()

# Combine the flux and its stats to be added to the resp data.frame
#-----------------------------------------------------------------------------------------#
addition <- c (treeIndex, chamberIndex, samplingDate, resfit [1:3], 
               convert_mumolPers_to_gPerday (resfit [1:3]),
               resfit [c (7, 13)])
names (addition) <- names (resp)

# Name columns
#-----------------------------------------------------------------------------------------#
#if ((study == 'Exp2017' & samplingDate == '2017-06-19' & treeIndex == 1) |
#    (study == 'Obs2018' & samplingDate == '2018-04-09' & treeIndex == 1) ) {
#  names (resp) <- c ('tree','chamber','date','flux','fluxMedian',
#                     'sdFlux','flux.g','fluxMedian.g','sdFlux.g','AIC','r2')  
#} else {
#  names (addition) <- names (resp)
#}

# Add an empty row to the data.frame, if there is no data
#-----------------------------------------------------------------------------------------#
#addition <- c (treeIndex, chamberIndex, samplingDate, rep (NA, 8))
#addition 

# Add tree to resp data.frame and display the tail to check for mistakes in sequence
#-----------------------------------------------------------------------------------------#
resp <- rbind (resp, addition)
tail (resp)

# Save the resp data.frame
#-----------------------------------------------------------------------------------------#
saveRDS (resp, file = paste ('/home/ttrademacher/NSF-DB Plant Growth/',study,'/stem_respiration/resp.dat', sep = ''))

# Delete a row, if there was a mistake
#-----------------------------------------------------------------------------------------#
#resp <- resp [-21, ]

# Add missing row where it belongs
#-----------------------------------------------------------------------------------------#
#resp <- rbind (resp [1:34,], 
#               addition, 
#              resp [36:length (resp [, 1]), ])

#=========================================================================================#