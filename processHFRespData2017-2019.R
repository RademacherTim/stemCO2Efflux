#========================================================================================
# One script to process all LiCor-820 and LiCor-840 respiration data from Harvard Forest
# 
# Script written by Tim Rademacher, Postdoc at Harvard University and 
#                                           Northern Arizona University 
# Email: trademacher@fas.harvard.edu
#
# Last updated: 2019-12-18
#
# Data are either LiCor-820 and LiCor-840 outputs or outputs of FluxPuppy (Carbone et 
# al., 2019) for either cylindrical stem chambers (4" diameter and a depth of 3" or 4") 
# or cylindrical soil respiration chambers (6" diameter and a 2" of depth).
#
# Data was collected by: Tim Rademacher, 
#                        Brooklynn Davis, 
#                        David Basler,
#                        Elise Miller,
#                        Emory Elis, 
#                        Kyle Wyche. 
#
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('googledrive')
library ('tidyverse')

# load source preprocess data (including chamber volume and bounds, plotting function)
#----------------------------------------------------------------------------------------
fileNames <- list.files (path = './RespChamberProc/R/', pattern = "*.R") [-5]
res <- sapply (sprintf ('./RespChamberProc/R/%s', fileNames), source); rm (res)

# load script to truncate data
#----------------------------------------------------------------------------------------
source ('selectData.R')

# set study for which to downlaod files
# TR - Eventually I should loop over all studies
#----------------------------------------------------------------------------------------
study <- 'Exp2019'

# get list of all dates for a study
#----------------------------------------------------------------------------------------
listGD <- drive_ls (paste0 ('./Respiration/',study,'/'))
measurementDates <- listGD [['name']]

# create a temporary directory
#----------------------------------------------------------------------------------------
tempDirName <- tempdir ()
workingDir <- getwd ()

# loop over dates
#----------------------------------------------------------------------------------------
for (dateTime in measurementDates) {
  
  # now processing 
  #--------------------------------------------------------------------------------------
  print (paste0 ('Now processing: ',dateTime))
  
  # list all respiration files measured at the same date and time
  #--------------------------------------------------------------------------------------
  listDir <- drive_ls (paste0 ('./Respiration/',study,'/',dateTime,'/'))
  
  # sort out meta-data files for now to reduce runtime
  #--------------------------------------------------------------------------------------
  listDir <- listDir %>% filter (substr (listDir [['name']], 1, 1) == 'G')
  
  # change directory to temporary directory
  #--------------------------------------------------------------------------------------
  setwd (tempDirName)
  
  # download the data for each date into a temporary folder
  #--------------------------------------------------------------------------------------
  res <- sapply (1:dim (listDir) [1], function (x) drive_download (file = as_id (listDir [['id']] [x],
                                                                                 verbose = FALSE, 
                                                                                 overwrite = TRUE)))
  rm (res)
  
  # return to original working directory
  #--------------------------------------------------------------------------------------
  setwd (workingDir)
  
  # read bounds for each file
  #------------------------------------------------------------------------------------#
  bounds <- read_csv (file = paste0 ('../data/respiration/Resp',study,'-bounds.csv'),
                      col_types = cols ())
  bounds <- bounds [bounds [['TIMESTAMP']] == dateTime, ]
  # TR does not work anymore bounds <- bounds [!is.na (bounds)]
  lowerBounds <- as.numeric (bounds [seq (3, length (bounds), 2)])
  upperBounds <- as.numeric (bounds [seq (4, length (bounds), 2)])
  
  # filter out only the relevant files for the study
  #--------------------------------------------------------------------------------------
  listDir <- listDir %>% filter (substring (listDir [['name']], 3, 2 + nchar (study)) == study)
  
  # extract metadata from file name
  #--------------------------------------------------------------------------------------
  tmp <- unlist (strsplit (listDir [['name']], '_'))
  datestr <- tmp [seq (3, length (tmp), 4)]
  timestr <- tmp [seq (4, length (tmp), 4)]
  timestr <- substring (timestr, 1, nchar (timestr) - 4)
  timestamp <- strptime (paste (datestr, timestr),"%Y%m%d %H%M%S", tz = 'EST')
  
  # get tree identifiers
  #--------------------------------------------------------------------------------------
  if (study == 'Exp2019') {
    treeIDs <- as.numeric (substring (listDir [['name']], 14, 14))
  } else if (study != 'Obs2018' & study != 'Obs2019') {
    treeIDs <- as.numeric (substring (listDir [['name']], 11, 12))
  } else if (study == 'Exp2018') {
    treeIDs <- as.numeric (substring (listDir [['name']], 12, 13))
  } else if (study == 'Exp2017') {
    treeIDs <- as.numeric (substring (listDir [['name']], 12, 13))
  }
  
  # get chamber identifiers
  #--------------------------------------------------------------------------------------
  if (study == 'Exp2019') {
    chamberIDs <- as.numeric (substring (listDir [['name']], 16, 16))
  } else if (study != 'Obs2018' & study != 'Obs2019') {
    chamberIDs <- as.numeric (substring (listDir [['name']], 16, 16))
  } else if (study == 'Exp2018') {
    chamberIDs <- rep (NA, length (treeIDs))
  }
  
  # For the Exp2018 the treeIDs changed, here we correct for obsolete treeIDs
  #--------------------------------------------------------------------------------------
  if (timestamp [1] < as.POSIXct ('20180620', format = '%Y%m%d') & study == 'Exp2018') {
    # Clean up data to make sure the trees are number correctly
    # N.B. the numbering was changed on the 2018/06/xx to reflect the imposed 
    # changes to the experimental design, aka adding additional trees to the 
    # treatments. 
    #------------------------------------------------------------------------------------
    treeIDs [timestamp < as.POSIXct ('180620', format = '%y%m%d') & treeIDs == 11] <- 14 # Original tree 11 became tree 14
    treeIDs [timestamp < as.POSIXct ('180620', format = '%y%m%d') & treeIDs == 12] <- 15 # Original tree 12 became tree 15
    treeIDs [timestamp < as.POSIXct ('180620', format = '%y%m%d') & treeIDs == 13] <- 11 # Original tree 13 became tree 11
    treeIDs [timestamp < as.POSIXct ('180620', format = '%y%m%d') & treeIDs == 9 ] <- 12 # Original tree 9 became tree 12
    treeIDs [timestamp < as.POSIXct ('180620', format = '%y%m%d') & treeIDs == 8 ] <- 9 # Original tree 8 became tree 9
    treeIDs [timestamp < as.POSIXct ('180620', format = '%y%m%d') & treeIDs == 10] <- 13 # Original tree 10 became tree 13
    treeIDs [timestamp < as.POSIXct ('180620', format = '%y%m%d') & treeIDs == 7 ] <- 10 # Original tree 7 became tree 10
    treeIDs [timestamp < as.POSIXct ('180620', format = '%y%m%d') & treeIDs == 6 ] <- 7  # Original tree 6 became tree 7
    treeIDs [timestamp < as.POSIXct ('180620', format = '%y%m%d') & treeIDs == 4 ] <- 6  # Original tree 4 became tree 6
    treeIDs [timestamp < as.POSIXct ('180620', format = '%y%m%d') & treeIDs == 5 ] <- 4  # Original tree 5 became tree 4
  }
  
  # geometry of chambers
  #--------------------------------------------------------------------------------------
  if (study == 'Exp2019') {
    h1 <- 0.0762
  } else if (study == 'Exp2018') {
    # tree number 5, 8, 9, 14, 15  
    h1 <- c (rep (0.1016, 12), rep (0.0762, 3), rep (0.1016, 6), rep (0.0762, 6), rep (0.1016, 12), rep (0.762, 6)) 
  } else {
    h1 <- 0.1016
  }
  chamberGeometry <- calcChamberGeometryCylinder (radius = 0.0508, # chamber radius
                                                  height = h1,     # chamber depth
                                                  taper  = 1.0)    # taper of chamber ranging from 0 for ... to 1 for ...

  # Add treatment
  #--------------------------------------------------------------------------------------
  if (study == 'Exp2018') {
    treatment <- rep ('control', length (treeIDs))
    treatment [treeIDs <= 10] <- 'compression'
    treatment [treeIDs <= 5]  <- 'chilling'   
  } else if (study == 'Obs2018' | study == 'Obs2019') {
    treatment <- rep ('control', length (treeIDs))
  } else if (study == 'Exp2019') {
    treatment <- rep ('control', length (treeIDs))
    treatment [treeIDs == 1] <- 'control'
    treatment [treeIDs == 2] <- 'chilling'
    treatment [treeIDs == 3] <- 'control'
    treatment [treeIDs == 4] <- 'chilling'
    treatment [treeIDs == 5] <- 'control'
    treatment [treeIDs == 6] <- 'chilling'
    treatment [treeIDs == 7] <- 'chilling'
    treatment [treeIDs == 8] <- 'control'
  }
  
  # Put all info together into a tibble
  #--------------------------------------------------------------------------------------
  sessionData <- tibble (file      = listDir [['name']],
                         treatment = treatment,
                         tree      = treeIDs,
                         chamber   = chamberIDs,
                         timestamp = as.POSIXct (timestamp),
                         session   = dateTime,
                         flux      = NA,
                         sdFlux    = NA, 
                         ea.Pa     = NA, 
                         airt.C    = NA, 
                         pres.Pa   = NA, 
                         H2O.ppt   = NA)
  
  # Pull appropriate meterological data from the HF website
  #--------------------------------------------------------------------------------------
  met_HF <- read_csv (file = url ("http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/data/p00/hf001/hf001-10-15min-m.csv","rb"))
  met_HF$TIMESTAMP <- as.POSIXct (met_HF$datetime, 
                                  format = '%Y-%m-%dT%H:%M')
  attr (met_HF$TIMESTAMP, "tzone") <- "EST"
  
  # TR - Maybe I could also include soil moisture here in the future
  #--------------------------------------------------------------------------------------

  
  # Loop through each measurement in the session 
  #--------------------------------------------------------------------------------------
  for (ifile in  1:nrow (sessionData)) {
    
    # assign each of the files to a general variable "currentfile"
    #------------------------------------------------------------------------------------
    currentFile <- sprintf ('%s/%s',
                            tempDirName,
                            sessionData$file [ifile])
    
    # read in the data file
    #------------------------------------------------------------------------------------
    measurement <- read_csv (file = currentFile, col_types = cols ())
    
    
    # truncate data to select only reasonable values
    #------------------------------------------------------------------------------------
    dat <- selectData (ds = measurement,
                       lowerBound = lowerBounds [ifile],
                       upperBound = upperBounds [ifile],
                       plotB = TRUE)
    
    # Find closest 15 minute interval
    #------------------------------------------------------------------------------------
    next_interval <- as.POSIXct (x = (round (as.numeric (median (sessionData$timestamp [ifile]))/
                                               (15 * 60)) * (15 * 60) + (15 * 60)), format = '%Y-%m-%d %H:%M:%S',
                                 origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', tz = 'UTC'), 
                                 tz = 'EST')
    
    # Extract pressure in Pa air temperature in deg C and relative humidity in percent 
    # from meteorological data
    #------------------------------------------------------------------------------------
    pres.Pa <- met_HF [['bar']]  [met_HF [['TIMESTAMP']] == next_interval & !is.na (met_HF [['TIMESTAMP']])] * 100.0 # Pa
    airt.C  <- met_HF [['airt']] [met_HF [['TIMESTAMP']] == next_interval & !is.na (met_HF [['TIMESTAMP']])]         # deg C
    rh.per  <- met_HF [['rh']]   [met_HF [['TIMESTAMP']] == next_interval & !is.na (met_HF [['TIMESTAMP']])]         # %
    
    # Calculate saturation water vapour pressure (esat) to convert relative humidity
    #------------------------------------------------------------------------------------
    es.Pa <- 0.61078 * exp ((17.269 * airt.C) / (237.3 + airt.C)) * 1000 # saturated water pressure [Pa]
    ea.Pa <- es.Pa * rh.per / 100.0                                         # get actual water vapour pressure [Pa]
    dat [['ea.Pa']]   <- rep (ea.Pa,   dim (dat [, 1]) [1])   # add actual water vapour pressure [Pa] to sessiondata data.frame
    dat [['airt.C']]  <- rep (airt.C,  dim (dat [, 1]) [1])   # add air temperature [degC] to sessiondata data.frame
    dat [['pres.Pa']] <- rep (pres.Pa, dim (dat [, 1]) [1])   # add atmospheric pressure [Pa] to aalldat data.frame
    dat [['H2O.ppt']] <- dat [['ea.Pa']] / (dat [['pres.Pa']] - dat [['ea.Pa']]) * 1.0e3   # add actual water vapour pressure [ppt] to sessiondata data.frame
    
    # Rename CO2 column
    #------------------------------------------------------------------------------------
    names (dat) [which (names (dat) == "CO2")] <- "CO2.ppm"
    
    # Correct CO2 concentration for water vapour
    #------------------------------------------------------------------------------------
    dat [['CO2.dry']] <- corrConcDilution (dat, 
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
    sessionData [['flux']]    [ifile] <- resFit [['flux']] # flux is in micromol / s
    sessionData [['sdFlux']]  [ifile] <- resFit [['sdFlux']]
    sessionData [['ea.Pa']]   [ifile] <- ea.Pa   # add actual water vapour pressure [Pa] to alldata data.frame
    sessionData [['airt.C']]  [ifile] <- airt.C   # add air temperature [degC] to alldata data.frame
    sessionData [['pres.Pa']] [ifile] <- pres.Pa   # add atmospheric pressure [Pa] to aalldat data.frame
    sessionData [['H2O.ppt']] [ifile] <- ea.Pa / (pres.Pa - ea.Pa) * 1.0e3   

    # Plot data, if so desired
    par (mfrow = c (1, 1))
    plot (x = dat [['RunTime']], 
          y = dat [['CO2.dry']],
          xlab = 'time [s]',
          ylab = 'CO2 concentration [ppm]')
    title (main = paste ('Stem Respiration:', 'tree', sessionData$tree [ifile], 'chamber',
                         sessionData$chamber [ifile], sessionData$timestamp [ifile]))
      
    # plot selected data bounds
    points (x = dat [['RunTime']],
            y = dat [['CO2.dry']],
            col  = '#91b9a499',
            pch  = 19,
            cex  = 0.9)
      
    # add a line for the calculated slope
    #------------------------------------------------------------------------------------
    abline (lm (dat [['CO2.dry']] ~ dat [['RunTime']]),
            lwd = 4,
            col = '#91b9a499')
  }
  
  # save the respiration session data for this date time
  #--------------------------------------------------------------------------------------
  saveRDS (sessionData, file = paste0 (dateTime,'_sessionData.rds'))
}

# Delete the temporary directory and containing files
#----------------------------------------------------------------------------------------
unlink (tempDirName, recursive = T) 

# Read all processed data
# Start with processed data from 2017 experiment, which was not using FluxPuppy
# After that read all the .rds file with data for each session
#----------------------------------------------------------------------------------------
data <- read_csv (file = '../data/respiration/resp_compression_2017_11_01.csv', 
                  col_types = cols ())



#========================================================================================