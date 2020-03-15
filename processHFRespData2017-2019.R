#========================================================================================
# One script to process all LiCor-820 and LiCor-840 respiration data from Harvard Forest
# 
# Script written by Tim Rademacher, Postdoc at Harvard University and 
#                                           Northern Arizona University 
# Email: trademacher@fas.harvard.edu
#
# Last updated: 2020-02-28
#
# Data are either LiCor-820 and LiCor-840 outputs or outputs of FluxPuppy (Carbone et 
# al., 2019) for either cylindrical stem chambers (4" diameter and a depth of 3" or 4").
#
# Data was collected by: Tim Rademacher, 
#                        Brooklynn Davis, 
#                        David Basler,
#                        Elise Miller,
#                        Emory Elis, 
#                        Kyle Wyche,
#                        Shawna Greyeyes
#
#----------------------------------------------------------------------------------------

# TR - To-do: - Process soil respiration measurements

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')
library ('hablar')

# load source preprocess data (including chamber volume and bounds, plotting function)
#----------------------------------------------------------------------------------------
fileNames <- list.files (path = './RespChamberProc/R/', pattern = "*.R") [-5]
res <- sapply (sprintf ('./RespChamberProc/R/%s', fileNames), source); rm (res)

# load script to truncate data
#----------------------------------------------------------------------------------------
source ('selectData.R')

# determine which machine you are on
#----------------------------------------------------------------------------------------
machine <- 'timNAU'

# set path to the data directory
#----------------------------------------------------------------------------------------
if (machine == 'timNAU') {
  dirPath <- '/media/tim/dataDisk/PlantGrowth/data/respiration/'
} else if (machine == 'timPersonal') {
  dirPath <- '../data/'
}

# read climate data from the Fisher meteorological station 
#--------------------------------------------------------------------------------------
if (!exists ('met_HF')) {
  if (machine == 'timNAU') {
    met_HF <- read_csv (file = "/media/tim/dataDisk/PlantGrowth/data/environmentalData/hf001-10-15min-m.csv", col_types = cols ())
  } else if (machine == 'timPersonal') {
    met_HF <- read_csv (file = '../data/respiration/hf001-10-15min-m.csv', col_types = cols ())
  }
  met_HF$TIMESTAMP <- as.POSIXct (met_HF$datetime,
                                  format = '%Y-%m-%dT%H:%M')
  attr (met_HF$TIMESTAMP, "tzone") <- "EST"
}

# read soil moisture data from the Barn Tower
#--------------------------------------------------------------------------------------
if (!exists ('soilMoisture_HF')) {
  soilMoistureBarn <- read_csv (file = '/home/tim/projects/PlantGrowth/data/environmentalData/BarnTowerData.dat',
                                 col_types = cols (), skip = 4, 
                                 col_names = c ("TIMESTAMP","RECORD","BattV_Min",
                                                "Panel_Temp_C_Avg","Air_Temp_C_Avg",
                                                "Soil_Temp_C_1","Soil_Temp_C_2",
                                                "Soil_Temp_C_3","Soil_Temp_C_4",
                                                "Total_Rad_Avg","Diffuse_Rad_Avg",
                                                "DeltaT_Avg","Diffuse_Rad_Max",
                                                "DeltaT_Max(1)","DeltaT_Max(2)",
                                                "Direct_Rad_Std","Total_Rad_Std",
                                                "Diffuse_Rad_Std","RH","VWC_1","VWC_2",
                                                "VWC_3","VWC_4","EC_1","EC_2","EC_3",
                                                "EC_4")) %>% 
                       select (TIMESTAMP, Soil_Temp_C_1, Soil_Temp_C_2, Soil_Temp_C_3, 
                               Soil_Temp_C_4, VWC_1, VWC_2, VWC_3, VWC_4, Total_Rad_Avg, 
                               Diffuse_Rad_Avg)
  soilMoisture_HF <- read_csv (file = '/home/tim/projects/PlantGrowth/data/environmentalData/soilMoisture/soilMoisture.csv',
                               col_types = cols ())
}

# loop over studies for which to process files
#----------------------------------------------------------------------------------------
for (study in c ('Exp2017','Exp2018','Exp2019','Obs2018','Obs2019','SoilResp2018')) {
  
  # get list of all dates for a study
  #----------------------------------------------------------------------------------------
  if (machine == 'timNAU') {
    tmp <- list.dirs (paste0 (dirPath,'raw/',study,'/'), full.names = FALSE, 
                      recursive = FALSE)
  } else if (machine == 'timPersonal') {
    tmp <- list.dirs (paste0 (dirPath,study,'/'), full.names = FALSE, recursive = FALSE)
  }
  tmp <- tmp [tmp != 'originalOldFormatRawData']
  
  # Sort out old format
  #----------------------------------------------------------------------------------------
  if (study == 'Exp2017' | study == 'Obs2018') {
    measurementDates <- tmp [substr (tmp, 1, 3) == '201']; rm (tmp)
  } else {
    measurementDates <- tmp; rm (tmp)  
  }
  
  # loop over dates
  #----------------------------------------------------------------------------------------
  for (dateTime in measurementDates) {
    
    # now processing 
    #--------------------------------------------------------------------------------------
    print (paste0 ('Now processing: ',dateTime,' for ',study))
    
    # list all respiration files measured at the same date and time
    #--------------------------------------------------------------------------------------
    if (exists ('listDir')) rm (listDir)
    if (machine == 'timNAU') {
      listDir <- list.files (paste0 (dirPath,'raw/',study,'/',dateTime,'/'))
    } else if (machine == 'timPersonal') {
      listDir <- list.files (paste0 (dirPath,study,'/',dateTime,'/'))
    }
    # sort out meta-data files for now to reduce runtime
    #--------------------------------------------------------------------------------------
    if (as.POSIXct (dateTime, format = '%Y%m%d_%H%M') > as.POSIXct ('2018-04-06')) {
      listDir <- listDir [substr (listDir, 1, 1) == 'G']      
    }
    
    # read bounds for each file
    #--------------------------------------------------------------------------------------
    if (machine == 'timNAU'){
      bounds <- read_csv (file = paste0 (dirPath,'raw/',study,'/StemResp',study,'-bounds.csv'),
                          col_types = cols ())
    } else if (machine == 'timPersonal') {
      bounds <- read_csv (file = paste0 (dirPath,study,'/StemResp',study,'-bounds.csv'),
                          col_types = cols ())
    }
    bounds <- bounds [bounds [['TIMESTAMP']] == dateTime, ]
    if (substr (study, 1, 3) == 'Exp') {
      boundaries <- tibble (boundary = as.numeric (bounds [2:dim (bounds) [2]]),
                            treeID = as.numeric (substr (names (bounds [2:dim (bounds) [2]]), 1, 2)),
                            chamberID = as.numeric (substr (names (bounds [2:dim (bounds) [2]]), 4, 4)),
                            lower = ifelse (substr (names (bounds [2:dim (bounds) [2]]), 5, 5) == 'l', TRUE, FALSE))
    } else {
      boundaries <- tibble (boundary = as.numeric (bounds [2:dim (bounds) [2]]),
                            treeID = as.numeric (substr (names (bounds [2:dim (bounds) [2]]), 2, 3)),
                            chamberID = 1,
                            lower = ifelse (substr (names (bounds [2:dim (bounds) [2]]), 4, 4) == 'l', TRUE, FALSE))
    }
    
    # # filter out only the relevant files for the study
    # #--------------------------------------------------------------------------------------
    # if (as.POSIXct (dateTime, format = '%Y%m%d_%H%M') > as.POSIXct ('2018-04-06')) {
    #   listDir <- listDir [substr (listDir, 3, 2 + nchar (study)) == study]
    # }
    # if (length (listDir) == 0) next
    
    # extract metadata from file name
    #--------------------------------------------------------------------------------------
    tmp <- unlist (strsplit (listDir, '_'))
    if (as.POSIXct (dateTime, format = '%Y%m%d_%H%M') > as.POSIXct ('2018-04-06')) {
      datestr <- tmp [seq (3, length (tmp), 4)]
      timestr <- tmp [seq (4, length (tmp), 4)]
      timestr <- substring (timestr, 1, nchar (timestr) - 4)
      timestamp <- strptime (paste (datestr, timestr),"%Y%m%d %H%M%S", tz = 'EST')
      rm (tmp, datestr, timestr)
    } else {
      datestr <- substr (tmp [seq (2, length (tmp), 2)], 1, 10)
      timestamp <- strptime (datestr,"%Y-%m-%d", tz = 'EST')
      rm (tmp, datestr)
    }
    
    # Get tree identifiers
    #--------------------------------------------------------------------------------------
    if (study == 'Exp2019') {
      treeIDs <- as.numeric (substring (listDir, 14, 14)) 
    } else if (study == 'Obs2018' | study == 'Obs2019') {
      treeIDs <- as.numeric (substring (listDir, 12, 13)) 
    } else if (study == 'Exp2018') {
      treeIDs <- as.numeric (substring (listDir, 11, 12))
    } else if (study == 'Exp2017') {
      if (as.POSIXct (dateTime, format = '%Y%m%d_%H%M') > as.POSIXct ('2018-04-06')) {
        treeIDs <- as.numeric (substring (listDir, 11, 12))
      } else {
        treeIDs <- as.numeric (substring (listDir, 1, 2))
      }
    }
    
    # get chamber identifiers
    #--------------------------------------------------------------------------------------
    if (study == 'Obs2018' | study == 'Obs2019') {
      chamberIDs <- rep (1, length (treeIDs))
    } else if (study == 'Exp2019' | study == 'Exp2018' | 
      (study == 'Exp2017' & as.POSIXct (dateTime, format = '%Y%m%d_%H%M') > as.POSIXct ('2018-04-06'))) {
      chamberIDs <- as.numeric (substr (listDir, 16, 16))
    }  else if (study == 'Exp2017') {
        chamberIDs <- as.numeric (substr (listDir, 6, 6))
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
      h1 <- treeIDs 
      h1 [h1 %in% c (5, 8, 9, 14, 15)] <- 0.0762 
      h1 [h1 != 0.0762] <- 0.1016
    } else {
      h1 <- 0.1016
    }
    chamberGeometry <- calcChamberGeometryCylinder (radius = 0.0508, # chamber radius
                                                    height = h1,     # chamber depth
                                                    taper  = 1.0)    # taper of chamber ranging from 0 for ... to 1 for ...
  
    # Add treatment
    #--------------------------------------------------------------------------------------
    if (study == 'Exp2018') {
      treatment <- rep (1, length (treeIDs))
      treatment [treeIDs <= 10] <- 4
      treatment [treeIDs <= 5]  <- 5   
    } else if (study == 'Obs2018' | study == 'Obs2019') {
      treatment <- rep (1, length (treeIDs))
    } else if (study == 'Exp2019') {
      treatment <- rep (1, length (treeIDs))
      treatment [treeIDs %in% c (2, 4, 6, 7)] <- 5
    } else if (study == 'Exp2017') {
      if (as.POSIXct (dateTime, format = '%Y%m%d_%H%M') > as.POSIXct ('2018-04-06')) {
        treatment <- as.numeric (substr (listDir, 14, 14))
      } else {
        treatment <- as.numeric (substr (listDir, 4, 4))
      }
    }
    
    # Determine the species of the trees
    #--------------------------------------------------------------------------------------
    if (substr (study, 1, 3) == 'Obs') {
      species <- substr (listDir, 11, 11)
      species [species == 'A'] <- 'Acer rubrum'
      species [species == 'P'] <- 'Pinus strobus'
      species [species == 'Q'] <- 'Quercus rubra'
    } else if (study == 'Exp2017' | study == 'Exp2018') {
      species <- 'Pinus strobus'
    } else if (study == 'Exp2019') {
      species <- 'Acer rubrum'
    }
    
    # decide on timestamp
    #--------------------------------------------------------------------------------------
    tmp <- if (unique (timestamp > as.POSIXct ('2018-04-06'))) timestamp else NA
    
    # Put all info together into a tibble
    #--------------------------------------------------------------------------------------
    sessionData <- tibble (file          = listDir,
                           study         = study,
                           treatment     = treatment,
                           tree          = treeIDs,
                           species       = species,
                           chamber       = chamberIDs,
                           chamberVolume = as.numeric (chamberGeometry [1:(length (chamberGeometry) - 1)]),
                           chamberArea   = chamberGeometry [[length (chamberGeometry)]],
                           timestamp     = as.POSIXct (tmp), 
                           session       = dateTime,
                           fluxRaw       = NA,
                           sdFluxRaw     = NA,
                           AICRaw        = NA,
                           r2Raw         = NA,
                           fluxAtm       = NA,
                           sdFluxAtm     = NA,
                           AICAtm        = NA,
                           r2Atm         = NA,
                           fluxInt       = NA,
                           sdFluxInt     = NA,
                           AICInt        = NA,
                           r2Int         = NA,
                           ea.Pa         = NA, 
                           airt.C        = NA, 
                           soilt1.C      = NA, # soil temperature at 
                           soilt2.C      = NA, 
                           soilt3.C      = NA, 
                           soilt4.C      = NA, 
                           pres.Pa       = NA, 
                           H2O.ppt.atm   = NA, # atmospheric water vapour pressure from the Fisher Met Station
                           H2O.ppt.int   = NA, # internal water vapour pressure from LiCor-840.
                           vwcDaily      = NA, # volumetic soil water content form the barn tower
                           vwc1          = NA,
                           vwc2          = NA,
                           vwc3          = NA,
                           vwc4          = NA,
                           totalRad      = NA,
                           diffuseRad    = NA) 
    
    # loop through each measurement in the session 
    #--------------------------------------------------------------------------------------
    for (ifile in  1:nrow (sessionData)) {
      
      # assign each of the files to a general variable "currentfile"
      #------------------------------------------------------------------------------------
      if (machine == 'timNAU'){
        currentFile <- sprintf ('%sraw/%s/%s/%s', dirPath, study, dateTime,
                                sessionData [['file']] [ifile])
      } else if (machine == 'timPersonal') {
        currentFile <- sprintf ('%s%s/%s/%s', dirPath, study, dateTime,
                                sessionData [['file']] [ifile])
      }
      
      # read in the data file
      #------------------------------------------------------------------------------------
      if (as.POSIXct (dateTime, format = '%Y%m%d_%H%M') > as.POSIXct ('2018-04-06')) {
        measurement <- read_csv (file = currentFile, col_types = cols ())
      } else {
        measurement <- read_delim (file = currentFile, delim = ' ', col_types = cols (), 
                                   skip = 2, col_names = FALSE)
        measurement <- rename (measurement, time = 1, CO2 = 2, cellTemp = 3, cellPres = 4)
        # create Seconds column
        measurement [['Seconds']] <- as.numeric (measurement [['time']] - 
                                                 measurement [['time']] [1])
      }
      
      # determine name of the time column depending on flux puppy version (age of the file) 
      #------------------------------------------------------------------------------------
      if (as.POSIXct (dateTime, format = '%Y%m%d_%H%M') < as.POSIXct ('2018-06-07')) {
        colTime <- 'Seconds'
      } else {
        colTime <- 'RunTime'
      }
      
      # select only columns of interest
      #------------------------------------------------------------------------------------
      if ('H2O' %in% colnames (measurement)) {
        measurement <- measurement %>% select (colTime, CO2, H2O) %>% rename (H2OInt = H2O) 
      } else {
        measurement <- select (measurement, colTime, CO2)
      }    

      # get lower boundary to adjust hte timestamp
      #------------------------------------------------------------------------------------
      condition <- boundaries [['treeID']]    == sessionData [['tree']]    [ifile] &
                   boundaries [['chamberID']] == sessionData [['chamber']] [ifile] &
                   boundaries [['lower']]     == TRUE
      lowerBoundary <- boundaries [['boundary']] [condition]
      
      # get the actual timestamp from the file, if the file was produced with LiCor software,
      # and add it to the sessionData
      #------------------------------------------------------------------------------------
      if (as.POSIXct (dateTime, format = '%Y%m%d_%H%M') < as.POSIXct ('2018-04-06')) {
        temp <- as.POSIXct (substr (readLines (currentFile, n = 1), 2, 20),
                            format = '%Y-%m-%d at %H:%M', tz = 'EST')
        sessionData [['timestamp']] [ifile] <- with_tz (as_datetime (temp + 
                                                                     lowerBoundary), 
                                                        tz = 'EST')
        # sessionData <- sessionData %>% 
        #                hablar::convert (dtm (timestamp)) %>%
        #                with_tz (tz = 'EST')
      } else {
        sessionData [['timestamp']] [ifile] <- sessionData [['timestamp']] [ifile] + lowerBoundary
      }
      
      # truncate data to select only reasonable values
      #----------------------------------------------------------------------------------
      PLOT1 <- FALSE
      condition <- boundaries [['treeID']] == sessionData [['tree']] [ifile] &
                   boundaries [['chamberID']] == sessionData [['chamber']] [ifile]  
      dat <- selectData (ds = measurement,
                         lowerBound = boundaries [['boundary']] [condition & boundaries [['lower']] == TRUE],
                         upperBound = boundaries [['boundary']] [condition & boundaries [['lower']] == FALSE],
                         plotB = PLOT1, 
                         colTime = colTime)
      # add title to plot
      if (PLOT1) title (main = paste ('Stem Respiration:', 'tree', sessionData [['tree']] [ifile], 
                                      'chamber', sessionData [['chamber']] [ifile], 
                                      as_datetime (sessionData [['timestamp']] [ifile], tz = 'EST')))
      
      # find closest 15 and 10 minute interval
      #------------------------------------------------------------------------------------
      next15_interval <- as.POSIXct (x = (round (as.numeric (median (sessionData [['timestamp']] [ifile]))/
                                         (15 * 60)) * (15 * 60)), 
                                     format = '%Y-%m-%d %H:%M:%S',
                                     origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', 
                                                          tz = 'UTC'), 
                                     tz = 'EST')
      next10_interval <- as.POSIXct (x = (round (as.numeric (median (sessionData [['timestamp']] [ifile]))/
                                          (10 * 60)) * (10 * 60)), 
                                     format = '%Y-%m-%d %H:%M:%S',
                                     origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', 
                                                          tz = 'UTC'), 
                                     tz = 'EST')
      
      # extract pressure in Pa air temperature in deg C and relative humidity in percent 
      # from meteorological data
      #------------------------------------------------------------------------------------
      pres.Pa <- met_HF [['bar']]  [met_HF [['TIMESTAMP']] == next15_interval & !is.na (met_HF [['TIMESTAMP']])] * 100.0 # Pa
      airt.C  <- met_HF [['airt']] [met_HF [['TIMESTAMP']] == next15_interval & !is.na (met_HF [['TIMESTAMP']])]         # deg C
      rh.per  <- met_HF [['rh']]   [met_HF [['TIMESTAMP']] == next15_interval & !is.na (met_HF [['TIMESTAMP']])]         # %
      
      # calculate saturation water vapour pressure (esat) to convert relative humidity
      #------------------------------------------------------------------------------------
      es.Pa <- 0.61078 * exp ((17.269 * airt.C) / (237.3 + airt.C)) * 1000 # saturated water pressure [Pa]
      ea.Pa <- es.Pa * rh.per / 100.0                                         # get actual water vapour pressure [Pa]
      dat [['ea.Pa']]   <- rep (ea.Pa,   dim (dat [, 1]) [1])   # add actual water vapour pressure [Pa] to sessiondata data.frame
      dat [['airt.C']]  <- rep (airt.C,  dim (dat [, 1]) [1])   # add air temperature [degC] to sessiondata data.frame
      dat [['pres.Pa']] <- rep (pres.Pa, dim (dat [, 1]) [1])   # add atmospheric pressure [Pa] to aalldat data.frame
      dat [['H2O.ppt']] <- dat [['ea.Pa']] / (dat [['pres.Pa']] - dat [['ea.Pa']]) * 1.0e3   # add actual water vapour pressure [ppt] to sessiondata data.frame
      
      # rename CO2 column
      #------------------------------------------------------------------------------------
      names (dat) [which (names (dat) == "CO2")] <- "CO2.ppm"
      
      # correct CO2 concentration for atmospheric water vapour concentration 
      #------------------------------------------------------------------------------------
      dat [['CO2.dry.atm']] <- corrConcDilution (dat, 
                                                 colConc   = 'CO2.ppm',
                                                 colVapour = 'H2O.ppt')
      # correct CO2 concentration for internal water vapour concentration of the LiCor-840
      #------------------------------------------------------------------------------------
      if ('H2OInt' %in% colnames (dat)) {
        dat [['CO2.dry.int']] <- corrConcDilution (dat, 
                                                   colConc   = 'CO2.ppm',
                                                   colVapour = 'H2OInt')

      }
      
      
      # determine volume and area of the chamber
      #------------------------------------------------------------------------------------
      chamberVolume <- sessionData [['chamberVolume']] [ifile]
      chamberArea   <- sessionData [['chamberArea']]   [ifile]
      
      # calculate chamber flux for entire timeseries from corrected 
      #------------------------------------------------------------------------------------
      suppressWarnings (resFitRaw <- calcClosedChamberFlux (dat,
                                                            colConc     = 'CO2.ppm',
                                                            colTime     = colTime, 
                                                            colTemp     = 'airt.C',
                                                            colPressure = 'pres.Pa',
                                                            volume      = chamberVolume,
                                                            area        = chamberArea))
      
      # Calculate chamber flux for entire timeseries from atmospherically corrected concentration 
      #------------------------------------------------------------------------------------
      suppressWarnings (resFitAtm <- calcClosedChamberFlux (dat,
                                                            colConc     = 'CO2.dry.atm',
                                                            colTime     = colTime, 
                                                            colTemp     = 'airt.C',
                                                            colPressure = 'pres.Pa',
                                                            volume      = chamberVolume,
                                                            area        = chamberArea))
      
      # Calculate chamber flux for entire timeseries from internally corrected concentration
      #------------------------------------------------------------------------------------
      if ('H2OInt' %in% colnames (dat)) {
        suppressWarnings (resFitInt <- calcClosedChamberFlux (dat,
                                                              colConc     = 'CO2.dry.int',
                                                              colTime     = colTime, 
                                                              colTemp     = 'airt.C',
                                                              colPressure = 'pres.Pa',
                                                              volume      = chamberVolume,
                                                              area        = chamberArea))
      } else {
        resFitInt <- tibble (flux = NA, sdFlux = NA, AIC = NA, r2 = NA)
      }
      
      # put all the data into the table 'sessiondata" you created earlier
      #------------------------------------------------------------------------------------
      sessionData [['fluxRaw']]     [ifile] <- resFitRaw [['flux']] # flux is in micromol / s
      sessionData [['sdFluxRaw']]   [ifile] <- resFitRaw [['sdFlux']]
      sessionData [['AICRaw']]      [ifile] <- resFitRaw [['AIC']]
      sessionData [['r2Raw']]       [ifile] <- resFitRaw [['r2']]
      sessionData [['fluxAtm']]     [ifile] <- resFitAtm [['flux']] # flux is in micromol / s
      sessionData [['sdFluxAtm']]   [ifile] <- resFitAtm [['sdFlux']]
      sessionData [['AICAtm']]      [ifile] <- resFitAtm [['AIC']]
      sessionData [['r2Atm']]       [ifile] <- resFitAtm [['r2']]
      sessionData [['fluxInt']]     [ifile] <- resFitInt [['flux']] # flux is in micromol / s
      sessionData [['sdFluxInt']]   [ifile] <- resFitInt [['sdFlux']]
      sessionData [['AICInt']]      [ifile] <- resFitInt [['AIC']]
      sessionData [['r2Int']]       [ifile] <- resFitInt [['r2']]
      sessionData [['ea.Pa']]       [ifile] <- ea.Pa   # add actual water vapour pressure [Pa] to alldata data.frame
      sessionData [['airt.C']]      [ifile] <- airt.C   # add air temperature [degC] to alldata data.frame
      sessionData [['pres.Pa']]     [ifile] <- pres.Pa   # add atmospheric pressure [Pa] to aalldat data.frame
      sessionData [['H2O.ppt.atm']] [ifile] <- ea.Pa / (pres.Pa - ea.Pa) * 1.0e3   
      sessionData [['vwcDaily']]    [ifile] <- 
        soilMoisture_HF [['vwc']] [date (soilMoisture_HF [['day']]) == date (next10_interval)]
      if (next10_interval > min (soilMoistureBarn [['TIMESTAMP']])) {
        sessionData [['vwc1']] [ifile] <- 
          soilMoistureBarn [['VWC_1']] [soilMoistureBarn [['TIMESTAMP']] == next10_interval]
        sessionData [['vwc2']] [ifile] <- 
          soilMoistureBarn [['VWC_2']] [soilMoistureBarn [['TIMESTAMP']] == next10_interval]
        sessionData [['vwc3']] [ifile] <- 
          soilMoistureBarn [['VWC_3']] [soilMoistureBarn [['TIMESTAMP']] == next10_interval]
        sessionData [['vwc4']] [ifile] <- 
          soilMoistureBarn [['VWC_4']] [soilMoistureBarn [['TIMESTAMP']] == next10_interval]
        sessionData [['soilt1']] [ifile] <- 
          soilMoistureBarn [['Soil_Temp_C_1']] [soilMoistureBarn [['TIMESTAMP']] == next10_interval]
        sessionData [['soilt2']] [ifile] <- 
          soilMoistureBarn [['Soil_Temp_C_2']] [soilMoistureBarn [['TIMESTAMP']] == next10_interval]
        sessionData [['soilt3']] [ifile] <- 
          soilMoistureBarn [['Soil_Temp_C_3']] [soilMoistureBarn [['TIMESTAMP']] == next10_interval]
        sessionData [['soilt4']] [ifile] <- 
          soilMoistureBarn [['Soil_Temp_C_4']] [soilMoistureBarn [['TIMESTAMP']] == next10_interval]
        sessionData [['totalRad']] [ifile] <- 
          soilMoistureBarn [['Total_Rad_Avg']] [soilMoistureBarn [['TIMESTAMP']] == next10_interval]
        sessionData [['diffuseRad']] [ifile] <- 
          soilMoistureBarn [['Diffuse_Rad_Avg']] [soilMoistureBarn [['TIMESTAMP']] == next10_interval]
      } 
      
      # Plot data, if so desired
      PLOT2 <- TRUE
      if (PLOT2) {
        png (paste0 ('./selectDataGraphs/selectedData_',study,'_',dateTime,'_',sessionData [['tree']] [ifile],'_',
                     sessionData [['chamber']] [ifile],'.png'))
        par (mfrow = c (1, 1))
        plot (x = dat [[colTime]], 
              y = dat [['CO2.ppm']],
              xlab = 'time [s]',
              ylab = 'CO2 concentration [ppm]')
        title (main = paste ('Stem Respiration:', 'tree', sessionData [['tree']] [ifile], 'chamber',
                             sessionData [['chamber']] [ifile], 
                             as_datetime (sessionData [['timestamp']] [ifile])))
          
        # plot selected data bounds
        points (x = dat [[colTime]],
                y = dat [['CO2.ppm']],
                col  = '#91b9a499',
                pch  = 19,
                cex  = 0.9)
          
        # add a line for the calculated slope
        #------------------------------------------------------------------------------------
        abline (lm (dat [['CO2.ppm']] ~ dat [[colTime]]),
                lwd = 4,
                col = '#91b9a499')
        dev.off ()
      } # end plot condition
    } # end ifile loop
    
    # save the respiration session data for this date time
    #--------------------------------------------------------------------------------------
    if (machine == 'timNAU') {
      saveRDS (sessionData, file = paste0 (dirPath, 'processed/',study,'/',dateTime,
                                           '_sessionData.rds'))
    } else if (machine == 'timPersonal') {
      saveRDS (sessionData, file = paste0 (dirPath,study,'/',dateTime,
                                           '_sessionData.rds'))
    }
  } # end measurement dates loop
} # end study loop
#========================================================================================