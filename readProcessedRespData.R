# Read all processed respiration data
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')

# Define data directory
#----------------------------------------------------------------------------------------
dataDir <- '/media/tim/dataDisk/PlantGrowth/data/respiration/processed'

# Start with processed data from 2017 experiment, which was not using FluxPuppy
#----------------------------------------------------------------------------------------
respData <- read_csv (file = paste0 (dataDir,'/Exp2017/resp_compression_2017_11_01.csv'), 
                      col_types = cols ())

# Wrangle data into same format as the .rds files
#----------------------------------------------------------------------------------------
respData <- tibble (file      = NA,
                    treatment = NA, 
                    tree      = respData [['tree']],
                    species   = 'Pinus strobus',
                    chamber   = respData [['chamber']],
                    timestamp = as.POSIXct (paste0 (respData [['date']],' 15:00'), tz = 'EST'),
                    session   = paste0 (format (respData [['date']], '%Y%m%d'),'_1500'),
                    flux      = respData [['flux']],
                    sdFlux    = respData [['fluxSD']],
                    ea.Pa     = NA,
                    airt.C    = NA,
                    pres.Pa   = NA,
                    H2O.ppt   = NA,
                    AIC       = respData [['AIC']],
                    r2        = respData [['r2']])

#--------------------------------------------------------------------------------------
# Add met data to the pre-Flux Puppy data
#--------------------------------------------------------------------------------------

# Pull appropriate meterological data from the HF website
#--------------------------------------------------------------------------------------
met_HF <- read_csv (file = url ("http://harvardforest.fas.harvard.edu/sites/harvardforest.fas.harvard.edu/files/data/p00/hf001/hf001-10-15min-m.csv","rb"),
                    col_types = cols ())
met_HF$TIMESTAMP <- as.POSIXct (met_HF$datetime, 
                                format = '%Y-%m-%dT%H:%M')
attr (met_HF$TIMESTAMP, "tzone") <- "EST"

# Loop over all rows in the pre-Flux Puppy data
#------------------------------------------------------------------------------------
for (i in 1:dim (respData) [1]) {
  # Find closest 15 minute interval
  #------------------------------------------------------------------------------------
  next_interval <- as.POSIXct (x = (round (as.numeric (median (respData [['timestamp']] [i]))/
                                             (15 * 60)) * (15 * 60) + (15 * 60)), format = '%Y-%m-%d %H:%M:%S',
                               origin = as.POSIXct ("1970-01-01", format = '%Y-%m-%d', tz = 'UTC'), 
                               tz = 'EST')
  # Extract pressure in Pa air temperature in deg C and relative humidity in percent 
  # from meteorological data
  #------------------------------------------------------------------------------------
  respData [['pres.Pa']] [i] <- met_HF [['bar']]  [met_HF [['TIMESTAMP']] == next_interval & 
                                                   !is.na (met_HF [['TIMESTAMP']])] * 100.0 # Pa
  respData [['airt.C']]  [i] <- met_HF [['airt']] [met_HF [['TIMESTAMP']] == next_interval &
                                                   !is.na (met_HF [['TIMESTAMP']])]         # deg C
  rh.per <- met_HF [['rh']]   [met_HF [['TIMESTAMP']] == next_interval & 
                                                   !is.na (met_HF [['TIMESTAMP']])]         # %
  # Calculate saturation water vapour pressure (esat) to convert relative humidity
  #------------------------------------------------------------------------------------
  es.Pa <- 0.61078 * exp ((17.269 * respData [['airt.C']] [i]) / 
                              (237.3 + respData [['airt.C']] [i])) * 1000 # saturated water pressure [Pa]
  respData [['ea.Pa']] [i] <- es.Pa * rh.per / 100.0                                         # get actual water vapour pressure [Pa]
  respData [['H2O.ppt']] [i] <- respData [['ea.Pa']] [i] / 
                                (respData [['pres.Pa']] [i] - respData [['ea.Pa']] [i]) * 1.0e3   # add actual water vapour pressure [ppt] 
}

# Define tree IDs for each group in the 2017 Experiment
#----------------------------------------------------------------------------------------
controlTrees <- c ( 1,  3,  4,  6,  7,  9, 18, 30, 31, 36)
girdledTrees <- c ( 5, 11, 15, 16, 19, 23, 29, 35, 39, 40)
compresTrees <- c (10, 12, 13, 17, 20, 21, 28, 32, 33, 38)
douCompTrees <- c ( 2,  8, 14, 22, 24, 25, 26, 27, 34, 37)

# Add the treatment to the pre-FluxPuppy data
#----------------------------------------------------------------------------------------
respData [['treatment']] [respData [['tree']] %in% controlTrees] <- 1
respData [['treatment']] [respData [['tree']] %in% girdledTrees] <- 2
respData [['treatment']] [respData [['tree']] %in% compresTrees] <- 3
respData [['treatment']] [respData [['tree']] %in% douCompTrees] <- 4

# Create list of all the .rds file with data for each session
#----------------------------------------------------------------------------------------
tmp <- list.files (path = dataDir, pattern = 'sessionData.rds', recursive = TRUE)
fileList <- tibble (study = unlist (strsplit (tmp, '/')) [seq (1, length (tmp), by = 2)],
                    fileName =  unlist (strsplit (tmp, '/')) [seq (2, length (tmp), by = 2)])

# Loop over each .rds file with processed data
#----------------------------------------------------------------------------------------
for (i in 1:dim (fileList) [1]) {
  # Read the ith file
  tmp <- readRDS (paste0 (dataDir,'/',fileList [['study']] [i],'/',fileList [['fileName']] [i]))
  # Append the session file to the end of the respData frame
  respData <- rbind (respData, tmp)
}

# Set small negative values with decent measurement to 0
#----------------------------------------------------------------------------------------
respData [['flux']] [respData [['file']] %in% c ("G-Exp2018_04pxp2_20180727_135344.csv",
                                                 "G-Exp2018_03pxp2_20180806_140110.csv",
                                                 "G-Exp2018_05pxp2_20180817_131839.csv",
                                                 "G-Exp2018_02pxp1_20180824_133717.csv",
                                                 "G-Exp2018_03pxp3_20180824_135305.csv",
                                                 "G-Exp2018_03pxp2_20181004_133713.csv",
                                                 "G-Exp2018_03pxp3_20181004_134337.csv",
                                                 "G-Exp2018_11pxp1_20190423_135013.csv")] <- 0.0
respData [['flux']] [c (1193, 1637)] <- 0.0

# Set bad measurements to NA
#----------------------------------------------------------------------------------------
respData [['flux']] [respData [['file']] %in% c ("G-Exp2018_11pxp2_20180625_080256.csv",
                                                 "G-Exp2018_02pxp3_20180727_133311.csv",
                                                 "G-Exp2018_04pxp3_20180727_135607.csv",
                                                 "G-Exp2018_05pxp2_20180727_141552.csv",
                                                 "G-Exp2018_11pxp2_20180824_142131.csv")] <- NA

# Find outliers
#----------------------------------------------------------------------------------------
boxplot (respData [['flux']])
respData [which (respData [['flux']] < 0), ]

# Plot the respiration rate versus temperature
#----------------------------------------------------------------------------------------
plot (respData [['airt.C']] [respData [['treatment']] == 1 & 
                             respData [['species']] == 'Pinus strobus'],
      respData [['flux']] [respData [['treatment']] == 1 & 
                           respData [['species']] == 'Pinus strobus'],
      ylab = 'stem CO2 efflux', xlab = 'temperature (degC)', xlim = c (0, 30), 
      ylim = c (0, 30), las = 1, pch = 19, col = '')
