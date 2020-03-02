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
respData <- tibble (file = NA,
                    treatment = NA, 
                    tree = respData [['tree']],
                    chamber = respData [['chamber']],
                    timestamp = as.POSIXct (respData [['date']], tz = 'EST'),
                    session = paste0 (format (respData [['date']], '%Y%m%d'),'_1500'),
                    flux = respData [['flux_g']],
                    sdFlux = respData [['fluxSD_g']],
                    ea.Pa = NA,
                    airt.C = NA,
                    pres.Pa = NA,
                    H2O.ppt = NA)

# Create list of all the .rds file with data for each session
#----------------------------------------------------------------------------------------
tmp <- list.files (path = dataDir, pattern = '.rds', recursive = TRUE)
fileList <- tibble (study = unlist (strsplit (tmp, '/')) [seq (1, 269, by = 2)],
                    fileName =  unlist (strsplit (tmp, '/')) [seq (2, 270, by = 2)])
# Loop over each .rds file with processed data
#----------------------------------------------------------------------------------------
for (i in 1:dim (fileList) [1]) {
  # Read the ith file
  tmp <- readRDS (paste0 (dataDir,'/',fileList [['study']] [i],'/',fileList [['fileName']] [i]))

  rbind (respData, tmp)
}