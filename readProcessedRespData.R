# Read all processed respiration data
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')

# define the work station
#----------------------------------------------------------------------------------------
machine <- 'timNAU'

# define data directory
#----------------------------------------------------------------------------------------
if (machine == 'timNAU') {
  dataDir <- '/media/tim/dataDisk/PlantGrowth/data/respiration/processed/'
} else if (machine == 'timPersonal') {
  dataDir <- '../data/'
}

# Create list of all the .rds file with data for each session
#----------------------------------------------------------------------------------------
for (study in  c ('Exp2017', 'Exp2018', 'Exp2019', 'Obs2018', 'Obs2019', 'SoilResp2018')) {
  tmp <- list.files (path = paste0 (dataDir,study,'/'), pattern = 'sessionData.rds', recursive = TRUE)
  fileList <- tibble (study =  study, fileName =  tmp); rm (tmp)
  
  # Loop over each .rds file with processed data
  #----------------------------------------------------------------------------------------
  for (i in 1:dim (fileList) [1]) {
    # Read the ith file
    tmp <- readRDS (paste0 (dataDir,'/',fileList [['study']] [i],'/',fileList [['fileName']] [i]))
    if (i == 1 & study == 'Exp2017') {
      respData <- tmp
    } else {
      # Append the session file to the end of the respData frame
      respData <- rbind (respData, tmp)
    }
  }
  
  # plot histogram
  #----------------------------------------------------------------------------------------
  hist (respData [['fluxRaw']])
  
}

# find outliers
#----------------------------------------------------------------------------------------
respData [which (respData [['fluxRaw']] < 0), ]

# write a csv file with all values
#----------------------------------------------------------------------------------------
WRITE <- FALSE
if (WRITE) {
  write_csv (respData, path = '../data/respiration/processed/respDataHF.csv')
}