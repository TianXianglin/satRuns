#### Finish processing split run for 1.9_optST_parallel. Split parts are combined here
#### to form stProbMod and dataSurMod datasets for the whole tile and these files are used 
#### in processing through rest of the script (matching lines: 82 - 105 in 1.9_optST_parallel)

library(devtools)
# Run settings (if modifiedSettings is not set to TRUE in batch job script, default settings from Github will be used)
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/settings.r")
if(file.exists("localSettings.r")) {source("localSettings.r")} # use settings file in local directory if one exists

# Run functions 
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/functions.r")


###set working directory
setwd(generalPath)

if (splitRun) {
  ####load error models
  load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/inputUncer.rdata"))
  load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/logisticPureF.rdata"))
  load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/step.probit.rdata"))
  ###load surrMods
  load("surErrMods/surMod.rdata")
  
  # Create empty data tables to where the combined data is added
  stProbMod_bind <- data.table()
  dataSurMod_bind <- data.table()
  
  # Iterate through stProbMod and dataSurMod data tables of the split parts (1-4). 
  # Bind split parts to a single data table 
  for (i in splitRange) {
    stProbMod_fileX <- load(paste0("procData/init",startingYear,"/calST_split/stProbMod",i,".rdata"))
    dataSurMod_fileX <- load(paste0("procData/init",startingYear,"/calST_split/dataSurMod",i,".rdata"))
    stProbMod_split <- get(stProbMod_fileX)
    rm(stProbMod_fileX)
    dataSurMod_split <- get(dataSurMod_fileX)
    rm(dataSurMod_fileX)
    
    stProbMod_bind <- rbindlist(list(stProbMod_bind, stProbMod_split))
    dataSurMod_bind <- rbindlist(list(dataSurMod_bind, dataSurMod_split))
    
    rm(stProbMod_split, dataSurMod_split)
  }
  
  # Save combined datasets, if wanted. NO NEED for saving for further processing
  #save(dataSurMod_bind, file = paste0(procDataPath,"init",startingYear,"/calST_split/dataSurMod.rdata"))
  
  print("Split data combined. stProbMod and dataSurMod data tables for the whole tile ready for
      further processing.")
  
  #### Finish processing 1.9 with the combined data. This block matches lines: 82 - 105 in 
  #    1.9_optST_parallel.
  
  nSeg <- nrow(dataSurMod_bind)
  
  ###calculate probit2016
  dataSurMod_bind[,st:=st1]
  step.probit1 <- step.probit[[paste0("y",startingYear)]][[paste0("t",tileX)]]
  probit1 <- predict(step.probit1,type='p',dataSurMod_bind[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data

  ###calculate probit2019
  dataSurMod_bind[,st:=st2]
  step.probit2 <- step.probit[[paste0("y",year2)]][[paste0("t",tileX)]]
  probit2 <- predict(step.probit2,type='p',dataSurMod_bind[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data

  stProb <- array(NA, dim=c(nSeg,5,3))
  stProb[,,1] <- probit1
  stProb[,,2] <- probit2
  stProb[,,3] <- as.matrix(stProbMod_bind)

  stProb <- apply(stProb, c(1,2), mean)

  save(stProb,probit1,probit2,stProbMod_bind,file="stProbMod.rdata")
  
}
