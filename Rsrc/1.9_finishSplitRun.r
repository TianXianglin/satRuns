#### Finish processing split run for 1.9_optST_parallel. Split parts are combined here
#### to form stProbMod and dataSurV datasets for the whole tile and these files are used 
#### in processing through rest of the script (matching lines: 82 - 105 in 1.9_optST_parallel)

# Run settings (if modifiedSettings is not set to TRUE in batch job script, default settings from Github will be used)
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/settings.r")
if(modifiedSettings) {
  source("/scratch/project_2000994/PREBASruns/assessCarbon/Rsrc/mainSettings.r") # in CSC
}

# Run functions 
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/functions.r")


###set working directory
setwd(generalPath)

if (splitRun) {
  # Load datasets (are all of these datasets needed here?)
  #load("C:/Users/minunno/GitHub/satRuns/data/inputUncer.rdata") # in local fm
  load("/scratch/project_2000994/PREBASruns/assessCarbon/data/inputUncer.rdata") # in CSC
  load("surErrMods/logisticPureF.rdata")
  load("surErrMods/stProbit.rdata")
  load("surErrMods/surMod.rdata")
  
  # Create empty data tables to where the combined data is added
  stProbMod_bind <- data.table()
  dataSurV_bind <- data.table()
  
  # Iterate through stProbMod and dataSurV data tables of the split parts (1-4). 
  # Bind split parts to a single data table 
  for (i in splitRange) {
    stProbMod_fileX <- load(paste0("procData/init",startingYear,"/calST_split/stProbMod",i,".rdata"))
    dataSurV_fileX <- load(paste0("procData/init",startingYear,"/calST_split/dataSurV",i,".rdata"))
    stProbMod_split <- get(stProbMod_fileX)
    rm(stProbMod_fileX)
    dataSurV_split <- get(dataSurV_fileX)
    rm(dataSurV_fileX)
    
    stProbMod_bind <- rbindlist(list(stProbMod_bind, stProbMod_split))
    dataSurV_bind <- rbindlist(list(dataSurV_bind, dataSurV_split))
    
    rm(stProbMod_split, dataSurV_split)
  }
  
  # Save combined datasets, if wanted. NO NEED for saving for further processing
  #save(stProbMod_bind, file = paste0(procDataPath,"init",startingYear,"/calST_split/stProbMod.rdata"))
  #save(dataSurV_bind, file = paste0(procDataPath,"init",startingYear,"/calST_split/dataSurV.rdata"))
  
  print("Split data combined. stProbMod and dataSurV data tables for the whole tile ready for
      further processing.")
  
  #### Finish processing 1.9 with the combined data. This block matches lines: 82 - 105 in 
  #    1.9_optST_parallel.
  
  nSeg <- nrow(dataSurV_bind)
  
  ###calculate probit2016
  dataSurV_bind[,st:=st1]
  probit1 <- predict(step.probit,type='p',dataSurV_bind[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data
  
  ###calculate probit2019
  dataSurV_bind[,st:=st2]
  probit2 <- predict(step.probit,type='p',dataSurV_bind[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data
  
  print("probit 1 and probit 2 created from binded data")
  
  stProb <- array(NA, dim=c(nSeg,5,3))
  stProb[,,1] <- probit1
  stProb[,,2] <- probit2
  stProb[,,3] <- as.matrix(stProbMod_bind)
  
  stProb <- apply(stProb, c(1,2), mean)
  
  print("stProb created from binded data")
  
  save(stProb,probit1,probit2,stProbMod_bind,file="stProbMod.rdata")
  
  print("final output from binded data saved")
  
}
