#### Finish processing split run for 1.8_DA_ST. Split parts are combined here
#### to form stProbMod and dataSurMod datasets for the whole tile and these files are used 
#### in processing through rest of the script.

# Run settings 
library(devtools)
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/settings.r")
if(file.exists("localSettings.r")) {source("localSettings.r")} # use settings file from local directory if one exists


# Run functions 
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/functions.r")


###set working directory
setwd(generalPath)


if(splitRun){ 
  
  # stProbMod and dataSurMod of all of the split parts are combined before continuing processing
  
  # Create empty data tables to where the combined data is added
  stProbMod_bind <- data.table()
  dataSurMod_bind <- data.table()
  
  # Iterate through stProbMod and dataSurMod data tables of the split parts. 
  # Bind split parts to a single data table 
  for (i in splitRange) {
    stProbMod_fileX <- load(paste0("procData/init",startingYear,"/calST_split/stProbMod",i,".rdata"))
    dataSurMod_fileX <- load(paste0("procData/init",startingYear,"/calST_split/dataSurMod",i,".rdata"))
    stProbMod_split <- get(stProbMod_fileX)
    rm(list = stProbMod_fileX)
    rm(stProbMod_fileX)
    dataSurMod_split <- get(dataSurMod_fileX)
    rm(list = dataSurMod_fileX)
    rm(dataSurMod_fileX)
    
    stProbMod_bind <- rbindlist(list(stProbMod_bind, stProbMod_split))
    dataSurMod_bind <- rbindlist(list(dataSurMod_bind, dataSurMod_split))
    
    rm(stProbMod_split, dataSurMod_split)
  }
  
  stProbMod <- stProbMod_bind
  dataSurMod <- dataSurMod_bind
  
  nSeg <- nrow(dataSurMod) # define nSeg after length of dataSurMod has changed
  
  ###calculate probit2016
  dataSurMod[,st:=st1]
  step.probit1 <- step.probit[[paste0("y",startingYear)]][[paste0("t",tileX)]]
  probit1 <- predict(step.probit1,type='p',dataSurMod[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data
  
  ###calculate probit2019
  dataSurMod[,st:=st2]
  step.probit2 <- step.probit[[paste0("y",year2)]][[paste0("t",tileX)]]
  probit2 <- predict(step.probit2,type='p',dataSurMod[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data
  
  stProb <- array(NA, dim=c(nSeg,5,3))
  stProb[,,1] <- probit1
  stProb[,,2] <- probit2
  stProb[,,3] <- as.matrix(stProbMod[,2:6])
  
  stProb <- apply(stProb, c(1,2), mean)
  stProb <- cbind(dataSurMod$segID,stProb)
  colnames(stProb) <- colnames(stProbMod)
  save(stProb,probit1,probit2,stProbMod,file="stProbMod.rdata")
}
