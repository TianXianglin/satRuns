### Run settings & functions
	source("Rsrc/settings.r")	
#source(Rsrc/mainSettings.r) ### in CSC	
source("Rsrc/functions.r")

###check and create output directories
setwd(generalPath)

yearX <- 3
	nSample = 1000 ### number of samples from the error distribution. 1000 for proper runs. 	
                 # While testing, use of a smaller amount is recommended (for example 100).	

# Load unique data.	
# If data is processed in split parts, unique data with split_id set in mainSettings is loaded.	
# If splitRun is not needed, the unique data dataset for the whole tile is loaded.	
if (splitRun) {	
  # This code doesn't remove the excess uniqueData1 data table after renaming it. Code that	
  # removes it should be added here	
  uniqueData_file <- load(paste0("procData/init",startingYear,"/calST_split/uniqueData", split_id, ".rdata"))	
  uniqueData <- get(uniqueData_file)	
  rm(uniqueData_file)	
  } else{	
  load(paste0("procData/init",startingYear,"/calST/uniqueData.rdata")) 	
  }	

# Load other needed datasets	
#load("C:/Users/minunno/GitHub/satRuns/data/inputUncer.rdata") # in local fm
load("/scratch/project_2000994/PREBASruns/assessCarbon/data/inputUncer.rdata") # in CSC
load("surErrMods/logisticPureF.rdata")
load("surErrMods/stProbit.rdata")
load("surErrMods/surMod.rdata")


uniqueData[,BAp:= (ba * pineP/(pineP+spruceP+blp))]
uniqueData[,BAsp:= (ba * spruceP/(pineP+spruceP+blp))]
uniqueData[,BAb:= (ba * blp/(pineP+spruceP+blp))]

dataSurV <- uniqueData[,.(h,dbh,BAp,BAsp,BAb,siteType1,siteType2,v2,segID)] 
setnames(dataSurV,c("H","D","BAp","BAsp","BAb","st1","st2","V2","segID"))


dataSurV[,BApPer:=.(BAp/sum(BAp,BAsp,BAb)*100),by=segID]
dataSurV[,BAspPer:=.(BAsp/sum(BAp,BAsp,BAb)*100),by=segID]
dataSurV[,BAbPer:=.(BAb/sum(BAp,BAsp,BAb)*100),by=segID]
dataSurV[,BAtot:=.(sum(BAp,BAsp,BAb)),by=segID]

print("dataSurV ready for pSTx function")

nSeg <- nrow(dataSurV)  ##200
stProbMod <- matrix(NA,nSeg,5)


# Processing time is measured with tictoc function. Returns the amount of seconds taken to 	
# process the block of code between tic() and toc().	
tic("total time taken to run the pSTx function")

# Run surrogate model in parallel. Number of cores used for processing is defined with 	
# mc.cores argument. mc.cores=1 disables parallel processing. For now the result is 	
# a matrix in which all 5 site probability values are in one column. This could be modified 	
# later on to produce a matrix where the 5 values are places in separate columns right away. 	
# That modification would decrease the need of reorganizing the data afterwards (in 	
# lines 73-82).
stProbMod <- matrix(mclapply(1:nrow(stProbMod), function(i,stProbMod){
 stProbMod[i,] <- pSTx(dataSurV[i],nSample)
},stProbMod=stProbMod,mc.cores = coresN))
toc()


#Clean and convert the matrix to data table
stProbMod <- as.data.table(stProbMod)
setDT(stProbMod)[, paste0("V", 1:5) := tstrsplit(V1, ", ")]
stProbMod$V1 <- gsub("^.{0,2}", "", stProbMod$V1)
stProbMod$V5 <- gsub(")", "", stProbMod$V5)
stProbMod <- stProbMod[, V1:=as.numeric(V1)]
stProbMod <- stProbMod[, V2:=as.numeric(V2)]
stProbMod <- stProbMod[, V3:=as.numeric(V3)]
stProbMod <- stProbMod[, V4:=as.numeric(V4)]
stProbMod <- stProbMod[, V5:=as.numeric(V5)]


if (splitRun) {	
  # If executing a split run, the output is saved at this point.	
  # Continue by processing the other parts of split data or by running 1.9_finishSplitRun 	
  # if all parts are processed this far.	
  save(stProbMod, file = paste0(procDataPath,"init",startingYear,"/calST_split/stProbMod",split_id,".rdata"))	
  save(dataSurV, file = paste0(procDataPath,"init",startingYear,"/calST_split/dataSurV",split_id,".rdata"))	
  print("stPRobMod and dataSurV of this split_id saved to calST_split folder")	
  print("Continue by sending remaining parts of split data to processing or by combining 	
        processed parts and finishing processing of 1.9 with 1.9_finishSplitRun.r")	
  	
} else {	
  # If the whole unique data is processed at once, the script is carried out all the way 	
  # here and final output of 1.9 saved.	
  	
  ###calculate probit2016	
  dataSurV[,st:=st1]	
  probit1 <- predict(step.probit,type='p',dataSurV[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data	
  	
  ###calculate probit2019	
  dataSurV[,st:=st2]	
  probit2 <- predict(step.probit,type='p',dataSurV[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data	
  	
  print("probit 1 and probit 2 created from original non-split data")	
  	
  stProb <- array(NA, dim=c(nSeg,5,3))	
  stProb[,,1] <- probit1	
  stProb[,,2] <- probit2	
  stProb[,,3] <- as.matrix(stProbMod)	
  	
  stProb <- apply(stProb, c(1,2), mean)	
  	
  print("stProb created from original non-split data")	
  	
  save(stProb,probit1,probit2,stProbMod,file="stProbMod.rdata")	
  	
  print("final output from original non-split data saved")	
}
