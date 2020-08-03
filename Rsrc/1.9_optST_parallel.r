#library(MASS)
### Run settings & functions
source("/Settings.r")
source("/functions.r")

###check and create output directories
setwd(generalPath)

yearX <- 3
nSample = 100 ###number of samples from the error distribution. 
load(paste0("procData/init",startingYear,"/calST/uniqueData.rdata")) 
#load("C:/Users/minunno/GitHub/satRuns/data/inputUncer.rdata") # local fm
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


nSeg <- nrow(dataSurV)  ##200
stProbMod <- matrix(NA,nSeg,5)


#Processing time is measured with tictoc
tic("total time taken to run pSTx function")

###Run surrogate model in parallel with mclapply function. Number of cores (coresN) is defined 
#  in settings file. mc.cores=1 disables parallel processing. 
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


 ###calculate probit2016
dataSurV[,st:=st1]
probit1 <- predict(step.probit,type='p',dataSurV[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data

 ###calculate probit2019
dataSurV[,st:=st2]
probit2 <- predict(step.probit,type='p',dataSurV[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data
 
stProb <- array(NA, dim=c(nSeg,5,3))
stProb[,,1] <- probit1
stProb[,,2] <- probit2
stProb[,,3] <- as.matrix(stProbMod)

stProb <- apply(stProb, c(1,2), mean)

save(stProb,probit1,probit2,stProbMod,file="stProbMod.rdata")
