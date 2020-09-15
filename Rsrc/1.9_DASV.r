library(MASS)
# Run settings (if modifiedSettings is not set to TRUE in batch job script, default settings from Github will be used)
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/settings.r")
if(modifiedSettings) {
  source("/scratch/project_2000994/PREBASruns/assessCarbon/Rsrc/mainSettings.r") # in CSC
}
# Run functions 
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/functions.r")

###check and create output directories
setwd(generalPath)

yearX <- 3
nSample = 1000 ###number of samples from the error distribution
load(paste0("procData/init",startingYear,"/calST/uniqueData.rdata"))  
#load("C:/Users/minunno/GitHub/satRuns/data/inputUncer.rdata")
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



nSeg <-1000# nrow(dataSurV)  ##200
load("stProbMod.rdata")
colnames(stProb) <- paste0("pST",1:5)
dataSurMod <- cbind(dataSurV[1:nSeg],stProb[1:nSeg,])

# pMvNorm <- matrix(NA,42,nSeg)
# system.time({
#   for(i in 1:nSeg){
#     pMvNorm[,i] <- pSVDA(dataSurMod[i],nSample)
#     if (i %% 100 == 0) { print(i) }
#   }
# })

system.time({
 pMvn <- dataSurMod[1:nSeg, pSVDA(.SD,nSample = nSeg), by = seq_len(nSeg)]
})
save(pMvn,file="pMvn.rdata")
