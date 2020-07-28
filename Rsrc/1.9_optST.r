library(MASS)
### Run settings & functions
source("Rsrc/settings.r")
source("Rsrc/functions.r")

###check and create output directories
setwd(generalPath)

yearX <- 3
nSample = 1000 ###number of samples from the error distribution
load(paste0("procData/init",startingYear,"/calST/uniqueData.rdata"))  
load("C:/Users/minunno/GitHub/satRuns/data/inputUncer.rdata")
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



nSeg <- 1000#nrow(dataSurV)  ##200
stProbMod <- matrix(NA,nSeg,5)

for(i in 1:nSeg){
  stProbMod[i,] <- pSTx(dataSurV[i],nSample)
  # if (i %% 100 == 0) { print(i) }
}

stProbMod <- data.table(stProbMod)

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
