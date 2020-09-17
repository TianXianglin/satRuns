library(devtools)
tileSettings = F
modifiedSettings = F

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

####load error models
load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/inputUncer.rdata"))
load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/logisticPureF.rdata"))
load(url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/data/step.probit.rdata"))
###load surrMods
load("surErrMods/surMod.rdata")


uniqueData[,BAp:= (ba * pineP/(pineP+spruceP+blp))]
uniqueData[,BAsp:= (ba * spruceP/(pineP+spruceP+blp))]
uniqueData[,BAb:= (ba * blp/(pineP+spruceP+blp))]

dataSur <- uniqueData[,.(h,dbh,BAp,BAsp,BAb,siteType1,
                          siteType2,v2,ba2,h2,dbh2,segID)] 
setnames(dataSur,c("H","D","BAp","BAsp","BAb","st1",
                    "st2","V2","ba2","h2","dbh2","segID"))


dataSur[,BApPer:=.(BAp/sum(BAp,BAsp,BAb)*100),by=segID]
dataSur[,BAspPer:=.(BAsp/sum(BAp,BAsp,BAb)*100),by=segID]
dataSur[,BAbPer:=.(BAb/sum(BAp,BAsp,BAb)*100),by=segID]
dataSur[,BAtot:=.(sum(BAp,BAsp,BAb)),by=segID]



nSeg <- nrow(dataSur)  ##200
stProbMod <- matrix(NA,nSeg,5)


for(i in 1:nSeg){
  stProbMod[i,] <- pSTx(dataSur[i],nSample)
  # if (i %% 100 == 0) { print(i) }
}

stProbMod <- data.table(stProbMod)

###calculate probit2016
dataSur[,st:=st1]
probit1 <- predict(step.probit,type='p',dataSur[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data

###calculate probit2019
dataSur[,st:=st2]
probit2 <- predict(step.probit,type='p',dataSur[1:nSeg,])   ### needs to be changed . We need to calculate with 2016 and 2019 data

stProb <- array(NA, dim=c(nSeg,5,3))
stProb[,,1] <- probit1
stProb[,,2] <- probit2
stProb[,,3] <- as.matrix(stProbMod)

stProb <- apply(stProb, c(1,2), mean)

save(stProb,probit1,probit2,stProbMod,file="stProbMod.rdata")

