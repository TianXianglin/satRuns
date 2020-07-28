library(MASS)
### Run settings & functions
source("Rsrc/settings.r")
source("Rsrc/functions.r")

###check and create output directories
setwd(generalPath)

yearX <- 3
nSample = 100 ###number of samples from the error distribution
load(paste0("procData/init",startingYear,"/","st",siteTypeX,"/uniqueData.rdata"))  
load("C:/Users/minunno/GitHub/satRuns/data/inputUncer.rdata")
load("surErrMods/logisticPureF.rdata")
load("surErrMods/stProbit.rdata")
load("surErrMods/surMod.rdata")


uniqueData[,BAp:= (ba * pineP/(pineP+spruceP+blp))]
uniqueData[,BAsp:= (ba * spruceP/(pineP+spruceP+blp))]
uniqueData[,BAb:= (ba * blp/(pineP+spruceP+blp))]

dataSurV <- uniqueData[,.(h,dbh,BAp,BAsp,BAb,siteType,v2,segID)] 
setnames(dataSurV,c("H","D","BAp","BAsp","BAb","st","V2","segID"))


dataSurV[,BApPer:=.(BAp/sum(BAp,BAsp,BAb)*100),by=segID]
dataSurV[,BAspPer:=.(BAsp/sum(BAp,BAsp,BAb)*100),by=segID]
dataSurV[,BAbPer:=.(BAb/sum(BAp,BAsp,BAb)*100),by=segID]
dataSurV[,BAtot:=.(sum(BAp,BAsp,BAb)),by=segID]



stProbs <- matrix(NA,nrow(dataSurV),5)
nSeg <- nrow(dataSurV)  ##200

for(i in 1:nSeg){
  stProbs[i,] <- pSTx(dataSurV[i],nSample)
  # if (i %% 100 == 0) { print(i) }
}

stProbs <- data.table(stProbs)
save(stProbs,file="stProbs.rdata")


# test <- predict(step.probit,type='p',dataSurV[1:200,])   ### needs to be changed . We need to calculate with 2016 and 2019 data
# ops <- array(NA, dim = c(200,5,2))
# ops[,,1] <- as.matrix(stProbs[1:200])
# ops[,,2] <- test
# 
# 
# # system.time(ll <- dataSurV[1:200, pSTx(.SDcol,nSample),by=segID])
# boxplot(stProbs[1:200])
# ciao <- data.table(apply(ops,1:2,mean))
# boxplot(data.table(test))
# boxplot(ciao)
