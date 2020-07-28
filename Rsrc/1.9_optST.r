library(MASS)
### Run settings & functions
source("Rsrc/settings.r")
source("Rsrc/functions.r")

###check and create output directories
setwd(generalPath)

yearX <- 3
nSample = 10
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

fixBAper <- function(BApers){
  minBA <- min(BApers)
  if(minBA<0) BApers <- BApers - minBA
  BApers <- BApers/sum(BApers)*100
  return(BApers)
}

pSTx <- function(segIDx,nSample){
    # set.seed(123)
    sampleError <- data.table(mvrnorm(nSample*2,mu=errData$all$mu,Sigma=errData$all$sigma))
  # segIDx <- dataSurV[segID==2]
  sampleX <- data.table()
  sampleX$H <- segIDx$H + sampleError$H
  sampleX$D <- segIDx$D + sampleError$D
  sampleX$BAtot <- segIDx$BAtot + sampleError$G
  sampleX$BApPer <- segIDx$BApPer + sampleError$BAp
  sampleX$BAspPer <- segIDx$BAspPer + sampleError$BAsp
  sampleX$BAbPer <- segIDx$BAbPer + sampleError$BAb
  sampleX <- sampleX[H>1.5]
  sampleX <- sampleX[D>0.5]
  sampleX <- sampleX[BAtot>0.045]
  sampleX <- sampleX[1:min(nSample,nrow(sampleX))]
  if(nrow(sampleX)<nSample){
    sample1 <- sampleX
    # set.seed(1234)
    sampleError <- data.table(mvrnorm(nSample*2,mu=errData$all$mu,Sigma=errData$all$sigma))
    # segIDx <- dataSurV[segID==2]
    sampleX <- data.table()
    sampleX$H <- segIDx$H + sampleError$H
    sampleX$D <- segIDx$D + sampleError$D
    sampleX$BAtot <- segIDx$BAtot + sampleError$G
    sampleX$BApPer <- segIDx$BApPer + sampleError$BAp
    sampleX$BAspPer <- segIDx$BAspPer + sampleError$BAsp
    sampleX$BAbPer <- segIDx$BAbPer + sampleError$BAb
    sampleX <- sampleX[H>1.5]
    sampleX <- sampleX[D>0.5]
    sampleX <- sampleX[BAtot>0.045]
    sampleX <- rbind(sample1,sampleX)
    sampleX <- sampleX[1:min(nSample,nrow(sampleX))]
  }
  
  sampleX[, c("BApPer", "BAspPer", "BAbPer"):=
            as.list(fixBAper(unlist(.(BApPer,BAspPer,BAbPer)))), 
          by = seq_len(nrow(sampleX))]
  
  max.pro.est<-apply(segIDx[, c('BApPer','BAspPer','BAbPer')], 1, which.max)
  segIDx[,max.pro.est:=max.pro.est]
  # set.seed(123)
  sampleX$pureF <- runif(min(nSample,nrow(sampleX)),0,1)<predict(logistic.model,type="response",newdata = segIDx)
  if(max.pro.est==1) sampleX[which(pureF),c("BApPer","BAspPer","BAbPer"):=list(100,0,0)]
  if(max.pro.est==2) sampleX[which(pureF),c("BApPer","BAspPer","BAbPer"):=list(0,100,0)]
  if(max.pro.est==3) sampleX[which(pureF),c("BApPer","BAspPer","BAbPer"):=list(0,0,100)]
  
  sampleX[,BAp:=BApPer*BAtot/100]
  sampleX[,BAsp:=BAspPer*BAtot/100]
  sampleX[,BAb:=BAbPer*BAtot/100]
  sampleX[,st:=segIDx$st]
  sampleX[,V2:=segIDx$V2]
  sampleX[,segID:=segIDx$segID]
  
  # sampleX$lnVmod<-log(sampleX$Vmod)
  # sampleX$st<-factor(sampleX$st,levels = 1:5)     ##!!!!Xianglin
  sampleX$st <- factor(sampleX$st)
  sampleX[,BAtot:=(BAp+BAsp+BAb)]
  sampleX[,BAh:=BAtot*H]
  sampleX[,N:=BAtot/(pi*(D/200)^2)]
  b = -1.605 ###coefficient of Reineke
  sampleX[,SDI:=N *(D/10)^b]
  
  # full.model<-lm(lnVmod~H+D+lnBAp+lnBAsp+lnBAb+st,data=dataX)
  sampleX$st <- factor(1)
  sampleX[,VsurST1 := pmax(0.,predict(step.model,newdata=sampleX))]
  sampleX$st <- factor(2)
  sampleX[,VsurST2 := pmax(0.,predict(step.model,newdata=sampleX))]
  sampleX$st <- factor(3)
  sampleX[,VsurST3 := pmax(0.,predict(step.model,newdata=sampleX))]
  sampleX$st <- factor(4)
  sampleX[,VsurST4 := pmax(0.,predict(step.model,newdata=sampleX))]
  sampleX$st <- factor(5)
  sampleX[,VsurST5 := pmax(0.,predict(step.model,newdata=sampleX))]
  
  pst1 <- mean(dnorm(sampleX$VsurST1 - segIDx$V2,mean=errData$all$muV,sd=errData$all$sdV))
  pst2 <- mean(dnorm(sampleX$VsurST2 - segIDx$V2,mean=errData$all$muV,sd=errData$all$sdV))
  pst3 <- mean(dnorm(sampleX$VsurST3 - segIDx$V2,mean=errData$all$muV,sd=errData$all$sdV))
  pst4 <- mean(dnorm(sampleX$VsurST4 - segIDx$V2,mean=errData$all$muV,sd=errData$all$sdV))
  pst5 <- mean(dnorm(sampleX$VsurST5 - segIDx$V2,mean=errData$all$muV,sd=errData$all$sdV))
  psum <- pst1 +pst2+pst3 +pst4+pst5
  pst1 <- pst1/psum
  pst2 <- pst2/psum
  pst3 <- pst3/psum
  pst4 <- pst4/psum
  pst5 <- pst5/psum
 return(pST=c(pst1,pst2,pst3,pst4,pst5)) 
}

stProbs <- matrix(NA,nrow(dataSurV),5)
nSeg <- 200 #nrow(dataSurV)

system.time(for(i in 1:nSeg){
  stProbs[i,] <- pSTx(dataSurV[i],nSample)
  if (i %% 100 == 0) { print(i) }
} )

stProbs <- data.table(stProbs)
test <- predict(step.probit,type='p',dataSurV[1:200,])   ### needs to be changed . We need to calculate with 2016 and 2019 data
ops <- array(NA, dim = c(200,5,2))
ops[,,1] <- as.matrix(stProbs[1:200])
ops[,,2] <- test


# system.time(ll <- dataSurV[1:200, pSTx(.SDcol,nSample),by=segID])
boxplot(stProbs[1:200])
ciao <- data.table(apply(ops,1:2,mean))
boxplot(data.table(test))
boxplot(ciao)
