library(MASS)
### Run settings & functions
source("Rsrc/settings.r")
source("Rsrc/functions.r")

###check and create output directories
setwd(generalPath)

yearX <- 3

load("procData/init2016/st2019/XYsegID.rdata")
load("output/init2016/st2016/CurrClim_sample1.rdata")
Vmod2019 <- rowSums(out[,yearX,6,])
load("initPrebas/init2016/st2016/CurrClim_sample1.rdata")
dataX <- data.table(cbind(initPrebas$multiInitVar[,3:5,1],initPrebas$multiInitVar[,5,2],
                          initPrebas$multiInitVar[,5,3],initPrebas$siteInfo[,3],Vmod2019))
setnames(dataX,c("H","D","BAp","BAsp","BAb","st","Vmod"))

## lmod <- lm(Vmod~H+D+BAp+BAsp+BAb+st,data=dataX)  ###Xianglin!!!!
#### Here we use stepwise regression to construct an emulator for volume prediction
library(MASS)
dataX$lnVmod<-log(dataX$Vmod)
dataX$st<-as.factor(dataX$st)
dataX$lnBAp<-log(dataX$BAp+1)
dataX$lnBAsp<-log(dataX$BAsp+1)
dataX$lnBAb<-log(dataX$BAb+1)
full.model<-lm(lnVmod~H+D+lnBAp+lnBAsp+lnBAb+st,data=dataX)
step.model <- stepAIC(full.model, direction = "both",
                        trace = FALSE)
#summary(step.model)
#sd(exp(predict(step.model))-dataX$Vmod)
#### 

load("procData/init2016/st2019/uniqueData.rdata")
load("outDT/init2016/st2019/V_NoHarv_CurrClimlayerall.rdata")
Vmod3 <- data.table(cbind(segID,V[,3]))
setnames(Vmod3,c("segID","Vpreb3y"))

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
  return(BApers)
}

nSample = 10
pSTx <- function(segIDx,nSample){
    set.seed(123)
    sampleError <- data.table(mvrnorm(nSample*2,mu=errData$all$mu,Sigma=errData$all$sigma))
  segIDx <- dataSurV[segID==2]
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
  
  
  
  sampleX[, c("BApPer", "BAspPer", "BAbPer"):=
            as.list(fixBAper(unlist(.(BApPer,BAspPer,BAbPer)))), 
          by = seq_len(nrow(sampleX))]
  
  
  sampleX[,BAp:=BApPer*BAtot/100]
  sampleX[,BAsp:=BAspPer*BAtot/100]
  sampleX[,BAb:=BAbPer*BAtot/100]
  sampleX[,st:=segIDx$st]
  sampleX[,V2:=segIDx$V2]
  sampleX[,segID:=segIDx$segID]
  

  sampleX$st <- 1
  sampleX[,VsurST1 := predict(lmod,sampleX)]
  sampleX$st <- 2
  sampleX[,VsurST2 := predict(lmod,sampleX)]
  sampleX$st <- 3
  sampleX[,VsurST3 := predict(lmod,sampleX)]
  sampleX$st <- 4
  sampleX[,VsurST4 := predict(lmod,sampleX)]
  sampleX$st <- 5
  sampleX[,VsurST5 := predict(lmod,sampleX)]
  
  pst1 <- mean(dnorm(sampleX$VsurST1,sd=10))
  pst2 <- mean(dnorm(sampleX$VsurST2,sd=10))
  pst3 <- mean(dnorm(sampleX$VsurST3,sd=10))
  pst4 <- mean(dnorm(sampleX$VsurST4,sd=10))
  pst5 <- mean(dnorm(sampleX$VsurST5,sd=10))
  psum <- pst1 +pst2+pst3 +pst4+pst5
  pst1 <- pst1/psum
  pst2 <- pst2/psum
  pst3 <- pst3/psum
  pst4 <- pst4/psum
  pst5 <- pst5/psum
 return(pST=c(pst1,pst2,pst3,pst4,pst5)) 
}

stProbs <- matrix(NA,nrow(dataSurV),5)
# system.time(for(i in 1:2000){
#   stProbs[i,] <- pSTx(dataSurV[i],nSample)
#   # if (i %% 100 == 0) { print(i) }
# } )

system.time(ll <- dataSurV[1:2000, pSTx(.SDcol,nSample),by=segID])




















ciao <- pSTx(dataSurV[segID==2],nSample)

###Xianglin!!!!
dataSurV <- transform(dataSurV, optST = pmin((V2-VsurST1)^2,         
                                             (V2-VsurST2)^2,
                                             (V2-VsurST3)^2,
                                             (V2-VsurST4)^2,
                                             (V2-VsurST5)^2))

dataSurV[, optST := apply(.SD, 1, which.min), 
         .SDcols = c("VsurST1","VsurST2","VsurST3","VsurST4","VsurST5")]








dataSurV
dataSurV[,Vsur := predict(lmod,dataSurV)]
setkey(dataSurV,segID)  
setkey(Vmod3,segID)  
dataSurV <- merge(dataSurV,Vmod3,by="segID")
# plot(dataSurV$Vsur,dataSurV$Vpreb3y,pch='.')
# abline(0,1)


dataSurV$st <- 1
dataSurV[,VsurST1 := predict(lmod,dataSurV)]
dataSurV$st <- 2
dataSurV[,VsurST2 := predict(lmod,dataSurV)]
dataSurV$st <- 3
dataSurV[,VsurST3 := predict(lmod,dataSurV)]
dataSurV$st <- 4
dataSurV[,VsurST4 := predict(lmod,dataSurV)]
dataSurV$st <- 5
dataSurV[,VsurST5 := predict(lmod,dataSurV)]

###Xianglin!!!!
dataSurV <- transform(dataSurV, optST = pmin((V2-VsurST1)^2,         
                                             (V2-VsurST2)^2,
                                             (V2-VsurST3)^2,
                                             (V2-VsurST4)^2,
                                             (V2-VsurST5)^2))

dataSurV[, optST := apply(.SD, 1, which.min), 
  .SDcols = c("VsurST1","VsurST2","VsurST3","VsurST4","VsurST5")]

