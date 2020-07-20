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

lmod <- lm(Vmod~H+D+BAp+BAsp+BAb+st,data=dataX)  ###Xianglin!!!!

load("procData/init2016/st2019/uniqueData.rdata")
load("outDT/init2016/st2019/V_NoHarv_CurrClimlayerall.rdata")
Vmod3 <- data.table(cbind(segID,V[,3]))
setnames(Vmod3,c("segID","Vpreb3y"))

uniqueData[,BAp:= (ba * pineP/(pineP+spruceP+blp))]
uniqueData[,BAsp:= (ba * spruceP/(pineP+spruceP+blp))]
uniqueData[,BAb:= (ba * blp/(pineP+spruceP+blp))]

dataSurV <- uniqueData[,.(h,dbh,BAp,BAsp,BAb,siteType,v2,segID)] 
setnames(dataSurV,c("H","D","BAp","BAsp","BAb","st","V2","segID"))

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

