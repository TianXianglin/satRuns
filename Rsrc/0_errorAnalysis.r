library(data.table)

load("data/traningSites.rdata")
data2016$year <- 2016
data2019$year <- 2019
dataAll <- rbind(data2016,data2019)

###function to calculate residuals and MVN distribution parameters
calError <- function(dataX){
  resids <- data.table()
  resids$G <- dataX$G.mea- dataX$G.est
  resids$D <- dataX$D.mea- dataX$D.est
  resids$H <- (dataX$H.mea- dataX$H.est)/10
  resids$BAp <- (dataX$PINE.mea- dataX$PINE.est)
  resids$BAsp <- (dataX$SPRUCE.mea- dataX$SPRUCE.est)
  resids$BAb <- (dataX$BL.mea- dataX$BL.est)
  Vres <- (dataX$V.mea- dataX$V.est)
  sigma <- cov(resids)
  mu <- colMeans(resids)
  muV <- mean(Vres)
  sdV <- sd(Vres)
  corMat <- cor(resids)
  return(list(resids = resids,mu=mu,sigma=sigma,corMat=corMat, muV = muV, sdV = sdV))
}
errData <- list()
errData$all <- calError(dataAll)
# errData$y2016$all <- calError(data2016)
# errData$y2019$all <- calError(data2019)
# errData$y2016$t35VLJ <- calError(data2016[S2Tile == "35VLJ"])
# errData$y2019$t35VLJ <- calError(data2019[S2Tile == "35VLJ"])
# errData$y2016$t35VNL <- calError(data2016[S2Tile == "35VNL"])
# errData$y2019$t35VNL <- calError(data2019[S2Tile == "35VNL"])
# errData$y2016$t34VEQ <- calError(data2016[S2Tile == "34VEQ"])
# errData$y2019$t34VEQ <- calError(data2019[S2Tile == "34VEQ"])
# errData$y2016$t35WMN <- calError(data2016[S2Tile == "35WMN"])
# errData$y2019$t35WMN <- calError(data2019[S2Tile == "35VLJ"])
# 
# errData$t35VLJ <- calError(dataAll[S2Tile == "35VLJ"])
# errData$t35VNL <- calError(dataAll[S2Tile == "35VNL"])
# errData$t34VEQ <- calError(dataAll[S2Tile == "34VEQ"])
# errData$t35WMN <- calError(dataAll[S2Tile == "35WMN"])
save(errData,file="data/inputUncer.rdata")


#### Probit regression between sitetype.ref and sitetype.est 
summary(dataAll)
dataAll[Class.est>5,Class.est:=5]
dataAll[Class.ref>5,Class.ref:=5]
dataAll$alpha<-NA
dataAll$alpha[dataAll$Class.est==1]<- 0.2833333 #mean(pCROB['alfar1',1:3])
dataAll$alpha[dataAll$Class.est==2]<- 0.4066667 #mean(pCROB['alfar2',1:3])
dataAll$alpha[dataAll$Class.est==3]<- 0.4966667 #mean(pCROB['alfar3',1:3])
dataAll$alpha[dataAll$Class.est==4]<- 0.6233333 #mean(pCROB['alfar4',1:3])
dataAll$alpha[dataAll$Class.est==5]<- 0.7866667 #mean(pCROB['alfar5',1:3])

dataAll$Class.est.f<-factor(dataAll$Class.est,levels = 1:5)
dataAll$Class.ref.f<-factor(dataAll$Class.ref,levels = 1:5)

full.probit<-polr(Class.ref.f ~ Class.est+alpha+H.est+D.est+G.est,data=dataAll ,method='probit')
step.probit <- stepAIC(full.probit, direction = "both",
                      trace = FALSE)
predict(step.probit,type='p')

save(step.probit,file = 'data/stProbit.rdata')

#### logistic regression to predict the pure forests
dataAll$max.pro.ref<-apply(dataAll[, c('PINE.mea','SPRUCE.mea','BL.mea')], 1, max)
dataAll$max.pro.est<-apply(dataAll[, c('PINE.est','SPRUCE.est','BL.est')], 1, max)
dataAll$pure.ref<-F
dataAll$pure.ref[dataAll$max.pro.ref>=100]<-T
# dataAll$pure.ref<-as.factor(dataAll$pure.ref)
logistic.model<-glm(formula = pure.ref ~ max.pro.est, family = binomial(link = "logit"), 
                 data = dataAll)
summary(logistic.model)
plot(dataAll$max.pro.est,predict(logistic.model,type="response"))
save(logistic.model,file = 'data/logisticPureF.rdata')
