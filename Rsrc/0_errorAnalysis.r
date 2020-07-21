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
  sigma <- cov(resids)
  mu <- colMeans(resids)
  corMat <- cor(resids)
  return(list(resids = resids,mu=mu,sigma=sigma,corMat=corMat))
}
errData <- list()
errData$all <- calError(dataAll)
errData$y2016$all <- calError(data2016)
errData$y2019$all <- calError(data2019)
errData$y2016$t35VLJ <- calError(data2016[S2Tile == "35VLJ"])
errData$y2019$t35VLJ <- calError(data2019[S2Tile == "35VLJ"])
errData$y2016$t35VNL <- calError(data2016[S2Tile == "35VNL"])
errData$y2019$t35VNL <- calError(data2019[S2Tile == "35VNL"])
errData$y2016$t34VEQ <- calError(data2016[S2Tile == "34VEQ"])
errData$y2019$t34VEQ <- calError(data2019[S2Tile == "34VEQ"])
errData$y2016$t35WMN <- calError(data2016[S2Tile == "35WMN"])
errData$y2019$t35WMN <- calError(data2019[S2Tile == "35VLJ"])

errData$t35VLJ <- calError(dataAll[S2Tile == "35VLJ"])
errData$t35VNL <- calError(dataAll[S2Tile == "35VNL"])
errData$t34VEQ <- calError(dataAll[S2Tile == "34VEQ"])
errData$t35WMN <- calError(dataAll[S2Tile == "35WMN"])
