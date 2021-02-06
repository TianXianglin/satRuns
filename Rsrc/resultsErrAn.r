library(ggpubr)
library(ggplot2)
library(devtools)
library(data.table)
devtools::source_url("https://raw.githubusercontent.com/ForModLabUHel/utilStuff/master/ErrorDecomposition/ErrorDecomposition.R")

load("~/research/assessCarbon/data2019res_34VEQ.rdata")
data2019res
plot(data2019res$G.est,data2019res$G.mea)
points(data2019res$BDA2019,data2019res$G.mea,col=2,pch=20)
points(data2019res$Bs2019,data2019res$G.mea,col=3,pch=20)
points(data2019res$Bm2019,data2019res$G.mea,col=4,pch=20)
abline(0,1)
p_rmse <- function(sim,obs) sqrt( mean( (sim-obs)^2) ) / ( max(obs)-min(obs) )*100

MSEs <- data.table(value=as.numeric(unlist(MSEdec("B_est", data2019res$G.est,data2019res$G.mea,method=1))[2:5]),
                       run="est",variable="B",components=c("sb","sdsd","lc","mse"),
                   rangeObs = max(data2019res$G.mea,na.rm=T)-min(data2019res$G.mea,na.rm=T))
MSEs <- rbind(MSEs,data.table(value=as.numeric(unlist(MSEdec("B", data2019res$Bm2019,data2019res$G.mea,method=1))[2:5]),
                   run="m2019",variable="B",components=c("sb","sdsd","lc","mse"),
              rangeObs = max(data2019res$G.mea,na.rm=T)-min(data2019res$G.mea,na.rm=T)))
MSEs <- rbind(MSEs,data.table(value=as.numeric(unlist(MSEdec("B", data2019res$Bs2019,data2019res$G.mea,method=1))[2:5]),
                              run="s2019",variable="B",components=c("sb","sdsd","lc","mse"),
              rangeObs = max(data2019res$G.mea,na.rm=T)-min(data2019res$G.mea,na.rm=T)))
MSEs <- rbind(MSEs,data.table(value=as.numeric(unlist(MSEdec("B", data2019res$BDA2019,data2019res$G.mea,method=1))[2:5]),
                              run="DA2019",variable="B",components=c("sb","sdsd","lc","mse"),
              rangeObs = max(data2019res$G.mea,na.rm=T)-min(data2019res$G.mea,na.rm=T)))

MSEs <- rbind(MSEs,data.table(value=as.numeric(unlist(MSEdec("D", data2019res$D.est,data2019res$D.mea,method=1))[2:5]),
                   run="est",variable="D",components=c("sb","sdsd","lc","mse"),
              rangeObs = max(data2019res$D.mea,na.rm=T)-min(data2019res$D.mea,na.rm=T)))
MSEs <- rbind(MSEs,data.table(value=as.numeric(unlist(MSEdec("D", data2019res$Dm2019,data2019res$D.mea,method=1))[2:5]),
                              run="m2019",variable="D",components=c("sb","sdsd","lc","mse"),
              rangeObs = max(data2019res$D.mea,na.rm=T)-min(data2019res$D.mea,na.rm=T)))
MSEs <- rbind(MSEs,data.table(value=as.numeric(unlist(MSEdec("D", data2019res$Ds2019,data2019res$D.mea,method=1))[2:5]),
                              run="s2019",variable="D",components=c("sb","sdsd","lc","mse"),
              rangeObs = max(data2019res$D.mea,na.rm=T)-min(data2019res$D.mea,na.rm=T)))
MSEs <- rbind(MSEs,data.table(value=as.numeric(unlist(MSEdec("D", data2019res$DDA2019,data2019res$D.mea,method=1))[2:5]),
                              run="DA2019",variable="D",components=c("sb","sdsd","lc","mse"),
              rangeObs = max(data2019res$D.mea,na.rm=T)-min(data2019res$D.mea,na.rm=T)))

MSEs <- rbind(MSEs,data.table(value=as.numeric(unlist(MSEdec("H", data2019res$H.est/10,data2019res$H.mea/10,method=1))[2:5]),
                              run="est",variable="H",components=c("sb","sdsd","lc","mse"),
              rangeObs = max(data2019res$H.mea/10,na.rm=T)-min(data2019res$H.mea/10,na.rm=T)))
MSEs <- rbind(MSEs,data.table(value=as.numeric(unlist(MSEdec("H", data2019res$Hm2019,data2019res$H.mea/10,method=1))[2:5]),
                              run="m2019",variable="H",components=c("sb","sdsd","lc","mse"),
              rangeObs = max(data2019res$H.mea/10,na.rm=T)-min(data2019res$H.mea/10,na.rm=T)))
MSEs <- rbind(MSEs,data.table(value=as.numeric(unlist(MSEdec("H", data2019res$Hs2019,data2019res$H.mea/10,method=1))[2:5]),
                              run="s2019",variable="H",components=c("sb","sdsd","lc","mse"),
              rangeObs = max(data2019res$H.mea/10,na.rm=T)-min(data2019res$H.mea/10,na.rm=T)))
MSEs <- rbind(MSEs,data.table(value=as.numeric(unlist(MSEdec("H", data2019res$HDA2019,data2019res$H.mea/10,method=1))[2:5]),
                              run="DA2019",variable="H",components=c("sb","sdsd","lc","mse"),
              rangeObs = max(data2019res$H.mea/10,na.rm=T)-min(data2019res$H.mea/10,na.rm=T)))

MSEs[components=="mse", RMSE:=sqrt(value)]

for(runX in unique(MSEs$run)){
  for(vX in unique(MSEs$variable)){
    MSEs[run==runX & variable==vX & !components%in%"mse",RMSE:=value/sum(value)]
    MSEs[run==runX & variable==vX & !components%in%"mse",]$RMSE <- MSEs[run==runX & variable==vX & !components%in%"mse",]$RMSE * MSEs[run==runX & variable==vX & components%in%"mse",]$RMSE
  }
}

MSEs$pRMSE <- MSEs$RMSE/MSEs$rangeObs*100



MSEs$run <- factor(MSEs$run, levels = c("m2019","s2019","est","DA2019"))
MSEs$components <- factor(MSEs$components, levels = c("sb","sdsd","lc","mse"))
varX = "B"
pMSE_B <- ggplot(data=MSEs[variable==varX & !components %in% "mse"& !run %in% "est"], aes(x=run, y=value, fill=components)) +
  geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)
varX = "D"
pMSE_D <- ggplot(data=MSEs[variable==varX & !components %in% "mse"& !run %in% "est"], aes(x=run, y=value, fill=components)) +
  geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)
varX = "H"
pMSE_H <- ggplot(data=MSEs[variable==varX & !components %in% "mse"& !run %in% "est"], aes(x=run, y=value, fill=components)) +
  geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)

varX = "B"
pMSE_B <- ggplot(data=MSEs[variable==varX & components %in% "mse"& !run %in% "est"],
                 aes(x=run, y=sqrt(value)/mean(value))) + geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)
varX = "D"
pMSE_D <- ggplot(data=MSEs[variable==varX & !components %in% "mse"& !run %in% "est"], aes(x=run, y=value, fill=components)) +
  geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)
varX = "H"
pMSE_H <- ggplot(data=MSEs[variable==varX & !components %in% "mse"& !run %in% "est"], aes(x=run, y=value, fill=components)) +
  geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)

ggarrange(pMSE_B,pMSE_D,pMSE_H)



####pBias
pBIASs <- data.table(value=100*sum(data2019res$G.est-data2019res$G.mea)/sum(data2019res$G.mea),
                   run="est",variable="B")
pBIASs <- rbind(pBIASs,data.table(value=100*sum(data2019res$Bm2019-data2019res$G.mea)/sum(data2019res$G.mea),
                              run="m2019",variable="B"))
pBIASs <- rbind(pBIASs,data.table(value=100*sum(data2019res$Bs2019-data2019res$G.mea)/sum(data2019res$G.mea),
                              run="s2019",variable="B"))
pBIASs <- rbind(pBIASs,data.table(value=100*sum(data2019res$BDA2019-data2019res$G.mea)/sum(data2019res$G.mea),
                              run="DA2019",variable="B"))

pBIASs <- rbind(pBIASs,data.table(value=100*sum(data2019res$D.est-data2019res$D.mea)/sum(data2019res$D.mea),
                     run="est",variable="D"))
pBIASs <- rbind(pBIASs,data.table(value=100*sum(data2019res$Dm2019-data2019res$D.mea)/sum(data2019res$D.mea),
                                  run="m2019",variable="D"))
pBIASs <- rbind(pBIASs,data.table(value=100*sum(data2019res$Ds2019-data2019res$D.mea)/sum(data2019res$D.mea),
                                  run="s2019",variable="D"))
pBIASs <- rbind(pBIASs,data.table(value=100*sum(data2019res$DDA2019-data2019res$D.mea)/sum(data2019res$D.mea),
                                  run="DA2019",variable="D"))

pBIASs <- rbind(pBIASs,data.table(value=100*sum(data2019res$H.est/10-data2019res$H.mea/10)/sum(data2019res$H.mea/10),
                     run="est",variable="H"))
pBIASs <- rbind(pBIASs,data.table(value=100*sum(data2019res$Hm2019-data2019res$H.mea/10)/sum(data2019res$H.mea/10),
                                  run="m2019",variable="H"))
pBIASs <- rbind(pBIASs,data.table(value=100*sum(data2019res$Hs2019-data2019res$H.mea/10)/sum(data2019res$H.mea/10),
                                  run="s2019",variable="H"))
pBIASs <- rbind(pBIASs,data.table(value=100*sum(data2019res$HDA2019-data2019res$H.mea/10)/sum(data2019res$H.mea/10),
                                  run="DA2019",variable="H"))



pBIASs$run <- factor(pBIASs$run, levels = c("m2019","s2019","est","DA2019"))
varX = "B"
pBIAS_B <- ggplot(data=pBIASs[variable==varX & !run %in% "est"],
                 aes(x=run, y=value)) + geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)
varX = "D"
pBIAS_D <- ggplot(data=pBIASs[variable==varX & !run %in% "est"],
                  aes(x=run, y=value)) + geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)
varX = "H"
pBIAS_H <- ggplot(data=pBIASs[variable==varX & !run %in% "est"],
                  aes(x=run, y=value)) + geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)




####pRMSE
pRMSEs <- data.table(value=p_rmse(data2019res$G.est,data2019res$G.mea),
                     run="est",variable="B")
pRMSEs <- rbind(pRMSEs,data.table(value=p_rmse(data2019res$Bm2019,data2019res$G.mea),
                                  run="m2019",variable="B"))
pRMSEs <- rbind(pRMSEs,data.table(value=p_rmse(data2019res$Bs2019,data2019res$G.mea),
                                  run="s2019",variable="B"))
pRMSEs <- rbind(pRMSEs,data.table(value=p_rmse(data2019res$BDA2019,data2019res$G.mea),
                                  run="DA2019",variable="B"))

pRMSEs <- rbind(pRMSEs,data.table(value=p_rmse(data2019res$D.est,data2019res$D.mea),
                                  run="est",variable="D"))
pRMSEs <- rbind(pRMSEs,data.table(value=p_rmse(data2019res$Dm2019,data2019res$D.mea),
                                  run="m2019",variable="D"))
pRMSEs <- rbind(pRMSEs,data.table(value=p_rmse(data2019res$Ds2019,data2019res$D.mea),
                                  run="s2019",variable="D"))
pRMSEs <- rbind(pRMSEs,data.table(value=p_rmse(data2019res$DDA2019,data2019res$D.mea),
                                  run="DA2019",variable="D"))

pRMSEs <- rbind(pRMSEs,data.table(value=p_rmse(data2019res$H.est/10,data2019res$H.mea/10),
                                  run="est",variable="H"))
pRMSEs <- rbind(pRMSEs,data.table(value=p_rmse(data2019res$Hm2019,data2019res$H.mea/10),
                                  run="m2019",variable="H"))
pRMSEs <- rbind(pRMSEs,data.table(value=p_rmse(data2019res$Hs2019,data2019res$H.mea/10),
                                  run="s2019",variable="H"))
pRMSEs <- rbind(pRMSEs,data.table(value=p_rmse(data2019res$HDA2019,data2019res$H.mea/10),
                                  run="DA2019",variable="H"))



pRMSEs$run <- factor(pRMSEs$run, levels = c("m2019","s2019","est","DA2019"))
varX = "B"
pRMSEs_B <- ggplot(data=pRMSEs[variable==varX & !run %in% "est"],
                  aes(x=run, y=value)) + geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)
varX = "D"
pRMSEs_D <- ggplot(data=pRMSEs[variable==varX & !run %in% "est"],
                  aes(x=run, y=value)) + geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)
varX = "H"
pRMSEs_H <- ggplot(data=pRMSEs[variable==varX & !run %in% "est"],
                  aes(x=run, y=value)) + geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)




###pRMSE decomposed

MSEs$run <- factor(MSEs$run, levels = c("m2019","s2019","est","DA2019"))
MSEs$components <- factor(MSEs$components, levels = c("sb","sdsd","lc","mse"))
varX = "B"
pRMSE_B <- ggplot(data=MSEs[variable==varX & !components %in% "mse"& !run %in% "est"], aes(x=run, y=pRMSE, fill=components)) +
  geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)
varX = "D"
pRMSE_D <- ggplot(data=MSEs[variable==varX & !components %in% "mse"& !run %in% "est"], aes(x=run, y=pRMSE, fill=components)) +
  geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)
varX = "H"
pRMSE_H <- ggplot(data=MSEs[variable==varX & !components %in% "mse"& !run %in% "est"], aes(x=run, y=pRMSE, fill=components)) +
  geom_bar(stat="identity") + ggtitle(varX)+
  xlab(NULL)+ylab(NULL)

# varX = "B"
# pRMSE_B <- ggplot(data=MSEs[variable==varX & components %in% "mse"& !run %in% "est"],
#                  aes(x=run, y=sqrt(pRMSE)/mean(pRMSE))) + geom_bar(stat="identity") + ggtitle(varX)+
#   xlab(NULL)+ylab(NULL)
# varX = "D"
# pRMSE_D <- ggplot(data=MSEs[variable==varX & !components %in% "mse"& !run %in% "est"], aes(x=run, y=pRMSE, fill=components)) +
#   geom_bar(stat="identity") + ggtitle(varX)+
#   xlab(NULL)+ylab(NULL)
# varX = "H"
# pRMSE_H <- ggplot(data=MSEs[variable==varX & !components %in% "mse"& !run %in% "est"], aes(x=run, y=pRMSE, fill=components)) +
#   geom_bar(stat="identity") + ggtitle(varX)+
#   xlab(NULL)+ylab(NULL)

ggarrange(pRMSE_B,pRMSE_D,pRMSE_H)




pmse <- ggarrange(pMSE_B,pMSE_D,pMSE_H,nrow=1,ncol=3,common.legend = T,legend = "bottom")
pmse <- annotate_figure(pmse,top = text_grob("MSE", color = "black", face = "bold", size = 14))
pbias <- ggarrange(pBIAS_B,pBIAS_D,pBIAS_H,nrow=1,ncol=3)
pbias <- annotate_figure(pbias,top = text_grob("pBias", color = "black", face = "bold", size = 14))
prmse <- ggarrange(pRMSEs_B,pRMSEs_D,pRMSEs_H,nrow=1,ncol=3)
prmse <- annotate_figure(prmse,top = text_grob("pRMSE", color = "black", face = "bold", size = 14))
prmsedec <- ggarrange(pRMSE_B,pRMSE_D,pRMSE_H,nrow=1,ncol=3)
prmsedec <- annotate_figure(prmsedec,top = text_grob("pRMSE", color = "black", face = "bold", size = 14))

# errorPlot <- ggarrange(pmse,pbias,prmse,nrow=3)
errorPlot <- ggarrange(prmsedec,pbias,nrow=3)
errorPlot
ggsave(errorPlot,file="errorPlot.jpeg",device = "jpeg")
# 
# dev.off()
# 
# 
# .libPaths(c("/projappl/project_2000994/project_rpackages", .libPaths()))
# libpath <- .libPaths()[1]
# 
# library(raster)
# load("/scratch/project_2000994/PREBASruns/assessCarbon/data/traningSites.rdata")
# 
# load("data/traningSites.rdata")
# tiles <- unique(data2019$S2Tile)
# 
# tileX  <- "34VEQ"
# 
# 
# Hda2019_r <- raster("C:/Users/minunno/Documents/research/assessCarbon/data/Finland/AC_training_FI_34VEQ/outRast/init2016/DA2019/HDA2019.tif")
# Hs2019_r <- raster("C:/Users/minunno/Documents/research/assessCarbon/data/Finland/AC_training_FI_34VEQ/outRast/init2016/DA2019/Hs2019.tif")
# Hm2019_r <- raster("C:/Users/minunno/Documents/research/assessCarbon/data/Finland/AC_training_FI_34VEQ/outRast/init2016/DA2019/Hm2019.tif")
# # plot(Hpost)
# xx <- data2019[S2Tile==tileX]
# Hda2019 <- extract(Hda2019_r,xx[,.(XCOORD,YCOORD)])
# Hm2019 <- extract(Hm2019_r,xx[,.(XCOORD,YCOORD)])
# Hs2019 <- extract(Hs2019_r,xx[,.(XCOORD,YCOORD)])
# plot(xx$H.mea/10,xx$H.est/10)
# abline(0,1)
# points(Hda2019,xx$H.mea/10,col=2,pch=20)
# points(xx$H.mea/10,Hm2019,col=3,pch=20)
# points(xx$H.mea/10,Hs2019,col=4,pch=20)
# 
# 
# ops <- lm(xx$H.mea/10~Hda2019)
# summary(ops)
# ops2 <- lm(xx$H.mea/10~Hs2019)
# summary(ops2)
# 
# 
# 
# 
# 
# Dda2019_r <- raster("C:/Users/minunno/Documents/research/assessCarbon/data/Finland/AC_training_FI_34VEQ/outRast/init2016/DA2019/DDA2019.tif")
# Ds2019_r <- raster("C:/Users/minunno/Documents/research/assessCarbon/data/Finland/AC_training_FI_34VEQ/outRast/init2016/DA2019/Ds2019.tif")
# Dm2019_r <- raster("C:/Users/minunno/Documents/research/assessCarbon/data/Finland/AC_training_FI_34VEQ/outRast/init2016/DA2019/Dm2019.tif")
# # plot(Hpost)
# # xx <- data2019[S2Tile==tileX]
# Dda2019 <- extract(Dda2019_r,xx[,.(XCOORD,YCOORD)])
# Dm2019 <- extract(Dm2019_r,xx[,.(XCOORD,YCOORD)])
# Ds2019 <- extract(Ds2019_r,xx[,.(XCOORD,YCOORD)])
# plot(Dda2019,xx$D.mea)
# abline(0,1)
# points(xx$D.mea,xx$D.est,col=2,pch=20)
# points(xx$D.mea,Dm2019,col=3,pch=20)
# points(xx$D.mea,Ds2019,col=4,pch=20)
# 
# 
# ops <- lm(xx$D.mea~Dda2019)
# summary(ops)
# ops2 <- lm(xx$D.mea~Ds2019)
# summary(ops2)
# ops3 <- lm(xx$D.mea~Dm2019)
# summary(ops3)
# 
# 
# 
# 
# 
# Bpost <- raster("C:/Users/minunno/Documents/research/assessCarbon/Bpost.tif")
# Bs2019_r <- raster("C:/Users/minunno/Documents/research/assessCarbon/B2.tif")
# Bm2019_r <- raster("C:/Users/minunno/Documents/research/assessCarbon/Bprior.tif")
# # plot(Hpost)
# # xx <- data2019[S2Tile==tileX]
# Bda2019 <- extract(Bpost,xx[,.(XCOORD,YCOORD)])
# Bm2019 <- extract(Bm2019_r,xx[,.(XCOORD,YCOORD)])
# Bs2019 <- extract(Bs2019_r,xx[,.(XCOORD,YCOORD)])
# plot(Bda2019,xx$G.mea)
# abline(0,1)
# points(xx$G.mea,xx$G.est,col=2,pch=20)
# points(xx$G.mea,Bm2019,col=3,pch=20)
# points(xx$G.mea,Bs2019,col=4,pch=20)
# 
# 
# ops <- lm(xx$G.mea~Bda2019)
# summary(ops)
# ops2 <- lm(xx$G.mea~Bs2019)
# summary(ops2)
# ops3 <- lm(xx$G.mea~Bm2019)
# summary(ops3)
