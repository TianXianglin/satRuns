library(devtools)
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/settings.r")
if(file.exists("localSettings.r")) {source("localSettings.r")} # use settings file from local directory if one exists

# Run functions 
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/functions.r")

###check and create output directories
###check and create output directories
setwd(generalPath)
mkfldr <- paste0("plots/",paste0("init",startingYear,"/DA",year2))
if(!dir.exists(file.path(generalPath, mkfldr))) {
  dir.create(file.path(generalPath, mkfldr), recursive = TRUE)
}


# yearX <- 3
# nSample = 1000 ###number of samples from the error distribution

vars <- c("H","D","B","perP","perSP","perB")

countx <- 0
for(varX in vars){
# for(runx in c("prior","2","posterior")){
  countx=countx+1
  runx="post"
  rastX <- raster(paste0("outRast/","init",startingYear,"/DA",year2,"/",varX,runx,".tif"))
  tabX <- data.table(value=getValues(rastX),run=runx)
  tab1 <- tabX[!is.na(value)]
  runx="prior"
  rastX <- raster(paste0("outRast/","init",startingYear,"/DA",year2,"/",varX,runx,".tif"))
  tabX <- data.table(value=getValues(rastX),run=runx)
  tab2 <- tabX[!is.na(value)]
  runx="2"
  rastX <- raster(paste0("outRast/","init",startingYear,"/DA",year2,"/",varX,runx,".tif"))
  tabX <- data.table(value=getValues(rastX),run=runx)
  tab3 <- tabX[!is.na(value)]
  assign(varX,rbind(tab1,tab2,tab3))
  rm(tab1,tab2,tab3);gc()
  print(varX)
}
       
load(paste0(procDataPath,"init",startingYear,"/DA",year2,"/allData.rdata"))         ### All data

print(str(data.all))

H <- rbind(H,data.table(value=data.all$h,run="1"))
print(str(H))
print(unique(H$run))
D <- rbind(D,data.table(value=data.all$dbh,run="1"))
B <- rbind(B,data.table(value=data.all$ba,run="1"))
perP <- rbind(perP,data.table(value=data.all$pineP,run="1"))
perSP <- rbind(perSP,data.table(value=data.all$spruceP,run="1"))
perB <- rbind(perB,data.table(value=data.all$blp,run="1"))
rm(data.all); gc()

test <- rbind(H[run=="post"][1:1000],H[run=="prior"][1:1000],H[run=="2"][1:1000],H[run=="1"][1:1000])
save(test,file="Htest.rdata")
library(ggplot2)
library(ggridges)

pH <- ggplot(H, 
              aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C") +
  labs(title = 'H') 
pD <- ggplot(D, 
             aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C")  +
  labs(title = 'D')
pB <- ggplot(B, 
             aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C")  +
  labs(title = 'B')
pperP <- ggplot(perP, 
             aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C")  +
  labs(title = '%cover pine')
pperSP <- ggplot(perSP, 
             aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C") +
  labs(title = '%cover spruce')
pperB <- ggplot(perB, 
             aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C") +
  labs(title = '%cover deciduous')

# save(pH,pD,pB,pperB,pperSP,pperP,file = paste0("plots/","init",startingYear,"/DA",year2,"/plots.rdata"))
#     
#     writeRaster(rastX,filename = rastName,overwrite=T)
#     print(varX)
#   }
# save plot to file without using ggsave

pdf("rplots.pdf")
pH
pD
pB
pperSP
pperP
pperB
dev.off()
# ggsave("H.pdf",plot=pH,device="pdf")
# ggsave("D.pdf",plot=pD,device="pdf")
# 
# ggsave("B.pdf",plot=pB,device="pdf")
# 
# ggsave("perP.pdf",plot=pperB,device="pdf")
# 
# ggsave("perSP.pdf",plot=pperSP,device="pdf")
# 
# 
# ggsave("perB.pdf",plot=pperB,device="pdf")
