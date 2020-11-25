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

for(varX in vars){
# for(runx in c("prior","2","posterior")){
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
       

library(ggplot2)
library(ggridges)

pH <- ggplot(H, 
              aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C") #+
  # labs(title = 'Correlation Coefficient distributions') 
pD <- ggplot(D, 
             aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C") #+
pB <- ggplot(B, 
             aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C") #+
pperP <- ggplot(perP, 
             aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C") #+
pperSP <- ggplot(perSP, 
             aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C") #+
pperB <- ggplot(perB, 
             aes(x = value, y = run, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "", option = "C") #+

save(pH,pD,pB,pperB,pperSP,pperP,file = paste0("plots/","init",startingYear,"/DA",year2,"/plots.rdata"))
#     
#     writeRaster(rastX,filename = rastName,overwrite=T)
#     print(varX)
#   }
