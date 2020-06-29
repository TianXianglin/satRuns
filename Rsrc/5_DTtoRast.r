source("Rsrc/settings.r")
source("Rsrc/functions.r")
setwd(generalPath)
if(!dir.exists("outRast")) {
  dir.create("outRast")
}
if(!dir.exists(paste0("outRast/",startingYear))) {
  dir.create(paste0("outRast/",startingYear))
}

load(paste0(procDataPath,startingYear,"/XYsegID.rdata"))
crsX <- crs(raster(baRast))

clims <- weather
mans <- harvscen

for(ij in yearOut){
  for(varX in varRast){
    createTifFromDT(clims, mans, ij, varX, layerDT, startingYear,XYsegID,crsX = crsX)
    print(varNames[varX])
  }
}



if(TRUE){
  fileDT=paste0("outDT/",startingYear,"/","startV_layertot.rdata")
  load(fileDT)
  setkey(XYsegID,segID)
  setkey(startV,segID)
  outXY <- merge(XYsegID,startV,all = T)
  ###remove coordinates NAs
  outXY <- outXY[!is.na(x)]
  
  ###create raster 
  rastX <- rasterFromXYZ(outXY[,c("x","y","value"),with=F])
  crs(rastX) <- crsX
  
  rastName <- paste0("outRast/",startingYear,"/","startV_startYear",startingYear,"_layertot.tif")
  writeRaster(rastX,filename = rastName,overwrite=T)
  
}
