# Run settings (if modifiedSettings is not set to TRUE in batch job script, default settings from Github will be used)
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/settings.r")
if(modifiedSettings) {
  source("/scratch/project_2000994/PREBASruns/assessCarbon/Rsrc/mainSettings.r") # in CSC
}
# Run functions 
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/functions.r")

# Check and create output directories
setwd(generalPath)
mkfldr <- paste0("outRast/","init",startingYear,"/st",siteTypeX)
if(!dir.exists(file.path(generalPath, mkfldr))) {
  dir.create(file.path(generalPath, mkfldr), recursive = TRUE)
}
 
load(paste0(procDataPath,"init",startingYear,"/","st",siteTypeX,"/XYsegID.rdata"))  
crsX <- crs(raster(baRast))

clims <- weather
mans <- harvscen

for(ij in yearOut){
  for(varX in varRast){
    createTifFromDT(clims, mans, ij, varX, layerDT, startingYear,XYsegID,crsX = crsX)
    print(varNames[varX])
  }
}



if(startingYear==siteTypeX){
  fileDT=paste0("outDT/","init",startingYear,"/","startV_layerall.rdata")
  load(fileDT)
  setkey(XYsegID,segID)
  setkey(startV,segID)
  outXY <- merge(XYsegID,startV,all = T)
  ###remove coordinates NAs
  outXY <- outXY[!is.na(x)]
  
  ###create raster 
  rastX <- rasterFromXYZ(outXY[,c("x","y","value"),with=F])
  crs(rastX) <- crsX
  
  rastName <- paste0("outRast/","init",startingYear,"/","startV_startYear",startingYear,"_layerall.tif")
  writeRaster(rastX,filename = rastName,overwrite=T)
  
}
