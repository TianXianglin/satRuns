
### Run settings
source("Rsrc/settings.r")
setwd(generalPath)
###check and create output directories
setwd(generalPath)
mkfldr <- paste0("output/","init",startingYear,"/st",siteTypeX)
if(!dir.exists(file.path(generalPath, mkfldr))) {
  dir.create(file.path(generalPath, mkfldr), recursive = TRUE)
}

###load Processed data
load(paste0(procDataPath,"init",startingYear,"/","st",siteTypeX,"/samples.rdata"))

nSamples <- length(samples)
sampleIDs <- 1:nSamples
rm(samples); gc()

if(testRun){
  sampleID <- 1
  rcpfile="CurrClim"
}


for (rcpfile in weather) { ## ---------------------------------------------
  print(date())
  print(rcpfile)

  for(sampleID in sampleIDs){

    file2load <- paste0(initPrebasPath,"init",startingYear,"/",
                       "st",siteTypeX,"/",
                       rcpfile,"_sample",sampleID,".rdata")
    load(file2load)
    
    out <- multiPrebas(initPrebas)$multiOut[,,saveVars,,1]
    dimnames(out) <- list(sites=NULL,years=NULL,varX=varNames[saveVars],layer=1:3)
    v0 <- data.table(segID=initPrebas$siteInfo[,1],value=apply(initPrebas$multiOut[,1,30,,1],1,sum))
    file2save <- paste0(outPath,"init",startingYear,"/",
                        "st",siteTypeX,"/",
                        rcpfile,"_sample",sampleID,".rdata")
    save(out,v0,file=file2save)
    rm(initPrebas,out,v0); gc()
    print(sampleID)
  }
}
  
