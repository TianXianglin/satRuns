
### Run settings
source("Rsrc/settings.r")
setwd(generalPath)
if(!dir.exists("output")) {
  dir.create("output")
}
if(!dir.exists(paste0("output/",startingYear))) {
  dir.create(paste0("output/",startingYear))
}

###load Processed data
load(paste0(procDataPath,startingYear,"/samples.rdata"))
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

    load(paste0(initPrebasPath,startingYear,"/",rcpfile,"_sample",sampleID,".rdata"))
    out <- multiPrebas(initPrebas)$multiOut[,,saveVars,,1]
    dimnames(out) <- list(sites=NULL,years=NULL,varX=varNames[saveVars],layer=1:3)
    v0 <- data.table(segID=initPrebas$siteInfo[,1],value=apply(initPrebas$multiOut[,1,30,,1],1,sum))
    save(out,v0,file=paste0(outPath,startingYear,"/",rcpfile,"_sample",sampleID,".rdata"))
    rm(initPrebas,out,v0); gc()
    print(sampleID)
  }
}
  
