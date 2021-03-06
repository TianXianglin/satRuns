# Run settings (if modifiedSettings is not set to TRUE in batch job script, default settings from Github will be used)
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/settings.r")
if(modifiedSettings) {
  source("/scratch/project_2000994/PREBASruns/assessCarbon/Rsrc/mainSettings.r") # in CSC
}

setwd(generalPath)
###check and create output directories
mkfldr <- paste0("output/","init",startingYear,"/st",siteTypeX)
if(!dir.exists(file.path(generalPath, mkfldr))) {
  dir.create(file.path(generalPath, mkfldr), recursive = TRUE)
}

###load Processed data
load(paste0(procDataPath,"init",startingYear,"/","st",siteTypeX,"/samples.rdata"))

#nSamples <- length(samples)
#sampleIDs <- 1:nSamples
#rm(samples); gc()

if(testRun){
  sampleID <- 1
  rcpfile="CurrClim"
}

#Processing time is measured with tictoc
tic("total time taken to run the model to sample data")

for (rcpfile in weather) { ## ---------------------------------------------
  print(date())
  print(rcpfile)

  # Run the model for sample data. Process data in parallel with mclapply command.
  # Number of cores used for processing can be defined with mc.cores argument. mc.cores=1 disables 
  # parallel processing. Recommended amount of cores to use with this code is 20.
  runModel <- mclapply(seq_along(samples), function(x) {
    data.sample <- samples[[x]]
    sampleID <- names(samples)[x]

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
  
  }, mc.cores = coresN)
}
  
toc()
