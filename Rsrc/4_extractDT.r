source("Rsrc/settings.r")
source("Rsrc/functions.r")

setwd(generalPath)
if(!dir.exists("outDT")) {
  dir.create("outDT")
}
if(!dir.exists(paste0("outDT/",startingYear))) {
  dir.create(paste0("outDT/",startingYear))
}

# for(clims in weather){
  clims <- weather
  mans <- harvscen
  # r_no <- 12
  # layerDT <- "tot" #1,2,3
  # startingYear <- c(2014,2019)
  for(stYear in startingYear){
    print(stYear)
    createDT(clims,"NoHarv",varDT,layerDT,stYear)
  }
  
  
########Need to clean data  
  # ###cleanData
  # sitesX <- numeric(0)
  # for(stYear in startingYear){
  #   for(vars in c(1:3,7,8,12:15)){
  #     varX <- varNames[varDT[vars]]
  #     fileX=paste0("outDT/",varX,"_",mans,"_",clims,"StartYear",stYear,"Sptot.rdata")
  #     load(fileX)
  #     siteNeg <- unique(which(get(varX)<0,arr.ind=T)[,1])
  #     siteNA <- unique(which(is.na(get(varX)),arr.ind=T)[,1])
  #     siteX <- union(siteNA,siteNeg)
  #     sitesX <- union(sitesX,siteX[!is.na(siteX)])
  #   }
  #   
  #   # get(varX)[sitesX[[r_no]]]
  #   sitesX <- as.integer(sitesX)
  #   for(vars in 1:length(varDT)){
  #     varX <- varNames[varDT[vars]]
  #     fileX=paste0("outputDT/",varX,"_",mans,"_",clims,"StartYear",stYear,"Sptot.rdata")
  #     load(fileX)
  #     set(get(varX),i=sitesX,j=1:3,value = NA)
  #     save(list=varX,file = fileX)
  #   }
  #   # get(varX)[sitesX[[r_no]]]
  #   # range(get(varX),na.rm=T)
  #   print(paste("cleaned year",stYear))
  # }
  # 
  
