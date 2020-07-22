
#####Run settings####
source("Rsrc/settings.r")
setwd(generalPath)
mkfldr <- paste0("procData/",paste0("init",startingYear,"/st",siteTypeX))
if(!dir.exists(file.path(generalPath, mkfldr))) {
  dir.create(file.path(generalPath, mkfldr), recursive = TRUE)
}


###extract CurrClim IDs
rastX <- raster(baRast)
if(testRun){
  extNew <- extent(rastX)
  extNew[2]   <- (extent(rastX)[1] + (extent(rastX)[2] - extent(rastX)[1])*fracTest)
  extNew[4]   <- (extent(rastX)[3] + (extent(rastX)[4] - extent(rastX)[3])*fracTest)
  rastX <- crop(rastX,extNew)
  maxSitesRun <- maxSitesRunTest
}

climID <- raster(climIDpath)
# climIDx <- crop(climID,rastX)
# plot(climIDx)
# plot(rastX,add=T)
# climIDs <- resample(climIDx,rastX,method="ngb")
# writeRaster(climIDs,paste0(rasterPath,"climIDs.tif"),overwrite=T)
# climIDs <- raster(paste0(rastersPath,"climIDs.tif"))
rm(rastX)
gc()


fileNames <- c(baRast,
                blPerRast,
                dbhRast,
                vRast,
                hRast,
                pinePerRast,
                sprucePerRast,
                siteTypeRast,
                siteTypeRast2,
                vRast2)

for(i in 1:length(fileNames)){
  rastX <- raster(fileNames[i])
  if(testRun) rastX <- crop(rastX,extNew)    ####if it is a test run rasters are cropped to a smaller area
  dataX <- data.table(rasterToPoints(rastX))
  if(i==1){
    data.all <- dataX 
  }else{
    data.all <- merge(data.all,dataX)
  }
  print(fileNames[i])
}

###attach weather ID
data.all$climID <- extract(climID,data.all[,.(x,y)])
# dataX <- data.table(rasterToPoints(climIDs))
# data.all <- merge(data.all,dataX)
setnames(data.all,c("x","y","ba","blp","dbh","v","h","pineP","spruceP","siteType1","siteType2","v2","climID"))

##filter data 
data.all <- data.all[!ba %in% baNA]
data.all <- data.all[!blp %in% blPerNA]
data.all <- data.all[!dbh %in% dbhNA]
data.all <- data.all[!v %in% vNA]
data.all <- data.all[!v2 %in% vNA]
data.all <- data.all[!h %in% hNA]
data.all <- data.all[!pineP %in% pinePerNA]
data.all <- data.all[!spruceP %in% sprucePerNA]
data.all <- data.all[!siteType1 %in% siteTypeNA]
data.all <- data.all[!siteType2 %in% siteTypeNA]

####convert data to prebas units
data.all <- data.all[, ba := ba * baConv]
data.all <- data.all[, blp := blp * blPerConv]
data.all <- data.all[, dbh := dbh * dbhConv]
data.all <- data.all[, v := v * vConv]
data.all <- data.all[, v2 := v2 * vConv]
data.all <- data.all[, h := h * hConv]
data.all <- data.all[, pineP := pineP * pinePerConv]
data.all <- data.all[, spruceP := spruceP * sprucePerConv]
data.all <- data.all[, siteType1 := siteType1 * siteTypeConv]
data.all <- data.all[, siteType2 := siteType2 * siteTypeConv]

if(siteTypeX==year2){
  data.all[,siteType:=siteType2]  
}else{
  data.all[,siteType:=siteType1]  
}

#####I'm excluding from the runs the areas that have been clearcutted and have ba=0 
# data.all[h==0. & dbh==0 & ba==0,clCut:=1]
data.all[,clCut:=0]
data.all[ba==0,clCut:=1]

###calculate tree density
data.all[clCut==0,N:=ba/(pi*(dbh/200)^2)]

####check where H is below minimum initial height and replace
smallH <- intersect(which(data.all$h < initH), which(data.all$clCut==0))
data.all[smallH, h:=initH]

###check where density is too high and replase stand variables with initial conditions
tooDens <- intersect(which(data.all$N> maxDens), which(data.all$clCut==0))
data.all[tooDens,h:=initH]
data.all[tooDens,ba:=initBA]
data.all[tooDens,dbh:=initDBH]
data.all[tooDens,N:=initN]


data.all[pineP == 0 & spruceP == 0 & blp ==0 & siteType ==1, blp:=1  ]
data.all[pineP == 0 & spruceP == 0 & blp ==0 & siteType <= 3 & siteType > 1, spruceP:=1  ]
data.all[pineP == 0 & spruceP == 0 & blp ==0 & siteType >= 4, pineP:=1  ]

###!!!!!!!!!!!!########careful with this part##########!!!!!!!!#########

####calculate dV
data.all[,dV := v2-v]
data.all[,dVy := (v2-v)/(year2 - startingYear)]

####group pixels by same values
data.all[, segID := .GRP, by = .(ba, blp,dbh, h, pineP, spruceP, siteType, climID,dVy,v2)]
# data.all[clCut==1 ,hist(dVy)]


####Count segID pix
data.all[, npix:=.N, segID]

# uniqueData <- data.table()
####find unique initial conditions
uniqueData <- unique(data.all[clCut==0 & dVy >0,.(ba,blp,dbh,h,pineP,spruceP,
                                                  siteType,N,climID,segID,npix,dVy,v2)])
uniqueData[,uniqueKey:=1:nrow(uniqueData)]
setkey(uniqueData, uniqueKey)
# uniqueData[,N:=ba/(pi*(dbh/200)^2)]
range(uniqueData$N)
uniqueData[,area:=npix*resX^2/10000]

###assign ID to similar pixels
XYsegID <- data.all[,.(x,y,segID)]

###!!!!!!!!!!!!########end careful with this part##########!!!!!!!!#########

# nSamples <- ceiling(dim(uniqueData)[1]/20000)
# sampleID <- 1
# 
# for(sampleID in sampleIDs){
#   set.seed(1)
#   samplesX <- split(uniqueData, sample(1:nSample, nrow(uniqueData), replace=T))
#   sampleX <- ops[[sampleID]]
#   sampleX[,area := N*resX^2/10000]
#   # sampleX[,id:=climID]
# }
  

nSamples <- ceiling(dim(uniqueData)[1]/maxSitesRun)
set.seed(1)
sampleset <- sample(1:nSamples, nrow(uniqueData),  replace=T)
samples <- split(uniqueData, sampleset) 

# adding sampleID, sampleRow (= row within sample) 
uniqueData[,sampleID:=sampleset]
uniqueData[,sampleRow:=1:length(h),by=sampleID]

segID <- numeric(0)
for(i in 1:nSamples){
  sampleX <- samples[[i]]
  segID <- c(segID,sampleX$segID)
}

save(data.all,file=paste0(procDataPath,"init",startingYear,"/","st",siteTypeX,"/allData.rdata"))         ### All data
save(uniqueData,file=paste0(procDataPath,"init",startingYear,"/","st",siteTypeX,"/uniqueData.rdata"))    ### unique pixel combination to run in PREBAS
save(samples,file=paste0(procDataPath,"init",startingYear,"/","st",siteTypeX,"/samples.rdata"))    ### unique pixel combination to run in PREBAS
save(XYsegID,segID,file=paste0(procDataPath,"init",startingYear,"/","st",siteTypeX,"/XYsegID.rdata"))    ### Coordinates and segID of all pixels



