#### MANAGEMENT MASK SETTINGS #####

# Run settings 
library(devtools)
source_url("https://raw.githubusercontent.com/ForModLabUHel/satRuns/master/Rsrc/settings.r")
if(file.exists("localSettings.r")) {source("localSettings.r")} # use settings file from local directory if one exists

ts <- T # trouble-shooting on/off

# set periods for declarations to be used
# note: submission 2 weeks -  3 years (!) prior to mgmt, no obligation to conduct declared mgmt
mm_startdate_tend <- "2014-07-01"
mm_startdate_cc <- "2014-07-01"
mm_enddate <- "2019-07-01"

mm_cc_buff <- 50 # m buffer around cc declarations, treated as polygons themselves
mm_tend_buff <- 25 # m buffer around tending declarations, treated as polygons themselves

mm_thresh <- 6 # threshold for growing stock change (dV) during modelling period (2016-2019) to be masked out within declaration polygons (m3/3a)

##  path for declaration input
declpath <- paste0("/scratch/project_2000994/PREBASruns/assessCarbon/data/mgmtmask/", areaID, "_", tileX, "_mkdecl.subset.gpkg")


#### READING DATA ####
# Growing stock rasters for 2016 and 2019
v16raw <- raster(paste0(rasterPath, areaID, "_", tileX, "-2016_GSV_10M_1CHS_16BITS.tif"))
v19raw <- raster(paste0(rasterPath, areaID, "_", tileX, "-2019_GSV_10M_1CHS_16BITS.tif"))

# read tileX-specific declaration dataset (preprocessed in qgis)
decl <- st_read(declpath)  


#### PROCESSING ####
# CALCULATE VOL CHANGE RASTER 
# remove NA dummy values (replace with NA)
rm_nadummies <- function(rast){return(ifelse(rast == 65533 |rast == 65534 |rast == 65535 , NA, rast))} # faster than %in% version (?)
v16 <- overlay(v16raw, fun=rm_nadummies)
v19 <- overlay(v19raw, fun=rm_nadummies)

if (ts) print(paste0("Tile ", tileX, ": na removal overlay ok"))

# calculate volume change
dV <- overlay(v16, v19, fun=function(v16, v19){return(v19-v16)})

if (ts) print(paste0("Tile ", tileX, ": dV overlay ok"))


# DECLARATION PROCESSING
# homogenise geometries
decl2 <- st_cast(decl, "MULTIPOLYGON")

# classify mgmt (preliminary, should be checked by someone a little more knowledgeable about Finnish forestry practices)
cc = c(4,5,8,16,19,21,22)
tend = c(1,2,3,6,7,12,13,20,23)
unspec = c(9,10,11,14,15,17,18)
decl2$mgmt <- ifelse(decl2$cuttingrealizationpractice %in% cc, "cc", ifelse(decl2$cuttingrealizationpractice %in% tend, "tend", ifelse(decl2$cuttingrealizationpractice %in% unspec, "unspec", NA)))

# might include empty geometries (cropping?) causing problems later on --> remove
decl3 <-  decl2[!st_is_empty(decl2),,drop=FALSE]

# pre-processing step: fasterize seems to have problems with mixing multipolygon and polygon geometries; casting to multipolygon fails, casting to polygon excludes all but first polygon of declarations with multiple polygons --> split data, then cast, merge
decl3_temp1<- decl3[st_geometry_type(decl3)=="POLYGON",] 
decl3_temp2<- decl3[st_geometry_type(decl3)=="MULTIPOLYGON",] 
decl3 <- rbind(decl3_temp1, st_cast(decl3_temp2, "POLYGON", warn=F))

# subsets for cc and tend/unspec
ss_cc <- decl3[decl3$mgmt=="cc",]
ss_tendunsp <- decl3[decl3$mgmt!="cc",]

# subset for period
ss_cc <- subset(ss_cc, ss_cc$declarationarrivaldate>=mm_startdate_cc & ss_cc$declarationarrivaldate<=mm_enddate)
ss_tendunsp <- subset(ss_tendunsp, ss_tendunsp$declarationarrivaldate>=mm_startdate_tend & ss_tendunsp$declarationarrivaldate<=mm_enddate)

# buffering
ss_cc_bfd <- st_buffer(ss_cc, dist=mm_cc_buff)
ss_tendunsp_bfd <- st_buffer(ss_tendunsp, dist=mm_tend_buff)

# rasterise polygons; mask=1, rest=NA (fasterize default)
cc_rast <- fasterize(ss_cc_bfd, dV)
ts_rast <- fasterize(ss_tendunsp_bfd, dV) 

if (ts) {
  print(paste0("Tile ", tileX, ": r(f)asterizing mgmt polygons ok"))
  print(paste0("ndV CRS:     ", crs(dV)))
  print(paste0("cc_rast crs: ", crs(cc_rast)))
  print(paste0("ts_rast crs: ", crs(ts_rast)))
  #print("starting reprojection")
}

# in CSC runs, the fasterized rasters' crs doesn't match dV's crs (does not happen when run locally)
#--> reproject (shouldn't change anything but the crs about the rasters in practice)
#cc_rast_reproj <- projectRaster(cc_rast, dV)
#ts_rast_reproj <- projectRaster(ts_rast, dV)

# if (ts) {
#   print(paste0("Tile ", tileX, ": reprojection / crs homogenisation done"))
#   print(paste0("ndV CRS:            ", crs(dV)))
#   print(paste0("cc_rast_reproj crs: ", crs(cc_rast_reproj)))
#   print(paste0("ts_rast_reproj crs: ", crs(ts_rast_reproj)))
#   print("starting mask overlay")
#   
# }

# building mask (mgmt = 1, none = 0, NAs matching those of input rasters (RasterToPoints consistency))
build_mm <- function(cc, tend, dv){return(ifelse(is.na(dv), NA, ifelse(!is.na(tend) & dv<=mm_thresh | !is.na(cc) & dv<=mm_thresh, 1, 0)))}
mgmtmask_rast <- overlay(projectRaster(cc_rast, dV), projectRaster(ts_rast, dV), dV, fun=build_mm)

if (ts) print(paste0("Tile ", tileX, ": final mgmtmask overlay ok, that was it!"))

# saving raster
writeRaster(mgmtmask_rast, file=paste0(rasterPath, areaID, "_", tileX, "_mgmtmask"), format="GTiff", overwrite=T)
