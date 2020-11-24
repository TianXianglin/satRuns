setwd("C:/Users/minunno/Documents/research/assessCarbon/data/Finland/AC_training_FI_35VLJ/")
load("pMvn_FSV_split1.rdata")
pMvNorm <- data.table(pMvNorm)
pMvNormAll <- pMvNorm
load("pMvn_FSV_split2.rdata")
pMvNorm <- data.table(pMvNorm)
pMvNormAll <- rbind(pMvNormAll,pMvNorm)

load("XYsegID.rdata")

funx <- function(id,datax){
  vecX <- as.numeric(c(id,unlist(datax[segID==id,2])))
  return(vecX)
}

funx(23,pMvNorm)

ciao <- data.table()
ix <- 0
for(i in unique(pMvNorm$segID)){
  ciao[ix] <- funx(i,pMvNorm)
  ix=ix+1
  if(ix %% 1000 == 0) print(ix)
}



pMvNorm$varNam <- rep(
  c("H","D","B","perP","perSP","perB",paste0("varcov1_",1:36),
                      "H2","D2","B2","perP2","perSP2","perB2",paste0("varcov2_",1:36),
                      "Hpost","Dpost","Bpost","perPpost","perSPpost","perBpost",paste0("varcov3_",1:36)),
                        times = nrow(pMvNorm)/126)
ops <- data.table(dcast(data = pMvNorm,
                formula = segID~varNam,value.var = "V1"))
