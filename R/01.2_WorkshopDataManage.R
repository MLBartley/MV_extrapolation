lakes=read.csv("./csv-data/wq2_single.csv")
missing_lakes = read.csv("./csv-data/wq0_single.csv") #has no holdout information
local.pred=read.csv("./csv-data/local_predictors.csv")
reg.pred=read.csv("./csv-data/regional_predictors.csv")


## summary(lakes)
## summary(local.pred)
## summary(reg.pred)
## str(lakes)
## str(local.pred)
## str(reg.pred)

##
## compiling data frame with 16 predictors
##

data16=lakes
data16_miss <- missing_lakes
## removing troublesome data points
## some lakes are "out of HU4" but not listed in a huc

idx.out.of.hu4=which(levels(data16$hu4_zoneid)[data16$hu4_zoneid]=="OUT_OF_HU4")
idx.out.of.hu4
data16=data16[-idx.out.of.hu4,]

#not needed for missing

## some lakes do not have response variables

idx.no.response=which(is.na(apply(data16[,c("tp","tn_combined","chla","secchi")],1,sum)))
idx.no.response


n.lakes=nrow(data16)
n.lakes
length(idx.no.response)

n.lakes_miss <- nrow(data16_miss)
n.lakes_miss


##
## adding local predictors
##

local.preds=c("nhd_lat","nhd_long",
                  "maxdepth_m","IWS_lk_ratio","iws_nlcd2006_for","lake_sdf","lakeconn_v2",
                  "streamdensity_mperha","elevation_m","wlconnections_shoreline_km","buff_roaddensity_mperha")

idx.local.preds=integer()
for(i in 1:length(local.preds)){
    idx.local.preds[i]=which(colnames(local.pred)==local.preds[i])
}
idx.local.preds


idx.lake.local=integer()
idx.lake.missing.local <- integer()

for(i in 1:n.lakes){
    idx.lake.local[i]=which(local.pred$lagoslakeid == data16$lagoslakeid[i])
}

for (i in 1:n.lakes_miss) { 
  idx.lake.missing.local[i] <- which(local.pred$lagoslakeid == data16_miss$lagoslakeid[i])
}


local.preds.tmp=local.pred[idx.lake.local,idx.local.preds]
colnames(local.preds.tmp)=local.preds
data16=cbind(data16,local.preds.tmp)
head(data16)

local.preds.tmp <- local.pred[idx.lake.missing.local, idx.local.preds]
colnames(local.preds.tmp)<-local.preds
data16_miss <- cbind(data16_miss, local.preds.tmp)
head(data16_miss)

##
## adding regional predictors
##
names(reg.pred)
reg.preds=c("baseflow_mean","runoff_mean","hu4_nlcd2006_agr","totalNdep_1990_mean",
            "totalNdep_2010_mean","prism_ppt_mean","prism_tmean_mean")
idx.reg.preds=integer()
for(i in 1:length(reg.preds)){
    idx.reg.preds[i]=which(colnames(reg.pred)==reg.preds[i])
}
idx.reg.preds


huc.ids=unique(reg.pred$hu4_zoneid)
n.hucs=length(huc.ids)
reg.pred.unique=integer()
for(i in 1:n.hucs){
    idx=which(reg.pred$hu4_zoneid==huc.ids[[i]])
    reg.pred.unique=rbind(reg.pred.unique,cbind(huc.ids[i],reg.pred[idx[1],idx.reg.preds]))
}    
reg.pred.unique



idx.lake.reg=integer()
for(i in 1:n.lakes){
    idx.lake.reg[i]=which(levels(reg.pred.unique$huc.ids)==levels(data16$hu4_zoneid)[data16$hu4_zoneid[i]])
}

idx.lake.miss.reg=integer()
for(i in 1:n.lakes_miss){
  idx.lake.miss.reg[i]=which(levels(reg.pred.unique$huc.ids)==levels(data16_miss$hu4_zoneid)[data16_miss$hu4_zoneid[i]])
}

reg.preds=reg.pred.unique[idx.lake.reg,]
reg.preds.miss <- reg.pred.unique[idx.lake.miss.reg,]
data16=cbind(data16,reg.preds)
head(data16)

data16_miss <- cbind(data16_miss, reg.preds.miss)
head(data16_miss)
## doulbe check to make sure hucs are the same
cbind(data16$huc.ids,data16$hu4_zoneid)[1:100,]
## remove 
data16=data16[,-30]
head(data16)
# save(data16,file="./rdata-data/data16_20180626.Rdata")


save(data16_miss, file = "./rdata-data/data16_miss_20180827.Rdata")

## data17=data16

## load("data16_20180625.Rdata")
## head(data16)
## head(data17)
## names(data16)
## names(data17)
