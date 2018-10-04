

load("./rdata-data/data16_20180626.Rdata") #created in WorkshopDataManage.R
load("./rdata-data/data16_miss_20180827.Rdata")
str(data16)
str(data16_miss)

lake.conn.levels=levels(data16[,25])
lake.conn.levels_m = levels(data16_miss[,20])

## fix this later
data16$lakeconn_v2=as.numeric(data16$lakeconn)
data16_miss$lakeconn_v2 = as.numeric(data16_miss$lakeconn_v2)
data16_miss <- data16_miss[, -25] #remove huc factor

# determine response variables that are missing
idx.no.response = which(is.na(apply(data16[,c("tp","tn_combined","chla","secchi")],1,sum)))
#idx.no.response
# determine covariates that are missing
idx.na = which(is.na(apply(data16[,19:36],1,sum)))
idx.na.miss = which(is.na(apply(data16_miss[ ,14:31], 1, sum)))
#idx.na

# remove observations with missing covariates
data16=data16[-idx.na,]
str(data16)

data16_miss <- data16_miss[-idx.na.miss, ]
str(data16)
##
## making X matrix
##

X=data16[,c(19:24,26:36)]
x.lakeconn=model.matrix(~0+as.factor(data16[,25]))
colnames(x.lakeconn)=lake.conn.levels
head(x.lakeconn)

X=cbind(X,x.lakeconn[,-1]) ## DR_LakeStream is the baseline category
head(X)

X_miss <- data16_miss[, c(14:19, 21:31)]
x.miss.lakeconn <- model.matrix(~0+as.factor(data16_miss[, 20]))
colnames(x.miss.lakeconn) <- lake.conn.levels_m
head(x.miss.lakeconn)

X_miss <- cbind(X_miss, x.miss.lakeconn[, -1])
head(X_miss)

##
## making Y matrix
##

Y=data16[,c("tp","tn_combined","chla","secchi")]
summary(Y)
logY=log(Y+.1)


# determine which Y values are missing

y.inds <- !is.na(logY) 
# rownames(y.inds) <- c()

# all missing
all.missing <- which(apply(y.inds, 1, sum) == 0)


#save the all missing for prediction afterwarads
logY_miss <- logY[all.missing, ]
X_miss_2 <- X[all.missing, ]

X_miss_comb <- rbind(X_miss, X_miss_2) 

# remove all missing from logY, y.inds, and X
logY <- logY[-all.missing, ]
X <- X[-all.missing, ]



## add in the last two hold-out sets, created by Noah
lagoslakeid=data16$lagoslakeid
clust=read.csv("./csv-data/cluster_holdout.csv")
data16.clust=merge(data16,clust,by.x=1,by.y=1)

Holdout=data16.clust[-all.missing,c(14:18,38:39)]
Holdout_missing <- data16.clust[all.missing, c(14:18, 38:39)]

head(Holdout)
head(Holdout_missing)

AllDataList_180627=list()
AllDataList_180627$lagoslakeid=lagoslakeid[-all.missing]
AllDataList_180627$logY=logY
AllDataList_180627$X=X
AllDataList_180627$Holdout=Holdout
# save(AllDataList_180627,file="./rdata-data/AllDataList_180627v2.Rdata")

MissingDataList_100815 <- list()
MissingDataList_100815$lagoslakeid= c(lagoslakeid[all.missing], data16_miss$lagoslakeid)
MissingDataList_100815$logY=logY_miss
MissingDataList_100815$X=X_miss_comb
MissingDataList_100815$Holdout=Holdout_missing
save(MissingDataList_100815,file="./rdata-data/MissingDataList_180815.Rdata")



# load("AllDataList_180627.Rdata")
# 
# 
# head(clust)
# ls()
# 
# DD=AllDataList_180627
# str(DD)
# 
# id=DD$
