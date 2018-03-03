##########################################
##
## Read in data
##
##########################################


library(data.table)

###### LAGOS v. 1.087.1 data pull for sequential analysis paper
### Criteria
# 1: Include lakes from 1990 - 2011
# 2: Use epi data from summer stratified period (15 June - 15 September)
# 3: Take median nutrient value from each lake within a season (single value/season) and across years
# 4: If TN data don't exist, but TKH and N02N03 do - then calculate TN for those lakes as TKN + N02N03
# 5: Response variables: total phosphorus (TP) and total nitrogen (TN) - could do others as well...e.g., chlorphyll a


dat <- fread('propagate_uncert.csv')
dat
dat[,.N]

# Number of obs (lakes) for response variables total phosphorus and total nitrogen  These are the RESSPONSE VARIABLES OF INTEREST
dat[, sum(!is.na(median_tp))]
dat[, sum(!is.na(median_tn))]

str(dat)


#add shoreline development  SLD to characterize lake shape
#perimeter is meters, and area is in hectares. Convert area to square meters
dat$SDI = dat$lake_perim_meters/(2*sqrt(3.14*(dat$lake_area_ha*10000)))



##
## plot data
##

library(maps)
library(maptools)

x=dat$nhd_long
y=dat$nhd_lat
plot(x,y,col="blue",pch=".",cex=2)
m=map("state",add=T,lwd=2)

## try some simple models

id=dat$lagoslakeid
tp=dat$median_tp
tn=dat$median_tn
secchi=dat$median_secchi
area=dat$lake_area_ha
meandepth=dat$meandepth

DF=data.frame(id,x,y,tn,tp,secchi,area,meandepth)
str(DF)

na.idx=which(is.na(apply(DF,1,sum)))


library(mgcv)

## fits a varying coefficient model for secchi depth and mean depth
## nitrogen[i] = b0 + secchi[i] * b(x[i],y[i]) + meandepth[i] * c(x[i],y[i])
fit.nit=gam(log(tn)~s(x,y,by=secchi)+area+s(x,y,by=meandepth),data=DF[-na.idx,])
summary(fit.nit)

## plot of b(x,y)
plot(fit.nit,select=1)
## plot of c(x,y)
plot(fit.nit,select=2)


