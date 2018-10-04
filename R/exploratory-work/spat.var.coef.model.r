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


dat <- fread('./csv-data/propagate_uncert.csv')
dat
dat[, .N]

# Number of obs (lakes) for response variables total phosphorus and total nitrogen  These are the RESSPONSE VARIABLES OF INTEREST
dat[, sum(!is.na(median_tp))]/nrow(dat)
dat[, sum(!is.na(median_tn))]/nrow(dat)
dat[, sum(!is.na(median_chla))]/nrow(dat)

str(dat)


#add shoreline development  SLD to characterize lake shape
#perimeter is meters, and area is in hectares. Convert area to square meters
dat$SDI = dat$lake_perim_meters / (2 * sqrt(3.14 * (dat$lake_area_ha * 10000)))



##
## plot data
##
library(brms)
library(itsadug)
library(mgcv)
library(ggplot2)
library(maps)
library(maptools)
library(magrittr)
library(visreg)

x = dat$nhd_long
y = dat$nhd_lat
plot(x, y, col = "blue", pch = ".", cex = 2)
m = map("state", add = T, lwd = 2)


states <- map_data("state") %>% 
  subset(region %in% c("minnesota", "iowa", "missouri", 
                      "wisconsin", "illinois", "michigan", 
                      "indiana", "ohio", "pennsylvania", 
                      "new jersey", "new york", "vermont", 
                      "new hampshire", "maine", "connecticut",
                     "rhode island", "massachusetts", "kentucky", 
                     "west virginia", "virginia", "maryland"))

ll <- ggplot(dat, aes(x = nhd_long, y = nhd_lat, color = log(median_tp))) +
        scale_color_gradientn(colours = terrain.colors(15), name = "log(Median P)") +
        # scale_colour_gradient(low = "white", high = "black") +
        geom_point(size = .5) +
        # scale_size_continuous(range = c(.001, 1)) +
        geom_polygon(data = states, aes(x = long, y = lat, group = group),
                    fill = NA, color = "black") + 
        coord_fixed(1.3) +
        theme(text = element_text(size=14),
             panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
             panel.background = element_blank(), 
             axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(), 
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank())
ll

ggsave("tp_bystate.pdf")

## try some simple models

id = dat$lagoslakeid
tp = dat$median_tp
tn = dat$median_tn
secchi = dat$median_secchi
area = dat$lake_area_ha
meandepth = dat$meandepth

# variables added by MLB
tchla = dat$median_chla
SDI <- dat$SDI
iws_ag <- dat$iws_ag
iws_tfor <- dat$iws_total_forest
iws_twet <- dat$iws_total_wetland
hu4_runoff <- dat$hu4_runoff_mean
streamdensity <- dat$iws_streamdensity_streams_density_mperha
WSA2LSA <- dat$wa_la
### MLB: any other variables to try? SDI? 


DF = data.frame(id, x, y, tn, tp, tchla, secchi, area, 
                iws_ag, iws_tfor, iws_twet, 
                hu4_runoff, streamdensity, 
                WSA2LSA)
str(DF)

na.idx = which(is.na(apply(DF, 1, sum)))

DF_tp = data.frame(tchla, id, x, y, tp, secchi, area, 
                iws_ag, iws_tfor, iws_twet, 
                hu4_runoff, streamdensity, WSA2LSA)

DF_tp <- DF_tp[which(DF_tp$tp != 0), ]

na.tp <- which(is.na(apply(DF_tp, 1, sum)))

## fits a varying coefficient model for secchi depth and mean depth
## nitrogen[i] = b0 + secchi[i] * b(x[i],y[i]) + iws_agg[i] * c(x[i],y[i]) + meandepth[i] * (x[i], y[i])
fit.nit = gam(sqrt(tn) ~ s(x, y, by = secchi, k = 65) + 
                s(x, y, by = iws_ag, k = 80) + 
                 # s(x, y, by = meandepth) + 
                s(x, y, by = iws_tfor, k = 80, sp = .05) + 
                s(x,  y, by = iws_twet, k = 85), 
              data = DF[-na.idx, ], method = "REML")
summary(fit.nit)

rsd <- residuals(fit.nit)
qq.gam(fit.nit,rep=100)

gam.check(fit.nit)

concurvity(fit.nit)

## plot of b(x,y)
plot.gam(fit.nit, select = 1)
m = map("state", add = T, lwd = 2)

vis.gam(fit.nit, plot.type = 'contour')
m = map("state", add = T, lwd = 2)

## plot of c(x,y)
plot(fit.nit, select = 2)
m = map("state", add = T, lwd = 2)

## plot of d(x, y)
plot(fit.nit, select = 3)
m = map("state", add= T, lwd = 2)



## fits a varying coefficient model for secchi depth and mean depth
## phosphorous[i] = b0 + secchi[i] * b(x[i],y[i]) + meandepth[i] * c(x[i],y[i])
fit.phos = gam(log(tp) ~ s(x, y, k = 100) +
                 s(x, y, by = secchi) +
                 # area +
                 # s(x, y, by = meandepth) + 
                 s(x, y, by = iws_ag) + 
                 s(x, y, by = iws_tfor) + 
                 # s(x, y, by = iws_twet) + 
                 # s(x, y, by = hu4_runoff) +
                 s(x, y, by = streamdensity) + 
                 s(x, y, by = WSA2LSA), 
              data = DF_tp[-na.tp,  ], family=gaussian(link="log"))
summary(fit.phos)


qq.gam(fit.phos,rep=100)

gam.check(fit.phos)

concurvity(fit.phos)

tp.missing <- dat[which(is.na(dat$median_tp)), ]
x.tp.missing <- tp.missing$nhd_long
y.tp.missing <- tp.missing$nhd_lat

tp.observed <- dat[which(!is.na(dat$median_tp)), ]
x.tp.obs <- tp.observed$nhd_long
y.tp.obs <- tp.observed$nhd_lat

# pdf("vis.gam.pdf",width=14,height=10,paper='special') 

vis.gam(fit.phos, view = c("x", "y"), plot.type = "contour")
points(x.tp.missing, y.tp.missing, 
       col = "black", pch = ".", cex = 3)
m = map("state", add = T, lwd = 2)
# dev.off()

## plot of b(x,y)

par(mfrow = c(1, 1))
pvisgam(fit.phos, select = 1,
        view = c("x", "y"), main="s(x, y)", 
        contour.col = "darkred")
points(x.tp.missing, y.tp.missing, 
       col = "black", pch = ".", cex = 3)
m = map("state", add = T, lwd = 2)


pvisgam(fit.phos, select = 2,
        view = c("x", "y"), main="s(x, y):secchi", 
        contour.col = "darkred")
points(x.tp.missing, y.tp.missing, 
       col = "black", pch = ".", cex = 3)
m = map("state", add = T, lwd = 2)

pvisgam(fit.phos, select = 3,
        view = c("x", "y"), main="s(x, y):iws_ag", 
        contour.col = "darkred")
points(x.tp.missing, y.tp.missing, 
       col = "black", pch = ".", cex = 3)
m = map("state", add = T, lwd = 2)

pvisgam(fit.phos, select = 4,
        view = c("x", "y"), main="s(x, y):iws_tfor", 
        contour.col = "darkred")
points(x.tp.missing, y.tp.missing, 
       col = "black", pch = ".", cex = 3)
m = map("state", add = T, lwd = 2)

pvisgam(fit.phos, select = 4,
        view = c("x", "y"), main="s(x, y):iws_tfor", 
        contour.col = "darkred")
points(x.tp.obs, y.tp.obs, 
       col = "black", pch = ".", cex = 3)
m = map("state", add = T, lwd = 2)


pvisgam(fit.phos, select = 5,
        view = c("x", "y"), main="s(x, y):streamdensity", 
        contour.col = "darkred")
points(x.tp.missing, y.tp.missing, 
       col = "black", pch = ".", cex = 3)
m = map("state", add = T, lwd = 2)

pvisgam(fit.phos, select = 6,
        view = c("x", "y"), main="s(x, y):WSA2LSA", 
        contour.col = "darkred")
points(x.tp.missing, y.tp.missing, 
       col = "black", pch = ".", cex = 3)
m = map("state", add = T, lwd = 2)

## fits a varying coefficient model for secchi depth and mean depth
## nitrogen[i] = b0 + secchi[i] * b(x[i],y[i]) + meandepth[i] * c(x[i],y[i])
fit.chla = gam(log(tchla) ~ s(x, y, by = secchi)  + s(x, y, by = meandepth), 
              data = DF[-na.idx, ])
summary(fit.chla)

## plot of b(x,y)
plot(fit.chla, select = 1)
m = map("state", add = T, lwd = 2)

## plot of c(x,y)
plot(fit.chla, select = 2)
m = map("state", add = T, lwd = 2)


