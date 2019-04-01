library(ggplot2)
library(ggpubr)
library("gridExtra")
library(maps)


load(file = "./rdata-data/extrap_full") #created in 04.1
load(file = "./rdata-data/list-into-extrap.Rdata") #created in 04.1
list2env(savelist,globalenv())


## Spatial Visualize

## want to color dots by Full/Partial/Missing Y data
x = LL$nhd_long
y = LL$nhd_lat
# miss <- c(Y$missing, rep(0, 723 - 113))

miss <- as.factor(Y$missing)
miss.f <- forcats::fct_collapse(miss, full = c("4"), 
                       partial = c("1", "2", "3"), missing = c("0"))

plot(x, y, col = miss.f, pch = ".", cex = 2)
m = map("state", add = T, lwd = 2)


states <- map_data("state") %>% 
  subset(region %in% c("minnesota", "iowa", "missouri", 
                       "wisconsin", "illinois", "michigan", 
                       "indiana", "ohio", "pennsylvania", 
                       "new jersey", "new york", "vermont", 
                       "new hampshire", "maine", "connecticut",
                       "rhode island", "massachusetts", "kentucky", 
                       "west virginia", "virginia", "maryland"))
gg_dat <- list()

gg_dat[[1]] <- cbind(LL, (t(LAGOS_extrapolate$extrapolate$EC_max)),
                     (t(LAGOS_extrapolate$extrapolate$EC_levmax)),
                     (t(LAGOS_extrapolate$extrapolate$EC_95quantile)), 
                     (t(LAGOS_extrapolate$extrapolate$EC_99quantile)))
colnames(gg_dat[[1]])[3:8] <- c("max_trace", "max_determ", "max_determ_noise",
                                "max_trace_n", "max_determ_n", "max_determ_noise_n" ) 
colnames(gg_dat[[1]])[9:14] <- c("levmax_trace", "levmax_determ", "levmax_determ_noise", 
                                 "levmax_trace_n", "levmax_determ_n", "levmax_determ_noise_n") 
colnames(gg_dat[[1]])[15:20] <- c("nf_trace", "nf_determ", "nf_determ_noise", 
                                  "nf_trace_n", "nf_determ_n", "nf_determ_noise_n") 
colnames(gg_dat[[1]])[21:26] <- c("nn_trace", "nn_determ", "nn_determ_noise", 
                                  "nn_trace_n", "nn_determ_n", "nn_determ_noise_n") 

gg_dat[[2]] <- LAGOS_extrapolate$cutoffs

 # save(gg_dat, file = "./rdata-data/extrap_values_20180927")

#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(3,"Paired")
names(myColors) <- levels(gg_dat$determ)

missColors <- brewer.pal(3, "Dark2")
names(missColors) <- levels(miss.f)

gg_dat_pred <- gg_dat[[1]][-Sampled,]

gg_dat <- cbind(gg_dat[[1]], Y)




## common arguments for following plots - only have to alter here
th.paper <- theme(text = element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  panel.background = element_blank(), 
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(), 
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())

th.poster <- theme(text = element_text(size=80),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), 
                   axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(), 
                   axis.title.y=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())

pt.size.paper <- geom_point(size = .4)

states.lines <- geom_polygon(data = states, aes(x = long, y = lat, group = group),
                             fill = NA, color = "black")


# maps of full, missing, parital data locations
datamiss <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = miss.f)) +
  scale_colour_manual(name = "Data Status",values = missColors[1:3]) +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper

datamiss

# ggsave("figures/datamisssingle.tiff",
               # width = 860, height = 573,
               # units = "mm")

## location subset

# gg_subset <- gg_dat %>% 
#               dplyr::select("nhd_lat", "nhd_long", "tp", "tn_combined", "chla", "secchi") %>% 
#               dplyr::filter(nhd_long > -73.3, nhd_lat > 42.7)

gg_subset <- gg_dat[, c(1, 2, 27, 28, 29, 30)] %>% 
  dplyr::filter(nhd_long > -73.3, nhd_lat > 42.7)

states_subset <- map_data("state") %>% 
  subset(region %in% c( "vermont","new hampshire", "maine"))

states.lines_subset <- geom_polygon(data = states_subset, aes(x = long, y = lat, group = group),
                             fill = NA, color = "black")


##TN only miss

TNmiss <- ggplot(gg_subset, 
                 aes(x = nhd_long,
                     y = nhd_lat, 
                     color = forcats::fct_collapse(as.factor(is.na(tn_combined)), 
                                          full = c("FALSE"), 
                                          missing = c("TRUE")))) +
  scale_colour_manual(name = " ", 
                      values = missColors[1:3]) +
  pt.size.paper + 
  states.lines_subset + 
  coord_fixed(1.3) +
  th.paper

TNmiss

##Ch-a only miss
Cmiss <- ggplot(gg_subset, 
                 aes(x = nhd_long,
                     y = nhd_lat, 
                     color = forcats::fct_collapse(as.factor(is.na(chla)), 
                                          full = c("FALSE"), 
                                          missing = c("TRUE")))) +
  scale_colour_manual(name = " ", 
                      values = missColors[1:3]) +
  pt.size.paper + 
  states.lines_subset + 
  coord_fixed(1.3) +
  th.paper

Cmiss

## TP only miss

TPmiss <- ggplot(gg_subset, 
                 aes(x = nhd_long,
                     y = nhd_lat, 
                     color = forcats::fct_collapse(as.factor(is.na(tp)), 
                                          full = c("FALSE"), 
                                          missing = c("TRUE")))) +
  scale_colour_manual(name = " ", 
                      values = missColors[1:3]) +
  pt.size.paper + 
  states.lines_subset + 
  coord_fixed(1.3) +
  th.paper

TPmiss

## Secchi only miss

Smiss <- ggplot(gg_subset, 
                 aes(x = nhd_long,
                     y = nhd_lat, 
                     color = forcats::fct_collapse(as.factor(is.na(secchi)), 
                                          full = c("FALSE"), 
                                          missing = c("TRUE")))) +
  scale_colour_manual(name = " ", 
                      values = missColors[1:3]) +
  pt.size.paper + 
  states.lines_subset + 
  coord_fixed(1.3) +
  th.paper

Smiss



ggarrange(TNmiss, TPmiss, Cmiss, Smiss,  
          labels = c("A. Total Nitrogen", 
                     "B. Total Phosphorous", 
                     "C. Chlorophyl-a", 
                     "D. Secchi Disk"),
          ncol = 2, nrow = 2, 
          common.legend = TRUE,
          legend = "bottom")


# ggsave("figures/datamissmap.tiff",
#        width = 860, height = 573,
#        units = "mm")

mt <- ggplot(na.omit(gg_dat_pred), 
             aes(x = nhd_long, y = nhd_lat, 
                 color = factor(max_trace))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C", "#A6CEE3"),
                      limits=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
mt

# ggsave("tp_bystate.pdf")

md <- ggplot(na.omit(gg_dat_pred), aes(x = nhd_long, y = nhd_lat, 
                                  color = factor(max_determ))) +
  scale_colour_manual(name = " ",
                      values = c("#A6CEE3", "#E41A1C"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper 
md

# ggsave("tp_bystate.pdf")

# mdn <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, 
#                           color = factor(max_determ_noise))) +
#   scale_colour_manual(name = "Extraplation",
#                       values = c("#E41A1C","#A6CEE3")) + 
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# mdn

# ggsave("tp_bystate.pdf")

##cutoff: -leverage max

lmt <- ggplot(na.omit(gg_dat_pred), 
              aes(x = nhd_long, y = nhd_lat, 
                  color = factor(levmax_trace))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  +  
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
lmt

# ggsave("tp_bystate.pdf")

lmd <- ggplot(na.omit(gg_dat), 
              aes(x = nhd_long, y = nhd_lat, 
                  color = factor(levmax_determ))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
lmd

# ggsave("figures/levmax_extrap.pdf")

# lmdn <- ggplot(na.omit(gg_dat), 
#                aes(x = nhd_long, y = nhd_lat, 
#                    color = factor(levmax_determ_noise))) +
#                scale_colour_manual(name = " ",
#                       values = c("#E41A1C","#A6CEE3"),
#                       breaks=c("0", "1"),
#                       labels=c("Extrapolation", "Prediction"))  + 
#                 pt.size.paper + 
#                 states.lines + 
#                 coord_fixed(1.3) +
#                 th.paper
# lmdn

# ggsave("tp_bystate.pdf")


## cutoff: 95%

nft <- ggplot(na.omit(gg_dat_pred), 
              aes(x = nhd_long, y = nhd_lat, 
                  color = factor(nf_trace))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
nft

# ggsave("tp_bystate.pdf")

nfd <- ggplot(na.omit(gg_dat), aes(x = nhd_long, y = nhd_lat, 
                                   color = factor(nf_determ))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction")) +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
nfd

# ggsave("figures/nfdeterm_extrap.pdf", 
# # width = 860, height = 573,
# units = "mm")

# nfdn <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(nf_determ_noise))) +
#   scale_colour_manual(name = "Extraplation",
#                       values = c("#E41A1C","#A6CEE3")) +  
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# nfdn

# ggsave("tp_bystate.pdf")

## cutoff: 99%

nnt <- ggplot(na.omit(gg_dat_pred), 
              aes(x = nhd_long, y = nhd_lat, 
                  color = factor(nn_trace))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction"))  + 
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
nnt

# ggsave("tp_bystate.pdf")

nnd <- ggplot(na.omit(gg_dat), aes(x = nhd_long, y = nhd_lat,
                                   color = factor(nn_determ))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction")) +
  pt.size.paper + 
  states.lines + 
  coord_fixed(1.3) +
  th.paper
nnd

# ggsave("tp_bystate.pdf")

# nndn <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(nn_determ_noise))) +
#   scale_colour_manual(name = "ivh",values = myColors[1:2]) +
#   pt.size.paper + 
#   states.lines + 
#   coord_fixed(1.3) +
#   th.paper
# nndn

# ggsave("tp_bystate.pdf")
rm(gg_dat)



ggarrange(mt, lmt, nnt, nft,  
          labels = c("A. Maximum Cutoff", 
                     "B. Leverage Cutoff", 
                     "C. 99% Cutoff", 
                     "D. 95% Cutoff"),
          ncol = 2, nrow = 2, 
          common.legend = TRUE,
          legend = "bottom")

ggsave("./figures/Fig2.eps", plot = last_plot(), device = "eps", path = NULL,
       scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)


# ggarrange(md, lmd, nnd, nfd,  
#           labels = c("A. Maximum Cutoff", 
#                      "B. Leverage Cutoff", 
#                      "C. 99% Cutoff", 
#                      "D. 95% Cutoff"),
#           ncol = 2, nrow = 2, 
#           common.legend = TRUE, 
#           legend = "bottom")
# 
# ggsave("./figures/Fig3.eps", plot = last_plot(), device = "eps", path = NULL,
#        scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"),
#        dpi = 300, limitsize = TRUE)
