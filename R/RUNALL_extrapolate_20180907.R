### extrapolation work carried over/cleaned up from RUNALL_extrapolate_20180810.R
library(dplyr)
library(forcats)
library(ggplot2)
library(maps)
library(magrittr)

load("./rdata-data/runAll_180808.Rdata") 

source("./R/RUNALL_eda_20180810.R") #gets leverage points and influential points
source("./R/extrapolate_functions.R") #gets useful functions

X <- rbind(runALL$data$X, runALL$data$Xtest)
LL <- rbind(runALL$data$LLxtrain, runALL$data$LLxtest)


Y <- as.data.frame(rbind(runALL$data$Y, runALL$data$Ytest)) #doesn't have full 'missing' Y length 
Y <- Y %>% mutate(missing = apply(!is.na(Y), 1, sum))


Beta <- runALL$beta
Sigma <- apply(runALL$Sigma, c(1,2), 'median')

Sampled <- 1:nrow(runALL$data$X)
rm(runALL)
 
Leverage <- leveragePoints[[1]] 

rm(leveragePoints, leverage_cutoff, hat, K, n, n.holdout, i)

LAGOS_extrapolate <- extrapolate(X = X, Beta = Beta, Sigma = Sigma,
                                 Sampled = Sampled, Leverage = Leverage,
                                 link = 'none')

save(LAGOS_extrapolate, file = "./rdata-data/extrap_full")

## Visualize

plot(LAGOS_extrapolate$trace,  ylim = c(0, .03),
     col = (LAGOS_extrapolate$extrapolate$EC_95quantile[1, ])+1)

plot(LAGOS_extrapolate$determ * 10^15, ylim = c(-0.0001, 0.0001))

## Spatial Visualize

## want to color dots by Full/Partial/Missing Y data
x = LL$nhd_long
y = LL$nhd_lat
miss <- c(Y$missing, rep(0, 723 - 113))
miss <- as.factor(miss)
miss.f <- fct_collapse(miss, full = c("4"), partial = c("1", "2", "3"), missing = c("0"))

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

gg_dat <- gg_dat[[1]]


# maps of full, missing, parital data locations

datamiss <-  ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = miss.f)) +
  scale_colour_manual(name = "Data Status",values = missColors[1:3]) +
  geom_point(size = 2.5) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = NA, color = "black") + 
  coord_fixed(1.3) +
  theme(text = element_text(size=80),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
datamiss

ggsave("figures/datamissmap.pdf", 
       width = 860, height = 573,
       units = "mm")

mt <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(max_trace))) +
  scale_colour_manual(name = "ivh",values = myColors[1:2]) +
  geom_point(size = .5) +
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
mt

# ggsave("tp_bystate.pdf")

md <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(max_determ))) +
  scale_colour_manual(name = "ivh",values = myColors[1:2]) +
  geom_point(size = .5) +
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
md

# ggsave("tp_bystate.pdf")

mdn <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(max_determ_noise))) +
  scale_colour_manual(name = "ivh",values = myColors[1:2]) +
  geom_point(size = .5) +
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
mdn

# ggsave("tp_bystate.pdf")

##cutoff: -leverage max

lmt <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(levmax_trace))) +
  scale_colour_manual(name = "ivh",values = myColors[1:2]) +
  geom_point(size = .5) +
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
lmt

# ggsave("tp_bystate.pdf")

lmd <- ggplot(na.omit(gg_dat), aes(x = nhd_long, y = nhd_lat, 
                                   color = factor(levmax_determ))) +
  scale_colour_manual(name = "Extraplation",
                      values = c("#E41A1C","#A6CEE3")) +
  geom_point(size = .5) +
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
lmd

ggsave("figures/levmax_extrap.pdf")

lmdn <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(levmax_determ_noise))) +
  scale_colour_manual(name = "ivh",values = myColors[c(2, 1)]) +
  geom_point(size = .5) +
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
lmdn

# ggsave("tp_bystate.pdf")


## cutoff: 95%

nft <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(nf_trace))) +
  scale_colour_manual(name = "ivh",values = myColors[1:2]) +
  geom_point(size = .5) +
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
nft

# ggsave("tp_bystate.pdf")

nfd <- ggplot(na.omit(gg_dat), aes(x = nhd_long, y = nhd_lat, color = factor(nf_determ))) +
  scale_colour_manual(name = " ",
                      values = c("#E41A1C","#A6CEE3"),
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction")) +
  geom_point(size = 2) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = NA, color = "black") + 
  coord_fixed(1.3) +
  theme(text = element_text(size=80),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
nfd

 ggsave("figures/nfdeterm_extrap.pdf", 
        width = 860, height = 573,
        units = "mm")

nfdn <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(nf_determ_noise))) +
  scale_colour_manual(name = "ivh",values = myColors[1:2]) +
  geom_point(size = .5) +
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
nfdn

# ggsave("tp_bystate.pdf")

## cutoff: 99%

nnt <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(nn_trace))) +
  scale_colour_manual(name = "ivh",values = myColors[1:2]) +
  geom_point(size = .5) +
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
nnt

# ggsave("tp_bystate.pdf")

nnd <- ggplot(na.omit(gg_dat), aes(x = nhd_long, y = nhd_lat, color = factor(nn_determ))) +
  scale_colour_manual(name = "Extrapolation",
                      values = c("#E41A1C","#A6CEE3"),  
                      breaks=c("0", "1"),
                      labels=c("Extrapolation", "Prediction")) +
  geom_point(size = 2) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = NA, color = "black") + 
  coord_fixed(1.3) +
  theme(text = element_text(size=80),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
nnd

# ggsave("tp_bystate.pdf")

nndn <- ggplot(gg_dat, aes(x = nhd_long, y = nhd_lat, color = factor(nn_determ_noise))) +
  scale_colour_manual(name = "ivh",values = myColors[1:2]) +
  geom_point(size = .5) +
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
nndn

# ggsave("tp_bystate.pdf")
rm(gg_dat)
