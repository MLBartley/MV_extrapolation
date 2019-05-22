###############################################################################
## This script aims to visualize the concept of extrapolation using LAGOS data.
## This "Patriotic Butterfly" plot is inspired by the work of Filstrup et al. 
##
## Created: January 23, 2019
## Updated 1: 
###############################################################################

# Outline
    ## get centered log values of Chl-a and P
    ## plot relationship
    ## choose subset of data to fit w/ linear model
    ## different color/shape of model fit/prediction data points
    ## add CI curves
    ## shade areas of extrapolation

library(ggplot2)
library(magrittr)
library(dplyr)


## get values of Chl-a and P

load(file = "./rdata-data/list-into-extrap.Rdata")

    ## data should already be log(data + 1)??
data <- na.omit(as.data.frame(cbind(savelist$Y$tp, savelist$Y$chla)))
data_cov <- na.omit(cbind(savelist$Y[, c(1, 3)], savelist$X))

data_scaled <- as.data.frame(scale(data))
datacov_scaled <- as.data.frame(scale(data_cov))
## plot relationship

aa <- ggplot(data_scaled, aes(x=V1, y=V2)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_smooth(color="darkred")

cov <- ggplot(datacov_scaled, aes(x=tp, y=chla)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_smooth(color="darkred")


## choose subset of data to fit w/ linear model

data_scaled <- data_scaled %>% 
               mutate(subset = ifelse(V1 < -2, "Prediction Locations", 
                                      ifelse(V1 > 2, "Prediction Locations", 
                                             "In-Sample Data")))

bb <- ggplot(data_scaled, aes(x=V1, y=V2, 
                              col = subset, 
                              shape = subset)) +
  geom_point() +
  geom_smooth(data = data_scaled[which(data_scaled$subset=="In-Sample Data"),], 
              method=lm, fullrange = T, aes(fill=subset)) 
  # geom_smooth(data =data_scaled)


## shade areas of extrapolation

    ## cutoff value for hull
X <- data_scaled$V1[which(data_scaled$subset =="In-Sample Data")]
hat <- X %*% solve(t(X) %*% X) %*% t(X)

max_diag_hat <- max(diag(hat))
max_diag_hat

X_new <- data_scaled$V1[which(data_scaled$subset == "Prediction Locations")]

v <- vector()
hull <- vector()

for(i in 1:length(X_new)){
v[i] <- X_new[i] *  solve(t(X) %*% X) * t(X_new[i])

hull[i] <- ifelse(v[i] <= max_diag_hat, "in", "out")
    
}


max_diag_hat <- max(diag(hat))
max_diag_hat

#from WA
shade <- 1.99879

    ## add the shading 
cc <- ggplot(data_scaled, aes(x=V1, y=V2, 
                             col = subset, 
                             shape = subset)) +
  geom_point() +
  scale_colour_manual(name = " ",
                      values = c("#A6CEE3", "#E41A1C"), 
                      breaks=c("Prediction Locations", "In-Sample Data"), 
                      labels = c("Prediction Locations", "In-Sample Data")) + 
  scale_shape_discrete(name = " ",
                       breaks=c("Prediction Locations", "In-Sample Data"), 
                       labels = c("Prediction Locations", "In-Sample Data")) +
  geom_smooth(data = data_scaled[which(data_scaled$subset=="In-Sample Data"),], 
              method=lm, fullrange = T, aes(fill=subset), 
              show.legend = F, se = F) +
  annotate("rect", xmin = -Inf, xmax = -shade, ymin = -5, ymax = 5,
           alpha = .2) +
  annotate("rect", xmin = shade, xmax = Inf, ymin = -5, ymax = 5,
           alpha = .2) + 
  labs(x = expression(log[10]~total~phosphorus~(mu~g~L^{-1})),
       y = expression(log[10]~chlorophyll~a~(mu~g~L^{-1}))) +
  theme(legend.position="top")

cc
##try prediction interval and CI
# Create prediction interval data frame with upper and lower lines 
# corresponding to sequence covering minimum and maximum of x values
# in original dataset
m <- lm(V2 ~ V1, data = data_scaled[which(data_scaled$subset=="In-Sample Data"),]) 

pred_interval <- predict(m, newdata = data.frame(V1 = data_scaled$V1), 
                         interval="prediction", level = .95, se.fit = T)
pred_interval <- as.data.frame(pred_interval)

data_scaled2 <- cbind(data_scaled, pred_interval)

dd <- ggplot(data_scaled2, aes(x = V1, y = V2, ymin = fit.lwr, ymax = fit.upr,
                           col = subset, 
                           shape = subset)) +
  geom_point() +
  geom_smooth(data = data_scaled2[which(data_scaled2$subset=="In-Sample Data"),], 
              method=lm, fullrange = T, aes(fill=subset), se = TRUE, 
              show.legend = F) +
  scale_colour_manual(name = " ",
                      values = c("#A6CEE3", "#E41A1C"), 
                      breaks=c("Prediction Locations", "In-Sample Data"), 
                      labels = c("Prediction Locations", "In-Sample Data")) +
  scale_shape_discrete(name = " ",
                       breaks=c("Prediction Locations", "In-Sample Data"), 
                       labels = c("Prediction Locations", "In-Sample Data")) +
  annotate("rect", xmin = -Inf, xmax = -shade, ymin = -5, ymax = 5,
           alpha = .2) +
  annotate("rect", xmin = shade, xmax = Inf, ymin = -5, ymax = 5,
           alpha = .2) + 
  labs(x = expression(log[10]~total~phosphorus~(mu~g~L^{-1})),
       y = expression(log[10]~chlorophyll~a~(mu~g~L^{-1}))) + 
  geom_line(aes(y=fit.lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=fit.upr), color = "red", linetype = "dashed") +
  # geom_ribbon(aes(ymin = lwr, ymax = upr), 
  #             fill = "blue", alpha = 0.2) + 
  theme(legend.position="top")

dd
#nope

## save plot as PDF


cc
ggsave("./figures/Extrapolation_Concept.pdf")

dd + geom_point(size = 4) + theme(text = element_text(size=40))

ggsave("./figures/figure1.pdf", 
       width = 860, height = 573,
       units = "mm")


ggsave("./figures/Extrapolation_Concept_CIPI.eps", 
       width = 860, height = 573,
       units = "mm")

