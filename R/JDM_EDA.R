library(data.table)
library(ggplot2)
library(mvinfluence)
# library(rggobi)
library(tourr)


dat <- fread('dat.csv')
dat[, .N]
head(dat)
summary(dat)


# Create response matrix
y_mat <- cbind(dat$log_tp, dat$log_tn, dat$log_chla, dat$log_secchi, dat$log_no3)
dim(y_mat)
colnames(y_mat) <- c("logTP", "logTN", "logCHLA", "logSECCHI", "logNO3")

# Explore data - what is missing and where? Missing ratios?

summary(y_mat)

# Number of obs (lakes) for response variables 
# These are the RESSPONSE VARIABLES OF INTEREST
sum(!is.na(y_mat[,"logTP"]))/nrow(y_mat)
sum(!is.na(y_mat[,"logTN"]))/nrow(y_mat)
sum(!is.na(y_mat[,"logCHLA"]))/nrow(y_mat)
sum(!is.na(y_mat[,"logSECCHI"]))/nrow(y_mat)
sum(!is.na(y_mat[,"logNO3"]))/nrow(y_mat)

str(y_mat)

#isolate data with no missing values
na.idx = which(is.na(apply(y_mat, 1, sum))) #5447 have at least one missing value

#how many are missing 1, 2, 3, 4, or 5?
na_count_perlake <- apply(is.na(y_mat), 1, sum)
# hist(na_count_perlake)

y_mat <- y_mat[-na.idx, ]

x <- as.matrix(dat[-na.idx, 77:84])
x.missing <- as.matrix(dat[na.idx, 77:84])

n <- nrow(x)
n.u <- nrow(x.missing)
K <- ncol(x)
J <- dim(x)[2]

##testing out mvinfluence package  - needs lm()

test_lm <- lm(y_mat ~ x)

test_influ <- influence(test_lm)
test_hat <- hatvalues(test_lm)

influencePlot(test_lm)  
influencePlot(test_lm, id.n = 2)
influencePlot(test_lm, id.n = 2, type = "stres")
inflPlot<-influencePlot(test_lm, id.n = 2, type = "LR")


inflPoints <- c(253, 517, 982, 1721, 1724)


leverage_cutoff <- 3* (K/n)
leveragePoints <- unique(which(test_hat > leverage_cutoff))

##explore with tourr package 

# animate_andrews(x, tour_path = grand_tour(3)) #takes a bit to run
