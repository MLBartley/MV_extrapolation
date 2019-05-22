##want to identify potential influential points



load("./PredictionCode/20181626/run1_180627.Rdata") #may need to change the data part of this file name
load("./PredictionCode/20181626/run2_180627.Rdata")
load("./PredictionCode/20181626/run3_180627.Rdata")
load("./PredictionCode/20181626/run4_180627.Rdata")
load("./PredictionCode/20181626/run5_180627.Rdata")

n.holdout <- 5

X_training <- list(run1$data$X[, -1],
                  run2$data$X[, -1],
                  run3$data$X[, -1],
                  run4$data$X[, -1],
                  run5$data$X[, -1]) #don't want intercept

# Z - list(run1$data$Z,
#          run2$data$Z,
#          run3$data$Z,
#          run4$data$Z,
#          run5$data$Z)

K <- list(run1$data$p,
          run2$data$p,
          run3$data$p,
          run4$data$p,
          run5$data$p)

n <- list(run1$data$n,
          run2$data$n,
          run3$data$n,
          run4$data$n,
          run5$data$n)

hat <- list()
leverage_cutoff <- list()
leveragePoints <- list()

for (i in 1:n.holdout){
  hat[[i]] <- diag(X_training[[i]] %*% solve(t(X_training[[i]]) %*% X_training[[i]]) %*% t(X_training[[i]]))
  leverage_cutoff[[i]] <- 3 * (K[[i]]/n[[i]])
  leveragePoints[[i]] <- unique(which(hat[[i]] > leverage_cutoff[[i]]))
}



