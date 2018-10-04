##want to identify potential influential points



load("./rdata-data/runALL_180808.Rdata") #may need to change the data part of this file name


n.holdout <- 1

X_training <- list(runALL$data$X[, -1]) #don't want intercept

K <- list(runALL$data$p - 1) 

n <- list(runALL$data$n)

hat <- list()
leverage_cutoff <- list()
leveragePoints <- list()

for (i in 1:n.holdout){
  hat[[i]] <- diag(X_training[[i]] %*% solve(t(X_training[[i]]) %*% X_training[[i]]) %*% t(X_training[[i]]))
  leverage_cutoff[[i]] <- 3 * (K[[i]]/n[[i]])
  leveragePoints[[i]] <- unique(which(hat[[i]] > leverage_cutoff[[i]]))
}




