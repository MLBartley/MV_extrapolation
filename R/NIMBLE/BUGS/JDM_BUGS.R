# rm(list=ls())
library(data.table)
library(R2WinBUGS)
library(MCMCpack)


dat <- fread('dat.csv')
dat[, .N]
head(dat)
summary(dat)




# Define model
sink("jdm.txt")
cat("
    model {
    
for(i in 1:n){
  y[i,1:K] ~ dmnorm(mu[i,], Tau.B[,])
    for(j in 1:5){
    mu[i,j] <- alpha[j] + beta[j,1]*x[i,1] + beta[j,2]*x[i,2] + beta[j,3]*x[i,3] +
beta[j,4]*x[i,4] + beta[j,5]*x[i,5]
              + beta[j,6]*x[i,6] + beta[j,7]*x[i,7] + beta[j,8]*x[i,8]
    }
  }


# Model variance-covariance
Tau.B[1:K,1:K] ~ dwish(W[,], df)
df <- K+1
Sigma.B[1:K,1:K] <- inverse(Tau.B[,])
for (k in 1:K){
  for (k.prime in 1:K){
    rho.B[k,k.prime] <- Sigma.B[k,k.prime]/
    sqrt(Sigma.B[k,k]*Sigma.B[k.prime,k.prime])
  }
  sigma.B[k] <- sqrt(Sigma.B[k,k])
}

## Priors for regression coefficients
for(j in 1:K){
    alpha[j] ~ dnorm(0, 0.0001)
   for(k in 1:nbeta) {
     beta[j,k] ~ dnorm(0, 0.0001)
   }
}
    

} # end model
    ",fill = TRUE)
sink()


head(dat)

# Number of response variables
K <- 5

# Create identity matrix for Wishart dist'n
W <- diag(K)

# Create response matrix
y_mat <- cbind(dat$log_tp, dat$log_tn, dat$log_chla, dat$log_secchi, dat$log_no3)
dim(y_mat)


# Predictor matrix
x <- as.matrix(dat[,77:84])
# Number of predictors
J <- dim(x)[2]

# Load data
data <- list(y = y_mat, n = dim(dat)[1], x = x, K = K, W = W, nbeta = dim(x)[2] )

# Initial values
inits <- function (){
  list (alpha = rnorm(K),
        beta = matrix(rnorm(J*K),nrow=K,ncol=J),
        Tau.B=rwish(K+1,diag(K)) )
}

# Parameters monitored
parameters <- c("alpha","beta","Sigma.B","rho.B")


# MCMC settings
ni <- 15000
nt <- 1
nb <- 5000
nc <- 3

bugs.dir <- "C:/Program Files/WinBUGS14/"
wine_bin <- "/Users/mlbartley/homebrew/bin/wine" #system("which wine", intern = TRUE)
winepath_bin <- "/Users/mlbartley/homebrew/bin/winepath" #system("which winepath", intern = TRUE)

start.time = Sys.time()         # Set timer 
out <- bugs(data = data, inits = inits, parameters.to.save = parameters, 
            model.file = "jdm.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb,debug = F, bugs.directory=bugs.dir, DIC=FALSE, 
            useWINE = TRUE, working.directory = ".", 
            WINE = wine_bin,
            WINEPATH = winepath_bin)
end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 
# Calculate computation time

print(out,3)

# Export
BugsOut <- out$summary
write.csv(BugsOut, "BUGSModelSummary.csv", row.names = T)
# Export MCMC samples 
mcmcOut <- out$sims.list
saveRDS(mcmcOut, file="JDMmcmc_out.rds")




