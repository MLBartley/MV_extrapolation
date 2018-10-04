# rm(list=ls())
library(data.table)
library(R2WinBUGS)
library(MCMCpack)


dat <- fread('dat.csv')
dat[, .N]
head(dat)
summary(dat)


# Number of lakes per HUC4, ranges from 1-986
range(table(dat$hu4_zoneid))
median(table(dat$hu4_zoneid)) # 66
length(unique(dat$hu4_zoneid)) # 62 HUC4 watersheds

# Allow parameters to vary by HUC4 watershed
# Define model
sink("jdm_1_01.txt")
cat("
    model {
    
for(i in 1:n){
  y[i,1:K] ~ dmnorm(mu[i,], Tau.B[,])
    for(j in 1:5){
    mu[i,j] <- alpha[huc[i],j] + beta[huc[i],j,1]*x[i,1] + beta[huc[i],j,2]*x[i,2] + beta[huc[i],j,3]*x[i,3] + beta[huc[i],j,4]*x[i,4] 
              + beta[huc[i],j,5]*x[i,5] + beta[huc[i],j,6]*x[i,6] + beta[huc[i],j,7]*x[i,7] + beta[huc[i],j,8]*x[i,8]
    }
  }


# Varying int and slope parameters by HUC4
# Priors on population average parameters for each nutrient, j
for(r in 1:H){ # number of HUC4s
  for(j in 1:5){  # of nutrients
    alpha[r,j] ~ dnorm(mu.alpha[j], tau.alpha[j])
    beta[r,j,1] ~ dnorm(mu.beta1[j], tau.beta1[j])
    beta[r,j,2] ~ dnorm(mu.beta2[j], tau.beta2[j])
    beta[r,j,3] ~ dnorm(mu.beta3[j], tau.beta3[j])
    beta[r,j,4] ~ dnorm(mu.beta4[j], tau.beta4[j])
    beta[r,j,5] ~ dnorm(mu.beta5[j], tau.beta5[j])
    beta[r,j,6] ~ dnorm(mu.beta6[j], tau.beta6[j])
    beta[r,j,7] ~ dnorm(mu.beta7[j], tau.beta7[j])
    beta[r,j,8] ~ dnorm(mu.beta8[j], tau.beta8[j])
  }
}


# Priors for population-average parameters for each nutrient
for(j in 1:5){
  mu.alpha[j] ~ dnorm(0,0.001)
  mu.beta1[j] ~ dnorm(0,0.001)
  mu.beta2[j] ~ dnorm(0,0.001)
  mu.beta3[j] ~ dnorm(0,0.001)
  mu.beta4[j] ~ dnorm(0,0.001)
  mu.beta5[j] ~ dnorm(0,0.001)
  mu.beta6[j] ~ dnorm(0,0.001)
  mu.beta7[j] ~ dnorm(0,0.001)
  mu.beta8[j] ~ dnorm(0,0.001)
}

# Priors for variances for varyling ints and slopes
for(j in 1:5){
  sigma.alpha[j] ~ dunif(0, 5)
  tau.alpha[j] <- pow(sigma.alpha[j],-2)
  sigma.beta1[j] ~ dunif(0, 5)
  tau.beta1[j] <- pow(sigma.beta1[j],-2)
  sigma.beta2[j] ~ dunif(0, 5)
  tau.beta2[j] <- pow(sigma.beta2[j],-2)
  sigma.beta3[j] ~ dunif(0, 5)
  tau.beta3[j] <- pow(sigma.beta3[j],-2)
  sigma.beta4[j] ~ dunif(0, 5)
  tau.beta4[j] <- pow(sigma.beta4[j],-2)
  sigma.beta5[j] ~ dunif(0, 5)
  tau.beta5[j] <- pow(sigma.beta5[j],-2)
  sigma.beta6[j] ~ dunif(0, 5)
  tau.beta6[j] <- pow(sigma.beta6[j],-2)
  sigma.beta7[j] ~ dunif(0, 5)
  tau.beta7[j] <- pow(sigma.beta7[j],-2)
  sigma.beta8[j] ~ dunif(0, 5)
  tau.beta8[j] <- pow(sigma.beta8[j],-2)
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

  

} # end model
    ",fill = TRUE)
sink()


head(dat)

# HUC4 watershed identifier for each observation
hucID <- as.numeric(as.factor(as.numeric(as.factor(dat$hu4_zoneid))))
# Number of HUCs
H <- length(unique(hucID))


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
data <- list(y = y_mat, n = dim(dat)[1], x = x, K = K, W = W, huc=hucID, H=H)

# Initial values
# beta = matrix(rnorm(J*K),nrow=K,ncol=J),
# alpha = rnorm(K),


inits <- function (){
  list (mu.alpha=rnorm(5),mu.beta1=rnorm(5),mu.beta2=rnorm(5),mu.beta3=rnorm(5),mu.beta4=rnorm(5),
        mu.beta5=rnorm(5),mu.beta6=rnorm(5),mu.beta7=rnorm(5),mu.beta8=rnorm(5),
        sigma.alpha=runif(5),sigma.beta1=runif(5),sigma.beta2=runif(5),sigma.beta3=runif(5),
        sigma.beta4=runif(5),sigma.beta5=runif(5),sigma.beta6=runif(5),sigma.beta7=runif(5),sigma.beta8=runif(5),
        Tau.B=rwish(K+1,diag(K)) )
}

# Parameters monitored
parameters <- c("alpha","beta","Sigma.B","rho.B","mu.beta1","mu.beta2",
                "mu.beta3","mu.beta4","mu.beta5","mu.beta6","mu.beta7","mu.beta8",
                "sigma.beta1","sigma.beta2","sigma.beta3","sigma.beta4","sigma.beta5","sigma.beta6",
                "sigma.beta7","sigma.beta8")


# MCMC settings
ni <- 15000
nt <- 1
nb <- 5000
nc <- 3

bugs.dir <- "C:/Program Files/WinBUGS14/"
 
start.time = Sys.time()         # Set timer 
out <- bugs(data = data, inits = inits, parameters.to.save = parameters, 
            model.file = "jdm_1_01.txt", n.chains = nc, n.thin = nt, n.iter = ni, 
            n.burnin = nb,debug = F, bugs.directory=bugs.dir, DIC=FALSE)
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



