###
### Recoding Ty and Erin's Model in NIMBLE (originally done in WinBUGS)
###

library(coda)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(igraph)
library(MCMCpack)
library(MCMCvis)
library(nimble)



dat <- fread('dat.csv')
dat[, .N]
head(dat)
summary(dat)



# Define model
modelCode <- nimbleCode({
    
    for(i in 1:n){
      for(j in 1:5){
      mu[i,j] <- alpha[j]+beta[j,1]*x[i,1]+beta[j,2]*x[i,2]+beta[j,3]*x[i,3]+beta[j,4]*x[i,4]+beta[j,5]*x[i,5]+beta[j,6]*x[i,6]+beta[j,7]*x[i,7]+beta[j,8]*x[i,8]
      # y[i,1:K] ~ dmnorm(mu[i, ], Tau.B[,]) #precision in BUGS
      y[i, 1:K] ~ dmnorm(mean = mu[i, ], cov = Sigma.B[, ]) #covar in NIMBLE
      # y.hat[i, 1:K] <- y.hat[i, 1:K] + y[i, 1:K]
      # y.hat.sq[i, 1:K] <- y.hat.sq[i, 1:K] + (y[i, 1:K])^2
        }
    }
    
    
    # Model variance-covariance
    # Tau.B[1:K,1:K] ~ dwish(W[,], df)
    df <- K+1
    # Sigma.B[1:K,1:K] ~ inverse(Tau.B[,])
    Sigma.B[1:K, 1:K] ~ dinvwish(S = W[, ], df = df) #just trying out identity matrix 
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
    }) # end model
   
# Number of response variables
K <- 5

# Create identity matrix for Wishart dist'n
W <- diag(K)

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
hist(na_count_perlake)

y_mat <- y_mat[-na.idx, ]


# Predictor matrix
 # x <- as.matrix(dat[,77:84]) #commented out while using only non-missing data
x <- as.matrix(dat[-na.idx, 77:84])
x.missing <- as.matrix(dat[na.idx, 77:84])

##EDA to check for influential points in both covariate sets (missing/non-missing)

varlist <- names(dat[, 77:84])

for(i in varlist) {
  plot(x[, i])
  points(x.missing[, i], col = "red")
}
##look up better MV EDA - tourr and rggobi R packages


# Number of predictors
J <- dim(x)[2]

# n = dim(dat)[1]
n = dim(dat[-na.idx,])[1]

# Load data
data <- list(y = y_mat, x = x)
model_constants <- list(n = n, K = K, nbeta = dim(x)[2], J = dim(x)[2])

# Initial values

S.B <- riwish(K+1, diag(K))
r.B <- matrix(NA, nrow = K, ncol = K)
s.B <- vector()
for (k in 1:K){
  for (k.prime in 1:K){
    r.B[k,k.prime] <- S.B[k,k.prime]/
      sqrt(S.B[k,k]*S.B[k.prime,k.prime])
  }
  s.B[k] <- sqrt(S.B[k,k])
}

inits <- list (alpha = rnorm(K),
        beta = matrix(rnorm(J*K),nrow=K,ncol=J),
        # Tau.B=rwish(K+1,diag(K)) 
        Sigma.B = S.B,
        W = diag(K), 
        sigma.B = s.B, 
        rho.B = r.B
        # , 
        # y.hat = matrix(0, nrow=n, ncol = K),
        # y.hat.sq = matrix(0, nrow = n, ncol = K)
        ) #unsure if I need to initialize everything but otherwise 
                     #show up as NA when creating the NIMBLE model
  

# Parameters monitored
parameters <- c("alpha","beta","Sigma.B","rho.B")



# Create the NIMBLE Model

jdm_nimble <- nimbleModel(code = modelCode, 
                          name = "JDM", 
                          constants = model_constants, 
                          data = data, 
                          inits = inits, 
                          dimensions = list(mu = c(n, 5), W = dim(W), Sigma.B = c(K, K))) #needed to specify or model will not build


#Model is not fully intialized - not an error but means NAs found. Fixed if only use 

jdm_nimble$initializeInfo()
jdm_nimble$getNodeNames()
# jdm_nimble$plotGraph()

jdm_nimble$x #the covariates
jdm_nimble$alpha #intercept of mu calculation
jdm_nimble$beta #beta covariates of mu calculation
jdm_nimble$mu #mean vector
jdm_nimble$rho.B #correlation
jdm_nimble$sigma.B #sqrt of cov matrix
jdm_nimble$Sigma.B #covariance matrix
jdm_nimble$getDistribution('Sigma.B')

jdm_nimble$y

jdm_nimble$checkConjugacy()

 # autoBlockConf <- configureMCMC(jdm_nimble, autoBlock = TRUE)

# MCMC settings
ni <- 105000
nt <- 1
nb <- 5000
nc <- 2

##MCMC with nimbleMCMC
  
start.time = Sys.time()         # Set timer 
#don't think you can customize MCMC (i.e. samplers used, etc)
#with nimbleMCMC - rather must configure, build, then run (see below)
out <-  nimbleMCMC(code = modelCode,constants = model_constants, data = data,
                   inits = inits, model = jdm_nimble, 
                   niter = ni, nchains = nc, nburnin = nb, summary = TRUE, WAIC = TRUE)
end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 
# Calculate computation time ~ est time: 
    #about 1 hour? took 141 minutes with computer closed for lunch
    #63 minutes with subset of (non-missing responses) data - 3 chains, 15000 iter + 5000 burnin




##Save summary and samples
# Export
NIMBLEOut <- out$summary
write.csv(NIMBLEOut, "NIMBLEModelSummary.csv", row.names = T)
# Export MCMC samples 
mcmcOut <- out$samples
saveRDS(mcmcOut, file="JDMmcmc_out.rds")

##everthing after this point is continued in the "JDM_ExtrapolateCode.R"





#check results - summary, overlapping chain plots, overlapping density plots
out$summary

plot(density(out$samples[[1]][, 1])) #must do 2+ chains to use list notation

plot(out$samples[[1]][, 1], type = 'l', xlab = 'iteration',  ylab = "Sigma.B[1, 1]")


## USING coda TO EXAMINE RESULTS
sample1 <- mcmc(out$samples[[1]])
sample2 <- mcmc(out$samples[[2]]) #in future make loop to do this for nc number of chains - MLB
coda_samples <- mcmc.list(sample1, sample2)
plot(coda_samples)   
crosscorr(coda_samples)
effectiveSize(coda_samples)
# using coda's default plotting of MCMC samples

#using MCMCvis package
MCMCsummary(coda_samples, n.eff = TRUE) #needs multiple chains?
MCMCpstr(coda_samples)
MCMCtrace(coda_samples)
MCMCchains(coda_samples)
MCMCplot(coda_samples, params = 'Sigma.B', horiz = FALSE)
MCMCplot(coda_samples, params = 'alpha', horiz = FALSE)
MCMCplot(coda_samples, params = 'beta', horiz = FALSE)






#obtain full data variences
  #initialize 
  # yhat.i <- array(0, c(5, n, M))
  yhat.sum.sq <- array(0, c(K, K, n))
  yhat.sum <- matrix(0, nrow = n, ncol = K)
  var.yhat.trace <- rep(0, length = n)
  var.yhat.det <- rep(0, length = n)
  
M <- length(out$samples[[1]][, 1])

#BL 
alpha <- out$samples[[1]][, 26:30]
dim(alpha)
B<-list()
for(s in 1:M){
B[[s]] <- matrix(out$samples[[1]][s, 31:70], nrow = K, ncol = J )
}
B[[1]]
length(B)

start.time = Sys.time()         # Set timer 
for(s in 1:M){
# print("a")
#   alpha <- out$samples[s, 26:30]
# print("b")
#     B <- matrix(out$samples[s, 31:70], nrow = K, ncol = J )
  # Sigma.B <- matrix(out$samples[s, 1:25], nrow = K, ncol = K) #only useful if adding N(0, Sigma.B) to y.hat

  for(i in 1:(n)){
    yhat.i <- alpha[s,] + B[[s]] %*% x[i, ] #not stored #should we also add N(0, Sigma.B.hat)?
    yhat.sum[i, ] <- yhat.sum[i, ] + t(yhat.i)
    yhat.sum.sq[ , , i] <- yhat.sum.sq[ , , i] + (yhat.i %*% t(yhat.i))
  }
}
end.time = Sys.time()
elapsed.time2 = round(difftime(end.time, start.time, units='mins'), dig = 2)

# 
# for(s in 1:M){
#   alpha <- out$samples[s, 26:30]
#   B <- matrix(out$samples[s, 31:70], nrow = K, ncol = J )
#   # Sigma.B <- matrix(out$samples[s, 1:25], nrow = K, ncol = K) #only useful if adding N(0, Sigma.B) to y.hat
#   for(i in 1:(n)){
#     yhat.i <- alpha + B %*% x[i, ] #not stored #should we also add N(0, Sigma.B.hat)?
#   yhat.sum[i, ] <- yhat.sum[i, ] + t(yhat.i)
#   yhat.sum.sq[ , , i] <- yhat.sum.sq[ , , i] + (yhat.i %*% t(yhat.i))
#   }
# }
# 
for (i in 1:n){
var.yhat <- 1/M * (yhat.sum.sq[, , i]) - ((1/M * yhat.sum[i,]) %*% t(1/M * yhat.sum[i,])) #doesn't save the full covariance matrix but what we want is trace or det
var.yhat.trace[i] <- sum(diag(var.yhat))
var.yhat.det[i] <- det(var.yhat)
}

#obtain missing response data predicted variences
n.u <- nrow(x.missing) 
yhat.u.sum.sq <- array(0, c(K, K, n.u))
yhat.u.sum <- matrix(0, nrow = n.u, ncol = K)
var.yhat.u.trace <- rep(0, length = n.u)
var.yhat.u.det <- rep(0, length = n.u)

start.time = Sys.time()         # Set timer 
for(s in 1:M){
  # print("a")
  #   alpha <- out$samples[s, 26:30]
  # print("b")
  #     B <- matrix(out$samples[s, 31:70], nrow = K, ncol = J )
  # Sigma.B <- matrix(out$samples[s, 1:25], nrow = K, ncol = K) #only useful if adding N(0, Sigma.B) to y.hat
  
  for(i in 1:(n.u)){
    yhat.i <- alpha[s,] + B[[s]] %*% x.missing[i, ] #not stored #should we also add N(0, Sigma.B.hat)?
    yhat.u.sum[i, ] <- yhat.u.sum[i, ] + t(yhat.i)
    yhat.u.sum.sq[ , , i] <- yhat.u.sum.sq[ , , i] + (yhat.i %*% t(yhat.i))
  }
}
for (i in 1:n.u){
  var.yhat <- 1/M * (yhat.u.sum.sq[, , i]) - ((1/M * yhat.u.sum[i,]) %*% t(1/M * yhat.u.sum[i,])) #doesn't save the full covariance matrix but what we want is trace or det
  var.yhat.u.trace[i] <- sum(diag(var.yhat))
  var.yhat.u.det[i] <- det(var.yhat)
}

end.time = Sys.time()
elapsed.time3 = round(difftime(end.time, start.time, units='mins'), dig = 2)



#visualize results (missing vs non-missing reponses)

plot(var.yhat.trace, xlim = c(0, nrow(dat)), 
     ylim = c(min(var.yhat.trace, var.yhat.u.trace),
              max(var.yhat.trace, var.yhat.u.trace)))
points(var.yhat.u.trace, col = "red", x = n+1:n.u)


plot(var.yhat.det, xlim = c(0, nrow(dat)), 
     ylim = c(min(var.yhat.det, var.yhat.u.det),
              max(var.yhat.det, var.yhat.u.det)))
points(var.yhat.u.det, col = "red", x = n+1:n.u)

# # MCMC with compiled runMCMC()

jdm_config <- configureMCMC(jdm_nimble)
jdm_config$printSamplers()
# #build MCMC
# jdm_MCMC <- buildMCMC(jdm_nimble)
# 
# #compile the model
# 
# compiled_Model <- compileNimble(jdm_nimble, jdm_MCMC)
# calculate(compiled_Model)
# 
# start.time = Sys.time()         # Set timer 
# compiled_Model$jdm_MCMC$run(10000)
# end.time = Sys.time()
# elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
# cat('Posterior computed in ', elapsed.time, ' minutes\n\n', sep='') 
# 
# #oh shit. this only took 15 minutes! but only one chain and no missing response data. 
# #annnnnd 40ish minutes with all of the data.
# 
# samples <- as.matrix(compiled_Model$jdm_MCMC$mvSamples)
# summary(samples)
# 
# plot(samples[, 1], type = 'l', xlab = 'iteration',  ylab = "Sigma.B[1, 1]")

####ISSUES####

#Key#

#Issue
  #open Issue approach taken
        #Solved/Solution

# At present, the NIMBLE MCMC does not provide a sampler for non-conjugate
# Wishart nodes. Users can implement an appropriate sampling algorithm 
# as a nimbleFunction, for use in the MCMC.

          #Solution: OK, we don't need tau_b because no longer using precision matrix 
          #so what we actually need is the INVERSE Wishart distribution on Sigma.B 
          #MLB 30 May 2018

# Error in FUN(X[[i]], ...) : 
# checkDistributionFunctions: density function for diwish is not available.  
# It must be a nimbleFunction (with no setup code).

          #Solution: I think we need to use the dinvwish_chol version of this distrbution
          #MLB 30 May 2018


# What are x and cholesky in dinvwish_chol??

          #May be not worth figuring out if dinvwish_chol is not available as denisty function


# Error in FUN(X[[i]], ...) : 
#checkDistributionFunctions: density function for dinvwish_chol is not available.  
# It must be a nimbleFunction (with no setup code).

          #Solution: looks like the NIMBLE code wants dinvwish not diwish or dinvwish_chol. 
          #BUT using riwish() to get the initial value for Sigma.B
          #MLB 30 May 2018



# Can NIMBLE deal with partial data? From CH 6 in manual: In NIMBLE, when data values are provided,
# any nodes with NA values will not be labeled as data. A node following a multivariate distribution 
# must be either entirely observed or entirely missing.

          #Need to explore data provided.
          #TEMP Solution: use only data without missing values. 
          #Doesn't seem to be able to run with missing values. Multiple imputation useful here??
          #SOLUTION:Use the estimates for alpha and betas to calulate yhats for missing values. 

# Do I need to initialize EVERYTHING in the model. 
# Even values that are derived from other ones? (i.e. sigma.B and rho.B)

          #Need to ask Ephraim - I think this is a similar issue as Sahar's, but why did the 
          #model run (incorrectly) before. 
          # SOLUTION: Simulate a Sigma.B to initialize and from it calculate sigma.B and rho.B to add to init


# How to avoid for loops when calulating variance with mcmc samples?

          #Talk to Ben Lee? 
          #SOLUTION: Could be improved but currently don't need to save the 
          #time and would rather just run code and take a break.

# Array vs list for yhat.sum.sq?

         #SOLUTION: Meh, either works. Let's try out an array.

# Why do the first 1500ish values of the trace or determinant of the missing 
#y variances have similar output to the non-missing y values (of same length)
# but then the rest are ~0??

        #SOLUTION: Was only looping over n, not n.u. Whoops! Fixed.
