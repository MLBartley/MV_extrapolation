##NIMBLE CODE found in "JDM_NIMBLE.R" file in same folder as this file
## 21 June 2018
library(coda)
library(dplyr)
library(magrittr)
library(MCMCpack)
library(MCMCvis)
library(rpart)
library(rpart.plot)

##Save summary and samples
# Import Summary
NIMBLE_summary <-read.csv("NIMBLEModelSummary.csv")
# Import MCMC samples 
out <- readRDS("JDMmcmc_out.rds")

source("JDM_EDA.R")


#check results - summary, overlapping chain plots, overlapping density plots

#following is DIFFERENT from check results code in JDM_NIMBLE
#specific to saved/reloaded MCMC output

plot(density(out[[1]][, 1])) #must do 2+ chains to use list notation
plot(out[[1]][, 1], type = 'l', xlab = 'iteration',  ylab = "Sigma.B[1, 1]")


## USING coda TO EXAMINE RESULTS
sample1 <- mcmc(out[[1]])
sample2 <- mcmc(out[[2]]) #in future make loop to do this for nc number of chains - MLB
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

M <- length(out[[1]][, 1])

#BL 
alpha <- out[[1]][, 26:30]
dim(alpha)
B<-list()
for(s in 1:M){
  B[[s]] <- matrix(out[[1]][s, 31:70], nrow = K, ncol = J )
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

## Exploring Cutoff Options - trace

plot(var.yhat.trace, xlim = c(0, nrow(dat)), 
     ylim = c(min(var.yhat.trace, var.yhat.u.trace),
              max(var.yhat.trace, var.yhat.u.trace)))
points(var.yhat.trace[leveragePoints], col = "yellow", x = leveragePoints) #created in JDM_EDA
points(var.yhat.trace[inflPoints], col = "magenta", x = inflPoints) #created in JDM_EDA
points(var.yhat.u.trace, col = "red", x = (n+1):(n+n.u))

#1. Max(var(yhat))
cutoff.max.trace <- max(var.yhat.trace)
abline(h = cutoff.max.trace, col = "blue")

#2. leverage (3(k/n) hat equiv)
cutoff.lev.trace <- max(var.yhat.trace[-leveragePoints])
abline(h = cutoff.lev.trace, col = "purple")

#3. max non-infl point - see JDM_EDA.R
cutoff.infl.trace <- max(var.yhat.trace[-inflPoints])
abline(h = cutoff.infl.trace, col = "orange" )

#4. 95% & 99% 
cutoff.95.trace <- quantile(var.yhat.trace, probs = .95)
cutoff.99.trace <- quantile(var.yhat.trace, probs = .99)
abline(h = cutoff.95.trace, col = "navy")
abline(h = cutoff.99.trace, col = "green")


#plot all cut offs - elbow?


## Exploring Cutoff Options - determinent 
plot(var.yhat.det, xlim = c(0, nrow(dat)), 
     ylim = c(min(var.yhat.det, var.yhat.u.det),
              max(var.yhat.det, var.yhat.u.det)))
points(var.yhat.u.det, col = "red", x = n+1:n.u)
points(var.)
#1. Max(var(yhat))

cutoff.max.det <- max(var.yhat.det)

#2. leverage

#3. 2k.n hat equiv

#4. max non-infl point

cutoff.max.det <- max(var.yhat.det[])

#5. 95% 


## CART model on IVH results

trace.IVH <- as.data.frame(var.yhat.u.trace) %>% 
                    mutate(IVH_binary = as.numeric(var.yhat.u.trace > cutoff.infl.trace)) %>% 
                    mutate(IVH_range = as.numeric(var.yhat.u.trace/cutoff.infl.trace))

x.missing.df <- as.data.frame(x.missing) #created in JDM_NIMBLE.R

trace.IVH <- merge(trace.IVH, x.missing.df)

fit <- rpart(IVH_binary ~ . - IVH_range, method = "class")

printcp(fit)	#display cp table
plotcp(fit)	#plot cross-validation results
# rsq.rpart(fit)	#plot approximate R-squared and relative error for different splits (2 plots). labels are only appropriate for the "anova" method.
print(fit)	#print results
summary(fit)	#detailed results including surrogate splits
plot(fit)	#plot decision tree
text(fit)	#label the decision tree plot
prp(fit)
# post(fit, file=)	#create postscript plot of decision tree

fit2 <- rpart(IVH_range ~ . - IVH_binarycaf, method = "anova")

printcp(fit2)	#display cp table
plotcp(fit2)	#plot cross-validation results
rsq.rpart(fit2)	#plot approximate R-squared and relative error for different splits (2 plots). labels are only appropriate for the "anova" method.
print(fit2)	#print results
summary(fit2)	#detailed results including surrogate splits
plot(fit2)	#plot decision tree
text(fit2)	#label the decision tree plot
prp(fit2, extra = 100, type = 1)
post(fit2, file=)	#create postscript plot of decision tree
