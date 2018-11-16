## Want to load results from LAGOS MCMC algorithm and check diagnostics
## August 10 2018

library(coda)

load("./rdata-data/runALL.Rdata")

n.holdout <- 5


## USING coda TO EXAMINE RESULTS
samples_list <- list()

for(i in 1:n.holdout){
  samples_list[[i]] <- mcmc(samples[[i]])  
}


coda_samples <- mcmc.list(samples_list)
plot(coda_samples)   
crosscorr(coda_samples)
effectiveSize(coda_samples)
# using coda's default plotting of MCMC samples

#using MCMCvis package
MCMCsummary(coda_samples, n.eff = TRUE) #needs multiple chains?
MCMCpstr(coda_samples)
MCMCtrace(coda_samples)
MCMCchains(coda_samples)
MCMCplot(coda_samples, params = 'Sigma', horiz = FALSE)
MCMCplot(coda_samples, params = 'beta', horiz = FALSE)




