

## USING coda TO EXAMINE RESULTS
samples_list <- list()

for(i in 1:nc){
  samples_list[[i]] <- mcmc(samples[[i]])  
}

# sample1 <- mcmc(out$samples[[1]])
# sample2 <- mcmc(out$samples[[2]]) #in future make loop to do this for nc number of chains - MLB

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
MCMCplot(coda_samples, params = 'Sigma.B', horiz = FALSE)
MCMCplot(coda_samples, params = 'alpha', horiz = FALSE)
MCMCplot(coda_samples, params = 'beta', horiz = FALSE)




