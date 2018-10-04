library(nimble)
library(igraph)


dyes_code <- nimbleCode({
  for (i in 1:BATCHES) {
    for (j in 1:SAMPLES) {
      y[i,j] ~ dnorm(mu[i], sd = sigma.within);
    }
    mu[i] ~ dnorm(theta, sd = sigma.between);
  }
  
  theta ~ dnorm(0.0, 1.0E-10);
  sigma.within ~ dunif(0, 100)
  sigma.between ~ dunif(0, 100)
})

dyes_data <-list(y = structure(
                   .Data = c(1545, 1440, 1440, 1520, 1580,
                             1540, 1555, 1490, 1560, 1495,
                             1595, 1550, 1605, 1510, 1560,
                             1445, 1440, 1595, 1465, 1545,
                             1595, 1630, 1515, 1635, 1625,
                             1520, 1455, 1450, 1480, 1445), .Dim = c(6, 5)))

dyes_inits <- list(theta = 1500,
                   mu = rnorm(6, 1500, 50),
                   sigma.within = 20,
                   sigma.between = 20)

dyes_constants <-  list(BATCHES = 6, SAMPLES = 5)

dyes_model <- nimbleModel(code = dyes_code, constants = dyes_constants,
                          data = dyes_data, inits = dyes_inits)


#look at model stuff - why? whynot?
dyes_model$getNodeNames()
dyes_model$mu


plot(dyes_model$getGraph())

# Calculate log probability densities for part or all of the model
calculate(dyes_model)


#build MCMC
dyes_MCMC <- buildMCMC(dyes_model)


#compile the model

compiled_dyesModel <- compileNimble(dyes_model, dyes_MCMC)
calculate(compiled_dyesModel)

compiled_dyesModel$dyes_MCMC$run(10000)

samples <- as.matrix(compiled_dyesModel$dyes_MCMC$mvSamples)
summary(samples)

plot(density(samples[, 1]))
plot(samples[ , 1], type = 'l', xlab = 'iteration',  ylab = expression(sigma))

plot(density(samples[, 2]))
plot(samples[ , 2], type = 'l', xlab = 'iteration',  ylab = expression(sigma))

plot(density(samples[, 3]))
plot(samples[ , 3], type = 'l', xlab = 'iteration',  ylab = expression(theta))


##other way to do mcmc?

dyes_MCMCout <- nimbleMCMC(code = dyes_code,constants = dyes_constants, data = dyes_data,
                           inits = dyes_inits, model = dyes_model, 
                           niter = 100000, nchains = 2, nburnin = 25000, summary = TRUE, WAIC = TRUE)

dyes_MCMCout$summary
