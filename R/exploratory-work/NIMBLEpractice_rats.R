#NIMBLE Practice - rats exmple from WinBUGS

library(nimble)

##code is exactly the same as BUGS code
rats_code <-nimbleCode({
                           for( i in 1 : N ) {
                             for( j in 1 : T ) {
                               # Y[i , j] ~ dnorm(mu[i , j],tau.c)
                               Y[i, j] ~ dnorm(mu[i, j], sigma) #changes from BUGS to NIMBLE
                               
                               mu[i , j] <- alpha[i] + beta[i] * (x[j] - xbar)
                               culmative.Y[i , j] <- culmative(Y[i , j], Y[i , j]) #cannot get this line to work - switching to another example :( 
                               post.pv.Y[i , j] <- post.p.value(Y[i , j])
                               prior.pv.Y[i , j] <- prior.p.value(Y[i , j])
                               replicate.post.Y[i , j] <- replicate.post(Y[i , j])
                               pv.post.Y[i , j] <- step(Y[i , j] - replicate.post.Y[i , j])
                               replicate.prior.Y[i , j] <- replicate.prior(Y[i , j])
                               pv.prior.Y[i , j] <- step(Y[i , j] - replicate.prior.Y[i , j])
                             }
                             alpha[i] ~ dnorm(alpha.c,alpha.tau)
                             beta[i] ~ dnorm(beta.c,beta.tau)
                           }
                           # tau.c ~ dgamma(0.001,0.001) #need to use st dev parameters not precision parameters
                           # sigma <- 1 / sqrt(tau.c)
                           
                           sigma ~ dunif(0, 100)
                           
                           alpha.c ~ dnorm(0.0,1.0E-6)   
                           alpha.tau ~ dgamma(0.001,0.001)
                           beta.c ~ dnorm(0.0,1.0E-6)
                           beta.tau ~ dgamma(0.001,0.001)
                           alpha0 <- alpha.c - xbar * beta.c   
                         } )

rats_data <- list(x = c(8.0, 15.0, 22.0, 29.0, 36.0),    
                  Y = structure(
                    .Data = c(151, 199, 246, 283, 320,
                              145, 199, 249, 293, 354,
                              147, 214, 263, 312, 328,
                              155, 200, 237, 272, 297,
                              135, 188, 230, 280, 323,
                              159, 210, 252, 298, 331,
                              141, 189, 231, 275, 305,
                              159, 201, 248, 297, 338,
                              177, 236, 285, 350, 376,
                              134, 182, 220, 260, 296,
                              160, 208, 261, 313, 352,
                              143, 188, 220, 273, 314,
                              154, 200, 244, 289, 325,
                              171, 221, 270, 326, 358,
                              163, 216, 242, 281, 312,
                              160, 207, 248, 288, 324,
                              142, 187, 234, 280, 316,
                              156, 203, 243, 283, 317,
                              157, 212, 259, 307, 336,
                              152, 203, 246, 286, 321,
                              154, 205, 253, 298, 334,
                              139, 190, 225, 267, 302,
                              146, 191, 229, 272, 302,
                              157, 211, 250, 285, 323,
                              132, 185, 237, 286, 331,
                              160, 207, 257, 303, 345,
                              169, 216, 261, 295, 333,
                              157, 205, 248, 289, 316,
                              137, 180, 219, 258, 291,
                              153, 200, 244, 286, 324),
                    .Dim = c(30,5)))

rats_inits <- list(alpha = c(250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 
                            250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250),
                  beta = c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 
                           6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6),         
                  alpha.c = 150, beta.c = 10, 
                  tau.c = 1, alpha.tau = 1, beta.tau = 1)

rats_constants <- list(xbar = 22, N = 30, T = 5)

rats_model <- nimbleModel(code = rats_code, constants = rats_constants, data = rats_data, inits = rats_inits)

rats_model$getNodeNames()
rats_model$x
rats_model$alpha0

rats_model$plotGraph() #not very helpful

# Calculate log probability densities for part or all of the model
calculate(rats_model)


#build MCMC
rats_MCMC <- buildMCMC(rats_model)


#compile the model

compiled_ratsModel <- compileNimble(rats_model, rats_MCMC)
calculate(compiled_ratsModel)

compiled_ratsModel$rats_MCMC$run(10000)

samples <- as.matrix(compiled_ratsModel$rats_MCMC$mvSamples)
summary(samples)

plot(density(samples[, 2]))
  


##other way to do mcmc?

rats_MCMCout <- nimbleMCMC(code = rats_code,constants = rats_constants, data = rats_data,
                           inits = rats_inits, model = rats_model, 
                           niter = 10000, nchains = 2, nburnin = 1000, summary = TRUE, WAIC = TRUE)

rats_MCMCout$summary
