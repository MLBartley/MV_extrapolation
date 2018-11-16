### extrapolation_cond function draft

extrapolate_cond <- function(X,
                             Y, 
                             Beta,
                             Sigma,
                             Sampled,
                             Leverage,
                             Cond.index = 1,
                             K = NULL,
                             Theta = NULL,
                             link = "none") {
  S = nrow(X) #how many lakes
  RV <- nrow(Beta) #how many random variables
  if (is.array(Beta) == FALSE)
    Beta = array(Beta, nrow = nrow(Beta), ncol = ncol(Beta))
  mcmc.length = length(Beta[1, 1, ])
  X.pred = X 
  X.obs = X[Sampled, ]
  
  ## MV version - gets  predicted value of all RV for each lake
  Mu.mat = array(0, dim = c(S, RV, mcmc.length))
  for (i in 1:mcmc.length){
    Mu.mat[, , i] = X.pred %*% t(Beta[, , i])
  }

  
  # MV/UV version - if you just want to look at one reponse conditioned on others
  
  Mu.cond = array(0, dim = c(S, 1, mcmc.length))
  for (i in 1: mcmc.length){
    Sigma11 <- Sigma[Cond.index, Cond.index, i]
    Sigma12 <- Sigma[Cond.index ,-Cond.index , i]
    Sigma21 <- Sigma[-Cond.index, Cond.index, i]
    Sigma22 <- Sigma[-Cond.index, -Cond.index, i]
    
    ## ok but what if we don't have all 3 other Variables?
    for (s in 1:S){
    f=which(is.na(Y[s,-Cond.index])==T) #index of na's 
    
    if(lenght(f) == (S - 1)){
      Mu.cond[s,,i] = Mu.mat[s, Cond.index, i]
    }else{
    Mu.cond[s, , i] <- Mu.mat[, Cond.index , i] - Sigma12 %*% solve(Sigma22[-f, -f]) %*% (Y[s, -c(Cond.index, f)] - Mu.mat[, -c(Cond.index, f), i]) ## Need to know X_-k ???
    # Sigma.cond <- Sigma11 - Sigma12 %*% solve(Sigma22) %*% Sigma21 #should be scalar?
        }
    }
  }
  
  
  
  Mu.cond.var = apply(Mu., 1, cov_fun)
  Mu.var = array(Mu.mat.var, dim = c(RV, RV, S))
  
  #Cutoff home
  extrapolate <- list()
  cutoff <- list()
  
  #Cutoff - max
  max.obs.t = max(trace[Sampled])
  max.obs.d = max(determinant[Sampled])
  max.obs.dn <- max(determ_noise[Sampled])
  extrapolate[["EC_max"]] = matrix(rep(1, S*2), nrow = 6, ncol = S)
  
  extrapolate[["EC_max"]][1, ] <- as.numeric(trace < max.obs.t)
  extrapolate[["EC_max"]][2, ] <- as.numeric(determinant < max.obs.d)
  extrapolate[["EC_max"]][3, ] <- as.numeric(determ_noise < max.obs.dn)
  extrapolate[["EC_max"]][4, ] <- as.numeric(trace / max.obs.t)
  extrapolate[["EC_max"]][5, ] <- as.numeric(determinant / max.obs.d)
  extrapolate[["EC_max"]][6, ] <- as.numeric(determ_noise / max.obs.dn)
  
  cutoff[["max"]] <- c(max.obs.t, max.obs.d, max.obs.dn)
  
  #Cutoff - max[-leverage]
  
  lev.obs.t <- max(trace[Sampled][-Leverage])
  lev.obs.d = max(determinant[Sampled][-Leverage])
  lev.obs.dn = max(determ_noise[Sampled][-Leverage])
  
  extrapolate[["EC_levmax"]] = matrix(rep(1, S*2), nrow = 6, ncol = S)
  
  extrapolate[["EC_levmax"]][1, ] <- as.numeric(trace < lev.obs.t)
  extrapolate[["EC_levmax"]][2, ] <- as.numeric(determinant < lev.obs.d)
  extrapolate[["EC_levmax"]][3, ] <- as.numeric(determ_noise < lev.obs.dn)
  extrapolate[["EC_levmax"]][4, ] <- as.numeric(trace / lev.obs.t)
  extrapolate[["EC_levmax"]][5, ] <- as.numeric(determinant / lev.obs.d)
  extrapolate[["EC_levmax"]][6, ] <- as.numeric(determ_noise / lev.obs.dn)
  
  cutoff[["levmax"]] <- c(lev.obs.t, lev.obs.d, lev.obs.dn)
  
  #Cutoff - 95%
  
  nf.obs.t <- quantile(trace[Sampled], probs = .95)
  nf.obs.d = quantile(determinant[Sampled], probs = .95)
  nf.obs.dn <- quantile(determ_noise[Sampled], probs = .95)
  extrapolate[["EC_95quantile"]] = matrix(rep(1, S*2), nrow = 6, ncol = S)
  
  extrapolate[["EC_95quantile"]][1, ] <- as.numeric(trace < nf.obs.t)
  extrapolate[["EC_95quantile"]][2, ] <- as.numeric(determinant < nf.obs.d)
  extrapolate[["EC_95quantile"]][3, ] <- as.numeric(determ_noise < nf.obs.dn)
  extrapolate[["EC_95quantile"]][4, ] <- as.numeric(trace / nf.obs.t)
  extrapolate[["EC_95quantile"]][5, ] <- as.numeric(determinant / nf.obs.d)
  extrapolate[["EC_95quantile"]][6, ] <- as.numeric(determ_noise / nf.obs.dn)
  
  cutoff[["95quantile"]] <- c(nf.obs.t, nf.obs.d, nf.obs.dn)
  
  #Cutoff - 99%
  nn.obs.t <- quantile(trace[Sampled], probs = .99)
  nn.obs.d = quantile(determinant[Sampled], probs = .99)
  nn.obs.dn <- quantile(determ_noise[Sampled], probs = 0.99)
  extrapolate[["EC_99quantile"]] = matrix(rep(1, S*2), nrow = 6, ncol = S)
  
  extrapolate[["EC_99quantile"]][1, ] <- as.numeric(trace < nn.obs.t)
  extrapolate[["EC_99quantile"]][2, ] <- as.numeric(determinant < nn.obs.d)
  extrapolate[["EC_99quantile"]][3, ] <- as.numeric(determ_noise < nn.obs.dn)
  extrapolate[["EC_99quantile"]][4, ] <- as.numeric(trace / nn.obs.t)
  extrapolate[["EC_99quantile"]][5, ] <- as.numeric(determinant / nn.obs.d)
  extrapolate[["EC_99quantile"]][6, ] <- as.numeric(determ_noise / nn.obs.dn)
  
  cutoff[["99quantile"]] <- c(nn.obs.t, nn.obs.d, nn.obs.dn)
  
  
  Out = list(extrapolate = extrapolate, Pred.var = Lambda.var, 
             trace = trace, determ = determinant, determ_noise = determ_noise, 
             cutoffs = cutoff)
  Out
  
}