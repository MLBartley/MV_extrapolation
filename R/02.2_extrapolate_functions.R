trace_fun <- function(cov_mat) {
  sum(diag(var(t(cov_mat))))
}

determ_fun <- function(cov_mat) {
  det(var(t(cov_mat)))
}

cov_fun <- function(cov_mat) {
  #need this bc transpose
  var(t(cov_mat))
}

#' Multivariate Response Extrapolation Measures and Cutoff Assignments
#'
#' @param X LAGOS Covariance Matrix with intercept. 
#' @param Beta MCMC values for Beta matrix 
#' @param Sigma MCMC value for Sigma matrix
#' @param Sampled The index for which lakes have been sampled (i.e. have 
#' covariate information). 
#' @param Leverage Indexes for which of the observed lakes should not be 
#' considered for the max.lev cutoff. 
#' @param K NULL - only need if spatial component included in model.
#' @param Theta NULL - only need if included in model - not here
#' @param link "none" - only needed for non-linear model (e.g. log link) 
#'
#' @return Returns list of extrapolation measures, predictive variance matrices, 
#' trace values, determinant values with and without noise included, and cutoff
#' values. 
#' @export
#'
# @examples
extrapolate <-
  function(X,
           Beta,
           Sigma,
           Sampled,
           Leverage,
           # Cond.index = 1,
           K = NULL,
           Theta = NULL,
           link = "none") {
    S = nrow(X)
    RV <- nrow(Beta)
    if (is.array(Beta) == FALSE)
      Beta = array(Beta, nrow = nrow(Beta), ncol = ncol(Beta))
    mcmc.length = length(Beta[1, 1, ])
    X.pred = X
    X.obs = X[Sampled, ]
    # if (is.null(K) & !is.null(Theta))
    #   K = diag(ncol(Theta))
    
    ######### Is this needed? #################
    Beta.var = apply(Beta, 3, cov_fun)
    Beta.var <-  array(Beta.var,
                       dim = c(RV, RV, 
                               ncol(Beta.var)))
    ###########################################
    
    
    # Mu.var = X.pred %*% tcrossprod(Beta.var, X.pred)
    
    ## MV version - gets value per lake
    Mu.mat = array(0, dim = c(S, RV, mcmc.length))
    for (i in 1:mcmc.length){
      Mu.mat[, , i] = X.pred %*% t(Beta[, , i])
    }
    
    
    # MV/UV version - if you just want to look at one reponse conditioned on others
    
    # Mu.mat.UVcond = array(0, dim = c(S, 1, mcmc.length))
    # for (i in 1: mcmc.length){
    #   Sigma11 <- Sigma[Cond.index, Cond.index, i]
    #   Sigma12 <- Sigma[Cond.index ,-Cond.index , i]
    #   Sigma21 <- Sigma[-Cond.index, Cond.index, i]
    #   Sigma22 <- Sigma[-Cond.index, -Cond.index, i]
    #     
    #   mu.cond <- Mu.mat[, Cond.index , i] - Sigma12 %*% solve(Sigma22) %*% () ## Need to know X_-k ??? 
    #   Sigma.cond <- Sigma11 - Sigma12 %*% solve(Sigma22) %*% Sigma21
    # }
    
    
    
    
#spatial aspect of model    
    # if (!is.null(Theta)) {
    #   Mu.var = Mu.var + K %*% tcrossprod(cov(Theta), K)
    #   for (i in 1:mcmc.length)
    #     Mu.mat[, i] = Mu.mat[, i] + K %*% Theta[i, ]
    # }
    
    ####### Is this needed? ########
    Mu <- apply(Mu.mat, 3, 'median')
    ################################
    
    Mu.mat.var = apply(Mu.mat, 1, cov_fun)
    Mu.var = array(Mu.mat.var, dim = c(RV, RV, S))

    #why median??
    # small = 0.00000001
    # if (link == "log")
    #   Lambda.var = (exp(Mu)) ^ 2 * diag(Mu.var)
    # if (link == "logit")
    #   Lambda.var = (exp(Mu) / (1 + exp(Mu)) ^ 2) ^ 2 * diag(Mu.var)
    # if (link == "probit")
    #   Lambda.var = ((pnorm(Mu + small) - pnorm(Mu)) / small) ^ 2 * diag(Mu.var)
    # if (link == "none") 
      Lambda.var = Mu.var
      Lambda.noise <-array(0, dim = dim(Mu.var))
      for (i in 1:length(Mu.var[1,1,])) {
        Lambda.noise[,,i] <- Mu.var[,,i] + apply(Sigma, c(1,2), 'median')
      }
      
    #MV version - 
   
    trace <- apply(Lambda.var, 3, trace_fun)
    determinant <- apply(Lambda.var, 3, determ_fun)
    determ_noise <- apply(Lambda.noise, 3, determ_fun)
    
    #Single Version
    
    
    
    #Cutoff home
    extrapolate <- list()
    cutoff <- list()
    
    #Cutoff - max
    max.obs.t = max(trace[Sampled])
    max.obs.d = max(determinant[Sampled])
    max.obs.dn <- max(determ_noise[Sampled])
    extrapolate[["EC_max"]] = matrix(rep(1, S*2), nrow = 6, ncol = S)
    
    extrapolate[["EC_max"]][1, ] <- as.numeric(trace <= max.obs.t)
    extrapolate[["EC_max"]][2, ] <- as.numeric(determinant <= max.obs.d)
    extrapolate[["EC_max"]][3, ] <- as.numeric(determ_noise <= max.obs.dn)
    extrapolate[["EC_max"]][4, ] <- as.numeric(trace / max.obs.t)
    extrapolate[["EC_max"]][5, ] <- as.numeric(determinant / max.obs.d)
    extrapolate[["EC_max"]][6, ] <- as.numeric(determ_noise / max.obs.dn)
    
    cutoff[["max"]] <- c(max.obs.t, max.obs.d, max.obs.dn)
    
    #Cutoff - max[-leverage]
    
    lev.obs.t <- max(trace[Sampled][-Leverage])
    lev.obs.d = max(determinant[Sampled][-Leverage])
    lev.obs.dn = max(determ_noise[Sampled][-Leverage])
    
    extrapolate[["EC_levmax"]] = matrix(rep(1, S*2), nrow = 6, ncol = S)
   
     extrapolate[["EC_levmax"]][1, ] <- as.numeric(trace <= lev.obs.t)
    extrapolate[["EC_levmax"]][2, ] <- as.numeric(determinant <= lev.obs.d)
    extrapolate[["EC_levmax"]][3, ] <- as.numeric(determ_noise <= lev.obs.dn)
    extrapolate[["EC_levmax"]][4, ] <- as.numeric(trace / lev.obs.t)
    extrapolate[["EC_levmax"]][5, ] <- as.numeric(determinant / lev.obs.d)
    extrapolate[["EC_levmax"]][6, ] <- as.numeric(determ_noise / lev.obs.dn)
    
    cutoff[["levmax"]] <- c(lev.obs.t, lev.obs.d, lev.obs.dn)
    
    #Cutoff - 95%
    
    nf.obs.t <- quantile(trace[Sampled], probs = .95)
    nf.obs.d = quantile(determinant[Sampled], probs = .95)
    nf.obs.dn <- quantile(determ_noise[Sampled], probs = .95)
    extrapolate[["EC_95quantile"]] = matrix(rep(1, S*2), nrow = 6, ncol = S)
    
    extrapolate[["EC_95quantile"]][1, ] <- as.numeric(trace <= nf.obs.t)
    extrapolate[["EC_95quantile"]][2, ] <- as.numeric(determinant <= nf.obs.d)
    extrapolate[["EC_95quantile"]][3, ] <- as.numeric(determ_noise <= nf.obs.dn)
    extrapolate[["EC_95quantile"]][4, ] <- as.numeric(trace / nf.obs.t)
    extrapolate[["EC_95quantile"]][5, ] <- as.numeric(determinant / nf.obs.d)
    extrapolate[["EC_95quantile"]][6, ] <- as.numeric(determ_noise / nf.obs.dn)
    
    cutoff[["95quantile"]] <- c(nf.obs.t, nf.obs.d, nf.obs.dn)
    
    #Cutoff - 99%
    nn.obs.t <- quantile(trace[Sampled], probs = .99)
    nn.obs.d = quantile(determinant[Sampled], probs = .99)
    nn.obs.dn <- quantile(determ_noise[Sampled], probs = 0.99)
    extrapolate[["EC_99quantile"]] = matrix(rep(1, S*2), nrow = 6, ncol = S)
    
    extrapolate[["EC_99quantile"]][1, ] <- as.numeric(trace <= nn.obs.t)
    extrapolate[["EC_99quantile"]][2, ] <- as.numeric(determinant <= nn.obs.d)
    extrapolate[["EC_99quantile"]][3, ] <- as.numeric(determ_noise <= nn.obs.dn)
    extrapolate[["EC_99quantile"]][4, ] <- as.numeric(trace / nn.obs.t)
    extrapolate[["EC_99quantile"]][5, ] <- as.numeric(determinant / nn.obs.d)
    extrapolate[["EC_99quantile"]][6, ] <- as.numeric(determ_noise / nn.obs.dn)
    
    cutoff[["99quantile"]] <- c(nn.obs.t, nn.obs.d, nn.obs.dn)
  
    
    Out = list(extrapolate = extrapolate, Pred.var = Lambda.var, 
               trace = trace, determ = determinant, determ_noise = determ_noise, 
               cutoffs = cutoff)
    Out
  }

#' Single Variable Conditional Extrapoplation Measure 
#'
#' @param X LAGOS Covariance Matrix with intercept. 
#' @param Y LAGOS Multivariate Response data for 4 variables.
#' @param Beta MCMC values for Beta matrix 
#' @param Sigma MCMC value for Sigma matrix
#' @param Sampled The index for which lakes have been sampled (i.e. have 
#' covariate information). 
#' @param Leverage Indexes for which of the observed lakes should not be 
#' considered for the max.lev cutoff. 
#' @param Cond.index Index for chosen repsonse variable to focus on. 
#' @param K NULL - only need if spatial component included in model.
#' @param Theta NULL - only need if included in model - not here
#' @param link "none" - only needed for non-linear model (e.g. log link) 
#'
#' @return
#' @export
#'
#' @examples
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
  
  #Want to divide up Sigma - see MVN conditional distribution
  Sigma11 <- Sigma[Cond.index, Cond.index, ] 
  Sigma12 <- Sigma[Cond.index ,-Cond.index, ]
  Sigma21 <- Sigma[-Cond.index, Cond.index, ]
  Sigma22 <- Sigma[-Cond.index, -Cond.index, ]
  
  f=apply(Y[, -Cond.index], 1,  function(x) which(is.na(x)==T))
  
  
  Mu.cond = array(0, dim = c(S, 1, mcmc.length))
  
  f.indx <- which(sapply(f, function(x) length(x) == 3))
  # all covariates missing (e.g. unsampled lakes), almost same as before
  # but need to add Var(sigma^hat | Y) bc rest is 
  # var(y_in |Y) = var(y^hat_i | Y) + var (sigma^hat | Y)
  Mu.cond[f.indx,,] = Mu.mat[f.indx, Cond.index, ] + 
    matrix(rep(Sigma11[], length(f.indx)), 
              length(f.indx), mcmc.length) 
  
  for (i in 1: mcmc.length){

    for (s in 1:S){
      if (length(f[[s]]) == 0){ # all covariates known (woo hoo!)
        Mu.cond[s, , i] <- rmvnorm(1, 
                                   Mu.mat[s, Cond.index , i] + 
                                      Sigma12[, i] %*% solve(Sigma22[,,i]) %*%
                                      t(as.matrix(Y[s, -Cond.index] - 
                                                    Mu.mat[s, -Cond.index, i])), 
                                    Sigma11[i] - Sigma12[, i] %*% 
                                      solve(Sigma22[, , i]) %*% Sigma21[, i])    
      }
      else if (length(f[[s]]) != 3){ #some missing, only want to condition on known
        Mu.cond[s, , i] <- rmvnorm(1, 
                                   Mu.mat[s, Cond.index , i] + #should this be +? MLB - feb 15 2019
                                     Sigma12[-(f[[s]]), i] %*%
                                     solve(Sigma22[-(f[[s]]), -(f[[s]]), i]) %*%
                                     t(as.matrix(Y[s, -Cond.index][-(f[[s]])] -
                                                   Mu.mat[s, -Cond.index, i][-(f[[s]])])), 
                                   Sigma11[i] - Sigma12[-(f[[s]]), i] %*% 
                                     solve(Sigma22[-(f[[s]]), -(f[[s]]), i]) %*% Sigma21[-(f[[s]]), i]
                                   )
      } #ends else if
    } #ends s in 1:S
  } # ends i in 1:mcmc.length
  
  Mu.cond.var <- apply(Mu.cond, c(1, 2), var)
}


#' Single Variable Conditional Extrapoplation Cutoff Assignments
#'
#' @param extrap 
#' @param Sampled 
#' @param S 
#'
#' @return
#' @export
#'
#' @examples
cutoffs_UV <- function(extrap, Sampled, S) {
  
  #Cutoff home
  extrapolate <- list()
  cutoff <- list()
  
  #Cutoff - max
  max.obs <- max(extrap[Sampled])
  
  extrapolate[["EC_max"]] = matrix(rep(1, S* 2), nrow = 2, ncol = S)
  #i want two rows: one for binary one for numeric measure
  
  extrapolate[["EC_max"]][1, ] <- as.numeric(extrap <= max.obs)
  extrapolate[["EC_max"]][2, ] <- as.numeric(extrap / max.obs)
  
  
  cutoff[["max"]] <- c(max.obs)
  
  #Cutoff - max[-leverage]
  
  lev.obs <- max(extrap[Sampled][-Leverage])
  
  extrapolate[["EC_levmax"]] = matrix(rep(1, S*2), nrow = 2, ncol = S)
  
  extrapolate[["EC_levmax"]][1, ] <- as.numeric(extrap <= lev.obs)
  extrapolate[["EC_levmax"]][2, ] <- as.numeric(extrap / lev.obs)
  
  
  cutoff[["levmax"]] <- c(lev.obs)
  
  #Cutoff - 95%
  
  nf.obs <- quantile(extrap[Sampled], probs = .95)
  
  extrapolate[["EC_95quantile"]] = matrix(rep(1, S*2), nrow = 2, ncol = S)
  
  extrapolate[["EC_95quantile"]][1, ] <- as.numeric(extrap <= nf.obs)
  extrapolate[["EC_95quantile"]][2, ] <- as.numeric(extrap / nf.obs)
  
  
  cutoff[["95quantile"]] <- c(nf.obs)
  
  #Cutoff - 99%
  nn.obs <- quantile(extrap[Sampled], probs = .99)
  
  extrapolate[["EC_99quantile"]] = matrix(rep(1, S*2), nrow = 2, ncol = S)
  
  extrapolate[["EC_99quantile"]][1, ] <- as.numeric(extrap <= nn.obs)
  extrapolate[["EC_99quantile"]][2, ] <- as.numeric(extrap / nn.obs)
  
  
  cutoff[["99quantile"]] <- c(nn.obs)
  
  
  Out = list(cond.extrap = extrapolate, cutoffs = cutoff)
  Out
}