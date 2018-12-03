### extrapolation_cond function draft

# extrapolate_cond <- function(X,
#                              Y, 
#                              Beta,
#                              Sigma,
#                              Sampled,
#                              Leverage,
#                              Cond.index = 1,
#                              K = NULL,
#                              Theta = NULL,
#                              link = "none") {
#   S = nrow(X) #how many lakes
#   RV <- nrow(Beta) #how many random variables
#   if (is.array(Beta) == FALSE)
#     Beta = array(Beta, nrow = nrow(Beta), ncol = ncol(Beta))
#   mcmc.length = length(Beta[1, 1, ])
#   X.pred = X 
#   X.obs = X[Sampled, ]
#   
#   ## MV version - gets  predicted value of all RV for each lake
#   Mu.mat = array(0, dim = c(S, RV, mcmc.length))
#   for (i in 1:mcmc.length){
#     Mu.mat[, , i] = X.pred %*% t(Beta[, , i])
#   }
# 
#   
#   # MV/UV version - if you just want to look at one reponse conditioned on others
#   
#   #Want to divide up Sigma - see MVN conditional distribution
#   # Sigma11 <- Sigma[Cond.index, Cond.index, i] 
#   Sigma12 <- Sigma[Cond.index ,-Cond.index , ]
#   # Sigma21 <- Sigma[-Cond.index, Cond.index, i]
#   Sigma22 <- Sigma[-Cond.index, -Cond.index, ]
#   
#   f=apply(Y[, -Cond.index], 1,  function(x) which(is.na(x)==T))
# 
#   
#   Mu.cond = array(0, dim = c(S, 1, mcmc.length))
#   
#   f.indx <- which(sapply(f, function(x) length(x) == 3))
#     #all covariates missing (e.g. unsampled lakes), same as before
#     Mu.cond[f.indx,,] = Mu.mat[f.indx, Cond.index, ]
#     
#   for (i in 1: mcmc.length){
#     
#     for (s in 1:S){
#       if (length(f[[s]]) == 0){ # all covariates known (woo hoo!)
#       Mu.cond[s, , i] <- Mu.mat[s, Cond.index , i] - 
#         Sigma12[, i] %*% solve(Sigma22[,,i]) %*% 
#         t(as.matrix(Y[s, -Cond.index] - Mu.mat[s, -Cond.index, i])) 
#     }
#       else if (length(f[[s]]) != 3){ #some missing, only want to condition on known
#     Mu.cond[s, , i] <- Mu.mat[s, Cond.index , i] - 
#                       Sigma12[-(f[[s]]), i] %*% 
#                       solve(Sigma22[-(f[[s]]), -(f[[s]]), i]) %*% 
#                       t(as.matrix(Y[s, -Cond.index][-(f[[s]])] - 
#                                     Mu.mat[s, -Cond.index, i][-(f[[s]])])) 
# 
#     # Sigma.cond <- Sigma11 - Sigma12 %*% solve(Sigma22) %*% Sigma21 
#     #should be scalar?
#         }
#     }
#   }
#   
#   Mu.cond.var <- apply(Mu.cond, c(1, 2), var)
#   # 
#   # Mu.cond.var = apply(Mu.cond, 1, cov_fun)
#   # Mu.var = array(Mu.mat.var, dim = c(RV, RV, S))
# }
# #   
# cutoffs_UV <- function(extrap, Sampled, S) {
#  
#   #Cutoff home
#   extrapolate <- list()
#   cutoff <- list()
# 
#   #Cutoff - max
#   max.obs <- max(extrap[Sampled])
#   
#   extrapolate[["EC_max"]] = matrix(rep(1, S* 2), nrow = 2, ncol = S)
#   #i want two rows: one for binary one for numeric measure
# 
#   extrapolate[["EC_max"]][1, ] <- as.numeric(extrap < max.obs)
#   extrapolate[["EC_max"]][2, ] <- as.numeric(extrap / max.obs)
# 
# 
#   cutoff[["max"]] <- c(max.obs)
# 
#   #Cutoff - max[-leverage]
# 
#   lev.obs <- max(extrap[Sampled][-Leverage])
# 
#   extrapolate[["EC_levmax"]] = matrix(rep(1, S*2), nrow = 2, ncol = S)
# 
#   extrapolate[["EC_levmax"]][1, ] <- as.numeric(extrap < lev.obs)
#   extrapolate[["EC_levmax"]][2, ] <- as.numeric(extrap / lev.obs)
# 
# 
#   cutoff[["levmax"]] <- c(lev.obs)
# 
#   #Cutoff - 95%
# 
#   nf.obs <- quantile(extrap[Sampled], probs = .95)
#    
#   extrapolate[["EC_95quantile"]] = matrix(rep(1, S*2), nrow = 2, ncol = S)
# 
#   extrapolate[["EC_95quantile"]][1, ] <- as.numeric(extrap < nf.obs)
#   extrapolate[["EC_95quantile"]][2, ] <- as.numeric(extrap / nf.obs)
#   
# 
#   cutoff[["95quantile"]] <- c(nf.obs)
# 
#   #Cutoff - 99%
#   nn.obs <- quantile(extrap[Sampled], probs = .99)
#    
#   extrapolate[["EC_99quantile"]] = matrix(rep(1, S*2), nrow = 2, ncol = S)
# 
#   extrapolate[["EC_99quantile"]][1, ] <- as.numeric(extrap < nn.obs)
#   extrapolate[["EC_99quantile"]][2, ] <- as.numeric(extrap / nn.obs)
#  
# 
#   cutoff[["99quantile"]] <- c(nn.obs)
# 
# 
#   Out = list(cond.extrap = extrapolate, cutoffs = cutoff)
#   Out
# }