##functions transfered to "extrapolate_functions.R"
##new code in "RUNALL_extraoplate_20180907.R"

load("./rdata-data/runAll_180808.Rdata")
load("./rdata-data/predALL_180815.Rdata")

source("./R/RUNALL_eda_20180810.R") #gets leverage points and influential points



n.holdout <- 1

##create indexes for missing single/all three WQ response variables


training <- list(runALL$Z) #need to split in to missing P, N, Chl-A, all 3
testing <- list(predALL$Y.pred) 
# testing.cond <- list(pred1$Y.pred, pred2$Y.pred, pred3$Y.pred, pred4$Y.pred, pred5$Y.pred, 
#                 pred6$Y.pred, pred7$Y.pred)

# trace_determ_list <- list()
load(file = "./rdata-data/trace_determ_list_20180817.Rdata")

# trace_z_trained <- list()
# determ_z_trained <- list()
#  trace_z_holdout <- list()
#  determ_z_holdout <- list()

start.time <- Sys.time()
for(h in 1:n.holdout){
  # trace_determ_list$t_trained[[h]] <- apply((training[[h]]), 1, trace_fun)
  # trace_determ_list$d_trained[[h]] <- apply((training[[h]]), 1, determ_fun) #currenlty always zero
  trace_determ_list$cov_trained[[h]] <- apply((training[[h]]), 1, cov_fun) #Make this into an array in the list?
  # trace_determ_list$t_holdout[[h]] <- apply((testing[[h]]), 1, trace_fun)
  # trace_determ_list$d_holdout[[h]] <- apply((testing[[h]]), 1, determ_fun) 
}
end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Trace and determinents computed in ', elapsed.time, ' minutes\n\n', sep='')

trace_determ_list$cov_trained[[1]] <- array(trace_determ_list$cov_trained[[1]],
                                            dim = c(4, 4, 
                                                    ncol(trace_determ_list$cov_trained[[1]])))

save(trace_determ_list, file = "./rdata-data/trace_determ_list_20180817.Rdata")

#visualize results (missing vs non-missing reponses)

## Exploring Cutoff Options - trace
cutoff <-list()

# cutoff.max.trace <- list()
# cutoff.lev.trace <- list()
# cutoff.95.trace <- list()
# cutoff.99.trace <- list()

for( i in 1:n.holdout){
  
  n.train <- length(trace_determ_list$t_trained[[i]]) #same for determin values
  n.hold <- length(trace_determ_list$t_holdout[[i]])
  n.all <- n.train + n.hold
  
  plot(trace_determ_list$t_trained[[i]], xlim = c(0, n.all), 
       ylim = c(min(trace_determ_list$t_trained[[i]], trace_determ_list$t_holdout[[i]]),
                max(trace_determ_list$t_trained[[i]], trace_determ_list$t_holdout[[i]])))
  points(trace_determ_list$t_trained[[i]][leveragePoints[[i]]], 
         col = "yellow", x = leveragePoints[[i]]) #created in RUNALL_eda
  points(trace_determ_list$t_holdout[[i]], col = "red", x = (n.train+1):(n.all))
  
  #1. Max(var(yhat))
  cutoff$max.trace[[i]] <- max(trace_determ_list$t_trained[[i]])
  abline(h = cutoff$max.trace[[i]], col = "blue")
  
  #2. leverage (3(k/n) hat equiv)
  cutoff$lev.trace[[i]] <- max(trace_determ_list$t_trained[[i]][-leveragePoints[[i]]])
  abline(h = cutoff$lev.trace[[i]], col = "purple")
  
  
  #3 & 4. 95% & 99% 
  cutoff$ninefive.trace[[i]] <- quantile(trace_determ_list$t_trained[[i]], probs = .95)
  cutoff$ninenine.trace[[i]] <- quantile(trace_determ_list$t_trained[[i]], probs = .99)
  abline(h = cutoff$ninefive.trace[[i]], col = "navy")
  abline(h = cutoff$ninenine.trace[[i]], col = "green")
  
#determinent plots
  
  # plot(trace_determ_list$d_trained[[i]], xlim = c(0, n.all), 
  #      ylim = c(min(trace_determ_list$d_trained[[i]], trace_determ_list$d_holdout[[i]]),
  #               max(trace_determ_list$d_trained[[i]], trace_determ_list$d_holdout[[i]])))
  # points(trace_determ_list$d_trained[[i]][leveragePoints[[i]]], 
  #        col = "yellow", x = leveragePoints[[i]]) #created in RUNALL_eda
  # points(trace_determ_list$d_trained[[i]], col = "red", x = (n.train+1):(n.all))
  # 
  # #1. Max(var(yhat))
  # cutoff$max.det[[i]] <- max(trace_determ_list$d_trained[[i]])
  # abline(h = cutoff$max.det[[i]], col = "blue")
  # 
  # #2. leverage (3(k/n) hat equiv)
  # cutoff$lev.det[[i]] <- max(trace_determ_list$d_trained[[i]][-leveragePoints[[i]]])
  # abline(h = cutoff$lev.det[[i]], col = "purple")
  # 
  # 
  # #3 & 4. 95% & 99% 
  # cutoff$ninefive.det[[i]] <- quantile(trace_determ_list$d_trained[[i]], probs = .95)
  # cutoff$ninenine.det[[i]] <- quantile(trace_determ_list$d_trained[[i]], probs = .99)
  # abline(h = cutoff$ninefive.det[[i]], col = "navy")
  # abline(h = cutoff$ninenine.det[[i]], col = "green")
  
}

#Missing Phosphorous

phos.vary <- trace_determ_list$cov_trained[[1]][1, 1, ]
phos.vary <- phos.vary[phos.vary > 0]

#Missing Nitrogen

nitr.vary <- trace_determ_list$cov_trained[[1]][2, 2, ]
nitr.vary <- nitr.vary[nitr.vary > 0]

#Missing Chlorophil

chla.vary <- trace_determ_list$cov_trained[[1]][3, 3, ]
chla.vary <- chla.vary[chla.vary > 0]

#Missing all 3 (maybe Secchi)
a <- c(0, 0, 0, 1)
b <- c(0, 0, 0, 0)

  
idx.3miss <- which(apply(runALL$data$Obs.mat, 1, function(x) all(x == a | b)))



miss3.trace <- apply(trace_determ_list$cov_trained[[1]][1:3, 1:3, idx.3miss], 3, function(x) sum(diag(x)))

###ISSUES

# 1. Need to pull forward lakeIDs


# 2. How to compare univariate/multivariate/semi-multivariate measures?
  #divide by number of response variables used?

# 3. Do we use UV or sMV measures to choose cutoff for testing measures?

# 4. can we swap Z to be all predictions for Y? less variance for known values? 
  #but still better to compare?

  