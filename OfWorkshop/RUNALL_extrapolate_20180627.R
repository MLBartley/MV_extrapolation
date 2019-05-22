# load("./PredictionCode/20181626/run1_180627.Rdata") #may need to change the data part of this file name
# load("./PredictionCode/20181626/run2_180627.Rdata")
# load("./PredictionCode/20181626/run3_180627.Rdata")
# load("./PredictionCode/20181626/run4_180627.Rdata")
# load("./PredictionCode/20181626/run5_180627.Rdata")
# # load("./PredictionCode/20181626/run6_180627.Rdata")
# # load("./PredictionCode/20181626/run7_180627.Rdata")
# 
# 
# load("./PredictionCode/20181626/pred1_180627.Rdata") #may need to change the data part of this file name
# load("./PredictionCode/20181626/pred2_180627.Rdata")
# load("./PredictionCode/20181626/pred3_180627.Rdata")
# load("./PredictionCode/20181626/pred4_180627.Rdata")
# load("./PredictionCode/20181626/pred5_180627.Rdata")
# # load("./PredictionCode/20181626/pred6_180627.Rdata")
# # load("./PredictionCode/20181626/pred7_180627.Rdata")
# 
# load("./PredictionCode/20181626/pred1_cond_180627.Rdata") #may need to change the data part of this file name
# load("./PredictionCode/20181626/pred2_cond_180627.Rdata")
# load("./PredictionCode/20181626/pred3_cond_180627.Rdata")
# load("./PredictionCode/20181626/pred4_cond_180627.Rdata")
# load("./PredictionCode/20181626/pred5_cond_180627.Rdata")
# # load("./PredictionCode/20181626/pred6_180627.Rdata")
# # load("./PredictionCode/20181626/pred7_180627.Rdata")


files.Rdata <- list.files(path = "./PredictionCode/20181626/",
                          pattern = "180627\\.Rdata$")
files.RData <- list.files(path = "./PredictionCode/20181626/",
                          pattern = "180627\\.RData$")
files.Rdata
files.RData

pasteLoad <- function(file, e){
  filepath <- paste("./PredictionCode/20181626/", file, sep = "")
  
  load(filepath, envir = e)
}

e <- environment()

lapply(files.Rdata, pasteLoad, e)
lapply(files.RData, pasteLoad, e)


source("./Meridith's Work/RUNALL_eda_20180627.R") #gets leverage points and influential points

trace_fun <- function(cov_mat) {
  sum(diag(var(cov_mat)))
}

determ_fun <- function(cov_mat) {
  det(var(cov_mat))
}

n.holdout <- 5

# M <- length(run1$beta[1,1,]) 
  
# Want all B iterations AND B^hat posterior mean estimate

# B_trained <- list(run1$beta, 
#                   run2$beta, 
#                   run3$beta, 
#                   run4$beta, 
#                   run5$beta) #list of arrays
# summary(B_trained)
# 
# B_hats <- list(apply(run1$beta, c(1, 2), mean), 
#                apply(run2$beta, c(1, 2), mean), 
#                apply(run3$beta, c(1, 2), mean), 
#                apply(run4$beta, c(1, 2), mean), 
#                apply(run5$beta, c(1, 2), mean))
# 
# 

#want to calculate var(z) representative values (trace/determ) for training sets

# trace_z_trained <- list(apply(run1$Z, 1, trace_fun), 
#                         apply(run2$Z, 1, trace_fun), 
#                         apply(run3$Z, 1, trace_fun), 
#                         apply(run4$Z, 1, trace_fun), 
#                         apply(run5$Z, 1, trace_fun),
#                          apply(run6$Z, 1, trace_fun),
#                          apply(run7$Z, 1, trace_fun)
                        # ) #need to see how z will be saved

# determ_z_trained <- list(apply(run1$Z, 1, determ_fun),
#                          apply(run2$Z, 1, determ_fun),
#                          apply(run3$Z, 1, determ_fun),
#                          apply(run4$Z, 1, determ_fun),
#                          apply(run5$Z, 1, determ_fun),
#                          apply(run6$Z, 1, determ_fun),
#                          apply(run7$Z, 1, determ_fun)
# ) #need to see how z will be saved

training <- list(run1$Z, run2$Z, run3$Z, run4$Z, run5$Z, run6$Z, run7$Z)
testing <- list(pred1$Y.pred, pred2$Y.pred, pred3$Y.pred, pred4$Y.pred, pred5$Y.pred, 
                pred6$Y.pred, pred7$Y.pred)
# testing.cond <- list(pred1$Y.pred, pred2$Y.pred, pred3$Y.pred, pred4$Y.pred, pred5$Y.pred, 
#                 pred6$Y.pred, pred7$Y.pred)

trace_z_trained <- list()
determ_z_trained <- list()
trace_z_holdout <- list()
determ_z_holdout <- list()

start.time <- Sys.time()
for(h in 1:n.holdout){
  trace_z_trained[[h]] <- apply(training[[h]], 1, trace_fun)
  determ_z_trained[[h]] <- apply(training[[h]], 1, determ_fun)
  trace_z_holdout[[h]] <- apply(testing[[h]], 1, trace_fun)
  determ_z_holdout[[h]] <- apply(testing[[h]], 1, determ_fun)
}
end.time = Sys.time()
elapsed.time = round(difftime(end.time, start.time, units='mins'), dig = 2)
cat('Trace and determinents computed in ', elapsed.time, ' minutes\n\n', sep='') 






#want to calculate var(z) representation values for holdout sets
# 
# trace_z_holdout <- list(apply(pred1$Y.pred, 1, trace_fun), 
#                         apply(pred2$Y.pred, 1, trace_fun), 
#                         apply(pred3$Y.pred, 1, trace_fun), 
#                         apply(pred4$Y.pred, 1, trace_fun), 
#                         apply(pred5$Y.pred, 1, trace_fun) ,
#                          apply(pred6$Y.pred, 1, trace_fun),
#                          apply(pred7$Y.pred, 1, trace_fun)
#                         ) 
# 
# determ_z_holdout <- list(apply(pred1$Y.pred, 1, determ_fun),
#                          apply(pred2$Y.pred, 1, determ_fun),
#                          apply(pred3$Y.pred, 1, determ_fun),
#                          apply(pred4$Y.pred, 1, determ_fun),
#                          apply(pred5$Y.pred, 1, determ_fun),
#                          apply(pred6$Y.pred, 1, determ_fun),
#                          apply(pred7$Y.pred, 1, determ_fun)
#                           )

#visualize results (missing vs non-missing reponses)

## Exploring Cutoff Options - trace

cutoff.max.trace <- list()
cutoff.lev.trace <- list()
cutoff.95.trace <- list()
cutoff.99.trace <- list()

for( i in 1:n.holdout){
  
  n.train <- length(trace_z_trained[[i]])
  n.hold <- length(trace_z_holdout[[i]])
  n.all <- n.train + n.hold
  
  plot(trace_z_trained[[i]], xlim = c(0, n.all), 
       ylim = c(min(trace_z_trained[[i]], trace_z_holdout[[i]]),
                max(trace_z_trained[[i]], trace_z_holdout[[i]])))
  points(trace_z_trained[[i]][leveragePoints[[i]]], col = "yellow", x = leveragePoints[[i]]) #created in RUNALL_eda
  points(trace_z_holdout[[i]], col = "red", x = (n.train+1):(n.all))
  
  #1. Max(var(yhat))
  cutoff.max.trace[[i]] <- max(trace_z_trained[[i]])
  abline(h = cutoff.max.trace[[i]], col = "blue")
  
  #2. leverage (3(k/n) hat equiv)
  cutoff.lev.trace[[i]] <- max(trace_z_trained[[i]][-leveragePoints[[i]]])
  abline(h = cutoff.lev.trace[[i]], col = "purple")
  
  
  #3 & 4. 95% & 99% 
  cutoff.95.trace[[i]] <- quantile(trace_z_trained[[i]], probs = .95)
  cutoff.99.trace[[i]] <- quantile(trace_z_trained[[i]], probs = .99)
  abline(h = cutoff.95.trace[[i]], col = "navy")
  abline(h = cutoff.99.trace[[i]], col = "green")
  
}


cutoff.max.det <- list()
cutoff.lev.det <- list()
cutoff.95.det <- list()
cutoff.99.det <- list()

for( i in 1:n.holdout){
  
  n.train <- length(determ_z_trained[[i]])
  n.hold <- length(trace_z_holdout[[i]])
  n.all <- n.train + n.hold
  
  plot(determ_z_trained[[i]], xlim = c(0, n.all), 
       ylim = c(min(determ_z_trained[[i]], determ_z_holdout[[i]]),
                max(determ_z_trained[[i]], determ_z_holdout[[i]])))
  points(determ_z_trained[[i]][leveragePoints[[i]]], col = "yellow", x = leveragePoints[[i]]) #created in RUNALL_eda
  points(determ_z_trained[[i]], col = "red", x = (n.train+1):(n.all))
  
  #1. Max(var(yhat))
  cutoff.max.det[[i]] <- max(determ_z_trained[[i]])
  abline(h = cutoff.max.det[[i]], col = "blue")
  
  #2. leverage (3(k/n) hat equiv)
  cutoff.lev.det[[i]] <- max(determ_z_trained[[i]][-leveragePoints[[i]]])
  abline(h = cutoff.lev.det[[i]], col = "purple")
  
  
  #3 & 4. 95% & 99% 
  cutoff.95.det[[i]] <- quantile(determ_z_trained[[i]], probs = .95)
  cutoff.99.det[[i]] <- quantile(determ_z_trained[[i]], probs = .99)
  abline(h = cutoff.95.det[[i]], col = "navy")
  abline(h = cutoff.99.det[[i]], col = "green")
  
}


  