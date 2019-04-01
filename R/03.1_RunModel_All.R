library(LAGOSNE)
library(mvtnorm)

source(here::here("R", "02.1_MCMCAlgorithm_functions.R"))
load("./rdata-data/AllDataList_180627v2.Rdata") #created in cluster.datamanage.R
load("./rdata-data/MissingDataList_180815.Rdata")
# source("subsetData.R")

# subData=subsetData(AllDataList_180627)[[1]] ############

data=list()
data$Y=as.matrix(AllDataList_180627$logY) #now not split into testing/training
data$n=nrow(data$Y) #number of lakes
data$K=ncol(data$Y)  #number of variables

data$X=as.matrix(cbind(rep(1,data$n),AllDataList_180627$X[,-c(1,2)])) # Covariate matrix (remove lat and lon)
data$p =ncol(data$X) #number of covariates (including intercept)
data$Obs.mat=matrix(0,nrow=data$n,ncol=data$K) #compute which nutrients observed for each lake
for(k in 1:data$K){data$Obs.mat[which(is.na(data$Y[,k])!=T),k]=1}

data$LLxtest = MissingDataList_100815$X[,1:2] ###### Put the lat and lons here!
data$LLxtrain = AllDataList_180627$X[,1:2]

 data$Ytest=as.matrix(MissingDataList_100815$logY)
 data$Xtest=as.matrix(cbind(rep(1,nrow(MissingDataList_100815$X)), 
                            MissingDataList_100815$X[,-c(1,2)]))
 
 

priors=list()
priors$Sigma.beta=diag(1000,data$K*data$p)
priors$Tau=diag(1,data$K)
priors$nu=data$K+1

pars=list()
#Beta=matrix(runif(data$K*data$p),nrow=data$K,ncol=data$p)
Beta=matrix(0,nrow=data$K,ncol=data$p)
for(k in 1:data$K){Beta[k,]=as.vector(lm(data$Y[,k]~-1+data$X)$coef)}
pars$beta=as.vector(t(Beta))
pars$Sigma=diag(.5,data$K)
pars$Z=matrix(0,ncol=data$K,nrow=data$n)
pars$Z=data$Y
pars$Z=Z.update(data,pars,priors)$Z


runALL=drive.lakes(data = data, pars = pars, priors = priors,
iters = 20000, print.out = 100) ##################
  save(runALL,file="./rdata-data/runAll_190326.Rdata")					#################
#

load("./rdata-data/runAll_180808.Rdata")
predALL=predict.all(run = runALL, b = 500, e = 2000, iters = 200)	################
# save(predALL,file="./rdata-data/predALL_180815.Rdata")				#######################
save(predALL,file="./rdata-data/predALL_190328.Rdata")
# 
# predALL.cond = predict.cond(runALL, 500, 2000, 200)
# save(predALL.cond, file = "pred1_cond_180627.RData")


##ISSUES/TODO##


#1) what about testing data?? testing code currently commented out but still needed

    #went though code to grab LPEP data and found 723 testing lakes.





