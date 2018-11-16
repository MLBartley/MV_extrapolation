library(Matrix)
library(mvtnorm)
library(MCMCpack)

beta.update=function(data,pars,priors){
  A=rep(0,data$p*data$K)	
  B=solve(priors$Sigma.beta)
  Sigma.Inv=solve(pars$Sigma)
  for(i in 1:data$n){
      B=B+kronecker(diag(1,data$K),data$X[i,])%*%Sigma.Inv%*%t(kronecker(diag(1,data$K),data$X[i,]))
      A=A+kronecker(diag(1,data$K),data$X[i,])%*%Sigma.Inv%*%pars$Z[i,]
  }
  pars$beta=as.vector(rmvnorm(1,solve(B)%*%A,solve(B)))
  return(pars)	
}

# beta.update=function(data,pars,priors){
# A=kronecker(diag(data$K),t(data$X))%*%kronecker(Diagonal(data$n),solve(pars$Sigma))%*%as.vector(t(pars$Z))
# B=kronecker(diag(data$K),t(data$X))%*%kronecker(Diagonal(data$n),solve(pars$Sigma))%*%kronecker(diag(data$K),(data$X))+solve(priors$Sigma.beta)
  # pars$beta=as.vector(rmvnorm(1,as.vector(solve(B)%*%A),as.matrix(solve(B))))
  # return(pars)	
# }



Sigma.update=function(data,pars,priors){
  TmXB=t(pars$Z-matrix(kronecker(diag(data$K),data$X)%*%pars$beta,ncol=data$K))
  pars$Sigma=riwish(data$n+priors$nu,TmXB%*%t(TmXB)+priors$Tau)	
  return(pars)	
}

l.ob=function(x){return(length(which(is.na(x)!=T)))}

Z.update=function(data,pars,priors){
  C=pars$Sigma
  for(i in which(apply(data$Obs.mat,1,sum)!=data$K)){
      Mu=as.vector(t(kronecker(diag(1,data$K),data$X[i,]))%*%pars$beta)
      f=which(is.na(data$Y[i,])==T)
      pars$Z[i,f]=c(rmvnorm(1,mean=Mu[f]+C[f,-f]%*%solve(C[-f,-f])%*%(pars$Z[i,-f]-Mu[-f]),
                           sigma=C[f,f]-C[f,-f]%*%solve(C[-f,-f])%*%C[-f,f]))
    }
return(pars)}
  

drive.lakes=function(data,pars,priors,iters,print.out){
      out=list()
      out$data=data
      out$priors=priors
      out$beta=array(dim=c(data$K,data$p,iters+1))
      out$beta[,,1]=matrix(pars$beta,ncol=data$p,byrow=T)
      out$Sigma=array(dim=c(data$K,data$K,iters+1))
      out$Sigma[,,1]=pars$Sigma
      out$Z=array(dim=c(data$n,data$K,iters+1))
      out$Z[,,1]=pars$Z
        
      for(j in 1:iters){
        pars=Z.update(data,pars,priors)
        pars=Sigma.update(data,pars,priors)
        pars=beta.update(data,pars,priors)
      if(j%%print.out==0){
        print(j)
      }
        out$beta[,,j+1]=matrix(pars$beta,ncol=data$p,byrow=T)
        out$Sigma[,,j+1]=pars$Sigma
        out$Z[,,j+1]=pars$Z   
      }  
      return(out)
}


predict.all=function(run,b,e,iters){
		
newX=as.matrix(run$data$Xtest)
Y.pred=array(dim=c(nrow(newX),ncol(run$data$Y),iters))

  s=sample(b:e,iters)
  for(j in 1:iters){
    for(i in 1:nrow(newX)){
    	
    Y.pred[i,,j]=as.vector(rmvnorm(1,as.vector(newX[i,]%*%t(run$beta[,,s[j]])), run$Sigma[,,s[j]]))	
    }
  }
  out=list()
  out$Y.pred=Y.pred
  out$Y.true=run$data$Ytest
  return(out)
}




predict.cond=function(run,b,e,iters){
 
newX=as.matrix(run$data$Xtest) 
 
  Y.cond=array(dim=c(nrow(newX),ncol(run$data$Y),iters))
  s=sample(b:e,iters)
    for(i in 1:nrow(newX)){
    	if(is.na(run$data$Ytest[i,4])==T){
    	Y.cond[i,,]=NA	
    	#for(j in 1:iters){
    	#Y.cond[i,,j]=as.vector(rmvnorm(1,as.vector(newX[i,]%*%t(run$beta[,,s[j]])), run$Sigma[,,s[j]]))	
    	#}	
    	}
    	if(is.na(run$data$Ytest[i,4])!=T){
    	for(j in 1:iters){
    		f=4 #this assumes secchi is the 4th variable
    		Y.cond[i,f,j]=run$data$Ytest[i,f]
    		C=run$Sigma[,,s[j]]
            Mu=newX[i,]%*%t(run$beta[,,s[j]])
            Y.cond[i,-f,j]=rmvnorm(1,mean=Mu[-f]+C[-f,f]%*%solve(C[f,f])%*%(run$data$Ytest[i,f]-Mu[f]),sigma=C[-f,-f]-C[-f,f]%*%solve(C[f,f])%*%C[f,-f])	
    	}	
    	}
    }	

  out=list()
  out$Y.cond=Y.cond
  out$Y.true=run$data$Ytest
  
  return(out)
}
