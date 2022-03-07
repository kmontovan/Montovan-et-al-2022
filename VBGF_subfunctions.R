## Define the models for growth



####################### Model 1: K=0 (No negative term) ###################### 

#Define the model for growth
Model1 = function(t,ps){
  with(as.list(ps),{
    x1=(H*(1-A)*t+m0^(1-A))^(1/(1-A));
    return(x1);
  })}

#Define a function to calculate the negative log likelihood by comparing a model result to the data
nLL1= function(P,tydat){
  Times = tydat$day
  Ys = tydat$mass
  xout = Model1(Times,c(H=exp(P[["H"]]),A=exp(P[["A"]]),m0=exp(P[["m0"]])))
  negloglikli = -sum(dnorm(x=Ys,mean=xout,exp(P[["S"]]),log=TRUE))
  return(negloglikli)
}

#Define a function to run the optimization to find the parameters that maximize the nLL
RunOptim1 <-  function(theta0,dat){
  P0= c(H=log(theta0[["H"]]), A=log(theta0[["A"]]), m0=log(theta0[["m0"]]), S=log(theta0[["sigma"]]))
  fit=optim(P0,nLL1,tydat=dat)
  out=rbind(c(fit1=c(exp(fit$par),nLL=fit$value,conv.code=fit$convergence)))
  return(out)
}


####################### Model 2: B=1 ###################### 

#Define the model for growth
Model2 = function(t,ps){
  with(as.list(ps),{
    c=log(H-K*m0^(1-A))/((A-1)*K)
    x1=((H-exp((A-1)*K*(t+c)))/K)^(1/(1-A))
    return(x1);
  })
}

#Define a function to calculate the negative log likelihood by comparing a model result to the data
nLL2= function(P,tydat){
  Times = tydat$day
  Ys = tydat$mass
  xout = Model2(Times,c(H=exp(P[["H"]]),K=exp(P[["K"]]),A=exp(P[["A"]]),m0=exp(P[["m0"]])))
  negloglikli = -sum(dnorm(x=Ys,mean=xout,exp(P[["S"]]),log=TRUE))
  return(negloglikli)
}

#Define a function to run the optimization to find the parameters that maximize the nLL
RunOptim2 <-  function(theta0,mydata){
  P0= c(H=log(theta0[["H"]]), K=log(theta0[["K"]]), A=log(theta0[["A"]]), m0=log(theta0[["m0"]]), S=log(theta0[["sigma"]]))
  fit=optim(P0,nLL2,tydat=mydata)
  out=rbind(c(fit1=c(exp(fit$par),nLL=fit$value,conv.code=fit$convergence)))
  return(out)
}


####################### Model 3: Unconstrained ###################### 

# there are no analytical solutions in this case and we need to approach finding the solution numerically
#Here we setup the differentail equation we will use in lsode to numerically find the size as a function of time
gen.gr<-function(t,x,p){
  with(as.list(p),{
    x1 <- H*(x[1])^A-K*x[1]^B
    return(list(c(x1)))
  })
}

Model3 <- function(t,ps){
  pars <- c(ps["H"],ps["A"],ps["K"],ps["B"])
  ic <- c(ps[["m0"]])
  time = unique(sort(c(seq(0,max(t)+1,by=.1),t)))
  gen.gr.out <- lsode(ic,time,gen.gr,pars)
  tmp= data.frame(t=gen.gr.out[,1], Pred=gen.gr.out[,2])
  x1 <- gen.gr.out[match(t,gen.gr.out[,1]),2]
  return(x1)
}

#Define a function to calculate the negative log likelihood by comparing a model result to the data
nLL3= function(P,tydat){
  t = as.numeric(tydat$day) #pull times from data
  mass = as.numeric(tydat$mass) #pull masses from data
  ps=c(H=exp(P[["H"]]),K=exp(P[["K"]]),A=exp(P[["A"]]),B=exp(P[["B"]]),m0=exp(P[["m0"]]))
  xout = Model3(t,ps)
  negloglikli = -sum(dnorm(x=mass,mean=xout,exp(P[["S"]]),log=TRUE))
  return(negloglikli)
}

#Define a function to run the optimization to find the parameters that maximize the nLL
RunOptim3 <-  function(theta0,mydata){
  theta0 = c(H=jitter(1.1),K=jitter(.5),A=jitter(.7),m0=jitter(.6),B=1.2,sigma=5)
  P0= c(H=log(theta0[["H"]]), K=log(theta0[["K"]]), A=log(theta0[["A"]]), m0=log(theta0[["m0"]]),
        B=log(theta0[["B"]]), S=log(theta0[["sigma"]]))
  fit=data.frame(convergence=1)
  fit$convergence=1
  while(fit$convergence==1){
    theta0 = c(H=jitter(1.1),K=jitter(.5),A=jitter(.7),m0=jitter(.6),B=1.2,sigma=5)
    P0= c(H=log(theta0[["H"]]), K=log(theta0[["K"]]), A=log(theta0[["A"]]), m0=log(theta0[["m0"]]),
          B=log(theta0[["B"]]), S=log(theta0[["sigma"]]))
    fit=optim(P0,nLL3,tydat=mydata)
  }
  
  out=data.frame(H=exp(fit$par['H']),K=exp(fit$par['K']),A=exp(fit$par['A']),B=exp(fit$par['B']),m0=exp(fit$par['m0']),S=fit$par['S'],nLL=fit$value,conv.code=fit$convergence)
  return(out)
}

##################### FUNCTIONS FOR MAKING SIZE=M AT TIME T ############################

## Make functions to find H for models 1 and 2 so that 
## the prey reaches the critical size (M) at a certain time T: x_1(T)=M

#For Model 1:
Model1ComputeH = function(ps,T,M){ 
  with(as.list(ps),{ 
    H=((M^(1-A))-m0^(1-A))/((1-A)*T) 
    return(H); 
  }) 
} 

#For Model 2:
Model2ComputeH = function(ps,T,M){
  with(as.list(ps),{
    c=log(H-K*m0^(1-A))/((A-1)*K)
    H=K*(M^(1-A)-m0^(1-A)*exp((A-1)*K*T))/(1-exp((A-1)*K*T))
    return(H);
  })
}

##################### MODIFIED GROWTH FUNCTIONS FOR CANNIBALS ############################

#Modify the growth function to be can_timeshift times faster for the first 2.5 days
gen.gr_can<-function(t,x,p){
  with(as.list(p),{
    if(t<2.5){
      x1 <- (H*(x[1]^A)-(K*(x[1]^B)))*can_timeshift
    }else{
      x1 <- H*(x[1]^A)-(K*(x[1]^B))
    }
    return(list(c(x1)))
  })
}

#setup a new model3 to run witht he new growth function
Model3_can <- function(t,ps){
  pars <- c(ps["H"],ps["A"],ps["K"],ps["B"],ps["can_timeshift"]) #parameters
  ic <- c(ps[["m0"]]) #initial condition
  time = unique(sort(c(seq(0,max(t)+1,by=.05),t))) #times to numerically integrate over
  gen.gr.out <- lsode(ic,time,gen.gr_can,pars) #result of integration
  tmp= data.frame(t=gen.gr.out[,1], Pred=gen.gr.out[,2]) #combine times and predictions of mass
  x1 <- gen.gr.out[match(t,gen.gr.out[,1]),2] #return the mass predictions for the times in the data
  return(x1)
}

#Find the nest value of can_timeshift
nLL3Can= function(P,tydat){
  t = as.numeric(tydat$day) #pull times from data
  mass = as.numeric(tydat$mass) #pull masses from data
  ps=c(H=0.9623584,K=0.4053057,A=1.2793227,B=1.4499259,m0=0.6706620,can_timeshift=exp(P[["can_timeshift"]]))
  xout = Model3_can(t,ps)
  negloglikli = -sum(dnorm(x=mass,mean=xout,exp(P[["S"]]),log=TRUE))
  return(negloglikli)
}


