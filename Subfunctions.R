RunModel <- function(param,ODE_func){
  Survival <- numeric(2)
  for(j in 0:1){ #run once with foaraging, once without
    param[['forage']]=j
    out=SolveSystem(param,ODE_func) 
    Survival[j+1]=tail(out[,3],1)
  }
  Size=out[first(which(out[,1]>=round(as.numeric(param['PredStart']),2))),2]
  #Compare survival to maturity for both strategies, record the best.
  if(Survival[1]<=0 & Survival[2]<=0){BestChoice='BothNegative'
  } else if(Survival[1]>Survival[2]){BestChoice='forage'
  } else{BestChoice='hide'}
  return(c(Size,max(Survival),BestChoice))
}

RunModel2 <- function(param,ODE_func){
  param[['forage']]=1; outh=SolveSystem(param,ODE_func) #hide
  param[['forage']]=0; outf=SolveSystem(param,ODE_func) #forage
  Survival=data.frame(x2h=tail(outh[,3],1),x2f=tail(outf[,3],1))

  Size=outh[first(which(outh[,1]>=round(as.numeric(param['PredStart']),2))),2]
  if(Survival[1]<0 & Survival[2]<0){
    BestChoice='BothNegative'
  } else if(Survival[2]>Survival[1]){
    BestChoice='forage'
  } else{
    BestChoice='hide'
  }
  return(data.frame(Size,Survival,Choice=BestChoice))
}

#Create a function that will define the state variables, timeframe 
# and will numerically find the solution for the ODE. Note, if a
# prey is less than $M$ mg at the end of the timeframe 
# then their survival is reset to 0. Growth also stops at $M$, 
# the critical size, regardless of when the prey reaches this size. 
SolveSystem<- function(param,ODE_func){
  times <- seq(param[['tmin']], param[['tmax']], by = 0.01)
  state <- c(x1 = param[['m0']], x2 = 1)
  out <- ode(func = ODE_func, y = state, parms = param, times = times)
  if(is.na(out[[length(out[,2]),2]])){
    print("Error")
    out=out[1,]
    out[3]=-999
  }else if(out[[length(out[,2]),2]]<param[['M']]){
    out[[length(out[,3]),3]]<-0
  } else{
    end=head(which((out[,2]>param[['M']])),n=1)
    out=out[1:end,]
  }
  return=out
}

# This is the general ODE model for prey growth and survival
# $x1$ is the size of the prey, and $x2$ is the probability 
# the prey survives to the critical mass in the allotted time period.
# Was PotatoBeetle
General_ODE <- function(t, state, param) {
  with(as.list(c(state, param)), {
    pre1= 1*(t<PredStart+PredVisitLength)*(t>PredStart) #Make a vector for each timestep with whether the predator is there
    u= 1*(t>=0) - forage*pre1 #forage unless the predator is present and you don't forage when pred is present
    if(t<2.5){
      dx1 <- u*(H*(x1^A)-K*(x1^B))*can_timeshift
    }else{
      dx1 <- u* (H*(x1^A)-K*(x1^B))
    }
    dx2 <- (-d*x2)-(m*u+b)*(1-(1-alpha)*x1/M)*p*x1*x2*pre1
    list(c(dx1, dx2))
  })
}

# Computes the parameter H for the VBGF growth model with B=1
# that makes the prey reach size M at time T when the starting size is m0 (at t=0)
Model2ComputeH = function(ps){
  with(as.list(ps),{
    c=log(H-K*m0^(1-A))/((A-1)*K)
    H=K*(M^(1-A)-m0^(1-A)*exp((A-1)*K*T))/(1-exp((A-1)*K*T))
    return(H);
  })
}

# Solve the ODE just for the growth function 'growth_func' and return the
# size at each timestep. 
GetGrowthCurve <- function(param,growth_func){
  times <- seq(param[['tmin']], param[['T']], by = 0.01)
  state <- c(x1 = param[['m0']])
  out <- as.data.frame(ode(func = growth_func, y = state, parms = param, times = times))
  return(out)
}

#This is the von Bertalanffy Growth Function. The change in the size
# (x1) is computed as a function of parameters and the current size(state)
# This function is used in an ODE solver to get the size through time.
# was PotatoBeetle_G
VBGF_growth <- function(t, state, param) {
  with(as.list(c(state, param)), {
    if(t<2.5){
      dx1 <- (H*(x1^A)-K*(x1^B))*can_timeshift
    }else{
      dx1 <- (H*(x1^A)-K*(x1^B))
    }
    list(c(dx1))
  })
}

# Make the plot of elasticities for the sensitivity analysis
LollyPopPlot <- function(y, names,ylimits,nvar) {
  par(mfrow=c(1,1),cex.axis=2,cex.lab=2)
  px=1:nvar;
  matplot(px,y,type="p",col=c("black"), pch=15, cex=2.3,cex.axis=1,cex.lab=1.4,
          xaxt="n",xlab="Parameters",ylab="Elasticity", ylim=ylimits,xlim=c(0.75,nvar+.25)); 
  axis(side=1,at=1:nvar,labels=names,cex.axis=1.2);
  points(c(0,nvar+1),c(0,0), type="l", lty=2, col="black")
  
  for(j in 1:nvar) {
    points(rep(px[j],2),c(0,y[j]),type="l",lwd=2,col="black")
  }
}
rescale<- function(x) {
  return((rank(x)-1)/(length(x)-1))
}
