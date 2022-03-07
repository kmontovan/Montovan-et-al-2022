library(deSolve)
library(reshape2)
library(ggplot2)
library(dplyr)
library(permute)
library(gridExtra)

#In this file I explore the reasonable parameter regimes and look at a range of predator arrvial times and stay 
#durations for each to determine how sensitive the pattern of cannibals hiding when non-cannibals forage is to 
#the choice of parameter values. The fraction of these where the cannibal kides but the non-canibal doesn't is recorded.

#This file relies on a subfile containing the functions that define the differential equations, solve the system, 
#and perform other repetitive tasks more reliably. We also define the baseline parameters in a subfile.

source("Subfunctions.R")
source("Params.R") 

#For this sensitivity analysis we will look at the fraction of runs (different predator visit times or lengths) for 
#that parameter set that result in the cannibal hiding when the non-cannibal doesn't. 

#Set up a range of parameter values, mix them up, then run the simulation for a range of predator 
#arrival times and lengths of stay. 

n=1000
range_A=sample(seq(.6,.99,by=.39/n)) # Make bigger - .3-1.2
range_K=sample(seq(1,2.5,by=1.5/n)) # 0-5
range_m=sample(seq(.0001,.01,by=(.01-.0001)/n))
range_b=sample(seq(.001,.1,by=(.1-.001)/n))
range_p=sample(seq(.4,.98,by=.58/n))
range_alpha=sample(seq(.1,.99,by=.89/n))
range_d=sample(seq(0.01,0.4,by=.39/n)) 

#Save the shuffled parameter values together in a data frame. Each row creates a parameter set for the model.
params = data.frame(A=range_A,K=range_K,m=range_m,b=range_b,p=range_p,alpha=range_alpha,d=range_d)

#Set the range for the pred visit start time (PredStart), in two hour increments
Start=seq(1/24,12,by=2/24)

#Run the simulation for each parameter combination for the range of predator arrrival times
#then record the times at which the strategy switches for each parameter set.
Switch=as.data.frame(matrix(ncol=5, nrow=0))
param=BaseParam
i=1

while(i<(n+1)){
  tmp=as.data.frame(matrix(ncol=4, nrow=0))
  param[c('A','K','m','b','p','alpha','d')]=params[i,]
  param['H']=Model2ComputeH(param)
  
  #Look at all the start times and determine the best strategy
  for(j in 1:length(Start)){
    param[['PredStart']]=Start[j]
    tmp=rbind(tmp,c(Start[j],RunModel(param,General_ODE))) #survival if forage, survival is no forage, best choice
  }
  
  #Find the start time where the strategy shifts. Note: this will return infomation about just before the strategy shifts (what it is shifting from) 
  new = tmp[c(tmp[2:length(tmp[,1]),4] != tmp[1:(length(tmp[,1])-1),4],FALSE),]
  if(length(new[,1])>0){
    new2= data.frame(ParamSet=i,SwitchTime=new[,1],SwitchSize=new[,2],BestStrategy=new[,4], NumSwitches=length(new[,1]))
    Switch=rbind(Switch,c(new2))
  }else{
    new2 = data.frame(ParamSet=i,SwitchTime=NA,SwitchSize=NA,BestStrategy=tmp[1,4],NumSwitches=0)
    Switch=rbind(Switch,c(new2))
  }
  print(new2)
  i=i+1
}

Switch=Switch %>% mutate(SwitchTime=as.numeric(SwitchTime),SwitchSize=as.numeric(SwitchSize),NumSwitches=as.numeric(NumSwitches))

#Combine with parameter information and save again
#'A','K','m','b','p','alpha','d'
Switch= Switch%>%
  mutate(A=params[ParamSet,"A"],K=params[ParamSet,"K"],m=params[ParamSet,"m"],b=params[ParamSet,"b"],p=params[ParamSet,"p"],alpha=params[ParamSet,"alpha"],d=params[ParamSet,"d"])

Switch= Switch%>%
  mutate(B=param$B,m0=param$m0,M=param$M,T=param$T,tmin=param$tmin, tmax=param$tmax, PredVisitLength=param$PredVisitLength)

# See if there are any cases where multiple switches occur. If there are, look into them and figure out what is happening for each.
MultSwitch = Switch %>% filter(NumSwitches>1) %>% group_by(ParamSet)%>% summarize(m=first(m),d=first(d),A=first(A),K=first(K),b=first(b),p=first(p),alpha=first(alpha))
MultSwitch


### What parameters affect the strategy switiching size?
names1=c(expression(d),expression(m),expression(b),expression(p),expression(alpha),expression(A),expression(K));
fit1<-lm(Switch$SwitchSize ~ rescale(Switch$d)+rescale(Switch$m)+rescale(Switch$b)+rescale(Switch$p)+rescale(Switch$alpha)+rescale(Switch$A)+rescale(Switch$K))
psqFrac=fit1$coef[-1]; summary(fit1);
LollyPopPlot(psqFrac,names1, c(-120,130),7)  

#To save the graph to put in manuscript
# png("~/Documents/Research Projects/Potato Beetle Egg Sibling Cannibalism/Manuscript/Figures/SensAnalSwitchSize.png",width = 400, height = 325,pointsize=10,res=100)
# LollyPopPlot(psqFrac,names1, c(-60,110),7)  
# dev.off()
