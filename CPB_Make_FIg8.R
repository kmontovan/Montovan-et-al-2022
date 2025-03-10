library(deSolve)
library(reshape2)
library(ggplot2)
library(dplyr)
library(permute)
library(gridExtra)
library(viridis)

#Load subfunctions and parameters
source("Subfunctions.R") 
source("Params.R") #Baseline parameters
param=BaseParam;

#Set the growth curve parameters to match the best fit for CPB
GCA=1.2831877; GCK=0.2689044; GCH=0.7837131; GCB = 1.4942732

param[['A']]=GCA; param[['K']]=GCK; param[['H']]=GCH; param[['B']]=GCB
param[['m0']]=0.7828322


# Setup ranges for m and d that we want to explore.
n=22 #set dimensions for the m by d grid
range_m=sample(seq(.005,.05,by=(.045)/(n-1))); #Range of m values to conside
range_d=sample(seq(0.05,0.5,by=.45/(n-1))) ; #Range of d values to consider
params = data.frame(expand.grid(m=range_m,d=range_d)) #make every combination of the two so we get a grid

#Set range of times at which the predator arrives to run the model for
Start=seq(1/24,12,by=2/24) #Range for Predator visit Start times (2 hour increments)

#Setup storage and initalize i. This allows us to stop and restart the model runs without having to start over. 
tmp=as.data.frame(matrix(ncol=7, nrow=0))
names(tmp)=c('time','non_size','non_survival','non_best','can_size','can_survival','can_best')  
tmp2=as.data.frame(matrix(ncol=13, nrow=0))
i=1

#Run for each parameter set and compare cannibals with non-cannibals
while(i<(length(params[,1])+1)){
  tmp=as.data.frame(matrix(ncol=4, nrow=0))
  param[c('m','d')]=params[i,]
  for(j in 1:length(Start)){
    param[['PredStart']]=Start[j]
    param['can_timeshift']=1; out_non=RunModel(param,General_ODE)
    param['can_timeshift']=1.19; out_can=RunModel(param,General_ODE)
    tmp=rbind(tmp,c(Start[j],out_non,out_can)) #survival if forage, survival is no forage, best choice
  }
  
  names(tmp)=c('time','non_size','non_survival','non_best','can_size','can_survival','can_best')  
  tmp<-tmp%>%mutate(time=as.numeric(time), non_size=as.numeric(non_size),non_survival=as.numeric(non_survival),can_size=as.numeric(can_size),can_survival=as.numeric(can_survival))%>%
    mutate(survival_dif=can_survival-non_survival, survival_dif_perc=(can_survival-non_survival)/non_survival)
  
  #Find places where the strategy shifts and record information about the switch time/size if there is exactly one switch in strategy.
  new_can = tmp[c(tmp[2:length(tmp[,1]),c(7)] != tmp[1:(length(tmp[,1])-1),c(7)],FALSE),c(1,5,7)]
  if(length(new_can[,'time'])==1){
    new2= data.frame(ParamSet=i,CanSwitchTime=new_can[,'time'],
                     CanSwitchSize=new_can[,'can_size'],CanSwitchFrom=new_can[,'can_best'])
  }else if(length(new_can[,'time'])==0){
    new2= data.frame(ParamSet=i,CanSwitchTime=NA,CanSwitchSize=NA,CanSwitchFrom=tmp[1,'can_best'])
  }else{
    new2= data.frame(ParamSet=i,CanSwitchTime=NA,CanSwitchSize=NA,CanSwitchFrom='Multiple')
  }
  
  new_non = tmp[c(tmp[2:length(tmp[,1]),c(4)] != tmp[1:(length(tmp[,1])-1),c(4)],FALSE),c(1,2,4)]
  if(length(new_non[,'time'])==1){
    new2= new2%>%mutate(NonSwitchTime=new_non[,'time'],NonSwitchSize=new_non[,'non_size'],
                        NonSwitchFrom=new_non[,'non_best'])
  }else if(length(new_can[,'time'])==0){
    new2= new2%>%mutate(NonSwitchTime=NA,NonSwitchSize=NA,NonSwitchFrom=tmp[1,'non_best'])
  }else{
    new2=new2%>%mutate(NonSwitchTime=NA,NonSwitchSize=NA,NonSwitchFrom='Multiple')
  }
  
  #Add in summary information about the survival over all the predator starttimes 
  new2=new2%>%
    mutate(min_survival_dif=min(tmp[,'survival_dif']), 
           max_survival_dif=max(tmp[,'survival_dif']),
           mean_survival_dif=mean(tmp[,'survival_dif']),
           min_survival_dif_perc=min(tmp[,'survival_dif_perc']), 
           max_survival_dif_perc=max(tmp[,'survival_dif_perc']),
           mean_survival_dif_perc=mean(tmp[,'survival_dif_perc']))
  
  #store this information         
  tmp2=rbind(tmp2,c(new2))
  i=i+1
}

#Combine with parameter information and save again 
tmp3= tmp2%>%
  mutate(m=params[ParamSet,"m"],d=params[ParamSet,"d"])
tmp3= tmp3%>%
  mutate(B=param$B,m0=param$m0,M=param$M,T=param$T,tmin=param$tmin, 
         tmax=param$tmax, A=param$A, K=param$K, b=param$b, p=param$p, 
         alpha=param$alpha, PredVisitLength=param$PredVisitLength)%>%
  mutate(DiffTimeNC=NonSwitchTime-CanSwitchTime, DiffSizeNC=NonSwitchSize-CanSwitchSize)

#Create Figures
ggplot()+
  geom_tile(data=tmp3%>%filter(CanSwitchFrom!='COMPLEX'),aes(x=m,y=d, fill=DiffSizeNC))+
  labs(x='Detection cost of foraging: m (per mg)',y='Baseline death rate: d')+ theme_bw()+ theme(legend.position="top")+ 
  scale_fill_gradient("Difference in Strategy\n Switching Size ",low = "gray20", high = "gray99",breaks=c(-20,-15,-10,-5)) 
ggplot()+
  geom_tile(data=tmp3%>%filter(CanSwitchFrom!='COMPLEX'),aes(x=m,y=d, fill=mean_survival_dif_perc))+
  labs(x='Detection cost of foraging: m (per mg)',y='Baseline death rate: d')+ theme_bw()+ theme(legend.position="top")+
  scale_fill_gradient("Proportional Difference\n in Survival",low = "gray99", high = "gray20",breaks=c(.05,.15,.25)) 

