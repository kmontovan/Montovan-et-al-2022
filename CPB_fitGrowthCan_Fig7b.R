require(ggplot2)
require(readr)
require(dplyr)
library(survival)
library(reshape)
library(deSolve)
require(tidyr)

#The growth models and other subfunctions are defined in this file:
source("VBGF_subfunctions.R")

## Growth for Cannibal Colorado potato beetle larvae

###################### LOAD IN AND PREP THE DATA ###################### 
sizes= read.csv("cannGR.csv")%>%
  mutate(cannibalf=as.factor(cannibal))%>%
  select(day, mass, cannibalf, pupation)

sizes2= read.csv(
  "Control_larval mass.csv")%>%
  select(day=Day, mass=Mass)%>%
  mutate(cannibalf=as.factor(1), pupation=0)

#Combine the two datasets into one and restrict to Cannibal data
Larval_Growth=rbind(sizes,sizes2)

Larval_Growth_non = Larval_Growth%>%
  filter(cannibalf==0 | day==0)

Larval_Growth_can = Larval_Growth%>%
  filter(cannibalf==1)

#Set the timescale to use for all models
t = seq(0,17,by=.1)

####################### Fit can_timeshift to cannibal data ####################### 
fit$convergence=1
while(fit$convergence==1){
  theta0 = c(can_timeshift=jitter(.5),sigma=5)
  P0= c(can_timeshift=log(theta0[["can_timeshift"]]), S=log(theta0[["sigma"]]))
  fit=optim(P0,nLL3Can,tydat=Larval_Growth_can)
}

Model3BestFitCan2=rbind(c(fit1=c(exp(fit$par),nLL=fit$value,conv.code=fit$convergence)))
FitparamsCan2= c(can_timeshift=Model3BestFitCan2[[1,'fit1.can_timeshift']],S=Model3BestFitCan2[[1,"fit1.S"]])
#nLL=1489.9

######################### Fit A, K, H, B for Model 3 to the cannibal data ######################### 
#Start with parameters for non-cannibals
theta0 = c(H=0.96,K=0.41,A=1.28,B=1.45,m0=1,sigma=5)

#Run the optimization to get the an estimate of the parameters.
fit$convergence=1
while(fit$convergence==1){
  theta0 = c(H=jitter(.96),K=jitter(.41),A=jitter(1.28),m0=jitter(1),B=jitter(1.45),sigma=5)
  P0= c(H=log(theta0[["H"]]), K=log(theta0[["K"]]), A=log(theta0[["A"]]), m0=log(theta0[["m0"]]),
        B=log(theta0[["B"]]), S=log(theta0[["sigma"]]))
  fit=optim(P0,nLL3,tydat=Larval_Growth_can)
}

Model3BestFitCan2=rbind(c(fit1=c(exp(fit$par),nLL=fit$value,conv.code=fit$convergence)))
Model3BestFitCan2
#nLL= 1488.433


### Compare models for goodness of fit using AIC

nLL=c(1489.9,1488.433)
k=c(1,5)
AIC= 2*k+2*nLL
prob= exp((min(AIC)-AIC)/2)
prob/sum(prob)

#Plot the best results of the cannibal model

t=seq(0,17,.1)
params = c(H=0.96,K=0.41,A=1.28,B=1.45,m0=1)
Model3OutPlot = data.frame(day=t, mass=Model3(t,params))

params = c(H=0.96,K=0.41,A=1.28,B=1.45,m0=1,can_timeshift=Model3BestFitCan[[1,'fit1.can_timeshift']])
#params= c(H=Model3BestFitCan2[[1,'fit1.H']],K=Model3BestFitCan2[[1,'fit1.K']],A=Model3BestFitCan2[[1,'fit1.A']],B=Model3BestFitCan2[[1,'fit1.B']],m0=Model3BestFitCan2[[1,'fit1.m0']])
Model3OutPlotCan = data.frame(day=t, mass=Model3_can(t,params))

########### FIGURE 7b ######################
pcan <- ggplot() + 
  geom_point(data = Larval_Growth_can, aes(x=jitter(day), y=mass),color='black',alpha=0.3)+
  # geom_point(data = Larval_Growth_non, aes(x=jitter(day), y=mass),color='black',alpha=0.3)+
  theme_bw() + 
  xlab("Age of larva (day)")+  ylab("Mass of larva (grams)")+
  geom_line(data=Model3OutPlot,aes(x=day, y=mass),color = "gray", linetype='dashed')+
  geom_line(data=Model3OutPlotCan,aes(x=(day), y=mass),color='black')
#  geom_line(data=Model3OutPlotCan2,aes(x=day, y=mass),color='green')
pcan


