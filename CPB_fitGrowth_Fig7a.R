require(ggplot2)
require(readr)
require(dplyr)
library(survival)
library(reshape)
library(deSolve)
require(tidyr)

#The growth models and other subfunctions are defined in this file:
source("VBGF_subfunctions.R")

## Growth for non-cannibal Colorado potato beetle larvae

# This analysis is of measurements of larval mass for larvae raised in the lab 
# all larvae used in this part were not allowed to cannibalize a sibling egg and
# were not exposed to predators

#Note: in this data, mass is in mg, and a larvae is a cannibal if cannibal=1. 

###################### LOAD IN AND PREP THE DATA ###################### 
sizes= read.csv("cannGR.csv")%>%
  mutate(cannibalf=as.factor(cannibal))%>%
  select(day, mass, cannibalf, pupation)

sizes2= read.csv(
  "Control_larval mass.csv")%>%
  select(day=Day, mass=Mass)%>%
  mutate(cannibalf=as.factor(1), pupation=0)

#Combine the two datasets into one and restrict to non-cannibal data
Larval_Growth=rbind(sizes,sizes2)

Larval_Growth_non = Larval_Growth%>%
  filter(cannibalf==0 | day==0)

Larval_Growth_can = Larval_Growth%>%
  filter(cannibalf==1)

#Set the timescale to use for all models
t = seq(0,17,by=.1)

###################### MODEL 1###################### 

# Run the optimization to get the an estimate of the parameters.
theta0 = c(A=jitter(.6),H=jitter(1.3),m0=jitter(.1),sigma=5)
Model1BestFit=RunOptim1(theta0,Larval_Growth_non)
params1= c(H=Model1BestFit[[1,'fit1.H']],A=Model1BestFit[[1,'fit1.A']],m0=Model1BestFit[[1,'fit1.m0']])
PredWeight = Model1(t,params1)
Model1OutPlot = data.frame(day=t, mass=PredWeight)

###################### MODEL 2 ######################  NOT WORKING ARGHHHHHHHHHHHHHHHHHHHHHHHHHH

#We want to fit the model parameters $H$, $K$, $A$, and $m0$ to the data. 
theta0 = c(H=1,K=.7,A=.7,m0=.63,sigma=5)
Model2BestFit = RunOptim2(theta0,Larval_Growth_non)
params2= c(H=Model2BestFit[[1,'fit1.H']],A=Model2BestFit[[1,'fit1.A']],K=Model2BestFit[[1,'fit1.K']],m0=Model2BestFit[[1,'fit1.m0']])
Model2OutPlot = data.frame(day=t, mass=Model2(t,params2))

###################### MODEL 3 ###################### NOT WORKING EITHER GRRRRRRRRRRRRRRRR
#We want to fit the model parameters $H$, $K$, $A$, $B$ to the data. (amd maybe m0) THIS IS NOT WORKING!!!
Model3BestFit=0
#Run the optimization to get the an estimate of the parameters.
Model3BestFit=RunOptim3(theta0,Larval_Growth_non)
params3= c(H=Model3BestFit[[1,'H']],A=Model3BestFit[[1,'A']],B=Model3BestFit[[1,'B']],K=Model3BestFit[[1,'K']],m0=Model3BestFit[[1,'m0']],S=Model3BestFit[[1,"S"]])
Model3OutPlot = data.frame(day=t, mass=Model3(t,params3))

###################### Plot the best fit curves for all three models ###################### 
# The solid line is the second model, the dashed line is the first model and the dotted line is the most general third model. 

ggplot(data = Larval_Growth, aes(x=jitter(day), y=mass)) + 
  theme_bw() + 
  geom_point(alpha=.3)+
  xlab("Age of larva (day)")+  ylab("Mass of larva (grams)")+
  geom_line(data=Model3OutPlot,aes(x=day, y=mass))+
  geom_line(data=Model2OutPlot,aes(x=day, y=mass),linetype = "dotted")+
  geom_line(data=Model1OutPlot,aes(x=day, y=mass), linetype  = "dashed")

