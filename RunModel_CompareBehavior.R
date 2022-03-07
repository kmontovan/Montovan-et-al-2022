library(deSolve)
library(reshape2)
library(ggplot2)
library(dplyr)
library(permute)

source("Subfunctions.R")
source("Params.R")

#Initialize the parameters and modify the ones that need to be modified
param=BaseParam #start with base parameters
param[['PredStart']]=4
param[['PredVisitLength']]=1
param[['H']]=Model2ComputeH(param)

#Run the model once with the prey foraging during the predator visit
param[['forage']]=0 #Forage
out_forage= data.frame(SolveSystem(param,General_ODE))

#Run the model again with the prey hiding during the predator visit
param[['forage']]=1 #don't forage
out_hide= data.frame(SolveSystem(param,General_ODE))

# Plot the survival probability as the prey develops and grows 
# Black: prey foraged when the predator is present 
# Blue: prey hides (and stops growing) while the predator is present 
ggplot()+
  geom_line(data=out_forage,aes(x=time,y=x2),color="black")+
  geom_line(data=out_hide,aes(x=time,y=x2),color="blue")+
  xlab('Age of prey')+
  ylab('Probability the prey survives to this age')

# Plot the size of the prey as a function of time for both
# foraging (black) and hiding (blue)
ggplot()+
  geom_line(data=out_forage,aes(x=time,y=x1),color="black")+
  geom_line(data=out_hide,aes(x=time,y=x1),color="blue")+
  xlab('Age of prey')+
  ylab('Mass of prey')

#This function returns the size at which the predator visit starts, the optimal behavior and the probability the 
#prey survives to escape predation if it uses the optimal strategy
out1=RunModel(param,General_ODE)  
out1

#This function returns the size at which the predator visit starts, the optimal behavior ("Choice") and the probability the 
#prey survives to escape predation if it hides ("x2h") or forages ("x2f") while the predator is present.
out2=RunModel2(param,General_ODE)  
out2
