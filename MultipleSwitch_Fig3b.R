#
#
#
# In this file we explore the effects of the speed of growth on the optimal fraging strategy in the presence of a predator.
# This file relies on a subfile containing the functions that define the differential equations, solve the system,
# and perform other repetitive tasks more reliably. It also relies on a subfile defining the base parameter values.

library(deSolve)
library(reshape2)
library(ggplot2)
library(dplyr)
library(permute)
library(gridExtra)
library(viridis)
library(gcookbook)

source("Subfunctions.R")
source("Params.R") #Baseline parameters
param=BaseParam;

#Define the ODE with growth occuring in bursts but the rest of the model the same as is used in the rest of the paper.
pulseODE <- function(t, state, param) {
  with(as.list(c(state, param)), {
    pre1= 1*(t<PredStart+PredVisitLength)*(t>PredStart) #Make a vector for each timestep with whether the predator is there
    u= 1*(t>=0) - forage*pre1 #forage unless the predator is present and then follow the strategfy defined by the parameter 'forage'
    dx1 <- u*(a1+(a2-a1)*(sin((t+3*pi/4)))^ae)
    dx2 <- (-d*x2)-(m*u+b)*(1-(1-alpha)*x1/M)*p*x1*x2*pre1
    list(c(dx1, dx2))
  })
}

#Define key parameters.
param[['a1']]=.05 # rate for slow growth
param[['a2']]=150 # rate for swift growth
param[['ae']]=50 #This makes the growth bursts very sharply increase
Start=seq(1/24,12,by=1/24) #Define the set of predator start times to test (in days)

tmp=as.data.frame(matrix(ncol=4, nrow=0))
for(j in 1:length(Start)){
  param[['PredStart']]=Start[j]
  out=RunModel(param,pulseODE)
  tmp=rbind(tmp,c(Start[j],out)) #survival if forage, survival is no forage, best choice
}
names(tmp)=c('time','size','survival','best')
tmp<-tmp%>%mutate(time=as.numeric(time), size=as.numeric(size),survival=as.numeric(survival))

#Find the times when growth is at least 1mg per timestep.
tmp$time[(tmp$size[2:144]-tmp$size[1:143])>1]

#Plot the mass of the prey at each timestep and whether the prey should hide or continue to forage if the
# predator were to arrive at that time. Shade periods of growth that are at least 1mg per timestep.

p<-ggplot()+
  geom_point(data=tmp,aes(x=time, y=size,color=best), size=1)+
  xlab('Time (days)' )+ylab('Mass (mg)')+
  theme_bw()+
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_blank(),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid",
                                         colour ="black"))+
  scale_colour_grey()+
  annotate("rect", xmin = 2.04, xmax = 2.56, ymin = 0, ymax = 160,
           alpha = .1,fill = "black")+
  annotate("rect", xmin = 5.2, xmax = 5.72, ymin = 0, ymax = 160,
           alpha = .1,fill = "black")+
  annotate("rect", xmin = 8.37, xmax = 8.9, ymin = 0, ymax = 160,
           alpha = .1,fill = "black")+
  annotate("rect", xmin = 11.45, xmax = 11.88, ymin = 0, ymax = 160,
           alpha = .1,fill = "black")
p

#This makes figure 3b for the paper
#png("~/Figures/Figure3b.png",width = 400, height = 325,pointsize=16,res=120)
#p
#dev.off()
