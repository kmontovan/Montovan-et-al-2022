BaseParam <- c(
  A=.8, # shape parameter for positive term in growth curve
  B=1,  # shape parameter for negative term in growth curve
  H=NA, # coefficeint for positive term 
  K=1.75, # coefficient for the negative term
  
  m=0.003, #increased detection prob for foraging larvae (per mg)
  b=0.0025, #detection probability for non-foraging larvae (per mg)
  
  p=.9, #probability a small detected larvae is killed 
  alpha=.5,
  d=0.05, #constant non-predator death of larvae
  
  m0 = 1, # size at time 0
  M = 200, #size at end of growth
  T = 20, #time at end of growth 
  
  tmin = 0, #time frame for growth begin # Start time at beginning
  tmax = 35, #end time frame for growth
  
  forage=NA,#forage while predator present? 1=no, 0=yes
  PredStart = NA, #start time of pred present
  PredVisitLength=3/24, # How long is predator present
  
  growthmodel=1,
  can_timeshift=1 #For non-cannibals, no time shift (1), for cannibals, best fit timeshift is 1.19
)
