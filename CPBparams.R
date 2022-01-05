
   BaseParam <- c(
  A=1.28, # shape parameter for positive term in growth curve
  B=1.45,  # shape parameter for negative term in growth curve
  H=0.96, # coefficeint for positive term 
  K=0.41, # coefficient for the negative term
  
  m=0.003, #increased detection prob for foraging larvae (per mg)
  b=0.003, #detection probability for non-foraging larvae (per mg)
  
  p=.9, #probability a small detected larvae is killed 
  alpha=.5,
  d=0.05, #constant non-predator death of larvae
  
  m0 =0.634, # size at time 0
  M=157, #size at end of larval growth
  T=15, #time at end of larval growth 
  
  tmin=0, #time frame for growth begin # Start time at beginning
  tmax=30, #end time frame for growth
  
  forage=NA,#forage while predator present? 1=no, 0=yes
  PredStart = NA, #start time of pred present
  PredVisitLength=3/24, # How long is predator present (3 hours)
  
  growthmodel=1,
  can_timeshift=1 #For non-cannibals, no time shift (1), for cannibals, best fit timeshift is 1.19
)
