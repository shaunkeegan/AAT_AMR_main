
r0sen <- function(inits, parms){
  
  
  
  N <- NC + NW
  
  RH1Vs <- (NC/(N))*(biterate*prob.infection/death.v) * (infectiousness/(infectiousness + death))
  RWVs <- (NW/N)*(biterate*prob.infection/death.v) * (infectiousness.w/(infectiousness.w + death.w))
  RVH1s <- prob.infection.v * biterate * (NV/N) * # rate of biting
    (1/(treatment + recovery + death)) * # length of time host infection in I
    (infectiousness.v/(infectiousness.v + death.v)) + # proportion of vectors that end up infected
    prob.infection.v * biterate * (NV/N) * #rate of biting
    (1/(recovery.st + death)) *  # length of time host infected in T
    (infectiousness.v/(infectiousness.v + death.v)) * # proportion of vectors that end up infected
    (treatment/((treatment + recovery + death))) # proportion of hosts proceeding from I to T
  
  RVWs  <- prob.infection.v * biterate * (NV/N) * (1/(recovery.w + death.w)) * (infectiousness.v/(infectiousness.v + death.v))
  
  R0s <- sqrt(RH1Vs * RVH1s + RWVs * RVWs)
  
  return(cbind(RH1Vs, RWVs, RVH1s, RVWs, R0s))
  
}

r0res <- function(inits, parms){
  
  RH1Vr <- (NC/(N))*(biterate*(prob.infection * fit.adj)/death.v) * (infectiousness/(infectiousness + death))
  RWVr <- (NW/N)*(biterate*(prob.infection * fit.adj)/death.v) * (infectiousness.w/(infectiousness.w + death.w))
  
  RVH1r <- prob.infection.v * biterate * (NV/N) *     #rate of biting
    (1/(treatment + (recovery) + death)) *  #length of time host infectious (in I category) 
    (infectiousness.v/(infectiousness.v + death.v)) + #proportion of vectors that actually end up as an infected vector
    
    prob.infection.v * biterate * (NV/N) *            #rate of biting again
    (1/(recovery * rec.adj + death)) *                          #time host spends in treated state
    (infectiousness.v/(infectiousness.v + death.v)) *  #proportion of vectors that actually end up as an infected vector
    (treatment/((treatment + (recovery) + death)))  #proportion og hosts proceeding to treated state
  
  RVWr  <- prob.infection.v * biterate * (NV/N) * (1/(recovery.w + death.w)) * (infectiousness.v/(infectiousness.v + death.v))
  
  R0r <- sqrt(RH1Vr * RVH1r + RWVr * RVWr)
  
  return(cbind(RH1Vr, RWVr, RVH1r, RVWr, R0r))
}