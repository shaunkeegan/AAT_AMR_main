library(codetools)

r0sen <- function(parms){
  
  NC <- parms["NC"]
  NV <- parms["NV"]
  NW <- parms["NW"]
  biterate <- parms["biterate"]
  prob.infection <- parms["prob.infection"]
  death <- parms["death"]
  infectiousness <- parms["infectiousness"]
  treatment <- parms["treatment"]
  recovery <- parms["recovery"]
  recovery.st <- parms["recovery.st"]
  
  death.v <- parms["death.v"]
  infectiousness.v <- parms["infectiousness.v"]
  prob.infection.v <- parms["prob.infection.v"]
  
  recovery.w <- parms["recovery.w"]
  death.w <- parms["death.w"]
  infectiousness.w <- parms["infectiousness.w"]
  
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
  
  #R0s <- sqrt(RH1Vs * RVH1s + RWVs * RVWs)
  R0s <- RH1Vs * RVH1s + RWVs * RVWs  #LM: changed to match Hargrove
  
  R0sen <- cbind(RH1Vs, RWVs, RVH1s, RVWs, R0s)
  names <- colnames(R0sen)
  R0sen <- as.vector(R0sen)
  names(R0sen) <- names
  
  return(R0sen)
  
}





r0res <- function(parms){
  
  NC <- parms["NC"]
  NV <- parms["NV"]
  NW <- parms["NW"]
  biterate <- parms["biterate"]
  prob.infection <- parms["prob.infection"]
  death <- parms["death"]
  infectiousness <- parms["infectiousness"]
  treatment <- parms["treatment"]
  recovery <- parms["recovery"]
  recovery.st <- parms["recovery.st"]
  rec.adj <- parms["rec.adj"]
  fit.adj <- parms["fit.adj"]
  
  death.v <- parms["death.v"]
  infectiousness.v <- parms["infectiousness.v"]
  prob.infection.v <- parms["prob.infection.v"]
  
  recovery.w <- parms["recovery.w"]
  death.w <- parms["death.w"]
  infectiousness.w <- parms["infectiousness.w"]
  
  N <- NC + NW  #LM: added
  infectiousness.v <- parms["infectiousness.v"]
  
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
  
  #R0r <- sqrt(RH1Vr * RVH1r + RWVr * RVWr)
  R0r <- RH1Vr * RVH1r + RWVr * RVWr  #LM: changed to match Hargrove
  
  R0res <- cbind(RH1Vr, RWVr, RVH1r, RVWr, R0r)
  names <- colnames(R0res)
  R0res <- as.vector(R0res)
  names(R0res) <- names
  
  return(R0res)

}

findGlobals(fun = r0sen, merge = FALSE)$variables
findGlobals(fun = r0res, merge = FALSE)$variables
