## This file contains parameters for the model AAT_AMR_main_dens_dep.R
## 
## 

## Authors: Shaun Keegan (shaun.keegan@glasgow.ac.uk)
##          Louise Matthews (louise.mattthews@glasgow.ac.uk)

library(codetools)

## Parameters & Initial Conditions ----

set1 <- function(output, birth.adj, fit.adj, K, prop_treat, prop.insecticide, NW, prop.prophylaxis){
  
  ## Cattle ----- 
  birth.c          <- 1 / 365
  biterate         <- 0.8 / 4
  prob.infection   <- 0.46
  infectiousness   <- 1 / 15
  resusceptible    <- 10
  death            <- birth.c
  recovery         <- 1 / 100
  treatment        <- prop_treat * (recovery + death) / (1 - prop_treat)
  recovery.st      <- recovery * 250 #LM: adjusted so that R0 drops below 1 when 99% treated to reflect Hargrove
  emergence        <- 0
  rec.adj          <- 1

  
  NC <- 50 # Total cattle
  CIr <- 0    # Infected (drug resistant strain)
  CIs <- 1    # Infected (drug sensitive strain)
  CS  <- NC * (1 - prop.prophylaxis) - CIs - CIr # Susceptible
  CEs <- 0    # Exposed (drug sensitive strain)
  CEr <- 0    # Exposed (drug resistant strain)
  
  CTs <- 0    # Treated (drug sensitive strain)
  CTr <- 0    # Treated (drug resistant strain)
  CR  <- 0    # Recovered
  
  PS  <- NC * prop.prophylaxis    # Susceptible   #LM: changed cattle to NC
  PEs <- 0    # Exposed (drug sensitive strain)
  PEr <- 0    # Exposed (drug resistant strain)
  PIs <- 0    # Infected (drug sensitive strain)
  PIr <- 0    # Infected (drug resistant strain)
  PTs <- 0    # Treated (drug sensitive strain)
  PTr <- 0    # Treated (drug resistant strain)
  PR  <- 0    # Recovered
  
  
  ## ----- Wildlife
  birth.w            <- 1 / 365
  prob.infection.s.w <- 0.46
  prob.infection.r.w <- 0.46
  infectiousness.w   <- 1 / 20
  resusceptible.w    <- 1 / 100
  death.w            <- birth.w
  recovery.w         <- recovery
  reversion          <- 0
  
  #NW <- 0
  WIs <- 0    # Infected (drug sensitive strain)
  WS  <- NW - WIs  # Susceptible
  WEs <- 0    # Exposed (drug sensitive strain)
  WEr <- 0    # Exposed (drug resistant strain)
  WIr <- 0    # Infected (drug resistant strain)
  WR  <- 0    # Recovered
  
  
  ## -----  Vectors
  
  qf <- 0.96 # Probability of surviving on a feeding day
  qn <- 0.98 # Probability of surviving on a non-feeding day
  feed.cyc <- 4 # Days between feeding
  feeding.rate     <-  0
  prob.infection.v <-  0.025
  incubation       <-  20
  prop.insecticide.actual <- prop.insecticide * NC/(NC + NW) # Proportion of insecticide adjusted for wildlife
  death.v <- -1 * log((1 - prop.insecticide.actual) * qf * qn ^ feed.cyc) / feed.cyc # Vector death rate
  birth.v = birth.adj *(-1) * log((1 - 0) * qf * qn ^ feed.cyc) / feed.cyc # Vector birth rate 
  equil_vector_pop <- max(0, K*(1 - death.v/birth.v)) # Vector equilibrium population
  infectiousness.v <- death.v * exp(-death.v * incubation) / (1 - exp(-death.v * incubation)) # Rate from E to I
  
  
  
  NV  <- equil_vector_pop #equil_vector_pop
  VS  <- NV # Susceptible
  VEs <- 0    # Exposed (drug sensitive strain)
  VEr <- 0    # Exposed (drug resistant strain)
  VIs <- 0    # Infected (drug sensitive strain)
  VIr <- 0    # Infected (drug resistant strain)
  
  
  
  
  
  ## ----- Parameters & initial conditions output
  
  params <- cbind(NC, NV, NW, birth.c, biterate, prob.infection, fit.adj, rec.adj, recovery.st, 
                  infectiousness, resusceptible, death, treatment, recovery, birth.v, 
                  death.v, feeding.rate, prob.infection.v, infectiousness.v, emergence, 
                  reversion, K, birth.w, infectiousness.w, resusceptible.w, death.w, recovery.w)
  names <- colnames(params)
  params <- as.vector(params)
  names(params) <- names
  
  inits <- cbind(CS, CEs, CEr, CIs, CIr, CTs, CTr, CR, PS, PEs, PEr, PIs,
                 PIr, PTs, PTr, PR, WS, WEs, WEr, WIs, WIr, WR, VS, VEs,
                 VEr, VIs, VIr)
  names <- colnames(inits)
  inits <- as.vector(inits)
  names(inits) <- names
  
  
  if (output == "P"){
    return(params)
  }else{
    return(inits)
  }
  
}


my_rootfun <- function(t, y, params) {
  return(c(y['CS'] - 20.0, y['CIs'] - 10.0))
}
my_rootfun2 <- function (t, y, params) {
  dstate <- unlist(AAT_AMR_dens_dep(t, y, params)) # rate of change vector
  condition1 <- (y['CIs'] - 1e-5)
  condition2 <- sum(abs(dstate)) - 1e-5
  return(c(condition1, condition2))
}



findGlobals(fun = set1, merge = FALSE)$variables

## Error Checks ----



#if(CIs + CIr > NC){cat(red("WARNING: Infected cattle (", CIs + CIr, ") is GREATER THAN total cattle(", NC, ")\n"))}
#if(WIs > NW){cat(red("WARNING: Infected wildlife (", WIs, ") is GREATER THAN total wildlife(", NW, ")\n"))}


