## This is an ordinary differential equation model of African Animal 
## Trypanosomiasis (AAT) that incorporates the emergence, spread and loss of 
## antimicrobial resistance (AMR) between cattle, tsetse fly vectors and 
## wildlife. 

## Lead Author:   Shaun Keegan (shaun.keegan@glasgow.ac.uk)
## Other Authors: Louise Matthews (louise.mattthews@glasgow.ac.uk)


## FORMAT: This file uses plain text descriptions of model parameters for user
##         accessibility. Mathematical model descriptions and corresponding 
##         parameter tables can be found at: 
##         http://github.com/shaunkeegan/AAT_AMR_main/model

## USAGE:  This file has been designed to be run and sourced from other files 
##         in the git repository, so that the model file is left untouched when 
##         exploring scenarios which are included at: 
##         http://github.com/shaunkeegan/AAT_AMR_main/scenarios


AAT_AMR_Main <- function(times, init, parms){
  
  # C - Cattle
  CS  <- init[1] # Susceptible
  CEs <- init[2] # Exposed (drug sensitive strain)
  CEr <- init[3] # Exposed (drug resistant strain)
  CIs <- init[4] # Infected (drug sensitive strain)
  CIr <- init[5] # Infected (drug resistant strain)
  CTs <- init[6] # Treated (drug sensitive strain)
  CTr <- init[7] # Treated (drug resistant strain)
  CR  <- init[8] # Recovered
  
  # P - Prophylactically treated cattle
  PS  <- init[9]  # Susceptible
  PEs <- init[10] # Exposed (drug sensitive strain)
  PEr <- init[11] # Exposed (drug resistant strain)
  PIs <- init[12] # Infected (drug sensitive strain)
  PIr <- init[13] # Infected (drug resistant strain)
  PTs <- init[14] # Treated (drug sensitive strain)
  PTr <- init[15] # Treated (drug resistant strain)
  PR  <- init[16] # Recovered
  
  # W - Wildlife
  WS  <- init[17] # Susceptible
  WEs <- init[18] # Exposed (drug sensitive strain)
  WEr <- init[19] # Exposed (drug resistant strain)
  WIs <- init[20] # Infected (drug sensitive strain)
  WIr <- init[21] # Infected (drug resistant strain)
  WR  <- init[22] # Recovered
  
  # V - Vectors
  VS  <- init[23] # Susceptible
  VEs <- init[24] # Exposed (drug sensitive strain) 
  VEr <- init[25] # Exposed (drug resistant strain)
  VIs <- init[26] # Infected (drug sensitive strain)
  VIr <- init[27] # Infected (drug resistant strain) 
  
  with(as.list(parms),{
    
    # Population total ----
    N <- CS + CEs + CEr + CIs + CIr + CTs + CTr + CR +
      PS + PEs + PEr + PIs + PIr + PTs + PTr + PR
    W <- WS + WEs + WEr + WIs + WIr + WR
    V <- VS + VEs + VEr + VIs + VIr
    
    # Cattle ----
    # 
    # CS, CEs, CEr, CIs, CIr, CTs, CTr, CR
    
    dCS.dt <- birth.c * N - biterate * prob.infection.s * VIs / (N + W) - 
      biterate * prob.infection.r * VIr / (N + W) + resusceptible * CR - 
      death * CS
    
    dCEs.dt <- biterate * prob.infection.s * VIs / (N + W) - infectiousness * 
      CEs - death * CEs 
    
    dCEr.dt <- biterate * prob.infection.r * VIr / (N + W) - infectiousness * 
      CEr - death * CEr 
    
    dCIs.dt <- infectiousness * CEs - treatment * CIs - recovery.s * CIs - 
      death * CIs
    
    dCIr.dt <- infectiousness * CEr - treatment * CIr - recovery.r * CIr - 
      death * CIr
    
    dCTs.dt <- treatment * CTs - recovery.s * CTs - death * CTs
      
    dCTr.dt <- treatment * CTr - recovery.r * CTr - death * CTr
      
    dCR.dt <- recovery.s * CIs + recovery.r * CIr + recovery.s * CTs + 
      recovery.r * CTr - resusceptible * CR - death * CR
      
      
    # Cattle with prophylaxis ----
    # 
    # PS, PEs, PEr, PIs, PIr, PTs, PTr, PR
    
    dPS.dt <- birth.c * N - biterate * prob.infection.s * VIs / (N + W) - 
      biterate * prob.infection.r * VIr / (N + W) + resusceptible * PR - 
      death * PS
    
    dPEs.dt <- biterate * prob.infection.s * VIs / (N + W) - infectiousness * 
      PEs - death * PEs 
    
    dPEr.dt <- biterate * prob.infection.r * VIr / (N + W) - infectiousness * 
      PEr - death * PEr 
    
    dPIs.dt <- infectiousness * PEs - treatment * PIs - recovery.s * PIs - 
      death * PIs
    
    dPIr.dt <- infectiousness * PEr - treatment * PIr - recovery.r * PIr - 
      death * PIr
    
    dPTs.dt <- treatment * PTs - recovery.s * PTs - death * PTs
    
    dPTr.dt <- treatment * PTr - recovery.r * PTr - death * PTr
    
    dPR.dt <- recovery.s * PIs + recovery.r * PIr + recovery.s * PTs + 
      recovery.r * PTr - resusceptible * PR - death * PR
    
    # Wildlife ----
    # 
    # WS, WEs, WEr, WIs, WIr, WTs, WTr, WR
    
    dWS.dt <- birth.c * N - biterate * prob.infection.s * VIs / (N + W) - 
      biterate * prob.infection.r * VIr / (N + W) + resusceptible * WR - 
      death * WS
    
    dWEs.dt <- biterate * prob.infection.s * VIs / (N + W) - infectiousness * 
      WEs - death * WEs 
    
    dWEr.dt <- biterate * prob.infection.r * VIr / (N + W) - infectiousness * 
      WEr - death * WEr 
    
    dWIs.dt <- infectiousness * WEs - recovery.s * WIs - death * WIs
    
    dWIr.dt <- infectiousness * WEr - recovery.r * WIr - death * WIr
    
    dWR.dt <- recovery.s * WIs + recovery.r * WIr + recovery.s * WTs + 
      recovery.r * WTr - resusceptible * WR - death * WR
    
    # Tsetse ----
    # 
    # VS, VEs, VEr, VIs, VIr, VTs, VTr
    
    dVS.dt <- birth.v * V -
    
    dVEs.dt <-  
    
    dVEr.dt <-  
    
    dVIs.dt <- 
    
    dVIr.dt <- 
    
    # Model output ----
    dX <- c(dS.dt, dStrt.dt, dI.dt, dIreb.dt, dR.dt)
    list(dX)
  })
}