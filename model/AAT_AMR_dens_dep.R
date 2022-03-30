## This is an ordinary differential equation model of African Animal 
## Trypanosomiasis (AAT) that incorporates the emergence, spread and loss of 
## antimicrobial resistance (AMR) between cattle, tsetse fly vectors and 
## wildlife. 

## Authors:   Shaun Keegan (shaun.keegan@glasgow.ac.uk)
##            Louise Matthews (louise.mattthews@glasgow.ac.uk)


## FORMAT: This file uses plain text descriptions of model parameters for user
##         accessibility. Mathematical model descriptions and corresponding 
##         parameter tables can be found at: 
##         http://github.com/shaunkeegan/AAT_AMR_main/model

## USAGE:  This file has been designed to be run and sourced from other files 
##         in the git repository, so that the model file is left untouched when 
##         exploring scenarios which are included at: 
##         http://github.com/shaunkeegan/AAT_AMR_main/scenarios

library(codetools)

AAT_AMR_dens_dep <- function(times, init, parms){
  
  # C - Cattle
  CS  <- init["CS"] # Susceptible
  CEs <- init["CEs"] # Exposed (drug sensitive strain)
  CEr <- init["CEr"] # Exposed (drug resistant strain)
  CIs <- init["CIs"] # Infected (drug sensitive strain)
  CIr <- init["CIr"] # Infected (drug resistant strain)
  CTs <- init["CTs"] # Treated (drug sensitive strain)
  CTr <- init["CTr"] # Treated (drug resistant strain)
  CR  <- init["CR"] # Recovered
  
  # P - Prophylactically treated cattle
  PS  <- init["PS"]  # Susceptible
  PEs <- init["PEs"] # Exposed (drug sensitive strain)
  PEr <- init["PEr"] # Exposed (drug resistant strain)
  PIs <- init["PIs"] # Infected (drug sensitive strain)
  PIr <- init["PIr"] # Infected (drug resistant strain)
  PTs <- init["PTs"] # Treated (drug sensitive strain)
  PTr <- init["PTr"] # Treated (drug resistant strain)
  PR  <- init["PR"] # Recovered
  
  # W - Wildlife
  WS  <- init["WS"] # Susceptible
  WEs <- init["WEs"] # Exposed (drug sensitive strain)
  WEr <- init["WEr"] # Exposed (drug resistant strain)
  WIs <- init["WIs"] # Infected (drug sensitive strain)
  WIr <- init["WIr"] # Infected (drug resistant strain)
  WR  <- init["WR"] # Recovered
  
  # V - Vectors
  VS  <- init["VS"] # Susceptible
  VEs <- init["VEs"] # Exposed (drug sensitive strain) 
  VEr <- init["VEr"] # Exposed (drug resistant strain)
  VIs <- init["VIs"] # Infected (drug sensitive strain)
  VIr <- init["VIr"] # Infected (drug resistant strain) 
  
  ## ----- Cattle
  birth.c          <- parms["birth.c"]
  biterate         <- parms["biterate"]
  prob.infection   <- parms["prob.infection"]
  infectiousness   <- parms["infectiousness"]
  resusceptible    <- parms["resusceptible"]
  death            <- parms["death"]
  recovery         <- parms["recovery"]
  treatment        <- parms["treatment"]
  recovery.st      <- parms["recovery.st"]
  emergence        <- parms["emergence"]
  rec.adj          <- parms["rec.adj"]
  prop.prophylaxis <- parms["prop.prophylaxis"]
  fit.adj          <- parms["fit.adj"]
  
  ## ----- Wildlife
  birth.w            <- parms["birth.w"]
  prob.infection.s.w <- parms["prob.infection.s.w"]
  prob.infection.r.w <- parms["prob.infection.r.w"]
  infectiousness.w   <- parms["infectiousness.w"]
  resusceptible.w    <- parms["resusceptible.w"]
  death.w            <- parms["death.w"]
  recovery.w         <- parms["recovery.w"]
  reversion          <- parms["reversion"]
  
  ## ----- Vectors
  K                <- parms["K"]
  feeding.rate     <-  parms["feeding.rate"]
  prob.infection.v <-  parms["prob.infection.v"]
  death.v <- parms["death.v"]
  birth.v <- parms["birth.v"]
  infectiousness.v <- parms["infectiousness.v"]
  
    
    # Population total ----
    N <- CS + CEs + CEr + CIs + CIr + CTs + CTr + CR +
      PS + PEs + PEr + PIs + PIr + PTs + PTr + PR +
      WS + WEs + WEr + WIs + WIr + WR
    C <- CS + CEs + CEr + CIs + CIr + CTs + CTr + CR
    P <- PS + PEs + PEr + PIs + PIr + PTs + PTr + PR
    W <- WS + WEs + WEr + WIs + WIr + WR
    V <- VS + VEs + VEr + VIs + VIr
    
    # Cattle ----
    # 
    # CS, CEs, CEr, CIs, CIr, CTs, CTr, CR
    
    dCS.dt <- birth.c * C - biterate * prob.infection * CS * VIs / N -  
      biterate * (prob.infection * fit.adj) * CS * VIr / N + resusceptible * CR - 
      death * CS
    
    dCEs.dt <- biterate * prob.infection * CS * VIs / N - infectiousness * 
      CEs - death * CEs 
    
    dCEr.dt <- biterate * (prob.infection * fit.adj) * CS * VIr / N - infectiousness * 
      CEr - death * CEr 
    
    dCIs.dt <- infectiousness * CEs - treatment * CIs - recovery  * CIs - 
      death * CIs
    
    dCIr.dt <- infectiousness * CEr - treatment * CIr - recovery  * CIr - 
      death * CIr
    
    dCTs.dt <- treatment * CIs - recovery.st  * CTs - death * CTs - emergence * CTs
    
    dCTr.dt <- treatment * CIr - (recovery * rec.adj)  * CTr - death * CTr + emergence * CTs
    
    dCR.dt <- recovery  * CIs + recovery  * CIr + recovery.st  * CTs +
      (recovery * rec.adj)  * CTr - resusceptible * CR - death * CR
    
    # dCTs.dt <- 0
    # 
    # dCTr.dt <- 0
    # 
    # dCR.dt <- treatment * CIs + treatment * CIr + recovery  * CIs + recovery  * CIr + recovery  * CTs + 
    #   recovery  * CTr - resusceptible * CR - death * CR
    
    
    # Cattle with prophylaxis ----
    # 
    # PS, PEs, PEr, PIs, PIr, PTs, PTr, PR
    
    dPS.dt <- birth.c * P - biterate * prob.infection * PS * VIs / N - 
      biterate * (prob.infection * fit.adj) * PS * VIr / N + resusceptible * PR - 
      death * PS
    
    dPEs.dt <- biterate * prob.infection * PS * VIs / N - infectiousness * 
      PEs - death * PEs 
    
    dPEr.dt <- biterate * (prob.infection * fit.adj) * PS *VIr / N - infectiousness * 
      PEr - death * PEr 
    
    dPIs.dt <- infectiousness * PEs - treatment * PIs - recovery.st  * PIs -
      death * PIs - emergence * PIs
    
    dPIr.dt <- infectiousness * PEr - treatment * PIr - recovery  * PIr -
      death * PIr + emergence * PIs
    
    dPTs.dt <- treatment * PIs - recovery.st  * PTs - death * PTs - emergence * PTs 
    
    dPTr.dt <- treatment * PIr - (recovery * rec.adj)  * PTr - death * PTr + emergence * PTs 
    
    dPR.dt <- recovery  * PIs + recovery  * PIr + recovery.st  * PTs + 
      (recovery * rec.adj)  * PTr - resusceptible * PR - death * PR
    
    # Wildlife ----
    # 
    # WS, WEs, WEr, WIs, WIr, WTs, WTr, WR #I CHANGED  death to death.w here
    
    dWS.dt <- birth.w * W - biterate * prob.infection * WS * VIs / N - 
      biterate * (prob.infection * fit.adj) * WS * VIr / N + resusceptible * WR - 
      death.w * WS
    
    dWEs.dt <- biterate * prob.infection * WS * VIs / N - infectiousness.w * 
      WEs - death.w * WEs 
    
    dWEr.dt <- biterate * (prob.infection * fit.adj) * WS * VIr / N - infectiousness.w * 
      WEr - death.w * WEr 
    
    dWIs.dt <- infectiousness.w * WEs - recovery.w * WIs - death.w * WIs + reversion * WIr
    
    dWIr.dt <- infectiousness.w * WEr - recovery.w * WIr - death.w * WIr - reversion * WIr
    
    dWR.dt <- recovery.w * WIs + recovery.w * WIr - resusceptible * WR - 
      death.w * WR
    
    # Tsetse ----
    # 
    # VS, VEs, VEr, VIs, VIr, 
    
    dVS.dt <- birth.v * V  * (1 - V / K ) - 
      
      prob.infection.v * biterate * (CIs/N) * VS - 
      prob.infection.v * biterate * (CIr/N) * VS - 
      prob.infection.v * biterate * (CTs/N) * VS -
      prob.infection.v * biterate * (CTr/N) * VS -
      prob.infection.v * biterate * (PIs/N) * VS - 
      prob.infection.v * biterate * (PIr/N) * VS - 
      prob.infection.v * biterate * (PTr/N) * VS - 
      prob.infection.v * biterate * (WIs/N) * VS - 
      prob.infection.v * biterate * (WIr/N) * VS - 
      death.v * VS
    
    dVEs.dt <-  prob.infection.v * biterate * (CIs/N) * VS + 
      prob.infection.v * biterate * (CTs/N) * VS + 
      prob.infection.v * biterate * (PIs/N) * VS + 
      prob.infection.v * biterate * (PTs/N) * VS + 
      prob.infection.v * biterate * (WIs/N) * VS - 
      infectiousness.v * VEs - death.v *VEs
    
    dVEr.dt <-  prob.infection.v * biterate * (CIr/N) * VS + 
      prob.infection.v * biterate * (CTr/N) * VS + 
      prob.infection.v * biterate * (PIr/N) * VS + 
      prob.infection.v * biterate * (PTr/N) * VS + 
      prob.infection.v * biterate * (WIr/N * VS ) - 
      infectiousness.v * VEr - death.v *VEr
    
    dVIs.dt <- infectiousness.v * VEs - death.v * VIs
    
    dVIr.dt <- infectiousness.v * VEr - death.v * VIr
    
    # Model output ----
    dX <- c(dCS.dt, dCEs.dt, dCEr.dt, dCIs.dt, dCIr.dt, dCTs.dt, dCTr.dt, dCR.dt, 
            dPS.dt, dPEs.dt, dPEr.dt, dPIs.dt, dPIr.dt, dPTs.dt, dPTr.dt, dPR.dt, 
            dWS.dt, dWEs.dt, dWEr.dt, dWIs.dt, dWIr.dt, dWR.dt, 
            dVS.dt, dVEs.dt, dVEr.dt, dVIs.dt, dVIr.dt)
    list(dX)


}


findGlobals(fun = AAT_AMR_dens_dep, merge = FALSE)$variables
