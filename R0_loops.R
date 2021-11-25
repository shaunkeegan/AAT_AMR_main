## This file allows exploration of the model file AAT_AMR_main.R
## It is intended to test the model and explore various parameters. This is NOT
## intended to run completed scenarios. 

## Lead Author:   Shaun Keegan (shaun.keegan@glasgow.ac.uk)
## Other Authors: Louise Matthews (louise.mattthews@glasgow.ac.uk)

## Working Directory ----

#setwd("/Users/shaunkeegan/Documents/OneDrive - University of Glasgow/research/AAT_AMR_main/model")

## Packages ----

library(deSolve)

## Model ----

source("model/AAT_AMR_main.R")
source("model/r0_functions.R")

#N_wl <- seq(0,100, by = 100)
#Trt_pop <- seq(0,10, by = 10)

N_wl <- seq(0,1000,by=25)
Trt_pop <- seq(0,0.99, by = 0.05)
vec_pop <- c(1000,3000,5000)

#R0_out <- matrix(1:30, ncol = 30)
#R0_out[] <- NA
#R0_out <- as.data.frame(R0_out)
#colnames(R0_out) <- c("EqNC","R0","NW", "TrtPrp")

df <- data.frame(CS =c(), total.cattle = c(), treat_prop = c(), W_st = c(), 
                 R_eq_sen = c(), R0_sen = c(),R_eq_res = c(), R0_res = c(),
                 No_trt_cat = c(), Incidence = c(), Vector_no = c(), 
                 Prob_onward_tran = c(), Risk = c())

for(NV in vec_pop){
  for(prop_treat in Trt_pop){
    for(NW in N_wl){
      ## Parameters ----
      
      # System
      
      prop.prophylaxis <- 0
      # Cattle 
      birth.c          <- 1/365
      biterate         <- 0.8/4
      prob.infection   <- 0.46         
      infectiousness   <- 1/15
      resusceptible    <- 1/100
      death            <- birth.c
      recovery         <- 1/100 
      #prop_treat       <- Trt_pop[j]   #LM: extra param to help us specify the treatment rate
      treatment        <- prop_treat * (recovery + death)/(1 - prop_treat)
      recovery.st      <- recovery * 250 #LM: adjusted so that R0 drops below 1 when 99% treated to reflect Hargrove
      emergence        <- 0
      fit.adj          <- 0.95  #LM: prefer to always use 0.8 etc rather than .8 as much less likely to have mistakes through typos or misreading 
      rec.adj          <- 1
      
      # Wildlife 
      birth.w            <- 1/365
      biterate.w         <- 0.7/4 #think we should keep the same biterate
      prob.infection.s.w <- 0.46
      prob.infection.r.w <- 0.46
      infectiousness.w   <- 1/20
      resusceptible.w    <- 1/100
      death.w            <- birth.w
      recovery.w         <- recovery     
      reversion          <- 0
      
      # Vectors
      birth.v          <-  0.03
      death.v          <-  birth.v #LM: better this way than have to type in 0.03 in each place
      feeding.rate     <-  0   
      prob.infection.v <-  0.025
      incubation       <- 20
      infectiousness.v <-  death.v * exp(- death.v * incubation) / (1 - exp(- death.v *incubation) )
      
      params <- cbind(birth.c, biterate, prob.infection, fit.adj, rec.adj, recovery.st,  #LM: replaced trt.adj with rec.adj
                      infectiousness, resusceptible, death, treatment, recovery, 
                      birth.v, death.v, feeding.rate, prob.infection.v, 
                      infectiousness.v, emergence, reversion)
      
      
      ## Initial Conditions ----
      
      #cattle <- 50 # Total number of cattle   #LM: just remove this line
      
      # C - Cattle
      NC  <- 50
      CIr <- 0    # Infected (drug resistant strain)
      CIs <- 1    # Infected (drug sensitive strain)
      CS  <- NC * (1 - prop.prophylaxis) - CIs - CIr # Susceptible
      CEs <- 0    # Exposed (drug sensitive strain)
      CEr <- 0    # Exposed (drug resistant strain)
      
      CTs <- 0    # Treated (drug sensitive strain)
      CTr <- 0    # Treated (drug resistant strain)
      CR  <- 0    # Recovered
      
      # P - Prophylactically treated cattle
      PS  <- NC * prop.prophylaxis    # Susceptible   #LM: changed cattle to NC
      PEs <- 0    # Exposed (drug sensitive strain)
      PEr <- 0    # Exposed (drug resistant strain)
      PIs <- 0    # Infected (drug sensitive strain)
      PIr <- 0    # Infected (drug resistant strain)
      PTs <- 0    # Treated (drug sensitive strain)
      PTr <- 0    # Treated (drug resistant strain)
      PR  <- 0    # Recovered
      
      # W - Wildlife
      #NW  <- N_wl[i]
      WIs <- 0    # Infected (drug sensitive strain)
      WS  <- NW-WIs  # Susceptible
      WEs <- 0    # Exposed (drug sensitive strain)
      WEr <- 0    # Exposed (drug resistant strain)
      WIr <- 0    # Infected (drug resistant strain)
      WR  <- 0    # Recovered
      
      # V - Vectors
      #NV  <- 
      VS  <- NV # Susceptible
      VEs <- 0    # Exposed (drug sensitive strain) 
      VEr <- 0    # Exposed (drug resistant strain)
      VIs <- 0    # Infected (drug sensitive strain)
      VIr <- 0    # Infected (drug resistant strain) 
      
      inits <- cbind(CS, CEs, CEr, CIs, CIr, CTs, CTr, CR, 
                     PS, PEs, PEr, PIs, PIr, PTs, PTr, PR, 
                     WS, WEs, WEr, WIs, WIr, WR, 
                     VS, VEs, VEr, VIs, VIr)
      
      
      R0sen <- r0sen(inits, parms)  #R0 function shouldn't need "inits"
      R0sen
      R0s <- R0sen[1,"R0s"]
      
      R0res <- r0res(inits, parms)
      R0res
      R0r <- R0res[1,"R0r"]
      
      ## Times ----
      
      times <- seq(0,3000,1)
      
      
      ## RUN MODEL ----
      
      out <- ode(y = inits, parms = params, func = AAT_AMR_main, times = times)
      
      names <- c("times", "CS", "CEs", "CEr", "CIs", "CIr", "CTs", "CTr", "CR", 
                 "PS", "PEs", "PEr", "PIs", "PIr", "PTs", "PTr", "PR", 
                 "WS", "WEs", "WEr", "WIs", "WIr", "WR", 
                 "VS", "VEs", "VEr", "VIs", "VIr")
      colnames(out) <- names
      colnames(out)
      out
      out <- as.data.frame(out)
      last <- tail(out,1)
      
      
      last$total.cattle <- last$CS + last$CEs + last$CEr + last$CIs + last$CIr + last$CTs + last$CTr + last$CR
      
      last$treat_prop <- treatment/(treatment + recovery + death)
      
      #Check get 1 at equilibrium when run with only sensitive strains
      #fraction of cattle available for infection by sensitive strain
      fC <- last$CS / NC
      #fraction of vectors available for infection by sensitive strain
      fV <- last$VS / NV
      #fraction of wildlife available for infection by sensitive strain
      if (NW > 0){fW = last$WS / NW} else {fW = 0}
      
      Rsen <- fC*R0sen[1] * fV*R0sen[3] + fW * R0sen[2] * fV *R0sen[4]  #Hurrah
      Rres <- fC*R0res[1] * fV*R0res[3] + fW * R0res[2] * fV *R0res[4]
      
      
      df = rbind(df, data.frame(CS = last$CS, total.cattle = last$total.cattle, treat_prop = last$treat_prop, W_st = out[1, "WS"],
                                R_eq_sen = Rsen, R0_sen = R0s, R_eq_res = Rres, R0_res = R0r, No_trt_cat = treatment * last$CIs * 365.25,
                                Incidence = infectiousness * last$CEs * 365.25, Vector_no = NV, Prob_onward_tran = 1-dpois(0,Rres),
                                Risk = (1-dpois(0,Rres)) * treatment * last$CIs * 365.25 ))
      
    }
  }
}


tail(df)
#df.16oct <- df
