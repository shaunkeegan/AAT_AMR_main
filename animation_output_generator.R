library(animation)
library(deSolve)
## make sure ImageMagick has been installed in your system




## This file allows exploration of the model file AAT_AMR_main.R
## It is intended to test the model and explore various parameters. This is NOT
## intended to run completed scenarios. 

## Lead Author:   Shaun Keegan (shaun.keegan@glasgow.ac.uk)
## Other Authors: Louise Matthews (louise.mattthews@glasgow.ac.uk)

## Working Directory ----

setwd("/Users/shaunkeegan/Documents/OneDrive - University of Glasgow/research/AAT_AMR_main")

## Packages ----

library(deSolve)

## Model ----

source("model/AAT_AMR_main.R")


## Working Directory ----

setwd("/Users/shaunkeegan/Documents/OneDrive - University of Glasgow/research/AAT_AMR_main/output_graphics")

trt <- as.numeric(seq(0,100, by = 5))
saveGIF({
  for (i in 1:101) {

    p <- (i-1)/100
    
    
    ## Parameters ----
    
    # System
    
    prop.prophylaxis <- p
    
    # Cattle 
    birth.c          <- 1/365
    biterate         <- 0.3/4
    prob.infection.s <- 0.46
    prob.infection.r <- 0.46
    infectiousness   <- 1/9
    resusceptible    <- 1/100
    death            <- birth.c
    treatment        <- 0
    recovery.s       <- 0.01      
    recovery.r       <- 0.01  
    emergence       <- 1/365
    
    # Wildlife 
    birth.w          <- 1/365
    biterate.w         <- 0.3/4
    prob.infection.s.w <- 0.46
    prob.infection.r.w <- 0.46
    infectiousness.w   <- 1/9
    resusceptible.w    <- 1/100
    death.w            <- birth.w
    recovery.s.w       <- 0.01      
    recovery.r.w       <- 0.01  
    reversion        <- 0
    
    # Vectors
    birth.v          <-  1/30
    death.v          <-  1/30 
    feeding.rate     <-  3   
    prob.infection.v <-  0.025
    infectiousness.v <-  1/3
    
    params <- cbind(birth.c, biterate, prob.infection.s, prob.infection.r, 
                    infectiousness, resusceptible, death, treatment, recovery.s, 
                    recovery.r, birth.v, death.v, feeding.rate, prob.infection.v, 
                    infectiousness.v, emergence, reversion)
    
    
    ## Initial Conditions ----
    
    cattle <- 100 # Total number of cattle
    
    # C - Cattle
    CS  <- cattle * (1 - prop.prophylaxis)    # Susceptible
    CEs <- 0    # Exposed (drug sensitive strain)
    CEr <- 0    # Exposed (drug resistant strain)
    CIs <- 1    # Infected (drug sensitive strain)
    CIr <- 0    # Infected (drug resistant strain)
    CTs <- 0    # Treated (drug sensitive strain)
    CTr <- 0    # Treated (drug resistant strain)
    CR  <- 0    # Recovered
    
    # P - Prophylactically treated cattle
    PS  <- cattle * prop.prophylaxis    # Susceptible
    PEs <- 0    # Exposed (drug sensitive strain)
    PEr <- 0    # Exposed (drug resistant strain)
    PIs <- 0    # Infected (drug sensitive strain)
    PIr <- 0    # Infected (drug resistant strain)
    PTs <- 0    # Treated (drug sensitive strain)
    PTr <- 0    # Treated (drug resistant strain)
    PR  <- 0    # Recovered
    
    # W - Wildlife
    WS  <- 0   # Susceptible
    WEs <- 0    # Exposed (drug sensitive strain)
    WEr <- 0    # Exposed (drug resistant strain)
    WIs <- 0    # Infected (drug sensitive strain)
    WIr <- 0    # Infected (drug resistant strain)
    WR  <- 0    # Recovered
    
    # V - Vectors
    VS  <- 3000 # Susceptible
    VEs <- 0    # Exposed (drug sensitive strain) 
    VEr <- 0    # Exposed (drug resistant strain)
    VIs <- 0    # Infected (drug sensitive strain)
    VIr <- 0    # Infected (drug resistant strain) 
    
    inits <- cbind(CS, CEs, CEr, CIs, CIr, CTs, CTr, CR, 
                   PS, PEs, PEr, PIs, PIr, PTs, PTr, PR, 
                   WS, WEs, WEr, WIs, WIr, WR, 
                   VS, VEs, VEr, VIs, VIr)
    

    
    
    ## Times ----
    
    times <- seq(0,10000,1)
    
    
    ## RUN MODEL ----
    
    out <- ode(y = inits, parms = params, func = AAT_AMR_main, times = times)
    
    names <- c("times", "CS", "CEs", "CEr", "CIs", "CIr", "CTs", "CTr", "CR", 
               "PS", "PEs", "PEr", "PIs", "PIr", "PTs", "PTr", "PR", 
               "WS", "WEs", "WEr", "WIs", "WIr", "WR", 
               "VS", "VEs", "VEr", "VIs", "VIr")
    colnames(out) <- names
    colnames(out)
    #out
    out <- as.data.frame(out)
    
    
    par(mfrow=c(1,3))
    plot((out$CEs + out$CIs + out$CTs + out$CEr + out$CIr + out$CTr +
            out$PEs + out$PIs + out$PTs + out$PEr + out$PIr + out$PTr)  ~ out$times, type = 'l', ylim = c(0, 100), lwd = 3, 
         col = 'blue', main = paste('Prophylaxis Proportion ', 100 - (i-1), '%'), xlab = "Time", ylab = "Number")

    plot((out$CEr + out$CIr + out$CTr +
           out$PEr + out$PIr + out$PTr)  ~ out$times, type = 'l', ylim = c(0, 100), lwd = 3, 
         col = 'red', main = paste('Prophylaxis Proportion ', 100 - (i-1), '%'), xlab = "Time", ylab = "Number")
    
    plot((out$CEs + out$CIs + out$CTs + 
            out$PEs + out$PIs + out$PTs )  ~ out$times, type = 'l', ylim = c(0, 100), lwd = 3, 
         col = 'green', main = paste('Prophylaxis Proportion ', 100 - (i-1), '%'), xlab = "Time", ylab = "Number")
    
    

    
    
  }
}, fps = 2  , ani.width = 900, ani.height = 900)
