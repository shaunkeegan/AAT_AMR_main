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

## Parameters ----

# System

prop.prophylaxis <- 0

# Cattle 
birth.c          <- 0
biterate         <- 0.3/4
prob.infection.s <- 0.46
prob.infection.r <- 0.46
infectiousness   <- 0.05
resusceptible    <- 0.01
death            <- 0
treatment        <- 0
recovery.s       <- 0.01      
recovery.r       <- 0.01      

# Wildlife 
birth.w          <- 0
biterate.w         <- 0.3/4
prob.infection.s.w <- 0.46
prob.infection.r.w <- 0.46
infectiousness.w   <- 0.05
resusceptible.w    <- 0.01
death.w            <- 0
recovery.s.w       <- 0.01      
recovery.r.w       <- 0.01     

# Vectors
birth.v          <-  0.5
death.v          <-  0.5      
feeding.rate     <-  3   
prob.infection.v <-  0.025
infectiousness.v <-  0.05

params <- cbind(birth.c, biterate, prob.infection.s, prob.infection.r, 
               infectiousness, resusceptible, death, treatment, recovery.s, 
               recovery.r, birth.v, death.v, feeding.rate, prob.infection.v, 
               infectiousness.v)


## Initial Conditions ----

cattle <- 1 # Total number of cattle

# C - Cattle
CS  <- cattle * (1 - prop.prophylaxis)   # Susceptible
CEs <- 0    # Exposed (drug sensitive strain)
CEr <- 0    # Exposed (drug resistant strain)
CIs <- 0    # Infected (drug sensitive strain)
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
WS  <- 0    # Susceptible
WEs <- 0    # Exposed (drug sensitive strain)
WEr <- 0    # Exposed (drug resistant strain)
WIs <- 0    # Infected (drug sensitive strain)
WIr <- 0    # Infected (drug resistant strain)
WR  <- 0    # Recovered

# V - Vectors
VS  <- 2999 # Susceptible
VEs <- 0    # Exposed (drug sensitive strain) 
VEr <- 0    # Exposed (drug resistant strain)
VIs <- 1    # Infected (drug sensitive strain)
VIr <- 0    # Infected (drug resistant strain) 

inits <- cbind(CS, CEs, CEr, CIs, CIr, CTs, CTr, CR, 
                PS, PEs, PEr, PIs, PIr, PTs, PTr, PR, 
                WS, WEs, WEr, WIs, WIr, WR, 
                VS, VEs, VEr, VIs, VIr)









## Times ----

times <- seq(0,1000,1)


## RUN MODEL ----

out <- ode(y = inits, parms = params, func = AAT_AMR_main, times = times)
out


par(mfrow=c(2,2))
plot(out[,2] ~ out[,1], type = 'l', ylim = c(0, max(out[,2])), col = 'blue', 
     main = "Cattle (No Prophylaxis)")
lines(out[,3] ~ out[,1], col = 'orange') # Exposed
lines(out[,4] ~ out[,1], col = 'darkorange') # Exposed
lines(out[,5] ~ out[,1], col = 'red') # Infected
lines(out[,6] ~ out[,1], col = 'darkred') # Infected
lines(out[,7] ~ out[,1], col = 'green') # Treated
lines(out[,8] ~ out[,1], col = 'darkgreen') # Treated
lines(out[,9] ~ out[,1], col = 'grey') # Recovered

plot(out[,10] ~ out[,1], type = 'l', ylim = c(0, max(out[,10])), col = 'blue',
     main = "Cattle (with Prophylaxis)")
lines(out[,11] ~ out[,1], col = 'orange') # Exposed
lines(out[,12] ~ out[,1], col = 'darkorange') # Exposed
lines(out[,13] ~ out[,1], col = 'red') # Infected
lines(out[,14] ~ out[,1], col = 'darkred') # Infected
lines(out[,15] ~ out[,1], col = 'green') # Treated
lines(out[,16] ~ out[,1], col = 'darkgreen') # Treated
lines(out[,17] ~ out[,1], col = 'grey') # Recovered


plot(out[,18] ~ out[,1], type = 'l', ylim = c(0, max(out[,18])), col = 'blue',
     main = "Wildlife")
lines(out[,19] ~ out[,1], col = 'orange') # Exposed
lines(out[,20] ~ out[,1], col = 'darkorange') # Exposed
lines(out[,21] ~ out[,1], col = 'red') # Infected
lines(out[,22] ~ out[,1], col = 'darkred') # Infected
lines(out[,23] ~ out[,1], col = 'grey') # Recovered


plot(out[,24] ~ out[,1], type = 'l', ylim = c(0, max(out[,24])), col = 'blue',
     main = "Vector")
lines(out[,25] ~ out[,1], col = 'orange') # Exposed
lines(out[,26] ~ out[,1], col = 'darkorange') # Exposed
lines(out[,27] ~ out[,1], col = 'red') # Infected
lines(out[,28] ~ out[,1], col = 'darkred') # Infected
