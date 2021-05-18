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
birth.c          <- 0.1
biterate         <- 0.3/4
prob.infection.s <- 0.46
prob.infection.r <- 0.46
infectiousness   <- 0.05
resusceptible    <- 0.01
death            <- 0.1
treatment        <- 0
recovery.s       <- 0.01      
recovery.r       <- 0.01      

# Vectors
birth.v          <-  0
death.v          <-  0      
feeding.rate     <-  3   
prob.infection.v <-  0.025
infectiousness.v <-  0.05

params <- cbind(birth.c, biterate, prob.infection.s, prob.infection.r, 
               infectiousness, resusceptible, death, treatment, recovery.s, 
               recovery.r, birth.v, death.v, feeding.rate, prob.infection.v, 
               infectiousness.v)


## Initial Conditions ----

cattle <- 10 # Total number of cattle

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
VS  <- 0    # Susceptible
VEs <- 0    # Exposed (drug sensitive strain) 
VEr <- 0    # Exposed (drug resistant strain)
VIs <- 0    # Infected (drug sensitive strain)
VIr <- 0    # Infected (drug resistant strain) 

inits <- cbind(CS, CEs, CEr, CIs, CIr, CTs, CTr, CR, 
                PS, PEs, PEr, PIs, PIr, PTs, PTr, PR, 
                WS, WEs, WEr, WIs, WIr, WR, 
                VS, VEs, VEr, VIs, VIr)









## Times ----

times <- seq(0,10,1)


## RUN MODEL ----

out <- ode(y = inits, parms = params, func = AAT_AMR_main, times = times)
out

