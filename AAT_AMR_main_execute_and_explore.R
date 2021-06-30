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

prop.prophylaxis <- 0.1

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
emergence       <- 1/100

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
emergence.w        <- 0

# Vectors
birth.v          <-  1/30
death.v          <-  1/30 
feeding.rate     <-  3   
prob.infection.v <-  0.025
infectiousness.v <-  1/3

params <- cbind(birth.c, biterate, prob.infection.s, prob.infection.r, 
               infectiousness, resusceptible, death, treatment, recovery.s, 
               recovery.r, birth.v, death.v, feeding.rate, prob.infection.v, 
               infectiousness.v, emergence, emergence.w)


## Initial Conditions ----

cattle <- 50 # Total number of cattle

# C - Cattle
CS  <- cattle * (1 - prop.prophylaxis) - 1   # Susceptible
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
WS  <- 50   # Susceptible
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

times <- seq(0,2000,1)


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


par(mfrow=c(2,2))
plot(out$CS ~ out$times, type = 'l', ylim = c(0, max(out[,2])+5), lwd = 3, 
     col = 'blue', main = "Cattle (No Prophylaxis)", xlab = "Time", ylab = "Number")
lines(out$CEs ~ out$times,lwd =3, col = 'orange') # Exposed
lines(out$CEr ~ out$times,lwd =3, col = 'darkorange') # Exposed
lines(out$CIs ~ out$times,lwd =3, col = 'red') # Infected
lines(out$CIr ~ out$times,lwd =3, col = 'darkred') # Infected
lines(out$CTs ~ out$times,lwd =3, col = 'green') # Treated
lines(out$CTr ~ out$times,lwd =3, col = 'darkgreen') # Treated
lines(out$CR ~ out$times,lwd =3, col = 'grey') # Recovered
lines((out$CEs + out$CEr + out$CIs + out$CIr + out$CTs + out$CTr + out$CR + out$CS) ~
        out$times, lty = 2)

plot(out$PS ~ out$times, type = 'l', ylim = c(0, max(out[,10])+5), col = 'blue',
     lwd = 3, main = "Cattle (with Prophylaxis)", xlab = "Time", ylab = "Number")
lines(out$PEs ~ out$times, lwd =3, col = 'orange') # Exposed
lines(out$PEr ~ out$times, lwd =3, col = 'darkorange') # Exposed
lines(out$PIs ~ out$times, lwd =3, col = 'red') # Infected
lines(out$PIr ~ out$times, lwd =3, col = 'darkred') # Infected
lines(out$PTs ~ out$times, lwd =3, col = 'green') # Treated
lines(out$PTr ~ out$times, lwd =3, col = 'darkgreen') # Treated
lines(out$PR  ~ out$times, lwd =3, col = 'grey') # Recovered
lines((out$PEs + out$PEr + out$PIs + out$PIr + out$PTs + out$PTr + out$PR + out$PS) ~
        out$times, lty = 2)

plot(out$WS ~ out$times, type = 'l', ylim = c(0, max(out[,18])+ 5), col = 'blue',
     lwd = 3, main = "Wildlife", xlab = "Time", ylab = "Number")
lines(out$WEs ~ out$times,lwd = 3, col = 'orange') # Exposed
lines(out$WEr ~ out$times,lwd = 3, col = 'darkorange') # Exposed
lines(out$WIs ~ out$times,lwd = 3, col = 'red') # Infected
lines(out$WIr ~ out$times,lwd = 3, col = 'darkred') # Infected
lines(out$WR ~ out$times,lwd = 3, col = 'grey') # Recovered
lines((out$WEs + out$WEr + out$WIs + out$WIr + out$WR + out$WS) ~
        out$times, lty = 2)

plot(out$VS ~ out$times, type = 'l', ylim = c(0, max(out[,24])+100), col = 'blue',
     lwd = 3, main = "Vector", xlab = "Time", ylab = "Number")
lines(out$VEs ~ out$times, lwd = 3, col = 'orange') # Exposed
lines(out$VEr ~ out$times, lwd = 3, col = 'darkorange') # Exposed
lines(out$VIs ~ out$times, lwd = 3, col = 'red') # Infected
lines(out$VIr ~ out$times, lwd = 3, col = 'darkred') # Infected
lines((out$VEs + out$VEr + out$VIs + out$VIr + out$VS) ~
        out$times, lty = 2)
