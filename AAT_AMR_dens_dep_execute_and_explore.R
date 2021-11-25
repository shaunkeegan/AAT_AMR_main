## This file allows exploration of the model file AAT_AMR_dens_dep.R
## It is intended to test the model and explore various parameters. This is NOT
## intended to run completed scenarios. 

## Lead Author:   Shaun Keegan (shaun.keegan@glasgow.ac.uk)
## Other Authors: Louise Matthews (louise.mattthews@glasgow.ac.uk)

## Working Directory ----

setwd("/Users/shaunkeegan/Documents/OneDrive - University of Glasgow/research/AAT_AMR_main")

## Packages ----

library(deSolve)

## Model ----

source("model/AAT_AMR_dens_dep.R")

## Parameters ----

# System

prop.prophylaxis <- 0

qf <- 0.96
qn <- 0.98
feed.cyc <- 4
p <- 0


# Cattle 
birth.c          <- 1/365
biterate         <- (0.3/feed.cyc)
prob.infection.s <- 0.46
prob.infection.r <- 0.46
infectiousness   <- 1/20
resusceptible    <- 1/100
death            <- birth.c
treatment        <- 1/6
recovery.s       <- 0.01      
recovery.r       <- 0.01  
emergence       <- 1/365

# Wildlife 
birth.w          <- 0
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
birth.v          <-  0.03*2
death.v          <-  -1 *log((1-p)* qf * qn^feed.cyc)/feed.cyc 
feeding.rate     <-  3   
prob.infection.v <-  0.025
infectiousness.v <-  1/3

params <- cbind(birth.c, biterate, prob.infection.s, prob.infection.r, 
                infectiousness, resusceptible, death, treatment, recovery.s, 
                recovery.r, birth.v, death.v, feeding.rate, prob.infection.v, 
                infectiousness.v, emergence, reversion)


## Initial Conditions ----

cattle <- 0 # Total number of cattle

# C - Cattle
CS  <- cattle * (1 - prop.prophylaxis) - 1   # Susceptible
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
WS  <- 0   # Susceptible
WEs <- 0    # Exposed (drug sensitive strain)
WEr <- 0    # Exposed (drug resistant strain)
WIs <- 0    # Infected (drug sensitive strain)
WIr <- 0    # Infected (drug resistant strain)
WR  <- 0    # Recovered

# V - Vectors
VS  <- 5000 # Susceptible
VEs <- 0    # Exposed (drug sensitive strain) 
VEr <- 0    # Exposed (drug resistant strain)
VIs <- 0    # Infected (drug sensitive strain)
VIr <- 0    # Infected (drug resistant strain) 
K <- 10000 

inits <- cbind(CS, CEs, CEr, CIs, CIr, CTs, CTr, CR, 
               PS, PEs, PEr, PIs, PIr, PTs, PTr, PR, 
               WS, WEs, WEr, WIs, WIr, WR, 
               VS, VEs, VEr, VIs, VIr)




simpleR0 <- prob.infection.v * biterate * (VS/CS) * 
  (1/(treatment + recovery.s + death)) * (infectiousness/(infectiousness + death)) +
  (biterate*prob.infection.s/death) * (infectiousness/(infectiousness + death))





## Times ----

times <- seq(0,2200,1)


## RUN MODEL ----

out <- ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times)

names <- c("times", "CS", "CEs", "CEr", "CIs", "CIr", "CTs", "CTr", "CR", 
           "PS", "PEs", "PEr", "PIs", "PIr", "PTs", "PTr", "PR", 
           "WS", "WEs", "WEr", "WIs", "WIr", "WR", 
           "VS", "VEs", "VEr", "VIs", "VIr")
colnames(out) <- names
colnames(out)
out
out <- as.data.frame(out)


#par(mfrow=c(2,2))
#plot(out$CS ~ out$times, type = 'l', ylim = c(0, max(out[,2])+5), lwd = 3, 
#     col = 'blue', main = paste(round(simpleR0, 2)), xlab = "Time", ylab = "Number")
#lines(out$CEs ~ out$times,lwd =3, col = 'orange') # Exposed
#lines(out$CEr ~ out$times,lwd =3, col = 'darkorange') # Exposed
#lines(out$CIs ~ out$times,lwd =3, col = 'red') # Infected
#lines(out$CIr ~ out$times,lwd =3, col = 'darkred') # Infected
#lines(out$CTs ~ out$times,lwd =3, col = 'green') # Treated
#lines(out$CTr ~ out$times,lwd =3, col = 'darkgreen') # Treated
#lines(out$CR ~ out$times,lwd =3, col = 'grey') # Recovered
#lines((out$CEs + out$CEr + out$CIs + out$CIr + out$CTs + out$CTr + out$CR + out$CS) ~
#        out$times, lty = 2)
#
#plot(out$PS ~ out$times, type = 'l', ylim = c(0, max(out[,10])+5), col = 'blue',
#     lwd = 3, main = "Cattle (with Prophylaxis)", xlab = "Time", ylab = "Number")
#lines(out$PEs ~ out$times, lwd =3, col = 'orange') # Exposed
#lines(out$PEr ~ out$times, lwd =3, col = 'darkorange') # Exposed
#lines(out$PIs ~ out$times, lwd =3, col = 'red') # Infected
#lines(out$PIr ~ out$times, lwd =3, col = 'darkred') # Infected
#lines(out$PTs ~ out$times, lwd =3, col = 'green') # Treated
#lines(out$PTr ~ out$times, lwd =3, col = 'darkgreen') # Treated
#lines(out$PR  ~ out$times, lwd =3, col = 'grey') # Recovered
#lines((out$PEs + out$PEr + out$PIs + out$PIr + out$PTs + out$PTr + out$PR + out$PS) ~
#        out$times, lty = 2)
#
#plot(out$WS ~ out$times, type = 'l', ylim = c(0, max(out[,18])+ 5), col = 'blue',
#     lwd = 3, main = "Wildlife", xlab = "Time", ylab = "Number")
#lines(out$WEs ~ out$times,lwd = 3, col = 'orange') # Exposed
#lines(out$WEr ~ out$times,lwd = 3, col = 'darkorange') # Exposed
#lines(out$WIs ~ out$times,lwd = 3, col = 'red') # Infected
#lines(out$WIr ~ out$times,lwd = 3, col = 'darkred') # Infected
#lines(out$WR ~ out$times,lwd = 3, col = 'grey') # Recovered
#lines((out$WEs + out$WEr + out$WIs + out$WIr + out$WR + out$WS) ~
#        out$times, lty = 2)
par(mfrow=c(1,1))
plot(out$VS ~ out$times, type = 'l', ylim = c(0, max(out[,24])+100), col = 'blue',
     lwd = 3, main = "Vector", xlab = "Time", ylab = "Number")
lines(out$VEs ~ out$times, lwd = 3, col = 'orange') # Exposed
lines(out$VEr ~ out$times, lwd = 3, col = 'darkorange') # Exposed
lines(out$VIs ~ out$times, lwd = 3, col = 'red') # Infected
lines(out$VIr ~ out$times, lwd = 3, col = 'darkred') # Infected
lines((out$VEs + out$VEr + out$VIs + out$VIr + out$VS) ~
        out$times, lty = 2)



