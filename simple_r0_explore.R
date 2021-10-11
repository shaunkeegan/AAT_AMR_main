## This file allows exploration of the model file AAT_AMR_main.R
## It is intended to test the model and explore various parameters. This is NOT
## intended to run completed scenarios. 

## Lead Author:   Shaun Keegan (shaun.keegan@glasgow.ac.uk)
## Other Authors: Louise Matthews (louise.mattthews@glasgow.ac.uk)

## Working Directory ----

setwd("/Users/shaunkeegan/Documents/OneDrive - University of Glasgow/research/AAT_AMR_main/model")

## Packages ----

library(deSolve)

## Model ----

source("AAT_AMR_main.R")

## Parameters ----

# System

prop.prophylaxis <- 0
# Cattle 
birth.c          <- 1/365
biterate         <- 0.8/4
prob.infection <- 0.46         
infectiousness   <- 1/15
resusceptible    <- 1/100
death            <- birth.c
treatment        <- 1/10
recovery       <- 1/100   
recovery.st    <- recovery * 10
emergence       <- 0
fit.adj         <- .8
rec.adj         <- .1

# Wildlife 
birth.w          <- 1/365
biterate.w         <- 0.7/4 #think we should keep the same biterate
prob.infection.s.w <- 0.46
prob.infection.r.w <- 0.46
infectiousness.w   <- 1/20
resusceptible.w    <- 1/100
death.w            <- birth.w
recovery.w       <- recovery     
reversion        <- 0

# Vectors
birth.v          <-  0.03
death.v          <-  0.03
feeding.rate     <-  0   
prob.infection.v <-  0.025
incubation <- 20
infectiousness.v <-  death.v * exp(- death.v * incubation) / (1 - exp(- death.v *incubation) )

params <- cbind(birth.c, biterate, prob.infection, fit.adj,trt.adj,recovery.st, 
                infectiousness, resusceptible, death, treatment, recovery, 
                birth.v, death.v, feeding.rate, prob.infection.v, 
                infectiousness.v, emergence, reversion)



## Initial Conditions ----

cattle <- 50 # Total number of cattle

# C - Cattle
NC <- cattle
CIr <- 1    # Infected (drug resistant strain)
CIs <- 1    # Infected (drug sensitive strain)
CS  <- NC * (1 - prop.prophylaxis) - CIs - CIr # Susceptible
CEs <- 0    # Exposed (drug sensitive strain)
CEr <- 0    # Exposed (drug resistant strain)

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
NW <- 0
WIs <- 0    # Infected (drug sensitive strain)
WS  <- NW-WIs  # Susceptible
WEs <- 0    # Exposed (drug sensitive strain)
WEr <- 0    # Exposed (drug resistant strain)
WIr <- 0    # Infected (drug resistant strain)
WR  <- 0    # Recovered

# V - Vectors
NV <- 5000
VS  <- NV # Susceptible
VEs <- 0    # Exposed (drug sensitive strain) 
VEr <- 0    # Exposed (drug resistant strain)
VIs <- 0    # Infected (drug sensitive strain)
VIr <- 0    # Infected (drug resistant strain) 

inits <- cbind(CS, CEs, CEr, CIs, CIr, CTs, CTr, CR, 
               PS, PEs, PEr, PIs, PIr, PTs, PTr, PR, 
               WS, WEs, WEr, WIs, WIr, WR, 
               VS, VEs, VEr, VIs, VIr)


R0sen <- r0sen(inits, parms)
R0s <- R0sen[1,"R0s"]

R0res <- r0res(inits, parms)
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
out$total.cattle <- out$CS + out$CEs + out$CEr + out$CIs + out$CIr + out$CTs + out$CTr + out$CR


par(mfrow=c(3,2))
plot(out$CS ~ out$times, type = 'l', ylim = c(0, NC), lwd = 3, 
     col = 'blue', main = paste(round(R0s, 4)^2), xlab = "Time", ylab = "Number")
lines(out$CEs ~ out$times,lwd =3, col = 'orange') # Exposed
#lines(out$CEr ~ out$times,lwd =3, col = 'darkorange') # Exposed
lines(out$CIs ~ out$times,lwd =3, col = 'red') # Infected
#lines(out$CIr ~ out$times,lwd =3, col = 'darkred') # Infected
#lines(out$CTs ~ out$times,lwd =3, col = 'green') # Treated
#lines(out$CTr ~ out$times,lwd =3, col = 'darkgreen') # Treated
#lines(out$CR ~ out$times,lwd =3, col = 'grey') # Recovered
lines((out$CEs + out$CEr + out$CIs + out$CIr + out$CTs + out$CTr + out$CR + out$CS) ~
        out$times, lty = 2)

plot(out$CS ~ out$times, type = 'l', ylim = c(0, 0.1), lwd = 3, 
     col = 'blue', main = paste(round(R0s^2, 4)), xlab = "Time", ylab = "Number")
lines(out$CEs ~ out$times,lwd =3, col = 'orange') # Exposed
#lines(out$CEr ~ out$times,lwd =3, col = 'darkorange') # Exposed
lines(out$CIs ~ out$times,lwd =3, col = 'red') # Infected
#lines(out$CIr ~ out$times,lwd =3, col = 'darkred') # Infected
#lines(out$CTs ~ out$times,lwd =3, col = 'green') # Treated
#lines(out$CTr ~ out$times,lwd =3, col = 'darkgreen') # Treated
#lines(out$CR ~ out$times,lwd =3, col = 'grey') # Recovered
lines((out$CEs + out$CEr + out$CIs + out$CIr + out$CTs + out$CTr + out$CR + out$CS) ~
        out$times, lty = 2)

plot(out$CS ~ out$times, type = 'l', ylim = c(0, NC), lwd = 3, 
    col = 'blue', main = paste(round(R0r^2, 4)), xlab = "Time", ylab = "Number")
#lines(out$CEs ~ out$times,lwd =3, col = 'orange') # Exposed
lines(out$CEr ~ out$times,lwd =3, col = 'darkorange') # Exposed
#lines(out$CIs ~ out$times,lwd =3, col = 'red') # Infected
lines(out$CIr ~ out$times,lwd =3, col = 'darkred') # Infected
#lines(out$CTs ~ out$times,lwd =3, col = 'green') # Treated
#lines(out$CTr ~ out$times,lwd =3, col = 'darkgreen') # Treated
#lines(out$CR ~ out$times,lwd =3, col = 'grey') # Recovered
lines((out$CEs + out$CEr + out$CIs + out$CIr + out$CTs + out$CTr + out$CR + out$CS) ~
        out$times, lty = 2)

plot(out$CS ~ out$times, type = 'l', ylim = c(0, 0.1), lwd = 3, 
     col = 'blue', main = paste(round(R0r^2, 4)), xlab = "Time", ylab = "Number")
#lines(out$CEs ~ out$times,lwd =3, col = 'orange') # Exposed
lines(out$CEr ~ out$times,lwd =3, col = 'darkorange') # Exposed
#lines(out$CIs ~ out$times,lwd =3, col = 'red') # Infected
lines(out$CIr ~ out$times,lwd =3, col = 'darkred') # Infected
#lines(out$CTs ~ out$times,lwd =3, col = 'green') # Treated
#lines(out$CTr ~ out$times,lwd =3, col = 'darkgreen') # Treated
#lines(out$CR ~ out$times,lwd =3, col = 'grey') # Recovered
lines((out$CEs + out$CEr + out$CIs + out$CIr + out$CTs + out$CTr + out$CR + out$CS) ~
        out$times, lty = 2)
# 
#plot(out$PS ~ out$times, type = 'l', ylim = c(0, NC), col = 'blue',
#     lwd = 3, main = paste(round(R0s, 4)), xlab = "Time", ylab = "Number")
#lines(out$PEs ~ out$times, lwd =3, col = 'orange') # Exposed
# lines(out$PEr ~ out$times, lwd =3, col = 'darkorange') # Exposed
#lines(out$PIs ~ out$times, lwd =3, col = 'red') # Infected
# lines(out$PIr ~ out$times, lwd =3, col = 'darkred') # Infected
# lines(out$PTs ~ out$times, lwd =3, col = 'green') # Treated
# lines(out$PTr ~ out$times, lwd =3, col = 'darkgreen') # Treated
# lines(out$PR  ~ out$times, lwd =3, col = 'grey') # Recovered
# lines((out$PEs + out$PEr + out$PIs + out$PIr + out$PTs + out$PTr + out$PR + out$PS) ~
#         out$times, lty = 2)

#plot(out$PS ~ out$times, type = 'l', ylim = c(0, 0.1), col = 'blue',
#     lwd = 3, main = paste(round(R0s, 4)), xlab = "Time", ylab = "Number")
#lines(out$PEs ~ out$times, lwd =3, col = 'orange') # Exposed
# lines(out$PEr ~ out$times, lwd =3, col = 'darkorange') # Exposed
#lines(out$PIs ~ out$times, lwd =3, col = 'red') # Infected
# lines(out$PIr ~ out$times, lwd =3, col = 'darkred') # Infected
# lines(out$PTs ~ out$times, lwd =3, col = 'green') # Treated
# lines(out$PTr ~ out$times, lwd =3, col = 'darkgreen') # Treated
# lines(out$PR  ~ out$times, lwd =3, col = 'grey') # Recovered
# lines((out$PEs + out$PEr + out$PIs + out$PIr + out$PTs + out$PTr + out$PR + out$PS) ~
#         out$times, lty = 2)
# 
plot(out$WS ~ out$times, type = 'l', ylim = c(0, NW ), col = 'blue',
     lwd = 3, main = "Wildlife", xlab = "Time", ylab = "Number")
lines(out$WEs ~ out$times,lwd = 3, col = 'orange') # Exposed
lines(out$WEr ~ out$times,lwd = 3, col = 'darkorange') # Exposed
lines(out$WIs ~ out$times,lwd = 3, col = 'red') # Infected
lines(out$WIr ~ out$times,lwd = 3, col = 'darkred') # Infected
lines(out$WR ~ out$times,lwd = 3, col = 'grey') # Recovered
lines((out$WEs + out$WEr + out$WIs + out$WIr + out$WR + out$WS) ~
        out$times, lty = 2)

plot(out$WS ~ out$times, type = 'l', ylim = c(0, 0.1 ), col = 'blue',
     lwd = 3, main = "Wildlife", xlab = "Time", ylab = "Number")
lines(out$WEs ~ out$times,lwd = 3, col = 'orange') # Exposed
lines(out$WEr ~ out$times,lwd = 3, col = 'darkorange') # Exposed
lines(out$WIs ~ out$times,lwd = 3, col = 'red') # Infected
lines(out$WIr ~ out$times,lwd = 3, col = 'darkred') # Infected
lines(out$WR ~ out$times,lwd = 3, col = 'grey') # Recovered
lines((out$WEs + out$WEr + out$WIs + out$WIr + out$WR + out$WS) ~
        out$times, lty = 2)
# 
# plot(out$VS ~ out$times, type = 'l', ylim = c(0, max(out[,24])+100), col = 'blue',
#      lwd = 3, main = "Vector", xlab = "Time", ylab = "Number")
# lines(out$VEs ~ out$times, lwd = 3, col = 'orange') # Exposed
# lines(out$VEr ~ out$times, lwd = 3, col = 'darkorange') # Exposed
# lines(out$VIs ~ out$times, lwd = 3, col = 'red') # Infected
# lines(out$VIr ~ out$times, lwd = 3, col = 'darkred') # Infected
# lines((out$VEs + out$VEr + out$VIs + out$VIr + out$VS) ~
#         out$times, lty = 2)

# plot(out$CS ~ out$times, type = 'l', lwd = 3,
#      col = 'blue', main = paste(round(R0, 2)), xlab = "Time", ylab = "Number")
# lines(out$CEs ~ out$times,lwd =3, col = 'orange') # Exposed
# lines(out$CEr ~ out$times,lwd =3, col = 'darkorange') # Exposed
# lines(out$CIs ~ out$times,lwd =3, col = 'red') # Infected
# lines(out$CIr ~ out$times,lwd =3, col = 'darkred') # Infected
# lines(out$CTs ~ out$times,lwd =3, col = 'green') # Treated
# lines(out$CTr ~ out$times,lwd =3, col = 'darkgreen') # Treated
# lines(out$CR ~ out$times,lwd =3, col = 'grey') # Recovered
# lines((out$CEs + out$CEr + out$CIs + out$CIr + out$CTs + out$CTr + out$CR + out$CS) ~
#         out$times, lty = 2)
# 
# simpleR0



head(out)
tail(out)

R0s
R0r


final.prev.cattle <- ( out[max(times), "CIs"] + out[max(times), "CTs"]) / 
  (out[max(times), "CEs"] + out[max(times), "CIs"] + out[max(times), "CTs"] + out[max(times), "CS"] + out[max(times), "CR"]) 
final.prev.cattle

final.prev.flies <- (out[max(times), "VIs"]) / 
  (out[max(times), "VEs"] + out[max(times), "VIs"] + out[max(times), "VS"]) 
final.prev.flies





NC <- tail(out$CS, 1)
NW <- tail(out$WS, 1)
NV <- tail(out$VS, 1)

RH1Vs <- (NC/(N))*(biterate*prob.infection/death.v) * (infectiousness/(infectiousness + death))
RWVs <- (NW/N)*(biterate*prob.infection/death.v) * (infectiousness.w/(infectiousness.w + death.w))
RVH1s <- prob.infection.v * biterate * (NV/N) * (1/(treatment + recovery + death)) * (infectiousness.v/(infectiousness.v + death.v)) +
  prob.infection.v * biterate * (NV/N) * (1/(recovery.st + death)) * (infectiousness.v/(infectiousness.v + death.v)) * 
  (treatment/((treatment + recovery + death)))
RVWs  <- prob.infection.v * biterate * (NV/N) * (1/(recovery.w + death.w)) * (infectiousness.v/(infectiousness.v + death.v))

R0s <- sqrt(RH1Vs * RVH1s + RWVs * RVWs)
R0s

R0sen
tail(out$CS, 1) * as.numeric(R0sen[1,"RH1Vs"])

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

R0r <- sqrt(RH1Vr * RVH1r + RWVr * RVWr)
R0r
tail(out$CS, 1) * as.numeric(R0res[1,"RH1Vr"])


