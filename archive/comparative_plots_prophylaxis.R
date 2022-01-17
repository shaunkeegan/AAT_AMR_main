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

emergence.rate = 1/365


output.data <- matrix(1:707, ncol = 7)
output.data[] <- NA
output.data[,1] <- seq(0,100,1)

for (i in 1:101) {
  
  p <- output.data[i,1]
  
  ## Parameters ----
  
  # System
  
  prop.prophylaxis <- p/100
  
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
  emergence       <- emergence.rate
  
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
  
  times <- seq(0,365,1)
  
  
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
  
  

  output.data[i,2] <- out[max(times),"CEs"] + out[max(times),"CEs"] + out[max(times),"CIs"] + out[max(times),"CTs"] + out[max(times),"PIs"] + out[max(times),"PTs"] 
  output.data[i,3] <- out[max(times),"CEr"] + out[max(times),"CEr"] + out[max(times),"CIr"] + out[max(times),"CTr"] + out[max(times),"PIr"] + out[max(times),"PTr"] 
  output.data[i,4] <- output.data[i,2] + output.data[i,3]
  output.data[i,5] <- output.data[i,2] / sum(out[max(times),2:17])
  output.data[i,6] <- output.data[i,3] / sum(out[max(times),2:17]) 
  output.data[i,7] <- output.data[i,4] / sum(out[max(times),2:17]) 
  
}


par(mfrow=c(2,2))

plot(output.data[,3] ~ output.data[,2], pch = 15, #xlim = c(0,60), ylim = c(0,60),
     xlab = "Final Number of Infections (AMR-)", ylab = "Final Number of Infections (AMR+)", type = 'b')

plot(output.data[,3] ~ output.data[,4], pch = 15, xlim = c(0,110), ylim = c(0,60),
     xlab = "Final Number of Infections (Total)", ylab = "Final Number of Infections (AMR+)", type= 'b')

output.df <- as.data.frame(output.data)
min.inf <- min(output.data[,4])
optimum1 <- output.df[which (output.df$V4 == min.inf),]
optimum.prop <- as.numeric(optimum1[1,1])


xy <- xy.coords(20,60)
points(optimum1[,3] ~ optimum1[,4], col = 'red', pch = 19, cex = 2)
legend(y = 60, x = 60, paste("Optimum \nProphylaxis =", optimum.prop,"%\n\n\nEmergence \nRate =",emergence.rate,"\n\nInfections\nPrevented =", 
                             signif(output.data[1,4] - output.data[101,4])), bty = 'n')


#plot(output.data[,2] ~ output.data[,1], pch = 15, xlim = c(0,100), ylim = c(0,60),
#     ylab = "Final Number of Infections (AMR-)", xlab = "Percentage Prophylaxis (%)", type = 'b')
#
#plot(output.data[,3] ~ output.data[,1], pch = 15, xlim = c(0,100), ylim = c(0,60),
#     ylab = "Final Number of Infections (AMR+)", xlab = "Percentage Prophylaxis (%)", type = 'b')
#
output.data[1,4] - output.data[101,4]
output.data[1,4]  
output.data[101,4]


plot(output.data[,5] ~ output.data[,1], pch = 15, xlim = c(0,100), ylim = c(0,1),
     ylab = "Final Prevalence of Infections", xlab = "Percentage Prophylaxis (%)", type = 'b', col = 'darkgoldenrod3')
points(output.data[,6] ~ output.data[,1], pch = 15, col = 'dodgerblue4')
points(output.data[,7] ~ output.data[,1], pch = 15, col = 'gray')
legend('topright', c("AMR- Prevalence", "AMR+ Prevalence", "Total Prevalence"), col =c("darkgoldenrod3", "dodgerblue4", "gray"), pch = 15, bty = 'n')

plot(output.data[,2] ~ output.data[,1], pch = 15, xlim = c(0,100), ylim = c(0,55),
     ylab = "Final Number of Infections", xlab = "Percentage Prophylaxis (%)",type = 'b', col ='darkgoldenrod3')
points(output.data[,3] ~ output.data[,1], pch = 15, col = 'dodgerblue4')
points(output.data[,4] ~ output.data[,1], pch = 15, col = 'gray')
legend('topright', c("AMR- Number", "AMR+ Number", "Total Number"), col =c("darkgoldenrod3", "dodgerblue4", "gray"), pch = 15, bty = 'n')
