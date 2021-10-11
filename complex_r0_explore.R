

No <- 1000
Adj <- 0:No
#Adj <- 1 - Adj/100

SusR0 <- 1:(No+1)
SusR0[] <- NA
ResR0 <- 1:(No+1)
ResR0[] <- NA


for (i in 1:(No+1)){
  prop.prophylaxis <- 0
  # Cattle 
  birth.c          <- 1/365
  biterate         <- 0.7/4
  prob.infection <- 0.46         
  infectiousness   <- 1/15
  resusceptible    <- 1/100
  death            <- birth.c
  treatment        <- 1/30
  recovery       <- 1/100
  recovery.st     <- recovery*10
  emergence       <- 0
  fit.adj         <- .7
  rec.adj         <- .1
  
  # Wildlife 
  birth.w          <- 1/365
  biterate.w         <- 0.2/4 #think we should keep the same biterate
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
  
  params <- cbind(birth.c, biterate, prob.infection, fit.adj, rec.adj,recovery.st,
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
  NW <- Adj[i]
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
  
  N <- NC + NW
  
  RH1Vs <- (NC/(N))*(biterate*prob.infection/death.v) * (infectiousness/(infectiousness + death))
  RWVs <- (NW/N)*(biterate*prob.infection/death.v) * (infectiousness.w/(infectiousness.w + death.w))
  RVH1s <- prob.infection.v * biterate * (NV/N) * # rate of biting
    (1/(treatment + recovery + death)) * # length of time host infection in I
    (infectiousness.v/(infectiousness.v + death.v)) + # proportion of vectors that end up infected
    prob.infection.v * biterate * (NV/N) * #rate of biting
    (1/(recovery.st + death)) *  # length of time host infected in T
    (infectiousness.v/(infectiousness.v + death.v)) * # proportion of vectors that end up infected
    (treatment/((treatment + recovery + death))) # proportion of hosts proceeding from I to T
  
  RVWs  <- prob.infection.v * biterate * (NV/N) * (1/(recovery.w + death.w)) * (infectiousness.v/(infectiousness.v + death.v))
  
  R0s <- sqrt(RH1Vs * RVH1s + RWVs * RVWs)
  
  
  
  
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
  
  SusR0[i] <- R0s
  ResR0[i] <- R0r
  
}

ResR0
SusR0


par(mfrow=c(2,1))
plot(ResR0^2 ~ Adj, type= 'l', col = 'red3', lwd = 3,
     ylab = "R0", xlab = "Wildlife Population (N)", 
     main = paste("Fitness Adjustment =", fit.adj,"\n Recovery Adjustment =", rec.adj ), ylim = c(0,max(SusR0)^2))
lines(SusR0^2 ~ Adj, col = 'darkblue', lwd = 3, lty = 2)
abline(h = 1, lty = 2)
legend("topright", c("Resistant", "Sensitive"), col = c('red3', 'darkblue'),pch = 15, bty = 'n')

Diff <- cbind(ResR0-SusR0, ResR0/SusR0,ResR0/SusR0-1, Adj)

plot(Diff[,1] ~ Adj, type = 'l', ylab = "R0 Resistant - R0 Sensitive ", xlab="Number of Wildlife (N)")

Diff.neg <- Diff[which(Diff[,1] <0),]
lines(Diff.neg[,1] ~ Diff.neg[,4], col = 'blue', lwd =3)
Diff.pos <- Diff[which(Diff[,1] >0),]
lines(Diff.pos[,1] ~ Diff.pos[,4], col = 'red', lwd =3)
abline(h=0, lty = 2)
legend("topright", c("Resistant", "Sensitive"), col = c('red3', 'darkblue'),pch = 15, bty = 'n')

abline(v=min(Diff.neg[,4]), lty= 2)
#points(min(Diff.pos[,1])~min(Diff.pos[,2]), col = "green", pch = 16, cex = 2)
points(0~min(Diff.neg[,4]), col = "green", pch = 16, cex = 2)



SusR0[1]^2
ResR0[1]^2

