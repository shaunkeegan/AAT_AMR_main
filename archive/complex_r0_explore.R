

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
  recovery.st     <- recovery*250
  emergence       <- 0
  fit.adj         <- 0.95
  rec.adj         <- 1
  
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
  CIr <- 0    # Infected (drug resistant strain)
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
  R0sen <- r0sen(inits, parms)  #R0 function shouldn't need "inits"
  R0sen
  R0s <- R0sen[1,"R0s"]
  
  R0res <- r0res(inits, parms)
  R0res
  R0r <- R0res[1,"R0r"]
  
  
  SusR0[i] <- R0s
  ResR0[i] <- R0r
  
}

ResR0
SusR0


par(mfrow=c(1,2))
plot(ResR0 ~ Adj, type= 'l', col = 'red3', lwd = 3,
     ylab = "R0", xlab = "Wildlife Population (N)", 
     main = paste("Fitness Adjustment =", fit.adj), ylim = c(0,max(ResR0)))
lines(SusR0 ~ Adj, col = 'darkblue', lwd = 3, lty = 2)
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

