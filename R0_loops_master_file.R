## This file allows exploration of the model file AAT_AMR_main.R
## It is intended to test the model and explore various parameters. This is NOT
## intended to run completed scenarios.

## Lead Author:   Shaun Keegan (shaun.keegan@glasgow.ac.uk)
## Other Authors: Louise Matthews (louise.mattthews@glasgow.ac.uk)

## Working Directory ----

setwd(
  "/Users/shaunkeegan/Documents/OneDrive - University of Glasgow/research/AAT_AMR_main/"
)

## Packages ----

library(deSolve)
library(tictoc)
library(progress)
library(ggplot2)
library(dplyr)
library(gghighlight)
library(cowplot)

## Model ----

source("model/AAT_AMR_main.R")
source("model/AAT_AMR_dens_dep.R")
source("functions/r0_functions.R")
source("functions/plot_functions.R")

loops <- TRUE


if (loops == TRUE) {
  N_wl <- seq(0, 250, by = 50)
  Trt_popA <- seq(0, 0.9, by = 0.2)      #full from 0-1
  Trt_popB <- seq(0.9, 0.99, by = 0.01)
  Trt_pop <- c(Trt_popA, Trt_popB)
  vec_pop <- c(5000, 3000, 1000)
  birth_vec <- seq(0.03, 0.15, by = 0.03) #second to be 0.15
  fit.adj_vec <- c(0.9, 0.95, 0.99, 1.0)
} else {
  N_wl <- 0
  Trt_pop <- c(0.95)
  vec_pop <- c(5000)
  fit.adj_vec <- c(0.9)
}


df <- data.frame(CS = c(), total.cattle = c(), treat_prop = c(), W_st = c(), 
                 R_eq_sen = c(), R0_sen = c(), R_eq_res = c(), R0_res = c(), 
                 No_trt_cat = c(), Incidence = c(), Vector_no = c(), 
                 Prob_onward_tran = c(), Risk = c(), prevalence = c(), 
                 vector_mortality = c(), fit.adj = c())

df2 <- data.frame()

tic()

for (fit.adj in fit.adj_vec) {
  print(paste0("loop1, fit.adj = ", fit.adj, "; Iteration ", which(fit.adj_vec == fit.adj), " of ", length(fit.adj_vec)))
  for (NV in vec_pop) {
    print(paste0("loop2, NV = ", NV, "; Iteration ", which(vec_pop == NV), " of ", length(vec_pop)))
    for (NW in N_wl) {
      print(paste0("loop3, NW = ", NW, "; Iteration ", which(NW == N_wl), " of ", length(N_wl)))
      for (birth.v in birth_vec) {
        print(paste0("loop4, birth.vec = ", birth.v, "; Iteration ", which(birth_vec == birth.v), " of ", length(birth_vec)))
        for (prop_treat in Trt_pop) {
          print(prop_treat)
          ## Parameters ----
          
          # System
          
          prop.prophylaxis <- 0
          # Cattle
          birth.c          <- 1 / 365
          biterate         <- 0.8 / 4
          prob.infection   <- 0.46
          infectiousness   <- 1 / 15
          resusceptible    <- 1 / 100
          death            <- birth.c
          recovery         <- 1 / 100
          treatment        <- prop_treat * (recovery + death) / (1 - prop_treat)
          recovery.st      <- recovery * 250 #LM: adjusted so that R0 drops below 1 when 99% treated to reflect Hargrove
          emergence        <- 0
          rec.adj          <- 1
          
          # Wildlife
          birth.w            <- 1 / 365
          #biterate.w         <- 0.7/4 #this isn't doing anything CHECK
          prob.infection.s.w <- 0.46
          prob.infection.r.w <- 0.46
          infectiousness.w   <- 1 / 20
          resusceptible.w    <- 1 / 100
          death.w            <- birth.w
          recovery.w         <- recovery
          reversion          <- 0
          
          # Vectors
          #birth.v          <-  0.03
          qf <- 0.96
          qn <- 0.98
          feed.cyc <- 4
          insecticide.prop <- 0
          
          
            death.v <- birth.v
           
         
          
          feeding.rate     <-  0
          prob.infection.v <-  0.025
          incubation       <-  20
          infectiousness.v <- death.v * exp(-death.v * incubation) / (1 - exp(-death.v * incubation))
          K <- 1000

          
          if (densdep == FALSE) {
            params <-cbind( birth.c, biterate, prob.infection, fit.adj, rec.adj, recovery.st, infectiousness, resusceptible, death, treatment, recovery, birth.v, death.v, feeding.rate, prob.infection.v, infectiousness.v, emergence, reversion)
          }
          
          
          
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
          WS  <- NW - WIs  # Susceptible
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
          
#          inits <- cbind(CS, CEs, CEr, CIs, CIr, CTs, CTr, CR, PS, PEs, PEr, 
                         # PIs, PIr, PTs, PTr, PR, WS, WEs, WEr, WIs, WIr, WR, 
                         # VS, VEs, VEr, VIs, VIr)
                         # 
          inits <- c("CS" = CS, "CEs" = CEs, "CEr" = CEr, "CIs" = CIs, 
                     "CIr" = CIr, "CTs" = CTs, "CTr"= CTr, "CR" = CR, 
                     "PS" = PS, "PEs" = PEs, "PEr" = PEr, "PIs" = PIs, 
                     "PIr" = PIr, "PTs" = PTs, "PTr" = PTr, "PR" = PR, "WS" = WS, 
                     "WEs" = WEs, "WEr" = WEr, "WIs" = WIs, "WIr" = WIr, 
                     "WR" = WR, "VS" = VS, "VEs" = VEs, "VEr" = VEr, "VIs" = VIs, 
                     "VIr" = VIr)
          
          
          R0sen <- r0sen(inits, parms)  #R0 function shouldn't need "inits"
          R0sen
          R0s <- R0sen[1, "R0s"]
          
          R0res <- r0res(inits, parms)
          R0res
          R0r <- R0res[1, "R0r"]
          
          ## Times ----
          times <- seq(0, 3000, 1)
          
          ## RUN MODEL ----
          
            out <- ode(y = inits, parms = params, func = AAT_AMR_main, times = times)

          # names <-c("times", "CS", "CEs", "CEr", "CIs", "CIr", "CTs", "CTr", "CR", 
          #     "PS", "PEs", "PEr", "PIs", "PIr", "PTs", "PTr", "PR", "WS", "WEs", 
          #     "WEr", "WIs", "WIr", "WR", "VS", "VEs", "VEr", "VIs", "VIr")
          # colnames(out) <- names
          # colnames(out)
          #out
          out <- as.data.frame(out)
          last <- tail(out, 1)
          
          if (loops == FALSE) {
            plot(CIs ~ times, data = out)
          }
          
          last$total.cattle <- last$CS + last$CEs + last$CEr + last$CIs + last$CIr + last$CTs + last$CTr + last$CR
          
          last$treat_prop <- treatment / (treatment + recovery + death)
          
          #Check get 1 at equilibrium when run with only sensitive strains
          #fraction of cattle available for infection by sensitive strain
          fC <- last$CS / NC
          #fraction of vectors available for infection by sensitive strain
          fV <- last$VS / NV
          #fraction of wildlife available for infection by sensitive strain
          if (NW > 0) {
            fW = last$WS / NW
          } else {
            fW = 0
          }
          
          Rsen <- fC * R0sen[1] * fV * R0sen[3] + fW * R0sen[2] * fV * R0sen[4]  #Hurrah
          Rres <- fC * R0res[1] * fV * R0res[3] + fW * R0res[2] * fV * R0res[4]
          
          selected_outputs <-
            data.frame( #CS = last$CS, total.cattle = last$total.cattle, treat_prop = last$treat_prop, 
              W_st = out[1, "WS"], R_eq_sen = Rsen, R0_sen = R0s, R_eq_res = Rres, 
              R0_res = R0r, No_trt_cat = treatment * last$CIs * 365.25, 
              Incidence = infectiousness * last$CEs * 365.25, Vector_no = NV, 
              Prob_onward_tran = 1 - dpois(0, Rres), 
              Risk = (1 - dpois(0, Rres)) * treatment * last$CIs * 365.25 , 
              prevalence = last$CIs / NC, vector_birth = birth.v, 
              vector_mortality = death.v, fit.adj = fit.adj)
          df = rbind(df, selected_outputs)
          
          wide <- cbind(selected_outputs, last)
          df2 = rbind(df2, wide)
          
        }
      }
    }
  }
}
toc()

tail(df)
tail(round(df2, 2))




par(mfrow = c(2, 2))
plot(out$CS ~ out$time, type = 'l', ylim = c(0, max(out[, 2]) + 5), lwd = 3, 
     col = 'blue', main = paste(round(simpleR0, 2)), xlab = "Time", ylab = "Number")
lines(out$CEs ~ out$time, lwd = 3, col = 'orange') # Exposed
lines(out$CEr ~ out$time, lwd = 3, col = 'darkorange') # Exposed
lines(out$CIs ~ out$time, lwd = 3, col = 'red') # Infected
lines(out$CIr ~ out$time, lwd = 3, col = 'darkred') # Infected
lines(out$CTs ~ out$time, lwd = 3, col = 'green') # Treated
lines(out$CTr ~ out$time, lwd = 3, col = 'darkgreen') # Treated
lines(out$CR ~ out$time, lwd = 3, col = 'grey') # Recovered
lines((out$CEs + out$CEr + out$CIs + out$CIr + out$CTs + out$CTr + out$CR + out$CS) ~ out$time, lty = 2)

plot(
  out$PS ~ out$time,
  type = 'l',
  ylim = c(0, max(out[, 10]) + 5),
  col = 'blue',
  lwd = 3,
  main = "Cattle (with Prophylaxis)",
  xlab = "Time",
  ylab = "Number"
)
lines(out$PEs ~ out$time, lwd = 3, col = 'orange') # Exposed
lines(out$PEr ~ out$time, lwd = 3, col = 'darkorange') # Exposed
lines(out$PIs ~ out$time, lwd = 3, col = 'red') # Infected
lines(out$PIr ~ out$time, lwd = 3, col = 'darkred') # Infected
lines(out$PTs ~ out$time, lwd = 3, col = 'green') # Treated
lines(out$PTr ~ out$time, lwd = 3, col = 'darkgreen') # Treated
lines(out$PR  ~ out$time, lwd = 3, col = 'grey') # Recovered
lines((
  out$PEs + out$PEr + out$PIs + out$PIr + out$PTs + out$PTr + out$PR + out$PS
) ~
  out$time,
lty = 2)

plot(
  out$WS ~ out$time,
  type = 'l',
  ylim = c(0, max(out[, 18]) + 5),
  col = 'blue',
  lwd = 3,
  main = "Wildlife",
  xlab = "Time",
  ylab = "Number"
)
lines(out$WEs ~ out$time, lwd = 3, col = 'orange') # Exposed
lines(out$WEr ~ out$time, lwd = 3, col = 'darkorange') # Exposed
lines(out$WIs ~ out$time, lwd = 3, col = 'red') # Infected
lines(out$WIr ~ out$time, lwd = 3, col = 'darkred') # Infected
lines(out$WR ~ out$time, lwd = 3, col = 'grey') # Recovered
lines((out$WEs + out$WEr + out$WIs + out$WIr + out$WR + out$WS) ~
        out$time,
      lty = 2)

plot(
  out$VS ~ out$time,
  type = 'l',
  ylim = c(0, max(out[, 24]) + 100),
  col = 'blue',
  lwd = 3,
  main = "Vector",
  xlab = "Time",
  ylab = "Number"
)
lines(out$VEs ~ out$time, lwd = 3, col = 'orange') # Exposed
lines(out$VEr ~ out$time, lwd = 3, col = 'darkorange') # Exposed
lines(out$VIs ~ out$time, lwd = 3, col = 'red') # Infected
lines(out$VIr ~ out$time, lwd = 3, col = 'darkred') # Infected
lines((out$VEs + out$VEr + out$VIs + out$VIr + out$VS) ~
        out$time, lty = 2)




#write.csv(df, "output_files/output_short_jan2022.csv")
#write.csv(df2, "output_files/output_jan2022.csv")

#output_jan2022 <- read.csv("output_files/output_jan2022.csv", header = T)
#save(output_jan2022, file = "output_files/output_jan2022.RDA")
#load("output_files/output_jan2022.RDA")

#unique(df2$vector_birth) # 0.03 0.06 0.09 0.12 0.15
#unique(df2$fit.adj) # 0.90 0.95 0.99 1.00
#unique(df2$treat_prop) # 0.00 0.20 0.40 0.60 0.80 0.90 0.91 0.92 0.93 0.94 0.95 0.96 0.97 0.98 0.99
#
#plot_R0_Sen(df2, trtprops = c(0,0.6,0.99), fitadj = 1, vecbirth =0.03) #Ro plots: default matches Hargrove; R0 declines with wildlife
#
#unique(df2$W_st) # 0  50 100 150 200 250
#plot_R0_Sen_wl(output_jan2022, Wn = c(0, 100, 250), fitadj = 1, vecbirth =0.03)
#
#plot_res_v_sen(output_jan2022, Wn =c(0,50,250), fitadj = 1, vecbirth = 0.03)
#
#plot_prev_v_trt(output_jan2022, Wn = c(250,100,0), fitadj = 1, vecbirth = 0.03)
#
#plot_inc_v_trt(output_jan2022, Wn = c(250,100, 0), fitadj = 1, vecbirth = 0.03)
#
#plot_trtcat_v_trt(output_jan2022, Wn = c(250,100,0), fitadj = 1, vecbirth = 0.03)
#
#plot_onward_v_trt(output_jan2022, Wn = c(250,100, 0), fitadj = 1, vecbirth = 0.03)
#
#plot_risk_v_trt(output_jan2022, Wn = c(250,100,0), fitadj =1, vecbirth = 0.03)
#
#
#
#