## This file allows exploration of the model file AAT_AMR_main_dens_dep.R
## It is intended to test the model and explore various parameters. This is NOT
## intended to run completed scenarios AS OF THIS TIME.

## Authors: Shaun Keegan (shaun.keegan@glasgow.ac.uk)
##          Louise Matthews (louise.mattthews@glasgow.ac.uk)
## Year:    2022

## Working Directory ----

setwd("/Users/shaunkeegan/Documents/OneDrive - University of Glasgow/research/AAT_AMR_main/")

## Packages ----

library(deSolve)
library(tictoc)
library(progress)
library(ggplot2)
library(dplyr)
library(gghighlight)
library(cowplot)
library(crayon) #can print to terminal in colour

## Model & Functions  ----

source("model/AAT_AMR_dens_dep.R")
source("functions/r0_functions.R")
source("functions/plot_functions.R")
source("functions/funcs.R")

loops <- FALSE


if (loops == TRUE) {
  N_wl <- c(0, 100, 250)
  Trt_popA <- seq(0, 0.9, by = 0.2)      #full from 0-1
  Trt_popB <- seq(0.9, 0.99, by = 0.01)
  Trt_pop <- c(Trt_popA, Trt_popB)
  K.vec <- c(10000, 6000, 2000)
  fit.adj_vec <- c(1.0)
  birth.adj.vec <- c(2)
  prop.insecticide.vec <- c(0.0, 0.05, 0.1, 0.15)
  prop.prophylaxis <- 0
} else {
  N_wl <- 200
  Trt_pop <- c(0.9)
  #equil_vector_pop <- c(3000)
  fit.adj_vec <- c(1)
  K.vec <- 10000
  prop.insecticide.vec <- 0
  birth.adj.vec <- 2
  prop.prophylaxis <- 0
}


df <-data.frame(CS = c(), total.cattle = c(), treat_prop = c(), W_st = c(), R_eq_sen = c(), 
                R0_sen = c(), R_eq_res = c(), R0_res = c(), No_trt_cat = c(), 
                Incidence = c(), Vector_no = c(), Prob_onward_tran = c(), 
                Risk = c(), prevalence = c(), vector_mortality = c(), fit.adj = c(), 
                prop.insecticide =c(), birth.adj =c(), eq_pop = c(), K = c())

df2 <- data.frame()

## ---- Run time estimates
tic()
start.time <- Sys.time()
total_steps <- length(birth.adj.vec) * length(prop.insecticide.vec) * length(fit.adj_vec) * length(K.vec) * length(N_wl) * length(Trt_pop)
step_started <- 0
progress_floor <- 0

## ---- Execute model

for (birth.adj in birth.adj.vec) {
  for (prop.insecticide in prop.insecticide.vec) {
    for (fit.adj in fit.adj_vec) {
      for (K in K.vec) {
        for (NW in N_wl) {
          for (prop_treat in Trt_pop) {
            
            step_started <- step_started + 1
            progress_old <- progress_floor
            progress <- step_started/total_steps*100
            progress_floor <- floor(progress)
            if (progress_floor == progress_old + 1){
              current.time <- Sys.time()
              time_taken_minutes <- as.numeric(difftime(current.time, start.time, units = "mins"))
              time_remaining <- 100*time_taken_minutes/progress - time_taken_minutes
              cat(red("% Complete = ", round(progress,2), "Time elapsed =", round(time_taken_minutes,2), 
                      "mins", "Time left =", round(time_remaining,2), "\n"))}
            
            params <- set1(output = "P", birth.adj, fit.adj, K, prop_treat, prop.insecticide, NW, prop.prophylaxis)
            inits <- set1(output = "I", birth.adj, fit.adj, K, prop_treat, prop.insecticide, NW, prop.prophylaxis)

            R0sen <- r0sen(params)  #R0 function shouldn't need "inits"
            R0s <- R0sen["R0s"]
            
            R0res <- r0res(params)
            R0r <- R0res["R0r"]
            
            ## Times ----
            times <- seq(0, 3000, 1)

            ## RUN MODEL ----
            out <-ode(y = inits, parms = params, func = AAT_AMR_dens_dep, times = times, rootfunc = my_rootfun2,
                      events = list(root = TRUE, terminalroot = c(1,2)))
            out <- as.data.frame(out)
            names(out)[names(out) == 'time'] <- "times"

            #if (loops == FALSE) { plot(CEs ~ times, data = out)}
            
            last <- tail(out, 1)
            last$total.cattle <- last$CS + last$CEs + last$CEr + last$CIs + last$CIr + last$CTs + last$CTr + last$CR
            
            last$treat_prop <- params["treatment"] / (params["treatment"] + params["recovery"] + params["death"])
            
            #Check get 1 at equilibrium when run with only sensitive strains
            #fraction of cattle available for infection by sensitive strain
            fC <- last$CS / (as.numeric(inits["CS"]+inits["CIs"]))
            #fraction of vectors available for infection by sensitive strain
            if (as.numeric(inits["VS"]) > 0){fV <- last$VS / as.numeric(inits["VS"])} else {fV <- 0}
            
            #fraction of wildlife available for infection by sensitive strain
            if (as.numeric(inits["WS"]) > 0) {fW = last$WS / as.numeric(inits["WS"])} else {fW = 0}
            
            Rsen <- fC * R0sen[1] * fV * R0sen[3] + fW * R0sen[2] * fV * R0sen[4]  #Hurrah
            Rres <- fC * R0res[1] * fV * R0res[3] + fW * R0res[2] * fV * R0res[4]
            
            selected_outputs <- data.frame(W_st = out[1, "WS"], R_eq_sen = Rsen, R0_sen = R0s, 
                                           R_eq_res = Rres, R0_res = R0r, 
                                           No_trt_cat = params["treatment"] * last$CIs * 365.25, 
                                           Incidence = params["infectiousness"] * last$CEs * 365.25, 
                                           Vector_no = as.numeric(inits["VS"]), Prob_onward_tran = 1 - dpois(0, Rres), 
                                           Risk = (1 - dpois(0, Rres)) * params["treatment"] * last$CIs * 365.25 , 
                                           prevalence = last$CIs / (as.numeric(inits["CS"]+inits["CIs"])), vector_birth = params["birth.v"], 
                                           vector_mortality = params["death.v"], fit.adj = params["fit.adj"], 
                                           prop.insecticide = prop.insecticide, birth.adj = birth.adj,
                                           eq_pop = params["equil_vector_pop"], K = params["K"])
            df = rbind(df, selected_outputs)
            
            wide <- cbind(selected_outputs, last)
            df2 = rbind(df2, wide)
            
            
            print(last$times)
            
          }
        }
      }
    }
  }
}  

toc()

tail(df)
tail(round(df2, 2))




par(mfrow = c(2, 1))
plot(out$CS ~ out$times, type = 'l', ylim = c(0, max(out[, 2]) + 5), 
     lwd = 3, col = 'blue', main = "Cattle (no Prophylaxis)", 
     xlab = "Time", ylab = "Number")
lines(out$CEs ~ out$times, lwd = 3, col = 'orange') # Exposed
lines(out$CEr ~ out$times, lwd = 3, col = 'darkorange') # Exposed
lines(out$CIs ~ out$times, lwd = 3, col = 'red') # Infected
lines(out$CIr ~ out$times, lwd = 3, col = 'darkred') # Infected
lines(out$CTs ~ out$times, lwd = 3, col = 'green') # Treated
lines(out$CTr ~ out$times, lwd = 3, col = 'darkgreen') # Treated
lines(out$CR ~ out$times, lwd = 3, col = 'grey') # Recovered
lines(( out$CEs + out$CEr + out$CIs + out$CIr + out$CTs + out$CTr + out$CR + out$CS) ~out$times,lty = 2)

 plot( out$PS ~ out$times, type = 'l', ylim = c(0, max(out[, 10]) + 5),col = 'blue',
   lwd = 3,main = "Cattle (with Prophylaxis)",xlab = "Time", ylab = "Number")
 lines(out$PEs ~ out$times, lwd = 3, col = 'orange') # Exposed
 lines(out$PEr ~ out$times, lwd = 3, col = 'darkorange') # Exposed
 lines(out$PIs ~ out$times, lwd = 3, col = 'red') # Infected
 lines(out$PIr ~ out$times, lwd = 3, col = 'darkred') # Infected
 lines(out$PTs ~ out$times, lwd = 3, col = 'green') # Treated
 lines(out$PTr ~ out$times, lwd = 3, col = 'darkgreen') # Treated
 lines(out$PR  ~ out$times, lwd = 3, col = 'grey') # Recovered
 lines((out$PEs + out$PEr + out$PIs + out$PIr + out$PTs + out$PTr + out$PR + out$PS) ~out$times, lty = 2)

plot(out$WS ~ out$times,type = 'l', ylim = c(0, max(out[, 18]) + 5),col = 'blue',lwd = 3,
     main = "Wildlife", xlab = "Time", ylab = "Number")
lines(out$WEs ~ out$times, lwd = 3, col = 'orange') # Exposed
lines(out$WEr ~ out$times, lwd = 3, col = 'darkorange') # Exposed
lines(out$WIs ~ out$times, lwd = 3, col = 'red') # Infected
lines(out$WIr ~ out$times, lwd = 3, col = 'darkred') # Infected
lines(out$WR ~ out$times, lwd = 3, col = 'grey') # Recovered
lines((out$WEs + out$WEr + out$WIs + out$WIr + out$WR + out$WS) ~ out$times,lty = 2)

plot(out$VS ~ out$times, type = 'l', ylim = c(0, max(out[, 24]) + 100), col = 'blue',
  lwd = 3, main = "Vector", xlab = "Time", ylab = "Number")
lines(out$VEs ~ out$times, lwd = 3, col = 'orange') # Exposed
lines(out$VEr ~ out$times, lwd = 3, col = 'darkorange') # Exposed
lines(out$VIs ~ out$times, lwd = 3, col = 'red') # Infected
lines(out$VIr ~ out$times, lwd = 3, col = 'darkred') # Infected
lines((out$VEs + out$VEr + out$VIs + out$VIr + out$VS) ~
        out$times, lty = 2)



param_summary <- df2 %>% select(W_st, vector_birth, vector_mortality, fit.adj, prop.insecticide, K) %>% distinct()
param_summary <- lapply(param_summary, unique)
param_summary

test <- df2
time <- format(Sys.time(), "%a %b %d %X %Y")
save(test, file = paste0("test_", time, ".Rda"))
save(test,file ="test.Rda")
