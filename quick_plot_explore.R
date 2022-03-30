library(ggplot2)
library(gghighlight)
library(cowplot)
library(dplyr)

source("functions/plot_functions.R")

defaultW <- getOption("warn") 
options(warn = -1) 

load("test_Wed Mar 30 16:59:40 2022.Rda")
test1 <- test %>% signif(4)


this_prop <- 0.05

ins1.dat <- test1 %>% filter(prop.insecticide == this_prop) 

plot_R0_Sen_dd(ins1.dat, trtprops = c(0,0.6,0.95), fitadj = 1) #Ro plots: default matches Hargrove; R0 declines with wildlife

plot_R0_Sen_wl_dd(ins1.dat, Wn = c(0, 100, 250), fitadj = 1)

plot_res_v_sen_dd(ins1.dat, Wn =c(0,100,250), fitadj = 1.0)

plot_prev_v_trt_dd(ins1.dat, Wn = c(250,100,0), fitadj = 1)

plot_inc_v_trt_dd(ins1.dat, Wn = c(250,100, 0), fitadj = 1)

plot_trtcat_v_trt_dd(ins1.dat, Wn = c(250,100,0), fitadj = 1)

plot_onward_v_trt_dd(ins1.dat, Wn = c(250,100, 0), fitadj = 1)

plot_risk_v_trt_dd(ins1.dat, Wn = c(250,100,0), fitadj =1)

plot.func <- plot_risk_v_trt_dd

this_test <- test1

ins.dat <- this_test[which(this_test$prop.insecticide == 0.0),]
a0 <- plot.func(ins.dat, Wn = c(0, 100, 250), fitadj = 1)

ins.dat <- this_test[which(this_test$prop.insecticide == 0.05),]
a05 <- plot.func(ins.dat, Wn = c(0, 100, 250), fitadj = 1)

ins.dat <- this_test[which(this_test$prop.insecticide == 0.1),]
a10 <- plot.func(ins.dat, Wn = c(0, 100, 250), fitadj = 1)

#legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
plot_grid(a0, a05, a10,nrow = 3)

options(warn = defaultW)
