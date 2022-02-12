plot_R0_Sen(output_jan2022, trtprops = c(0,0.6,0.95), fitadj = 1, vecbirth = 0.03) #Ro plots: default matches Hargrove; R0 declines with wildlife

ins.dat <- df2[which(df2$prop.insecticide == 0.3),]
plot_R0_Sen_dd(ins.dat, trtprops = c(0,0.6,0.95), fitadj = 1) #Ro plots: default matches Hargrove; R0 declines with wildlife


plot_R0_Sen_wl(output_jan2022, Wn = c(0, 100, 250), fitadj = 1, vecbirth = 0.03)
plot_R0_Sen_wl_dd(output_feb2022, Wn = c(0, 100, 250), fitadj = 1)

plot_res_v_sen(output_jan2022, Wn =c(0,50,250), fitadj = 1, vecbirth = 0.03)
plot_res_v_sen_dd(output_feb2022, Wn =c(0,50,250), fitadj = 1)


plot_prev_v_trt(output_jan2022, Wn = c(250,100,0), fitadj = 1, vecbirth = 0.03)
plot_prev_v_trt_dd(output_feb2022, Wn = c(250,100,0), fitadj = 1)

plot_inc_v_trt(output_jan2022, Wn = c(250,100, 0), fitadj = 1, vecbirth = 0.03)
plot_inc_v_trt_dd(output_feb2022, Wn = c(250,100, 0), fitadj = 1)

plot_trtcat_v_trt(output_jan2022, Wn = c(250,100,0), fitadj = 1, vecbirth = 0.03)
plot_trtcat_v_trt_dd(output_feb2022, Wn = c(250,100,0), fitadj = 1)

plot_onward_v_trt(output_jan2022, Wn = c(250,100, 0), fitadj = 1, vecbirth = 0.03)
plot_onward_v_trt_dd(output_feb2022, Wn = c(250,100, 0), fitadj = 1)

plot_risk_v_trt(output_jan2022, Wn = c(250,100,0), fitadj =1, vecbirth = 0.03)
plot_risk_v_trt_dd(output_feb2022, Wn = c(250,100,0), fitadj =1)



###




ins.dat <- df2[which(df2$prop.insecticide == 0.05),]
plot_R0_Sen_dd(ins.dat, trtprops = c(0,0.6,0.95), fitadj = 1) #Ro plots: default matches Hargrove; R0 declines with wildlife
plot_R0_Sen_wl_dd(ins.dat, Wn = c(0, 100, 250), fitadj = 1)
plot_res_v_sen_dd(ins.dat, Wn =c(0,50,250), fitadj = 1)
plot_prev_v_trt_dd(ins.dat, Wn = c(250,100,0), fitadj = 1)
plot_inc_v_trt_dd(ins.dat, Wn = c(250,100, 0), fitadj = 1)
plot_trtcat_v_trt_dd(ins.dat, Wn = c(250,100,0), fitadj = 1)
plot_onward_v_trt_dd(ins.dat, Wn = c(250,100, 0), fitadj = 1)
plot_risk_v_trt_dd(ins.dat, Wn = c(250,100,0), fitadj =1)


plot.func <- plot_risk_v_trt_dd

ins.dat <- df2[which(df2$prop.insecticide == 0.05),]
a05 <- plot.func(ins.dat, Wn = c(0, 100, 250), fitadj = 1)
ins.dat <- df2[which(df2$prop.insecticide == 0.10),]
a10 <- plot.func(ins.dat, Wn = c(0, 100, 250), fitadj = 1)
ins.dat <- df2[which(df2$prop.insecticide == 0.3),]
a30 <- plot.func(ins.dat, Wn = c(0, 100, 250), fitadj = 1)

#legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
plot_grid(a05,a10,a30,nrow = 3)


df2$eq_pop
