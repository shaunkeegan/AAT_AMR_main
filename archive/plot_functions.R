## plot_functions


plot_R0_Sen <- function(df2, trtprops, fitadj, vecbirth){
  
  #Ro plots: default matches Hargrove; R0 declines with wildlife 
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  
  my_par <- list(colour = "grey92")
  
  p <- ggplot(df2) + my_theme() +
    geom_point(aes(x = W_st, y = R0_sen, colour = as.factor(Vector_no)), size = 3) + 
    geom_line(aes(x = W_st, y = R0_sen, colour = as.factor(Vector_no))) +
    xlab("Wildlife") +
    guides(col = guide_legend("Vector_no"))
  p
  p1 <- p + gghighlight(treat_prop == trtprops[1], fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par)
  p2 <- p + gghighlight(treat_prop > trtprops[2]-0.0001, treat_prop < trtprops[2] + 0.0001, fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par)
  p3 <- p + gghighlight(treat_prop > trtprops[3]-0.0001, treat_prop < trtprops[3] +0.0001, fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p1 + theme(legend.position="none") + geom_line(aes(x = W_st, y = R0_sen, colour = as.factor(Vector_no),shape = as.factor(W_st))), 
            p2 + theme(legend.position="none") + geom_line(aes(x = W_st, y = R0_sen, colour = as.factor(Vector_no),shape = as.factor(W_st))), 
            p3 + theme(legend.position="none") + geom_line(aes(x = W_st, y = R0_sen, colour = as.factor(Vector_no),shape = as.factor(W_st))), legend, ncol = 4)
  
  
}

plot_R0_Sen_wl <- function(df2, Wn, fitadj, vecbirth){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  
  #R0 plots: R0 declines with treatment and with wildlife, increases with vectors
  my_par <- list(colour = "grey92")
  
  pA <- df2 %>% filter(W_st %in% Wn) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = R0_sen, colour = as.factor(Vector_no), shape = as.factor(W_st)), size = 3) + 
    #geom_line(aes(x = treat_prop, y = R0_sen, colour = as.factor(Vector_no),shape = as.factor(W_st))) +
    xlab("Treatment") + ylab("R0 sensitive strain") +
    guides(col = guide_legend("Vectors"), shape = guide_legend("Wildlife"))
  pA 
  pA + gghighlight(Vector_no == 3000, keep_scales = TRUE)
  
  p <- pA
  p1 <- p + gghighlight(Vector_no == 1000,  fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(Vector_no == 3000,  fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(Vector_no == 5000,  fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = R0_sen, colour = as.factor(Vector_no),shape = as.factor(W_st))), 
            p2 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = R0_sen, colour = as.factor(Vector_no),shape = as.factor(W_st))), 
            p1 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = R0_sen, colour = as.factor(Vector_no),shape = as.factor(W_st))), legend, ncol = 4)
  
  
}


plot_res_v_sen <- function(df2, Wn, fitadj, vecbirth){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  
  #R0 plots: Resistant strain does better than sensitive apart from very low treatment rates
  my_par <- list(colour = "grey92")
  
  p1 <- df2 %>% filter(W_st %in% Wn) %>% filter(fit.adj %in% fitadj) %>% filter(vector_birth %in% vecbirth) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = R_eq_res/R_eq_sen, colour = as.factor(W_st), shape = as.factor(Vector_no)), size = 2) + 
    geom_line(aes(x = treat_prop, y = R_eq_res/R_eq_sen, colour = as.factor(W_st), shape = as.factor(Vector_no))) + 
    xlab("Treatment") + ylab("R resistant / R sensitive") + ylim(0,10) +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("Vectors"))
  p1
  
  legend <- get_legend(p1 + theme(legend.box.margin = margin(0, 0, 0, 12)))
  
  p2 <- df2 %>% filter(W_st %in% Wn) %>% filter(fit.adj %in% fitadj) %>% filter(vector_birth %in% vecbirth) %>%ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = R_eq_res/R_eq_sen, colour = as.factor(W_st), shape = as.factor(Vector_no)), size = 2) + 
    geom_line(aes(x = treat_prop, y = R_eq_res/R_eq_sen, colour = as.factor(W_st), shape = as.factor(Vector_no))) + 
    geom_abline(intercept = 1, slope = 0, linetype = 2) + 
    xlab("Treatment") + ylab("R resistant / R sensitive") + ylim(0,2) +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("Vectors"))
  p2 #+ gghighlight(R_eq_res/R_eq_sen < 1)
  plot_grid(p1 + theme(legend.position="none"), p2 + theme(legend.position="none"), legend, ncol = 3)
}


plot_prev_v_trt <- function(df2, Wn, fitadj, vecbirth){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  my_par <- list(colour = "grey92")
  p <- df2 %>% filter(W_st %in% Wn) %>%  filter(fit.adj %in% fitadj) %>% filter(vector_birth %in% vecbirth) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = prevalence, colour = as.factor(W_st), shape = as.factor(Vector_no)), size = 3) + 
    #geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))) +
    xlab("Prop treated") +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("Vectors"))
  p
  
  p1 <- p + gghighlight(W_st == Wn[1], unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(W_st == Wn[2], unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(W_st == Wn[3], unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = prevalence, colour = as.factor(W_st), shape = as.factor(Vector_no))), 
            p2 + theme(legend.position="none")+ geom_line(aes(x = treat_prop, y = prevalence, colour = as.factor(W_st), shape = as.factor(Vector_no))), 
            p1 + theme(legend.position="none")+ geom_line(aes(x = treat_prop, y = prevalence, colour = as.factor(W_st), shape = as.factor(Vector_no))), legend, ncol = 4)
  
}


plot_inc_v_trt <- function(df2, Wn, fitadj, vecbirth){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  my_par <- list(colour = "grey92")
  #Incidence increases as treatment increases and declines in some cases
  #this is because by treating animals returns them more quickly to the 
  #susceptible class
  p <- df2 %>% filter(W_st %in% Wn) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no)), size = 3) + 
    #geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))) +
    xlab("Prop treated") +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("Vectors"))
  p
  p1 <- p + gghighlight(W_st == Wn[1], fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(W_st == Wn[2], fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(W_st == Wn[3], fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))), 
            p2 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))), 
            p1 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))), legend, ncol = 4)
  
}


plot_trtcat_v_trt <- function(df2, Wn, fitadj, vecbirth){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  my_par <- list(colour = "grey92")
  #No cattle treated increases as treatment increases and declines in some cases
  p <- df2 %>% filter(W_st %in% Wn) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = No_trt_cat, colour = as.factor(W_st), shape = as.factor(Vector_no)), size = 3) + 
    #geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))) +
    xlab("Prop treated") +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("Vectors"))
  p
  p1 <- p + gghighlight(W_st == Wn[1], fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(W_st == Wn[2], fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(W_st == Wn[3], fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = No_trt_cat, colour = as.factor(W_st), shape = as.factor(Vector_no))), 
            p2 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = No_trt_cat, colour = as.factor(W_st), shape = as.factor(Vector_no))), 
            p1 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = No_trt_cat, colour = as.factor(W_st), shape = as.factor(Vector_no))), legend, ncol = 4)
  
  
}


plot_onward_v_trt <- function(df2, Wn, fitadj, vecbirth){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  
  ################################################################################
  my_par <- list(colour = "grey92")
  
  #Likelihood of strain spreading of it emerges
  p <- df2 %>% filter(W_st %in% Wn) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = Prob_onward_tran, colour = as.factor(W_st), shape = as.factor(Vector_no)), size = 3) + 
    ylim(0.5,1) +
    #geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))) +
    xlab("Prop treated") +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("Vectors"))
  p
  p1 <- p + gghighlight(W_st == Wn[1], fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(W_st == Wn[2], fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(W_st == Wn[3], fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Prob_onward_tran, colour = as.factor(W_st), shape = as.factor(Vector_no))), 
            p2 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Prob_onward_tran, colour = as.factor(W_st), shape = as.factor(Vector_no))), 
            p1 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Prob_onward_tran, colour = as.factor(W_st), shape = as.factor(Vector_no))), legend, ncol = 4)
  
  
}

plot_risk_v_trt <- function(df2, Wn, fitadj, vecbirth){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  my_par <- list(colour = "grey92")
  #Overall risk
  p <- df2 %>% filter(W_st %in% Wn) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = Risk, colour = as.factor(W_st), shape = as.factor(Vector_no)), size = 3) + 
    #geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))) +
    xlab("Prop treated") +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("Vectors"))
  p
  p1 <- p + gghighlight(W_st == Wn[1], fit.adj == fitadj, vector_birth == vecbirth,  unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(W_st == Wn[2], fit.adj == fitadj, vector_birth == vecbirth,  unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(W_st == Wn[3], fit.adj == fitadj, vector_birth == vecbirth, unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Risk, colour = as.factor(W_st), shape = as.factor(Vector_no))), 
            p2 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Risk, colour = as.factor(W_st), shape = as.factor(Vector_no))), 
            p1 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Risk, colour = as.factor(W_st), shape = as.factor(Vector_no))), legend, ncol = 4)
  
}








#####--------- DENS DEP PLOTS



plot_R0_Sen_dd <- function(df2, trtprops, fitadj){
  
  #Ro plots: default matches Hargrove; R0 declines with wildlife 
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  
  my_par <- list(colour = "grey92")
  
  p <- ggplot(df2) + my_theme() +
    geom_point(aes(x = W_st, y = R0_sen, colour = as.factor(eq_pop)), size = 3) + 
    geom_line(aes(x = W_st, y = R0_sen, colour = as.factor(eq_pop))) +
    xlab("Wildlife") +
    guides(col = guide_legend("eq_pop"))
  p
  p1 <- p + gghighlight(treat_prop == trtprops[1], fit.adj == fitadj,  unhighlighted_params = my_par)
  p2 <- p + gghighlight(treat_prop > trtprops[2]-0.0001, treat_prop < trtprops[2] + 0.0001, fit.adj == fitadj,   unhighlighted_params = my_par)
  p3 <- p + gghighlight(treat_prop > trtprops[3]-0.0001, treat_prop < trtprops[3] +0.0001, fit.adj == fitadj,   unhighlighted_params = my_par)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p1 + theme(legend.position="none") + geom_line(aes(x = W_st, y = R0_sen, colour = as.factor(eq_pop),shape = as.factor(W_st))), 
            p2 + theme(legend.position="none") + geom_line(aes(x = W_st, y = R0_sen, colour = as.factor(eq_pop),shape = as.factor(W_st))), 
            p3 + theme(legend.position="none") + geom_line(aes(x = W_st, y = R0_sen, colour = as.factor(eq_pop),shape = as.factor(W_st))), legend, ncol = 4)
  
  
}

plot_R0_Sen_wl_dd <- function(df2, Wn, fitadj){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  
  #R0 plots: R0 declines with treatment and with wildlife, increases with vectors
  my_par <- list(colour = "grey92")
  
  pA <- df2 %>% filter(W_st %in% Wn) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = R0_sen, colour = as.factor(eq_pop), shape = as.factor(W_st)), size = 3) + 
    #geom_line(aes(x = treat_prop, y = R0_sen, colour = as.factor(Vector_no),shape = as.factor(W_st))) +
    xlab("Treatment") + ylab("R0 sensitive strain") +
    guides(col = guide_legend("Vectors"), shape = guide_legend("Wildlife"))
  pA 
  pA + gghighlight(eq_pop == 3000, keep_scales = TRUE)
  
  p <- pA
  p1 <- p + gghighlight(eq_pop == 1000,  fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(eq_pop == 3000,  fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(eq_pop == 5000,  fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = R0_sen, colour = as.factor(eq_pop),shape = as.factor(W_st))), 
            p2 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = R0_sen, colour = as.factor(eq_pop),shape = as.factor(W_st))), 
            p1 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = R0_sen, colour = as.factor(eq_pop),shape = as.factor(W_st))), legend, ncol = 4)
  
  
}


plot_res_v_sen_dd <- function(df2, Wn, fitadj){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  
  #R0 plots: Resistant strain does better than sensitive apart from very low treatment rates
  my_par <- list(colour = "grey92")
  
  p1 <- df2 %>% filter(W_st %in% Wn) %>% filter(fit.adj %in% fitadj)  %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = R_eq_res/R_eq_sen, colour = as.factor(W_st), shape = as.factor(eq_pop)), size = 2) + 
    geom_line(aes(x = treat_prop, y = R_eq_res/R_eq_sen, colour = as.factor(W_st), shape = as.factor(eq_pop))) + 
    xlab("Treatment") + ylab("R resistant / R sensitive") + ylim(0,10) +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("Vectors"))
  p1
  
  legend <- get_legend(p1 + theme(legend.box.margin = margin(0, 0, 0, 12)))
  
  p2 <- df2 %>% filter(W_st %in% Wn) %>% filter(fit.adj %in% fitadj)  %>%ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = R_eq_res/R_eq_sen, colour = as.factor(W_st), shape = as.factor(eq_pop)), size = 2) + 
    geom_line(aes(x = treat_prop, y = R_eq_res/R_eq_sen, colour = as.factor(W_st), shape = as.factor(eq_pop))) + 
    geom_abline(intercept = 1, slope = 0, linetype = 2) + 
    xlab("Treatment") + ylab("R resistant / R sensitive") + ylim(0,2) +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("Vectors"))
  p2 #+ gghighlight(R_eq_res/R_eq_sen < 1)
  plot_grid(p1 + theme(legend.position="none"), p2 + theme(legend.position="none"), legend, ncol = 3)
}


plot_prev_v_trt_dd <- function(df2, Wn, fitadj){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  my_par <- list(colour = "grey92")
  
  p <- df2 %>% filter(W_st %in% Wn) %>%  filter(fit.adj %in% fitadj)  %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = prevalence, colour = as.factor(W_st), shape = as.factor(eq_pop)), size = 3) + 
    #geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))) +
    xlab("Prop treated") +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("eq_pop"))
  p
  
  p1 <- p + gghighlight(W_st == Wn[1], unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(W_st == Wn[2], unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(W_st == Wn[3], unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = prevalence, colour = as.factor(W_st), shape = as.factor(eq_pop))), 
            p2 + theme(legend.position="none")+ geom_line(aes(x = treat_prop, y = prevalence, colour = as.factor(W_st), shape = as.factor(eq_pop))), 
            p1 + theme(legend.position="none")+ geom_line(aes(x = treat_prop, y = prevalence, colour = as.factor(W_st), shape = as.factor(eq_pop))), legend, ncol = 4)
  
}


plot_inc_v_trt_dd <- function(df2, Wn, fitadj){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  my_par <- list(colour = "grey92")
  
  #Incidence increases as treatment increases and declines in some cases
  #this is because by treating animals returns them more quickly to the 
  #susceptible class
  p <- df2 %>% filter(W_st %in% Wn) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(eq_pop)), size = 3) + 
    #geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))) +
    xlab("Prop treated") +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("eq_pop"))
  p
  p1 <- p + gghighlight(W_st == Wn[1], fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(W_st == Wn[2], fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(W_st == Wn[3], fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(eq_pop))), 
            p2 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(eq_pop))), 
            p1 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(eq_pop))), legend, ncol = 4)
  
}


plot_trtcat_v_trt_dd <- function(df2, Wn, fitadj){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  my_par <- list(colour = "grey92")
  #No cattle treated increases as treatment increases and declines in some cases
  p <- df2 %>% filter(W_st %in% Wn) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = No_trt_cat, colour = as.factor(W_st), shape = as.factor(eq_pop)), size = 3) + 
    #geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))) +
    xlab("Prop treated") +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("eq_pop"))
  p
  p1 <- p + gghighlight(W_st == Wn[1], fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(W_st == Wn[2], fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(W_st == Wn[3], fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = No_trt_cat, colour = as.factor(W_st), shape = as.factor(eq_pop))), 
            p2 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = No_trt_cat, colour = as.factor(W_st), shape = as.factor(eq_pop))), 
            p1 + theme(legend.position="none") + geom_line(aes(x = treat_prop, y = No_trt_cat, colour = as.factor(W_st), shape = as.factor(eq_pop))), legend, ncol = 4)
  
  
}


plot_onward_v_trt_dd <- function(df2, Wn, fitadj){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  my_par <- list(colour = "grey92")
  ################################################################################
  #Likelihood of strain spreading of it emerges
  p <- df2 %>% filter(W_st %in% Wn) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = Prob_onward_tran, colour = as.factor(W_st), shape = as.factor(eq_pop)), size = 3) + 
    ylim(0.5,1) +
    #geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))) +
    xlab("Prop treated") +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("eq_pop"))
  p
  p1 <- p + gghighlight(W_st == Wn[1], fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(W_st == Wn[2], fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(W_st == Wn[3], fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Prob_onward_tran, colour = as.factor(W_st), shape = as.factor(eq_pop))), 
            p2 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Prob_onward_tran, colour = as.factor(W_st), shape = as.factor(eq_pop))), 
            p1 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Prob_onward_tran, colour = as.factor(W_st), shape = as.factor(eq_pop))), legend, ncol = 4)
  
  
}

plot_risk_v_trt_dd <- function(df2, Wn, fitadj){
  ################################################################################
  my_theme <- function () { 
    theme_grey(base_size=16) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  }
  ################################################################################
  my_par <- list(colour = "grey92")
  #Overall risk
  p <- df2 %>% filter(W_st %in% Wn) %>% ggplot() + my_theme() +
    geom_point(aes(x = treat_prop, y = Risk, colour = as.factor(W_st), shape = as.factor(eq_pop)), size = 3) + 
    #geom_line(aes(x = treat_prop, y = Incidence, colour = as.factor(W_st), shape = as.factor(Vector_no))) +
    xlab("Prop treated") +
    guides(col = guide_legend("Wildlife"), shape = guide_legend("eq_pop"))
  p
  p1 <- p + gghighlight(W_st == Wn[1], fit.adj == fitadj,    unhighlighted_params = my_par, keep_scales = TRUE)
  p2 <- p + gghighlight(W_st == Wn[2], fit.adj == fitadj,    unhighlighted_params = my_par, keep_scales = TRUE)
  p3 <- p + gghighlight(W_st == Wn[3], fit.adj == fitadj,   unhighlighted_params = my_par, keep_scales = TRUE)
  
  legend <- get_legend(p + theme(legend.box.margin = margin(0, 0, 0, 12)))
  plot_grid(p3 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Risk, colour = as.factor(W_st), shape = as.factor(eq_pop))), 
            p2 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Risk, colour = as.factor(W_st), shape = as.factor(eq_pop))), 
            p1 + theme(legend.position="none")+geom_line(aes(x = treat_prop, y = Risk, colour = as.factor(W_st), shape = as.factor(eq_pop))), legend, ncol = 4)
  
}




