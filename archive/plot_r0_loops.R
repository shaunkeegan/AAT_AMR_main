library("ggplot2")
library('gghighlight')
library('cowplot')

## --------

## 


  
p1 <- ggplot(data = df[which (df$treat_prop == as.character(0.95)),])+
  geom_line(aes(x= W_st, y = R0_sen, col = as.factor(Vector_no)), size = 1.3)+
  geom_line(aes(x= W_st, y = R0_res, col = as.factor(Vector_no)), linetype = 2, size = 1.3)+
  theme_cowplot(12) 
p1
p1 <- p1 + labs(colour = "Vector \nPopulation",
                x = "Wildlife Number",
                y = "R0")

p1
p1 + gghighlight( R0_res > R0_sen)

unique(df$treat_prop)


