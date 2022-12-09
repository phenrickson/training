# 
library(tidyverse)
library(splines)

set.seed(1999)
n = 250
x=rnorm(n,0,1)
u=rnorm(n,0,1.5)
form = -1+(-0.5*x^3)+(-2*x^2)+x
y=form+u
ytruth = form

dat = tibble(x, y)
        
dat %>% ggplot(aes(x=x,
                   y=y))+
        geom_point()+
        theme_bw() +
        geom_line(data = tibble(x, ytruth),
                  aes(x=x,
                      y=ytruth),
                  color = 'orange')

library(patchwork)
library(splines)

plot_smoother <- function(deg_free) {
        dat %>% ggplot(aes(x=x,
                           y=y))+
                geom_point(alpha = 0.5)+
                theme_bw() +
                geom_smooth(
                        method = lm,
                        formula = y ~ ns(x, df = deg_free),
                        color = "blue",
                        se = FALSE
                ) +
                labs(title = paste(deg_free, "Spline Terms"))+
                geom_line(data = tibble(x, ytruth),
                          aes(x=x,
                              y=ytruth),
                          color = 'orange')
        
}

plot = ( plot_smoother(1)+ggtitle("Linear") + plot_smoother(3)+ plot_smoother(100) )
