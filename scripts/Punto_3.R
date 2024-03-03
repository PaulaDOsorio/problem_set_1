#Cargar paquetes necesarios
#install.packages(pacman)
require(pacman)
p_load(rio, skimr, visdat, corrplot, stargazer, tidyverse,rvest, 
       Hmisc, lattice, survival, Formula, ggplot2, robustHD, psych, 
       openxlsx, writexl, robotstxt, showtext,boot, bootstrap,readxl)
#Punto 3 Age-Wage profile

#Estimación de los coeficientes
regresion1 <- lm(log_salario_m ~ edad + edad_2,  data=datos)
stargazer(regresion1,type="text", align = TRUE, 
          no.space = TRUE, title = "Estimación Salario por Edad",
          out = "C:/Users/USUARIO/Documents/GitHub/problem_set_1/views/regresion1.tex") 
summary(regresion1)

str(regresion1)
regresion1$coefficients

library(readxl)
datos_t <- read_excel("C:/Users/USUARIO/Documents/GitHub/problem_set_1/views/db_organizada.xlsx")

#Usar booBootstrap para construir los intervalos de confianza

require(pacman)
library(bootstrap)
p_load("boot")

u_err_est<-function(data,index){
  coef(lm(log_salario_m~edad + edad_2, data = datos, subset = index)) 
}

u_err_est(datos,1:nrow(datos))

bootstrap <- boot(datos_t, u_err_est, R = 1000)
coef_bootstrap <- bootstrap$t0
error_estandar <- apply(bootstrap$t,2,sd)


