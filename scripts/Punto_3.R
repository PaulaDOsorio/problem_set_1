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

library(stargazer)
stargazer(regresion1)
str(regresion1)
regresion1$coefficients

residuos <- residuals(regresion1)
hist(residuos, breaks = 20, col = "lightblue", main = "Histograma de Residuos")
curve(dnorm(x, mean = mean(residuos), sd = sd(residuos)), add = TRUE, col = "red")

library(lmtest)
library(zoo)
ramsey.reset(regresion1)

resettest(regresion1, power = 2)
bptest(regresion1)

library(readxl)
datos_t <- read_excel("C:/Users/USUARIO/Documents/GitHub/problem_set_1/views/db_organizada.xlsx")

#Usar booBootstrap para construir los intervalos de confianza

require(pacman)
library(bootstrap)
p_load("boot")

salario<-function(data,index){
  coef(lm(log_salario_m ~ edad + edad_2 ,data = datos, subset = index))[2]
}

salario(datos,1:nrow(datos))

bootstrap <- boot(datos, salariof, R = 1000)
coef_bootstrap <- bootstrap$t0
error_estandar <- apply(bootstrap$t,2,sd)

set.seed(123)
boot(datos, salario, R = 1000)
boot_result<- boot(datos, salario, R=1000)
boot_ci <- boot.ci(boot_result, type = "perc")
boot_ci

quantile(salariof, c(0.025, 0.975))
conf_int <- quantile(salariof, c(0.025, 0.975))
class(salariof)
boot.ci



