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
plot(boot_ci)
#realizar gráfico perfil edad-ganancias

ggplot(data = datos , mapping = aes(x = edad , y = log_salario_m))
ggplot(data = datos , mapping = aes(x = edad , y = log_salario_m)) +
  geom_point(col = "red" , size = 0.5)

ggplot(data = datos, mapping = aes (x= edad, y = log_salario_m)) +
  geom_point() +
  geom_point(data = punto_max, aes(color = "Punto Máximo"), size = 3) +
  scale_color_manual(values = "red") +
  labs(title = "Gráfico pefil edad-ganancias")

bootstrap <- boot(datos, salariof, R = 1000)
coef_boot <- bootstrap$t0
error_estandar <- apply(bootstrap$t,2,sd)

# Creamos df con las variables x y las y
coef_boot <- bootstrap$t0
x <- seq(18, 90, length.out = 100)
y <- coef_boot[1] + coef_boot[2] * x + coef_boot[3] * x^2
y_i <- (coef_boot[1]-1.96*error_estandar[1]) + (coef_boot[2]-1.96*error_estandar[2])*x + 
  (coef_boot[3]-1.96*error_estandar[3])*x^2
y_s <- (coef_boot[1]+1.96*error_estandar[1]) + (coef_boot[2]+1.96*error_estandar[2])*x + 
  (coef_boot[3]+1.96*error_estandar[3])*x^2

df <- data.frame(x, y, y_i, y_s)

# Graficar la función

grafico_3 <- ggplot(df, aes(x = x, y = y)) +
  geom_line(aes(color = "Estimado"), size = 1) +
  geom_line(aes(x = x, y = y_i, color = "Límite inferior"), linetype = "dotted", size = 1) +
  geom_line(aes(x = x, y = y_s, color = "Límite superior"), linetype = "dotted", size = 1) +
  scale_color_manual(name = "", values = c("Estimado" = "black", "Límite inferior" = "blue", "Límite superior" = "red")) +
  labs(x = "Edad", y = "Log(Salario)") +
  theme_classic() +
  scale_x_continuous(limits = c(18, 90)) +
  geom_vline(xintercept = 43, linetype = "dotted") +
  theme(legend.position = "bottom")
grafico_3
