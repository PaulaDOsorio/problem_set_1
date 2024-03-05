############################################
########## Problem set 1            ########
###### Big Data y Maching Learning #########
###### Paula Osorio:201327186   ############
###### Sandra Gamarra: 202225782 ###########
###### Erika M. Macías:  Educacion continua#  
###### Ingrith Sierra: 201720654    ########
############################################

################
### Punto 2 ###
###############


# Cargar paquete necesarios -----------------------------
# install.packages('pacman')
require(pacman)
p_load(rio, skimr, visdat, corrplot, stargazer, tidyverse,rvest, 
       Hmisc, lattice, survival, Formula, ggplot2, robustHD, psych, 
       openxlsx, writexl, robotstxt, showtext)

# Cargar datos ------------------------------------------

# Crear tabla vacía para añadir datos
datos<-tibble()
# Extraer tabla de cada data chunk y unirlo en una sola tabla
for (i in 1:10){
  
  #crear link para descargar datos de cada uno de los 10 data chunks
  link <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",i,".html")
  
  #descargar los datos del data chunk actual en una tabla
  my_table <- read_html(link) %>% html_table()
  
  #Unir información de cada nuevo data chunks con la información ya guardada
  datos <- bind_rows(datos,my_table)
}

# Inspeccionando los datos
head(datos)
skim (datos) %>% head()

# Mirando ggplot de salario
## + geometry
ggplot(data = datos , mapping = aes(x = age , y = y_ingLab_m)) +
  geom_point(col = "#FF007F" , size = 0.5)
## por grupo 
ggplot(data = datos , 
       mapping = aes(x = age , y = y_ingLab_m , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()

## density: salario mensual por sexo
g1 <- ggplot(data=datos) + 
  geom_histogram(mapping = aes(x=y_ingLab_m , group=as.factor(sex) , fill=as.factor(sex)))
g1
g1 + scale_fill_manual(values = c("0"="#FF007F" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")

## box_plot: estrato1 vs totalHoursWorked
box_plot <- ggplot(data=datos , mapping = aes(as.factor(estrato1) , totalHoursWorked)) + 
  geom_boxplot() 
box_plot

## add another geometry
box_plot <- box_plot +
  geom_point(aes(colour=as.factor(sex))) +
  scale_color_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")
box_plot

# Limpieza de la base de datos
# Ver variables con datos missing
datos_miss <- skim(datos) %>% select( skim_variable, n_missing)

Nobs= nrow(datos) 
Nobs

datos_miss<- datos_miss %>% mutate(p_missing= n_missing/Nobs)
head(datos_miss)

# Ordenar variables con mayor número de missing
datos_miss <- datos_miss %>% arrange(-n_missing) ## or arrange(desc(n_missing))
datos_miss<- datos_miss %>% filter(n_missing!= 0)
#Visualizando la estructura de los missing
ggplot(datos_miss, aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings")+ 
  theme(axis.text = element_text(size = 5)) 

#Filtramos datos de interés
# Trabajadores con Edad mayores de 18 años)

datos <- datos[datos$age >= 18, ]

# Renombrando variable edad

datos <- rename(datos, c("edad" = "age"))

# Años de educación

datos$edad_2 <- datos$edad^2

# Sexo

datos$mujer <- ifelse(datos$sex == 0, 1, 0)
datos$mujer[datos$sex == 1] <- 0

# Ocupación

# Estudiante

datos$estudiante <- ifelse(datos$p6240 == 3, 1, 0)
datos$estudiante[datos$p6240 != 3] <- 0
datos$estudiante[datos$p6240 == "."] <- NA

# Busca trabajo

datos$busca_trabajo <- ifelse(datos$p6240 == 2, 1, 0)
datos$busca_trabajo[datos$p6240 != 2] <- 0
datos$busca_trabajo[datos$p6240 == "."] <- NA

# Oficios del hogar

datos$ama_casa <- ifelse(datos$p6240 == 4, 1, 0)
datos$ama_casa[datos$p6240 != 4] <- 0
datos$ama_casa[datos$p6240 == "."] <- NA

# Parentesco
# Hijos en el hogar

datos$hijos_hogar <- ifelse(datos$p6050 == 3, 1, 0)
datos$hijos_hogar[datos$p6050 != 3] <- 0
datos$hijos_hogar[datos$p6050 == "."] <- NA

# Nivel Primaria

datos$primaria <- ifelse(datos$p6210 == 1, 1, 0)
datos$primaria[datos$p6210 == "."] <- NA

# Nivel Secundaria

datos$secundaria <- ifelse(datos$p6210 == 4, 1, 0)
datos$secundaria[datos$p6210 == "."] <- NA

# Nivel Media

datos$media <- ifelse(datos$p6210 == 5, 1, 0)
datos$media[datos$p6210 == "."] <- NA

# Nivel Superior

datos$superior <- ifelse(datos$p6210 == 6, 1, 0)
datos$superior[datos$p6210 == "."] <- NA

# Renombrar el Salario mensual

datos <- rename(datos, c("salario_mensual" = "y_ingLab_m"))

# Renombrar la variable p6240 por ocupacion

datos <- rename(datos, c("ocupacion" = "p6240"))

# Nos quedamos con los que estan trabjando

datos <- datos[datos$ocupacion == 1, ]

# Ingreso Total

datos <- rename(datos, c("ingreso_total" = "ingtot"))

# Experiencia trabajo actual

datos <- rename(datos, c("exp_trabajo_actual" = "p6426"))

# Estrato

datos <- rename(datos, c("estrato" = "estrato1"))

# Datos de Cabecera

datos$cabecera <- ifelse(datos$clase == 1, 1, 0)

# Horas de trabajo a la semana

datos <- rename(datos, c("horas_trab_usual" = "hoursWorkUsual"))

# Ciudad

datos <- rename(datos, c("ciudad" = "dominio"))

# 2.a. Nos quedamos con las variables a usar

datos<- subset(datos, select = c("directorio", "secuencia_p", "orden",
                                 "mes", "edad", "edad_2", "mujer", 
                                 "estudiante", "busca_trabajo", "ama_casa",
                                 "hijos_hogar", "primaria", "secundaria",
                                 "media", "superior", "salario_mensual",
                                 "ingreso_total", "exp_trabajo_actual",
                                 "estrato", "cabecera", "horas_trab_usual",
                                 "ocupacion", "informal",
                                 "ciudad"))

# Vemos datos faltantes de las variables seleccionadas
vis_dat(datos)
vis_miss(datos)

# create a dataset with all variables== 1 if missing
db1 <- datos %>% mutate_all(~ifelse(!is.na(.), 1, 0))
## drop  variables with not missing or  with all missing.

db1 <-  datos %>%  select(which(apply(db1, 2, sd) > 0))

M <- cor(db1)
corrplot(M) 

# Vemos la distribución de salario_mensual que es la variable con missing
ggplot(datos, aes(salario_mensual)) +
  geom_histogram(color = "gray", fill = "#0099F8") +
  ggtitle("Distribución Salario Mensual") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))


# Distribucuón del salario mensual
ggplot(datos, aes(salario_mensual)) +
  geom_histogram(color = "gray", fill = "#0099F8") +
  geom_vline(xintercept = median(datos$salario_mensual, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(datos$salario_mensual, na.rm = TRUE), linetype = "dashed", color = "blue") +  
  #ggtitle(" Ingreso mensual") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))

# Dado que la distribución del ingreso mensual tiene una cola larga a la derecha
# usamos la mediana para imputar
datos <- datos  %>%
  mutate(salario_mensual = ifelse(is.na(salario_mensual) == TRUE, median(datos$salario_mensual, na.rm = TRUE) , salario_mensual))

# Vemos ahora como quedo la base
vis_dat(datos)
vis_miss(datos)

ggplot(data = datos , mapping = aes(x = edad , y = salario_mensual)) +
  geom_point(col = "red" , size = 0.5)

# Limpieza de valores faltantes 

# Eliminamos las observaciones que tienen valores faltantes en la variable salario mensual

#datos <- datos %>% filter(!is.na(salario_mensual))

# Tratamiento de valores atípicos

# Tratamiento con winsorize

datos$salario_mensual <- psych::winsor(datos$salario_mensual, trim = 0.01)

# Logaritmo del Salario

datos$log_salario_m <- log(datos$salario_mensual)

# Estadísticas descriptivas

Tabla_descriptivas <- datos[c("mujer","edad", "ama_casa", "hijos_hogar",
                              "estrato", "estudiante", "primaria", 
                              "secundaria", "media", "superior", 
                              "salario_mensual", "ingreso_total", 
                              "exp_trabajo_actual", "horas_trab_usual", 
                              "informal")]
#vis_dat(Tabla_descriptivas)
#vis_miss(Tabla_descriptivas)
estadisticas_todos <- data.frame(sapply(Tabla_descriptivas, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_todos, file = "C:/Users/sandr/Documents/GitHub/BIG DATA/Taller1_problem_set_1/views/Estadisticas_descriptivas.xlsx")

# Mujeres

db_Mujeres <- Tabla_descriptivas[Tabla_descriptivas$mujer == 1, ]

estadisticas_mujer <- data.frame(sapply(db_Mujeres, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_mujer, file = "C:/Users/sandr/Documents/GitHub/BIG DATA/Taller1_problem_set_1/views/Estadisticas_Mujer.xlsx")


# Hombres

db_Hombres <- Tabla_descriptivas[Tabla_descriptivas$mujer == 0, ]

estadisticas_hombre <- data.frame(sapply(db_Hombres, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_hombre, file = "C:/Users/sandr/Documents/GitHub/BIG DATA/Taller1_problem_set_1/views/Estadisticas_Hombre.xlsx")



# Contruimos los gráficos de distribución salarial

grafico_1 <- ggplot(datos, aes(x = salario_mensual)) +
  geom_histogram(bins = 280, color = "blue", fill = "gray") +
  labs(x = "Salario mensual", y = "Frecuencia") +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste0("$", format(x, big.mark = ",", scientific = FALSE)), limits = c(0, 13000000), expand = c(0,0), breaks = seq(0,13000000,1000000)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ","), limits = c(0, 1000), expand = c(0,0), breaks = seq(0,1000,100)) +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1))

grafico_1

datos$genero <- factor(datos$mujer) 

datos$genero = as.factor(datos$mujer)
levels(datos$genero) = c("Hombre", "Mujer")

grafico_2 <- ggplot(datos, aes(x = salario_mensual, fill = genero)) +
  geom_histogram(bins=120, color = "gray", alpha = 0.5) +
  labs(x = "Salario mensual", y = "Frecuencia") +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste0("$", format(x, big.mark = ",", scientific = FALSE)), limits = c(0, 13000000), expand = c(0,0), breaks = seq(0,13000000,1000000)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ","), limits = c(0, 1000), expand = c(0,0), breaks = seq(0,1000,100)) +
  scale_fill_manual(name = "", values = c("#00008B", "#FF007F"), labels = c("Hombre", "Mujer")) +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1),
        legend.position = "top") 
grafico_2

# Guardar DB
db_organizada <- "db_organizada.xlsx"

# Exportando el dataframe a un archivo Excel
write.xlsx(datos, file = "C:/Users/sandr/Documents/GitHub/BIG DATA/Taller1_problem_set_1/views/db_organizada.xlsx", row.names = FALSE)


##Fin del código ##

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

library(zoo)
library(lmtest)

resettest(regresion1, power = 2)
bptest(regresion1)

library(readxl)
datos_t <- read_excel("C:\Users\USUARIO\Documents\GitHub\problem_set_1\problem_set_1")

#Usar booBootstrap para construir los intervalos de confianza

require(pacman)
library(bootstrap)
p_load("boot")

salario<-function(data,index){
  coef(lm(log_salario_m ~ edad + edad_2 ,data = datos, subset = index))[2]
}

salario(datos,1:nrow(datos))

bootstrap <- boot(datos, salario, R = 1000)
coef_bootstrap <- bootstrap$t0
error_estandar <- apply(bootstrap$t,2,sd)

set.seed(123)
boot(datos, salario, R = 1000)
boot_result<- boot(datos, salario, R=1000)
boot_ci <- boot.ci(boot_result, type = "perc")
boot_ci

class(salario)
boot.ci

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

#Estimar punto máximo

# Definir una función cuadrática

cuadratica <- function(regresion1) {
  return(-(a*x^2 + b*x + c))  # Agregamos un signo negativo para que encuentre el máximo
}

# Generar algunos datos de ejemplo
# Coeficientes del modelo
coeficientes <- coef(regresion1)

# Extraer los coeficientes relevantes
beta_1 <- coeficientes["edad"]
beta_2 <- coeficientes["edad_2"]

# Calcular la edad del punto máximo
edad_maximo <- -beta_1 / (2 * beta_2)
print(edad_maximo)


## Punto 4

install.packages(c("boot","stargazer", "caret"))
library(boot)
library(stargazer)
library(caret)

# a. Wage gap -------------------------------------------------------------

#log(w) = β1 + β2F emale + u 

modelo_1 <- lm(log_salario_m ~ mujer, data = datos)

modelo_2 <- lm(log_salario_m ~ mujer + edad + edad_2 +
                 secundaria + media + superior + informal, data = datos)

# b. Equal Pay for Equal Work? --------------------------------------------

# i. FWL

modelo_com <- lm(log_salario_m ~ edad + edad_2 +
                   secundaria + media + superior + informal, data = datos)
fit_y <- resid(modelo_com)

fit_X <- lm( mujer ~ edad + edad_2 + secundaria + media 
             + superior + informal, data = datos)


modelo_par <- lm(fit_y ~  resid(fit_X), data = datos)

#2. FWL with bootstrap

train <- trainControl(method = "boot", number = 1000)

boot_fn <- function(data, indices) {
  fit <- lm(log_salario_m ~ edad + edad_2 +
              secundaria + media + superior + informal, data = data, subset = indices)
  
  residuales_fit <- lm( mujer ~ edad + edad_2 + secundaria + media 
                        + superior + informal, data = datos, subset = indices)
  
  modelo_fit <- lm(resid(fit) ~  resid(residuales_X), data = datos, subset = indices)
  
  return(coef(modelo_fit)[2])
}

boot_fn(datos, 1:nrow(datos))

boot_results <- boot(data = datos, statistic = boot_fn, R = 1000)

modelo_list <- list(modelo_1, modelo_2, modelo_par)

stargazer(modelo_list) #default, latex

# c. Plot -----------------------------------------------------------------
modelo_brecha <- lm(log_salario_m ~ mujer + edad + edad_2 + secundaria + 
                      media + superior + informal + mujer*edad, data = datos)  

edad_minima = min(datos$edad)
edad_máxima = max(datos$edad)

salario_hombres <- predict(modelo_brecha, newdata = data.frame(mujer = 0, edad = seq(edad_minima, edad_máxima), 
                                                               edad_2 = seq(edad_minima, edad_máxima)^2,
                                                               secundaria = 0, media = 0, superior = 0,
                                                               informal = 0), interval = "confidence")

salario_mujeres <- predict(modelo_brecha, newdata = data.frame(mujer = 1, edad = seq(edad_minima, edad_máxima), 
                                                               edad_2 = seq(edad_minima, edad_máxima)^2,
                                                               secundaria = 0, media = 0, superior = 0,
                                                               informal = 0), interval = "confidence")

# Graficar los perfiles de salario en función de la edad para hombres y mujeres
plot(seq(edad_minima, edad_máxima), salario_hombres[, "fit"], type = "l", col = "#9A32CD", 
     ylim = range(salario_hombres[, "fit"], salario_mujeres[, "fit"]),
     xlab = "Edad", ylab = "Salario") 
lines(seq(edad_minima, edad_máxima), salario_mujeres[, "fit"], type = "l", col = "#CD2626") 
legend("topright", legend = c("Hombres", "Mujeres"), col = c("#9A32CD", "#CD2626"), lty = 1)


# Encontrar las edades en las que el salario alcanza su máximo para hombres y mujeres
edad_max_salario_hombres <- seq(edad_minima, edad_máxima)[which.max(salario_hombres[, "fit"])]
edad_max_salario_mujeres <- seq(edad_minima, edad_máxima)[which.max(salario_mujeres[, "fit"])]

# Imprimir las edades en las que el salario alcanza su máximo para hombres y mujeres
cat("Edad en la que el salario alcanza su máximo para hombres:", edad_max_salario_hombres, "\n")
cat("Edad en la que el salario alcanza su máximo para mujeres:", edad_max_salario_mujeres, "\n")


# Coeficientes y errores estándar
coeficientes <- coef(modelo_brecha)
errores <- summary(modelo_brecha)$coefficients[, "Std. Error"]


# Calcular la "edad pico" y sus errores estándar por género
peak_age_male <- -coeficientes["edad"] / (2 * (coeficientes["edad_2"]))
peak_age_female <- -(coeficientes["edad"]+coeficientes["mujer:edad"]) / (2 * (coeficientes["edad_2"]))

# Calcular los errores estándar para la "edad pico" por género
se_peak_age_male <- sqrt((errores["edad"] / (2 * coeficientes["edad_2"]))^2 + ((coeficientes["edad"] / (2 * coeficientes["edad_2"]^2)) * errores["edad_2"])^2)
se_peak_age_female <- sqrt(((errores["edad"] + errores["mujer:edad"]) / (2 * coeficientes["edad_2"]))^2 + (((coeficientes["edad"] + coeficientes["mujer:edad"]) / (2 * coeficientes["edad_2"]^2)) * errores["edad_2"])^2)

#Invervalos de confianza

ci.female = cat(peak_age_female-1.96*se_peak_age_female,
                peak_age_female+1.96*se_peak_age_female)

ci.male = cat(peak_age_male-1.96*se_peak_age_male,
                peak_age_male+1.96*se_peak_age_male)

#Salario en edad pico por genero
salario_mujer <- coeficientes["(Intercept)"] + coeficientes["mujer"] + 
  coeficientes["edad"] * peak_age_female +
  coeficientes["edad_2"] * (peak_age_female^2) +
  coeficientes["mujer:edad"] * peak_age_female

salario_hombre <- coeficientes["(Intercept)"] +  
  coeficientes["edad"] * peak_age_male +
  coeficientes["edad_2"] * (peak_age_male^2) 

# Contraste de las edades pico utilizando pruebas estadísticas relevantes
# Por ejemplo, prueba de hipótesis para comparar las edades pico entre géneros
t_statistic <- (salario_hombre - salario_mujer) / sqrt(salario_hombre^2 + salario_mujer^2)
degrees_of_freedom <- min(length(datos$mujer[datos$mujer == 1]), length(datos$mujer[datos$mujer == 0])) - 1
p_value <- 2 * pt(abs(t_statistic), df = degrees_of_freedom)

cat("T-Statistic:", t_statistic, "\n")
cat("P-Value:", p_value, "\n")

######################################
#Punto 5a

#cargar paquetes necesarios
p_load(rio, tidyverse,caret, gridExtra,skimr) 

#establecer semilla para hacer replicable el proceso
set.seed(10101)

#crear variables no lineales para especificaciones más complejas
datos$estrato_4 <- datos$estrato^4
datos$exp_trabajo_actual_2 <- datos$exp_trabajo_actual^2

#Dividir la muestra para que el 70% se utilice para entrenamiento y el 30% para pruebas
inTrain <- createDataPartition(
  y = datos$log_salario_m,  
  p = .70, 
  list = FALSE
)

training <- datos[ inTrain,]
testing  <- datos[-inTrain,]

#Punto 5b

#Definir las especificaciones de los modelos a probar
modelo_1 <- log_salario_m ~ edad + edad_2
modelo_2 <- log_salario_m ~ mujer
modelo_3 <- log_salario_m ~ mujer+ edad + edad_2 + secundaria + media + superior + informal
modelo_4 <-log_salario_m ~ edad + edad_2 + estrato + busca_trabajo
modelo_5 <-log_salario_m ~ superior + exp_trabajo_actual
modelo_6 <-log_salario_m ~ mujer + edad + edad_2 + estrato_4 + busca_trabajo + superior 
modelo_7 <-log_salario_m ~ mujer + edad + edad_2 + estrato_4 + busca_trabajo + superior + exp_trabajo_actual_2 + hijos_hogar + cabecera
modelo_8 <-log_salario_m ~ mujer + edad + edad_2 + estrato_4 + busca_trabajo + superior + exp_trabajo_actual_2 + hijos_hogar + cabecera + secundaria + media + informal

modelos <- c(modelo_1,modelo_2,modelo_3,modelo_4,modelo_5,modelo_6,modelo_7,modelo_8)

#función que calcula las predicciónes de y para los modelos y el RMSE
t_RMSE <- function(modelos,training, testing, var_y){
  resultados <- tibble()
  for (i in 1:length(modelos)) {
    m <- lm(modelos[[i]], data=training) #saca los betas del modelo con los datos de entrenamiento
    p <- predict(m, testing) #calcula la variable dependiente con el modelo para los x de prueba
    score<- RMSE(p, testing[[var_y]]) #calcula el RMSE del modelo estimado
    resultados <- bind_rows(resultados, tibble(modelo=paste0("modelo",i), RMSE=score)) #llenado de tabla con los resultados 
  }
  return(resultados) #devuelve el resultado de la función
}

#calculo de los RMSE de los modelos especificados
tabla <- t_RMSE(modelos,training, testing, "log_salario_m") #correr función con los inputs específicos

#computar los errores de la predicción para el modelo con el error más bajo
modelo_error_bajo <- lm(modelo_8,data = training)
testing$predictions <- predict(modelo_error_bajo, testing)
errores <- with(testing,(log_salario_m-predictions)^2)
errores1 <- with(testing,(log_salario_m-predictions))

#graficar la distribución de los errores del mejor modelo (en valor absoluto y sin valor absoluto)
ggplot(testing, aes(x = errores )) +
  geom_histogram(bins = 50, fill = "darkblue") +
  labs(x = "error de predicción", y = "Cantidad") +
  theme_bw()

ggplot(testing, aes(x = errores1 )) +
  geom_histogram(bins = 50, fill = "darkblue") +
  labs(x = "error de predicción", y = "Cantidad") +
  theme_bw()

#graficar la dispersión entre errores y variable Y
plot(errores, testing$log_salario_m, main = "Scatter Plot", xlab = "error de predicción", ylab = "log salario test", pch = 16, col = "blue")
plot(errores1, testing$log_salario_m, main = "Scatter Plot", xlab = "error de predicción", ylab = "log salario test", pch = 16, col = "blue")

#Punto 5d
ctrl <- trainControl(method = "LOOCV") #establece el método de cross-validation a utilizar

#se utiliza el método solo con los 2 modelos con los errores predictivos más bajos
#para el modelo 7
cross_validation_1 <- train(modelo_7,data = datos,method = 'lm',trControl= ctrl)
score1<-RMSE(cross_validation_1$pred$pred, datos$log_salario_m) #se guarda el resultado de error de predicción

#para el modelo 8
cross_validation_2 <- train(modelo_8,data = datos,method = 'lm',trControl= ctrl)
score2<-RMSE(cross_validation_2$pred$pred, datos$log_salario_m) #se guarda el resultado de error de predicción

#comparar el error del testeo con la validación LOOCV
comparativo<- data.frame( Model= c(7, 8),RMSE_vsa= c(as.numeric(tabla[3,2]), as.numeric(tabla[8,2])),RMSE_loocv= c(score1, score2))
