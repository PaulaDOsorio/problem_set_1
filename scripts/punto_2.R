############################################
########## Problem set 1            ########
###### Big Data y Maching Learning #########
###### Paula Osorio:  ############
###### Sandra Gamarra: 202225782 ###########
###### Erika M. Macías:  #########  
###### Ingrith Sierra:   #########
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
  geom_point(col = "red" , size = 0.5)
## by group
ggplot(data = datos , 
       mapping = aes(x = age , y = y_ingLab_m , group=as.factor(formal) , color=as.factor(formal))) +
  geom_point()

## density: income by sex
g1 <- ggplot(data=datos) + 
  geom_histogram(mapping = aes(x=y_ingLab_m , group=as.factor(sex) , fill=as.factor(sex)))
g1
g1 + scale_fill_manual(values = c("0"="red" , "1"="blue") , label = c("0"="Hombre" , "1"="Mujer") , name = "Sexo")

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
  geom_histogram(color = "#000000", fill = "#0099F8") +
  ggtitle("Distribucion Salario Mensual") +
  theme_classic() +
  theme(plot.title = element_text(size = 18))


# Distribucuón del salario mensual
ggplot(datos, aes(salario_mensual)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  geom_vline(xintercept = median(datos$salario_mensual, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(datos$salario_mensual, na.rm = TRUE), linetype = "dashed", color = "blue") +  
  ggtitle(" Ingreso mensual") +
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

write.xlsx(estadisticas_todos, file = "C:/Users/sandr/Documents/GitHub/BIG DATA/problem_set_1/views/Estadisticas_descriptivas.xlsx")

# Mujeres

db_Mujeres <- Tabla_descriptivas[Tabla_descriptivas$mujer == 1, ]

estadisticas_mujer <- data.frame(sapply(db_Mujeres, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_mujer, file = "C:/Users/sandr/Documents/GitHub/BIG DATA/problem_set_1/views/Estadisticas_Mujer.xlsx")


# Hombres

db_Hombres <- Tabla_descriptivas[Tabla_descriptivas$mujer == 0, ]

estadisticas_hombre <- data.frame(sapply(db_Hombres, function(x) 
  c(mean = mean(x), sd = sd(x))))

write.xlsx(estadisticas_hombre, file = "C:/Users/sandr/Documents/GitHub/BIG DATA/problem_set_1/views/Estadisticas_Hombre.xlsx")



# Contruimos los gráficos de distribución salarial

grafico_1 <- ggplot(datos, aes(x = salario_mensual)) +
  geom_histogram(bins = 280, color = "blue", fill = "gray") +
  labs(x = "Salario nominal mensual", y = "Frecuencia") +
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
  labs(x = "Salario nominal mensual", y = "Frecuencia") +
  theme_minimal() +
  scale_x_continuous(labels = function(x) paste0("$", format(x, big.mark = ",", scientific = FALSE)), limits = c(0, 13000000), expand = c(0,0), breaks = seq(0,13000000,1000000)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ","), limits = c(0, 1000), expand = c(0,0), breaks = seq(0,1000,100)) +
  scale_fill_manual(name = "Género", values = c("blue", "#FF007F"), labels = c("Hombre", "Mujer")) +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1),
        legend.position = "top") 
grafico_2
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




