##############################
# Punto 2
##############################


# Cargar paquete necesarios -----------------------------
# install.packages('pacman')
require(pacman)
p_load(tidyverse,rvest, Hmisc, lattice, survival, Formula, ggplot2, 
       robustHD, psych, openxlsx, writexl, robotstxt, showtext)

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

# Limpieza de la base de datos

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

# Logaritmo del Salario

datos$log_salario_m <- log(datos$salario_mensual)

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
                                 "ocupacion", "log_salario_m", "informal",
                                 "ciudad"))

# 2.c. Limpieza de valores faltantes 

# Eliminamos las observaciones que tienen valores faltantes en el las siguientes variables:
# salario nominal mensual

datos <- datos %>% filter(!is.na(salario_mensual))

# Tratamiento de valores atípicos

# Tratamiento con winsorize

datos$salario_mensual <- psych::winsor(datos$salario_mensual, trim = 0.01)
datos$log_salario_m <- log(datos$salario_mensual)

# Estadísticas Descriptivas 

# Variables de análisis

# Estadísticas descriptivas

Tabla_descriptivas <- datos[c("mujer","edad", "ama_casa", "hijos_hogar",
                              "estrato", "estudiante", "primaria", 
                              "secundaria", "media", "superior", 
                              "salario_mensual", "ingreso_total", 
                              "exp_trabajo_actual", "horas_trab_usual", 
                              "informal")]

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

##Fin del código ##git checkout -b 


