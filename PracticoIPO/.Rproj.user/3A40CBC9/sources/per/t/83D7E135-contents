#Práctico 4 - Codigo de análisis

#Esta práctica asume como base el desarrollo de la Práctica anterior, a la cual se hará referencia permanente. Sin embargo, en esta oportunidad la base de datos contiene los datos de los 18 países participantes en la encuesta, no solo Chile. 
#El código específico que crea esta base está disponible acá

pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))
proc_data <- latinobarometro2020 %>% select(p13st_e, # Confianza en el Gobierno
                                            p13st_d, # Confianza en el congreso
                                            p13st_f, # Confianza en el Poder Judicial
                                            p13st_g, # Confianza en los partidos políticos
                                            reeduc_1,# nivel educacional
                                            sexo,# sexo
                                            edad,# edad
                                            idenpa) # pais

# Comprobar
names(proc_data)

proc_data$p13st_e <- recode(proc_data$p13st_e, "c(-2,-1)=NA")
proc_data$p13st_d <- recode(proc_data$p13st_d, "c(-2,-1)=NA")
proc_data$p13st_f <- recode(proc_data$p13st_f, "c(-2,-1)=NA")
proc_data$p13st_g <- recode(proc_data$p13st_g, "c(-2,-1)=NA")

proc_data$p13st_e <- recode(proc_data$p13st_e, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_d <- recode(proc_data$p13st_d, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_f <- recode(proc_data$p13st_f, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_g <- recode(proc_data$p13st_g, "1=3; 2=2; 3=1; 4=0")

proc_data <- proc_data %>% rename("conf_gob"=p13st_e, # Confianza en el gobierno
                                  "conf_cong"=p13st_d, # Confianza en el congreso
                                  "conf_jud"=p13st_f, # Confianza en el Poder Judicial
                                  "conf_partpol"=p13st_g) # Confianza en los partidos políticos

proc_data$conf_gob <- set_label(x = proc_data$conf_gob,label = "Confianza: Gobierno")
get_label(proc_data$conf_gob)

proc_data$conf_cong  <- set_label(x = proc_data$conf_cong, label = "Confianza: Congreso")
get_label(proc_data$conf_cong)

proc_data$conf_jud  <- set_label(x = proc_data$conf_jud, label = "Confianza: Poder judicial")
get_label(proc_data$conf_jud)

proc_data$conf_partpol  <- set_label(x = proc_data$conf_partpol, label = "Confianza: Partidos politicos")
get_label(proc_data$conf_partpol)

proc_data$conf_inst <- (proc_data$conf_gob+proc_data$conf_cong+proc_data$conf_jud+proc_data$conf_partpol)
summary(proc_data$conf_inst)
proc_data$conf_inst  <- set_label(x = proc_data$conf_inst, label = "Confianza en instituciones")

proc_data$conf_gob <- set_labels(proc_data$conf_gob,
                                 labels=c( "Ninguna"=0,
                                           "Poca"=1,
                                           "Algo"=2,
                                           "Mucha"=3))

proc_data$conf_cong <- set_labels(proc_data$conf_cong,
                                  labels=c( "Ninguna"=0,
                                            "Poca"=1,
                                            "Algo"=2,
                                            "Mucha"=3))

proc_data$conf_jud <- set_labels(proc_data$conf_jud,
                                 labels=c( "Ninguna"=0,
                                           "Poca"=1,
                                           "Algo"=2,
                                           "Mucha"=3))

proc_data$conf_partpol <- set_labels(proc_data$conf_partpol,
                                     labels=c( "Ninguna"=0,
                                               "Poca"=1,
                                               "Algo"=2,
                                               "Mucha"=3))


proc_data$reeduc_1 <- car::recode(proc_data$reeduc_1, "c(1,2,3)=1; c(4,5)=2; c(6,7)=3")

proc_data$reeduc_1 <- factor(proc_data$reeduc_1,
                             labels = c("Educacion basica", "Educacion media", "Educacion superior"),
                             levels = c(1, 2, 3))

proc_data <- rename(proc_data,"educacion"=reeduc_1)

proc_data$educacion <- set_label(x = proc_data$educacion,label = "Educación")

proc_data$sexo <- car::recode(proc_data$sexo, "1=0;2=1")

proc_data$sexo <- factor(proc_data$sexo,
                         labels=c( "Hombre",
                                   "Mujer"),
                         levels=c(0,1))


proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")
get_label(proc_data$edad)
proc_data$edad <- set_label(x = proc_data$edad,label = "Edad")
proc_data <- as.data.frame(proc_data)

save(proc_data,file = "C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Practicos/PracticoIPO/input/data-proc/latinobarometro_total.RData")

#El análisis se divide en descripción de variables y contraste de hipótesis. 
#En esta práctica nos enfocaremos en la primera fase, que llega hasta el punto 3 del código de análisis: Descripción de Variables

#Al igual que el Código de Preparación, el Código de Análisis posee una estructura definida. 
#En este caso son 4 partes, donde las primeras son similares al código de preparación:
  
#0. Identificación y descripción general: Título, autor(es), fecha, información breve sobre el contenido del documento
#1. Librerías principales (de R) a utilizar en el análisis
#2.Datos (que provienen de los preparados en la fase anterior)
#Descripción de variables
 #a. Tabla general de variables para la sección metodológica del reporte
 #b. Exploración descriptiva de relaciones entre variables
#3. Contraste de hipótesis / inferencia estadística según la técnica que corresponda  
  
#Código de Análisis - Latinobarómetro 2020
  
#1. Librerías

pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2) # Para la mayoría de los gráficos
  
#2. Cargar base de datos
#Vamos a cargar la base de datos latinobarometro_proc.Rdata, que generamos durante el práctico 3:

load(url("https://github.com/Kevin-carrasco/R-data-analisis/raw/main/files/data/latinobarometro_total.RData"))

names(proc_data) # Muestra los nombres de las variables de la base de datos

dim(proc_data) # Dimensiones

#Recordando el contenido de cada variable preparada en la práctica anterior:
  
#[conf_gob] = Confianza en el gobierno.
#[conf_cong] = Confianza en el congreso.
#[conf_jud] = Confianza en el poder judicial.
#[conf_partpol] = Confianza en los partidos políticos.
#[conf_inst] = Indice sumativo de confianza en instituciones políticas.
#[educacion] = Nivel educacional(1 = Educacion básica, 2 = Educacion media, 3 = superior)
#[sexo] = Sexo (O = Hombre; 1 = Mujer)
#[edad] = ¿Cuáles su edad?

#3. Descripción de variables

#Los resultados referidos a descripción de variables se presentan en dos momentos del reporte de investigación:
 #En la sección de metodología, cuando se presentan las variables del estudio en una tabla descriptiva de variables.
 #En la sección de análisis, que en general comienza con una exploración de asociaciones entre variables, también conocido como análisis descriptivo.

#3.1 Tabla descriptiva de variables para sección metodológica

#A continuación se presentan dos opciones de generar esta tabla descriptiva de variables con distintas librerías de R.

 #a. Tabla descriptiva con stargazer
 #La función stargazer (de la librería del mismo nombre) permitirá mostrar los principales estadísticos descriptivos univariados de las variables: 
 #medidas de tendencia central (media), de dispersión (desviación estándar) y posición (mínimo, máximo, percentiles).

stargazer(proc_data,type = "text")

#La opción type="text" permite que podamos ver los resultados directamente en la consola, 
#de manera bastante rudimentaria. Con otras opciones que veremos más adelante se puede estilizar para su publicación.

#Una distinción relevante a considerar cuando se describen variables es si estas son categóricas o continuas. 
#La definición de si una variables es tratada como categórica o continua es algo que hace el/la autor/a del reporte, 
#sin embargo hay variables nominales como sexo que claramente corresponden a categóricas, y por lo tanto no corresponde hacer un promedio entre ambas. 
#Sin embargo, como esta variable está codificada 0 (hombre) y 1 (mujer), en este caso lo que indica el valor de la columna promedio 
#(Mean=0.537) es la proporción de mujeres vs hombres. En otras palabras, hay un 54% de mujeres y 46% de hombres en la muestra.

#b. Tablas descriptivas con descr, librería sjmiscsjmisc::descr

sjmisc::descr(proc_data)

#En este caso utilizamos la forma librería::función (sjmisc::descr), ya que la función descr también existe en otras librerías 
#Seleccionamos algunas columnas específicas con información más relevante con la opción show. Además, agregamos la función kable para obtener una tabla que luego sea fácilmente publicable en distintos formatos

sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")

#c. Tabla descriptiva con summarytools::dfSummary

#Esta tercera opción nos ofrece una tabla aún más detallada, con gráficos para cada variable, las frecuencias para cada valor, 
#y las etiquetas de las variables, por lo que es muy recomendable.

summarytools::dfSummary(proc_data, plain.ascii = FALSE)

view(dfSummary(proc_data, headings=FALSE))

##Nota sobre casos perdidos (NAs) na.omit(data)##

#Hasta ahora hemos mantenido los casos perdidos en la base de datos, ya que son importantes de reportar en la tabla general de variables. 
#Sin embargo, de aquí en adelante se recomienda trabajar solo con casos completos, es decir, sacar los casos perdidos.

#El quitar los casos perdidos de una base de datos es muy simple con la función na.omit, 
#pero para tomar precauciones y asegurarse que funciona se recomienda el siguiente procedimiento:

 #respaldar la base de datos original en el espacio de trabajo (por si queremos en adelante realizar algún análisis referido a casos perdidos), la dejaremos con el nombre proc_data_original.
 #contamos el número de casos con el comando dim
 #contamos el número de casos perdidos con sum(is.na(proc_data))
 #borramos los casos perdidos con proc_data <-na.omit(proc_data)
 #contamos nuevamente con dim para asegurarnos que se borraron
 #por temas de funcionamiento de R, al realizar la operación de sacar casos perdidos, se borra toda la información de las etiquetas (labels), así que las recuperamos de la base original con el comando copy_labels, de la librería sjlabelled.

proc_data_original <-proc_data
dim(proc_data)

sum(is.na(proc_data))

proc_data <-na.omit(proc_data)
dim(proc_data)

proc_data <-sjlabelled::copy_labels(proc_data,proc_data_original)

#3.2 Visualización de variables
#Para visualizar variables mediante gráficos, en R el paquete más comúnmente usado es ggplot2

ggplot(proc_data, aes(x = conf_inst))

proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar()

proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar(fill = "coral")

proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar(fill = "coral")+
  labs(title = "Confianza en instituciones",
       x = "Confianza en instituciones",
       y = "Frecuencia")

# Crear el gráfico usando ggplot2

graph1 <- proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar(fill = "coral")+
  labs(title = "Confianza en instituciones",
       x = "Confianza en instituciones",
       y = "Frecuencia") +
  theme_bw()

graph1

# y lo podemos guardar:

ggsave(graph1, file="output/graph1.png")

#3.3 Exploración de asociación entre variables

#Dado que las hipótesis de investigación corresponden a asociación entre variables, antes de realizar el contraste de hipótesis se suele presentar un análisis descriptivo que explora las asociaciones entre variables.
#La forma de explorar las asociaciones entre variables dependen de la naturaleza de las variables que se asocian:
 #Variables categóricas: tabla de contingencia
 #Variable categórica y continua: tabla de promedios por cada categoría

#Tablas de contingencia para variables categóricas

#Para tablas de contingencia categóricas utilizaremos la función sjt.xtab, de la librería sjPlot. 
#Veamos primero una especificación simple: sjPlot::sjt.xtab

sjt.xtab(proc_data$educacion, proc_data$sexo)

sjt.xtab(proc_data$educacion, proc_data$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8"
)

#Tablas de promedio de variable continua por una categóricas

#Vamos a explorar datos de nuestra variable de confianza en instituciones conf_inst por los niveles educacionales educacion.
#Una forma rápida de explorar esto es mediante la función tapply, que nos entrega de manera simple el promedio de una variable por otra:

tapply(proc_data$conf_inst, proc_data$educacion, mean)

#Aquí vemos en promedio de conf_inst para cada uno de los 3 niveles de la variable educación educacion. 
#Si se estima conveniente este tipo de cruces se puede representar también en una tabla con más opciones de información y también de publicación. 

#Para esto utilizaremos una función algo más compleja de la librería dplyr.dplyr Esta librería permite aplicar una serie de funciones concatenadas y enlazadas mediante el operador %>%

proc_data %>% # se especifica la base de datos
  select(conf_inst,educacion) %>% # se seleccionan las variables
  dplyr::group_by(Educación=sjlabelled::as_label(educacion)) %>% # se agrupan por la variable categórica y se usan sus etiquetas con as_label
  dplyr::summarise(Obs.=n(),Promedio=mean(conf_inst),SD=sd(conf_inst)) %>% # se agregan las operaciones a presentar en la tabla
  kable(., format = "markdown") # se genera la tabla

#Esta asociación también se puede representar de manera más simple con un gráfico, en este caso de cajas o boxplot mediante la función geom_boxplot de gplot2:

graph <- ggplot(proc_data, aes(x =educacion, y = conf_inst)) +
  geom_boxplot() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_minimal()

graph

ggsave(graph, file="output/graph.png")

#Sin embargo, al ser los promedios similares no permite ver demasiadas diferencias… Probemos otro

ggplot(proc_data, aes(x =educacion, y = conf_inst)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_minimal()

#En este gráfico cada punto representa una observación para cada categoría. 
#Por lo tanto, al existir tantos valores difernetes en cada categoría, el gráfico tampoco nos presenta información sustantiva ¿Qué necesitamos hacer? 
#Necesitamos obtener exactamente los datos que queremos graficar, esto es, el promedio por cada categoría. Volvamos a group_by

datos <- proc_data %>% group_by(educacion) %>% 
  summarise(promedio = mean(conf_inst))

ggplot(datos, aes(x =educacion, y = promedio)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_minimal()+
  ylim(0, 12)

#Este gráfico entrega un poco más de información, pero al ver pocas diferencias en el promedio de cada categoría no se logran evidenciar

proc_data$idenpa <- factor(proc_data$idenpa,
                           labels=c("Argentina",
                                    "Bolivia",
                                    "Brasil",
                                    "Chile",
                                    "Colombia",
                                    "Costa Rica",
                                    "Cuba",
                                    "República Dominicana",
                                    "Ecuador",
                                    "El Salvador",
                                    "Guatemala",
                                    "Honduras",
                                    "México",
                                    "Nicaragua",
                                    "Panamá",
                                    "Paraguay",
                                    "Uruguay",
                                    "Venezuela"),
                           levels=c("32",
                                    "68",
                                    "76",
                                    "152",
                                    "170",
                                    "188",
                                    "214",
                                    "218",
                                    "222",
                                    "320",
                                    "340",
                                    "484",
                                    "558",
                                    "591",
                                    "600",
                                    "604",
                                    "858",
                                    "862"))


graph_box <- ggplot(proc_data, aes(x = idenpa, y = conf_inst)) +
  geom_boxplot() +
  labs(x = "País", y = "Confianza en instituciones") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje x

graph_box

# y lo podemos guardar:

ggsave(graph_box, file="output/graph.png")

#De manera alternativa, podemos seguir explorando nuestros datos con otros gráficos
#Para varias variables univariadas, tipo escala likert, una buena alternativa es el paquete sjPlot, en este caso la función plot_stackfrq:

graph2 <- sjPlot::plot_stackfrq(dplyr::select(proc_data, conf_gob,
                                              conf_cong,
                                              conf_jud,
                                              conf_partpol),
                                title = "Confianza en instituciones políticas") +
  theme(legend.position="bottom")

graph2

# Guardamos

ggsave(graph2, file="output/graph2.png")

#Para asociación de dos variables, retomemos el primer gráfico:

graph3 <- proc_data %>% ggplot(aes(x = conf_inst, fill = sexo)) + 
  geom_bar() +
  xlab("Confianza en instituciones") +
  ylab("Cantidad") + 
  labs(fill="Sexo")+
  scale_fill_discrete(labels = c('Hombre','Mujer'))

graph3

# Guardamos

ggsave(graph3, file="output/graph3.png")

#Una forma alternativa

proc_data %>% ggplot(aes(x = conf_inst)) + 
  geom_bar() +
  xlab("Confianza en instituciones") +
  ylab("Cantidad")+
  facet_wrap(~sexo)

#Para variables continuas

graph4 <- ggplot(proc_data, aes(x = as.numeric(edad))) +
  geom_histogram(binwidth=0.6, colour="black", fill="yellow") +
  theme_bw() +
  xlab("Edad") +
  ylab("Cantidad")

graph4 

# Guardamos

ggsave(graph4, file="output/graph4.png")

#y lo podemos complicar un poco más…

#Asociación entre tres variables

datos <- proc_data %>% group_by(educacion, sexo) %>% 
  summarise(promedio = mean(conf_inst))

ggplot(datos, aes(x =educacion, y = promedio)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_bw()+
  ylim(0, 12)+
  facet_wrap(~sexo)

#o alternativamente…

ggplot(datos, aes(x =sexo, y = promedio)) +
  geom_point() +
  labs(x = "Sexo", y = "Confianza en instituciones") +
  theme_bw()+
  ylim(0, 12)+
  facet_wrap(~educacion)

#Probemos otras agrupaciones. Por ejemplo, categorizar edad en grupos para estimar promedios grupales. 
#Una función clave para lograr esto puede ser case_when de dplyr, combinándolo con mutate. Es decir, crear una nueva variable a partir de un condicional

summary(proc_data$edad)

proc_data <- proc_data %>% 
  mutate(edad_groups = case_when(edad >=16 & edad<=25 ~ "Entre 16 y 25 años",
                                 edad >=26 & edad<=39 ~ "Entre 26 y 39 años",
                                 edad >=40 & edad<=65 ~ "Entre 40 y 65 años",
                                 edad >65 ~ "Más de 65 años"))

table(proc_data$edad_groups)

#Ahora creamos este gráfico

datos <- proc_data %>% group_by(educacion, edad_groups) %>% 
  summarise(promedio = mean(conf_inst))

ggplot(datos, aes(x =educacion, y = promedio)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_bw()+
  ylim(0, 7)+
  facet_wrap(~edad_groups)

#y lo podemos seguir complicando, por ejemplo, agregando otra variable en el gráfico

datos <- proc_data %>% group_by(educacion, sexo, edad_groups) %>% 
  summarise(promedio = mean(conf_inst))

ggplot(datos, aes(x =educacion, y = promedio, color=sexo)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_bw()+
  ylim(0, 7)+
  facet_wrap(~edad_groups)

#Con ‘color’ (gráfico anterior) solo se diferencia la variable según color. 
#Con ‘shape’ (gráfico siguiente) también se diferencia según la forma de la representación

ggplot(datos, aes(x =educacion, y = promedio, color=sexo, shape=sexo)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_bw()+
  ylim(0, 7)+
  facet_wrap(~edad_groups)

#Y tenemos aún un problema… las categorías del eje x se están solapando. Eso lo podemos corregir modificando el ángulo del eje x.

ggplot(datos, aes(x = educacion, y = promedio, color = sexo, shape = sexo)) +
  geom_point() +
  labs(x = "Educación", y = "Confianza en instituciones") +
  theme_bw() +
  ylim(0, 7) +
  facet_wrap(~edad_groups) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Unir bases de datos

#Una de las grandes ventajas del uso de sofwares para el análisis de datos es poder utilizar distintas fuentes de datos y combinarlas en una sola base de datos que nos permita potenciar y profundizar nuestros análisis. 
#En este ejemplo, vincularemos la base del Latinobarómetro con la World Values Survey

#Cargamos ambas bases de datos

load(url("https://github.com/Kevin-carrasco/R-data-analisis/raw/main/files/data/latinobarometro_total.RData")) 
load(url("https://github.com/Kevin-carrasco/R-data-analisis/raw/main/files/data/data_wvs.RData")) 

#Para trabajar con ambas bases, agruparemos las variables de interés por país, por lo que ya no trabajaremos directamente con individuos.

context_data <- wvs %>% group_by(B_COUNTRY) %>% # Agrupar por país
  summarise(gdp = mean(GDPpercap1, na.rm = TRUE), # Promedio de GDP per capita
            life_exp = mean(lifeexpect, na.rm = TRUE), # Promedio esperanza de vida
            gini = mean(giniWB, na.rm = TRUE)) %>%  # Promedio gini
  rename(idenpa=B_COUNTRY) # Para poder vincular ambas bases, es necesario que la variable de identificación se llamen igual
context_data$idenpa <- as.numeric(context_data$idenpa) # Como era categórica, la dejamos numérica

proc_data <- proc_data %>% group_by(idenpa) %>%  # agrupamos por país
  summarise(promedio = mean(conf_inst, na.rm = TRUE)) # promedio de confianza en instituciones por país

#Para vincular nuestras bases de datos existen múltiples opciones, 
#la primera es ‘merge’ de R base y las siguinetes tres vienen desde dplyr: ‘right_join’, ‘full_join’ y ‘left_join’.

# merge #

data <- merge(proc_data, context_data, by="idenpa") #data tiene 12 observaciones y 5 variables. 
#En el fondo lo que hace es vincular solo aquellos casos que tienen datos disponibles para ambas bases originales

data1 <- right_join(proc_data, context_data, by="idenpa") #data1 tiene 64 observaciones y 5 variables. 
#Esta función lo que hace es mantener todos los valores de la base de la ‘derecha’ (los 64 originales) y añadir los de la ‘izquierda’ con la identificación que corresponde. 
#Para el resto de países que no tienen valores en la base del latinobarómetro, los agrega como NA

data2 <- left_join(proc_data, context_data, by="idenpa") #data2 tiene 18 observaciones y 5 variables. 
#Esta función es el proceso inverso de right_join. Mantiene todos los valores de la base de la ‘izquierda’ y los vincula con los de la ‘derecha’ cuando corresponde. 
#Para el resto de países de latinobarómetro que no tienen casos en WVS, los deja como NA en las nuevas varaibles

data3 <- full_join(proc_data, context_data, by="idenpa") #Finalmente data3 tiene 70 observaciones y 5 variables. 
#Esta función junta todo lo que pueda, es decir, mantiene todos los casos de latinobarómetro y de WVS, por eso si nos fijamos en el país que tiene por código 909, 
#esta función lo mantiene aunque no tenga valores en latinobarómetro ni en WVS.

#¿Cuál es la mejor opción para este caso?
#Vamos a quedarnos con la función original de R base ‘merge’ ya que para poder visualizar y representar resultados en un gráfico, 
#en este ejemplo solo nos interesa mostrar aquellos casos completos.

data <- data %>%
  mutate(idenpa = as.character(idenpa)) %>%
  mutate(idenpa = case_when(
    idenpa == "32" ~ "Argentina",
    idenpa == "68" ~ "Bolivia",
    idenpa == "76" ~ "Brasil",
    idenpa == "152" ~ "Chile",
    idenpa == "170" ~ "Colombia",
    idenpa == "188" ~ "Costa Rica",
    idenpa == "214" ~ "Cuba",
    idenpa == "218" ~ "República Dominicana",
    idenpa == "222" ~ "Ecuador",
    idenpa == "320" ~ "El Salvador",
    idenpa == "340" ~ "Guatemala",
    idenpa == "484" ~ "Honduras",
    idenpa == "558" ~ "México",
    idenpa == "591" ~ "Nicaragua",
    idenpa == "600" ~ "Panamá",
    idenpa == "604" ~ "Paraguay",
    idenpa == "858" ~ "Uruguay",
    idenpa == "862" ~ "Venezuela"))
data$gdp <- as.numeric(data$gdp)
data$gdp[data$gdp==0] <- NA
data <- na.omit(data)

data %>%
  ggplot(aes(x = gdp, y = promedio, label = idenpa)) +
  geom_point() +
  geom_text(vjust = -0.5) +
  labs(x = "GDP", y = "Promedio") +
  theme_bw()

































