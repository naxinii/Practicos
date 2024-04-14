#Práctico 3: Código de Preparación

#El documento de código de preparación posee 5 partes, más una sección de identificación inicial:
  
#0. Identificación y descripción general: Título, autor(es), fecha, información breve sobre el contenido del documento
#1. Librerías: cargar librerías a utilizar
#2. Datos: carga de datos
#3.Selección de variables a utilizar
#4. Procesamiento de variables
#5. Generación de base de datos preparada para el análisis.

#Antecedentes de los datos a utilizar

#Latinobarómetro es un estudio de opinión pública que aplica anualmente alrededor de 20.000 entrevistas 
#en 18 países de América Latina representando a más de 600 millones de habitantes

#El presente ejercicio tiene por objetivo el procesar los datos para obtener las variables relevantes
#para el estudio de la Confianza en instituciones políticas, entendida como el grado en que los individuos 
#confian en distintas instituciones políticas a nivel nacional, como el gobierno, la justicia, los partidos 
#políticos, etc. Para ello, junto con variables de confianza, consideraremos también variables de estatus (educación), 
#y variables de caracterización sociodemográfica (sexo y edad).

#Código de Preparación - Latinobarómetro 2020

#1. Librerías principales (de R) a utilizar en el análisis

install.packages("pacman")
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

#2. Cargar base de datos

rm(list=ls())       
options(scipen=999) 

load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))

dim(latinobarometro2020)
View(latinobarometro2020)

save(latinobarometro2020,file = "C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Practicos/PracticoIPO/input/data-orig/latinobarometro2020.RData")


#3. Selección de variables a utilizar

#Este paso consiste en crear un subset reducido de datos que contenga 
#solo las variables de interés. Para ello:

#1. Se identifica el nombre de las variables que registran la información de 
#preguntas o items del instrumento: esto aparece en el libro de códigos y/o en el 
#cuestionario, o también se puede hacer buscando en la base de datos mediante alguna palabra clave asociada a la pregunta. 
#Por ejemplo, si queremos buscar variables asociadas a educación, utilizamos la función 
#find_var (de sjmisc, librería que cargamos en el paso 1), que nos entrega nombre de la variable en columna var.name. 

#Ej. Variables asociadas al concepto "Confianza":

find_var(data = latinobarometro2020,"Confianza")

#Nos informa que hay una serie de variables relacionadas con confianza interpersonal y con instituciones. 

#Ej. Probemos con la variable p13st_e

#Mediante la función select de dplyr, seleccionamos cada una de nuestras variables de interés y creamos una 
#nueva base con el nombre proc_data, donde “proc” hace referencia a base procesada:

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

#Mediante el comando get_label obtenemos el atributo label de las variables.

sjlabelled::get_label(proc_data)

#Podemos ver que son largas o con códigos poco informativos, por lo tanto, es necesario cambiarlas por etiquetas más cortas y de fácil identificación.

#Para facilitar el análisis, vamos a filtrar la base de datos para quedarnos solo con los casos de Chile. 
#Para esto utilizamos la función filter de dplyr. Si revisamos el libro de códigos, el identificador de Chile es 152

proc_data <- proc_data %>% dplyr::filter(idenpa==152)

#4. Procesamiento de variables: se seguirá el siguiente flujo de trabajo:
 #a. Descriptivo básico
 #b. Recodificación: datos perdidos y valores (en caso de ser necesario)
 #c. Etiquetamiento: de variable y valores (en caso de ser necesario)
 #d. Otros ajustes

#Y se recomienda también un descriptivo final para revisar que el procesamiento de cada variable está ok.

#En Latinobarómetro, lass variables que permiten medir la Confianza en instituciones políticas en Chile son las siguientes:
  
 #[p13st_e]: “P13ST.E Confianza en el Gobierno” (1 = Mucha; 4 = Ninguna)
 #[p13st_d]: “P13ST.D Confianza en el Congreso” (1 = Mucha; 4 = Ninguna)
 #[p13st_f]: “P13ST.F Confianza en el Poder Judicial” (1 = Mucha; 4 = Ninguna)
 #[p13st_g]: “P13ST.G Confianza en los Partidos Políticos” (1 = Mucha; 4 = Ninguna)

#4.1 Confianza en el Gobierno

 #a. Descriptivo: Para los descriptivos se utilizará la función frq, de la librería sjmisc:

frq(proc_data$p13st_e)

#En esta variable vemos valores asociados a la opción 
#“No contesta” (-2) y “No sabe” (-1), que corresponde definirlos como casos perdidos (en el caso de R, como casos NA). 
#El resto de los valores y etiquetas se encuentran en un orden contraintuitivo (mayor valor indica menos confianza), 
#así que en la recodificiación nos haremos cargo de los casos perdidos y de reordenar las categorías.

 #b. Recodificación: Para recodificar utilizamos la función recode, de la librería car

proc_data$p13st_e <- recode(proc_data$p13st_e, "c(-2,-1)=NA")
proc_data$p13st_d <- recode(proc_data$p13st_d, "c(-2,-1)=NA")
proc_data$p13st_f <- recode(proc_data$p13st_f, "c(-2,-1)=NA")
proc_data$p13st_g <- recode(proc_data$p13st_g, "c(-2,-1)=NA")

#NOTA: con la función set_na de la librería sjmisc podemos recodificar toda la base de datos con un solo código, 
#pero debemos estar completamente segur-s de que estos valores no tienen otra categoría asociada en otra variable.

proc_data <- proc_data %>% set_na(., na = c(-2, -1))

#Para reordenar las categorías volvemos a utilizar la función recode, de la librería car

proc_data$p13st_e <- recode(proc_data$p13st_e, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_d <- recode(proc_data$p13st_d, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_f <- recode(proc_data$p13st_f, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_g <- recode(proc_data$p13st_g, "1=3; 2=2; 3=1; 4=0")

#c. Etiquetado: Vamos a dar un nombre más sustantivo a las variables con la función rename, de la librería dplyr:

proc_data <- proc_data %>% rename("conf_gob"=p13st_e, # Confianza en el gobierno
                                  "conf_cong"=p13st_d, # Confianza en el congreso
                                  "conf_jud"=p13st_f, # Confianza en el Poder Judicial
                                  "conf_partpol"=p13st_g) # Confianza en los partidos políticos 

#Además de cambiar el nombre, queremos cambiar las etiquetas de las variables.

proc_data$conf_gob <- set_label(x = proc_data$conf_gob,label = "Confianza: Gobierno")
get_label(proc_data$conf_gob)

proc_data$conf_cong  <- set_label(x = proc_data$conf_cong, label = "Confianza: Congreso")
get_label(proc_data$conf_cong)

proc_data$conf_jud  <- set_label(x = proc_data$conf_jud, label = "Confianza: Poder judicial")
get_label(proc_data$conf_jud)

proc_data$conf_partpol  <- set_label(x = proc_data$conf_partpol, label = "Confianza: Partidos politicos")
get_label(proc_data$conf_partpol)

#d. Otros ajustes

#Para este caso vamos a crear una variable que sea la suma de los cuatro items de confianza.

proc_data$conf_inst <- (proc_data$conf_gob+proc_data$conf_cong+proc_data$conf_jud+proc_data$conf_partpol)
summary(proc_data$conf_inst)

get_label(proc_data$conf_inst)

#Vemos que una etiqueta de la variable anterior.

proc_data$conf_inst  <- set_label(x = proc_data$conf_inst, label = "Confianza en instituciones")

#Revisión final: Nuevamente un descriptivo de cada variable para confirmar que el procesamiento está ok:

frq(proc_data$conf_gob)

frq(proc_data$conf_cong)

frq(proc_data$conf_inst)

#Vemos que los valores (labels) de cada categoría de las primeras variables que recodificamos no se corresponden con el nuevo valor. 1
#Para re-etiquetar valores usamos la función set_labels, de la librería sjlabelled

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

frq(proc_data$conf_gob)

frq(proc_data$conf_cong)

#4.2. Educación
 #[reeduc_1] = REEDUC.1 Nivel de estudios alcanzado - Entrevistado (recodificado)

#a. Descriptivo

frq(proc_data$reeduc_1)

#b. Recodificación

 #- Vemos que no hay datos perdidos
 #- Valores

#Para hacer más fácil el análisis, recodificamos en tres categorías (en este caso decisión arbitraria. 
#Se debería tener una razón teórica para recodificar)

 #1.  Analfabeto                                =   Educacion basica    =   1
 #2   Básica incompleta                         =   Educacion basica    =   1
 #3.  Básica completa                           =   Educacion basica    =   1
 #4.  Secundaria, media, técnica incompleta     =   Educacion media     =   2
 #5.  Secundaria, media, técnica completa       =   Educacion media     =   2
 #6.  Superior incompleta                       =   Educacion superior  =   3
 #7.  Superior completa                         =   Educacion superior  =   3

#Recodificacion usando funcion 'recode' de la libreria car

proc_data$reeduc_1 <- car::recode(proc_data$reeduc_1, "c(1,2,3)=1; c(4,5)=2; c(6,7)=3")

#Comprobar

frq(proc_data$reeduc_1)

#Se observa que los valores coinciden con la recodificación (los casos se acumulan entre las categorías 1 y 3), 
#pero las etiquetas ahora no coinciden; se soluciona en el siguiente paso.

#c. Etiquetado

#Para re-etiquetar valores usamos la función factor, de R base. 
#Con esta función aprovechamos de transformar la variable educación en una variable categórica, que es lo que corresponde para una variable ordinal.

proc_data$reeduc_1 <- factor(proc_data$reeduc_1,
                             labels = c("Educacion basica", "Educacion media", "Educacion superior"),
                             levels = c(1, 2, 3))

#renombramos la variable con un nombre más sustantivo

proc_data <- rename(proc_data,"educacion"=reeduc_1)

#Además de cambiar el nombre, queremos cambiar la etiqueta de la variable.

get_label(proc_data$educacion)

proc_data$educacion <- set_label(x = proc_data$educacion,label = "Educación")

#4.3. Sexo
#[sexo] = SEXO Sexo

#a. Descriptivo

frq(proc_data$sexo)

#b. Recodificación
#En general esta variable no tiene problemas de casos perdidos ni de etiquetas, 
#pero de todas maneras vamos a hacer un cambio de acuerdo a convenciones en análisis de datos, 
#donde por lo general hombres tienen valor 0 y mujeres 1:

proc_data$sexo <- car::recode(proc_data$sexo, "1=0;2=1")

#c. Etiquetado
#cambiamos las etiquetas de acuerdo a la recodificación anterior

proc_data$sexo <- factor(proc_data$sexo,
                         labels=c( "Hombre",
                                   "Mujer"),
                         levels=c(0,1))

#cambiar la etiqueta de la variable.

get_label(proc_data$sexo)

proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")

#Revisar

frq(proc_data$sexo)

#4.4 Edad
#[edad] = EDAD Edad

#a. Descriptivo

frq(proc_data$edad)

#b. Recodificación:  no es necesario en este caso

#c. Etiquetado
#Cambio la etiqueta de la variable.

get_label(proc_data$edad)

proc_data$edad <- set_label(x = proc_data$edad,label = "Edad")

#5. Generación de base de datos procesada para el análisis

#Antes de guardar la base procesada, revisamos nuevamente todas las variables con 
#una tabla descriptiva general mediante la función stargazer (de la librería homónima)

#Primero vamos a reformatear el objeto proc_data como base de datos (as.data.frame)

proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

##Si se desea modificar las columnas que aparecen en la tabla se puede ocupar la opción summary.stat, donde se pueden especificar:
  
#“max” maximum
#“mean” mean
#“median” median
#“min” minimum
#“n” number of observations
#“p25” 25th percentile
#“p75” 75th percentile
#“sd” standard deviation

## Por ejemplo, si quiero una tabla solo con promedio, n, sd y p75: stargazer(data, type="text", summary.stat = c("mean", "n", "sd", "p75"))

#Guardar base de datos procesada: en carpeta local > C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Practicos/PracticoIPO/input/data-proc

save(proc_data,file = "C:/Users/nachi/OneDrive/Documentos/Universidad/UAH 5° Semestre/OFC R para análisis estadístico/Practicos/PracticoIPO/input/data-proc/latinobarometro_proc.RData")

##Descriptivos básicos de las variables
#Podemos conocer ciertas medidas de tendencia central utilizando algunas funciones de dplyr

#Media por grupos

proc_data %>% dplyr::group_by(sexo) %>% summarise(mean(conf_inst, na.rm=TRUE))

proc_data %>% dplyr::group_by(educacion) %>% summarise(mean(conf_inst, na.rm=TRUE))

#Representación

library(sjPlot)

sjt.xtab(proc_data$educacion, proc_data$conf_inst, encoding = "UTF-8")













































































































































