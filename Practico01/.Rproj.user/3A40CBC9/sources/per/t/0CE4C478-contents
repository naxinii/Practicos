#Práctico 1. Aproximación a R

10 + 5 # ¿cuánto es 10 + 5?

10 * 5 # ¿cuánto es 10 * 5?

a <- 28
b <- 8

a + b

c <- a + b

sum(28,8)

round(10.14536) #aproximar

#Librerías

install.packages("pacman")

pacman::p_load(dplyr, guaguas, ggplot2)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

base <- guaguas

dim(base)

names(base)

head(base)

table(base$sexo)

filter(base, nombre=="Kevin")

d <- filter(base, nombre=="Kevin" & anio==1996)
sum(d$n)
datos <- filter(base, nombre=="Kevin")
graph <- ggplot(datos, aes(x = anio, y = n)) +
  geom_line() + 
  labs(x = "Año", y = "Número de personas", title = "Número de personas llamadas Kevin por año")
ggsave(graph, file="files/img/graph.png")

graph1 <- guaguas %>% 
  filter(nombre %in% c("Salvador", "Augusto"), anio >= 1960 & anio <= 1979) %>% 
ggplot(aes(anio, n, color = nombre)) + 
  geom_line() +
  labs(x = "año", y = "total inscripciones", color = "nombre", 
       title = "Inscripciones de 'Salvador' y 'Augusto' entre 1960 - 1979")
ggsave(graph1, file="files/img/graph1.png")

i <- filter(base, nombre=="Ignacia" & anio==2002)
sum(i$n)
datos <- filter(base, nombre=="Ignacia")
graph2 <- ggplot(datos, aes(x = anio, y = n)) +
  geom_line() + 
  labs(x = "Año", y = "Número de personas", title = "Número de personas llamadas Ignacia por año")
ggsave(graph2, file="files/img/graph2.png")

