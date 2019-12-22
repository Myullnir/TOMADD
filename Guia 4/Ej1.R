# Ejercicio 1. Acá voy a armar las funciones básicas que me calculan el d' y el c
# Donde d' me parametriza la capacidad del usuario de discriminar las señales y c me parametriza el sesgo

rm(list=ls()) # Esto debería ser como un clear all 

library(lestat) # Esto me carga la librería si está instalada
# Si no está instalada, usar install.packages("lestat")

# La funcion fun_basicsdt devuelve un vector

fun_basicsdt <- function(h,f){
  d <- invcdf(normal(),h)-invcdf(normal(),f)
  c <- -0.5*(invcdf(normal(),h)+invcdf(normal(),f))
  devol <- c(c,d)
  return(devol)
}

#Importacion de datos---------------------------------------------------------------------------------------
# Ahora voy a importar los datos de R

# llevar a R para que trabaje en la ubicación en la que están los datos. Por ejemplo
setwd("~/Laboratorio/TOMADD/Guía 4")
# Podés buscar la carpeta de manera manual, y después copias la dirección desde
# la consola

# importar los datos que están en 'data.csv'
datos <- read.csv(file="datos_guia4.csv", header=TRUE, sep=",")
# Imagino que header es para mantener los encabezados

# visualizar los datos
# View(datos)
#Ranking--------------------------------------------------------------------------
library(dplyr) # Biblioteca copada para trabajar con datos

# Ranking de usuarios
u.nombre <- unique(datos$nombre)
N <- length(u.nombre)
ranking <- data.frame(u.nombre, Pc = rep(NA,N))

# Armé mi dataframe, pero necesito poner la información del porcentaje
# de respuestas correctas para cada usuario.
for(i in 1:N){
  estimulo <- filter(datos,datos[,1]==u.nombre[i])$stim
  respuesta <- filter(datos,datos[,1]==u.nombre[i])$resp
  Ntrials <- length(estimulo)
  coincidencias <- 0
  for(j in 1:Ntrials) if(estimulo[j]==respuesta[j]) coincidencias <- coincidencias+1
  ranking$Pc[i] <- coincidencias/Ntrials
}

# Esto me los ordena según Pc
ranking <- arrange(ranking, desc(ranking$Pc)) # desc() es para que
# me lo ordene de forma descendente
#Punto C------------------------------------------------------------------
# Ahora para el punto c empezamos haciendo un histograma 
# de los tiempos de respuesta según el nivel de coherencia.

# guardo los niveles de coherencia utilizados en el experimento.
u.coherencia <- sort( unique(datos$coherencia) )

# para cada nivel de coherencia, hago un histograma de los tiempos de respuesta.
d1 <- filter(datos, datos[,2]==u.coherencia[1])
plot(density(d1$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(1,0,0), 
     lwd=3, main='distribuciones de RT', xlab="RT (s)")
grid()

d2 <- filter(datos, datos[,2]==u.coherencia[2])
lines(density(d2$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(0,1,0), lwd=3)

d3 <- filter(datos, datos[,2]==u.coherencia[3])
lines(density(d3$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(0,0,1), lwd=3)

d4 <- filter(datos, datos[,2]==u.coherencia[4])
lines(density(d4$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(1,1,0), lwd=3)

d5 <- filter(datos, datos[,2]==u.coherencia[5])
lines(density(d5$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(1,0,1), lwd=3)

legend(3, 1.5, legend = c(u.coherencia[1],u.coherencia[2],u.coherencia[3],u.coherencia[4],u.coherencia[5]), lwd=3, col=c(rgb(1,0,0),rgb(0,1,0),rgb(0,0,1),rgb(1,1,0),rgb(1,0,1)))

# Ahora quiero graficar los tiempos de respuesta en función de la coherencia

all.rt <- rep(NA,length(u.coherencia))

for(i in 1:length(u.coherencia)){
  all.rt[i] <- mean(filter(datos,datos[,2]==u.coherencia[i])$rt)
}

plot(u.coherencia,all.rt, col = "red", type = "p", xlab = "Coherencia", ylab = "RT (s)", main = "RT vs Coherencia")
grid()

# Ahora quiero graficar el porcentaje de respuestas correctas en función de la coherencia

all.pc <- rep(NA,length(u.coherencia))

for(i in 1:length(u.coherencia)){
  estimulo <- filter(datos, datos[,2]==u.coherencia[i])$stim
  respuesta <- filter(datos, datos[,2]==u.coherencia[i])$resp
  Ntrials <- length(estimulo)
  coincidencias <- 0
  for(j in 1:Ntrials) if(estimulo[j]==respuesta[j]) coincidencias <- coincidencias+1
  all.pc[i] <- (coincidencias/Ntrials)*100
}

plot(u.coherencia,all.pc, col = "blue", type = "p", xlab = "Coherencia", ylab = "Pc", main = "Pc vs Coherencia")
grid()

#Punto D ---------------------------------------------------------
# Este lo copio directo de lo que hizo el profesor

# respuestas correctas
dc <- filter(datos, stim==resp )
plot(density(dc$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(1,0,0), lwd=3, 
     main='distribuciones de RT', xlab="RT (s)")
grid()

# errores
di <- filter(datos, stim!=resp )
lines(density(di$rt), xlim=c(0,4), ylim=c(0,2), col=rgb(0,0,1), lwd=3)

legend("topright", legend=c('rtas. correctas','rtas. incorrectas'), 
       lwd=3, col = c('red','blue'))