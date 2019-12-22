# Primer parcial de toma de Decisiones, Alumno Favio Di Ciocco

#Bases general--------------------------------------------------------
# Acá voy a armar las funciones básicas que me calculan el d' y el c
# Donde d' me parametriza la capacidad del usuario de discriminar las señales y c me parametriza el sesgo
# También voy a poner las librerías básicas que puedo llegar a necesitar

rm(list=ls()) # Esto debería ser como un clear all 
library(lestat) # Esto me carga la librería si está instalada
library(dplyr) # Biblioteca copada para trabajar con datos

# La funcion fun_basicsdt me calcula el valor de c, d' y el log(beta)
fun_basicsdt <- function(h,f){
  d <- invcdf(normal(),h)-invcdf(normal(),f)
  c <- -0.5*(invcdf(normal(),h)+invcdf(normal(),f))
  beta <- 0.5*(invcdf(normal(),f)*invcdf(normal(),f)-invcdf(normal(),h)*invcdf(normal(),h))
  devol <- c(d,c,beta)
  return(devol)
}

# La función rw_decisiones me simula una caminata al azar, y me devuelve el valor del umbral alcanzado y la cantidad de pasos que requirió para llegar
rw_decisiones <- function(n_pasos=2000, x_ini=0, drift=0, sd_rw=1, umbral=10){ 
  # Defino el x en el cual voy a realizar mi caminata al azar
  x <- rep(NA,n_pasos)
  x[1] <- x_ini
  i <- 1
  # Empiezo a realizar mi caminata al azar, agregando pasos obtenidos de una distribución normal, con una media en el drift 
  # y desviación dada por el sd_rw
  while(i<n_pasos & -umbral<x[i] & x[i]<umbral){
    x[i+1] <- x[i]+rnorm(1,drift,sd_rw)
    i <- i+1
  }
  # Con esto me aseguro que una vez llegado al umbral, ya sea el superior o el inferior, todos los valores después de este
  # sean el valor obtenido del umbral correspondiente
  x[i] <- ifelse(x[i] > umbral, umbral, -umbral)
  for(j in i:n_pasos) x[j] <- x[i]
  # Ahora calculo el tiempo de respuesta y mi respuesta
  rt <- i # Fijate que la cantidad de pasos que te tomó llegar a la respuesta es i
  resp <- x[n_pasos] # La respuesta va a ser el último tipo, porque justamente yo sobreescribí todos los valores hasta el final
  # usando el valor del umbral al cual llegué, por eso el último seguro que tiene el valor del umbral alcanzado
  out <- c(rt,resp)
  return(out)
}

## 1A-----------------------------------------------------------------------------------------------------------------------
# Ahora voy a importar los datos del csv a R
# llevar a R para que trabaje en la ubicación en la que están los datos. Por ejemplo
setwd("~/Documents/Toma de Decisiones/Parcial")

# importar los datos que están en 'data.csv'
datos <- read.csv(file="datos_exp.csv", header=TRUE, sep=",")

# visualizar los datos
# View(datos)

## 1B---------------------------------------------------------------------------------------------------------------------------
# Ahora vamos a calcular d',c,log(Beta) y Pc para cada participante
# d' y c salen directamente de la función fun_basicsdt

# Para empezar, por enunciado yo sé que la probabilidad de que el estímulo sea derecha es 40% y la de que sea izquierda por tanto es 60%.
Pd <- 0.4
Pi <- 0.6

# Armemos una lista de los participantes, y ordenémosla
participantes <- sort(unique(datos$id))
Np <- length(participantes) # Igual por enunciado esto vale 10

# Me armo una matríz para guardar los datos de cada participante
Resultados <- data.frame(participantes, dp = rep(NA,Np), c = rep(NA,Np), lbeta = rep(NA,Np), Pc = rep(NA,Np))

# Armo un for para calcular iterativamente para cada participante
for(i in 1:length(participantes)){
  datos2 <- filter(datos,id==participantes[i])
  h <- table(filter(datos2,stim==1)$resp)[["1"]]/length(filter(datos2,stim==1)$resp)
  # Para calcular el h, la fracción de hits, estoy mirando de la cantidad de veces que hubo señal, cuantas respondio que sí había señal
  # Es decir, cuantas veces ocurrió que stim=1 y resp=1
  f <- table(filter(datos2,stim==0)$resp)[["1"]]/length(filter(datos2,stim==0)$resp)
  # f es la fracción de falsas alarmas, es decir, estoy mirando los casos en que stim=0 y resp=1
  rc <- table(filter(datos2,stim==0)$resp)[["0"]]/length(filter(datos2,stim==0)$resp)
  # El rc es la fracción de rechazos correctos, es decir la fracción de veces que stim=0 y resp=0
  
  # Vale aclarar que al hablar de hits y falsas alarmas, estoy de alguna manera considerando que los casos en que el estímulo
  # es a la izquierda, son mi ruido, y los casos en que el estímulo es a la derecha, son mi señal
  
  # Ahora, con h, f y rc, puedo calcular d', c, log(beta) y pc
  parametros <- fun_basicsdt(h,f)
  Resultados$dp[i] <- parametros[1]
  Resultados$c[i] <- parametros[2]
  Resultados$lbeta[i] <-parametros[3]
  Resultados$Pc[i] <- Pd*h+Pi*rc
}

# La tabla resultados contiene los valores de d', c, log(beta) y Pc para todos los participantes

## 1C---------------------------------------------------------------------------------------------------------------------

# Me armo dos rankings según d' y según Pc

Rankingdp <- arrange(Resultados, desc(Resultados$dp))$participantes
RankingPc <- arrange(Resultados, desc(Resultados$Pc))$participantes

# La razón de que sea preferible utilizar el d' en vez del Pc es porque el Pc tiene mezclado el sesgo del participante.
# En cambio, el d' me habla específicamente de la separación entre las distribuciones de "señales" y "ruido"
# es decir que únicamente toma en cuenta la capacidad del individuo de discriminar entre ambas señales. Por eso es que es un mejor
# criterio justamente para medir la capacidad del individuo de discriminar las señales.

## 1D----------------------------------------------------------------------------------------------------------------------
# Ahora voy a querer hacer un gráfico de la distribución interna del participante con d' más alto.

x <- seq(-6,6,by=0.1)

Dizquierda <- dnorm(x,0,1)
Dderecha <- dnorm(x,filter(Resultados,participantes==Rankingdp[1])$dp,1) 
# Acá estoy centrando la distribución de "señal", (es decir la de que el círculo de la derecha tiene más puntos), en el d' del 
# participante con mayor d'.

# Necesito calcular la ubicación del criterio óptimo también. Para eso voy a usar que log(beta) = c*d'
# Despejo c de esta ecuación. Y ya que c es el criterio menos d'/2, le sumo d'2 al valor de c para obtener el criterio

critopt <- (filter(Resultados,participantes==Rankingdp[1])$dp)/2 + (filter(Resultados,participantes==Rankingdp[1])$lbeta)/(filter(Resultados,participantes==Rankingdp[1])$dp)

plot(x,Dderecha,col = "blue", type = "l", ylab = "Distribución", main = "Modelo de detección para el participante con mayor d")
grid()
lines(x,Dizquierda,col = "red")
abline(v = critopt, col = "black")
legend("topleft",legend =  c("Izquierda","Derecha"), fill = c("red","blue"))

cat("El criterio óptimo para el participante con el d' más alto se ubica en un valor de: ",critopt)

## 1E------------------------------------------------------------------------------------------------------------------------
# Ahora hago lo mismo que en el punto D, pero para el participante con menor d'

x <- seq(-6,6,by=0.1)

Dizquierda <- dnorm(x,0,1)
Dderecha <- dnorm(x,filter(Resultados,participantes==Rankingdp[Np])$dp,1) 
# Acá estoy centrando la distribución de "señal", (es decir la de que el círculo de la derecha tiene más puntos), en el d' del 
# participante con mayor d'.

# Necesito calcular la ubicación del criterio óptimo también. Para eso voy a usar que log(beta) = c*d'
# Despejo c de esta ecuación. Y ya que c es el criterio menos d'/2, le sumo d'2 al valor de c para obtener el criterio

critopt <- (filter(Resultados,participantes==Rankingdp[Np])$dp)/2 + (filter(Resultados,participantes==Rankingdp[Np])$lbeta)/(filter(Resultados,participantes==Rankingdp[Np])$dp)

plot(x,Dderecha,col = "blue", type = "l", ylab = "Distribución", main = "Modelo de detección para el participante con mayor d")
grid()
lines(x,Dizquierda,col = "red")
abline(v = critopt, col = "black")
legend("topleft",legend =  c("Izquierda","Derecha"), fill = c("red","blue"))

cat("El criterio óptimo para el participante con el d' más bajo se ubica en un valor de: ",critopt)

# El criterio óptimo no se ubica en el mismo lugar para ambos participantes. Una respuesta mirando las ecuaciones puede ser simplemente
# porque el criterio óptimo depende de d' y c, y como el d' y c es distinto en ambos, entonces no deberían ser iguales.
# Una respuesta considerando los gráficos en cambio sería que al cambiar la superposición en los gráficos, es decir cambiar el d', 
# el área debajo de la curva que queda a la derecha del criterio cambia, por tanto siguiendo nuestra definición de que el criterio
# óptimo es el que minimiza las falsas alarmas y maximiza los hits, el criterio deberá cambiar de lugar para poder volver a maximizar
# ambos parámetros

## 2A---------------------------------------------------------------------------------------------------------------------------------------
# Para el modelo del experimento propuesto, voy a querer hacer 5000 simulaciones para estudiar el porcentaje de aciertos
# Donde se entiende que el porcentaje de aciertos es la cantidad de veces que la simulación de caminata aleatoria alcanza el valor umbral de 10

Ntrials <- 5000
Pc <- 0

for (i in 1:Ntrials) {
  Caminata <- rw_decisiones(n_pasos = 10000, drift = 0.03)
  if(Caminata[2]==10) Pc <- Pc+1
}
Pc <- (Pc/Ntrials)*100

cat("El porcentaje de aciertos es: ", Pc,"%")

## 2B----------------------------------------------------------------------------------------------------------------------------------------
# Ahora voy a graficar la distribución de tiempos de respuesta para aciertos y errores. Así que primero voy a necesitar
# Armarme una tabla donde guarde mis resultados

Ntrials <- 5000

Resultados <- data.frame(Ntrial = 1:Ntrials, RT = rep(NA,Ntrials), resp = rep(NA,Ntrials))

for (i in 1:Ntrials) {
  Caminata <- rw_decisiones(n_pasos = 10000, drift = 0.03)
  Resultados$RT[i] <- Caminata[1]
  Resultados$resp[i] <- Caminata[2]
}

# Ahora que ya tengo las respuestas y sus tiempos de respuesta, puedo graficarlo directamente

plot(density(filter(Resultados,resp==10)$RT),col = "blue",lwd=3, main='Distribuciones de RT', xlab="RT (s)")
grid()
lines(density(filter(Resultados,resp==-10)$RT), col = "red", lwd = 3)
legend("topright",legend =  c("Errores","Aciertos"), fill = c("red","blue"))

# Se puede ver en el gráfico los tiempos de respuesta para los errores y aciertos son similares en este modelo,
# cosa que no es lo que se observa en las mediciones

## 2C---------------------------------------------------------------------------------------------------------------------------------------
# Ahora voy a probar el caso en que mi drift sea variable, para ver si con eso puedo corregir el problema que tiene el modelo para
# reproducir los tiempos de respuesta de los errores.

Ntrials <- 5000

Resultados <- data.frame(Ntrial = 1:Ntrials, RT = rep(NA,Ntrials), resp = rep(NA,Ntrials))

for (i in 1:Ntrials) {
  driftexp <- rnorm(1,0.3,0.2)
  Caminata <- rw_decisiones(n_pasos = 10000, drift = driftexp)
  Resultados$RT[i] <- Caminata[1]
  Resultados$resp[i] <- Caminata[2]
}

# Ahora que ya tengo las respuestas y sus tiempos de respuesta, puedo graficarlo directamente

plot(density(filter(Resultados,resp==10)$RT),col = "blue",lwd=3, main='Distribuciones de RT, drift variable', xlab="RT (s)")
grid()
lines(density(filter(Resultados,resp==-10)$RT), col = "red", lwd = 3)
legend("topright",legend =  c("Errores","Aciertos"), fill = c("red","blue"))

# En este caso se lograron diferenciar las distribuciones, quedando la distribución de errores corrida un poco más a la derecha y con
# una amplitud mayor y un pico menos pronunciado. Este modelo describe un poco mejor que el anterior los tiempos de respuesta de los sujetos.

## 3-------------------------------------------------------------------------------------------------------------------------------------------

# Lo que está sucediendo en el caso del estudio sobre la interpretación de la figura ambigua que puede ser un conejo o un pato, es que 
# la percepción que la gente tiene respecto del dibujo (el estado del mundo para ellos), está muy influenciada por el prior que ellos portan.
# Lo que estaría sucediendo es que en el Domingo de Pascua, la gente tiene expectativa de ver conejos en todas partes, claramente apareado
# con el hecho de que en esa época festiva están bombeardeados por imágenes comerciales de conejos. Esto genera una información previa
# que predomina a la hora de determinar la interpretación que harán de la realidad, es decir del dibujo.
# En cambio en domingos cualquiera, al no estar tan presente la imagen del conejo de Páscuas en la mente de la gente, el prior que porta la gente,
# es decir sus expectativas ante los estados del mundo, reducen esa tendencia a asociar cosas con conejos.

## 4A--------------------------------------------------------------------------------------------------------------------------------
# Ahora lo que quiero es reproducir el gráfico y luego agregar la distribución posterior. Para eso primero armo
# los likelihoods auditivos y visuales.

# Defino x, mi vector de puntos con los que voy a evaluar mis funciones
x <- seq(-3,12,by = 0.05)

# Ahora armo mis likelihoods
LikelihoodA <- dnorm(x,5,1.5)
LikelihoodV <- dnorm(x,1,1)

# Dadas mis distribuciones de Likelihood, calculo mi posterior
Posterior <- (LikelihoodA*LikelihoodV)/sum(LikelihoodA*LikelihoodV)
Posterior <- Posterior/0.05

# Como el Prior es uniforme, puedo no considerarlo, ya que en definitiva aparecería tanto en el enunciado como en el denominador
# Y se cancelaría.

# Ya tengo todas mis distribuciones, ahora voy a graficarlas

plot(x,LikelihoodA, col = "blue", lwd=3, type = "l", ylim=c(0,0.5), main='Distribuciones de Likelihood y Posterior')
grid()
lines(x,LikelihoodV, col = "red", lwd = 3)
lines(x,Posterior, col = "black", lwd = 3)
legend("topright",legend =  c("LikelihoodA","LikelihoodV","Posterior"), fill = c("blue","red","black"))

## 4B-------------------------------------------------------------------------------------------------------------------------------
# Para poder descubrir cuál es la respuesta del participante según la estimación MAP, necesito descubrir dónde se halla el máximo
# de mi distribución posterior.

cat("La respuesta del participante según la estimación MAP será: ",x[which.max(Posterior)])

## 4C--------------------------------------------------------------------------------------------------------------------------------

# El valor medio de la distribución del posterior no se encuentra en 3, que es el punto medio entre las medias de las distribuciones de
# Likelihood, sino que se encuentra en 2,25. La razón de que el valor medio de la distribución posterior esté corrido hacia la izquierda
# del valor medio entre los Likelihoods es que la varianza es distinta para cada Likelihood. Esto representa una certeza mayor en uno de
# los dos, y por tanto el Posterior tiene su media más cercana a la distribución con menor varianza. Por esto es que el Posterior se encuentra
# más cercano a la distribución del Likelihood Visual que a la Auditiva.
