# Acá voy a hacer el ejercicio 4 de la guía 3. El ej 3 lo hice en la carpeta

rm(list=ls()) # Esto debería ser como un clear all 

library(lestat) # Esto me carga la librería si está instalada
# Esta es la función que a partir de los hits y las fa te calcula el d' y el c
# La voy a actualizar para que también ahora te calcule el beta

fun_basicsdt <- function(h,f){
  d <- invcdf(normal(),h)-invcdf(normal(),f)
  c <- -0.5*(invcdf(normal(),h)+invcdf(normal(),f))
  beta <- 0.5*(invcdf(normal(),f)*invcdf(normal(),f)-invcdf(normal(),h)*invcdf(normal(),h))
  devol <- c(d,c,beta)
  return(devol)
}
#----------------------------------------------------------------------------
# Igual que en el ej 7, tengo que simular 10000 trials

# Voy a usar la funcion rnorm para simular los trials
# Quiero generar 5000 muestras de la distribución señal y 5000 de ruido
muS <- 2
muN <- 0
Sigma <- 1
Nmediciones <- 10000
ntrialsS <- Nmediciones/2
ntrialsN <- Nmediciones/2

 
# Voy a registrar en un vector cuáles son los estímulos con señal y cuáles con ruido
signalLabels <- rep(1,ntrialsS)
noiseLabels  <- rep(0,ntrialsN)

 
# Tomo las muestras de cada distribución
signalSamples <- rnorm(ntrialsS,muS,Sigma)
noiseSamples <- rnorm(ntrialsN,muN,Sigma)


# Ahora junto los dos vectores, poniendo primero las señales
allsamples <- append(signalSamples,noiseSamples)
alllabels <- append(signalLabels,noiseLabels)
# Hasta acá tengo todos los trials simulados, y tengo los labels que me marcan cuáles son ruido y cuales no
######################################################################################################

# Ahora tengo que tomar decisiones según criterios de confianza:
criterios <- c(-0.5,0.2,0.75,1.7,2.5)
# El criterio de la persona está centrado en 0.75, que es el medio de ambas distribuciones
# Es decir que no está sesgado. Las otras son las que determinan la confianza.

# Ahora voy a hacer el juicio del sujeto
# El juicio del sujeto lo tengo que comparar con el criterio
# Vamos a ordenar las decisiones en este orden: 
# 1 NO, ALTA CONFIANZA
# 2 NO, MEDIA CONFIANZA
# 3 NO, BAJA CONFIANZA
# 4 SI, BAJA CONFIANZA
# 5 SI, MEDIA CONFIANZA
# 6 SI, ALTA CONFIANZA

 
decision <- rep(NA,length(allsamples))

for(i in 1:length(allsamples)){
  ubicado <- FALSE
  for(j in 1:length(criterios)){
    if(ubicado==FALSE && allsamples[i]<criterios[j]){
      decision[i] <- j
      ubicado <- TRUE
    }
  }
  if(ubicado==FALSE) decision[i] <- 6
}
###########################################################################################################

# Hasta acá tengo las decisiones del usuario con su correspondiente confianza. Ahora tengo que anotar sus
# resultados en una tabla clasificando según confianza

Datos <- matrix(data = 0, nrow = 2, ncol = 6)

for(i in 1:length(decision)){
  Datos[alllabels[i]+1,decision[i]] <- Datos[alllabels[i]+1,decision[i]] + 1
}
# Bueno, eso fue fácil. Me alegro del sistema que tomé
# Acordate que el label de ruido es 0, el de seña es 1, por eso les sumas 1
# El código de las decisiones está más arriba
#------------------------------------------------------------------------------

# Ahora tengo que graficar la curva ROC empírica.
H <- rep(NA,6)
FA <- rep(NA,6)
# Estos son los ejes que voy a graficar
for(i in 6:1){
  H[7-i] <- sum(Datos[2,i:6])/ntrialsS
  FA[7-i] <- sum(Datos[1,i:6])/ntrialsN
}

# Ahora lo grafico y listo

X <- seq(0,1,length.out = 100)

plot(X,X,col = "black", type = "l",xlab = "Fraccion de FA", ylab = "Fraccion de Hits",main = "ROC empirica")
points(FA,H,col = "blue")
grid()
legend("topleft", c("Datos"), fill = c("blue"))