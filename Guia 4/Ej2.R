# Ejercicio 2.
#Bases general--------------------------------------------------------
# Acá voy a armar las funciones básicas que me calculan el d' y el c
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
#Punto A--------------------------------------------------------------
# Defino los datos iniciales
n_pasos <- 100
drift   <- 0.0
umbral  <- 10
x_ini   <- umbral/2

x <- rep(NA,n_pasos)
x[1] <- x_ini
i <- 1

while(i<n_pasos & 0<x[i] & x[i]<umbral){
  x[i+1] <- x[i]+rnorm(1)
  i <- i+1
}

x[i] <- ifelse(x[i] > umbral, umbral, 0)

for(j in i:n_pasos) x[j] <- x[i]

plot(x, type = 'l', xlim = c(1,n_pasos), ylim = c(-1,umbral+1), xlab = "Pasos", ylab = "Decisión", lwd=3, col = "blue")
grid()
abline(umbral, 0, lwd=3)
abline(0, 0, lwd=3)