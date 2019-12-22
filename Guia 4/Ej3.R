# Ejercicio 3.
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
# Si no recuerdo mal, el = hace que si no le ponés nada, toma eso por default
rw_decisiones <- function(n_pasos=2000, x_ini=0, drift=0, sd_rw=1, umbral=10){ 
  # Defino el x en el cual voy a realizar mi caminata al azar
  x <- rep(NA,n_pasos)
  x[1] <- x_ini+umbral/2
  i <- 1
  # Empiezo a realizar mi caminata al azar, agregando pasos obtenidos de una distribución normal, con una media en el drift 
  # y desviación dada por el sd_rw
  while(i<n_pasos & 0<x[i] & x[i]<umbral){
    x[i+1] <- x[i]+rnorm(1,drift,sd_rw)
    i <- i+1
  }
  # Con esto me aseguro que una vez llegado al umbral, ya sea el superior o el inferior, todos los valores después de este
  # sean el valor obtenido del umbral correspondiente
  x[i] <- ifelse(x[i] > umbral, umbral, 0)
  for(j in i:n_pasos) x[j] <- x[i]
  # Ahora calculo el tiempo de respuesta y mi respuesta
  rt <- i # Fijate que la cantidad de pasos que te tomó llegar a la respuesta es i
  resp <- x[n_pasos] # La respuesta va a ser el último tipo, porque justamente yo sobreescribí todos los valores hasta el final
  # usando el valor del umbral al cual llegué, por eso el último seguro que tiene el valor del umbral alcanzado
  out <- c(rt,resp)
  return(out)
}
