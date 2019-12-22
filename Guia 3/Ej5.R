# Acá voy a hacer el ejercicio 5, que es el último de la guía 3

# Pongamos las cosas básicas que voy a necesitar

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
# Datos del enunciado: Tengo 6 condiciones, de la A a la F. Voy a numerarlas correspondientemente del 1 al 6.
# Las columnas son: (ruido, si), (ruido, no), (Señal, si), (Señal, no)

Datos <- matrix(nrow = 6, ncol = 4)
Datos[1,] <- c(264,36,294,6)
Datos[2,] <- c(168,132,273,27)
Datos[3,] <- c(102,198,252,48)
Datos[4,] <- c(30,270,198,102)
Datos[5,] <- c(17,283,171,129)
Datos[6,] <- c(2,298,108,192)

##############################################################################
# Ahora, con esto quiero calcular el d' para cada una de las condiciones.
# Necesito h y f para cada condición

Dprimas <- rep(NA,dim(Datos)[1])
H <- rep(NA,dim(Datos)[1])
FA <- rep(NA,dim(Datos)[1])

for(i in 1:dim(Datos)[1]){
  h <- Datos[i,3]/sum(Datos[i,3:4])
  fa <- Datos[i,1]/sum(Datos[i,1:2])
  calculo <- fun_basicsdt(h,fa)
  Dprimas[i] <- calculo[1]
  H[i] <- invcdf(normal(),h)
  FA[i] <- invcdf(normal(),fa)
}

# Para el punto b voy a querer hacer la ROC en coordenadas z(hits). Es decir invcdf(normal(),h)
# Lo hago directamente en el for de arriba todo

X <- seq(-3,3,length.out = 100)

Ajuste <- lm(FA~H)

plot(FA,H,col = "blue", type = "p",xlab = "Fraccion de FA", ylab = "Fraccion de Hits",main = "ROC")
lines(X,X,col = "black")
grid()
legend("topleft", c("Datos"), fill = c("blue"))