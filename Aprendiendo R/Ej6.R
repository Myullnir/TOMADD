# Ejercicio 6. Hacer un ajuste por Rectas. Acá está la plata.

# Primero, graficar el conjunto de datos

X <- c(-5.0, -3.9, -2.8, -1.7, -0.6, 0.6, 1.7, 2.8, 3.9, 5.0)
Y <- c(9.1, 5.5, 11.2, 6.7, 1.3, 5.7, 1.0, -4.4, -4.0, -11.0)

plot(X,Y,col = "red", type = "l",ylab = "Datos Y",xlab = "Datos X",main = "Conjunto de datos")

# Segundo, armar una tabla de valores a y b para la recta de ajuste
# y=ax+b

# Los valores de a los voy a tomar entre la pendiente del primer 
# y segundo punto y la pendiente del primer y último punto
# Los valores de b los voy a tomar para el rango entre el punto
# a izquierda del cero y el punto a derecha del cero

ai <- (Y[2]-Y[1])/(X[2]-X[1])
af <- (Y[length(Y)]-Y[1])/(X[length(X)]-X[1])

for(l in 1:(length(X)-1)){
  if(X[l]*X[l+1]<0){
    bi <- Y[l]
    bf <- Y[l+1]
  }
}

Param <- expand.grid(a = seq(ai,af,(af-ai)/100),b = seq(bi,bf,(bf-bi)/100))

# Tercero, tengo que calcular el error cuadrático para cada par (a,b)
# Hagámoslo como me enseñó Frank. Sos grande Guillermo

# Esto calcula el error cuadrático para una serie de datos X, Y
# Ajustados por una recta con pendiente "a" y ordenada al origen "b"
Error2 <- function(Y,X,a,b){
  XX <- matrix(c(X,rep(1,length(X))),nrow = length(X),ncol = 2, byrow = FALSE)
  param <- c(a,b)
  error <- Y-XX%*%param
  cuadrat <- 0
  for(l in 1:length(error)){
    cuadrat <- cuadrat+error[l]*error[l]
  }
  cuadrat <- sqrt(cuadrat)
  return(cuadrat)
}

# Por ahora no tiró error, comparémoslo al final a ver que tal
# Ahora sí, calculemos el error cuadrático de cada uno de estos valores

dimparam <- dim(Param)
Errcuadrat <- c(rep(NA,dimparam[1,1]*dimparam[1,2]))

for(fila in 1:dimparam[1,1]){
  for(columna in 1:dimparam[1,2]){
    Errcuadrat[(fila-1)*2+columna] <- Error2(Y,X,Param[fila,columna],Param[fila,columna])
  }
}

Tablaerrcuad <- matrix(Errcuadrat,nrow = length(unique(Param[,1])),ncol = length(unique(Param[,2])),byrow = TRUE)
