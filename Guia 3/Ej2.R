# Voy a hacer el ejercicio 2 de la guía 3 acá

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
#------------------------------------------------------------------------------

# El participante de este ejercicio tiene h = 0.826 y f = 0.182
# Calculo d', c y log(beta) como pide el punto a

parti <- fun_basicsdt(0.826,0.182)
cat("El valor de d' es: ",parti[1],"El de c es: ", parti[2],"El de log(beta) es: ",parti[3])

#--------------------------------------------------------------------------

# Ahora hago el punto b, que quiere graficar las distribuciones y el criterio, para ver si tengo una buena
# idea de cómo se ve todo. Yo creo que si

# Datos del gráfico

muS <- parti[1]
muN <- 0
sigma <- 1 #Venimos poniendo 1 porque así el d' es la distancia entre picos
criterio <- parti[2]+parti[1]/2

# Vector X e Y

x <- seq(-3,5,length.out = 300)
yN <- dnorm(x,muN,sigma)
yS <- dnorm(x,muS,sigma)

plot(x,yS,type = "l", lwd = 3, col="red", xlab = "respuesta interna", ylab = "Probabilidad",main = "Distribucion de señal y ruido")
lines(x,yN, lwd = 3, col="blue")
abline(v = criterio, lwd=3)
grid()
legend(+3, 0.3, legend = c('ruido','señal'), col = c("red", "blue"), lty = 1,  lwd = 3)

#-----------------------------------------------------------------------------------

# Ahora voy a hacer el punto c

critopt <- function(muS,muN,sigma,Pn,Ps,criterios){
  Pc <- rep(NA,length(criterios))
  
  # Tengo que definir mi número de samples, cuantos ruidos y señales, labels, y demás
  ntrialsN <- as.integer(Pn*10000)
  ntrialsS <- as.integer(Ps*10000)
  Nmediciones <- ntrialsN+ntrialsS
  
  labelsS <- rep(1,ntrialsS)
  labelsN <- rep(0,ntrialsN)
  
  samplesS <- rnorm(ntrialsS,muS,sigma)
  samplesN <- rnorm(ntrialsN,muN,sigma)
  
  alllabels <- append(labelsS,labelsN)
  allsamples <- append(samplesS,samplesN)
  
  for(i in 1:length(criterios)){
    isHit <- 0
    isFA <- 0
    decision <- rep(NA,length(allsamples))
    for(j in 1:length(allsamples)){
      if(allsamples[j]<criterios[i]) decision[j] <- 0
      else decision[j] <- 1
    }
    for(j in 1:length(decision)){
      if(decision[j]==1 && alllabels[j]==1) isHit <- isHit+1
      if(decision[j]==1 && alllabels[j]==0) isFA <- isFA+1
    }
    Pc[i] <- ((isHit+ntrialsN-isFA)/(Nmediciones))*100
  }
  Pmax <- max(Pc)
  iPmax <- match(max(Pc),Pc)
  Critmax <- criterios[iPmax]
  devol <- c(Critmax,Pmax)
  return(devol)
} 

# Datos iniciales

Pn = 400/1000
Ps = 600/1000
criterios <- seq(muN-3*sigma,muS+3*sigma,length.out = 5000)

# Calculo el criterio optimo

CPOptimo <- critopt(muS,muN,sigma,Pn,Ps,criterios)

cat("El valor del criterio optimo es: ",CPOptimo[1],"El valor del porcentaje optimo es: ",CPOptimo[2])

# Ahora vamos a comparar con los datos previos que tenía

nuevoCB <- c(CPOptimo-(parti[1]/2),pnorm(CPOptimo[1],muS,sigma)/pnorm(CPOptimo[1],muN,sigma))

cat("La diferencia entre el criterio del participante y el ideal es: ",parti[2]-nuevoCB[1])
#------------------------------------------------------------------------------------------

# Ahora tengo que comparar los valores hallados con los optimos a traves de 
# calcular Beta usando Pn y Ps

tercerBe <- Pn/Ps
finalc <- log(tercerBe)/parti[1]

cat("El c final es: ",finalc,"El c inicial es: ",parti[2], "El c de criterio optimo es: ",nuevoCB[1])

# Para que sepas, el c de criterio optimo cambia mucho, habría que tomarle un promedio. Pero para eso, se te muere antes
# R por el tiempo de cálculo que te requiere. Te queremos C, te perdonamos todo
