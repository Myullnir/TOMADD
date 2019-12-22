# Voy a hacer acá el ejercicio 7 de la guia 2, porque me olvide de subir al git las cosas que hice

# Función que calcula d' y c

library(lestat) # Esto me carga la librería si está instalada
# Si no está instalada, usar install.packages("lestat")

# La funcion fun_basicsdt devuelve un vector

fun_basicsdt <- function(h,f){
  d <- invcdf(normal(),h)-invcdf(normal(),f)
  c <- -0.5*(invcdf(normal(),h)+invcdf(normal(),f))
  devol <- c(d,c)
  return(devol)
}

#------------------------------------------------------------------------
# Voy a fijar los parámetros
dp <-1.5
criterio <-dp/2 # Esto es el criterio, no el sesgo. Va en la mitad de las distribuciones
SigmaS <- 1 # No sé, lo tomo porque pintó. Es desviación señal+ruido
SigmaN <- 1 # No sé, lo tomo porque pintó. Es desviación ruido
muS <- dp # Es el valor medio señal+ruido
muN<- 0 # Es el valor medio ruido. No pierdo generalidad por setearlo a cero

#----------------------------------------------------------------------
# Voy a usar la funcion rnorm para simular los trials
# Quiero generar 500 muestras de la distribución señal y 500 de ruido
# ntrialsS <- 500
# ntrialsN <- 500
# 
# # Voy a registrar en un vector cuáles son los estímulos con señal y cuáles con ruido
# signalLabels <- rep(1,ntrialsS)
# noiseLabels  <- rep(0,ntrialsN)
# 
# # Tomo las muestras de cada distribución
# signalSamples <- rnorm(ntrialsS,muS,SigmaS)
# noiseSamples <- rnorm(ntrialsN,muN,SigmaN)
# 
# # Ahora junto los dos vectores, poniendo primero las señales
# allsamples <- append(signalSamples,noiseSamples)
# alllabels <- append(signalLabels,noiseLabels)
# 
# # Ahora voy a hacer el juicio del sujeto
# # El juicio del sujeto lo tengo que comparar con el criterio
# 
# decision <- rep(NA,length(allsamples))
# 
# for(i in 1:length(allsamples)){
#   if(allsamples[i]<criterio){
#     decision[i] <- 0
#   }
#   else decision[i] <- 1
# }
# 
# # Ahora revisamos si el sujeto respondió bien o mal
# 
# isCorrect <- 0
# 
# for(i in 1:length(decision)){
#   if(decision[i]==alllabels[i]) isCorrect <- isCorrect+1
# }
# 
# percentCorrect <- (isCorrect/length(decision))*100
# 
# # Voy a calcular ahora la cantidad de hits y de falsas alarmas
# 
# isHit <- 0
# isFA <- 0
# 
# for(i in 1:length(decision)){
#   if(decision[i]==1 && alllabels[i]==1) isHit <- isHit+1
#   if(decision[i]==1 && alllabels[i]==0) isFA <- isFA+1
# }
# 
# h <- isHit/length(signalSamples)
# f <- isFA/length(noiseSamples)
# 
# # Ahora con los valores de h y f obtenidos, calculo el d' y el c del sistema
# 
# Calculados <- fun_basicsdt(h,f)

# cat("La diferencia entre el d' inicial y el calculado es: ",dp-Calculados[1])
# cat("La diferencia entre el c inicial y el calculado es: ",-Calculados[2])
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------

# Ahora voy a hacer una prueba para ver cómo varía la diferencia entre d' y c entre
# valores iniciales y calculados a medida que aumento la cantidad de mediciones

Nmediciones <- seq(100,100000,100)
diferenciaDP <- rep(NA,length(Nmediciones))
diferenciaC <- rep(NA,length(Nmediciones))
for(N in Nmediciones){
  ntrialsS <- N/2
  ntrialsN <- N/2

  # Voy a registrar en un vector cuáles son los estímulos con señal y cuáles con ruido
  signalLabels <- rep(1,ntrialsS)
  noiseLabels  <- rep(0,ntrialsN)

  # Tomo las muestras de cada distribución
  signalSamples <- rnorm(ntrialsS,muS,SigmaS)
  noiseSamples <- rnorm(ntrialsN,muN,SigmaN)

  # Ahora junto los dos vectores, poniendo primero las señales
  allsamples <- append(signalSamples,noiseSamples)
  alllabels <- append(signalLabels,noiseLabels)

  # Ahora voy a hacer el juicio del sujeto
  # El juicio del sujeto lo tengo que comparar con el criterio

  decision <- rep(NA,length(allsamples))

  for(i in 1:length(allsamples)){
    if(allsamples[i]<criterio){
      decision[i] <- 0
    }
    else decision[i] <- 1
  }

  # Ahora revisamos si el sujeto respondió bien o mal

  isCorrect <- 0

  for(i in 1:length(decision)){
    if(decision[i]==alllabels[i]) isCorrect <- isCorrect+1
  }

  percentCorrect <- (isCorrect/length(decision))*100

  # Voy a calcular ahora la cantidad de hits y de falsas alarmas

  isHit <- 0
  isFA <- 0

  for(i in 1:length(decision)){
    if(decision[i]==1 && alllabels[i]==1) isHit <- isHit+1
    if(decision[i]==1 && alllabels[i]==0) isFA <- isFA+1
  }

  h <- isHit/length(signalSamples)
  f <- isFA/length(noiseSamples)

  # Ahora con los valores de h y f obtenidos, calculo el d' y el c del sistema

  Calculados <- fun_basicsdt(h,f)
  diferenciaDP[((N-100)/100)+1] <- dp-Calculados[1]
  diferenciaC[((N-100)/100)+1] <- -Calculados[2]
}

# Cosa para mejorar en el futuro, hace un gráfico logarítmico

plot(Nmediciones,diferenciaDP,col = "red", type = "p",xlab = "Cantidad de Mediciones", ylab = "Diferencia",main = "Diferencia entre parámetros calculados y datos iniciales")
points(Nmediciones,diferenciaC,col = "blue")
grid()
legend("topleft", c("dprima","C"), fill = c("red","blue"))