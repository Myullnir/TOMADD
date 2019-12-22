# Acá voy a hacer el ejercicio 1 de la guía 3
# Tengo un participante con d' = 1,5. Quiero estudiar el porcentaje de sus 
# respuestas correctas a medida que varío el criterio desde el valor medio
# menos tres desviaciones de ruido hasta el valor medio más tres desviaciones de señal

# Defino primero los parametros

dp <- 1.5
muS <- 1.5
muN <- 0
SIGMA <- 1 # Le pongo 1 para que dp sea equivalente a la distancia entre picos

# Ahora me defino la cantidad de señales y el vector que me marca las señales y los ruidos
Nmediciones <- 1000

ntrialsS <- Nmediciones/2
ntrialsN <- Nmediciones/2

signalLabels <- rep(1,ntrialsS)
noiseLabels <-rep(0,ntrialsN)

signalSamples <- rnorm(ntrialsS,muS,SIGMA)
noiseSamples <- rnorm(ntrialsN,muN,SIGMA)

# Ahora defino el vector que tiene todos los registros de si fue señal o ruido
# y todos los registros de los trials. También pongo en ambos casos primero la señal, después el ruido

alllabels <- append(signalLabels,noiseLabels)
allsamples<- append(signalSamples,noiseSamples)

# Ahora voy tengo que definir mi lista de los criterios que voy a usar

criterios <- seq(muN-3*SIGMA,muS+3*SIGMA,(muS+3*SIGMA-(muN-3*SIGMA))/5000)

# Ahora si voy a hacer las mediciones
Pc <- rep(NA,length(criterios))

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

plot(criterios,Pc,col = "red", type = "l",xlab = "Criterio", ylab = "Porcentaje de respuestas correctas",main = "Pc vs Criterio")
grid()

# Hice solo el punto a, el b y c los dejé de lado. Voy tarde, ya fue