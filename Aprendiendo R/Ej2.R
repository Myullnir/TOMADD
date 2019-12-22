# Ejercicio 2. Calcular la probabilidad de que dos personas de un grupo cumplan en el mismo día

# set.seed(1) # Total, por ahora no hay necesidad de que los cálculos vayan cambiando
# N <- readline(prompt = "¿Cuantas personas hay en el grupo?: ")
# cumpleaños <- sample(1:365,N,replace = TRUE) # Esto me arma la lista de cumpleaños de los N integrantes del grupo
# 
# # Voy a usar la función unique para remover elementos repetidos del vector
# 
# # length(unique(cumpleaños))< length(cumpleaños) ;;; Si esto se cumple, entonces había repetidos. La diferencia entre ambos
# # es la cantidad de repetidos.
# 
# Nrep <- 10000 # Voy a simular esto 10000 veces
# n_coincidencias <- 0
# 
# for(i in 1:Nrep){
#   cumpleaños <- sample(1:365,N,replace = TRUE) # Esto me arma la lista de cumpleaños de los N integrantes del grupo
#   if(length(unique(cumpleaños))< length(cumpleaños)) n_coincidencias <- n_coincidencias+1
# }
# 
# cat("La fracción de veces en que hubo cumpleaños repetidos es: ",n_coincidencias/Nrep)

#-------------------------------------------------------------------------------------------------
# Voy a armar la función pcumples. Recibe N y Nrep, y me devuelve la probabilidad de que haya coincidencias

pcumples <- function(N,Nrep){
  n_coincidencias <- 0
  for(i in 1:Nrep){
    cumpleaños <- sample(1:365,N,replace = TRUE) # Esto me arma la lista de cumpleaños de los N integrantes del grupo
    if(length(unique(cumpleaños))< length(cumpleaños)) n_coincidencias <- n_coincidencias+1
  }
  proba <- n_coincidencias/Nrep
  return(proba)
}

#-------------------------------------------------------------------------------------------
# Voy a armar un gráfico para comparar pcumples con pbirthday

Nrep <- 50000
Resesp <- rep(NA,100)
Resing <- rep(NA,100)
X <- 1:80

for(N in X){
  Resesp[N] <- pcumples(N,Nrep)
  Resing[N] <- pbirthday(N)
}

plot(X,Resesp,col = "red", type = "l",ylab = "Probabilidad",xlab = "Cantidad de personas",main = "Gráfico de comparación entre cumples y birthdays")
points(X,Resing,col = "blue")
grid()
legend("topleft", c("Cumples","Birthdays"), fill = c("red","blue"))
