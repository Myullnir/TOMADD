# Ejercicio 1. Simular tirada de dados

caras <- 1:6 # Estas son los posibles resultados del dado

# Quiero hacer N tiradas de dados, sumarlas todas y guardarlo

set.seed(1) # Seteo la semilla inicialmente
N <- readline(prompt = "¿Cuantas tiradas queres realizar?: ")

sumar_dados <- function(N){
  tiradas <- sample(caras,N,replace = TRUE)
  return(sum(tiradas))
}

# Ya tengo la funcion que me hace N tiradas y me las suma
# Ahora tengo que guardarlo en un vector

output <- rep(NA,10000) # Le ponemos NA para que ayude a distinguir errores

for(i in 1:length(output)){
  output[i] <- sumar_dados(N)
}

# Ahora lo plotteo. Porque sino no hay forma de ver esto

X <- 1:length(output)

plot(output~X)