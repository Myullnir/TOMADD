# Ejercicio 5. Regla de la mayoría

# Vamos a considerar un grupo de N personas, con dos posibles opiniones. +1 y -1
# Inicialmente hay una cantidad n1<N tal que tiene el estado inicial 1
# En cada iteración, las personas interactuarán de a 3, y al final de la interacción, la opinión
# de los tres será la de la mayoría. Si había 2 con +1 y 1 con -1, los tres tendrán 1

set.seed(1)

# Primero: Crear el vector Op con el estado de todas las personas en el grupo
# Voy a empezar con 600 personas

N <- 500
n1 <- 400

Op <- c(rep(1,n1),rep(-1,N-n1))

# Segundo: Hacer una ronda de interación, buscar la opinión mayoritaria y luego modificar la opinión
# Hagamos de esto una función

Interaccion <- function(Op){
  Participantes <- sample(1:length(Op),3,replace = FALSE)
  Mayoria <- Op[Participantes[1]]+Op[Participantes[2]]+Op[Participantes[3]]
  if(Mayoria>0){
    for(p in Participantes){
      Op[p] <- 1
    }
  }
  if(Mayoria<0){
    for(p in Participantes){
      Op[p] <- -1
    }
  }
  return(Op)
}

# Tercero: Repetir el proceso hasta llegar a un consenso

while(length(unique(Op))!=1){
  Op <- Interaccion(Op)
}

cat("El valor del consenso es: ",unique(Op))