#### TOMA DE DECISIONES - 2do cuatrimestre 2019 ####
## Guillermo Solovey
## Instituto de Calculo (UBA)

#### EJERCICIO 1 ####

## 1A 
# Usando la función sample de R simule una tirada de un dado. 
sample(XXX, XXX, replace = XXX)

## 1B
# Escribir una función que simule N tiradas de un dado y devuelva la suma de los valores obtenidos.
# Ayuda: cada tirada del dado corresponde a elegir uno de los siguientes números con igual probabilidad:
# {1, 2, 3, 4, 5, 6}. Use el siguiente esquema como punto de partida.

# ----8<-------8<-------8<-------8<---
# fija la semilla del generador de numeros pseudo-aleatorios
set.seed(1)

# función que realiza la suma de los resultados de N dados
suma_dados <- function(N){
  todos     <- sample(c(1,2,3,4,5,6), XXX, replace = XXX)
  respuesta <- XXX
  return(XXX)
}

# evaluar la función 
suma_dados(XXX)
# ----8<-------8<-------8<-------8<---


## 1C
# Escribir un loop que repita el proceso anterior muchas veces, guardando el resultado en un vector.

# ----8<-------8<-------8<-------8<---
# Inicializar el vector que va a guardar los datos
output <- rep(NA, 10000)

# Repetimos el proceso suma_dados 10000 veces
for(i in 10000){
  # completa la 
  output[XXX] <- roll_dice(XXX)
}
# ----8<-------8<-------8<-------8<---



#### EJERCICIO 2 ####
## En un grupo de N personas, ¿cuál es la probabilidad de que al menos dos personas cumplan años el mismo día?
  
## 2A
# Simular las fechas de cumpleaños de N personas y guardarlas en un vector

# ----8<-------8<-------8<-------8<---
set.seed(1)
N <- 50
birthdays <- sample(XXX, N, replace = XXX)
# ----8<-------8<-------8<-------8<---

## 2B
# Evaluar si hubo o no una coincidencia 
length(unique(birthdays)) < n

## 2C
# Repetir el proceso anterior 10000 veces y contar todos los casos en los que hubo al menos 1 coincidencia

# ----8<-------8<-------8<-------8<---
# variable que va a contar las coincidencias
n_coincidencias <- 0

# simula 10000 grupos de N personas y verifica si hubo coincidencias o no
for(i in 1:10000){
  birthdays <- sample(seq(1,365,1), n, replace = TRUE)
  if(length(unique(birthdays)) < n){
    n_coincidencias <- XXX
  } 
}

# calcula la probabilidad estimada de coincidencias y la imprime en la consola
p_coincidencias <- n_coincidencias / 10000
print(n_coincidencias)
# ----8<-------8<-------8<-------8<---

## 2D
# La función pbirthday(N) de R calcula la probabilidad de coincidencia en un grupo de N personas. 
# Comparar el resultado anterior con el que se obtiene con la función de R


## 2E
# Qué sucede si en lugar de 10000 hacemos 100 simulaciones para estimar p. Hacer un grafico de p vs. repeticiones

## 2F
# Hacer un gráfico que muestre la probabilidad de coincidencia en función del número de personas en el grupo.
# Puede usar la función pbirthday() del ejercicio 2D.

# ----8<-------8<-------8<-------8<---
# Define el vector con los tamaños de los grupos.
N_vec <- XXX

# ejecuta la función para cada elemento del vector anterior
for (i in 1:length(Nvec)){
  p_c[XXX] <- pbirthday(XXX)
}

# una forma equivalente de hacer el loop anterior es usando la función sapply
p_c[XXX] <- sapply(XXX)

# crea el gráfico
plot(XXX ~ XXX)
# ----8<-------8<-------8<-------8<---





#### EJERCICIO 3 ####
# Si en una votación entre dos candidatos, n votantes votan por A y m votantes votan por B (con n>m) 
# la probabilidad de A le vaya ganando a b a lo largo de todo el escrutinio es (n-m)/(n+m)
# (la demostración es se puede ver acá https://twitter.com/pgroisma/status/1160524228510343170?s=20)

# ----8<-------8<-------8<-------8<---
n <- 500
m <- 400
# ----8<-------8<-------8<-------8<---

## 3A 
# crear un vector que contenga todos los votos, usando la notacion +1 para votos para el candidato A
# y -1 para el candidato B (por que es conveniente esto?)
votos <- c( XXX, XXX )

# 3B
# Un escrutinio es un posible orden en el que van apareciendo las boletas.
# realizar un escrutinio y evaluar si A se mantuvo al frente durante todo el escrutinio.
# ----8<-------8<-------8<-------8<---
escrutinio <- sample(XXX, n+m, replace = FALSE)
resultado  <- cumsum(XXX)
# <- evaluar si A se mantuvo siempre al frente (puede servir usar la funcion cumsum. Que hace?)
# ----8<-------8<-------8<-------8<---

## 3C
# repetir el proceso anterior 1000 veces y contar en cuantos escrutinios A se mantuvo 
# al frente en todo el escrutinio

# ----8<-------8<-------8<-------8<---
cuenta <- 0
Nrep   <- 1000
for (i in 1:Nrep){
  
  escrutinio <- sample(votos, n+m, replace = FALSE)
  resultado  <- cumsum(escrutinio)
  if ( min(resultado) > 0 ){
    XXX
  }
  
}

p <- XXX
print(p)
print((n-m)/(n+m))
# ----8<-------8<-------8<-------8<---



#### EJERCICIO 4 ####
# Tiras una moneda 30 veces y obtenes 22 caras. Si no estuviera cargada, que probabilidad tendrias de 
# obtener 22 o mas caras tirando 30 veces la moneda? Esta cargada la moneda?


#### EJERCICIO 5 ####
# Monty Hall. Un participante de un concurso tiene que elegir entre tres puertas. Detrás de una de ellas 
# hay un premio. Al elegir una puerta, el conductor del concurso le señala cual de las otras dos puertas 
# seguro no tiene el premio. El participante tiene la opción de quedarse con su opción inicial o cambiar 
# a la otra puerta. ¿Qué le conviene?
# Calcule con una simulacion la probabilidad de exito si el participante elige quedarse o si decide cambiar.