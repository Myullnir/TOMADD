# Ejercicio 3. Ejercicios sobre el escrutinio de votos para dos candidatos.

# Primero: Armar un vector donde se asignan los votos a cada candidato.
# Defino los votos del candidato A =+1 y los de B = -1

va <- 600
vb <- 400
Nrep <- 10000


# Segundo: Mirar la votación y analizar si A siempre estuvo al frente
# Tercero: Repetir el proceso 10000 veces y ver cuántas veces A estuvo al frente

Conteo <- function(Nrep,va,vb){
  Votos <- c(rep(1,va),rep(-1,vb))
  n_favorables <- Nrep
  for(rep in 1:Nrep){
    Sumatoria <- 0
    Votacion <- sample(Votos,va+vb,replace=FALSE)
    for(n in 1:length(Votacion)){
      Sumatoria <- Sumatoria+Votacion[n]
      if(Sumatoria<=0){
        #print("A ya no está a la cabeza")
        n_favorables <- n_favorables-1
        break
      }
    }
  }
  return(n_favorables/Nrep)
}

print(Conteo(Nrep,va,vb))

# Cuarto: Supongamos que ahora 505 personas votaron por A, 495 por B, la pregunta es,
# ¿Cuál es la probabilidad de que A siga a la cabeza hasta haber contado el voto 800?
# Resolver esto es modificar va y vb al principio, y en el segundo for, ponerle como tope 800
# en vez de length(Votacion)
# Ah pará, también quiere la probabilidad de que vaya ganando B hasta el voto 800

va <- 505
vb <- 495

# Veamos la cosa para B

ConteoB <- function(Nrep,va,vb){
  Votos <- c(rep(1,va),rep(-1,vb))
  n_favorables <- Nrep
  for(rep in 1:Nrep){
    Sumatoria <- 0
    Votacion <- sample(Votos,va+vb,replace=FALSE)
    for(n in 1:800){
      Sumatoria <- Sumatoria+Votacion[n]
      if(Sumatoria>=0){
        #print("B ya no está a la cabeza")
        n_favorables <- n_favorables-1
        break
      }
    }
  }
  return(n_favorables/Nrep)
}

ConteoA <- function(Nrep,va,vb){
  Votos <- c(rep(1,va),rep(-1,vb))
  n_favorables <- Nrep
  for(rep in 1:Nrep){
    Sumatoria <- 0
    Votacion <- sample(Votos,va+vb,replace=FALSE)
    for(n in 1:800){
      Sumatoria <- Sumatoria+Votacion[n]
      if(Sumatoria<=0){
        #print("A ya no está a la cabeza")
        n_favorables <- n_favorables-1
        break
      }
    }
  }
  return(n_favorables/Nrep)
}

pA <- ConteoA(Nrep,va,vb)
pB <- ConteoB(Nrep,va,vb)

cat("La fracción de veces que A se mantuvo a la cabeza es: ",pA)
cat("La fracción de veces que B se mantuvo a la cabeza es: ",pB)
cat("La suma de las dos fracciones es: ",pA+pB)
