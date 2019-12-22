# Ejercicio 3. Acá voy a armar las funciones básicas que me calculan el d' y el c
# Donde d' me parametriza la capacidad del usuario de discriminar las señales y c me parametriza el sesgo

library(lestat) # Esto me carga la librería si está instalada
# Si no está instalada, usar install.packages("lestat")

# La funcion fun_basicsdt devuelve un vector

fun_basicsdt <- function(h,f){
  d <- invcdf(normal(),h)-invcdf(normal(),f)
  c <- -0.5*(invcdf(normal(),h)+invcdf(normal(),f))
  devol <- c(c,d)
  return(devol)
}

#---------------------------------------------------------------------------------------
# Resolvamos aca mismo el ejercicio 4 tambien

# Datos espiritista: h = 0.92, f = 0.48
# Datos esceptico: h = 0.58, f = 0.09

Espiritista <- fun_basicsdt(0.92,0.48)
Esceptico <- fun_basicsdt(0.58,0.09)

cat("El valor de d' del Espiritista es: ",Espiritista[2], "Y el valor de c es: ",Espiritista[1])
cat("El valor de d' del Esceptico es: ",Esceptico[2], "Y el valor de c es: ",Esceptico[1])