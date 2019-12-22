# Esto es cómo hacer que el programa te pida algo para laburar

# my.name <- readline(prompt="Enter name: ")
# my.age <- readline(prompt="Enter age: ")
# # convert character into integer
# my.age <- as.integer(my.age)
# print(paste("Hi,", my.name, "next year you will be", my.age+1, "years old."))
#--------------------------------------------------------------------------------

# x <- seq(-10,10,0.25)
# 
# valormedio <- 1
# std1 <- 1
# 
# plot(x,dnorm(x,valormedio,std1),col = "red", type = "p",main = "Distribuciones normales comparadas")
# for(std2 in 2:5){
#   points(x,dnorm(x,valormedio,std2), col = "blue")
# }
# grid()

# Hagamos la prueba de desviaciones para el punto a del 7
#---------------------------------------------------------------------------
x <- seq(-2,4,0.01)
desviacion <- 0.5

plot(x,dnorm(x,0,desviacion),col = "red", type = "l",xlab = "Juicio interno", ylab = "Probabilidad",main = "Distribuciones normales comparadas")
lines(x,dnorm(x,1.5,desviacion),col = "blue", lwd = 2)
grid()
legend("topleft", c("Ausente","Presente"), fill = c("red","blue"))