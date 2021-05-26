
#regresion lineal simple
#metodo de los minimo cuadrados(Gauss)

#restaurante
#Restaurante i
#1,2,3,4,5,6,7,8,9,10
#Poblacion de estudiantes(Miles)
#2,6,8,8,12,16,20,20,22,26
#ventas trimestrales(Miles de $)
#58,105,88,118,117,137,157,169,149,202
#INGRESO DE DATOS X
x=c(2,6,8,8,12,16,20,20,22,26)
#INGRESO DE DATOS y
y=c(58,105,88,118,117,137,157,169,149,202)

plot(x,y)#diagrama de dispersion
#aplicación de la formula

#funciones directas
#lm linear modeling 
reg=lm(y~x)
abline(reg)#recta en la grafica
summary(reg)

#corelacion
cor(x,y)
 #y estimado
yest2=reg$coefficients[1]+reg$coefficients[2]*x
#reg$coefficients