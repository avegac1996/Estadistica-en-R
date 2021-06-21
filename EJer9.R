
#########EJercicio 9##########
 
x = c(22,47,38,21,31,73,69,51,17,18,44,16,34,18,6,15,9)##Variable Independiente 
y = c(70,75,81,31,30,164,84,231,35,28,39,14,42,25,23,15,8)##Variable Dependiente 
# Grafica 
plot(x,y,pch=19,col="red") 

#Regresion estimada que relaciona y con x
reg = lm(y~x)
b0 = reg$coefficients[1]#Coeficente b0
b1 = reg$coefficients[2]#Coeficente b1
#Ecuacion de Regresion Estimada
yest = b0+b1*x 

r_estandar=rstandard(reg)
# Regresion estimada (Logaritmica) que relaciona y con x1,x2
log_y1 = lm(log10(y)~log10(x)) 
b0log = log_y1$coefficients[1]#Coeficente logaritmico b0
b1log = log_y1$coefficients[2]#Coeficente logaritmico b1

summary(log_y1)

#Ecuacion de Regresion Estimada Logaritmica
yestlog = b0log+b1log*x 
r_estlog =rstandard(log_y1)

ylog = log(y) #logaritmo de la variable dependiente 
reg2 =  lm (ylog~x) #Regresion estimada que relaciona el logaritmo de y con x

summary(reg2) #Summary de Regresion

b00 = reg2$coefficients[1] #Coeficente b0
b11 = reg2$coefficients[2] #Coeficente b1

yest1 = exp(b0)+exp(b1)*x #Ecuacion de Regresion Estimada  

rst =rstandard(reg2)


