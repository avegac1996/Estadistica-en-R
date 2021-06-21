# x1 ---> Variable Independiente 
# y ---> Variable Dependiente 
x1 = c(9,11,16,21,27,30)
y  = c(1.66, 1.12,0.83,0.62,0.51,0.47)

reg = lm(y~x1)#Regresion estimada que relaciona y con x1
b0 = reg$coefficients[1] #Coeficente b0
b1 = reg$coefficients[2] #Coeficente b1

yest= b0+b1*(x1) #Ecuacion de Regresion Estimada
plot(x1,y,pch=19,col="green",type="overplotted") # Grafica 

x2=x1^2
reg2 = lm(y~x1+x2)#Regresion estimada que relaciona y con x1,x2

summary(reg2)

b00 = reg2$coefficients[1] #Coeficente b0
b11 = reg2$coefficients[2] #Coeficente b1
b22 = reg2$coefficients[3] #Coeficente b1
yest2= b00+b11*(x1)+b22*(x2) #Ecuacion de Regresion Estimada

print (b00)
print (b11)
print (b22)