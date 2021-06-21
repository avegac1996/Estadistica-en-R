# x1 ---> Variable Independiente 
# y ---> Variable Dependiente 
x1 = c(167,159,126,108,153,123,118,123,119)
y = c(86,82,81,81,81,81,80,80,80)

plot(x1,y,pch=19,col="blue",type=) # Grafica 

# x1 ---> Variable Independiente 
# y ---> Variable Dependiente 

x1 = c(167,159,126,108,153,123,118,123,119)
y = c(86,82,81,81,81,81,80,80,80)

x2=x1^2
reg = lm(y~x1+x2)#Regresion estimada que relaciona y con x1,x2

summary(reg)

b00 = reg$coefficients[1] #Coeficente b0
b11 = reg$coefficients[2] #Coeficente b1
b22 = reg$coefficients[3] #Coeficente b1
yest2= b00+b11*(x1)+b22*(x2) #Ecuacion de Regresion Estimada

print(b00)
print(b11)
print(b22)
