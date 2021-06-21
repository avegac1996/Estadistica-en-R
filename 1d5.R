x = c(22,24,26,30,35,40) 
y = c(12,21,33,35,40,36)
#----------------------a--------------------
reg = lm(y~x) 
summary(reg) 
b0 = reg$coefficients[1] 
b1 = reg$coefficients[2] 
yest = b0+b1*x 
#-----------------------------b------------------------
x = c(22,24,26,30,35,40) 
y = c(12,21,33,35,40,36)
reg = lm(y~x)
summary(reg) 
0.05893 < 0.05

#-------------------c-----------------------------
x = c(22,24,26,30,35,40) 
y = c(12,21,33,35,40,36)


b0 = reg$coefficients[1]
b1 = reg$coefficients[2]

yest = b0+b1*x 

plot(x,y,pch=19,col="blue",type="overplotted") # Grafica 
#-----------------------d-----------
# x Variable Independiente
# y  Variable Dependiente
x = c(22,24,26,30,35,40)
y = c(12,21,33,35,40,36)
# x1  Variable Independiente elevado al cuadrado
x1 = x^2
reg2 = lm(y~x+x1)
summary(reg2)
b0 = reg2$coefficients[1]
b1 = reg2$coefficients[2]
b2 = reg2$coefficients[3]
yest = b0+b1*x+b2*x1 #Ecuacion de Regresion Estimada
#----------------e-----------------
# x Variable Independiente
# y Variable Dependiente
x = c(22,24,26,30,35,40)
y = c(12,21,33,35,40,36)
# x1 ---> Variable Independiente elevado al cuadrado
x1 = x^2
reg2 = lm(y~x+x1)
summary(reg2)
0.01297 < 0.05
#_---------f-----------------------

x = c(22,24,26,30,35,40)
y = c(12,21,33,35,40,36)

x1 = x^2
reg2 = lm(y~x+x1)
b00 = reg2$coefficients[1]#Coeficente b0
b11 = reg2$coefficients[2]#Coeficente b1
b22 = reg2$coefficients[3]#Coeficente b2

yest1 = b00 + b11*(25) + b22*(25)^2

print(yest1)




