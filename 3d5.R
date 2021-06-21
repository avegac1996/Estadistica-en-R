#----------a-------------
x = c(2,3,4,5,7,7,7,8,9) 
y = c(4,5,4,6,4,6,9,5,11)
reg = lm(y~x) #Regresion estimada que relaciona y con x
summary(reg) 
b0 = reg$coefficients[1] #Coeficente b0
b1 = reg$coefficients[2] #Coeficente b1
yest = b0+b1*x #Ecuacion de Regresion Estimada

plot(x,y,pch=19,col="green",type="overplotted") 
x = c(2,3,4,5,7,7,7,8,9) 
y = c(4,5,4,6,4,6,9,5,11)
reg = lm(y~x) #Regresion estimada que relaciona y con x
summary(reg)
#----------b---------------
x = c(2,3,4,5,7,7,7,8,9) 
y = c(4,5,4,6,4,6,9,5,11)
reg = lm(y~x) 
summary(reg) 
b0 = reg$coefficients[1] #Coeficente b0
b1 = reg$coefficients[2] #Coeficente b1
yest = b0+b1*x #Ecuacion 
print(b0)
print(b1)
#-----------c----------------
rstan = rstandard(reg) 
plot(yest,rstan)
#------------------d---------------
x = c(2,3,4,5,7,7,7,8,9)
y = c(4,5,4,6,4,6,9,5,11)
ylog = log(y) 
reg2 = lm (ylog~x)
summary(reg2) 
b00 = reg2$coefficients[1] 
b11 = reg2$coefficients[2] 
yest1 = exp(b00)+exp(b11)*x 



