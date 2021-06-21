# x1 ---> Variable Independiente
# y ---> Variable Dependiente
x1 = c(35,40,30,45,50 ,25)
y = c(1256, 1329, 1226, 1335, 1349, 1124)
reg = lm(y~x1) #Regresion estimada que relaciona y con x1
summary(reg) #Summary de Regresion

b0 = reg$coefficients[1] #Coeficente b0
b1 = reg$coefficients[2] #Coeficente b1
yest= b0+b1*(x1) #Ecuacion de Regresion Estimada
# Donde b0 y b1 es 
print(b0)
## (Intercept) 
##    943.0476
print(b1)
##       x1 
## 8.714286


