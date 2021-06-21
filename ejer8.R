
#########EJercicio 8##########
x1=c(18,19,18,19,16,19,18,17,17,15,14,16,17,16,13)#Variable Independiente 
y=c(1600,4000,1000,1300,350,2650,375,450,140,77.5,62,125,400,250,70)#Variable Dependiente 
# Grafica 
plot(x1,y,pch=19,col="red") 


#Regresion estimada que relaciona y con x1,x2
x2=x1^2
reg2 = lm(y~x1+x2)
summary(reg2)

b00 = reg2$coefficients[1] #Coeficente b0
b11 = reg2$coefficients[2] #Coeficente b1
b22 = reg2$coefficients[3] #Coeficente b1
yest2= b00+b11*(x1)+b22*(x2) #Ecuacion de Regresion Estimada
##Dónde b0, b1 y b2 es:
print (b00)
print (b11)
print (b22)
##yest2=33828.5-4570.609x+153.555x^2


# Regresion estimada (Logaritmica) que relaciona y con x1,x2
log_y1 = lm(log10(y)~log10(x1)) 
b0log = log_y1$coefficients[1]#Coeficente logaritmico b0
b1log = log_y1$coefficients[2]#Coeficente logaritmico b1

summary(log_y1)
#Ecuacion de Regresion Estimada Logaritmica
yestlog = exp(b0log)+exp(b1log)*x1 

