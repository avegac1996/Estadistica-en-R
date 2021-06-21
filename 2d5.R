#-----------------a--------------------
x = c(9,32,18,15,26) 
y = c(10,20,21,16,22)

reg = lm(y~x) #Regresion estimada que relaciona y con x
summary(reg) 
0.11719 <=0.05
#------------------b----------------
x1 = x^2
reg2 = lm(y~x+x1)
summary(reg2)
b00 = reg2$coefficients[1]#Coeficente b0
b11 = reg2$coefficients[2]#Coeficente b1
b22 = reg2$coefficients[3]#Coeficente b2

yest1 = b00+b11*x+b22*x1 
#---------------------c--------------
yest1 = b00+b11*x+b22*x1 
yest1 =-8.10139 +  2.41271*(20) - 0.04797*(20)^2
print (yest1)
