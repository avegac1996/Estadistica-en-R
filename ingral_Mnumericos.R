#integrar mediante metodos numericos

#declaro una funcion
i1=function(x){1/9*x^3}
#declaro los limtes, inferior y superior
r1= integrate(i1,lower=0,upper = 3)

#declaro una funcion
i2=function(x){1/9*(x-2.25)^2*x^2}
#declaro los limtes, inferior y superior
r2= integrate(i2,lower=0,upper = 3)


#Calculo de z
x=90
mu=200
sigma=50
#probabilidad
r1=pnorm(x,mu,sigma)

#estadistico
p2=qnorm(0.0139)