#regresion lineal simple
#Suposiciones del modelo
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

plot(x,y)
#aplicación de la formula

#media de X y Y
medx= mean(x)
medy= mean(y)
#Nominador
x_medx= x-medx
y_medy= y-medy
#producto
prod=x_medx*y_medy
#sumatoria
s1=sum(prod)
#denominador
s2= sum (x_medx^2)
b1=s1/s2
b0= medy-b1*medx

#estimacion. formula para predecir
yest=b0+b1*x#x=poblacion estamidada

#Error o residual
error = y-yest
#SUMATORIA DE CUADRADOS DEVIDO AL EEROROR
SCE= sum(error^2)

#SUMATORIA TOTAL DE CUADRADOS

STC=sum(y_medy^2)
#suma de cuadrados debido a la regresion
SCR= STC-SCE

#cOEFICIENTE DE DETERMINACION, VALOR DE LA VONDAD DE AJUSTE ENTRE 0 Y 1
#DONDE 0 es donde la bondad de ajuste no predice nada y 1 es que no existe error
#>0.7, es un buen modelo

#+1 perfectamete relacional positiva
#-1 perfectamente relacionadas negativa
#0 Sin relación 

r2=SCR/STC

#coeficiente de correlacion muestral

rxy=sign(b1)*sqrt(r2)

#CALCULO DE N
n=length(x)
#ERROR CUADRATICO MEDIO
ECM= SCE/(n-2)
#error estandar de estimacion
s= sqrt(ECM)



#regresion
reg=lm(y~x)
abline(reg) #agregr linea 
summary(reg)

#deviation estandar
sb1= s/sqrt(s2)

#prueba t
t= b1/sb1
CMR=SCR/1

#prueba F
F=CMR/ECM


#Suposiciones del modelo

plot(x.error)
