####################################################
##########    The Wall Street Journal    ########### 
####################################################

#Datos del Ejercicio (p???gina 604 ejercicio 54)
x = c(32977.4,162365.1,31363.8,56849.0,68848.0,507216.8,44180.1,194455.9,143131.0,35377.5,31062.1,92923.7,54421.2,144152.9,116840.8,62259.4,120966.5,30040.7,36450.8,61288.1) #vector de capitalizaci???n de mercado
y = c(1130,1400,800,1350,1000,3325,978,2000,1365,950,700,1275,1625,1318.3,773,1200,116,950,897,750) #vector de salario del CEO
i = c(1:length(x))  #vector de datos

#Calculo la media de x e y (ver diapositiva 47)
xmed = mean(x) #media (valor esperado) de x (formula 1)
ymed = mean(y) #media (valor esperado) de y (formula 1)

#Calculo de la ecuacion de regresion estimada 
x_xmed = x-xmed #vector poblaci???n menos su media (formula 2)  
y_ymed = y-ymed #vector ventas menos su media (formula 3)
prod = x_xmed*y_ymed #producto entre x_xmed y y_ymed (formula 4)
cuad = x_xmed^2 #x_xmed elevado al cuadrado (formula 5)
s1 = sum(prod) #sumatorio del producto entre x_xmed y y_ymed (numerador formula 6)
s2 = sum(cuad) #sumatorio de x_xmed elevado al cuadrado (denominador formula 6)
b1 = s1/s2 #calculo de b1 (formula 6) 
b0 = ymed-b1*xmed #calculo de b0 (formula 7) 
y_est = b0+b1*x #ecuacion de regresion estimada 

#Graficos

par(mfrow=c(3,3)) # varios gr???ficos en una sola pantalla
plot(x,y, pch = 19, col="red",main="Regresi???n Lineal (Manual): X vs. Y") #grafico de dispersion
lines(x,y_est,type="l",col="green") #grafico de la recta de regresion (uso comando "lines" en vez de "plot" para graficar el diagrama de dispersion y la recta en el mismo grafico)

#Coeficiente de determinacion
error = y-y_est #vector ventas menos vector de ventas estimadas
SCE = sum(error^2) #suma de cuadrados debido al error SCE
desv = y-ymed #desviacion del vector ventas menos vector de ventas media
STC = sum(desv^2) #suma total de cuadrados STC
SCR = STC-SCE #suma de cuadrados debidos a la refresion SCR
r2 = SCR/STC #coeficiente de determinaci???n
sig = sign(b1) #signo de b1
rxy = sig*sqrt(r2) #coeficiente de correlacion muestral

#################################################################
########################### T Test ##############################

n = length (i) #numero de muestras (longitud del vector i)
gl = n-2 #grados de libertad
ECM = SCE/gl #error cuadrado medio (estimaci???n de la varianza)
s = sqrt(ECM) #raiz de la varianza
sb1 = s/sqrt(s2) #desviacion estandar estimada de b1
t = b1/sb1 #estadistico de prueba t 
alfa = 0.01 #nivel de significancia (ciencias exactas 99% es decir 0.01 y para ciencias sociales 95% osea 0.05)
area_t = alfa/2 #area en la cola superior
t_tabla = 3.355 #revisar en la tabla de distribucion de t
p_value_t = 2*pt(-abs(t),df=gl) #calculo el valor-p (ya no es necesario consultar este valor en el internet) 
if (p_value_t < alfa) { #t debe ser menor que dos veces 0.005 (comando if compara dos valores) (dos veces porque son dos colas) 
  print ("Rechazo Hip???tesis Nula - Prueba t") #si se cumple que es menor, entonces muestra el mensaje rechazo la hipotesis nula  
} else {  
  print ("Acepto Hip???tesis Nula - Prueba t") #en caso contrario  muestra el mensaje rechazo la hipotesis nula  
}   

#Intervalo de confianza
int_conf_izq = b1-(t_tabla)*sb1 #intervalo de confianza (rango inferior) 
int_conf_der = b1+(t_tabla)*sb1 #intervalo de confianza (rango superior) 
int_conf = c(int_conf_izq, int_conf_der) #intervalo de confianza total (en forma de vector) 

#t.test(x,y,conf.level=0.99) #funcion que permite hacer la prueba t directamente, pero no permite escoger los grados de libertad

#################################################################
########################### F Test ##############################

CMR = SCR/1 #cuadrado medio de la regresion
F = CMR/ECM #estadistico de prueba F 
F_tabla = 11.26 #revisar en la tabla de distribucion de F
gl_num = 1 #n???mero de grados de libertad numerador
gl_den = n-2 #n???mero de grados de libertad denominador
p_value_F = pf(F, gl_num, gl_den, lower.tail=FALSE) #calculo el valor-p (ya no es necesario consultar este valor en el internet) 
if (p_value_F < alfa) { #F debe ser menor que 0.01 (una sola cola) 
  print ("Rechazo Hip???tesis Nula - Prueba F") #si se cumple que es menor, entonces muestra el mensaje rechazo la hipotesis nula  
} else {  
  print ("Acepto Hip???tesis Nula - Prueba F") #en caso contrario  muestra el mensaje rechazo la hipotesis nula  
}   
#var.test(x,y,conf.level=0.99) #funcion que permite hacer la prueba f directamente, pero no permite escoger los grados de libertad


#################################################################
######## Regresi???n Lineal - ANOVA (Funciones Directas) ##########

pizza = lm(y ~ x) #calculo de la regresion lineal (funcion directa)
plot(x, y, pch = 19, col="red",main="Regresi???n Lineal (Directa): X vs. Y") #grafico de dispersion
abline(pizza) #linea de regresion
print(summary(pizza)) #resumen de los resultados de la regresion lineal 
print (anova(pizza)) #resumen de los resultados de la tabla Anova 

#################################################################
################# An???lisis de Residuales ########################

#Grafico: Residuales vs X
plot(x,error,col="blue",main="Residuales vs. X")
eje = rep(0, length(i)) 
lines(x,eje,type="c", col="black") 

#Grafico: Residuales vs y_estimada
plot(y_est,error, col="blue",main="Residuales vs. Y_Estimada")
lines(y_est,eje,type="c", col="black") 

# Grafico: Residual Estadarizado vs X
div = cuad/s2
hi = 1/n + div
syi_yi = s*sqrt(1-hi) #desviaci???n est???ndar del residual
res_est = error/syi_yi #residual estadarizado
plot(x,res_est,col="blue",main="Residual Estadarizado vs. X")
eje = rep(0, length(i)) 
lines(x,eje,type="c", col="black") 

#Grafico de Probabilidad Normal
norm = qqnorm(i, plot.it = FALSE)$x #puntos noramales (funci???n directa genera puntos normales)
res_est_ord = sort(res_est) #residual estadarizado ordenados
plot(norm, res_est_ord, col="blue",main="Probabilidad Normal") 
eje2 = c(-1.5,1.5) #vector para graficar una linea recta de 45 grados de inclinacion
lines(eje2,eje2,type="c", col="black") 


#################################################################
############# An???lisis de Residuales: Outliers ##################
x_out = c(1,1,2,3,3,3,4,4,5,6) #datos ejemplo libro (p???gina 598 libro)
y_out = c(45,55,50,75,40,45,30,35,25,15) #datos ejemplo libro (p???gina 598 libro)
plot(x_out, y_out, pch = 19, col=12, main="Regresi???n Lineal: Outliers") #grafico de dispersion

ejem3 = lm(y_out ~ x_out) #calculo de la regresion lineal (funcion directa)
abline(ejem3) #linea de regresion
print (summary(ejem3)) #resumen de los resultados de la regresion lineal 
print (anova(ejem3)) #resumen de los resultados de la tabla Anova 
print (influence(ejem3)) #valores atipicos

#################################################################
####### An???lisis de Residuales: Valores Influyentes #############

x_inf = c(10,10,15,20,20,25,70) #datos ejemplo libro (p???gina 601 libro)
y_inf = c(125,130,120,115,120,110,100) #datos ejemplo libro (p???gina 601 libro)
plot(x_inf, y_inf, pch = 19, col=12, main="Regresi???n Lineal: Valor Influyente") #grafico de dispersion

ejem4 = lm(y_inf ~ x_inf) #calculo de la regresion lineal (funcion directa)
abline(ejem4) #linea de regresion
print (summary(ejem4)) #resumen de los resultados de la regresion lineal 
print (anova(ejem4)) #resumen de los resultados de la tabla Anova 
print (influence(ejem4)) #valores atipicos