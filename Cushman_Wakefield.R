####################################################
##########    Cushman Wakefield, Inc.    ########### 
####################################################

#Datos del Ejercicio (p???gina 587 ejercicio 44)
i = c(1:18)  #vector de datos
x = c(21.9,6,22.8,18.1,12.7,14.5,20,19.2,16,6.6,15.9,9.2,19.7,20,8.3,17.1,10.8,11.1) #vector de tasa de desocupacion
y = c(18.54,33.7,19.67,21.01,35.09,19.41,25.58,17.02,24.04,31.42,18.74,26.76,27.72,18.2,25,29.78,37.03,28.64) #vector de tasa promedio

#Calculo la media de x e y (ver diapositiva 47)
xmed = mean(x) #media (valor esperado) de x (formula 1)
ymed = mean(y) #media (valor esperado) de y (formula 1)

#Calculo de la ecuacian de regresion estimada 
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
plot(x,y,col="red") #grafico de dispersion
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
alfa = 0.05 #nivel de significancia (ciencias exactas 99% es decir 0.01 y para ciencias sociales 95% osea 0.05)
area_t = alfa/2 #area en la cola superior
t_tabla = 2.120 #revisar en la tabla de distribucion de t
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
F_tabla = 4.49 #revisar en la tabla de distribucion de F
gl_num = 1 #n???mero de grados de libertad numerador
gl_den = n-2 #n???mero de grados de libertad denominador
p_value_F = pf(F, gl_num, gl_den, lower.tail=FALSE) #calculo el valor-p (ya no es necesario consultar este valor en el internet) 
if (p_value_F < alfa) { #F debe ser menor que 0.01 (una sola cola) 
  print ("Rechazo Hip???tesis Nula - Prueba F") #si se cumple que es menor, entonces muestra el mensaje rechazo la hipotesis nula  
} else {  
  print ("Acepto Hip???tesis Nula - Prueba F") #en caso contrario  muestra el mensaje rechazo la hipotesis nula  
}   
#var.test(x,y,conf.level=0.99) #funcion que permite hacer la prueba f directamente, pero no permite escoger los grados de libertad