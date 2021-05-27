####################################################
################    Transporte    ################## 
####################################################

#Datos del Ejercicio:
x1 = c(100,50,100,100,50,80,75,65,90,90) #vector de millas recorridas
x2 = c(4,3,4,2,2,2,3,4,3,2) #vector de cantidad de entregas
y = c(9.3,4.8,8.9,6.5,4.2,6.2,7.4,6.0,7.6,6.1) #vector de tiempo de recorrido 
i = c(1:length(x1))  #vector del n???mero de recorridos enumerados del 1 al 10

#################################################################
######## Regresi???n Lineal - ANOVA (Funciones Directas) ##########

par(mfrow=c(1,3)) # varios gr???ficos en una sola pantalla
plot(x1, y, pch = 19, col=12, main="Diagrama de Dispersi???n: X1 vs. Y") #grafico de dispersion x1
plot(x2, y, pch = 19, col=18, main="Diagrama de Dispersi???n: X2 vs. Y") #grafico de dispersion x2
#install.packages("rgl") #instala el paquete rgl para graficar en 3D
library(rgl) #cargo la libreria rgl
plot3d(x1, x2, y, pch = 19, col=18) #grafico de dispersion x2
ejem2 = lm(y ~ x1 + x2) #calculo de la regresion lineal (funcion directa)
print (summary(ejem2)) #resumen de los resultados de la regresion lineal 
print (anova(ejem2)) #resumen de los resultados de la tabla Anova 

#################################################################
########################## Residuales ###########################

trans = summary(ejem2) #resumen de los resultados de la regresion lineal 
b0_m = trans$coefficients[1] #calculo de b0
b1_m = trans$coefficients[2] #calculo de b1
b2_m = trans$coefficients[3] #calculo de b2
y_est_m = b0_m+b1_m*x1+b2_m*x2 #ecuacion de regresion multiple estimada 
error = y-y_est_m #residual
trans2 = influence(ejem2) #analisis de los residuales
error2 = trans2$wt.res #residual (otra forma de obtenerlo)
s = trans$sigma
hi = trans2$hat #influencia
syi_yi = s*sqrt(1-hi) #desviaci???n est???ndar del residual
res_est = error/syi_yi #residual estadarizado

#Grafico: Y_estimada vs Residuales Estandarizados
plot(y_est_m,res_est,col="blue",main="Y_estimada vs. Residuales Estandarizados")
eje = rep(0, length(i)) 
lines(y_est_m,eje,type="c", col="black") 

#################################################################
################## Residual Estudentizado #######################
restudentizado = rstudent(ejem2)
t_tabla = qt(0.05/2,6,lower.tail = FALSE)

#################################################################
######################### Influyentes ###########################
p = trans$df[1]-1 #n???mero de variables independientes (grados de libertad en el numerador)
n = length (i) #numero de muestras (longitud del vector i)
treshold = 3*(p+1)/n  #establezco un umbral maximo 
z = hi[which(hi > treshold)] #en el vector hi (infleuncia de la observaci???n), escojo el valor (o valores) de hi que sea mayor que mi umbral
cat("Los Valores hi Influyentes son:", z, "\n") #imprime los valores hi influyentes
val_inf_x1 = x1[which(hi > treshold)] #coordenada en x1 del valor influyente
val_inf_x2 = x2[which(hi > treshold)] #coordenada en x2 del valor influyente
val_inf_y = y[which(hi > treshold)] #coordenada en y del valor influyente
cat("Los Valores Influyentes son los puntos que tienen coordenadas en X1:", val_inf_x1, "en X2:",val_inf_x2, "y coordenadas en Y:",val_inf_y, "\n") #imprime las coordenadas de los puntos influyentes

#################################################################
##################### Distancia de Cook #########################
cook = cooks.distance(ejem2)
