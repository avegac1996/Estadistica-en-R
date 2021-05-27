####################################################
#############  Ejercicio en Clase    ############### 
####################################################

#Datos del Ejercicio:
x1 = c(1,1,2,3,4,4,5,15) #vector x1
y = c(18,21,22,21,23,24,26,39) #vector y 
i = c(1:length(x1))  #vector del n???mero de elementos

#################################################################
######## Regresi???n Lineal - ANOVA (Funciones Directas) ##########

par(mfrow=c(1,3)) # varios gr???ficos en una sola pantalla
plot(x1, y, pch = 19, col=12, main="Diagrama de Dispersi???n: X1 vs. Y") #grafico de dispersion x1
#plot(x2, y, pch = 19, col=18, main="Diagrama de Dispersi???n: X2 vs. Y") #grafico de dispersion x2
#install.packages("rgl") #instala el paquete rgl para graficar en 3D
#library(rgl) #cargo la libreria rgl
#plot3d(x1, x2, y, pch = 19, col=18) #grafico de dispersion x2
ejem2 = lm(y ~ x1) #calculo de la regresion lineal (funcion directa)
print (summary(ejem2)) #resumen de los resultados de la regresion lineal 
print (anova(ejem2)) #resumen de los resultados de la tabla Anova 

#################################################################
########################## Residuales ###########################

trans = summary(ejem2) #resumen de los resultados de la regresion lineal 
b0_m = trans$coefficients[1] #calculo de b0
b1_m = trans$coefficients[2] #calculo de b1
y_est_m = b0_m+b1_m*x1 #ecuacion de regresion multiple estimada 
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

#################################################################
######################### Influyentes ###########################
p = trans$df[1]-1 #n???mero de variables independientes (grados de libertad en el numerador)
n = length (i) #numero de muestras (longitud del vector i)
treshold = 3*(p+1)/n  #establezco un umbral maximo 
z = hi[which(hi > treshold)] #en el vector hi (infleuncia de la observaci???n), escojo el valor (o valores) de hi que sea mayor que mi umbral
cat("Los Valores hi Influyentes son:", z, "\n") #imprime los valores hi influyentes
val_inf_x1 = x1[which(hi > treshold)] #coordenada en x1 del valor influyente
val_inf_y = y[which(hi > treshold)] #coordenada en y del valor influyente
cat("Los Valores Influyentes son los puntos que tienen coordenadas en X1:", val_inf_x1, "y coordenadas en Y:",val_inf_y, "\n") #imprime las coordenadas de los puntos influyentes

#################################################################
##################### Distancia de Cook #########################
cook = cooks.distance(ejem2)
