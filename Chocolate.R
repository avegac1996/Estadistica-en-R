####################################################
################    Chocolate    ################### 
####################################################

#Datos del Ejercicio:
x1 = c(3.17,3.58,1.49,2.91,0.76,3.70,5.08,2.11,2.20,4.76,7.05,3.36,3.22,6.55,0.70,1.06,4.66,0.70,1.21) #vector de precio
y = c(1,1,0,1,0,1,1,1,0,1,1,0,0,1,0,1,1,0,0) #vector de evaluaci???n
i = c(1:length(x1))  #vector del n???mero de fabricantes 

#################################################################
#################### Regresi???n Logistica ########################

par(mfrow=c(1,1)) # varios gr???ficos en una sola pantalla
plot(x1, y, pch = 19, col=12, main="Diagrama de Dispersi???n: X1 vs. Y") #grafico de dispersion x1
choco = glm(y ~ x1,family=binomial()) #calculo de la regresion lineal (funcion directa)
print (summary(choco)) #resumen de los resultados de la regresion lineal 
y_estim = exp(choco$coefficients[1]+choco$coefficients[2]*x1) / (1+exp(choco$coefficients[1]+choco$coefficients[2]*x1)) #ecuacion de regresion logistica estimada
plot(x1, y_estim, pch = 19, col=18, main="Diagrama de Dispersi???n: X1 vs. Y Estimada") #grafico de dispersion x1

#################################################################
########################### LOGIT ###############################

choco2 = glm(y ~ x1,family=binomial("logit")) #calculo de la regresion lineal (funcion directa)
print (summary(choco2)) #resumen de los resultados de la regresion lineal logit
g_estim = exp(choco2$coefficients[1]+choco2$coefficients[2]*x1) / (1+exp(choco2$coefficients[1]+choco2$coefficients[2]*x1)) #ecuacion de regresion logistica estimada logit
plot(x1, g_estim, pch = 19, col=17, main="Diagrama de Dispersi???n: X1 vs. g Estimada") #grafico de dispersion x1

#################################################################
########################## ODDS Ratio ###########################
odds_x1 = exp(choco$coefficients[2]) #odds para x1