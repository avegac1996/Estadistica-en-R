library(readr)

train_titanic <- read_csv("D:/as/period58UPS/ESTADISTICA/segundo_semestre/train_titanic.csv")
View(train_titanic)

y = train_titanic$Survived
x1= train_titanic$Pclass
x2= train_titanic$Sex
x3= train_titanic$Age
x4= train_titanic$SibSp
x5 = train_titanic$Parch
x6 = train_titanic$Fare

#0 para mujeres y 1 para hombres.

x2 = (x2=='male')*1

#Limpieza de datos
#creamos el vector eliminando los NA que aparecen en la variable fecha, dejando 714 observaciones. 
train_titanic2 =  train_titanic[! is.na(train_titanic$Age),]
y2 = train_titanic2$Survived
x12= train_titanic2$Pclass
x22= train_titanic2$Sex
x32= train_titanic2$Age
x42= train_titanic2$SibSp
x52 = train_titanic2$Parch
x62 = train_titanic2$Fare

#0 para mujeres, 1 para hombres.

x22 = (x22=='male')*1

#Embarked
#C x7 = 0 x8 = 0
#S x7 = 1 x8 = 0
#Q x7 = 0 x8 = 1

#x72 = (x72=='S')*1
#x82 = (x82=='Q')*1

#mm = cbind(y,x1,x2,x3,x4,x5,x6,x7,x8)
mm = cbind(y2,x12,x22,x32,x42,x52,x62)

#vemos si hay una correlacion alta entre x32 (edad) con otra variable
correlac = cor(mm) #no se encuentra alta correlacion con ninguna otra variable. 
#Se toma la correlacion mas alta, en este caso con la x12 con una correlacion de -0.3692 
#hacemos la ecuacion de regresion con las variables de x32 en base a x12.
reg = lm(x32~x12)

#bucle que buscara y reemplaza los valores de la x3 en base a la regresion obtenida
for(i in 1:nrow(train_titanic))
  if(is.na(train_titanic$Age[i])){
    train_titanic$Age[i] = reg$coefficients[1]+reg$coefficients[2]*x1[i]
    train_titanic$Age[i] = round(train_titanic$Age[i],0)
    x3[i] = train_titanic$Age[i]
  }



mm2 = cbind(y,x1,x2,x3,x4,x5,x6)
correlac2 = cor(mm2)#Correlacion del dataframe sin Missing Values. 

set.seed(1)# semilla establecida para que los datos aleatorios no cambien.
aleatorio = sample(891,replace = F) #datos aleatorios de las observaciones
#obtenemos la matriz aleatoria de los datos. 
trainrandom = train_titanic[c(aleatorio),] #matriz con numeros de filas aleatorizadas.

#training 70% de 891 = 624 datos
training =trainrandom[1:624,]
#Creamos las variables de entrenamiento para el modelo de regresion logistica
yE = training$Survived
x1E = training$Pclass
x2E = training$Sex
x3E = training$Age
x4E = training$SibSp
x5E = training$Parch
x6E = training$Fare
x2E = (x2E=='male')*1

#modelo de regresion logistica con las variables de entrenamiento
reg2 = glm(yE~x1E+x2E+x3E+x4E+x5E+x6E,family=binomial())
summary(reg2)

#TEST (30% de los datos) -> 267 datos
test =trainrandom[625:length(y),]
#Creamos las variables de prueba
yP = test$Survived
x1P = test$Pclass
x2P = test$Sex
x3P = test$Age
x4P = test$SibSp
x5P = test$Parch
x6P = test$Fare

x2P = (x2P=='male')*1


#Ecuacion de regresion y probamos con las variables de test
yest = exp(reg2$coefficients[1]+reg2$coefficients[2]*x1P+reg2$coefficients[3]*x2P+reg2$coefficients[4]*x3P+reg2$coefficients[5]*x4P+reg2$coefficients[6]*x5P+reg2$coefficients[7]*x6P)/(1+exp(reg2$coefficients[1]+reg2$coefficients[2]*x1P+reg2$coefficients[3]*x2P+reg2$coefficients[4]*x3P+reg2$coefficients[5]*x4P+reg2$coefficients[6]*x5P+reg2$coefficients[7]*x6P))

yest1 = round(yest) # se redondea los valores de yest para trasnformarlos en boolenos.

error = (yP==yest1)*1
#modelo de regresion
accuracy = sum(error)/length(y2) #El modelo es bastante malo, ya que solo tiene una precision del 30.67%

#Se hace prueba segun ciertos parametros cambiando los valores de la variable con mayor correlacion con y, para comprobar el odds. 
mm3 = cbind(yP,x1P,x2P,x3P,x4P,x5P,x6P)
correlac3 = cor(mm3) #Se nota que la variable x2 (sexo) tiene la correlacion mas alta de -0.5643

mx1=round(mean(x1P))
mx3=round(mean(x3P))
mx4 =round(mean(x4P))
mx5 = round(mean(x5P))
mx6 = mean(x6P)

#tomamos valores de las medias de las variables, y el cambio en x2P--> hombres- 0 mujeres.
#Se quiere ver las probabilidades y odds segun estas variables para los pasajeros que sobrevivieron. 

#probabilidad de que el pasajero haya sobrevivido cuando es de 2da clase, tiene 30 años, es hombre, tiene 0 hermanos/conyuges a bordo,
#0 numero de padres/hijos acompañando al pasajero, y ha pagado una tarifa de 29.9
yhombre= exp(reg2$coefficients[1]+reg2$coefficients[2]*2+reg2$coefficients[3]*1+reg2$coefficients[4]*30+reg2$coefficients[7]*29.9)/(1+exp(reg2$coefficients[1]+reg2$coefficients[2]*2+reg2$coefficients[3]*1+reg2$coefficients[4]*30+reg2$coefficients[7]*29.9))
# la probabilidad estimada de que el pasajero hombre haya sobrevivido es del 25.5%

ymujer = exp(reg2$coefficients[1]+reg2$coefficients[2]*2+reg2$coefficients[3]*0+reg2$coefficients[4]*30+reg2$coefficients[7]*29.9)/(1+exp(reg2$coefficients[1]+reg2$coefficients[2]*2+reg2$coefficients[3]*0+reg2$coefficients[4]*30+reg2$coefficients[7]*29.9))
# la probabilidad estimada de que la pasajera mujer haya sobrevivido es del 83.7%

oddshombre = yhombre / (1-yhombre)
oddsmujer = ymujer / (1-ymujer)
odds = oddshombre/oddsmujer #la posibilidad estimada de que el pasejero sobreviva siendo hombre es de 0.067 veces sobre la posiblidad de que sobreviva siendo mujer. 



#Probablidad de que Jack haya sobrevivido
yjack= exp(reg2$coefficients[1]+reg2$coefficients[2]*2+reg2$coefficients[3]*1+reg2$coefficients[4]*23+reg2$coefficients[7]*15)/(1+exp(reg2$coefficients[1]+reg2$coefficients[2]*2+reg2$coefficients[3]*1+reg2$coefficients[4]*23+reg2$coefficients[7]*15))
#Probablidad de que Jack haya sobrevivido con Rose
yjack2= exp(reg2$coefficients[1]+reg2$coefficients[2]*2+reg2$coefficients[3]*1+reg2$coefficients[4]*23+reg2$coefficients[5]*1+reg2$coefficients[7]*15)/(1+exp(reg2$coefficients[1]+reg2$coefficients[2]*2+reg2$coefficients[3]*1+reg2$coefficients[4]*23+reg2$coefficients[5]*1+reg2$coefficients[7]*15))