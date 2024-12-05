# Uso de la base de datos del TITANIC para estudio de modelado de datos

install.packages("titanic")
library(titanic)

data("titanic_train")

datos<-titanic_train

####EDA Y LIMPIEZA DE DATOS####

str(datos)

summary(datos)


#Al quitar un NA, se quita toda la fila. En este caso vamos a quitar las celdas
#vacias de la columan 'edad'
datosfiltrados<-datos[!is.na(datos$Age),]

#Crear un modelo para ver si alguien sobrevive o no sobrevive

#Variable dependiente es la columna "Survived"

#Variables independientes seran: "Edad","Sexo","Clase"

#El problema es clasificar y saber quien sobrevive y quien no sobrevive


#El comando 'table' divide como esta organizado una columna
table(datosfiltrados$Survived)

#De la muestra, 424 no sobrevivieron (0) y 290 si sobrevivieron (1)

#Para dividirlo en porcentaje
prop.table(table(datosfiltrados$Survived))

#El 59.4% no sobrevivio y el 40.6% si sobrevivio

#Analizar si se sobrevivio o no dependiendo del sexo

table(datosfiltrados$Survived,datosfiltrados$Sex)

prop.table(table(datosfiltrados$Survived,datosfiltrados$Sex))

#Analizar si se sobrevivio o no dependiendo de clase donde se viajaba 

table(datosfiltrados$Survived,datosfiltrados$Pclass)

prop.table(table(datosfiltrados$Survived,datosfiltrados$Pclass))

#Analizar si se sobrevivio o no dependiendo de la edad

table(datosfiltrados$Survived,datosfiltrados$Age)

#El comando 'table' funciona mejor con variables categoricas

# Para hacer una regresión logistica para un ejercicio de clasificación es 
# importante saber que la variable dependiente es categorica 0 o 1/V O F/Sobrevivio o
# no sobrevivio

#La regresion lineal se construye de casi la misma forma que una regresion logística

#Regresion lineal (lm)
#Regresión logística (glm)

#Para una regresión logística, la variable dependiente tiene que ser categórica
#En este caso, la cagegoría es si sobrevivio o no(Survived) y la variable 
#independiente es la edad(Age)

#####REGRESION LOGISTICA####

modelo_logistico<-glm(datosfiltrados$Survived~datosfiltrados$Age,family = "binomial")
summary(modelo_logistico)

#La H0 nula se rechaza mediante el uso de la varible de edad Age
#La H0 nula se acepta mediante el intercepto 
#En los modelos logit se pueden interpretrar el SIGNO pero no la magnitud

#En este caso, si el signo es negativo, quiere decir que hay una relacion inversa

#Si la edad aumenta, la probabilidad de sobrevivir disminuye y viceversa

#Vamos a analizar con la variable sexo

modelo_logistico_sexo_edad<-glm(datosfiltrados$Survived~datosfiltrados$Age+
                           datosfiltrados$Sex,family = "binomial")
summary(modelo_logistico_sexo_edad)

#Debido al valor p de la variable Age, es superior al nivel de significancia 
#el cual es 0.10% por lo cual no se rechaza la hipotesis nula y por lo tanto, no tiene 
#mayor relevancia la variable Age concluyendo que no es significativo su resultado

#Si el pvalor es 0.39, es decir, 39 veces de cada 100, el coeficiente es nulo.

#Siendo ese el caso, la H0 es saber si la edad no infiere en la supervivencia 

#Si infiere, se rechaza la hipotesis nula
#Si no infiere, no se rechaza la hipotesis nula

#Por defecto R en la variable sex solo identifica a los hombres 'male'
#dentro del modelo

#Si uno es hombre, siendo esta relacion negativa, no sobrevives

#Vamos a incorporar la variable de clase de viaje (Pclass)

modelo_logistico_sexo_edad_clase<-glm(datosfiltrados$Survived~
                                        datosfiltrados$Age+datosfiltrados$Sex+
                                        datosfiltrados$Pclass,family = "binomial")
summary(modelo_logistico_sexo_edad_clase)

#Todos los coeficientes son significativos, ya que su Pvalor es inferior al valor
#significativo 0.10%. Por lo tanto rechazamos la hipotesis nula y concluimos que los
#valores si infieren en la supervivencia

#Por otro lado, el signo es negativo en las 3 variables descubriendo que tanto
#el sexo como la clase son las variables que mas infieren en la supervivencia de 
#forma negativa

str(datosfiltrados)
#R identifica a las clases como valores enteros INT pero se supone que son variables
#categoricas

#Se utiliza el comando as.factor para dividiro las categorias tanto 2 como 3
modelo_logistico_sexo_edad_clase_dividido<-glm(datosfiltrados$Survived~
                                        datosfiltrados$Age+datosfiltrados$Sex+
                                        as.factor(datosfiltrados$Pclass),family 
                                        = "binomial")
summary(modelo_logistico_sexo_edad_clase_dividido)

#Se logran ver los resultados de las categorias 2 y 3 con respecto a la categoria 1


#Ahora se va a evaluar el modelo para ver su nivel de exactitud con respecto
#a los datos reales

#Analisis de los datos reales

table(datosfiltrados$Survived)

#Dentro de los datos, hay 424 personas que no sobrevivieron '0' y 290 que si
#sobrevivieron '1'

#####ANALISIS DEL MODELO PREDICTIVO####

prediccion<-predict(modelo_logistico_sexo_edad_clase_dividido, type = "response")

View(prediccion)

#Los valores tienen que ser codificados y redondeados a 0 o 1, dependiendo de que 
#valor se aproximen mas a 0 o 1 

modelo_logistico_sexo_edad_clase_dividido_codificado<-ifelse(prediccion>0.5,1,0)

View(modelo_logistico_sexo_edad_clase_dividido_codificado)                                                        

#Ahora que ya tiene valores tanto 1 sobrevivio como 0 no sobrevivio, analizamos

table(datosfiltrados$Survived,modelo_logistico_sexo_edad_clase_dividido_codificado)

#El modelo acerto 356 veces con respecto al 0 = No sobrevivio (ACIERTO)
#El modelo acerto 207 veces con respecto al 1 = Sobrevivio (ACIERTO)
#El modelo no acerto 83 veces con respecto al 0 = No sobrevivio (NO ACERTO)
#El modelo no acerto 68 veces con respecto al 1 = Sobrevivio (NO ACERTO)

#Numero total de aciertos 356+207=563
#Numero total del modelo real: 714

#Porcentaje de exactitud: 513/714*100=78.85%
563/714*100

#El modelo acierta 78.85% de las veces con respecto a los datos reales


#####CREACION DE UN MODELO LOGISTICO DE FORMA ALTERNATIVA 

modelo_logistico_alternativo<-glm(Survived~Age+Sex+as.factor(Pclass), 
                                  data = datosfiltrados,family="binomial")

summary(modelo_logistico_alternativo)


#Calculo de la matriz de confusion y otras metricas de manera automatica 

install.packages("caret")
library(caret)

#Calculo de la matriz de confusion 

confusionMatrix(as.factor(datosfiltrados$Survived), 
                as.factor(modelo_logistico_sexo_edad_clase_dividido_codificado))

#En base al modelo, cual es la probabilidad de alguien con las caracteristicas de 
#Rose para que sobreviva en el Titanic

Rose<-predict(modelo_logistico_alternativo, 
              newdata = data.frame(Age=18,
                                   Sex="female",
                                   Pclass=1))

View(Rose)

exp(Rose)/(1+exp(Rose))

#La probabilidad de sobrevivencia de Rose es de 95%

#NOTA: Para calcular esta prediccion, se necesita aplicar el formato del 
#modelo_logistico_alternativo

#Ahora la probabilidad de Jack

Jack<-predict(modelo_logistico_alternativo, 
              newdata = data.frame(Age=22,
                                   Sex="male",
                                   Pclass=3))
summary(modelo_logistico_alternativo)

exp(Jack)/(1+exp(Jack))

#La probabilidad de Jack es de 0.10%

#Mediante exponenciales, se puede calcular la probabilidad de las variables estudiadas

summary(modelo_logistico_alternativo)

#En el caso de los hombres 'male'

exp(-2.522781)

#Se sabe que siendo hombre, se tenia un 92% menos de probabilidad de sobrevivir 
#que la mujer
#La mujer tenia en cambio un 92% de probabilidades de sobrevivir que el hombre

#Ahora de la edad

exp( -0.036985)

#Por cada año, la probabilidad de no sobrevivir aumenta en un 3.7%



####ARBOL DE DECISION####

#Se necesitan dos librerías 
#rpart y rpart.plot

install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

arbol<-rpart(Survived~Age+Sex+as.factor(Pclass), 
              data = datosfiltrados,method ="class")

rpart.plot(arbol)

#El diagrama empieza desde 1 (sobrevivir) y 0 (no sobrevivir) y se caracteriza 
#por la densidad de su color 

#Primer cuadro y linea: Del 100% de las personas, sobrevivieron 41%, por eso esta de 
#color azul ya que este resultado se aproxima mas a 0

#Segunda linea: Es hombre/no es hombre

#Cuadro izquierdo: Es hombre y del 100% de la muestra, 63% eran hombres y de ese 63%,
#21% sobrevivieron. Es de color azul el cuadro porque 21% se aproxima mas a 0

#Cuadro derecha: Es mujer y representa el 37% de la poblacion donde el 75% 
#de las mujeres sobrevivieron y se representa de color verde porque el 75%
#se aproxima mas a 1

#Tercera linea izquierda: Es hombre y la edad es >=6.5 años

#Cuadro izquierdo:Del 63% de los hombres de toda la muestra,el 60% de los hombres 
#mayores a 6.5 solo sobrevivieron el 18%

#Cuadro derecha: El 3% que eran menores a 6.5 años siendo hombres, sobrevivio 
# el 67% de esta micro muestra


#Para medir la importancia de las variables

summary(arbol)

#Para hacer una comparativa entre la prediccion del modelo logistico y el modelo
#del arbol



arbol_prediccion<-predict(arbol, type = "class")
table(arbol_prediccion)
#arbol_prediccion_codificado<-ifelse(arbol_prediccion>0.5,1,0)

#OJO: En la prediccion, hay que cambiar el tipo(type) a "class" en vez de "response"
#En este caso, ya no es necesario codificar los valores porque ya vienen codificados

confusionMatrix(as.factor(datosfiltrados$Survived),as.factor(arbol_prediccion))

#El arbol de decision tiene un grado de exactitud del 81.65%

#Resultados de cada prediccion entre 0 y 1
summary(arbol_prediccion)
#O tambien funciona
table(arbol_prediccion)
table(modelo_logistico_sexo_edad_clase_dividido_codificado)

#La regresion logistica solo funciona para 2 opciones con respecto a la variable 
#dependientes
#El arbol de decision funciona para 2 o mas opciones con respecto a la variable 
#dependiente
