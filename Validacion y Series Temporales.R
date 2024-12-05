library(readxl)
turistas_UK <- read_excel("C:/Users/Moi/Desktop/Maestría Inteligencia de Negocios/Análisis de Datos Masivos/Datasets/turistas_UK.xlsx")
View(turistas_UK)

datos<-turistas_UK

plot.ts(datos$UK)

#Para la siguiente prediccion, se separan los datos de entrenamiento desde 
#el 1 valor[2000/01/31] hasta el valor 227 [2018/11/31]
#Cabe recalcar que los datos solo proporicionan año y mes pero se insinua que el
#valor es a finales de cada mes si pensamos en el día

#Para los datos de validación, se empieza desde el valor/linea 228 [2018/12/31] 
#hasta el ultimo valor/linea 239 [2019/11/31] 

#Es importante recalcar igual que el orden si importan en estos datos ya que es una 
#serie temporal

#El objetivo es validar la prediccion con los datos de validacion y realizar 
#una prediccion estimada con el conjunto de entrenamiento el cual va a ser entrenado
#para comparar que tan acertado prodia ser un modelo predictivo para valores 
#desconocidos hacia el futuro


entrenamiento<-datos[1:227,2]
validacion<-datos[228:239,2]


#Se dividen los valores en frecuencia de 12 meses 
#empezando desde el mes 1 del año 2000 con los datos de
#entrenamiento y desde el mes 12 del año 2018 para los 
#datos de validacion con el fin de estudiar cada fila 

entrenamiento_st<-ts(entrenamiento, start = c(2000,1),frequency = 12)
plot(entrenamiento_st)
validacion_st<-ts(validacion, start = c(2018,12),frequency = 12)
plot(validacion_st)


library(forecast)

#Se aplica el modelo arima para que logre entrenar los datos de entrenamiento 
#logrando estimar posibles resultados hacia el futuro
modelo_arima_entrenamiento<-auto.arima(entrenamiento_st)

#Se aplica la prediccion para los siguientes 12 meses desde
#[2018/12/31] hasta [2019/11/31] siendo estos rangos los mismos rangos con los que 
#se va a validar el conjunto de datos de validacion 
prediccion_12_meses<-forecast(modelo_arima_entrenamiento,12)


#La grafica enseña el posible comportamiento segun la prediccion
#La linea azul son los valores promedio de la prediccion y los bordes grises son 
#los rangos de estos valores promedios
plot(prediccion_12_meses)

prediccion_12_meses_valores<-prediccion_12_meses$mean


#Los valores promedio hacia el futuro
prediccion_12_meses_valores

prediccion_12_meses$mean


#Ahora graficamos el comportamiento de la serie del conjunto de datos de validacion
#que empieza desde [2018/12/31] hasta [2019/11/31]
plot(validacion_st)

#Comparamos con una linea azul el comportamiento de la predicción 
lines(prediccion_12_meses_valores, col="blue")

#Son casi similares los valores tanto del conjunto de validacion(datos reales)
#asi como el conjunto de datos a futuro(prediccion)


#El error medio(ME) es de 77356.06 con respecto a los valores 
#El error promedio(MPE) es de 4.94%   
accuracy(validacion_st,prediccion_12_meses_valores)


