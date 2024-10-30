library(readr)
housing_in_london_monthly_variables <- read_csv("C:/Users/Moi/Desktop/Maestría Inteligencia de Negocios/Análisis de Datos Masivos/Datasets/housing_in_london_monthly_variables.csv")
View(housing_in_london_monthly_variables)

datos<-housing_in_london_monthly_variables
copiadatos<-housing_in_london_monthly_variables

print(datos)

ncol(datos)
nrow(datos)
dim(datos)

str(datos)
summary(datos)

is.na(datos)

is.na(datos$houses_sold)
which(is.na(datos$houses_sold))
which(is.na(datos$no_of_crimes))

# Para rellenar a mano y manipular el set de datos, se usan corchetes 
# los cuales identifican [filas,columnas]

datos[601,"houses_sold"]<-158 # Es una manera de agregar datos a mano 
# identificando las filas y columnas

datos[601,5]<-150 # Tambien se lo puede editar sabiendo el numero de la columna

mean(datos$houses_sold) # Si existen NA's los cuales no permiten realizar un calculo

mean(datos$houses_sold, na.rm = TRUE)# Se usa el comando na.rm(remove na) para no 
#tomarlos en cuenta 

datos$houses_sold[is.na(datos$houses_sold)]<-mean(datos$houses_sold,na.rm = TRUE)
#Con este metodo, se llenaron todos los NA's de la fila de casas vendidas

summary(datos)# Ya no hay NA's en los datos de casas vendidas
summary(copiadatos) #Hay 94 NA's en los datos de casas vendidas

datosfiltrados<-copiadatos[!is.na(copiadatos$houses_sold),]
#Se crea otro dataset con la eliminacion de todas las filas que tenian NA's en los
#datos de la columna de casas vendidas 

summary(datosfiltrados)
dim(datosfiltrados)
dim(datos)

# Para extraer informacion de algun dato que se repite en x numero de filas
# En este caso, solo se quieren sacar los datos del barrio Enfield

enfiled<-datosfiltrados[datosfiltrados$area=="enfield",]
bexley<-datosfiltrados[datosfiltrados$area=="bexley",]

# Se va a sacar la media y varianza 
mean(bexley$average_price)
mean(enfiled$average_price)

# Haciendo un analisis multivariante, se puede calcular la covarianza

cov(bexley$average_price,bexley$houses_sold)
cov(enfiled$average_price,enfiled$houses_sold)
cov(enfiled$houses_sold,enfiled$average_price) # El orden no afecta el resultado

# Se entiende que tiene una relacion de covarianza negativa ya que si el precio 
# aumenta, las casas vendidas disminuyen

# Graficas para notar la correlacion

plot(bexley$average_price,bexley$houses_sold)
plot(enfiled$average_price,enfiled$houses_sold)

boxplot(enfiled$average_price)
boxplot(bexley$houses_sold) #Existe un dato anomalo dentro de los maximos
summary(bexley$houses_sold) #Dentro de los datos, se vedieron 633 casas pero
#eso es algo fuera de lo comun ya que su promedio de ventas es de 325

plot(enfiled$average_price)
plot.ts(enfiled$average_price)


cov(bexley$average_price,bexley$houses_sold)
cor(bexley$average_price,bexley$houses_sold) #Hay correlacion negativa pero esta 
#algo cercana a cero, significa que no es muy estrecha la relacion 

#Medirlo para enfield

plot(enfiled$average_price,enfiled$houses_sold)
cov(enfiled$average_price,enfiled$houses_sold)
cor(enfiled$average_price,enfiled$houses_sold)#La correlacion es mas amplia que la de
#bexley pero no es muy lejano 

#Analisis de Regresion 

#Relacion casas vendidas y precio 
#Tomar la variable dependiente(casas vendidas) y la variable independiente
#la cual es (precio)

lm(datosfiltrados$houses_sold~datosfiltrados$average_price)
#Los resultados representan que la intercepcion en la regresion lineal entre 
#casas vendidas(Y)  y precio (X) se encuentran en 6756 casas vendidas cuando
#el precio promedio llegaria a ser 0

#Por otro lado, si se le multimplica x 100 al precio, eso quiere decir que al 
# aumentar 100$ al precio de una casas (0.01091*100=1.09), las ventas en las 
# casas disminuyen en 1.09 unidades

lm(enfiled$houses_sold~enfiled$average_price)


plot(enfiled$average_price, enfiled$houses_sold, 
     main = "Regresión Lineal de Casas Vendidas vs. Precio Promedio",
     xlab = "Precio Promedio", 
     ylab = "Casas Vendidas", 
     pch = 16, 
     col = "blue")

if(!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)

ggplot(enfiled, aes(x = average_price, y = houses_sold)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regresión Lineal de Casas Vendidas vs. Precio Promedio",
       x = "Precio Promedio",
       y = "Casas Vendidas")