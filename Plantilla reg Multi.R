# Plantilla para ejercicio de regresion multiple
library(car)
library(ggplot2)
library(ggpubr)
library(GGally)
library(leaps)
library(lmtest)
library(gridExtra)

seed(100)
#Separar los datos de prueba, y los datos de entrenamiento
i.train <- sample(1:32, 20)
datos <- mtcars[i.train, ]
datos.test <- mtcars[-i.train, ]

#Se aplicar lo siguiente para las variables categoricas
datos[["CATEGORICA"]] <- factor(datos[["CATEGORICA"]])
datos.test[["CATEGORICA"]] <- factor(datos.test[["CATEGORICA"]])
stopifnot(levels(datos[["CATEGORICA"]]) == levels(datos.test[["CATEGORICA"]]))


ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

# Matriz de regresiones 
round(cor(x = datos, method = "pearson"), 3)
# Se puede observar tanto la relacion lineal entre cada par de variables como
# el coeficiente de relacion de las mismas. Ademas, se puede visualizar un 
# histograma de la la variable. 

########(significativo > .04)########

########ESTO NO VA EN EL CODIGO########
nulo <- lm(VARY ~ 1, data = datos)
completo <- lm(VARY ~ ., data = datos)

modelo <- step(
  nulo,
  scope = list(lower = nulo, upper = completo),
  direction = "both"
)
########ESTO NO VA EN EL CODIGO########


###### AGREGAMOS UNA VARIABLE X AL MODELO CREADO AUTOMATICAMENTE

# En la matriz de regresiones se puede observar que la VARY tiene una relacion
# con la VARX1, VARX2, VARX3 y VARX4

# Por tanto, se procedera a crear un modelo con los elementos anteriores.
modelo <- lm(VARY ~ varx1+ varx2+ varx3+ varx4, data= datos) 


# Agregar elemento
modelo_b <- update(modelo, . ~ . + analfabetismo)

# Se compara el modelo (original vs con elemento eliminado) 

summary(modelo)
summary(modelo_b)

# R^2 : Se utiliza este para pensalizar el ingreso de una nueva variable
# predictora se usa el R^2, el cual indica la varianza explicada por el 
# modelo (Es mas apropiado que el R).
# Mayor es mejor

anova(modelo, modelo_b)
# Ya que el Alpha << .05, se puede rechazar la hipotesis nula, por ende se puee
# concluir de que existe una diferencia entre la variable explicada de un
# modelo, por sobre otro (un modelo es considerablemente mejor).

AIC(modelo, modelo_b)
# Aplicando el criterio matematico del AIC se puede determinar si un modelo
# mejora o empeora en relacion a la extraccion de un predictor. Este criterio
# toma la AIC menor como mejor opcion.

#Eliminar elemento
modelo_c <- update(modelo, . ~ . - varx4)

modelo <- modelo_c

# INTERPRETACION: Si el resto de variables se mantienen constantes, por cada 
# unidad que aumentael predictor en cuestión,la variable (Y) varía en promedio
# tantas unidades como indica la pendiente.
# Para este ejemplo, por cada unidad que aumenta el predictor universitarios,
# la esperanza de vida aumenta en promedio 0.04658 unidades, manteniéndose 
# constantes el resto de predictores.

# Ahora para aceptar el modelo, es necesario que este cumpla con las siguientes
# condiciones:

datos$prediccion <- modelo$fitted.values
datos$residuos   <- modelo$residuals

# 1. Relación lineal entre los predictores numéricos y la variable respuesta
# Para verificar este punto se relalizara un diagrama de dispersión entre
# cada uno de los predictores y los residuos del modelo.
# Si la relación es lineal, los residuos deben de distribuirsealeatoriamente
# en torno a 0 con una variabilidad constante a lo largo del eje X. 

#Uno por cada para predictor~modelo
plot1 <- ggplot(data = datos, aes(habitantes, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos, aes(asesinatos, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos, aes(universitarios, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos, aes(heladas, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(plot1, plot2, plot3, plot4)
#######EXPLICAR########


# 2. Los residuos estan distribuidos de forma normal.
# Para verificar esta condicion se realizara un grafico Quintil-Quintil, junto
# con un test de normalidad "Shapiro-Wilk" para los residuos.
qqnorm(modelo$residuals)
qqline(modelo$residuals)
#plot(modelo)
shapiro.test(modelo$residuals)

# Concluir sobre el grafico (linea roja)
# Como los p-valores dan sobre .05, no existe evidencia para dudar de la 
# normalidad de los residuos.

# 3. Variabilidad constante de los residuos (homocedasticidad):
# Al representar los residuos frente a los valores ajustados por el modelo,
# los primeros se tienen que distribuir de forma aleatoria en torno a cero,
# manteniendo aproximadamente la misma variabilidad a lo largo del eje X.
# Si se observa algún patrón específico, por ejemplo forma cónica o mayor 
# dispersión en los extremos, significa que la variabilidad es dependiente
# del valor ajustado y por lo tanto no hay homocedasticidad.

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Tambien se puede hacer con un test
bptest(modelo) # Nohay evidencias que indicen falta de homocedastidad (p-valor>0,5)

# Ni la representación gráfica (relativamente lineal), ni el contraste de
# hipótesis muestran evidencias que haga sospechar falta de homocedasticidad.


# 4. No debe existir multicolinialidad entre los predictores del modelo

# Esta condicion se relaciona con los casos en que uno de los predictores 
# introducidos en el modelo se relaciona linealmente con otro predictor
# (la informacion de la varianza explicada por uno de los predicotrrs es
# redundante en presencia de otro)
# Esta condicion se puede verificar por medio de la inflacion de varianza (VIF)
# y estadistico de tolerancia (1/VIF).

inf.varianza <- vif(modelo)
est.tolerancia <- 1/inf.varianza

# VIF = 1: Ausencia total de colinialidad

# 1 < VIF < 5: La regresión puede verse afectada por cierta colinialidad.
# 5 < VIF < 10: Cierta colinealidad, pero dependiendo del contexto no suele ser preocupante
# Si es sobre 10, no se puede aceptar

# 0.2 < TOL: No hay motivos para preocuparse, debido a que existe un muy leve
# colinialidad-
# 0.2 > TOL: Hay motivos para preocuparse e, debido a que existe un muy alta
# colinialidad.

#Para el caso que se debe eliminar
modelo_d <- lm(VARY ~ varx1+varx2+varx3, data = datos[-c(malulo1, malulo2)])

# 5. Debe existir una relacion de independencia entre los residuos (No autocorrelacion) 
# Los residuos de cada observación debene ser independientes unos de otros, esto
# se puede comprobar empleando el test de hipótesis de Durbin-Watson.
dwt(modelo, alternative = "two.sided")

# Como es mayor a .05, se puede afirmar que no existe autocorrelacion entre
# blablabla...

# Ya que se demostro la validez de todas las condiciones, se procedera a 
# analizar la existencia de posibles valores atipicos, en donde se hara
# uso de un test que analiza las medidas potencialmente influyentes,
# el cual incluye los Laverages (hat) y distancia Hook's (cook.d) de cada 
# elemento atipico del modelo.
summary(influence.measures(model = modelo))

# Lo anterior se puede visualizar por medio de un grafico de unfluencias 
influencePlot(modelo)

# Se analizan los resultados en busqueda de
#(1)
# Si bien algunos datos superan el laverage promedio, ninguno de estos tiene un valor
# de Hook mayor a 1, por lo que no deberian causar problemas en el modelo.
#(2)
# Tomando en cuenta que ninguno de los datos superan el laverage promedio,
# y que ninguno de estos tiene un valor de Hook mayor a 1, se puede afirmar que 
# no deberian causar problemas en el modelo.


# Ahora se procedera a analizar la calidad del modelo utilizando los
# datos de prueba.

preds <- predict(m2, datos.test)
errs <- datos.test[["mpg"]] - preds
MSE <- mean(errs^2)
RMSE <- sqrt(MSE)

preds1 <- predict(m1, datos.test)
errs1 <- datos.test[["mpg"]] - preds1
MSE1 <- mean(errs1^2)
RMSE1 <- sqrt(MSE1)

cat("Error de los modelos\n")
# Error cuadratico medio
cat("MSE:", round(MSE, 3))
# Error a escala de los datos
cat("RMSE:", round(RMSE, 3))

# Error cuadratico medio
cat("MSE1:", round(MSE1, 3))
# Error a escala de los datos
cat("RMSE1:", round(RMSE1, 3))

# Concluir en base a lo anterior, mientras menor el error, mejor
summary(modelo)

# Tanto el coeficiente libre, como los parciales significativos, debido a 
# que los Pr son << .05.
# El modelo es capaz de explicar el ???% de la variabilidad observada en la VARIABLE Y
# (R2: 0.736). 
# La varianza de la variables independientes son capaces de explicar el ???% de la
# la variable dependiente. El test F muestra que es significativo << .05. 

# Con todo lo anterior expuesto, se puede afirmar que el modelo es aceptable
