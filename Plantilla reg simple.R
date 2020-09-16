# Plantilla para ejercicio de regresion simple
library(car)
library(ggplot2)
library(ggpubr)
library(lmtest)

#########Cambiar titulos y variables##########

# Se utiliza el grafico de dispersión con el fin de analizar visualmente si existe 
# un comportamiento de relación lineal entre las VARIABLEX e VARIABLEY
ggplot(data = datos, mapping = aes(x = numero_bateos, y = runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Diagrama de dispersión', x  =  'número  de bateos') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Observar: 
#   Puntos muy alejados
#   Puntos muy juntos
#   Traslape

# Se procede a cuantificar el grado de relación entre las dos variables, por
# medio de la covarianza. Para ello, se utilizará "Coeficiente de Pearson"
# Pero antes de usarlo, se tiene que verificar el cumplimiento de las siguientes
# condiciones.

# 1. Las variables presentan una relación lineal
# Como se ve en el grafico de dispersion, las variables tienen una clara 
# relacion lineal.


#########EDITAR ESTO A RAZON DE LA VARIABLE#########
# 2. La naturaleza de las variables presenta una escala de intervalo
# Las variables presentan esta propiedad, debido a que se tratan de elementos que pueden
# ser ordenados en rangos de valores, como el PESO en donde 2 es la mitad de 4, y
# el doble de 1.

#########EDITAR ESTO A RAZON DE LA VARIABLE#########
# 3. Las variables presentan una distribución normal.
# Para verificar esta condicion, se realizara un grafico Quintil-Quintil, junto
# a un test normalidad para cada variable.
qqnorm(DATOS$VARIABLEX, main = "variable X", col = "darkred")
qqline(DATOS$VARIABLEX)
qqnorm(DATOS$VARIABLEY, main = "variable Y", col = "blue")
qqline(DATOS$VARIABLEY)

# A partir del grafico, se puede ver que las observaciones se distribuyen de
# una forma uniforme a lo largo de la linea, lo que se puede interpretar como
# un comportamiento relativamente normal.
# Pero aun asi, esto se corroborara una con test de normalidad Shapiro-Wilk
shapiro.test(DATOS$VARIABLEX)
shapiro.test(DATOS$VARIABLEY)
# Ambos p-valores dan por sobre los .05, por tanto, no hay pruebas suficientes para
# demostrar que las variables no vienen de una población con distribución normal.

#########EDITAR ESTO A RAZON DE LA VARIABLE#########
# 4. Los datos presentan homocedasticidad
# Esto se puede verificar analizando el grafico de dispersion, si los puntos
# muestran una forma "canonica".
ggplot(data = Cars93, aes(x = Weight, y = Horsepower)) + 
  geom_point(colour = "red4") +
  ggtitle("Diagrama de dispersión") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#########EDITAR ESTO A RAZON DE LA VARIABLE#########
# 5. Existe una ausencia, o leves casos, de valores atipicos.
# Utilizando el grafico de cajas, se puede visualizar de una forma mas clara
# los valores atipicos de los datos.
boxplot(datos$VARX)
boxplot(datos$VARY)


# Se procede a realizar el test de correlacion:

#########SI NO CUMPLE NORMALIDAD#########
# Como no se cumple la condicion de normalidad, se procedera a ralizar métodos no 
# parametricos
#########EDITAR ESTO A RAZON DE LA VARIABLE#########
# Coeficiente de Spearman
# Como los datos son continuos, ordinales y transformables en rangos, se puede utilizar
# como alternativa el coeficiente de Spearman. Este se utiliza cuando el numero
# observariones es bajo y traslape es grande
a <- cor.test(estatura, peso, conf.level = 0.95, method = "spearman")

#########EDITAR ESTO A RAZON DE LA VARIABLE#########
#Coeficiente de Kendall
# Como los datos son ordinales y transformables en rangos, se puede utilizar
# como alternativa no paramétrica el coeficiente de Kendall. 
a <- cor.test(estatura, peso, conf.level = 0.95, method = "kendall")

#########EDITAR ESTO A RAZON DE LA VARIABLE#########
# Coeficiente de Pearson
# Como se cumplen todas las condiciones, se procederá a aplicar el test de Pearson
a <- cor.test(estatura, peso, conf.level = 0.95, method = "pearson")

# El p-valor obtenido es > .05, lo que indica que la verdadera correlacion
# no es igual a 0, por lo que esta es significativa
# Por otro lado, el coeficiente de correlacion es cercano a 1, lo que indica
# que la relacion es muy alta.

# Por tanto, es posible aplicar regresión lineal, ya que se cumplieron todas las
# condiciones
modelo <- lm(Weight ~ Height, datos)
summary(modelo)

# Los valoes de los coeficientes de B0 y B1 son altamente significativos
# para el modelo, debido a que sus Pr < .05
# El p-value obtenido en el test F (??????) nos indica que es significativamente
# superior la varianza explicada por el modelo en comparación a la varianza
# total. Por ende, nuestro modelo es significativo y por lo tanto se puede 
# aceptar.
# El valor de R2 indica que el modelo calculado explica el ????% de la
# variabilidad presente en la variable respuesta  mediante la variable
# independiente.


#Para aceptar el modelo, este tiene que cumplir con las siguientes condiciones:

# 1. Debe existir linealidad entre la variable dependiente e independiente
# Este punto se cumple si los residuos se distribuyen aleatoriamente alrededor
# del 0. Esto se puede verificar visualmente por medio de un grafico de
# dispersion.

datos$prediccion <- modelo_lineal$fitted.values
datos$residuos   <- modelo_lineal$residuals

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# 2. Debe existir una relacion de independencia entre los residuos
# Para este punto, se analiza que los residuos no siguan un estandar o patron.
# Debido al orden de los puntos (obtenidos en el grafico del punto anteior)
# se puede intuir que no siguen patron alguno.

# 3. Los residuos se tienen que distribuir de forma normal.
# Para verificar esta condicion, se realizara un grafico Quintil-Quintil, junto
# a un test normalidad para todos los residuos del modelo
qqnorm(modelo_lineal$residuals)
qqline(modelo_lineal$residuals)
shapiro.test(modelo_lineal$residuals)

# Se puede ver graficamente que la distribucion de los residuos se distribuye
# de forma normal (alrededor de la linea), y ahora lo contrastaremos con un
# test de analisis de normalidad Shapiro-Wilk 
shapiro.test(DATOS$VARIABLEX)
shapiro.test(DATOS$VARIABLEY)
# Como los p-valores dan sobre .05, no existe evidencia para dudar de la 
# normalidad de los residuos


# 4. Debe existir homocedasticidad en los residuos.
# Para verificar esta condicion, se procedera a ralizar un grafico de
# dispersion de la distribucion de la varianza de los residuos, en busqueda
# de que esta sea constante a lo largo del eje x (homnogeneidad de la varianza)

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  geom_smooth(se = FALSE, color = "firebrick") +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#Con test de breush-pagan
bptest(modelo_lineal)

# Ni la representación gráfica (relativamente lineal), ni el contraste de
# hipótesis muestran evidencias que haga sospechar falta de homocedasticidad.

# 5. Autocorrelación de residuos:
# Cuando se trabaja con intervalos de tiempo, es muy importante comprobar que no existe 
# aoutocorrelación de los residuos, es decir que son independientes. Esto puede hacerse detectando 
# visualmente patrones en la distribución de los residuos cuando se ordenan según se han registrado 
# o con el test de Durbin-Watson

ggplot(data = datos, aes(x = seq_along(residuos), y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_line(size = 0.3) +
  labs(title = "Distribución de los residuos", x = "index", y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# En este caso, la representación de los residuos no muestra ninguna tendencia


# 6. Existe una ausencia, o leves casos, de valores atipicos.
# Para comprobar este punto, se hara uso del grafico de medidas potencialmente influyentes,
# el cual, ayuda a determinar cuales valores se alejan del valor esperado por el modelo.

influencePlot(model = modelo_lineal)
summary(influence.measures(modelo_lineal))

# Este nos ayuda a identificar las observaciones mas influyentes, mostrandolas
# con un circulo de mayor tamaño

# En donde se indentifica lo siguiente:
# El valor ___ se detecta como atipico, pero no influyente, por tanto se conseva.
# El valor ___ se detecta como atipico, y influyente, por tanto se elimina

# Se obtiene un nuevo modelo, con los elementos atipicos eliminados:
modelo_sin.atipicos <- lm(Weight ~ Height, datos[c(-130,-162)])


summary(modelo_lineal)

# Tanto el coeficiente libre, como el que explica la varianza de la independiente
# son significativos, debido a que los Pr son << .05
# El modelo es capaz de explicar el ???% de la variabilidad observada en la VARIABLE Y
# (R2: 0.736). 
# La varianza de la variable independiente es capaza de explicar el ???% de la
# la variable dependiente. El test F muestra que es significativo << .05. 
# Se satisfacen todas las condiciones para este tipo de regresión simple y de correlacion.
