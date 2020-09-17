# Plantilla para ejercicio de regresion Logistica multi

library(car)
library(gmodels)
library(ggpubr)
library(caret)
library(e1071)
library(tidyr)
library(plyr)
library(pROC)

datos <- DATOS

set.seed(100)

# Antes que nada, se aplica la funcion "factor" a las variables categoricas
datos[["CATEGORICA"]] <- factor(datos[["CATEGORICA"]])

# Para eliminar columnas malulas
datos$COLUMNAMALA <- NULL 

#Se pasan los datos a formato long para posteriormente graficarlos 
respaldo.datos <- datos
respaldo.datos.dependiente <- datos$VARY
respaldo.datos$VARY <- NULL

datos.long <- gather(respaldo.datos, key = "Medida", value = "Valor")
datos.long[["Medida"]] <- factor(datos.long[["Medida"]])
datos.long[["VARY"]] <- factor(rep(respaldo.datos.dependiente, ncol(respaldo.datos)))


#Para cambiar las categorias
# OJO: NO USAR CUANDO LAS VARIABLES SON CARACTERES
datos$VARY <- ifelse(datos$VARY==0,"CASO NEGATIVO","cASO POSITIVO")
datos.long$VARY <-ifelse(datos.long$VARY==0,"CASO NEGATIVO","cASO POSITIVO")

# Se realiza un grafico de cajas por cada variable numerica en relacion las categorias de 
# la variable de estudio. Esto con el fin de analizar si existe una posible influencia de esta.
grafico.pivote <- ggboxplot(
  data = datos.long,
  x = "VARDEPENDIENTE", y = "Valor", 
  color = "Medida", legend = "none"
)
grafico1 <- facet(grafico.pivote, facet.by = "Medida", scales = "free")
grafico1

# Para el caso de las variables categoricas, se realizara una comparativa de las frecuencias de 
# la categoria respecto a la variable de estudio


datos.CategoricaA <- count(data.frame(var_indep=datos$VARX,Categoria=datos$VARY))
graficoA <- ggplot(data=datos.CategoricaA, aes(x=var_indep, y=freq, fill=Categoria)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=freq), vjust=1, color="black",
            position = position_dodge(0.9), size=3) +
  ggtitle("CAMBIAR TITULO DE GRAFICO")
theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))


datos.CategoricaB <- count(data.frame(var_indep=datos$VARX,Categoria=datos$VARY))
graficoB <- ggplot(data=datos.CategoricaB, aes(x=var_indep, y=freq, fill=Categoria)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=freq), vjust=1, color="black",
            position = position_dodge(0.9), size=3) +
  ggtitle("CAMBIAR TITULO DE GRAFICO")
theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

all.plot <- ggarrange(graficoA, graficoB)



# Concluir en base a los graficos.


#############ESTO NO VA EN EL CODIGO

# OJO: La variable dependiente tiene que estar si o si en 0 - 1
datos$VARY <- ifelse(datos$VARY=="CASO NEGATIVO",0,1)
total_cochinon <- glm(VARY ~ ., family = binomial(link = "logit"), data = datos)

total <- glm(
  VARY ~ VARX1 + VARX2+ VARX3 +VARX4 + VARX5,
  family = binomial(link = "logit"),
  data = datos
)

nulo <- glm(
  VARY ~ 1,
  family = binomial(link = "logit"),
  data = datos
)

modelo <- step(nulo, scope = list(lower = nulo, upper = total),
              direction = "forward", test = "LRT", trace = 1)
#############ESTO NO VA EN EL CODIGO

############# HASTA ESTE PUNTO, SE TIENE IDENTIFICADA LAS VARIABLES

# OJO: A ESTE SE LE AGREGA UNA VARIABLE CHALLA (Que tenga sentido con el contexto)
# En lo anterior, se puede ver que la clasificacion de la varaible VARY esta 
# influenciada por las observacione de las variables VARX1, VARX2, VARX3 y VARX4
# Por lo que, se procedera a crear un modelo RL con los anteriores.
modelo_a <- glm(
  VARY ~ varx1+ varx2+ varx3+ varx4,
  family = binomial(link = "logit"), 
  data= datos) 

# Ahora se analiza el grado de significancia de los predictores
summary(modelo_a)

# Se observa que varx4 tiene un valor Pr >> .05, por lo que se analizara que 
# sucede con el modelo si este se elimina.
modelo_b <- update(modelo_a, . ~ . - varx4)

# Se aplicaran dos criterios para comparar los modelos, el primero Likelihood
# ratio se encargara de calcular la significancia de la diferencia de residuos
# de ambos modelos, y el segundo, el metodo AIC, el cual analiza si un modelo
# mejora o empeora su eficacia en predecir la variable de respuesta.

print(anova(modelo_a, modelo_b, test = "LRT"))

# Ya que el Alpha << .05, se puede rechazar la hipotesis nula, por ende se puee
# concluir de que existe una diferencia entre la variable explicada de un
# modelo, por sobre otro (un modelo es considerablemente mejor).


# Aplicando el criterio matematico del AIC  Este criterio
# toma la AIC menor como mejor opcion.
AIC(modelo_a, modelo_b)

# Por principio de parsimonia, se prefiere sacar la variable, ya que esta no se le afecta
# significativamente su calidad.

#Concluir en el momento

##### Ahora se tiene listo el modelo

modelo <- modelo_b



# Ahora para aceptar el modelo, es necesario que este cumpla con las siguientes
# condiciones:
# 1. Linealidad
# Debe existir una relaci�n lineal entre el logaritmo natural de odds y la variable
# continua, esto se puede verificar realizado un analisis entre los predictores
# y su transformacion logaritmica, en busqueda de una relacion significativa
#OJO: Tienen que llevar si o si log y Int
datos.pivote <- datos
datos.pivote$log_VARX1_Int <- log(datos.pivote$VARX1) * datos.pivote$VARX1
datos.pivote$log_VARX2_Int <- log(datos.pivote$VARX2) * datos.pivote$VARX2

# Luego se adieren a un hipotetico modelo RL, el cual contiene las variables iniciales
# junto con los valores obtenidos anteriormente.
modelo_hipotetico <- glm(VARY ~ VARX1 + VARX2 + log_VARX1_Int + log_VARX2_Int, 
                         data = datos.pivote, 
                         family = binomial(link = "logit"))
summary(modelo_hipotetico)
# Como los Pr son > .05, se puede aceptar la condicion de linealidad.


# 2. Independencia del error
# Esta condicion indica que no debe existir autocorrelacion entre los residuos
# del modelo, lo anterior se puede verificar por medio de la prueba Darbin-Watson.

#print(durbinWatsonTest(modelo, max.lag = 5))
dwt(modelo, alternative = "two.sided")

# Como es mayor a .05, se puede afirmar que no existe autocorrelacion entre
# blablabla...


##### ##### ##### ##### ##### 
##### OJO: APLICAR ESTO SOLO CON COMBINACION DE CATEGORICAS
##### TAMBIEN SE PUEDE ANALIZAR SOLO EL CASO DE LA CATEGORIA DEPENDIENTE
# 3. Informarcion incompleta
# Esta condicion hace referencia a que deben existir una proporcion equitativa
# entre los representantes de cada posible combinacion de predictores.
# Se realizara una crossTable, para analizar lo anterior.

#Variable independiente categorica + Variable dependiente categorica
CrossTable(
  datos$VARX, datos$VARY,
  format = "SPSS", expected = TRUE
)
# Se observa una equitativa distribucion entre los % de cada combinacion de
# predictores
##### ##### ##### ##### ##### 


# 4. Separacion perfecta
# Esta situacion ocurre cuando una variable separa perfectamente las clases
# del modelos. Para verificar esto, podemos valernos de un posible Warning que
# nos entrega R al momento generar el modelo, el cual nos advierte de una
# posible explicacion directa de una variable dependiente para una de las
# variables predictoras (Todos los Pr se aproximan a 1)
summary(modelo)

# Analizando el summary del modelo, se puede ver que todos los Pr son distintos
# de 1, por lo que se acepta esta condicion.


# 5. No debe existir multicolinialidad entre los predictores del modelo

# Esta condicion se relaciona con los casos en que uno de los predictores 
# introducidos en el modelo se relaciona linealmente con otro predictor
# Esta condicion se puede verificar por medio de la inflacion de varianza (VIF)
# y estadistico de tolerancia (1/VIF).

inf.varianza <- vif(modelo)
cat("VIF:", round(inf.varianza, 3))
est.tolerancia <- 1/inf.varianza
cat("TOL:", round(est.tolerancia, 3))

# VIF = 1: Ausencia total de colinialidad

# 1 < VIF < 5: La regresión puede verse afectada por cierta colinialidad.
# 5 < VIF < 10: Cierta colinealidad, pero dependiendo del contexto no suele ser preocupante
# Si es sobre 10, no se puede aceptar

# 0.2 < TOL: No hay motivos para preocuparse, debido a que existe un muy leve
# colinialidad-
# 0.2 > TOL: Hay motivos para preocuparse e, debido a que existe un muy alta
# colinialidad.

# Se crean los datos de prueba.
set.seed(49)
i.para.entrenar <- createDataPartition(
  y = datos[["VARY"]],
  times = 1,
  p = .5, 
  list = FALSE
)
i.para.entrenar <- c(i.para.entrenar)
i.para.probar <- (1:nrow(datos))[-i.para.entrenar]

datos.train <- datos[i.para.entrenar, ]
datos.test  <- datos[-i.para.probar, ]

# Se indica por factor, que la variable dependiete tiene dos niveles
datos.test[["VARY"]] <- factor(datos.test$VARY, levels = c("CASO NEGATIVO", "CASO POSITIVO"))

# OJO
# Realizar esto solo si las variables dependientes estan con 0-1
datos.test$VARY <-ifelse(datos.test$VARY==0,"CASO NEGATIVO","CASO POSITIVO")

################################################
########## SE ELIGE EL MEJOR
# Por ejemplo, podemos indicar al algoritmo que no haga nada y use todos
# los datos proporcionados.
control4 <- trainControl(method = "none")

# De hecho, podemos usar el área bajo la curva (AUC) ROC como criterio
# para seleccionar el mejor modelo.
control5 <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

# También podemos conseguir un modelo usando validación cruzada.
# Aqu�?, como son 30 datos, haremos 6 folds de 5 casos cada uno.
control6 <- trainControl(
  method = "cv",
  number = 6,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

# Y podemos llevar la validación cruzada al extremo, usando todos menos
# un caso para entrenamiento. 
control7 <- trainControl(
  method = "LOOCV",
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)
################################################


# OJO: Tienen que ser caracteres (no niveles 0-1)
# OJO: Todos los categoricos tienen que tener su respestvo factor
modelo_entrenado <- train(
  VARY ~ VARX1 + VARX2,
  data = datos.train,
  method = "glm",
  trControl = control4
)


#######
# En el caso de que se necesite predecir directamente desde el modelo (no el entrenado)
prediccion <- predict(modelo, datos.test, type = "response")
#######

# # # # # # # # # # # # # 
# SIEMPRE QUE SE CAMBIEN LOS DATOS CATEGORICOS, ES NECESARIO TIRAR UN FACTOR
# Para cambiar los datos
datos$VARY <- ifelse(datos$VARY==0,"CASO NEGATIVO","cASO POSITIVO")
predicciones <- factor(predicciones, levels = c("CASO NEGATIVO", "CASO POSITIVO"))
# Para aplciar factor

# # # # # # # # # # # # # 



# Predecir con el modelo entrenado (objeto que entrega el train)
predicciones <- predict(modelo_entrenado, newdata = datos.test)
predicciones <- factor(predicciones, levels = c("CASO NEGATIVO", "CASO POSITIVO"))


# Tienen que ser caracteres
matriz.confusion <- confusionMatrix(
  data = predicciones,
  reference = datos.test[["VARY"]],
  positive = "CASO POSITIVO"
)

# Se concluye algo como esto.
#   Specificity : 0.8902   
#   Sensitivity : 0.7078   
#   Accuracy : 0.82 
#   No Information Rate : 0.615

# Sobre el accuracy, es el porcentaje de casos que fueron clasificados de forma correcta
# sin importar si son negativos o positivos.

# Sobre el miss clasification rate (No information rate) es el porcentaje de casos que
# NO fueron clasificados de forma correcta sin importar si son negativos o positivos.

# Sensibilidad (Sensitivity), es el porcentaje que es realmente positivo, respecto al
# total de positivos (claasificados y reales positivos)

# Especificidad (Specificity), es el porcentaje que es realmente negativo, respecto al
# total de negativos (clasificados y reales negativos)


# Para graficar ROG, se necesita cambiar los valores de niveles de la variable 
# dependiente. Esto a razon de: 0: Caso negativo y 1: Caso positivo
predicciones <-ifelse(predicciones=="CASO NEGATIVO",0,1)
datos.test$VARY <-ifelse(datos.test$VARY=="CASO NEGATIVO",0,1)

predicciones_prob <- predict(modelo_entrenado, newdata = datos.test, type = "prob")


# OJO: Solo tiene que ser PROB los casos de las predicciones
modelo.roc <- roc(datos.test[["VARY"]], predicciones_prob[["CASO POSITIVO"]],
                   percent = TRUE, print.auc = TRUE)
plot(modelo.roc)
# Explicar modelo ROC

# Como se puede ver en la curva ROG, los datos correspondientes al cruze de sensitividad
# especificidad estan por sobre la diagonal de referencia, lo que indica que las predicciones
# del modelo son bastantes cercanas a la realidad, por ende, es un buen modelo.

# Graficar el modelo

# Para graficar un RL
#plot(modelo)
summary(modelo)

# Concluir el modelo








