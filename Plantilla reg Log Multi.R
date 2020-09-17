# Plantilla para ejercicio de regresion Logistica multi

library(car)
library(gmodels)
library(ggpubr)
library(caret)
library(e1071)
library(tidyr)
library(plyr)
library(pROC)


respaldo1 <- datos.wide
respaldo2 <- datos.wide


datos.generales <- DATOS
total.datos <- nrow(datos.generales)

set.seed(100)
# Se separan XXX datos para el entrenamiento, y YYY datos para la prueba
i.train <- sample(1:total.datos, 20)
datos <- datos.generales[i.train, ]
datos.test <- datos.generales[-i.train, ]

# Antes que nada, se aplica la funcion "factor" a las variables categoricas
datos[["CATEGORICA"]] <- factor(datos[["CATEGORICA"]])
datos.test[["CATEGORICA"]] <- factor(datos.test[["CATEGORICA"]])
stopifnot(levels(datos[["CATEGORICA"]]) == levels(datos.test[["CATEGORICA"]]))

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
datos$VARY <- ifelse(datos.wide$VARY==0,"Normal","Sobrepeso")


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

datos.VARY.CategoricaA <- count(data.frame(VariableX=datos$VARX,categoria=datos$VARY))
graficoA <- ggplot(data=datos.VARY.CategoricaA, aes(x=VARX, y=freq, fill=VARY)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=freq), vjust=1, color="black",
            position = position_dodge(0.9), size=3) +
  ggtitle("Frecuency Plot for sex")
theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))


datos.VARY.CategoricaB <- count(data.frame(VariableX=datos$VARX,categoria=datos$VARY))
graficoA <- ggplot(data=datos.VARY.CategoricaB, aes(x=VARX, y=freq, fill=VARY)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=freq), vjust=1, color="black",
            position = position_dodge(0.9), size=3) +
  ggtitle("Frecuency Plot for sex")
theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))

all.plot <- ggarrange(graficoA,graficoB)

# Concluir en base a los graficos.


#############ESTO NO VA EN EL CODIGO
total_cochinon <- glm(VARY ~ ., family = binomial(link = "logit"), data = datos)

total <- glm(
  VARY ~ VARX1 + VARX2+ VARX3 +VARX4 + VARX5,
  family = binomial(link = "logit"),
  data = datos
)

nulo <- glm(
  VARY ~ 1,
  family = binomial(link = "logit"),
  data = datos.wide
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
modelo_b <- update(modelo_a, . ~ . + varx4)

# Se aplicaran dos criterios para comparar los modelos, el primero Likelihood
# ratio se encargara de calcular la significancia de la diferencia de residuos
# de ambos modelos, y el segundo, el metodo AIC, el cual analiza si un modelo
# mejora o empeora su eficacia en predecir la variable de respuesta.

print(anova(modelo_a, modelo_b, test = "LRT"))
#Mientras mas baja la desviacion, mejor

# Aplicando el criterio matematico del AIC  Este criterio
# toma la AIC menor como mejor opcion.
AIC(modelo, modelo_b, modelo_c)

#Concluir en el momento

##### Ahora se tiene listo el modelo

modelo <- modelo_b



# El modelo esta definido para este punto

# Se realizara un featurePlot Para analizar el traslape entre las variables
# predictoras
vps <- c("Hip.Girth", "Chest.Girth", "Calf.Maximum.Girth", "Waist.Girth")
pl1 <- featurePlot(datos.wide[, vps], datos.wide[["categoria"]], plot = "pairs")

# Concluir rapidamente



# Ahora para aceptar el modelo, es necesario que este cumpla con las siguientes
# condiciones:
# 1. Linealidad

# Esto se puede verificar realizado un analisis entre los predictores
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
##### OJO APLICAR ESTA CONDICION SOLO SI SE TIENEN 2 CATEGORIAS (INCLUYENDO LA Y)
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
  y = datos.wide[["categoria"]],
  times = 1,
  p = .5, 
  list = FALSE
)
i.para.entrenar2 <- c(i.para.entrenar)
i.para.probar2 <- (1:nrow(datos.wide))[-i.para.entrenar]
datos.train2 <- datos.wide[i.para.entrenar2, ]
datos.test2  <- datos.wide[-i.para.probar2, ]

set.seed(49*29)
i.para.entrenar <- createDataPartition(
  y = datos.wide[["categoria"]],
  times = 1,
  p = .5, 
  list = FALSE
)
i.para.entrenar1 <- c(i.para.entrenar)
i.para.probar1 <- (1:nrow(datos.wide))[-i.para.entrenar]
datos.train1 <- datos.wide[i.para.entrenar2, ]
datos.test1<- datos.wide[-i.para.probar2, ]



datos.test1[["categoria"]] <- factor(datos.test1$categoria, levels = c("Normal", "Sobrepeso"))
datos.test1$categoria <-ifelse(datos.test1$categoria==0,"Normal","Sobrepeso")

# OJO: Tienen que ser caracteres (no niveles 0-1)
modelo_entrenado <- train(
  categoria ~ Hip.Girth + Chest.Girth,
  data = datos.test1,
  method = "glm"
)


#######
# En el caso de que se necesite predecir directamente desde el modelo
prediccion1 <- predict(modelo_entrenado, datos.test1, type = "response")
#######

# Predecir con el objeto que entrega train
predicciones1 <- predict(modelo_entrenado, newdata = datos.test2)
datos.test1$categoria <- ifelse(datos.wide$datos.test1 == 1,"Sobrepeso","Normal")

predicciones1 <- factor(predicciones1, levels = c("Normal", "Sobrepeso"))
datos.test1$categoria <- factor(datos.wide$categoria, levels = c("Normal", "Sobrepeso"))

# Tienen que ser caracteres
matriz.confusion1 <- confusionMatrix(
  data = predicciones1,
  reference = datos.test1[["categoria"]],
  positive = "Sobrepeso"
)
# Se concluye algo como esto.
# Vemos que el modelo ahora alcanza un 81,3% de aciertos, con un
# 66,7% de sensibilidad y muy buena especificidad (90,0%).
# Accuracy
# Miss clasification rate
# Specifity


predicciones1 <-ifelse(predicciones1=="Normal",1,0)
datos.test1$categoria <-ifelse(datos.test1$categoria=="Normal",1,0)

# OJO: Tienen que ser numueros (0-1)
modelo1.roc <- roc(datos.test1[["categoria"]], predicciones1,
                   percent = TRUE, print.auc = TRUE)
plot(modelo1.roc)

# Explicar modelo ROC


# Concluir el modelo





