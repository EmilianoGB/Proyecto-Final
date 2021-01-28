rm(list = ls())
############################################
#Modelado
############################################
#Librerías
library(tidyverse)
library(caret)
library(broom)
library(mice)
library(MASS)
library(corrplot)
library(car)
library(DataExplorer)
library(recipes)
library(lmtest)
library(ISLR)
library(leaps)
library(ranger)
setwd("C:/Users/emili/Desktop/ORT Analitica/Proyecto/datos")
getwd() 
############################################################
## PROMOTORES LOGISTIC MODELO DE REGRESION LINEAL MULTIPLE
############################################################
#Importacion del dataset final
datos<- readRDS("datos_ModeladoL.rds")
# Dejar los datos con nombres para aplicar cualquier algoritmo
names(datos) <- make.names(names(datos))
#Se utilizan las observaciones completas
datos <- datos[complete.cases(datos),]
# Ver la correlación lineal con Peso romotores
ParaCor <- datos %>% select_if(is.numeric)   
#Correlaciones contra peso promotores numericas
cor(ParaCor[c(18,19)], ParaCor[-1], use="complete.obs") %>% View()
# Mas correlacionadas son AVG Rating, Tiempo promedio de llegada tarde y Vouchers promedio. 
#Correlaciones entre candidatos
datos %>% select_if(is.numeric) %>% dplyr::select(-c(1,18,19)) %>% plot_correlation( cor_args = list("use" = "pairwise.complete.obs"))
# Alta corelacion entre las variables de chats y rlcionadas con Vouchers pueden indicar multicolinealidad.
# Antes de pasar al entrenamiento del modelo se Estandarizan los predictores para mejorar la interpretacion de los mismos.
varss <- datos %>% select_if(is.numeric) %>% dplyr::select(-c(1,18,19)) %>% names()
 for (i in varss){
   datos[[i]] <- scale(datos[[i]])
 }
# Verificacion
summary(datos)
# Estimacion por cross Validation
#Se eliminan las columnas que no son predictores
datos = datos[,-c(1,2,29)]
#Se eliminan las correlaciones altas
corr_altos <- datos %>% select_if(is.numeric) %>% dplyr::select(-17) %>% cor(use = "complete.obs") %>% findCorrelation(cutoff = 0.56, names = TRUE)
#Se cambian la variable avg vendor late por delivery promedio ya que está mas relacionada con la variable objetivo
corr_altos[4] = names(datos)[13]
#elimino variables altamente correlacionadas
datos <- datos %>%  dplyr::select(-corr_altos)
# Es importante que la Asignación aleatoria de K grupos
grupo <- sample(rep(x = 1:10, length = nrow(datos))) 
#Se comprueba que la distribución es aproximadamente equitativa
table(grupo)

# Dado un objeto creado por la función regsubsets(), que es una lista de
# modelos, y un nuevo set de observaciones, la función predict.regsubsets() 
# devuelve las predicciones para cada uno de los modelos.

predict.regsubsets  <- function(object, newdata, id, ...){
  # Extraer la fórmula del modelo (variable dependiente ~ predictores)
  form <- as.formula(object$call[[2]])
  # Generar una matriz modelo con los nuevos datos y la fórmula
  mat <- model.matrix(form, newdata)
  # Extraer los coeficientes del modelo
  coefi <- coef(object , id = id)
  # Almacenar el nombre de las variables predictoras del modelo
  xvars <- names(coefi)
  # Producto matricial entre los coeficientes del modelo y los valores de
  # los predictores de las nuevas observaciones para obtener las 
  # predicciones
  mat[ , xvars] %*% coefi
}

# Matriz que almacena los test-error estimados. Cada columna representa un
# modelo. Cada fila representa uno de los 10 grupos en los que se han dividido
# las observaciones.
error_matrix <- matrix(data = NA, nrow = 10, ncol = 22,
                       dimnames = list(NULL, c(1:22)))

# Loop en el que se excluye en cada iteración un grupo distinto
# Loop para 21 posibles pdictores
num_validaciones <- 10
num_predictores <- 22

for (k in 1:num_validaciones) {
  # Identificación de datos empleados como training
  train <- datos[grupo != k, ]
  # Selección de los mejores modelos para cada tamaño basándose en RSS
  mejores_modelos <- regsubsets(peso_promotores~., data = train, nvmax = 22,
                                method = "forward")
  # Para cada uno de los modelos "finalistas" se calcula el test-error con
  # el grupo excluido
  for (i in 1:num_predictores) {
    test <- datos[grupo == k, ]
    # Las predicciones del modelo i almacenado en el objeto regsubsets se
    # extraen mediante la función predict.regsubsets() definida arriba
    predicciones <- predict.regsubsets(object = mejores_modelos,
                                       newdata = test, id = i)
    # Cálculo y almacenamiento del MSE para el modelo i
    error_matrix[k,i] <- mean((test$peso_promotores - predicciones)^2)
  }
}

# Cada columna de la matriz error_matrix contiene los 10 valores de error
# calculados por cv
mean_cv_error <- apply(X = error_matrix, MARGIN = 2, FUN = mean)
# plot(sqrt(mean_cv_error), type = "b", pch = 19)
which.min(x = mean_cv_error)

ggplot(data = data.frame(n_predictores = 1:22, mean_cv_error = mean_cv_error),
       aes(x = n_predictores, y = mean_cv_error)) +
  geom_line() +
  geom_point() +
  geom_point(aes(x = n_predictores[which.min(mean_cv_error)],
                 y = mean_cv_error[which.min(mean_cv_error)]),
             colour = "red", size = 3) +
  scale_x_continuous(breaks = c(0:22)) +
  theme_bw() +
  labs(title = "Cross-validation mean error vs número de predictores Promotores Logistic",
       x = "número predictores")

#El mejor modelo identificado mediante 10-Cross-Validation es el formado por 11 predictores.

modelo_final <- regsubsets(peso_promotores~., data = datos, nvmax = 21,
                           method = "forward")
coef(object = modelo_final, 11) 

Modelo_Promotores_Logistic_LM <- lm(peso_promotores~ Restaurant.Total.Pics.BO + 
                                      Restaurant.Avg.Rating+ Voucher_rate + peso_triggers+ 
                                      chatstransf_rate + Orders.Avg.Vendor.Late..Min.+
                                      reviews_rate + Antiguedad+ Atencion_Ciudad+
                                      Categoria.agrupada,  data=datos)                      
                                      
RLPL <- summary(Modelo_Promotores_Logistic_LM)
sink("RLPL.png")
print(RLPL)
sink() 
#Validación de condiciones para la regresión múltiple lineal
#Multicoliealiad
vif(Modelo_Promotores_Logistic_LM)
#Analisis de Residuos
#Si la relación es lineal, los residuos deben de distribuirse aleatoriamente en torno a 0 con una variabilidad constante a lo largo del eje X
par(mfrow=c(2,2))
plot(Modelo_Promotores_Logistic_LM,1)
#Distribución normal de los residuos:
plot(Modelo_Promotores_Logistic_LM,2)
shapiro.test(Modelo_Promotores_Logistic_LM$residuals)
#Tanto el análisis gráfico como es test de hipótesis confirman falta de normalidad.
#Variabilidad constante de los residuos (homocedasticidad):
plot(Modelo_Promotores_Logistic_LM,3)
bptest(Modelo_Promotores_Logistic_LM)
#El análii gráfico y el test de hipotesis indican falta de homoscedasticidad
plot(Modelo_Promotores_Logistic_LM, 4)
#Importancia de las variables
varImp(Modelo_Promotores_Logistic_LM, scale = FALSE) %>% rownames_to_column() %>%  arrange(desc(Overall))
rm(list = ls())
#############################################################
## DETRACTORES LOGISTIC MODELO DE REGRESION LINEAL MULTIPLE
#############################################################
#Importacion del dataset final
datos<- readRDS("datos_ModeladoL.rds")
# Dejar los datos con nombres para aplicar cualquier algoritmo
names(datos) <- make.names(names(datos))
#Se utilizan las observaciones completas
datos <- datos[complete.cases(datos),]
#Se toman solo las obervaciones con un peso detractr mayor a 0
datos <- datos %>% filter(peso_detractores>0)
# Antes de pasar al entrenamiento del modelo se Estandarizan los predictores para mejorar la interpretacion de los mismos.
varss <- datos %>% select_if(is.numeric) %>% dplyr::select(-c(1,18,19)) %>% names()
for (i in varss){
  datos[[i]] <- scale(datos[[i]])
}
# Verificacion
summary(datos)
# Estimacion por cross Validation
#Se eliminan las columnas que no son predictores
datos = datos[,-c(1,2,28)]
#Se eliminan las correlaciones altas
corr_altos <- datos %>% select_if(is.numeric) %>% dplyr::select(-17) %>% cor(use = "complete.obs") %>% findCorrelation(cutoff = 0.56, names = TRUE)
#Se cambian la variable avg vendor late por delivery promedio ya que está mas relacionada con la variable objetivo
corr_altos[4] = names(datos)[13]
#elimino variables altamente correlacionadas
datos <- datos %>%  dplyr::select(-corr_altos)
# Es importante que la Asignación aleatoria de K grupos
grupo <- sample(rep(x = 1:10, length = nrow(datos))) 
#Se comprueba que la distribución es aproximadamente equitativa
table(grupo)
# Dado un objeto creado por la función regsubsets(), que es una lista de
# modelos, y un nuevo set de observaciones, la función predict.regsubsets() 
# devuelve las predicciones para cada uno de los modelos.
predict.regsubsets  <- function(object, newdata, id, ...){
  # Extraer la fórmula del modelo (variable dependiente ~ predictores)
  form <- as.formula(object$call[[2]])
  # Generar una matriz modelo con los nuevos datos y la fórmula
  mat <- model.matrix(form, newdata)
  # Extraer los coeficientes del modelo
  coefi <- coef(object , id = id)
  # Almacenar el nombre de las variables predictoras del modelo
  xvars <- names(coefi)
  # Producto matricial entre los coeficientes del modelo y los valores de
  # los predictores de las nuevas observaciones para obtener las 
  # predicciones
  mat[ , xvars] %*% coefi
}

# Matriz que almacena los test-error estimados. Cada columna representa un
# modelo. Cada fila representa uno de los 10 grupos en los que se han dividido
# las observaciones.
error_matrix <- matrix(data = NA, nrow = 10, ncol = 22,
                       dimnames = list(NULL, c(1:22)))

# Loop en el que se excluye en cada iteración un grupo distinto
# Loop para 21 posibles pdictores
num_validaciones <- 10
num_predictores <- 22

for (k in 1:num_validaciones) {
  # Identificación de datos empleados como training
  train <- datos[grupo != k, ]
  # Selección de los mejores modelos para cada tamaño basándose en RSS
  mejores_modelos <- regsubsets(peso_detractores~., data = train, nvmax = 22,
                                method = "forward")
  # Para cada uno de los modelos "finalistas" se calcula el test-error con
  # el grupo excluido
  for (i in 1:num_predictores) {
    test <- datos[grupo == k, ]
    # Las predicciones del modelo i almacenado en el objeto regsubsets se
    # extraen mediante la función predict.regsubsets() definida arriba
    predicciones <- predict.regsubsets(object = mejores_modelos,
                                       newdata = test, id = i)
    # Cálculo y almacenamiento del MSE para el modelo i
    error_matrix[k,i] <- mean((test$peso_detractores - predicciones)^2)
  }
}

# Cada columna de la matriz error_matrix contiene los 10 valores de error
# calculados por cv
mean_cv_error <- apply(X = error_matrix, MARGIN = 2, FUN = mean)
# plot(sqrt(mean_cv_error), type = "b", pch = 19)
which.min(x = mean_cv_error)

 ggplot(data = data.frame(n_predictores = 1:22, mean_cv_error = mean_cv_error),
       aes(x = n_predictores, y = mean_cv_error)) +
  geom_line() +
  geom_point() +
  geom_point(aes(x = n_predictores[which.min(mean_cv_error)],
                 y = mean_cv_error[which.min(mean_cv_error)]),
             colour = "red", size = 3) +
  scale_x_continuous(breaks = c(0:22)) +
  theme_bw() +
  labs(title = "Cross-validation mean error vs número de predictores Detractores Logistic",
       x = "número predictores")

#El mejor modelo identificado mediante 10-Cross-Validation es el formado por 9 predictores. Finalmente se identifica el mejor modelo formado por 9 predictores empleando todas las observaciones (training + test).

modelo_final <- regsubsets(peso_detractores~., data = datos, nvmax = 22,
                           method = "forward")
coef(object = modelo_final, 10) 

Modelo_Detractores_Logistic_LM <- lm(peso_detractores ~ Restaurant.Total.Pics.BO + 
                                      Restaurant.Avg.Rating + 
                                      Restaurant.Accepts.Vouchers..Yes...No. +
                                       Orders.Avg.Vendor.Late..Min.+ 
                                      Voucher_rate + fail_rate + reviews_rate + conversion_rate + 
                                      Atencion_Ciudad, data=datos)                      

summary(Modelo_Detractores_Logistic_LM)
RLDL <- summary(Modelo_Detractores_Logistic_LM)
sink("RLDL.txt")
print(RLDL)
sink() 

#Validación de condiciones para la regresión múltiple lineal
#Multicoliealiad
vif(Modelo_Detractores_Logistic_LM)
par(mfrow=c(2,2))
#Analisis de Residuos
#Si la relación es lineal, los residuos deben de distribuirse aleatoriamente en torno a 0 con una variabilidad constante a lo largo del eje X
plot(Modelo_Detractores_Logistic_LM,1)
#Distribución normal de los residuos:
plot(Modelo_Detractores_Logistic_LM,2)
shapiro.test(Modelo_Detractores_Logistic_LM$residuals)
#Tanto el análisis gráfico como es test de hipótesis confirman falta de normalidad.
#Variabilidad constante de los residuos (homocedasticidad):
plot(Modelo_Detractores_Logistic_LM,3)
bptest(Modelo_Detractores_Logistic_LM)
#El análii gráfico y el test de hipotesis indican falta de homoscedasticidad
plot(Modelo_Detractores_Logistic_LM, 4)
#Importancia de las variables
varImp(Modelo_Detractores_Logistic_LM, scale = FALSE) %>% rownames_to_column() %>%  arrange(desc(Overall))
#####################################
# ROMOTORES LOGISTIC RANDOM FOREST
######################################
rm(list = ls())
datos<- readRDS("datos_ModeladoL.rds")
# Dejar los datos con nombres para aplicar cualquier algoritmo
names(datos) <- make.names(names(datos))
#Se utilizan las observaciones completas
datos <- datos[complete.cases(datos),]
#DATOS PARA PROMOTORES
datos_promL<-datos[,-c(1,2,18:20,29, 24)]

train_promL <- sample(1:nrow(datos_promL), size = nrow(datos_promL)/2)

#Identificación del valor óptimo del hiperparámetro mtry.
tuning_rf_mtry <- function(df, y, ntree){
  
  require(dplyr)
  max_predictores <- ncol(df) - 1
  n_predictores   <- rep(NA, max_predictores)
  oob_mse         <- rep(NA, max_predictores)
  for (i in 1:max_predictores) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- ranger(formula = f, data = df, mtry = i, num.trees = ntree)
    n_predictores[i] <- i
    oob_mse[i] <- tail(modelo_rf$prediction.error, n = 1)
  }
  results <- data_frame(n_predictores, oob_mse)
  return(results)
}


hiperparametro_mtry_promL <-  tuning_rf_mtry(df = datos_promL, y = "peso_promotores", ntree = 500)
hiperparametro_mtry_promL %>% arrange(oob_mse)


ggplot(data = hiperparametro_mtry_promL, aes(x = n_predictores, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_mtry_promL$n_predictores) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_mtry_promL %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs mtry (promotores logistic)",
       x = "nº predictores empleados") +
  theme_bw()

#Se identifica el valor 4 como número óptimo de predictores a evaluar en cada división.

#Identificación del valor óptimo del hiperparámetro nodesize.
tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
    require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_mse <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- ranger(formula = f, data = df, mtry = 3, num.trees = ntree,
                        min.node.size = i)
    oob_mse[i] <- tail(modelo_rf$prediction.error, n = 1)
  }
  results <- data_frame(size, oob_mse)
  return(results)
}

hiperparametro_nodesize_promL <-  tuning_rf_nodesize(df = datos_promL, y = "peso_promotores",
                                                     size = c(1:40))
hiperparametro_nodesize_promL %>% arrange(oob_mse)


ggplot(data = hiperparametro_nodesize_promL, aes(x = size, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_nodesize_promL$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_nodesize_promL %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs nodesize (promotores logistic)",
       x = "nº observaciones en nodos terminales") +
  theme_bw()

#Se identifica el 36 como número óptimo de observaciones mínimas que deben contener los nodos terminales.

modelo_randomforest_promL <- randomForest(peso_promotores ~ ., data = datos_promL, 
                                          subset =   train_promL, mtry = 4 , ntree = 500, 
                                          nodesize = 36, importance = TRUE)
summary(modelo_randomforest_promL)
oob_mse_promL <- data.frame(oob_mse_promL = modelo_randomforest_promL$mse,
                            arboles = seq_along(modelo_randomforest_promL$mse))

ggplot(data = oob_mse_promL, aes(x = arboles, y = oob_mse_promL )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles (promotores logistic)",
       x = "nº árboles") +
  theme_bw()

#Alcanzados en torno a 400 árboles el error del modelo se estabiliza.

#ajuste final:
set.seed(17)
modelo_randomforest_promL <- randomForest(peso_promotores ~ ., data = datos_promL, 
                                          subset = train_promL,
                                          mtry = 4 , ntree = 400, nodesize = 36,
                                          importance = TRUE)
modelo_randomforest_promL

predicciones_promL <- predict(object = modelo_randomforest_promL, 
                              newdata = datos_promL[-train_promL, ])

test_mse_promL <- mean((predicciones_promL - datos_promL[-train_promL, "peso_promotores"])^2)
paste("Error de test (mse) del modelo:", round(test_mse_promL, 2))

# Empleando todas las observaciones en el proceso de randomforest
set.seed(123)
modelo_randomforest_promL <- randomForest(peso_promotores ~ ., data = datos_promL, mtry = 4, 
                                          ntree = 400,
                                          nodesize = 36, importance = TRUE)
modelo_randomforest_promL

#Identificacion de los predictores más influyentes

#importancia_pred <- as.data.frame(importance(modelo_randomforest_promM, scale = TRUE)) era asi pero no me funciona
importancia_pred_promL <- as.data.frame(modelo_randomforest_promL$importance) #probe esto...chequear
importancia_pred_promL <- rownames_to_column(importancia_pred_promL, var = "variable")

p1_promL <- ggplot(data = importancia_pred_promL, aes(x = reorder(variable, `%IncMSE`),
                                                      y = `%IncMSE`,
                                                      fill = `%IncMSE`)) +
  labs(x = "variable", title = "Reducción de MSE (promotores logistic)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2_promL <- ggplot(data = importancia_pred_promL, aes(x = reorder(variable, IncNodePurity),
                                                      y = IncNodePurity,
                                                      fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción de pureza (promotores logistic)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

ggarrange(p1_promL, p2_promL)

#####################################
# DETRCTORES LOGISTIC RANDOM FOREST
####################################
rm(list = ls())
datos<- readRDS("datos_ModeladoL.rds")
# Dejar los datos con nombres para aplicar cualquier algoritmo
names(datos) <- make.names(names(datos))
#Se utilizan las observaciones completas
datos <- datos[complete.cases(datos),]
# Nos quedamos con los que tinen peso detractores
datos <- datos %>% filter(peso_detractores>0)
#DATOS PARA DETRACTORES
datos_detL<-datos[,-c(1,2,18:20,28,24)]

train_detL <- sample(1:nrow(datos_detL), size = nrow(datos_detL)/2)

#Identificación del valor óptimo del hiperparámetro mtry.
tuning_rf_mtry <- function(df, y, ntree){
  
  require(dplyr)
  max_predictores <- ncol(df) - 1
  n_predictores   <- rep(NA, max_predictores)
  oob_mse         <- rep(NA, max_predictores)
  for (i in 1:max_predictores) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- ranger(formula = f, data = df, mtry = i, num.trees = ntree)
    n_predictores[i] <- i
    oob_mse[i] <- tail(modelo_rf$prediction.error, n = 1)
  }
  results <- data_frame(n_predictores, oob_mse)
  return(results)
}

hiperparametro_mtry_detL <-  tuning_rf_mtry(df = datos_detL, 
                                            y = "peso_detractores", ntree = 500)
hiperparametro_mtry_detL %>% arrange(oob_mse)


ggplot(data = hiperparametro_mtry_detL, aes(x = n_predictores, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_mtry_detL$n_predictores) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_mtry_detL %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs mtry (detractores logistic)",
       x = "nº predictores empleados") +
  theme_bw()


#Se identifica el valor 4 como número óptimo de predictores a evaluar en cada división.

#Identificación del valor óptimo del hiperparámetro nodesize.

tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  
  require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_mse <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- ranger(formula = f, data = df, mtry = 5, num.trees = ntree,
                        min.node.size = i)
    oob_mse[i] <- tail(modelo_rf$prediction.error, n = 1)
  }
  results <- data_frame(size, oob_mse)
  return(results)
}

hiperparametro_nodesize_detL <-  tuning_rf_nodesize(df = datos_detL,
                                                    y = "peso_detractores",
                                                    size = c(1:20))
hiperparametro_nodesize_detL %>% arrange(oob_mse)

ggplot(data = hiperparametro_nodesize_detL, aes(x = size, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_nodesize_detL$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_nodesize_detL %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs nodesize (detractores logistic)",
       x = "nº observaciones en nodos terminales") +
  theme_bw()

#Se identifica el 5 como número óptimo de observaciones mínimas que deben contener los nodos terminales.


modelo_randomforest_detL <- randomForest(peso_detractores ~ ., 
                                         data = datos_detL, subset = train_detL,
                                         mtry = 4 , ntree = 500, nodesize = 5,
                                         importance = TRUE)

oob_mse_detL <- data.frame(oob_mse_detL = modelo_randomforest_detL$mse,
                           arboles = seq_along(modelo_randomforest_detL$mse))

ggplot(data = oob_mse_detL, aes(x = arboles, y = oob_mse_detL )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles (detractores   logistic)", x = "nº árboles") + theme_bw()

#Alcanzados en torno a 400 árboles el error del modelo se estabiliza.

#ajuste final:
set.seed(17)
modelo_randomforest_detL <- randomForest(peso_detractores ~ ., 
                                         data = datos_detL, subset = train_detL,
                                         mtry = 4 , ntree = 400, nodesize = 5,
                                         importance = TRUE)

modelo_randomforest_detL

predicciones_detL <- predict(object = modelo_randomforest_detL,
                             newdata = datos_detL[-train_detL, ])
test_mse_detL <- mean((predicciones_detL - datos_detL[-train_detL, 
                                                      "peso_detractores"])^2)
paste("Error de test (mse) del modelo:", round(test_mse_detL, 2))

# Empleando todas las observaciones en el proceso de randomforest
set.seed(123)
modelo_randomforest_detL <- randomForest(peso_detractores ~ ., 
                                         data = datos_detL, mtry = 4, 
                                         ntree = 200,
                                         nodesize = 5, importance = TRUE)
modelo_randomforest_detL

#Identificacion de los predictores más influyentes

#importancia_pred <- as.data.frame(importance(modelo_randomforest_promM, scale = TRUE)) era asi pero no me funciona
importancia_pred_detL <- as.data.frame(modelo_randomforest_detL$importance) #probe esto...chequear
importancia_pred_detL <- rownames_to_column(importancia_pred_detL, 
                                            var = "variable")

p1_detL <- ggplot(data = importancia_pred_detL, 
                  aes(x = reorder(variable,
                                  `%IncMSE`),
                      y = `%IncMSE`, fill = `%IncMSE`)) +
  labs(x = "variable", title = "Reducción de MSE (detractores logistic)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2_detL <- ggplot(data = importancia_pred_detL, 
                  aes(x = reorder(variable, IncNodePurity),                                       y = IncNodePurity, fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción de pureza (detractores logistic)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

ggarrange(p1_detL, p2_detL)

############################################################
## PROMOTORES MARKETPLACE MODELO DE REGRESION LINEAL MULTIPLE
############################################################
rm(list = ls())
#Importacion del dataset final
datos<- readRDS("datos_ModeladoM.rds")
# Dejar los datos con nombres para aplicar cualquier algoritmo
names(datos) <- make.names(names(datos))
#Se utilizan las observaciones completas
datos <- datos[complete.cases(datos),]
# Ver la correlación lineal con Peso romotores
ParaCor <- datos %>% select_if(is.numeric)   
#Correlaciones contra peso promotores numericas
cor(ParaCor[c(15,16)], ParaCor[-1], use="complete.obs") %>% View()
# Mas correlacionadas son AVG Rating, Tiempo promedio de llegada tarde y Vouchers promedio. 
#Correlaciones entre candidatos
datos %>% select_if(is.numeric) %>% dplyr::select(-c(1,15,16)) %>% plot_correlation( cor_args = list("use" = "pairwise.complete.obs"))
# Alta corelacion entre las variables de chats y rlcionadas con Vouchers pueden indicar multicolinealidad.
# Antes de pasar al entrenamiento del modelo se Estandarizan los predictores para mejorar la interpretacion de los mismos.
varss <- datos %>% select_if(is.numeric) %>% dplyr::select(-c(1,15,16)) %>% names()
for (i in varss){
  datos[[i]] <- scale(datos[[i]])
}
# Verificacion
summary(datos)
# Estimacion por cross Validation
#Se eliminan las columnas que no son predictores
datos = datos[,-c(1,2,26)]
#Se eliminan las correlaciones altas
corr_altos <- datos %>% select_if(is.numeric) %>% dplyr::select(-17) %>% cor(use = "complete.obs") %>% findCorrelation(cutoff = 0.5, names = TRUE)
#elimino variables altamente correlacionadas
datos <- datos %>%  dplyr::select(-corr_altos)
# Es importante que la Asignación aleatoria de K grupos
grupo <- sample(rep(x = 1:10, length = nrow(datos))) 
#Se comprueba que la distribución es aproximadamente equitativa
table(grupo)

# Dado un objeto creado por la función regsubsets(), que es una lista de
# modelos, y un nuevo set de observaciones, la función predict.regsubsets() 
# devuelve las predicciones para cada uno de los modelos.

predict.regsubsets  <- function(object, newdata, id, ...){
  # Extraer la fórmula del modelo (variable dependiente ~ predictores)
  form <- as.formula(object$call[[2]])
  # Generar una matriz modelo con los nuevos datos y la fórmula
  mat <- model.matrix(form, newdata)
  # Extraer los coeficientes del modelo
  coefi <- coef(object , id = id)
  # Almacenar el nombre de las variables predictoras del modelo
  xvars <- names(coefi)
  # Producto matricial entre los coeficientes del modelo y los valores de
  # los predictores de las nuevas observaciones para obtener las 
  # predicciones
  mat[ , xvars] %*% coefi
}

# Matriz que almacena los test-error estimados. Cada columna representa un
# modelo. Cada fila representa uno de los 10 grupos en los que se han dividido
# las observaciones.
error_matrix <- matrix(data = NA, nrow = 10, ncol = 20,
                       dimnames = list(NULL, c(1:20)))

# Loop en el que se excluye en cada iteración un grupo distinto
# Loop para 21 posibles pdictores
num_validaciones <- 10
num_predictores <- 20

for (k in 1:num_validaciones) {
  # Identificación de datos empleados como training
  train <- datos[grupo != k, ]
  # Selección de los mejores modelos para cada tamaño basándose en RSS
  mejores_modelos <- regsubsets(peso_promotores~., data = train, nvmax = 20,
                                method = "forward")
  # Para cada uno de los modelos "finalistas" se calcula el test-error con
  # el grupo excluido
  for (i in 1:num_predictores) {
    test <- datos[grupo == k, ]
    # Las predicciones del modelo i almacenado en el objeto regsubsets se
    # extraen mediante la función predict.regsubsets() definida arriba
    predicciones <- predict.regsubsets(object = mejores_modelos,
                                       newdata = test, id = i)
    # Cálculo y almacenamiento del MSE para el modelo i
    error_matrix[k,i] <- mean((test$peso_promotores - predicciones)^2)
  }
}

# Cada columna de la matriz error_matrix contiene los 10 valores de error
# calculados por cv
mean_cv_error <- apply(X = error_matrix, MARGIN = 2, FUN = mean)
# plot(sqrt(mean_cv_error), type = "b", pch = 19)
which.min(x = mean_cv_error)

ggplot(data = data.frame(n_predictores = 1:20, mean_cv_error = mean_cv_error),
       aes(x = n_predictores, y = mean_cv_error)) +
  geom_line() +
  geom_point() +
  geom_point(aes(x = n_predictores[which.min(mean_cv_error)],
                 y = mean_cv_error[which.min(mean_cv_error)]),
             colour = "red", size = 3) +
  scale_x_continuous(breaks = c(0:21)) +
  theme_bw() +
  labs(title = "Cross-validation mean error vs número de predictores Promotores Marketplace",
       x = "número predictores")

#El mejor modelo identificado mediante 10-Cross-Validation es el formado por 15 predictores.

modelo_final <- regsubsets(peso_promotores~., data = datos, nvmax = 20,
                           method = "forward")
coef(object = modelo_final, 15)


Modelo_Promotores_Market_LM <- lm(peso_promotores~ Restaurant.Avg.Rating + Restaurant.Total.Products.BO+ 
                                      Categoria.agrupada + Restaurant.Has.Mov..Yes...No. +
                                      Voucher_rate + reviews_rate +  Voucher_rate + fail_rate + reviews_rate+
                                      Restaurant.Has.Discount..Yes...No.+ chatstransf_rate+ conversion_rate + Antiguedad+ 
                                      Atencion_Ciudad, data=datos)                      

summary(Modelo_Promotores_Market_LM)
RLPM <- summary(Modelo_Promotores_Market_LM)
sink("RLPM.txt")
print(RLPM)
sink() 
#Validación de condiciones para la regresión múltiple lineal
#Multicoliealiad
vif(Modelo_Promotores_Market_LM)
#Analisis de Residuos
#Si la relación es lineal, los residuos deben de distribuirse aleatoriamente en torno a 0 con una variabilidad constante a lo largo del eje X
par(mfrow=c(2,2))
plot(Modelo_Promotores_Market_LM,1)
#Distribución normal de los residuos:
plot(Modelo_Promotores_Market_LM,2)
shapiro.test(Modelo_Promotores_Market_LM$residuals)
#Tanto el análisis gráfico como es test de hipótesis confirman falta de normalidad.
#Variabilidad constante de los residuos (homocedasticidad):
plot(Modelo_Promotores_Market_LM,3)
bptest(Modelo_Promotores_Market_LM)
#El análii gráfico y el test de hipotesis indican falta de homoscedasticidad
plot(Modelo_Promotores_Market_LM, 4)
# Var importance
varImp(Modelo_Promotores_Market_LM, scale = FALSE) %>% rownames_to_column() %>%  arrange(desc(Overall))
#############################################################
## DETRACTORES MARKET MODELO DE REGRESION LINEAL MULTIPLE
#############################################################
#Importacion del dataset final
datos<- readRDS("datos_ModeladoM.rds")
# Dejar los datos con nombres para aplicar cualquier algoritmo
names(datos) <- make.names(names(datos))
#Se utilizan las observaciones completas
datos <- datos[complete.cases(datos),]
#Se toman solo las obervaciones con un peso detractr mayor a 0
datos <- datos %>% filter(peso_detractores>0)
# Antes de pasar al entrenamiento del modelo se Estandarizan los predictores para mejorar la interpretacion de los mismos.
varss <- datos %>% select_if(is.numeric) %>% dplyr::select(-c(1,15,16)) %>% names()
for (i in varss){
  datos[[i]] <- scale(datos[[i]])
}
# Verificacion
summary(datos)
# Estimacion por cross Validation
#Se eliminan las columnas que no son predictores
datos = datos[,-c(1,2,25)]
#Se eliminan las correlaciones altas
corr_altos <- datos %>% select_if(is.numeric) %>% dplyr::select(-14) %>% cor(use = "complete.obs") %>% findCorrelation(cutoff = 0.5, names = TRUE)
#elimino variables altamente correlacionadas
datos <- datos %>%  dplyr::select(-corr_altos)
# Es importante que la Asignación aleatoria de K grupos
grupo <- sample(rep(x = 1:10, length = nrow(datos))) 
#Se comprueba que la distribución es aproximadamente equitativa
table(grupo)
# Dado un objeto creado por la función regsubsets(), que es una lista de
# modelos, y un nuevo set de observaciones, la función predict.regsubsets() 
# devuelve las predicciones para cada uno de los modelos.
predict.regsubsets  <- function(object, newdata, id, ...){
  # Extraer la fórmula del modelo (variable dependiente ~ predictores)
  form <- as.formula(object$call[[2]])
  # Generar una matriz modelo con los nuevos datos y la fórmula
  mat <- model.matrix(form, newdata)
  # Extraer los coeficientes del modelo
  coefi <- coef(object , id = id)
  # Almacenar el nombre de las variables predictoras del modelo
  xvars <- names(coefi)
  # Producto matricial entre los coeficientes del modelo y los valores de
  # los predictores de las nuevas observaciones para obtener las 
  # predicciones
  mat[ , xvars] %*% coefi
}

# Matriz que almacena los test-error estimados. Cada columna representa un
# modelo. Cada fila representa uno de los 10 grupos en los que se han dividido
# las observaciones.
error_matrix <- matrix(data = NA, nrow = 10, ncol = 20,
                       dimnames = list(NULL, c(1:20)))

# Loop en el que se excluye en cada iteración un grupo distinto
# Loop para 21 posibles pdictores
num_validaciones <- 10
num_predictores <- 20

for (k in 1:num_validaciones) {
  # Identificación de datos empleados como training
  train <- datos[grupo != k, ]
  # Selección de los mejores modelos para cada tamaño basándose en RSS
  mejores_modelos <- regsubsets(peso_detractores~., data = train, nvmax = 20,
                                method = "forward")
  # Para cada uno de los modelos "finalistas" se calcula el test-error con
  # el grupo excluido
  for (i in 1:num_predictores) {
    test <- datos[grupo == k, ]
    # Las predicciones del modelo i almacenado en el objeto regsubsets se
    # extraen mediante la función predict.regsubsets() definida arriba
    predicciones <- predict.regsubsets(object = mejores_modelos,
                                       newdata = test, id = i)
    # Cálculo y almacenamiento del MSE para el modelo i
    error_matrix[k,i] <- mean((test$peso_detractores - predicciones)^2)
  }
}

# Cada columna de la matriz error_matrix contiene los 10 valores de error
# calculados por cv
mean_cv_error <- apply(X = error_matrix, MARGIN = 2, FUN = mean)
# plot(sqrt(mean_cv_error), type = "b", pch = 19)
which.min(x = mean_cv_error)

ggplot(data = data.frame(n_predictores = 1:20, mean_cv_error = mean_cv_error),
       aes(x = n_predictores, y = mean_cv_error)) +
  geom_line() +
  geom_point() +
  geom_point(aes(x = n_predictores[which.min(mean_cv_error)],
                 y = mean_cv_error[which.min(mean_cv_error)]),
             colour = "red", size = 3) +
  scale_x_continuous(breaks = c(0:20)) +
  theme_bw() +
  labs(title = "Cross-validation mean error vs número de predictores Detractores Marketplace",
       x = "número predictores")

#El mejor modelo identificado mediante 10-Cross-Validation es el formado por todos los predictores. Por parsimonia se opta por un modelo con 10 predictores ya que a partir de esos predictores la disminución es marginal 

modelo_final <- regsubsets(peso_detractores~., data = datos, nvmax = 20,
                           method = "forward")
coef(object = modelo_final, 10) 

Modelo_Detractores_Market_LM <- 
  lm(peso_detractores ~ Restaurant.Total.Products.BO + Restaurant.Avg.Rating +
                        Restaurant.Has.Stamps..Yes...No. +
                        Voucher_rate + fail_rate + chatstransf_rate +
       reviews_rate + conversion_rate + Atencion_Ciudad, data=datos)                      

summary(Modelo_Detractores_Market_LM)
RLDM <- summary(Modelo_Detractores_Market_LM)
sink("RLDM.txt")
print(RLDM)
sink() 

#Validación de condiciones para la regresión múltiple lineal
#Multicoliealiad
vif(Modelo_Detractores_Market_LM)
#Analisis de Residuos
#Si la relación es lineal, los residuos deben de distribuirse aleatoriamente en torno a 0 con una variabilidad constante a lo largo del eje X
plot(Modelo_Detractores_Market_LM,1)
#Distribución normal de los residuos:
plot(Modelo_Detractores_Market_LM,2)
shapiro.test(Modelo_Detractores_Market_LM$residuals)
#Tanto el análisis gráfico como es test de hipótesis confirman falta de normalidad.
#Variabilidad constante de los residuos (homocedasticidad):
plot(Modelo_Detractores_Market_LM,3)
bptest(Modelo_Detractores_Market_LM)
#El análii gráfico y el test de hipotesis indican falta de homoscedasticidad
plot(Modelo_Detractores_Market_LM, 4)   
#Importancia de las variables
varImp(Modelo_Detractores_Market_LM, scale = FALSE)%>% rownames_to_column() %>%  arrange(desc(Overall))
##################################
## PROMOTORES MARKETPLACE RANDOM FOREST
##################################
#Importacion del dataset final
datos<- readRDS("datos_ModeladoM.rds")
# Dejar los datos con nombres para aplicar cualquier algoritmo
names(datos) <- make.names(names(datos))
#Se utilizan las observaciones completas
datos <- datos[complete.cases(datos),]
datos_promM<-datos[,-c(1,2,15:17,26,21)]
train_promM <- sample(1:nrow(datos_promM), size = nrow(datos_promM)/2)
#Identificación del valor óptimo del hiperparámetro mtry.
tuning_rf_mtry <- function(df, y, ntree){
  
  require(dplyr)
  max_predictores <- ncol(df) - 1
  n_predictores   <- rep(NA, max_predictores)
  oob_mse         <- rep(NA, max_predictores)
  for (i in 1:max_predictores) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- ranger(formula = f, data = df, mtry = i, num.trees = ntree)
    n_predictores[i] <- i
    oob_mse[i] <- tail(modelo_rf$prediction.error, n = 1)
  }
  results <- data_frame(n_predictores, oob_mse)
  return(results)
}

hiperparametro_mtry_promM <-  tuning_rf_mtry(df = datos_promM, 
                                      y = "peso_promotores", ntree = 500)
hiperparametro_mtry_promM %>% arrange(oob_mse)


ggplot(data = hiperparametro_mtry_promM, aes(x = n_predictores, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_mtry_promM$n_predictores) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_mtry_promM %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs mtry (promotores marketplace)",
       x = "nº predictores empleados") +
  theme_bw()


#Se identifica el valor 4 como número óptimo de predictores a evaluar en cada división.

#Identificación del valor óptimo del hiperparámetro nodesize.

tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  
  require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_mse <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- ranger(formula = f, data = df, mtry = 3, num.trees = ntree,
                        min.node.size = i)
    oob_mse[i] <- tail(modelo_rf$prediction.error, n = 1)
  }
  results <- data_frame(size, oob_mse)
  return(results)
}

hiperparametro_nodesize_promM <-  tuning_rf_nodesize(df = datos_promM, y = "peso_promotores",
                                                     size = c(1:20))
hiperparametro_nodesize_promM %>% arrange(oob_mse)


ggplot(data = hiperparametro_nodesize_promM, aes(x = size, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_nodesize_promM$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_nodesize_promM %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs nodesize (promotores marketplace)",
       x = "nº observaciones en nodos terminales") +
  theme_bw()

#Se identifica el 18 como número óptimo de observaciones mínimas que deben contener los nodos terminales.


modelo_randomforest_promM <- randomForest(peso_promotores ~ ., 
                                          data = datos_promM, subset = train_promM, mtry = 4 , ntree = 500, nodesize = 18,importance = TRUE)

oob_mse_promM <- data.frame(oob_mse_promM = modelo_randomforest_promM$mse,
                            arboles = seq_along(modelo_randomforest_promM$mse))

ggplot(data = oob_mse_promM, aes(x = arboles, y = oob_mse_promM )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles (promotores marketplace)",
       x = "nº árboles") +
  theme_bw()

#Alcanzados en torno a 400 árboles el error del modelo se estabiliza.

#ajuste final:
set.seed(123)
modelo_randomforest_promM <- randomForest(peso_promotores ~ ., 
                                          data = datos_promM, subset = train_promM, mtry = 4 , ntree = 400, nodesize = 13, importance = TRUE)
modelo_randomforest_promM

predicciones_promM <- predict(object = modelo_randomforest_promM, newdata = datos_promM[-train_promM, ])
test_mse_promM <- mean((predicciones_promM - datos_promM[-train_promM, "peso_promotores"])^2)
paste("Error de test (mse) del modelo:", round(test_mse_promM, 2))

# Empleando todas las observaciones en el proceso de randomforest
set.seed(123)
modelo_randomforest_promM <- randomForest(peso_promotores ~ .,
                              data = datos_promM, mtry = 4, ntree = 400,
                              nodesize = 18, importance = TRUE)
modelo_randomforest_promM

#Identificacion de los predictores más influyentes

#importancia_pred <- as.data.frame(importance(modelo_randomforest_promM, scale = TRUE)) era asi pero no me funciona
importancia_pred_promM <- as.data.frame(modelo_randomforest_promM$importance) #probe esto...chequear
importancia_pred_promM <- rownames_to_column(importancia_pred_promM, var = "variable")

p1_promM <- ggplot(data = importancia_pred_promM, aes(x = reorder(variable, `%IncMSE`),
                                                      y = `%IncMSE`,
                                                      fill = `%IncMSE`)) +
  labs(x = "variable", title = "Reducción de MSE (promotores MP)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2_promM <- ggplot(data = importancia_pred_promM, aes(x = reorder(variable, IncNodePurity),
                                                      y = IncNodePurity,
                                                      fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción de pureza (promotores MP)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

ggarrange(p1_promM, p2_promM)

##################################
## DETRACTORES MARKETPLACE RANDOM FOREST
##################################
#Importacion del dataset final
datos<- readRDS("datos_ModeladoM.rds")
# Dejar los datos con nombres para aplicar cualquier algoritmo
names(datos) <- make.names(names(datos))
#Se utilizan las observaciones completas
datos <- datos[complete.cases(datos),]
# Nos quedamos con los que tinen peso detractores
datos <- datos %>% filter(peso_detractores>0)

datos_detM<-datos[,-c(1,2,15:17,25,21)]
train_detM <- sample(1:nrow(datos_promM), size = nrow(datos_promM)/2)

#Identificación del valor óptimo del hiperparámetro mtry.
tuning_rf_mtry <- function(df, y, ntree){
  
  require(dplyr)
  max_predictores <- ncol(df) - 1
  n_predictores   <- rep(NA, max_predictores)
  oob_mse         <- rep(NA, max_predictores)
  for (i in 1:max_predictores) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- ranger(formula = f, data = df, mtry = i, num.trees = ntree)
    n_predictores[i] <- i
    oob_mse[i] <- tail(modelo_rf$prediction.error, n = 1)
  }
  results <- data_frame(n_predictores, oob_mse)
  return(results)
}

hiperparametro_mtry_detM <-  tuning_rf_mtry(df = datos_detM, y = "peso_detractores", ntree = 500)
hiperparametro_mtry_detM %>% arrange(oob_mse)


ggplot(data = hiperparametro_mtry_detM, aes(x = n_predictores, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_mtry_detM$n_predictores) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_mtry_detM %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs mtry (detractores marketplace)",
       x = "nº predictores empleados") +
  theme_bw()

#Se identifica el valor 5 como número óptimo de predictores a evaluar en cada división.

#Identificación del valor óptimo del hiperparámetro nodesize.

tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  
  require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_mse <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- ranger(formula = f, data = df, mtry = 8, num.trees = ntree,
                        min.node.size = i)
    oob_mse[i] <- tail(modelo_rf$prediction.error, n = 1)
  }
  results <- data_frame(size, oob_mse)
  return(results)
}

hiperparametro_nodesize_detM <-  tuning_rf_nodesize(df = datos_detM, y = "peso_detractores",
                                                    size = c(1:40))
hiperparametro_nodesize_detM %>% arrange(oob_mse)


ggplot(data = hiperparametro_nodesize_detM, aes(x = size, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_nodesize_detM$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_nodesize_detM %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs nodesize (detractores marketplace)",
       x = "nº observaciones en nodos terminales") +
  theme_bw()

#Se identifica el 30 como número óptimo de observaciones mínimas que deben contener los nodos terminales.


modelo_randomforest_detM <- randomForest(peso_detractores ~ ., 
                                         data = datos_detM, 
                                         subset = train_detM, mtry = 5 , 
                                         ntree = 500, nodesize = 30,
                                         importance = TRUE)

oob_mse_detM <- data.frame(oob_mse_detM = modelo_randomforest_detM$mse,
                           arboles = seq_along(modelo_randomforest_detM$mse))

ggplot(data = oob_mse_detM, aes(x = arboles, y = oob_mse_detM )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles (detractores MP)",
       x = "nº árboles") +
  theme_bw()

#Alcanzados en torno a 250 árboles el error del modelo se estabiliza.

#ajuste final:
set.seed(17)

modelo_randomforest_detM <- randomForest(peso_detractores ~ ., 
                                         data = datos_detM, 
                                         subset = train_detM,
                                         mtry = 5 , ntree = 250, nodesize = 30,
                                         importance = TRUE)
modelo_randomforest_detM

predicciones_detM <- predict(object = modelo_randomforest_detM, 
                             newdata = datos_detM[-train_detM, ])
test_mse_detM <- mean((predicciones_detM - datos_detM[-train_detM, "peso_detractores"])^2)
paste("Error de test (mse) del modelo:", round(test_mse_detM, 2))

# Empleando todas las observaciones en el proceso de randomforest
set.seed(123)
modelo_randomforest_detM <- randomForest(peso_detractores ~ .,
                                         data = datos_detM, mtry = 5, 
                                         ntree = 250,
                                         nodesize = 30, importance = TRUE)
modelo_randomforest_detM

#Identificacion de los predictores más influyentes

#importancia_pred <- as.data.frame(importance(modelo_randomforest_promM, scale = TRUE)) era asi pero no me funciona
importancia_pred_detM <- as.data.frame(modelo_randomforest_detM$importance) #probe esto...chequear
importancia_pred_detM <- rownames_to_column(importancia_pred_detM, var = "variable")

p1_detM <- ggplot(data = importancia_pred_detM, aes(x = reorder(variable, `%IncMSE`),
                                                    y = `%IncMSE`,
                                                    fill = `%IncMSE`)) +
  labs(x = "variable", title = "Reducción de MSE (detractores MP)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2_detM <- ggplot(data = importancia_pred_detM, aes(x = reorder(variable, IncNodePurity),
                                                    y = IncNodePurity,
                                                    fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción de pureza (detractores MP)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

ggarrange(p1_detM, p2_detM)
