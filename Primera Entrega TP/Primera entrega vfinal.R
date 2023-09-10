# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/Kalou/OneDrive/Escritorio/DMEyF") # Establezco el Working Directory
# Poner sus semillas
semillas <- c(350003, 350029, 350033, 350039, 350087)

# cargo el dataset
dataset <- fread("./datasets/competencia_01.csv")


# Función para sumar dos columnas fila por fila y asignar el resultado a una nueva columna
sumar_y_asignar <- function(dataset, nueva_columna, columna1, columna2) {
  dataset[, (nueva_columna) := rowSums(.SD, na.rm = TRUE), .SDcols = c(columna1, columna2)]
}

# Llamar a la función para cada par de columnas
sumar_y_asignar(dataset, "ctarjeta_visa_master", "ctarjeta_visa", "ctarjeta_master")
sumar_y_asignar(dataset, "ctarjeta_visa_master_transacciones", "ctarjeta_visa_transacciones", "ctarjeta_master_transacciones")
sumar_y_asignar(dataset, "mtarjeta_visa_master_consumo", "mtarjeta_visa_consumo", "mtarjeta_master_consumo")
sumar_y_asignar(dataset, "ctarjeta_visa_master_debitos_automaticos", "ctarjeta_visa_debitos_automaticos", "ctarjeta_master_debitos_automaticos")
sumar_y_asignar(dataset, "ctarjeta_visa_master_descuentos", "ctarjeta_visa_descuentos", "ctarjeta_master_descuentos")
sumar_y_asignar(dataset, "visa_master_delicuency", "Master_delinquency", "Visa_delinquency")
sumar_y_asignar(dataset, "visa_master_status", "Master_status", "Visa_status")
sumar_y_asignar(dataset, "visa_master_mfinanciacion_limite", "Master_mfinanciacion_limite", "Visa_mfinanciacion_limite")
sumar_y_asignar(dataset, "visa_master_Fvencimiento", "Master_Fvencimiento", "Visa_Fvencimiento")
sumar_y_asignar(dataset, "visa_master_Finiciomora", "Master_Finiciomora", "Visa_Finiciomora")
sumar_y_asignar(dataset, "visa_master_msaldototal", "Master_msaldototal", "Visa_msaldototal")
sumar_y_asignar(dataset, "visa_master_msaldopesos", "Master_msaldopesos", "Visa_msaldopesos")
sumar_y_asignar(dataset, "visa_master_msaldodolares", "Master_msaldodolares", "Visa_msaldodolares")
sumar_y_asignar(dataset, "visa_master_mconsumospesos", "Master_mconsumospesos", "Visa_mconsumospesos")
sumar_y_asignar(dataset, "visa_master_mconsumosdolares", "Master_mconsumosdolares", "Visa_mconsumosdolares")
sumar_y_asignar(dataset, "visa_master_mlimitecompra", "Master_mlimitecompra", "Visa_mlimitecompra")
sumar_y_asignar(dataset, "visa_master_madelantopesos", "Master_madelantopesos", "Visa_madelantopesos")
sumar_y_asignar(dataset, "visa_master_madelantodolares", "Master_madelantodolares", "Visa_madelantodolares")
sumar_y_asignar(dataset, "visa_master_fultimo_cierre", "Master_fultimo_cierre", "Visa_fultimo_cierre")
sumar_y_asignar(dataset, "visa_master_mpagado", "Master_mpagado", "Visa_mpagado")
sumar_y_asignar(dataset, "visa_master_mpagospesos", "Master_mpagospesos", "Visa_mpagospesos")
sumar_y_asignar(dataset, "visa_master_mpagosdolares", "Master_mpagosdolares", "Visa_mpagosdolares")
sumar_y_asignar(dataset, "visa_master_fechaalta", "Master_fechaalta", "Visa_fechaalta")
sumar_y_asignar(dataset, "visa_master_mconsumototal", "Master_mconsumototal", "Visa_mconsumototal")
sumar_y_asignar(dataset, "visa_master_cconsumos", "Master_cconsumos", "Visa_cconsumos")
sumar_y_asignar(dataset, "visa_master_cadelantosefectivo", "Master_cadelantosefectivo", "Visa_cadelantosefectivo")
sumar_y_asignar(dataset, "visa_master_mpagominimo", "Master_mpagominimo", "Visa_mpagominimo")

# Elimino las variables con las que cree estas nuevas variables
dataset[, c("ctarjeta_master_transacciones", "ctarjeta_visa_transacciones", "ctarjeta_master",
            "ctarjeta_visa", "ctarjeta_master_descuentos" , 
            "ctarjeta_visa_descuentos")] <- NULL

dataset[, c("ctarjeta_master_debitos_automaticos","ctarjeta_visa_debitos_automaticos" ,
            "mtarjeta_master_consumo", "mtarjeta_visa_consumo","Visa_status" , 
            "Master_status" ,"Visa_delinquency","Master_delinquency",
            "Master_Fvencimiento", "Visa_Fvencimiento"
)] <- NULL

dataset[, c( "Visa_mfinanciacion_limite", "Master_mfinanciacion_limite" , "Visa_Finiciomora", 
            "Master_Finiciomora" ,"Master_msaldototal", "Visa_msaldototal" , 
            "Master_msaldodolares" ,"Visa_msaldodolares" ,"Visa_msaldopesos", 
            "Master_msaldopesos" ,"Visa_mconsumototal", "Master_mconsumototal", 
            "Visa_fechaalta","Master_fechaalta", "Visa_cconsumos","Master_cconsumos"
            ,"Master_mpagominimo","Visa_mpagominimo"
            ,"Visa_cadelantosefectivo","Master_cadelantosefectivo"
)] <- NULL

# Creación de nuevas variables - ratios

# Función para calcular el cociente entre dos columnas y agregarla al data table
calcular_cociente <- function(data_table, columna1, columna2, nueva_columna) {
  data_table[, (nueva_columna) := get(columna1) / get(columna2)]
  return(data_table)
}


# Llamar a la función para calcular el cociente y agregarlo como "cociente_columnas"
dataset <- calcular_cociente(dataset, "cliente_edad", "cliente_antiguedad", "edad_antiguedad")
dataset <- calcular_cociente(dataset, "mrentabilidad", "cliente_antiguedad", "rentabiilidad_antiguedad")
dataset <- calcular_cociente(dataset, "mcomisiones", "mrentabilidad", "comisiones_rentabilidad")
dataset <- calcular_cociente(dataset, "ctarjeta_debito_transacciones", "ctarjeta_debito", "transaccionesXDebito")
dataset <- calcular_cociente(dataset, "ctarjeta_visa_master_transacciones", "ctarjeta_visa_master", "transaccionesXtarjeta")
dataset <- calcular_cociente(dataset, "mactivos_margen", "mpasivos_margen", "activos_sobre_pasivo")
dataset <- calcular_cociente(dataset, "cproductos", "cliente_antiguedad", "productos_sobre_antiguedad")
dataset <- calcular_cociente(dataset, "ccheques_emitidos_rechazados", "ccheques_emitidos", "cheq_rechazados_emitidos")
dataset <- calcular_cociente(dataset, "mtransferencias_emitidas", "mcuentas_saldo", "monto_transf_saldo")
dataset <- calcular_cociente(dataset, "visa_master_mfinanciacion_limite", "visa_master_msaldototal", "monto_transf_saldo")
dataset <- calcular_cociente(dataset, "visa_master_mlimitecompra", "visa_master_mpagospesos", "limite_pagos")
dataset <- calcular_cociente(dataset, "visa_master_mlimitecompra", "visa_master_mconsumototal", "limite_consumo")

# Variables para representar los gastos sobre los ingresos

# Sumar los valores de las columnas deseadas para obtener el monto total
monto_total <- dataset[, sum(ccomisiones_otras, mcomisiones_otras, mpagomiscuentas, mpagodeservicios,mtarjeta_visa_master_consumo)]

# Dividir el monto total por la columna mpayroll
dataset[, gasto_sobre_ingreso := monto_total / mpayroll]

# Con los cocientes anteriores obtuve valores infinitos, creo una función para reemplazarlos con NA

reemplazar_infinitos_con_na <- function(data, columnas) {
  for (col in columnas) {
    data[[col]][is.infinite(data[[col]])] <- NA
  }
  return(data)
}

columnas_a_procesar <- c("monto_transf_saldo", "limite_pagos", "limite_consumo",
                         "gasto_sobre_ingreso","activos_sobre_pasivo")
dataset <- reemplazar_infinitos_con_na(dataset, columnas_a_procesar)

# Función para normalizar columnas numéricas con drifting
normalizar_columnas <- function(col) {
  if (is.numeric(col)) {
    media <- mean(col)
    desviacion <- sd(col)
    return((col - media) / desviacion)
  } else {
    return(col)
  }
}

# Normalizo las columnas con drifting a partir de las densidades graficadas en 2013 vs 2015

columnas_a_normalizar <- c("mcuentas_saldo", "mprestamos_personales", "mcuenta_corriente",
                           "ccomisiones_otras","mcomisiones","mtarjeta_visa_master_consumo",
                           "mrentabilidad", "mpayroll",
                           "cpayroll_trx","mrentabilidad_annual","visa_master_msaldopesos",
                           "visa_master_msaldototal","visa_master_mpagominimo",
                           "ccomisiones_mantenimiento", "chomebanking_transacciones",
                           "visa_master_Fvencimiento","ccajas_consultas",
                           "mcaja_ahorro_dolares","mtransferencias_recibidas",
                           "mtransferencias_emitidas",
                           "visa_master_mfinanciacion_limite","cprestamos_prendarios",
                           "mcheques_emitidos_rechazados",
                           "minversion1_pesos","visa_master_delicuency","minversion2",
                           "mcheques_emitidos","mprestamos_prendarios")

# Aplicar la función a todas las columnas del data table
dataset[, (columnas_a_normalizar) := lapply(.SD, normalizar_columnas), .SDcols = columnas_a_normalizar]

# elimino variables muy altamente correlacionadas

dataset[, mcomisiones_otras := NULL]
dataset[, cforex_sell := NULL]
dataset[, mextraccion_autoservicio := NULL]


# Filtrar solo las columnas numéricas
columnas_numericas <- names(dataset)[sapply(dataset, is.numeric)]
df_numericas <- dataset[, ..columnas_numericas]
df_numericas
# Calcular la matriz de correlación
matriz_correlacion <- cor(as.matrix(df_numericas))
dt <- as.data.table(matriz_correlacion)
# Mostrar la matriz de correlación
print(matriz_correlacion)

# Separo los data set segun el mes

dtrain_03 <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

# Armamos una función que nos calcule la ganancia, usando el punto de corte de
# 0.025
ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "BAJA+2", 273000, -7000))
  )
}

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dtrain_03$Etiqueta,
                                          p = 0.70, list = FALSE) 
dtrain  <-  dtrain_03[in_training, ]
dtest   <-  dtrain_03[-in_training, ]

# con los mejores parametros obtenidos de la hiperparametrización bayesiana construyo el modelo

# genero el modelo,
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo_final <- rpart(
  formula = "Etiqueta ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = -1, # esto significa no limitar la complejidad de los splits
  minsplit = 1689, # minima cantidad de registros para que se haga el split
  minbucket = 258, # tamaño minimo de una hoja
  maxdepth = 8
) # profundidad maxima del arbol

library(caret)
feature_importance <- varImp(modelo_final)

# Mostrar la importancia de las características
print(feature_importance)

# grafico el arbol
prp(modelo_final,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)

# Evaluo el modelo en los datos de test
pred_testing <- predict(modelo_final, dtest, type = "prob")
gan <- ganancia(pred_testing[, "BAJA+2"], dtest$Etiqueta) / 0.3
gan


# Evaluo el modelo en los datos de 2015
pred_2015 <- predict(modelo_final, dapply, type = "prob")
gan_2015 <- ganancia(pred_2015[, "BAJA+2"], dapply$Etiqueta) / 0.3
gan_2015

# Hago un summary para evaluar los parametros del modelo

summary(modelo_final)

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := pred_2015[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

orden <- order(-dapply$prob_baja2)  # El signo negativo es para dar orden descendente
df_ordenado <- dapply[orden, ]
df_ordenado

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
#dir.create("./exp/")
#dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(df_ordenado[, list(numero_de_cliente, Predicted)],
       file = "./exp/KA2001/K204_006.csv",
       sep = ","
)