# para correr el Google Cloud
#   8 vCPU
#  16 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "KA5240"

PARAM$input$dataset <- "./datasets/temp.csv"

# meses donde se entrena el modelo
PARAM$input$training <- c(202011,202012,202101, 202102, 202103, 202104, 202105)
PARAM$input$future <- c(202107) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 350087

PARAM$finalmodel$num_iterations <- 1672
PARAM$finalmodel$learning_rate <- 0.0432410419137825
PARAM$finalmodel$feature_fraction <- 0.722566208286548
PARAM$finalmodel$min_data_in_leaf <- 84
PARAM$finalmodel$num_leaves <- 565


PARAM$finalmodel$max_bin <- 31

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/Kalou/OneDrive/Escritorio/DMEyF/") # Establezco el Working Directory


# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


#--------------------------------------

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
  data_table[, (nueva_columna) := ifelse(is.na(get(columna1)) | is.na(get(columna2)), NA, get(columna1) / get(columna2))]
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
dataset <- calcular_cociente(dataset, "visa_master_mfinanciacion_limite", "visa_master_msaldototal", "financiacionlimite_saldo")
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

guardar_csv <- function(dataset, nombre_archivo) {
  # Verificar si el dataset es un data frame
  if (!is.data.frame(dataset)) {
    stop("El dataset debe ser un data frame.")
  }
  
  # Guardar el dataset en formato CSV
  write.csv(dataset, file = nombre_archivo, row.names = FALSE)
  cat("El dataset se ha guardado correctamente en", nombre_archivo, "\n")
}

guardar_csv(dataset, "dataset_con_FE.csv")

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
library(dplyr)

# Lista de campos a considerar
campos <- c(
  'limite_consumo', 'limite_pagos', 'monto_transf_saldo',
  'mrentabilidad', 'mrentabilidad_annual', 'cheq_rechazados_emitidos', 'productos_sobre_antiguedad',
  'activos_sobre_pasivo', 'transaccionesXtarjeta', 'transaccionesXDebito', 'comisiones_rentabilidad',
  'rentabiilidad_antiguedad', 'edad_antiguedad', 'visa_master_mpagominimo', 'visa_master_cadelantosefectivo',
  'visa_master_cconsumos', 'visa_master_mconsumototal', 'visa_master_mlimitecompra', 'visa_master_mconsumosdolares',
  'visa_master_mconsumospesos', 'visa_master_msaldototal',
  'visa_master_Finiciomora', 'visa_master_Fvencimiento', 'visa_master_mfinanciacion_limite', 'visa_master_status',
  'cplazo_fijo', 'mplazo_fijo_dolares', 'mplazo_fijo_pesos', 'cinversion1', 'minversion1_pesos', 'minversion1_dolares',
  'ccaja_seguridad', 'cpayroll_trx', 'mpayroll', 'ccomisiones_mantenimiento', 'mcomisiones_mantenimiento',
  'ctransferencias_recibidas', 'mtransferencias_recibidas', 'thomebanking', 'chomebanking_transacciones',
  'ccajas_transacciones', 'ccajas_consultas', 'ccajas_depositos', 'ccajas_extracciones', 'ccajas_otras', 'catm_trx',
  'matm', 'ctrx_quarter'
)

campos_light <- c(
  'limite_consumo', 'limite_pagos', 'monto_transf_saldo',
  'mrentabilidad', 'mrentabilidad_annual', 'cheq_rechazados_emitidos', 'productos_sobre_antiguedad',
  'activos_sobre_pasivo', 'transaccionesXtarjeta', 'transaccionesXDebito', 'comisiones_rentabilidad',
  'rentabiilidad_antiguedad','ctrx_quarter'
)

# Función para calcular las estadísticas
calcular_estadisticas <- function(dataset, variables, meses_pasados = 3) {
  dataset <- dataset %>%
    arrange(foto_mes) %>%
    group_by(numero_de_cliente) %>%
    filter(foto_mes >= max(foto_mes) - meses_pasados) %>%  # Filtrar los últimos 3 meses
    mutate(
      across(all_of(variables), ~mean(., na.rm = TRUE, trim = 0), .names = "{.col}_avg"),
      across(all_of(variables), ~max(., na.rm = TRUE), .names = "{.col}_max"),
      across(all_of(variables), ~min(., na.rm = TRUE), .names = "{.col}_min")
    ) %>%
    ungroup()
  
  return(dataset)
}

# Aplicar la función a tu dataset
dataset <- calcular_estadisticas(dataset, campos_light)