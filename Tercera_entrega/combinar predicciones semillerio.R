#Levanto archivos de predicciones


experimento <- "competencia_3"

dir <- paste0("~/buckets/b1/exp/",experimento)


archivos <- list.files(dir, pattern = "pred",full.names = TRUE)

for (i in 1:length(archivos)) {
  # Leer el archivo
  temp <- read.table(archivos[i], sep = "\t", header = TRUE)
  
  # Asegurar que tiene las columnas correctas
  expected_columns <- c("numero_de_cliente", "foto_mes", "prob")
  if (!all(expected_columns %in% colnames(temp))) {
    cat("Error en el archivo:", archivos[i], "\n")
    stop("El archivo no tiene las columnas esperadas.")
  }
  
  # Combinar marcos de datos
  if (i == 1) {
    predicciones <- temp
  } else {
    predicciones <- merge(predicciones, temp,
                          by.x = c("numero_de_cliente", "foto_mes"),
                          by.y = c("numero_de_cliente", "foto_mes"))
  }
}



library(tidyverse)


predicciones <- predicciones %>% 
  pivot_longer(cols = -c(numero_de_cliente,foto_mes)) %>% 
  group_by(numero_de_cliente,foto_mes) %>% 
  mutate(prob_media = mean(value)) %>% 
  ungroup() %>% 
  distinct(numero_de_cliente,.keep_all = T)

require(data.table)

predicciones <- predicciones %>% 
  arrange(desc(prob_media)) %>% 
  as.data.table()

cortes <- c(seq(9000, 15000, by = 500))



for (envios in cortes) {
  
  tb_entrega <- predicciones[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]
  
  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
         file = paste0(dir,"/",experimento,"_",envios,".csv"),
         sep = ","
  )
  
}
