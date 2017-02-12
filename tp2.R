library(sampling)
library(dplyr)

load("data/radiosTP2.RData")
summary(radiosTP2)

radiosTP2$Estrato <- as.factor(radiosTP2$Estrato)

tomar_unidades_estrato <- function(n){
  radiosTP2[c(radiosTP2$Estrato == n),]
}

# Por estrato:
generar_estratos <- function(UP) {

  ids_estratos = as.numeric(levels(UP$Estrato))
  n_UP_por_estrato <- c(6, 9, 3)
  n_US_por_radio <- c(90, 50, 80)
  estratos <- list()
  
  for (i in ids_estratos) {
    
    # Genero una lista con constantes fundamentales
    estratos[[i]] = list(
      id = i,
      nUP = n_UP_por_estrato[i],
      nUS = n_US_por_radio[i])
  }
  
  return(estratos)
}

procesar_estrato <- function(estrato) {
  
  # Adjunto las unidades primarias
  estrato$UP <- tomar_unidades_estrato(estrato$id)

  # Calculo las \Pi_(i|h)^UP
  estrato$UP$PIK_UP <- inclusionprobabilities(estrato$UP$Tviv, estrato$nUP)

  # Calculo la matriz de \Pi_(ij|h)^UP
  estrato$PIKL_UP <- UPsampfordpi2(estrato$UP$PIK_UP)
  
  # El muestreo de segunda etapa es MSA, asi que las  
  estrato$UP$PIK_US <- estrato$UP$PIK_UP * estrato$nUS / estrato$UP$Tviv
  # estrato$PIKL_US <- 
  return(estrato)
}

generar_muestra <- function(estrato) {
  # Genero un vector de inclusion 'I' al azar
  estrato$UP$I <- UPsampford(estrato$UP$PIK_UP)
  
  # Separo la muestra 's' que le corresponde
  estrato$s <- estrato$UP[as.logical(estrato$UP$I),]
  
  return(estrato)
}


chequear_consistencia <- function(estratos) {
  for (estrato in estratos) {
    print(sprintf("# Estrato %s", estrato$id))
    print(sprintf("-- Todas las PIK de primera etapa menores a 1: %s",
                  any(estrato$UP$PIK_UP < 1)))
    print(sprintf("-- La muestra 's' tiene tamanio 'n': %s",
                  nrow(estrato$s) == estrato$nUP))
  }
}
# Compruebo que ninguna PIK sea mayor a 1:
# No hay!

# Tomo una muestra por estrato y las compongo en una `muestra_completa`

estratos <- generar_estratos(radiosTP2)
estratos <- lapply(estratos, procesar_estrato)
estratos <- lapply(estratos, generar_muestra)
chequear_consistencia(estratos)

# Este pedazo podria volar ,lo mantengo por compatibilidad y temor
muestras <- list()
for (estrato in estratos) {
  muestras[[estrato$id]] <- estrato$s
}
muestra_completa <- rbind(
  muestras[[1]], muestras[[2]], muestras[[3]]
)

