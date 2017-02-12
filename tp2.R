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
  n_muestral_estratos <- c(6, 9, 3)
  estratos <- list()
  
  for (i in ids_estratos) {
    
    # Genero una lista con constantes fundamentales
    estratos[[i]] = list(
      id = i,
      n = n_muestral_estratos[i]
    )
  }
  
  return(estratos)
}

procesar_estrato <- function(estrato) {
  
  # Adjunto las unidades primarias
  estrato$UP <- tomar_unidades_estrato(estrato$id)

  # Calculo las \Pi_(i|h)^UP
  estrato$UP$PIK <- inclusionprobabilities(estrato$UP$Tviv, estrato$n)

  # Calculo la matriz de \Pi_(ij|h)^UP
  estrato$PIKL <- UPsampfordpi2(estrato$UP$PIK)

  return(estrato)
}

generar_muestra <- function(estrato) {
  # Genero un vector de inclusion 'I' al azar
  estrato$UP$I <- UPsampford(estrato$UP$PIK)
  
  # Separo la muestra 's' que le corresponde
  estrato$s <- estrato$UP[as.logical(estrato$UP$I),]
  
  return(estrato)
}


chequear_consistencia <- function(estratos) {
  for (estrato in estratos) {
    print(sprintf("# Estrato %s", estrato$id))
    print(sprintf("-- Todas las PIK menores a 1: %s", any(estrato$UP$PIK < 1)))
    print(sprintf("-- La muestra 's' tiene tamanio 'n': %s", nrow(estrato$s) == estrato$n))
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

