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
  estrato$N_UP <- nrow(estrato$UP)
  # Calculo las \Pi_(i|h)^UP
  estrato$UP$PIK_UP <- inclusionprobabilities(estrato$UP$Tviv, estrato$nUP)

  # Calculo la matriz de \Pi_(ij|h)^UP
  estrato$PIKL_UP <- UPsampfordpi2(estrato$UP$PIK_UP)

  # Todas las viviendas (US) de un radio (UP) tienen la misma \Pi_(K|h)
  # porque el disenio de 2da etapa es un MSA
  estrato$UP$PIK_US <- estrato$UP$PIK_UP * estrato$nUS / estrato$UP$Tviv
  
  # Para la matriz de \Pi_(kl|h) hay que considerar varios casos
  # -- Si k==l, entonces \Pi_(kl|h) = \Pi_(k|h) = \Pi_(i|h) * \Pi_(k|i)
  estrato$UP$PIKL_US_k_eq_l <- estrato$UP$PIK_US
  # Sea r(k) el radio censal de la vivienda k.
  # -- Si k != l y r(k)==r(l)=i, entonces 
  # -- \Pi_(kl|h) = = \Pi_(i|h) * \Pi_(kl|h)
  estrato$UP$PIKL_US_i_eq_j <- estrato$UP$PIK_UP * (estrato$nUS / estrato$UP$Tviv) * ((estrato$nUS - 1) / (estrato$UP$Tviv - 1))
  
  # -- Si i != j entonces \Pi_(kl|h) = \Pi_(ij/h) *\Pi_(k|i) * \Pi_(l|j)
  estrato$PIKL_US_i_ne_j <- matrix(nrow = estrato$N_UP, ncol = estrato$N_UP)
  
  for (i in 1:estrato$N_UP) {
    for (j in 1:estrato$N_UP) {
      estrato$PIKL_US_i_ne_j[i,j] <-estrato$PIKL_UP[i,j] * estrato$UP$PIK_US[i] * estrato$UP$PIK_US[j]
    }
  }
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

