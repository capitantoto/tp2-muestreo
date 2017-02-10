library(sampling)
library(dplyr)
??sampling::UPsampford

load("data/radiosTP2.RData")
summary(radiosTP2)

radiosTP2$Estrato <- as.factor(radiosTP2$Estrato)
n_muestral_estratos <- c(6, 9, 3)

# Separo radios en estratos para aplicar Sampford a cada uno
get_unidades_estrato <- function(n){
  radiosTP2[c(radiosTP2$Estrato == n),]
}
estratos <- lapply(c(1,2,3), get_unidades_estrato)

# Genero las PIK por estrato, dados los tamanios muestrales propuestos
adjuntar_piks <- function(estrato, n_muestral){
  estrato$PIK <- inclusionprobabilities(estrato$Tviv, n_muestral)
  return(estrato)
}

ids_estratos = as.numeric(levels(radiosTP2$Estrato))

for (i in ids_estratos) {
  estratos[[i]] <- adjuntar_piks(estratos[[i]], n_muestral_estratos[i])
}

# Compruebo que ninguna PIK sea mayor a 1:
for (i in 1:3) {
  print(any(estratos[[i]]$PIK < 1))
}
# No hay!

# Tomo una muestra por estrato y las compongo en una `muestra_completa`
get_muestra <- function(estrato) {
  vector_inclusion <- UPsampford(estrato$PIK)
  return(estrato[as.logical(vector_inclusion),])
}

muestras <- lapply(estratos, get_muestra)

muestra_completa <- rbind(
  muestras[[1]], muestras[[2]], muestras[[3]]
)

muestra_completa

??UPsampfordpi2
