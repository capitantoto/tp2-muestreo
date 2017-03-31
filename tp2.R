library(sampling)
library(samplingVarEst)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(ggplot2)

load("data/radiosTP2.RData")

n_UP_por_estrato <- c(6, 9, 3)
n_US_por_radio <- c(90, 50, 80)


radiosTP2 %>%
  mutate(
    n_UP_por_estrato = n_UP_por_estrato[Estrato],
    n_US_por_radio = n_US_por_radio[Estrato]) %>%
  group_by(Estrato) %>%
  arrange(Estrato, Radio) %>%
  mutate(id_radio_intra_estrato = 1:n(),
         PIK_UP = inclusionprobabilities(Tviv, n_UP_por_estrato),
         I = UPsampford(PIK_UP),
         PIK_US_radio = n_US_por_radio / Tviv,
         PIKL_US_radio = PIK_US_radio * ((n_US_por_radio - 1) / (Tviv - 1)),
         PIK_US = PIK_UP * PIK_US_radio) %>%
  ungroup() -> radios

PIKL_UP_por_estrato <- map(
  c(1:3),
  function(i) (
    radios %>% filter(Estrato == i) %>% .[["PIK_UP"]] %>% UPsampfordpi2
  )
)

get_pikl_up_muestra_estrato <- function(i) {
  muestra_radios %>% filter(Estrato == i) %>% .[["id_radio_intra_estrato"]] -> inc
  return(PIKL_UP_por_estrato[[i]][inc, inc])
}
map(c(1:3), get_pikl_up_muestra_estrato)
load("data/muestraTP2.RData")

radios %>%
  filter(I == 1) %>% 
  group_by(Estrato) %>%
  mutate(upm = 1:n()) %>%
  ungroup -> muestra_radios

N <- Est.Total.NHT(muestra_radios$Tviv, muestra_radios$PIK_UP)

muestraTP2 %>%
  mutate(
    uno = 1,
    n = n(),
    id_vivienda = 1:n(),
    N = N,
    PIK_US_MSA = n/N,
    SaludTot = SaludF + SaludM,
    FtTot = FTedad2 + FTedad3 + FTedad4,
    OcupTot = Oedad2 + Oedad3 + Oedad4,
    DesocTot = FtTot - OcupTot,
    clase = ifelse(totper >= 5,  "D",
                   ifelse(totper >= 3, "C",
                          ifelse(totper == 2, "B", "A")))
    ) %>%
  # Adjunto valores de parte A a la muestra de la parte B
  merge(muestra_radios, 
        by = c("Estrato", "upm")) %>%
  as_data_frame -> viviendas

all(viviendas$totper > 0)

get_vivienda <- function(i) viviendas %>% slice(i)

calcular_pikl_us <- function(k, l) {
  vivienda_k <- get_vivienda(k)
  vivienda_l <- get_vivienda(l)
  PIK_UP_k <- vivienda_k$PIK_UP
  PIK_US_radio_k <- vivienda_k$PIK_US_radio
  PIK_UP_l <- vivienda_l$PIK_UP
  PIK_US_radio_l <- vivienda_l$PIK_US_radio
  
  if (vivienda_k$Estrato != vivienda_l$Estrato) {
    return((PIK_UP_k * PIK_US_radio_k) * (PIK_UP_l * PIK_US_radio_l))
  
  } else if (vivienda_k$upm != vivienda_l$upm) {
    PIKL_UP_estrato <- PIKL_UP_por_estrato[[vivienda_k$Estrato]][
      vivienda_k$id_radio_intra_estrato,
      vivienda_l$id_radio_intra_estrato]
    return(PIKL_UP_estrato *  (PIK_US_radio_k * PIK_US_radio_l))
      
  } else if (k != l) {
    PIKL_US_radio <- vivienda_k$PIKL_US_radio    
    return(PIK_UP_k * PIKL_US_radio)
    
  } else
    return(PIK_UP_k * PIK_US_radio_k)
}

generar_PIKL_US <- function(viviendas) {
  n <- dim(viviendas)[1]
  mat <- matrix(NA, n, n)
  for (k in 1:n) {
    print(k)
    for (l in 1:n) {
      mat[k,l] = calcular_pikl_us(k,l)
    }
  }
  return(mat)
}

generar_pikl_msa <- function(n, N) {
  outer(1:n, 1:n,
        function(x, y) {
          ifelse(x == y, (n/N), (n/N)*((n-1)/(N-1)))
        })
}

construir_estimador_total <- function(df, x){
  df %>%
    summarise(
      name = x,
      est = Est.Total.NHT(.[[x]], PIK_US),
      var_2s = VE.HT.Total.NHT(.[[x]], PIK_US, PIKL_US),
      var_msa = VE.HT.Total.NHT(.[[x]], PIK_US_MSA, PIKL_US_MSA),
      cv = sqrt(var_2s)/est,
      deff = var_2s / var_msa
    )
}

construir_estimador_razon <- function(df, y, x){
  df %>%
    summarise(
      name = paste(y, '/', x),
      est = Est.Ratio(.[[y]], .[[x]], PIK_US),
      var_2s = VE.Lin.HT.Ratio(.[[y]], .[[x]], PIK_US, PIKL_US),
      var_msa = VE.Lin.HT.Ratio(.[[y]], .[[x]], PIK_US_MSA, PIKL_US_MSA),
      cv = sqrt(var_2s)/est,
      deff = var_2s / var_msa
    )
}

calcular_indicadores <- function(df, y, x) {
    if (x == ''){
    construir_estimador_total(df, y)
  } else
    construir_estimador_razon(df, y, x)
}


PIKL_US <- generar_PIKL_US(viviendas)
PIKL_US_MSA <- generar_pikl_msa(1230, 22390)

cocientes <- read.csv("data/cocientes.csv", as.is=TRUE)
map2_df(cocientes$y, cocientes$x,
          function(y, x) (calcular_indicadores(viviendas, y, x))) -> resultados_globales


ingreso_medio_por_dominio <- function(var_dominio){
viviendas %>%
  group_by(dominio = viviendas[[var_dominio]]) %>%
  mutate(n_clase = n(),
         N_clase = round(Est.Total.NHT(uno, PIK_US)),
         PIK_US_MSA_dom = n_clase / N_clase) %>%
  summarise(ingreso_medio = Est.Mean.NHT(ingresoh, PIK_US, max(N_clase)),
            var_2s = VE.HT.Mean.NHT(ingresoh, PIK_US, 
                                 PIKL_US[id_vivienda, id_vivienda],
                                 max(N_clase)),
            var_msa = VE.HT.Mean.NHT(ingresoh, PIK_US_MSA_dom, 
                                  generar_pikl_msa(max(n_clase), max(N_clase)),
                                  max(N_clase)),
            cv = sqrt(var_2s) / ingreso_medio,
            deff = var_2s / var_msa
         
  )
}

ingreso_medio_hog10 <- ingreso_medio_por_dominio("hog10")
ingreso_medio_clase <- ingreso_medio_por_dominio("clase")
