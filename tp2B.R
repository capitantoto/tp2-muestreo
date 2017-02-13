library(sampling)
library(dplyr)

load("data/muestraTP2.RData")
load("parteA.RData")

# Compruebo que FTedad1 y Oedad1 son siempre iguales a cero
all(muestraTP2$FTedad1 == 0)
all(muestraTP2$Oedad1 == 0)

# Agrego variables extras necesarias para la estimacion
muestraTP2$SaludTot <- muestraTP2$SaludF + muestraTP2$SaludM
muestraTP2$FtTot <- muestraTP2$FTedad2 + muestraTP2$FTedad3 + muestraTP2$FTedad4
muestraTP2$OcupTot <- muestraTP2$Oedad2 + muestraTP2$Oedad3 + muestraTP2$Oedad4
muestraTP2$DesocTot <- muestraTP2$FtTot - muestraTP2$OcupTot

muestraTP2$clase <- "X"
muestraTP2$clase <- ifelse(muestraTP2$totper >= 5, "D",
                           muestraTP2$clase)
muestraTP2$clase <- ifelse(muestraTP2$totper == 4, "C", muestraTP2$clase)
muestraTP2$clase <- ifelse(muestraTP2$totper == 3, "C", muestraTP2$clase)
muestraTP2$clase <- ifelse(muestraTP2$totper == 2, "B", muestraTP2$clase)
muestraTP2$clase <- ifelse(muestraTP2$totper == 1, "A", muestraTP2$clase)
muestraTP2$clase <- as.factor(muestraTP2$clase)
