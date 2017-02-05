setwd("~/Downloads/MPF 2016 UBA/Clases/")
load("Marco.Empresas 2016.RData")
library(SamplingStrata)

head(Marco.Empresas)
table(Marco.Empresas$REG)
table(Marco.Empresas$TIPO)
table(Marco.Empresas$ACTIV)
summary(Marco.Empresas$PO)

#############################
# Armado del Marco
##############################
Marco=Marco.Empresas
########################################
# Renombrar las variables Continuas (Y)
# que se desan estimar con precision
########################################
Marco$Y1=Marco$VENTAS
Marco$Y2=Marco$VMP

####################################
# Sin Estimaciones a Nivel Dominio
# Cuidado tiene que ser NUMERIC
########################################
Marco$domainvalue=1

########################################
# Conversion de las variables Continuas
# que definen la estratificacion (X)
########################################
Marco$X1=as.factor(Marco$REG)
Marco$X2=as.factor(Marco$ACTIV)
Marco$X3=var.bin(Marco$PO,bins=8)

max(Marco$REG)*max(Marco$ACTIV)*max(Marco$X3)

############  K=120 Micro Estratos ######
#                X1*X2*X3
#                 B120
##########################################
# N, Promedios y STD en cada
# Micro Estrato para pasar a la Optimizacion
##########################################
StratosMarco=buildStrataDF(Marco)
nrow(StratosMarco)

########################
# Precisiones deseadas 
# sobre las variables Y
# en cada dominio
########################

DOM="DOM1"
CV1=0.05
CV2=0.05
domainvalue=1
errores=data.frame(DOM,CV1,CV2,domainvalue)
errores


#############################
#       Optimizacion        #
#############################
solu=optimizeStrata(strata=StratosMarco,
                    errors=errores,                        
                    iter=200,
                    pops=30,
                    initialStrata=30,
#                   addStrataFactor=0.00,
                    mut_chance=0.05,
                    elitism_rate=0.20,
                    minnumstr=5,
                    writeFiles=TRUE)


# tama#o muestral
sum(ceiling(solu$aggr_strata$SOLUZ))

solu$aggr_strata

solu$indices

table(solu$indices)

# solution1.txt == solu$indices 
# copia en txt de los indices o LABELs que le corresponden a los
# estratos viejos

Vinculos=read.table("solution1.txt")
Vinculos

# Nuevos Estratos por
# Reagrupamiento de los iniciales
# Actualizcion de Estratos y del Marco

EstratosNuevos=updateStrata(StratosMarco,solu)

MarcoNuevo=updateFrame(Marco,EstratosNuevos)

### Seleccion de la muestra bajo MSA
muestra=selectSample(MarcoNuevo,solu$aggr_strata)
## Resumen
# Total Pob
sum(muestra$WEIGHTS)
# Total de muestra por dominio
table(muestra$DOMAINVALUE)
# Total de unidades auto-representadas
autorep=muestra[muestra$FPC==1,]
sum(autorep$FPC)

################################################################################
#                            Solucion con Dominios
################################################################################
Marco=Marco.Empresas
Marco$Y1=Marco$VENTAS
Marco$Y2=Marco$VMP
Marco$X1=as.factor(Marco$REG)
Marco$X2=as.factor(Marco$ACTIV)
Marco$X3=var.bin(Marco$PO,bins=8)

##########################################
#  Los Dominios
##########################################
Marco$domainvalue=Marco$REG

##########################################
# Efectivos, Promedios y STD en cada
# Micro Estrato para pasar a la Optimizacion
##########################################
StratosMarco=buildStrataDF(Marco)


########################
# Precisiones deseadas 
# sobre las variables Y
# en cada dominio
########################
DOM=rep("DOM1",3)
CV1=c(0.05,0.07,0.10)
CV2=c(0.10,0.10,0.12)
domainvalue=c(1,2,3)
errores=data.frame(DOM,CV1,CV2,domainvalue)
errores

#############################
#       Optimizacion        #
#############################
solu=optimizeStrata(strata=StratosMarco,
                    errors=errores,                        
                    iter=100,
                    pops=30,
                    initialStrata=120,
                    mut_chance=0.05,
                    elitism_rate=0.20,
                    minnumstr=5,
                    writeFiles=TRUE)


EstratosNuevos=updateStrata(StratosMarco,solu,writeFile=TRUE)

MarcoNuevo=updateFrame(Marco,EstratosNuevos)

attach(EstratosNuevos)

Ordenado=EstratosNuevos[order(DOM1,LABEL),]

ComoAgrego=read.delim("strata_aggregation.txt")

# Muestra por Estrato
tapply(solu$aggr_strata$SOLUZ,solu$aggr_strata$DOM1,sum)

# Evaluacion de la solucion sobre un conjunto de muestras

evalSolution(MarcoNuevo,solu$aggr_strata,nsampl=50,writeFiles=TRUE)

CV_esperados=read.csv("expected_cv.csv")

CV_esperados=cbind(CV_esperados,errores)
CV_esperados


### Seleccion de la muestra bajo MSA
muestra=selectSample(MarcoNuevo,solu$aggr_strata)
## Resumen
# Total Pob
sum(muestra$WEIGHTS)
# Total Pob por dominio
tapply(muestra$WEIGHTS,muestra$DOMAINVALUE,sum)
# Total de muestra por dominio
table(muestra$DOMAINVALUE)
# Total de unidades auto-representadas
autorep=muestra[muestra$FPC==1,]
sum(autorep$FPC)
# Total de autorepresentadas por dominio
tapply(autorep$WEIGHTS,autorep$DOMAINVALUE,sum)
