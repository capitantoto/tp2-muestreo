library(dplyr)
library(ggplot2)
library(stratification)
load("~/Downloads/MPF 2016 UBA/Clases/Base.Productores 2016.RData")
attach(Base.Productores)
head(Base.Productores)

# Verificacion de Asimetria

nrow(Base.Productores)
summary(TAMANIO)

plot(density(TAMANIO))


####################
# Estratificacion
####################
geo=strata.geo(x=TAMANIO,CV=0.03,Ls=3,alloc=c(0.5,0,0.5))
geo
geo$bh
geo=strata.geo(x=TAMANIO,n=40,Ls=3,alloc=c(0.5,0,0.5))
geo
geo$bh
plot(geo)

############################################
# Kozak para determinar 1 Take-All
############################################
Take1=strata.LH(x=TAMANIO,CV=0.05,Ls=2,takeall=1,algo="Kozak")
Take1$nsol
Take1
Take1$bh
# Tamanio Muestral sin Autorepresentados para cv0=0.05
n=(sd(TAMANIO)/(mean(TAMANIO)*0.05))**2
n
plot(Take1)

###########################################
#        Kozak No Model
###########################################
Kozak=strata.LH(x=TAMANIO,CV=0.02,Ls=4,alloc=c(0.5,0,0.5))
Kozak
plot(Kozak)

#  25% no-respuesta y con Autorepresentados
Kozak1=strata.LH(x=TAMANIO,CV=0.02,Ls=4,alloc=c(0.5,0,0.5),takeall=1,
                rh=0.75)
Kozak1
Kozak1$bh
Kozak1$meanh
Kozak1$varh
Kozak1$nh
Kozak1$Nh
Kozak1$nsol
Kozak1$iter.detail[1:10,]
Kozak1$run.detail
Kozak1$run.min
plot(Kozak1)

##################################
# Evaluacion de la Estratificacion
# en otras Variables de Estudio 
#################################
var.strata(Kozak1,VENTAS)


##################################
# Dado n=n0
# minimizar CV
#################################


strata.LH(x=TAMANIO,n=75,Ls=4,alloc=c(0.5,0,0.5),takeall=1,
                 rh=0.75)


##################################
# Modelos de Discrepancia
##################################

# Model="loglinear"

KozakLog=strata.LH(x=TAMANIO,CV=0.03,Ls=4,alloc=c(0.5,0,0.5),takeall=0,
                   rh=0.8,model="loglinear",
                   model.control=list(beta=1,sig2=0.05,ph=0.98))
KozakLog
KozakLog$bh

# Model="linear"

KozakReg=strata.LH(x=TAMANIO,CV=0.03,Ls=4,alloc=c(0.5,0,0.5),takeall=1,
                   rh=0.8,model="linear",
                   model.control=list(beta=1, sig2=1.8,gamma=1.7))
KozakReg
KozakReg$bh

# Model="random"

KozakR=strata.LH(x=TAMANIO,CV=0.03,Ls=4,alloc=c(0.5,0,0.5),takeall=0,
                 rh=0.98,model="random",model.control=list(epsilon=0.03))
KozakR
KozakR$bh

### Comandos Adicionales ####

plot(KozakLog,main="Estratificacion H=4",xlab="Tama#o del Establecimiento")


#################################
# Actualizacion de la Base
#################################

Estrato=KozakLog$stratumID
base=cbind(Base.Productores,Estrato)

Ponderacion=KozakLog$Nh/KozakLog$nh
Pik=1/Ponderacion

stratumID=unique(KozakLog$stratumID)

resumen=cbind.data.frame(stratumID,KozakLog$Nh,KozakLog$nh,Pik,Ponderacion)
colnames(resumen)=c("Estrato","Nh","nh","Pik","Ponderacion")
resumen

base=merge(base,resumen,by="Estrato")

##################################################################
# Seleccion de la Muestra Estratificacion con muestreo Sistematico
##################################################################

library(sampling)
seleccion=strata(base,stratanames="Estrato",
                 size=KozakLog$nh,
                 method="systematic",
                 pik=base$Pik)

muestra1=getdata(base,seleccion)
str(muestra1)
muestra1%>%group_by(Estrato)%>%summarize(n())

