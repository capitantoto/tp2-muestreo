library(sampling)
library(survey)
library(dplyr)
library(Matrix)

load("~/Downloads/MPF UBA 2016/Clases/Base.Productores 2015.RData")

Marco1=Base.Productores
attach(Marco1)
head(Marco1)

###################
#    Dise#os      #
#      PPT        #
###################

# Creacion de las PIK
PIK=inclusionprobabilities(VENTAS,25)
# Incorporacion de las PIK al marco
Marco1$UNO=1
Marco1=cbind(Marco1,PIK)

############ PPT Poisson ##############
sample1=UPpoisson(PIK)
seleccion1=(1:nrow(Marco1))[sample1==1]
seleccion1
length(seleccion1)
poisson=getdata(Marco1,sample1)

############# PPT Sampford ############

# Seleccion de la Muestra
sample2=UPsampford(PIK)
# Indices de la unidades seleccionadas
seleccion2=(1:nrow(Marco1))[sample2==1]
seleccion2
# Muestra PPT por Sampford
sampf=getdata(Marco1,sample2)

###### Tambien se puede >>>>>>>
# sampf=getdata(Marco1,UPsampford(PIK))
# sampf=getdata(Marco1,UPsampford(inclusionprobabilities(VENTAS,25)))

##### Pikl de Sampford ##############
PIKLsampf=UPsampfordpi2(PIK)

#### ES INSESGADO LA VHT (Pikl>0) ?
all(PIKLsampf>0)

#### ES DeltaKL <=0 para VSGY<=VHT ?
all(PIKLsampf-diag(diag(PIK))<=0)

# Probabilidades de 2do Orden de la muestra
PIKLsampf=PIKLsampf[seleccion2,seleccion2]

######## PPT Systematic ##### 
sample3=UPsystematic(PIK)
seleccion3=(1:nrow(Marco1))[sample3==1]
seleccion3
sampSys=getdata(Marco1,sample3)
PIKLsys=UPsystematicpi2(PIK)

#### ES INSESGADO LA VHT (Pikl>0) ?
all(PIKLsys>0)

#### ES DeltaKL <=0 para VSGY<=VHT ?
all(PIKLsys-diag(diag(PIK))<=0)

######## PPT Tille ################# 
sample4=UPtille(PIK)
seleccion4=(1:nrow(Marco1))[sample4==1]
seleccion4
sampTille=getdata(Marco1,sample4)
PIKLtille=UPtillepi2(PIK)

#### ES INSESGADO LA VHT (Pikl>0) ?
all(PIKLtille>0)

#### ES DeltaKL <=0 para VSGY<=VHT ?
all(PIKLtille-diag(diag(PIK))<=0)

# Probabilidades de 2do Orden de la muestra
PIKLtille=PIKLtille[seleccion2,seleccion2]

#########################
# ESTIMACION con Sampling
#########################
mean(Marco1$TAMANIO)
N=nrow(Marco1)

# Bajo MSA #

msas=getdata(Marco1,srswor(25,nrow(Marco1)))
PromHT=HTestimator(msas$TAMANIO,pik=rep(25/764,25))/N
PromHT
VarMSA=varest(msas$TAMANIO,pik=rep(25/764,25))/N**2
cv=sqrt(VarMSA)/PromHT
cv

# Promedio HT Poisson

PiklPoisson=outer(PIK,PIK,"*")
PiklPoisson=PiklPoisson[seleccion1,seleccion1]
PromHT=HTestimator(poisson$TAMANIO,pik=poisson$PIK)/N
PromHT
VariHT=varHT(poisson$TAMANIO,pikl=PiklPoisson,1)/N**2
cv=sqrt(VariHT)/PromHT
cv

# Promedio HT Sampford

PromHT=HTestimator(sampf$TAMANIO,pik=sampf$PIK)/N
PromHT
VariHT=varHT(sampf$TAMANIO,pikl=PIKLsampf,2)/N**2
VariHT
cv=sqrt(VariHT)/PromHT
cv

# Promedio como cociente de Totales estimados

Nsampf=HTestimator(sampf$UNO,pik=sampf$PIK)
Nsampf

PromRHT=HTestimator(sampf$TAMANIO,pik=sampf$PIK)/Nsampf
PromRHT

VariRHT=vartaylor_ratio(sampf$TAMANIO,sampf$UNO,pikl=PIKLsampf)
VariRHT

cv=sqrt(VariRHT$estvar)/PromRHT
cv

# Promedio Hajek (Cocientes de Totales)

PromH=Hajekestimator(sampf$TAMANIO,pik=sampf$PIK,type="mean")
PromH

VariH=vartaylor_ratio(sampf$TAMANIO,sampf$UNO,pikl=PIKLsampf)
VariH

cv=sqrt(VariH$estvar)/PromH
cv

# Promedio HT Tille

PromHT_=HTestimator(sampTille$TAMANIO,pik=sampTille$PIK)/N
PromHT_
VariHT_=varHT(sampTille$TAMANIO,pikl=PIKLtille,2)/N**2
cv=sqrt(VariHT_)/PromHT_
cv



#######################
# Estimacion con Survey
#######################

DSampf=svydesign(ids=~1,fpc=~PIK,data=sampf,pps=ppsmat(PIKLsampf),variance="YG")

Prom=svymean(~TAMANIO,DSampf)
Prom
cv(Prom)

DSampf=svydesign(ids=~1,fpc=~PIK,data=sampf,pps=ppsmat(PIKLsampf),variance="HT")

Prom=svymean(~TAMANIO,DSampf)
Prom
cv(Prom)

svyratio(~VENTAS,~TAMANIO,DSampf)

cv(svyratio(~TAMANIO,~UNO,DSampf))

svyhist(~TAMANIO,DSampf)

svyquantile(~TAMANIO,DSampf,c(0.25,0.5,0.75),ci=TRUE)

cdf.est=svycdf(~TAMANIO,DSampf)
cdf.est
## function
cdf.est[[1]]

## Comparacion entre lad CDF Poblacional y la Muestral Expandida

cdf.pop=ecdf(Marco1$TAMANIO)
plot(cdf.pop, main="Poblacion vs Estimada", xlab="TAMANIO")
lines(cdf.est[[1]],col.points="red")

#############################
# Disenios Con Entropia Alta
# Aprox. de las PIKL   
#############################
# Package sampling
# Metdodo de DEVILLE

VarAprox=varest(sampf$TAMANIO,pik=sampf$PIK)/N**2
cv=sqrt(VarAprox)/PromHT
cv

VarAprox=varest(sampTille$TAMANIO,pik=sampTille$PIK)/N**2
cv=sqrt(VarAprox)/PromHT_
cv

#############################
# Disenios Con Entropia Alta
# Aprox. de las PIKL   
#############################
# Package survey

# Overton
Dsampf_O=svydesign(ids=~1,fpc=~PIK,data=sampf,pps="overton")
Prom=svymean(~TAMANIO,Dsampf_O)
Prom
cv(Prom)

# Brewer
Dsampf_B=svydesign(ids=~1,fpc=~PIK,data=sampf,pps="brewer")
Prom=svymean(~TAMANIO,Dsampf_B)
Prom
cv(Prom)

# Hartley & Rao
Dsampf_HR=svydesign(ids=~1,fpc=~PIK,data=sampf,pps=HR(sum(Marco1$PIK**2)/25))
Prom=svymean(~TAMANIO,Dsampf_HR)
Prom
cv(Prom)
 




