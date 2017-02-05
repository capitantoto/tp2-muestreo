library(sampling)
library(dplyr)


load("~/Downloads/Cursos de Muestreo/MPF 2016 UBA/Marco.Empresas 2016.RData")

Marco1=Marco.Empresas
attach(Marco1)

#########################################
# Seleccion de una Muestra Estratificada 
#########################################

# Opcion "srswor"

Marco1=Marco1[order(Marco1$REG),]

est1=strata(Marco1,
            stratanames="REG",
            size=c(100,100,200),
            method="srswor",
            description=TRUE)

MEsrswor=getdata(Marco1,est1)
head(MEsrswor)


# Opcion systematic
Marco1=cbind(Marco1,UNO=1)
est2=strata(Marco1,stratanames="REG",size=c(100,100,200),method="systematic",
            pik=UNO,description=TRUE)
MEsystem=getdata(Marco1,est2)

# Dos variables estratificadoras

table(REG,ACTIV)

# Muestra de 100,150 y 150 por Region
# Muestra de 80 por c/Actividad
# Total de la muestra=400

Marco1=Marco1[order(Marco1$REG,Marco1$ACTIV),]

est4=strata(Marco1,stratanames=c("REG","ACTIV"),size=c(rep(20,5),rep(30,5),rep(30,5)),
            method="srswor",description=TRUE)
ME2Var=getdata(Marco1,est4)


# Asignacion Proporcional

N_h=table(REG)
N_h
n_h=round(N_h*400/nrow(Marco1))
n_h
est5=strata(Marco1,stratanames="REG",size=n_h,method="srswor",
            description=TRUE)
MEasigProp=getdata(Marco1,est5)

###### Estimacion con SAMPLING ###

Tv=HTestimator(MEsrswor$VENTAS,MEsrswor$Prob)
Tv
Tv_=HTstrata(MEsrswor$VENTAS,MEsrswor$Prob,MEsrswor$Stratum,description = TRUE)
Tv_
STDv=sqrt(varest(MEsrswor$VENTAS,pik=MEsrswor$Prob))
STDv
CV=STDv*100/Tv
CV


##### Estimaciones con SURVEY ###

# Totales
MEdesign=svydesign(ids=~1,fpc=~Prob,data=MEsrswor,strata=~Stratum)
Total=svytotal(~VENTAS,MEdesign)
cat(Total,cv(Total),vcov(Total),SE(Total),sqrt(vcov(Total)))

# Promedios
svymean(~VENTAS+PO,MEdesign)
confint(svymean(~VENTAS+PO,MEdesign))

# Proporciones
Prop=svymean(~factor(TIPO),MEdesign)
Prop
# Intervalo de Confianza
confint(Prop)

# by N x REG se involucro en el disenio
svyby(~VENTAS,~factor(REG),MEdesign,svymean,vartype=c("se","cvpct","ci"))
# by N x ACTIV NO se involucro en el disenio
svyby(~VENTAS,~factor(ACTIV),MEdesign,svymean,vartype=c("se","cvpct","ci"))

# Tablas con Totales por Dominio
tabla1=svyby(~VENTAS,~factor(TIPO),MEdesign,svytotal,vartype=c("cvpct"))
ftable(tabla1)
# Tablas con Razones por Dominio
tabla2=svyby(~VENTAS,~factor(REG)+factor(TIPO),MEdesign,svymean,vartype=c("cvpct"))
round(ftable(tabla2),2)


# Estimacion de Razones o Cocientes 

R=svyratio(~VENTAS,~VMP,MEdesign,deff=TRUE)
R
deff(R)
SE(R)
cv(R)*100

# Tablas con Estimaciones por Cociente por Dominios
Rby=svyby(~VENTAS,by=~factor(TIPO),denominator=~VMP,design=MEdesign,svyratio,vartype=c("cvpct"))
Rby

# Cuartiles
svyquantile(~VENTAS,MEdesign,c(.25,.5,.75))
# Deciles
svyquantile(~VENTAS,MEdesign,seq(0.1,1,0.1))
# Total de casos muestrales
svyby(~VENTAS, ~TIPO, MEdesign, unwtd.count)

# Funcion de Distribucion
FD=svycdf(~VENTAS+PO,MEdesign)
plot(FD)
FD[[1]](3000)
FD[[2]](150)

#########################
# Post-Estratificacion
#########################

# Seleccion Muestra MSA
MSA=getdata(Marco1,srswor(400,4782))
MSA$Pik=400/4782

# Definicion Disenio
MSADesign=svydesign(id=~1,data=MSA,fpc=~Pik)
T_MSA=svymean(~VENTAS,MSADesign)
T_MSA
cv(T_MSA)

# Postestratificacion

table(MSA$REG)
table(Marco1$REG)

PopREG=data.frame(REG=c(1,2,3),
                 Freq=c(1831,1632,1769))
PopREG

# Defino Disenio POST-Estratificado
PostDesign=postStratify(MSADesign,
                        strata = ~REG,
                        population = PopREG)

T_POST=svymean(~VENTAS,PostDesign)
T_POST
cv(T_POST)

# Algunos Graficos con el Disenio Post-Estratificado

plot(svytable(~REG+TIPO, PostDesign))

mns=svyby(~VENTAS, ~ACTIV+REG, svytotal, design=PostDesign)

dotchart(mns,
         cex=0.8,col=rep(c("royalblue","magenta"),each=5),
         xlab="Total de VENTAS x ACTIVIDAD y REGION")

barplot(mns,horiz=TRUE,xlab="Ventas en miles$")

