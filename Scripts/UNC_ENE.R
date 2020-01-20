
ENE_CALC<-function(file, ## Archivo con actividades y factores
                   dir.s=getwd()) ## Directorio de scripts
{

##Librerias
require(openxlsx)
  
## FUnción de funciones
print("Cargando funciones...")

  source(paste0(dir.s,'/FIT_PFD.R'),encoding = "UTF-8")
  source(paste0(dir.s,'/Unc_FDP.R'),encoding = "UTF-8")
  source(paste0(dir.s,'/EMCALC.R'),encoding = "UTF-8")
  source(paste0(dir.s,'/SelDist.R'))
  source(paste0(dir.s,'/getmode.R'))
  source(paste0(dir.s,"/CALC.R"),encoding = "UTF-8")
  source(paste0(dir.s,"/SUMCALC.R"),encoding = "UTF-8")
  source(paste0(dir.s,'/inc_sum.r'),encoding = "UTF-8")

print("Cargando base de datos de Actividades...")
B2N=read.xlsx(file,sheet=1)
print("Cargando base de datos de Factores...")
F2N=read.xlsx(file,sheet=2)



#### Cálculos por categorias IPCC

# 3A1 Fermentación Entética 

### Seleccionando Actividad


cat("\014")  
print("Calculando Emisiones para Industrias de la Energía")

ECO2_1A1a=CALC(B2N,F2N,CAT="1A1a",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = "FE",
               by.act = "SELECT,C2",by.fct = "SELECT,C1") #OK
ECH4_1A1a=CALC(B2N,F2N,CAT="1A1a",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CH4",FACTS = "FE",
               by.act = "SELECT,C2",by.fct = "SELECT,C1") #OK
EN2O_1A1a=CALC(B2N,F2N,CAT="1A1a",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="N2O",FACTS = "FE",
               by.act = "SELECT,C2",by.fct = "SELECT,C1") #OK
E1A1a=rbind(ECO2_1A1a,ECH4_1A1a,EN2O_1A1a)
E1A1a$Sector=E1A1a$C1


ECO2_1A1b=CALC(B2N,F2N,CAT="1A1b",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = "FE",
               by.act = "SELECT,C2",by.fct = "SELECT,C1") #OK
ECH4_1A1b=CALC(B2N,F2N,CAT="1A1b",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CH4",FACTS = "FE",
               by.act = "SELECT,C2",by.fct = "SELECT,C1") #OK
EN2O_1A1b=CALC(B2N,F2N,CAT="1A1b",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="N2O",FACTS = "FE",
               by.act = "SELECT,C2",by.fct = "SELECT,C1") #OK
E1A1b=rbind(ECO2_1A1b,ECH4_1A1b,EN2O_1A1b)
E1A1b$Sector="Petróleo y Gas"

B2i=B2N[B2N$NOMBRE.DE.LA.ACTIVIDAD=="Produccion nacional de Coque",] 
F2i=F2N[F2N$NOMBRE.DEL.FACTOR=="F.E. COMBUSTION EN PRODUCCION DE COQUE",]

ECO2_1A1ci=CALC(B2i,F2i,CAT="1A1c",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = "FE",
                by.act = "SELECT",by.fct = "SELECT",fc=1e-3) #OK
ECH4_1A1ci=CALC(B2i,F2i,CAT="1A1c",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CH4",FACTS = "FE",
                by.act = "SELECT",by.fct = "SELECT",fc=1e-9) #OK

E1A1ci=rbind(ECO2_1A1ci,ECH4_1A1ci)
E1A1ci$C1="Producción de Coque"
E1A1ci$Sector="Minería de Carbón y Producción de Coque"

B2i=B2N[B2N$NOMBRE.DE.LA.ACTIVIDAD!="Produccion nacional de Coque",] 
F2i=F2N[F2N$NOMBRE.DEL.FACTOR!="F.E. COMBUSTION EN PRODUCCION DE COQUE",]

ECO2_1A1cii=CALC(B2i,F2i,CAT="1A1c",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = "FE",
                 by.act = "C2",by.fct = "C1") #OK
ECH4_1A1cii=CALC(B2i,F2i,CAT="1A1c",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CH4",FACTS = "FE",
                 by.act = "C2",by.fct = "C1") #OK
EN2O_1A1cii=CALC(B2i,F2i,CAT="1A1c",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="N2O",FACTS = "FE",
                 by.act = "C2",by.fct = "C1") #OK

E1A1cii=rbind(ECO2_1A1cii,ECH4_1A1cii,EN2O_1A1cii)
E1A1cii$Sector="Minería de Carbón y Producción de Coque"
E1A1cii$Sector[E1A1cii$C1=="Ext petroleo y gas natural"]="Petróleo y Gas"
E1A1c=rbind(E1A1ci,E1A1cii)



E1A1=rbind(E1A1a,E1A1b,E1A1c)

cat("\014")  
print("Calculando Emisiones para Consumo de energía en Transporte") #OK

ECO2_1A3e=CALC(B2N,F2N,CAT="1A3e",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = "FE",
               by.act = "C2",by.fct = "C1") #OK
ECH4_1A3e=CALC(B2N,F2N,CAT="1A3e",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CH4",FACTS = "FE",
               by.act = "C2",by.fct = "C1") #OK
EN2O_1A3e=CALC(B2N,F2N,CAT="1A3e",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="N2O",FACTS = "FE",
               by.act = "C2",by.fct = "C1") #OK
E1A3=rbind(ECO2_1A3e,
           ECH4_1A3e,
           EN2O_1A3e)
E1A3$Sector="Petróleo y Gas"



cat("\014")  
print("Calculando Emisiones fugitivas para la extracción de Carbon")

B2i=B2N[B2N$NOMBRE.DE.LA.ACTIVIDAD=="Producción de Carbón"&B2N$CLASIFICACIÓN.ADICIONAL=="Minas subterráneas",]
F2i=F2N[F2N$CLASIFICACIÓN.ADICIONAL=="Minas subterráneas"&!is.na(F2N$CLASIFICACIÓN.ADICIONAL),]
ECO2_1B1a=CALC(B2i,F2i,CAT="1B1a",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = "FE",
     by.act = "C1",by.fct = "C1",fc=0.00000183,PDF_def = "NORMAL")

ECH4_1B1a=CALC(B2N,F2N,CAT="1B1a",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CH4",FACTS = "FE",
               by.act = "SELECT,C1",by.fct = "SELECT,C1",PDF_def = "NORMAL",fc=0.00000067)
ECH4_1B1a$C2=c("Minería","Post-Minería")

E1B1=rbind(ECO2_1B1a,ECH4_1B1a)
E1B1$Sector="Minería de Carbón y Producción de Coque"



print("Calculando Emisiones fugitivas para la extracción de Petróleo")

### Reconstruyendo un nuevo data frame


B2i=B2N[B2N$NOMBRE.DE.LA.CATEGORIA.IPCC=="1B2a  Petróleo",]
F2i=F2N[F2N$NOMBRE.DE.LA.CATEGORIA.IPCC=="1B2a  Petróleo",]

# Venteo, Producción de Petróleo


B2ii=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Producción de petroleo",]
B2ii$CLASIFICACION.ADICIONAL.2="Venteo"
B2ii$CLASIFICACIÓN.ADICIONAL="Producción de petroleo"
F2ii=F2i[F2i$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN VENTEO"&F2i$CLASIFICACIÓN.ADICIONAL=="PRODUCCION DE PETROLEO",]
F2ii$CLASIFICACION.ADICIONAL.2="Venteo"
F2ii$CLASIFICACIÓN.ADICIONAL="Producción de petroleo"


### Venteo, Transporte
B2iii=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Petroleo transportado en camion cisterna",]
B2iii$CLASIFICACION.ADICIONAL.2="Venteo"
B2iii$CLASIFICACIÓN.ADICIONAL="Petroleo transportado en camion cisterna"
F2iii=F2i[F2i$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN VENTEO"&F2i$CLASIFICACIÓN.ADICIONAL=="TRANSPORTE",]
F2iii$CLASIFICACION.ADICIONAL.2="Venteo"
F2iii$CLASIFICACIÓN.ADICIONAL="Petroleo transportado en camion cisterna"


## Antorchas, Producción
B2iv=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Producción de petroleo",]
B2iv$CLASIFICACION.ADICIONAL.2="Antorchas"
B2iv$CLASIFICACIÓN.ADICIONAL="Producción de petroleo"
F2iv=F2i[F2i$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN QUEMA DE ANTORCHAS"&F2i$CLASIFICACIÓN.ADICIONAL=="PRODUCCION DE PETROLEO",]
F2iv$CLASIFICACION.ADICIONAL.2="Antorchas"
F2iv$CLASIFICACIÓN.ADICIONAL="Producción de petroleo"

# Antorchas, Prueba de Pozos

B2v=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Número de pozos perforados",]
B2v$CLASIFICACION.ADICIONAL.2="Antorchas"
B2v$CLASIFICACIÓN.ADICIONAL="Prueba de Pozos"
F2v=F2i[F2i$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN QUEMA DE ANTORCHAS"&F2i$CLASIFICACIÓN.ADICIONAL=="PRUEBA DE POZOS",]
F2v$CLASIFICACION.ADICIONAL.2="Antorchas"
F2v$CLASIFICACIÓN.ADICIONAL="Prueba de Pozos"


# Antorchas, servicios a Pozos

B2vi=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Número de pozos perforados",]
B2vi$CLASIFICACION.ADICIONAL.2="Antorchas"
B2vi$CLASIFICACIÓN.ADICIONAL="Perforación de pozos"
F2vi=F2i[F2i$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN QUEMA DE ANTORCHAS"&F2i$CLASIFICACIÓN.ADICIONAL=="PERFORACION DE POZOS",]
F2vi$CLASIFICACION.ADICIONAL.2="Antorchas"
F2vi$CLASIFICACIÓN.ADICIONAL="Perforación de pozos"

# Antorchas, SErvicios a pozos

B2vii=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Numero de pozos en produccion",]
B2vii$CLASIFICACION.ADICIONAL.2="Antorchas"
B2vii$CLASIFICACIÓN.ADICIONAL="Servicios a Pozos"
F2vii=F2i[F2i$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN QUEMA DE ANTORCHAS"&F2i$CLASIFICACIÓN.ADICIONAL=="SERVICIOS A POZOS",]
F2vii$CLASIFICACION.ADICIONAL.2="Antorchas"
F2vii$CLASIFICACIÓN.ADICIONAL="Servicios a Pozos"

# Otras, Distribución de productos refinados

B2viii=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Producción de petroleo",]
B2viii$CLASIFICACION.ADICIONAL.2="Otros"
B2viii$CLASIFICACIÓN.ADICIONAL="Producción y Refinación"
F2viii=F2i[F2i$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN TODAS LAS DEMAS ACTIVIDADES"&F2i$CLASIFICACIÓN.ADICIONAL=="PRODUCCION Y REFINACION",]
F2viii$CLASIFICACION.ADICIONAL.2="Otros"
F2viii$CLASIFICACIÓN.ADICIONAL="Producción y Refinación"

#Otras, Transporte GLP

B2ix=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="GLP producido",]
B2ix$CLASIFICACION.ADICIONAL.2="Otros"
B2ix$CLASIFICACIÓN.ADICIONAL="Transporte de GLP"
F2ix=F2i[F2i$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN TODAS LAS DEMAS ACTIVIDADES"&F2i$CLASIFICACION.ADICIONAL.2=="GASES LICUADOS"&!is.na(F2i$CLASIFICACION.ADICIONAL.2),]
F2ix$CLASIFICACION.ADICIONAL.2="Otros"
F2ix$CLASIFICACIÓN.ADICIONAL="Transporte de GLP"

#Otras, Transporte Tuberias

B2x=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Petroleo transportado por oleoductos",]
B2x$CLASIFICACION.ADICIONAL.2="Otros"
B2x$CLASIFICACIÓN.ADICIONAL="Transporte de petróleo por oleoductos"
F2x=F2i[F2i$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN TODAS LAS DEMAS ACTIVIDADES"&F2i$CLASIFICACION.ADICIONAL.2=="TUBERIAS"&!is.na(F2i$CLASIFICACION.ADICIONAL.2),]
F2x$CLASIFICACION.ADICIONAL.2="Otros"
F2x$CLASIFICACIÓN.ADICIONAL="Transporte de petróleo por oleoductos"


#Otras, Refinación

B2xi=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Petroleo refinado",]
B2xi$CLASIFICACION.ADICIONAL.2="Otros"
B2xi$CLASIFICACIÓN.ADICIONAL="Refinación"
F2xi=F2i[F2i$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN TODAS LAS DEMAS ACTIVIDADES"&F2i$CLASIFICACIÓN.ADICIONAL=="REFINACION",]
F2xi$CLASIFICACION.ADICIONAL.2="Otros"
F2xi$CLASIFICACIÓN.ADICIONAL="Refinación"


B2R=rbind(B2ii,B2iii,B2iv,B2v,B2vi,B2vii,B2viii,B2ix,B2x,B2xi)
F2R=rbind(F2ii,F2iii,F2iv,F2v,F2vi,F2vii,F2viii,F2ix,F2x,F2xi)


ECO2_1B2a=CALC(B2R,F2R,CAT="1B2a",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = "FE",
               by.act = "C1,C2",by.fct = "C1,C2",fc=1)
ECH4_1B2a=CALC(B2R,F2R,CAT="1B2a",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CH4",FACTS = "FE",
               by.act = "C1,C2",by.fct = "C1,C2",fc=1)
EN2O_1B2a=CALC(B2R,F2R,CAT="1B2a",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="N2O",FACTS = "FE",
               by.act = "C1,C2",by.fct = "C1,C2",fc=1)



print("Calculando Emisiones fugitivas para la extracción de Gas Natural")



### Reconstruyendo un nuevo data frame


B2i=B2N[B2N$NOMBRE.DE.LA.CATEGORIA.IPCC=="1B2b  Gas Natural",]
F2i=F2N[F2N$NOMBRE.DE.LA.CATEGORIA.IPCC=="1B2b  Gas Natural",]

# Ges Enviado a planta

B2ii=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Gas enviado a planta",]
tmp=c(array("Venteo",nrow(B2ii)),array("Antorchas",nrow(B2ii)),array("Otros",nrow(B2ii)))
B2ii=rbind(B2ii,B2ii,B2ii)
B2ii$CLASIFICACION.ADICIONAL.2=tmp
B2ii$CLASIFICACIÓN.ADICIONAL="Procesamiento de gas"


F2ii=F2i[F2i$CLASIFICACIÓN.ADICIONAL=="PROCESAMIENTO DE GAS",]
F2ii[F2ii$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN VENTEO","CLASIFICACION.ADICIONAL.2"]="Venteo"
F2ii[F2ii$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN QUEMA DE ANTORCHAS","CLASIFICACION.ADICIONAL.2"]="Antorchas"
F2ii[F2ii$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN TODAS LAS DEMAS ACTIVIDADES","CLASIFICACION.ADICIONAL.2"]="Otros"
F2ii$CLASIFICACIÓN.ADICIONAL="Procesamiento de gas"


# Producción de Gas Fiscalizada

B2iii=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Produccion de gas fiscalizada",]

tmp=c(array("Antorchas",nrow(B2iii)),array("Otros",nrow(B2iii)))
B2iii=rbind(B2iii,B2iii)
B2iii$CLASIFICACION.ADICIONAL.2=tmp
B2iii$CLASIFICACIÓN.ADICIONAL="Producción de gas"

F2iii=F2i[F2i$CLASIFICACIÓN.ADICIONAL=="PRODUCCION DE GAS",]
F2iii[F2iii$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN QUEMA DE ANTORCHAS","CLASIFICACION.ADICIONAL.2"]="Antorchas"
F2iii[F2iii$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN TODAS LAS DEMAS ACTIVIDADES","CLASIFICACION.ADICIONAL.2"]="Otros"
F2iii$CLASIFICACIÓN.ADICIONAL="Producción de gas"

## Transmisión y almacenamiento

B2iv=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Gas comercializable" ,]

tmp=c(array("Venteo",nrow(B2iv)),array("Otros",nrow(B2iv)),array("Otros",nrow(B2iv)))
tmp2=c(array("Transmisión y Almacenamiento",nrow(B2iv)),array("Transmisión",nrow(B2iv)),array("Almacenamiento",nrow(B2iv)))
B2iv=rbind(B2iv,B2iv,B2iv)
B2iv$CLASIFICACION.ADICIONAL.2=tmp
B2iv$CLASIFICACIÓN.ADICIONAL=tmp2

F2iv=F2i[F2i$CLASIFICACIÓN.ADICIONAL=="TRANSMISION Y ALMACENAMIENTO",]
F2iv$CLASIFICACIÓN.ADICIONAL="Transmisión y Almacenamiento"
F2iv$CLASIFICACIÓN.ADICIONAL[F2iv$CLASIFICACION.ADICIONAL.2=="ALMACENAMIENTO"&!is.na(F2iv$CLASIFICACION.ADICIONAL.2)]<-"Almacenamiento"
F2iv$CLASIFICACIÓN.ADICIONAL[F2iv$CLASIFICACION.ADICIONAL.2=="TRANSMISION"&!is.na(F2iv$CLASIFICACION.ADICIONAL.2)]<-"Transmisión"

F2iv[F2iv$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN VENTEO","CLASIFICACION.ADICIONAL.2"]="Venteo"
F2iv[F2iv$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN QUEMA DE ANTORCHAS","CLASIFICACION.ADICIONAL.2"]="Antorchas"
F2iv[F2iv$NOMBRE.DEL.FACTOR=="F.E. EMISIONES FUGITIVAS EN TODAS LAS DEMAS ACTIVIDADES","CLASIFICACION.ADICIONAL.2"]="Otros"

### Demanda de gas

B2v=B2i[B2i$NOMBRE.DE.LA.ACTIVIDAD=="Distribucion de gas" ,]
B2v$CLASIFICACIÓN.ADICIONAL="Distribución de gas"
B2v$CLASIFICACION.ADICIONAL.2="Otros"

F2v=F2i[F2i$CLASIFICACIÓN.ADICIONAL=="DISTRIBUCION DE GAS",]
F2v$CLASIFICACIÓN.ADICIONAL="Distribución de gas"
F2v$CLASIFICACION.ADICIONAL.2="Otros"

B2R=rbind(B2ii,B2iii,B2iv,B2v)
F2R=rbind(F2ii,F2iii,F2iv,F2v)


ECO2_1B2b=CALC(B2R,F2R,CAT="1B2b",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = "FE",
               by.act = "C1,C2",by.fct = "C1,C2",fc=1)
ECH4_1B2b=CALC(B2R,F2R,CAT="1B2b",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CH4",FACTS = "FE",
               by.act = "C1,C2",by.fct = "C1,C2",fc=1)
EN2O_1B2b=CALC(B2R,F2R,CAT="1B2b",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="N2O",FACTS = "FE",
               by.act = "C1,C2",by.fct = "C1,C2",fc=1)


E1B2=rbind(ECO2_1B2a,ECO2_1B2b,
           ECH4_1B2a,ECH4_1B2b,
           EN2O_1B2a,EN2O_1B2b)


E1B2$Sector = "Petróleo y Gas"


### Petroquímica 2B8

ECO2_2B8b=CALC(B2N,F2N,CAT="2B8b",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = c("FE","AG"),
               by.act = c("SELECT","SELECT"),by.fct = c("SELECT","SELECT"),fc=1e-3,f1="VALOR_AG",eq="em=act*fe*f1")
ECH4_2B8b=CALC(B2N,F2N,CAT="2B8b",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CH4",FACTS = "FE",
               by.act = "SELECT",by.fct = "SELECT",fc=1e-6)

ECO2_2B8c=CALC(B2N,F2N,CAT="2B8c",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = "FE",
               by.act = "SELECT",by.fct = "SELECT",fc=1e-3)

ECH4_2B8c=CALC(B2N,F2N,CAT="2B8c",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CH4",FACTS = "FE",
               by.act = "SELECT",by.fct = "SELECT",fc=1e-6)


E2B8=rbind(ECO2_2B8b,ECH4_2B8b,ECO2_2B8c,ECH4_2B8c)
E2B8$Sector = "Petróleo y Gas"
### Ferro niquel

ECO2_2C2=CALC(B2N,F2N,CAT="2C2",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = "FE",
               by.act = "SELECT",by.fct = "SELECT",fc=1e-3)

E2C2=ECO2_2C2
E2C2$Sector="Producción de Ferroniquel"

### Difusión por embalses

ECO2_3B4a=CALC(B2N,F2N,CAT="3B4a",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CO2",FACTS = c("FE","AT"),
               by.act = c("C2","C1"),by.fct = c("C1","C1"),fc=365*1e-6,fe="VALOR_FE",f1="VALOR_AT",eq="em=act*fe*f1")

ECH4_3B4a=CALC(B2N,F2N,CAT="3B4a",SELECT_ACT = "REGION",SELECT_FACTS = "REGION",GAS="CH4",FACTS = c("FE","AT"),
               by.act = c("C2","C1"),by.fct = c("C1","C1"),fc=365*1e-6,fe="VALOR_FE",f1="VALOR_AT",eq="em=act*fe*f1")

E3B4=rbind(ECO2_3B4a,ECH4_3B4a)
E3B4$Sector="Sistema Interconectado Nacional" 

### Agregación en grandes Categorías
cat("\014")  
print("Creando archivo R2 de compilación de resultados")

R_2=rbind(E1A1,E1A3,E1B1,E1B2,E2B8,E2C2,E3B4)

write.csv(R_2,paste0('R2 ENE.csv'),row.names=F)
return(R_2)

}