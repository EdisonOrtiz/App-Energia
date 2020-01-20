
CALC<-function(B2,F2,SELECT_ACT="DEPARTAMENTO",SELECT_FACTS="DEPARTAMENTO",CAT="3A1a",FACTS="FE",GAS="CH4",
               act="VALOR_ACT",fe="VALOR_FE",f1="",f2="",f3="",f4="",f5="",
               eq="em=act*fe",fc=1e-6,it=10000,fit_method="mme",PDF_def="None",
               inc_calc="BOTH",dir.s=dir.s,by.act=c("C2"),by.fct=c("C1"))
  
{
### Seleccionando Actividad
require(triangle)

  
print(paste0("Calculando ",GAS," para la categoría ",CAT))


### Seleccionando Actividades
TMP=B2$NOMBRE.DE.LA.CATEGORIA.IPCC
TMP=substr(TMP,1,4)
TMP=gsub(" ","",TMP)
ACT=B2[TMP==CAT,c("NOMBRE.DE.LA.CATEGORIA.IPCC",SELECT_ACT,"NOMBRE.DE.LA.ACTIVIDAD","ANO","CLASIFICACIÓN.ADICIONAL","CLASIFICACION.ADICIONAL.2",
                  "VALOR","INCERTIDUMBRE.NEGATIVA","INCERTIDUMBRE.POSITIVA")]

names(ACT)=c("NCAT.IPCC","SELECT","N.ACT","ANO","C1","C2","VALOR_ACT","UN_ACT","UP_ACT")

ACT=SelDist(ACT,VALOR="VALOR_ACT",UN="UN_ACT",UP="UP_ACT",SUFFIX = "_ACT") ## Seleccionado distribuciones

#Seleccionando factores

### Seleccionando Actividades
TMP=F2$NOMBRE.DE.LA.CATEGORIA.IPCC
TMP=substr(TMP,1,4)
TMP=gsub(" ","",TMP)
FCT=F2[TMP==CAT,c("NOMBRE.DE.LA.CATEGORIA.IPCC",SELECT_FACTS,"NOMBRE.DEL.FACTOR","ANO","CLASIFICACIÓN.ADICIONAL","CLASIFICACION.ADICIONAL.2",
                  "SIMBOLO","GAS","VALOR","INCERTIDUMBRE.NEGATIVA","INCERTIDUMBRE.POSITIVA")]

names(FCT)=c("NCAT.IPCC","SELECT","N.FCT","ANO","C1","C2","SB","GAS","VALOR","UN","UP")


DAT=ACT
for (FACT in FACTS)
{
  h=which(FACT==FACTS)
  by.x=by.act[h];by.x=unlist(strsplit(by.x,","));by.x=gsub(" ","",by.x);by.x=c(by.x)
  by.y=by.fct[h];by.y=unlist(strsplit(by.y,","));by.y=gsub(" ","",by.y);by.y=c(by.y)
  FEh=FCT[FCT$SB==FACT&(FCT$GAS==GAS|FCT$GAS==""|is.na(FCT$GAS)),]
  FEh=SelDist(FEh,SUFFIX = paste0("_",FACT))
  names(FEh)[9:11]=paste0(names(FEh)[9:11],"_",FACT)
  
  DAT=merge(DAT,FEh[c(by.y,names(FEh)[c(9:15)])],by.x=by.x,by.y=by.y,all.x=T)
}
  
suffix=paste0("_",c("ACT",FACTS))



if (inc_calc=="PROP"|inc_calc=="BOTH")
{
RES_PROP=EMCALC(DAT,UNC_type = "PROP",act=act,fe=fe,eq=eq,f1=f1,f2=f2,f3=f3,f4=f4,f5=f5,it=it,fc=fc,suffix=suffix)
CALC1=RES_PROP[c("ANO","SELECT","NCAT.IPCC","C1","C2","EM","UN_EM","UP_EM","PAR1_EM","PAR2_EM","PAR3_EM","DIST_EM")]
CALC1$Method="PROP"
}else{CALC1=NULL}

if (inc_calc=="MC"|inc_calc=="BOTH")
{
RES_MC=EMCALC(DAT,UNC_type = "MONTECARLO",act=act,fe=fe,eq=eq,f1=f1,f2=f2,f3=f3,f4=f4,f5=f5,it=it,fc=fc,suffix=suffix,method=fit_method,PDF_def=PDF_def)

CALC2=RES_MC[c("ANO","SELECT","NCAT.IPCC","C1","C2","EM","UN_EM","UP_EM","PAR1_EM","PAR2_EM","PAR3_EM","DIST_EM")]
CALC2$Method="MC"
}else{CALC2=NULL}

CALCR=rbind(CALC1,CALC2)
CALCR$GAS=GAS
CALCR=CALCR[order(CALCR$Method,CALCR$C1,CALCR$C2,CALCR$ANO),]
return(CALCR)

}
