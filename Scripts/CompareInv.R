CompareInv<-function(Repo,
                     R2)
{
  require("openxlsx")
  require("reshape2")
  require("ggplot2")
  Repo="C:/Users/edison ortiz/OneDrive/ENECALC_APP/Emisiones Base/Reporte_2010_2018_v4.xlsx"
  R2="C:/Users/edison ortiz/OneDrive/ENECALC_APP/Emisiones Base/R2 ENE.csv"
  DataR=read.xlsx(Repo,sheet="Calculo_Emisiones",startRow = 14,colNames = T)
  DataR2=read.csv(R2)
  DataR2=DataR2[DataR2$Method=="MC",]
  
  ### Categoría 1
  
  ErrT=NULL
  
  cat="1A1a"
  TMP1=DataR$Sucategoria.1
  TMP1=substr(TMP1,1,4)
  TMP1=gsub(" ","",TMP1)
  TMP2=DataR2$NCAT.IPCC 
  TMP2=substr(TMP2,1,4)
  TMP2=gsub(" ","",TMP2)
  Data.tmp1=DataR[TMP1==cat,]
  Data.tmp2=DataR2[TMP2==cat,]
  Data.tmp1=Data.tmp1[!is.na(Data.tmp1$Año),]
  Data.tmp2=Data.tmp2[!is.na(Data.tmp2$ANO),]
  Data.tmp1$C1="Sistema Interconectado Nacional"
  Data.tmp1$C1[Data.tmp1$Sucategoria.2=="1A1ai  Generación de electricidad - ZNI"]="Zonas No Interconectadas"
  Data.tmp1$C2=Data.tmp1$Combustible.BECO
  Data.tmp1$ANO=Data.tmp1$Año
  D1=Data.tmp1[c("ANO","C1","C2","Emisiones.CO2.(Gg.CO2)","Emision.CH4.(Gg.CH4)","Emisiones.N2O.(Gg.N2O)")]
  names(D1)[4:6]=c("CO2","CH4","N2O")
  D1=melt(D1,id.vars = c("ANO","C1","C2"))
  names(D1)[4:5]=c("GAS","EMR")
  D2=Data.tmp2[c("ANO","NCAT.IPCC","SELECT","C1","C2","GAS","EM")]
  DM=merge(D2,D1,by=c("ANO","C1","C2","GAS"),all=T)
  DM$Err=100*abs(DM$EM-DM$EMR)/DM$EM
  g1A1a=ggplot(DM,aes(C2,Err))+geom_boxplot(aes(fill=GAS))+theme_bw()+ylab("Error [%]")+xlab("Clasificación 1")+ggtitle(cat)+facet_wrap(~C1)+
    theme(axis.text.x = element_text(angle = 90))
  Err=DM[DM$Err>1&!is.na(DM$Err),]
  ErrT=rbind(ErrT,Err)
  
  #cat 1A1b
  cat="1A1b"
  TMP1=DataR$Sucategoria.1
  TMP1=substr(TMP1,1,4)
  TMP1=gsub(" ","",TMP1)
  TMP2=DataR2$NCAT.IPCC 
  TMP2=substr(TMP2,1,4)
  TMP2=gsub(" ","",TMP2)
  Data.tmp1=DataR[TMP1==cat,]
  Data.tmp2=DataR2[TMP2==cat,]
  Data.tmp1=Data.tmp1[!is.na(Data.tmp1$Año),]
  Data.tmp2=Data.tmp2[!is.na(Data.tmp2$ANO),]
  Data.tmp1$C1="Combustible usado para refinación"
  Data.tmp1$C2=Data.tmp1$Combustible.BECO
  Data.tmp1$ANO=Data.tmp1$Año
  D1=Data.tmp1[c("ANO","C1","C2","Emisiones.CO2.(Gg.CO2)","Emision.CH4.(Gg.CH4)","Emisiones.N2O.(Gg.N2O)")]
  names(D1)[4:6]=c("CO2","CH4","N2O")
  D1=melt(D1,id.vars = c("ANO","C1","C2"))
  names(D1)[4:5]=c("GAS","EMR")
  D2=Data.tmp2[c("ANO","NCAT.IPCC","SELECT","C1","C2","GAS","EM")]
  DM=merge(D2,D1,by=c("ANO","C1","C2","GAS"),all=T)
  DM$Err=100*abs(DM$EM-DM$EMR)/DM$EM
  Err=DM[DM$Err>1&!is.na(DM$Err)|(is.na(DM$EM)|is.na(DM$EMR)),]
  ErrT=rbind(ErrT,Err)
  g1A1b=ggplot(DM,aes(C2,Err))+geom_boxplot(aes(fill=GAS))+theme_bw()+ylab("Error [%]")+xlab("Clasificación 1")+ggtitle(cat)+facet_wrap(~C1)+
    theme(axis.text.x = element_text(angle = 90))
  #cat 1A1c
  cat="1A1c"
  TMP1=DataR$Sucategoria.1
  TMP1=substr(TMP1,1,4)
  TMP1=gsub(" ","",TMP1)
  TMP2=DataR2$NCAT.IPCC 
  TMP2=substr(TMP2,1,4)
  TMP2=gsub(" ","",TMP2)
  Data.tmp1=DataR[TMP1==cat,]
  Data.tmp2=DataR2[TMP2==cat,]
  Data.tmp1=Data.tmp1[!is.na(Data.tmp1$Año),]
  Data.tmp2=Data.tmp2[!is.na(Data.tmp2$ANO),]
  Data.tmp1$C1=Data.tmp1$Sucategoria.3
  Data.tmp1$C1[is.na(Data.tmp1$C1)]="Producción de Coque"
  Data.tmp1$C2=Data.tmp1$Combustible.BECO
  Data.tmp1$ANO=Data.tmp1$Año
  D1=Data.tmp1[c("ANO","C1","C2","Emisiones.CO2.(Gg.CO2)","Emision.CH4.(Gg.CH4)","Emisiones.N2O.(Gg.N2O)")]
  names(D1)[4:6]=c("CO2","CH4","N2O")
  D1=melt(D1,id.vars = c("ANO","C1","C2"))
  names(D1)[4:5]=c("GAS","EMR")
  D1$C2[D1$C2=="Gasolina para motores"]="Gasolina Motor"
  D1$C2[D1$C2=="Gas natural"]="Gas Natural"
  D2=Data.tmp2[c("ANO","NCAT.IPCC","SELECT","C1","C2","GAS","EM")]
  D2$C2=as.character(D2$C2)
  D2$C2[D2$C1=="Producción de Coque"]="Coque"
  DM=merge(D2,D1,by=c("ANO","C1","C2","GAS"),all=T)
  DM$Err=100*abs(DM$EM-DM$EMR)/DM$EM
  Err=DM[DM$Err>1&!is.na(DM$Err)|(is.na(DM$EM)|is.na(DM$EMR)),]
  ErrT=rbind(ErrT,Err)
  g1A1c=ggplot(DM,aes(C2,Err))+geom_boxplot(aes(fill=GAS))+theme_bw()+ylab("Error [%]")+xlab("Clasificación 1")+ggtitle(cat)+facet_wrap(~C1)+
    theme(axis.text.x = element_text(angle = 90))
  
  
  #1A3e

  cat="1A3e"
  TMP1=DataR$Sucategoria.1
  TMP1=substr(TMP1,1,4)
  TMP1=gsub(" ","",TMP1)
  TMP2=DataR2$NCAT.IPCC 
  TMP2=substr(TMP2,1,4)
  TMP2=gsub(" ","",TMP2)
  Data.tmp1=DataR[TMP1==cat,]
  Data.tmp2=DataR2[TMP2==cat,]
  Data.tmp1=Data.tmp1[!is.na(Data.tmp1$Año),]
  Data.tmp2=Data.tmp2[!is.na(Data.tmp2$ANO),]
  Data.tmp1$C1="Transporte por gasoducto"
  Data.tmp1$C2=Data.tmp1$Combustible.BECO
  Data.tmp1$ANO=Data.tmp1$Año
  D1=Data.tmp1[c("ANO","C1","C2","Emisiones.CO2.(Gg.CO2)","Emision.CH4.(Gg.CH4)","Emisiones.N2O.(Gg.N2O)")]
  names(D1)[4:6]=c("CO2","CH4","N2O")
  D1=melt(D1,id.vars = c("ANO","C1","C2"))
  names(D1)[4:5]=c("GAS","EMR")
  D2=Data.tmp2[c("ANO","NCAT.IPCC","SELECT","C1","C2","GAS","EM")]
  DM=merge(D2,D1,by=c("ANO","C1","C2","GAS"),all=T)
  DM$Err=100*abs(DM$EM-DM$EMR)/DM$EM
  Err=DM[DM$Err>1&!is.na(DM$Err)|(is.na(DM$EM)|is.na(DM$EMR)),]
  ErrT=rbind(ErrT,Err[names(ErrT)])
  g1A3e=ggplot(DM,aes(C2,Err))+geom_boxplot(aes(fill=GAS))+theme_bw()+ylab("Error [%]")+xlab("Clasificación 1")+ggtitle(cat)+facet_wrap(~C1)+
    theme(axis.text.x = element_text(angle = 90))
  
  ## 1B1a
  
  
  cat="1B1a"
  TMP1=DataR$Sucategoria.1
  TMP1=substr(TMP1,1,4)
  TMP1=gsub(" ","",TMP1)
  TMP2=DataR2$NCAT.IPCC 
  TMP2=substr(TMP2,1,4)
  TMP2=gsub(" ","",TMP2)
  Data.tmp1=DataR[TMP1==cat,]
  Data.tmp2=DataR2[TMP2==cat,]
  Data.tmp1=Data.tmp1[!is.na(Data.tmp1$Año),]
  Data.tmp2=Data.tmp2[!is.na(Data.tmp2$ANO),]
  Data.tmp2$C2=c("Mineria","Posterior a la mineria")
  Data.tmp1$C1="Minas de superficie"
  Data.tmp1$C1[Data.tmp1$Sucategoria.2=="1B1ai  Minas subterráneas"]="Minas subterráneas"
  Data.tmp1$C2=Data.tmp1$Sucategoria.3
  Data.tmp1$ANO=Data.tmp1$Año
  Data.tmp1$SELECT=Data.tmp1$Descripción
  D1=Data.tmp1[c("ANO","C1","C2","Emisiones.CO2.(Gg.CO2)","Emision.CH4.(Gg.CH4)","Emisiones.N2O.(Gg.N2O)","SELECT")]
  names(D1)[4:6]=c("CO2","CH4","N2O")
  D1=melt(D1,id.vars = c("ANO","C1","C2","SELECT"))
  names(D1)[5:6]=c("GAS","EMR")
  D2=Data.tmp2[c("ANO","NCAT.IPCC","SELECT","C1","C2","GAS","EM")]
  DM=merge(D2,D1,by=c("ANO","C1","C2","GAS","SELECT"),all=T)
  DM$Err=100*abs(DM$EM-DM$EMR)/DM$EM
  Err=DM[DM$Err>1&!is.na(DM$Err)|(is.na(DM$EM)|is.na(DM$EMR)),]
  ErrT=rbind(ErrT,Err[names(ErrT)])
  g1B1a=ggplot(DM,aes(C2,Err))+geom_boxplot(aes(fill=GAS))+theme_bw()+ylab("Error [%]")+xlab("Clasificación 1")+ggtitle(cat)+facet_wrap(~C1)+
    theme(axis.text.x = element_text(angle = 90))
  
  ### 1B2a
  
  cat="1B2a"
  TMP1=DataR$Sucategoria.1
  TMP1=substr(TMP1,1,4)
  TMP1=gsub(" ","",TMP1)
  TMP2=DataR2$NCAT.IPCC 
  TMP2=substr(TMP2,1,4)
  TMP2=gsub(" ","",TMP2)
  Data.tmp1=DataR[TMP1==cat,]
  Data.tmp2=DataR2[TMP2==cat,]
  Data.tmp1=Data.tmp1[!is.na(Data.tmp1$Año),]
  Data.tmp2=Data.tmp2[!is.na(Data.tmp2$ANO),]
  Data.tmp1$C1="Todas las Actividades"
  Data.tmp1$C2=Data.tmp1$Sucategoria.2
  Data.tmp1$C2[Data.tmp1$Sucategoria.2=="1B2ai  Venteo"]="Venteo"
  Data.tmp1$C2[Data.tmp1$Sucategoria.2=="1B2aii  Quema en antorcha"]="Antorchas"
  Data.tmp1$C2[Data.tmp1$Sucategoria.2=="1B2aiii  Todos los demás "]="Otros"
  Data.tmp1$ANO=Data.tmp1$Año
  Data.tmp2$C1="Todas las Actividades"
  D1=Data.tmp1[c("ANO","C1","C2","Emisiones.CO2.(Gg.CO2)","Emision.CH4.(Gg.CH4)","Emisiones.N2O.(Gg.N2O)")]
  names(D1)[4:6]=c("CO2","CH4","N2O")
  D1=aggregate(D1[c("CO2","CH4","N2O")],D1[c(c("ANO","C1","C2"))],sum,na.rm=T)
  
  D1=melt(D1,id.vars = c("ANO","C1","C2"))
  names(D1)[4:5]=c("GAS","EMR")
  D2=Data.tmp2[c("ANO","NCAT.IPCC","SELECT","C1","C2","GAS","EM")]
  D2=aggregate(D2[c("EM")],D2[c(c("ANO","NCAT.IPCC","SELECT","C1","C2","GAS"))],sum,na.rm=T)
  DM=merge(D2,D1,by=c("ANO","C1","C2","GAS"),all=T)
  DM$Err=100*abs(DM$EM-DM$EMR)/DM$EM
  Err=DM[DM$Err>1&!is.na(DM$Err)|(is.na(DM$EM)|is.na(DM$EMR)),]
  ErrT=rbind(ErrT,Err[names(ErrT)])
  g1B2a=ggplot(DM,aes(C2,Err))+geom_boxplot(aes(fill=GAS))+theme_bw()+ylab("Error [%]")+xlab("Clasificación 1")+ggtitle(cat)+facet_wrap(~C1)+
    theme(axis.text.x = element_text(angle = 90))
  
  
  ### 1B2b
  
  cat="1B2b"
  TMP1=DataR$Sucategoria.1
  TMP1=substr(TMP1,1,4)
  TMP1=gsub(" ","",TMP1)
  TMP2=DataR2$NCAT.IPCC 
  TMP2=substr(TMP2,1,4)
  TMP2=gsub(" ","",TMP2)
  Data.tmp1=DataR[TMP1==cat,]
  Data.tmp2=DataR2[TMP2==cat,]
  Data.tmp1=Data.tmp1[!is.na(Data.tmp1$Año),]
  Data.tmp2=Data.tmp2[!is.na(Data.tmp2$ANO),]
  Data.tmp1$C1="Todas las Actividades"
  Data.tmp1$C2=Data.tmp1$Sucategoria.2
  Data.tmp1$C2[Data.tmp1$Sucategoria.2=="1B2bi  Venteo"]="Venteo"
  Data.tmp1$C2[Data.tmp1$Sucategoria.2=="1B2bii  Quema en antorcha"]="Antorchas"
  Data.tmp1$C2[Data.tmp1$Sucategoria.2=="1B2biii  Todos los demás "]="Otros"
  Data.tmp1$ANO=Data.tmp1$Año
  Data.tmp2$C1="Todas las Actividades"
  D1=Data.tmp1[c("ANO","C1","C2","Emisiones.CO2.(Gg.CO2)","Emision.CH4.(Gg.CH4)","Emisiones.N2O.(Gg.N2O)")]
  names(D1)[4:6]=c("CO2","CH4","N2O")
  D1=aggregate(D1[c("CO2","CH4","N2O")],D1[c(c("ANO","C1","C2"))],sum,na.rm=T)
  
  D1=melt(D1,id.vars = c("ANO","C1","C2"))
  names(D1)[4:5]=c("GAS","EMR")
  D2=Data.tmp2[c("ANO","NCAT.IPCC","SELECT","C1","C2","GAS","EM")]
  D2=aggregate(D2[c("EM")],D2[c(c("ANO","NCAT.IPCC","SELECT","C1","C2","GAS"))],sum,na.rm=T)
  DM=merge(D2,D1,by=c("ANO","C1","C2","GAS"),all=T)
  DM$Err=100*abs(DM$EM-DM$EMR)/DM$EM
  Err=DM[DM$Err>1&!is.na(DM$Err)|(is.na(DM$EM)|is.na(DM$EMR)),]
  ErrT=rbind(ErrT,Err[names(ErrT)])
  g1B2b=ggplot(DM,aes(C2,Err))+geom_boxplot(aes(fill=GAS))+theme_bw()+ylab("Error [%]")+xlab("Clasificación 1")+ggtitle(cat)+facet_wrap(~C1)+
    theme(axis.text.x = element_text(angle = 90))
  
  
  ### 1B2b
  
  cat="2B8b"
  TMP1=DataR$Sucategoria.1
  TMP1=substr(TMP1,1,4)
  TMP1=gsub(" ","",TMP1)
  TMP2=DataR2$NCAT.IPCC 
  TMP2=substr(TMP2,1,4)
  TMP2=gsub(" ","",TMP2)
  Data.tmp1=DataR[TMP1==cat,]
  Data.tmp2=DataR2[TMP2==cat,]
  Data.tmp1=Data.tmp1[!is.na(Data.tmp1$Año),]
  Data.tmp2=Data.tmp2[!is.na(Data.tmp2$ANO),]
  Data.tmp1$ANO=Data.tmp1$Año
  D1=Data.tmp1[c("ANO","Emisiones.CO2.(Gg.CO2)","Emision.CH4.(Gg.CH4)","Emisiones.N2O.(Gg.N2O)")]
  names(D1)[2:4]=c("CO2","CH4","N2O")

  D1=melt(D1,id.vars = c("ANO"))
  names(D1)[2:3]=c("GAS","EMR")
  D2=Data.tmp2[c("ANO","NCAT.IPCC","SELECT","C1","C2","GAS","EM")]
  DM=merge(D2,D1,by=c("ANO","GAS"),all=T)
  DM$Err=100*abs(DM$EM-DM$EMR)/DM$EM
  Err=DM[DM$Err>1&!is.na(DM$Err)|(is.na(DM$EM)|is.na(DM$EMR)),]
  ErrT=rbind(ErrT,Err[names(ErrT)])
  g2B8b=ggplot(DM,aes(C2,Err))+geom_boxplot(aes(fill=GAS))+theme_bw()+ylab("Error [%]")+xlab("Clasificación 1")+ggtitle(cat)+facet_wrap(~C1)+
    theme(axis.text.x = element_text(angle = 90))
  
  
  ### 1B2b
  
  cat="3B4a"
  TMP1=DataR$Sucategoria.1
  TMP1=substr(TMP1,1,4)
  TMP1=gsub(" ","",TMP1)
  TMP2=DataR2$NCAT.IPCC 
  TMP2=substr(TMP2,1,4)
  TMP2=gsub(" ","",TMP2)
  Data.tmp1=DataR[TMP1==cat,]
  Data.tmp2=DataR2[TMP2==cat,]
  Data.tmp1=Data.tmp1[!is.na(Data.tmp1$Año),]
  Data.tmp2=Data.tmp2[!is.na(Data.tmp2$ANO),]
  Data.tmp1$ANO=Data.tmp1$Año
  D1=Data.tmp1[c("ANO","Emisiones.CO2.(Gg.CO2)","Emision.CH4.(Gg.CH4)","Emisiones.N2O.(Gg.N2O)")]
  names(D1)[2:4]=c("CO2","CH4","N2O")
  
  D1=melt(D1,id.vars = c("ANO"))
  names(D1)[2:3]=c("GAS","EMR")
  D2=Data.tmp2[c("ANO","NCAT.IPCC","SELECT","C1","C2","GAS","EM")]
  D2=aggregate(D2[c("EM")],D2[c(c("ANO","NCAT.IPCC","GAS"))],sum,na.rm=T)
  
  DM=merge(D2,D1,by=c("ANO","GAS"),all=T)
  DM$Err=100*abs(DM$EM-DM$EMR)/DM$EM
  DM$SELECT=NA
  DM$C1=NA
  DM$C2=NA
  Err=DM[DM$Err>1&!is.na(DM$Err)|(is.na(DM$EM)|is.na(DM$EMR)),]

  ErrT=rbind(ErrT,Err[names(ErrT)])
  g3B4a=ggplot(DM,aes(C2,Err))+geom_boxplot(aes(fill=GAS))+theme_bw()+ylab("Error [%]")+xlab("Clasificación 1")+ggtitle(cat)+facet_wrap(~C1)+
    theme(axis.text.x = element_text(angle = 90))
  
  ErrT=ErrT[!(is.na(ErrT$EM)&is.na(ErrT$EMR)),]
  
  return(ErrT)
  
  
}