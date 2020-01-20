plot.emis=function(DAT,
                  it=100,
                  dir.s="./Scripts",
                  checkSectors = c("Sistema Interconectado Nacional","Petróleo y Gas",
                                   "Minería de Carbón y Producción de Coque"),
                  checkipcc = c("1A1a  Producción de electricidad y calor como actividad principal ",
                                "1A1b  Refinación de petróleo",
                                "1A1c Fabricación de combustibles sólidos y otras industrias energéticas",
                                "1A3e Otro transporte",
                                "1B1a  Minería carbonífera y manejo del carbón",
                                "1B2a  Petróleo",
                                "1B2b  Gas Natural",
                                "2B8b Etileno",
                                "2B8c Dicloruro de etileno y monómero cloruro de vinilo",
                                "2C2 Producción de ferroaleaciones"
                  ),
                  year=c(2012,2018),
                  SelectGraph="Ts Sector", #Ts IPCC, Ts all, bar Sector, bar IPCC, bar all
                  showT=TRUE,
                  addU=TRUE,
                  addBubble=TRUE
                  )
{
### Opciones
  
  
  
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
  
DAT$NCAT.IPCC=as.character(DAT$NCAT.IPCC)
DAT$Sector=as.character(DAT$Sector)
DAT1=NULL
for (sect in checkSectors)
{
  tmp=DAT[DAT$Sector==sect,]
  DAT1=rbind(DAT1,tmp)
}
DAT2=NULL
for (ipcc in checkipcc)
{
  tmp=DAT1[DAT1$NCAT.IPCC==ipcc,]
  DAT2=rbind(DAT2,tmp)
}
Dat=DAT2[DAT2$ANO>=year[1]&DAT2$ANO<=year[2],]
it=100
GWP=read.csv(paste0(dir.s,"./GWP.csv"))

gwp=GWP[c(4,3)]

Dat1=merge(Dat,gwp,by.x="GAS",by.y="Conv.2",all.x=T)
Dat1=Dat1[-c(10:13)]
Dat1$EM=Dat1$EM*Dat1$GWP/1000 ### Paso a Mt
Dat1=SelDist(Dat1,"EM","UN_EM","UP_EM","_EM")


if (SelectGraph=="Ts Sector")
{
  Agg=SUMCALC(Dat1,agg=c("Sector"),it=it)
  data.plot=Agg$PE
  if (showT==TRUE)
  {
    Tot=SUMCALC(Dat1,agg=NULL,it=it)
    TotPE=Tot$PE
    TotPE$Sector="Total de Sectores Seleccionados"
    data.plot=rbind(data.plot,TotPE[names(data.plot)])
  }
  data.plot$SN=data.plot$EM*(1-data.plot$UN)
  data.plot$SP=data.plot$EM*(1+data.plot$UP)
  data.plot$UM=100*(data.plot$UN+data.plot$UN)/2
  plot=ggplot(data.plot,aes(ANO,EM))+geom_line(aes(color=Sector),size=1)+theme_bw()+
    scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))
  if (addU==TRUE)
  {
    plot=plot+geom_errorbar(aes(x=ANO,ymin=SN, ymax=SP,color=Sector), width=.2)
  }
  if (addBubble==TRUE)
  {
    plot=plot+geom_point(aes(color=Sector,size=UM))+labs(colour="Sector",size="Incertidumbre [%]")+
      scale_size_continuous(breaks=seq(10,100,10))
  }
}

if (SelectGraph=="Ts IPCC")
{
  {
    Agg=SUMCALC(Dat1,agg=c("NCAT.IPCC"),it=it)
    data.plot=Agg$PE
    if (showT==TRUE)
    {
      Tot=SUMCALC(Dat1,agg=NULL,it=it)
      TotPE=Tot$PE
      TotPE$NCAT.IPCC="Total de Cat. IPCC Seleccionados"
      data.plot=rbind(data.plot,TotPE[names(data.plot)])
    }
    data.plot$SN=data.plot$EM*(1-data.plot$UN)
    data.plot$SP=data.plot$EM*(1+data.plot$UP)
    data.plot$UM=100*(data.plot$UN+data.plot$UN)/2
    plot=ggplot(data.plot,aes(ANO,EM))+geom_line(aes(color=NCAT.IPCC),size=1)+theme_bw()+
      scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))+
      scale_color_discrete("Categoría IPCC")
    if (addU==TRUE)
    {
      plot=plot+geom_errorbar(aes(x=ANO,ymin=SN, ymax=SP,color=NCAT.IPCC), width=.2) 
    }
    if (addBubble==TRUE)
    {
      plot=plot+geom_point(aes(color=NCAT.IPCC,size=UM))+labs(colour="Categoría IPCC",size="Incertidumbre [%]")+
        scale_size_continuous(breaks=seq(10,100,10))
    }

  }
}  

if (SelectGraph=="bar IPCC")
{
  {
    Agg=SUMCALC(Dat1,agg=c("NCAT.IPCC"),it=it)
    data.plot=Agg$PE
    plot=ggplot(data.plot,aes(ANO,EM))+geom_bar(aes(fill=NCAT.IPCC),stat="identity")+theme_bw()+
      scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))+scale_fill_brewer(palette = "YlOrRd")
  }
}

if (SelectGraph=="bar Sector")
{
  Agg=SUMCALC(Dat1,agg=c("Sector"),it=it)
  data.plot=Agg$PE

  plot=ggplot(data.plot,aes(ANO,EM))+geom_bar(aes(fill=Sector),stat="identity")+theme_bw()+
    scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))+scale_fill_brewer(palette = "YlOrRd")
}

if (SelectGraph=="bar all")
{
  Agg=SUMCALC(Dat1,agg=c("Sector","NCAT.IPCC"),it=it)
  data.plot=Agg$PE
  
  plot=ggplot(data.plot,aes(ANO,EM))+geom_bar(aes(fill=NCAT.IPCC),stat="identity")+theme_bw()+facet_wrap(~Sector,scales = "free_y")+
    scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))+scale_fill_brewer(palette = "YlOrRd")+
    scale_color_discrete("Categoría IPCC")
}

if (SelectGraph=="Ts all")
{
  Agg=SUMCALC(Dat1,agg=c("Sector","NCAT.IPCC"),it=it)
  data.plot=Agg$PE
  data.plot$SN=data.plot$EM*(1-data.plot$UN)
  data.plot$SP=data.plot$EM*(1+data.plot$UP)
  data.plot$UM=100*(data.plot$UN+data.plot$UN)/2
  plot=ggplot(data.plot,aes(ANO,EM))+geom_line(aes(col=NCAT.IPCC),size=1)+theme_bw()+facet_wrap(~Sector,scales = "free_y")+
    scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))+
    scale_color_discrete("Categoría IPCC")
  if (addU==TRUE)
  {
    plot=plot+geom_errorbar(aes(x=ANO,ymin=SN, ymax=SP,color=NCAT.IPCC), width=.2) 
  }
  if (addBubble==TRUE)
  {
    plot=plot+geom_point(aes(color=NCAT.IPCC,size=UM))+labs(colour="Categoría IPCC",size="Incertidumbre [%]")+
      scale_size_continuous(breaks=seq(10,100,10))
  }
}


### Grafico de Burbujas


plot=plot+theme(legend.position = "bottom",axis.text.x = element_text(angle = 90),text = element_text(size=20))+
  guides(col = guide_legend(ncol = 1, byrow = TRUE),fill = guide_legend(ncol = 1, byrow = TRUE),
                                                   size = guide_legend(ncol = 1, byrow = TRUE))+
  expand_limits(y = 0)
return(plot)
}