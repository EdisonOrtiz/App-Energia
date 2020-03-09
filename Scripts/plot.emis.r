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
                  addBubble=TRUE,
                  palette="Spectral", ## paleta de colores
                  txt.size=20,
                  mn.tck=0
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
if (is.null(checkSectors))
{DAT1=NULL}else
{
for (sect in checkSectors)
{
  tmp=DAT[DAT$Sector==sect,]
  DAT1=rbind(DAT1,tmp)
}
}

DAT2=NULL
if (is.null(checkipcc))
{DAT2=NULL} else
{
for (ipcc in checkipcc)
{
  tmp=DAT1[DAT1$NCAT.IPCC==ipcc,]
  DAT2=rbind(DAT2,tmp)
}
}

if(is.null(nrow(DAT2)))
{plot=NA} 
else if ((nrow(DAT2)==0))
{plot=NULL
} else
{
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
    data.plot=Agg$MC
    if (showT==TRUE)
    {
      Tot=SUMCALC(Dat1,agg=NULL,it=it)
      TotPE=Tot$MC
      TotPE$Sector="Total de Sectores Seleccionados"
      data.plot=rbind(data.plot,TotPE[names(data.plot)])
    }
    data.plot$SN=data.plot$EM*(1-data.plot$UN)
    data.plot$SP=data.plot$EM*(1+data.plot$UP)
    data.plot$UM=100*(data.plot$UN+data.plot$UN)/2
    data.plot$brksz=0
    data.plot$brksz[data.plot$UM<5]<-"<5%"
    data.plot$brksz[data.plot$UM>=5&data.plot$UM<10]<-"5%-10%"
    data.plot$brksz[data.plot$UM>=10&data.plot$UM<25]<-"10%-25%"
    data.plot$brksz[data.plot$UM>=25&data.plot$UM<50]<-"25%-50%"
    data.plot$brksz[data.plot$UM>=50&data.plot$UM<75]<-"50%-75%"
    data.plot$brksz[data.plot$UM>=75&data.plot$UM<100]<-"75%-100%"
    data.plot$brksz[data.plot$UM>100]<-">100%"
    data.plot$brksz=factor(data.plot$brksz,levels = c("<5%","5%-10%","10%-25%","25%-50%","50%-75%","75%-100%",">100%"))
    
    
    mp=unique(data.plot$brksz)
    
    cbr=switch (length(mp),
                "1" = 10,
                "2" = 8,
                "3" = 5,
                "4" = 3,
                "5" = 2,
                "6" = 1,
                "7" = 1
    )
    
    
    plot=ggplot(data.plot,aes(ANO,EM))+geom_line(aes(color=Sector),size=1)+theme_bw()+
      scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))+
    if (addU==TRUE)
    {
      plot=plot+geom_errorbar(aes(x=ANO,ymin=SN, ymax=SP,color=Sector), width=.2)
    }
    if (addBubble==TRUE)
    {
      plot=plot+geom_point(aes(color=Sector,size=brksz),alpha=0.3)+labs(colour="Sector",size="Incertidumbre [%]")+
        scale_size_manual(values=c(1:7)*cbr)+
        guides(size=guide_legend(ncol=2))
    }
    
  }
  
  if (SelectGraph=="Ts IPCC")
  {
    {
      Agg=SUMCALC(Dat1,agg=c("NCAT.IPCC"),it=it)
      data.plot=Agg$MC
      if (showT==TRUE)
      {
        Tot=SUMCALC(Dat1,agg=NULL,it=it)
        TotPE=Tot$MC
        TotPE$NCAT.IPCC="Total de Cat. IPCC Seleccionados"
        data.plot=rbind(data.plot,TotPE[names(data.plot)])
      }
      data.plot$SN=data.plot$EM*(1-data.plot$UN)
      data.plot$SP=data.plot$EM*(1+data.plot$UP)
      data.plot$UM=100*(data.plot$UN+data.plot$UN)/2
      data.plot$brksz=0
      data.plot$brksz[data.plot$UM<5]<-"<5%"
      data.plot$brksz[data.plot$UM>=5&data.plot$UM<10]<-"5%-10%"
      data.plot$brksz[data.plot$UM>=10&data.plot$UM<25]<-"10%-25%"
      data.plot$brksz[data.plot$UM>=25&data.plot$UM<50]<-"25%-50%"
      data.plot$brksz[data.plot$UM>=50&data.plot$UM<75]<-"50%-75%"
      data.plot$brksz[data.plot$UM>=75&data.plot$UM<100]<-"75%-100%"
      data.plot$brksz[data.plot$UM>100]<-">100%"
      data.plot$brksz=factor(data.plot$brksz,levels = c("<5%","5%-10%","10%-25%","25%-50%","50%-75%","75%-100%",">100%"))
      
      
      mp=unique(data.plot$brksz)
      
      cbr=switch (length(mp),
                  "1" = 10,
                  "2" = 8,
                  "3" = 5,
                  "4" = 3,
                  "5" = 2,
                  "6" = 1,
                  "7" = 1
      )
      
      
      plot=ggplot(data.plot,aes(ANO,EM))+geom_line(aes(color=NCAT.IPCC),size=1)+theme_bw()+
        scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))+
        scale_color_discrete("Categoría IPCC")
      if (addU==TRUE)
      {
        plot=plot+geom_errorbar(aes(x=ANO,ymin=SN, ymax=SP,color=NCAT.IPCC), width=.2) 
      }
      if (addBubble==TRUE)
      {
        plot=plot+geom_point(aes(color=NCAT.IPCC,size=brksz),alpha=0.3)+labs(colour="Categoría IPCC",size="Incertidumbre [%]")+
          scale_size_manual(values=c(1:7)*cbr)+
          guides(size=guide_legend(ncol=2))
      }
  
    }
  }  
  
  if (SelectGraph=="bar IPCC")
  {
    {
      Agg=SUMCALC(Dat1,agg=c("NCAT.IPCC"),it=it)
      data.plot=Agg$MC
      plot=ggplot(data.plot,aes(ANO,EM))+geom_bar(aes(fill=NCAT.IPCC),stat="identity")+theme_bw()+
        scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))+scale_fill_brewer("Categoría IPCC",palette = "YlOrRd")
    }
  }
  
  if (SelectGraph=="bar Sector")
  {
    Agg=SUMCALC(Dat1,agg=c("Sector"),it=it)
    data.plot=Agg$MC
  
    plot=ggplot(data.plot,aes(ANO,EM))+geom_bar(aes(fill=Sector),stat="identity")+theme_bw()+
      scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))+scale_fill_brewer("Sector",palette = "Spectral")
  }
  
  if (SelectGraph=="bar all")
  {
    Agg=SUMCALC(Dat1,agg=c("Sector","NCAT.IPCC"),it=it)
    data.plot=Agg$MC
    
    plot=ggplot(data.plot,aes(ANO,EM))+geom_bar(aes(fill=NCAT.IPCC),stat="identity")+theme_bw()+facet_wrap(~Sector,scales = "free_y")+
      scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))+
      labs(fill="Categoría IPCC")

  }
  
  if (SelectGraph=="Ts all")
  {
    Agg=SUMCALC(Dat1,agg=c("Sector","NCAT.IPCC"),it=it)
    data.plot=Agg$MC
    data.plot$SN=data.plot$EM*(1-data.plot$UN)
    data.plot$SP=data.plot$EM*(1+data.plot$UP)
    data.plot$UM=100*(data.plot$UN+data.plot$UN)/2
    data.plot$brksz=0
    data.plot$brksz[data.plot$UM<5]<-"<5%"
    data.plot$brksz[data.plot$UM>=5&data.plot$UM<10]<-"5%-10%"
    data.plot$brksz[data.plot$UM>=10&data.plot$UM<25]<-"10%-25%"
    data.plot$brksz[data.plot$UM>=25&data.plot$UM<50]<-"25%-50%"
    data.plot$brksz[data.plot$UM>=50&data.plot$UM<75]<-"50%-75%"
    data.plot$brksz[data.plot$UM>=75&data.plot$UM<100]<-"75%-100%"
    data.plot$brksz[data.plot$UM>100]<-">100%"
    data.plot$brksz=factor(data.plot$brksz,levels = c("<5%","5%-10%","10%-25%","25%-50%","50%-75%","75%-100%",">100%"))
    
    
    mp=unique(data.plot$brksz)
    
    cbr=switch (length(mp),
                "1" = 10,
                "2" = 8,
                "3" = 5,
                "4" = 3,
                "5" = 2,
                "6" = 1,
                "7" = 1
    )
    
    
    
    plot=ggplot(data.plot,aes(ANO,EM))+geom_line(aes(col=NCAT.IPCC),size=1)+theme_bw()+facet_wrap(~Sector,scales = "free_y")+
      scale_x_continuous("Año",breaks = year[1]:year[2])+ylab(expression("Mt"*~~CO[2]*"eq"))+
      scale_color_discrete("Categoría IPCC")
    if (addU==TRUE)
    {
      plot=plot+geom_errorbar(aes(x=ANO,ymin=SN, ymax=SP,color=NCAT.IPCC), width=.2) 
    }
    if (addBubble==TRUE)
    {
      plot=plot+geom_point(aes(color=NCAT.IPCC,size=brksz),alpha=0.3)+labs(colour="Categoría IPCC",size="Incertidumbre [%]")+
        scale_size_manual(values=c(1:7)*cbr)+
        guides(size=guide_legend(ncol=2))
    }
  }


    if (SelectGraph=="Sankey Diagram")
    {
      Agg=SUMCALC(Dat1,agg=c("Sector","NCAT.IPCC"),it=it)
      data.plot=Agg$MC
      
      data.plot2=aggregate(data.plot["EM"],data.plot[c("NCAT.IPCC","Sector")],mean,na.rm=T)
      
      
      data.plot2$Sector[data.plot2$Sector=="Minería de Carbón y Producción de Coque"]<-"Carbón y Coque"
      data.plot2$Sector[data.plot2$Sector=="Sistema Interconectado Nacional"]<-"SIN"
      data.plot2$Sector[data.plot2$Sector=="Zonas No Interconectadas"]<-"ZNI"
      data.plot2$Sector=factor(data.plot2$Sector,levels=c("SIN","ZNI","Petróleo y Gas","Carbón y Coque"))
      data.plot2$NCAT.IPCC=substr(data.plot2$NCAT.IPCC, start = 1, stop = 4)
      data.plot2$NCAT.IPCC=gsub(" ","",data.plot2$NCAT.IPCC)
      

      
      plot=ggplot(data.plot2,
                  aes(y = EM, axis1 = Sector, axis2 = NCAT.IPCC)) +
        theme_bw()+
        geom_alluvium(aes(fill = Sector,color=Sector))+
        scale_fill_brewer(palette = "Spectral")+
        scale_color_brewer(palette = "Spectral")+
        geom_stratum(width = 1/12, fill = "black", color = "grey")+
        geom_label(stat = "stratum", infer.label = TRUE)+
        scale_x_discrete(limits = c("Sector", "Categoría IPCC"), expand = c(.05, .05))+
        theme(legend.position = "none")+
        ylab(expression("Mt"*~~CO[2]*"eq"))
      

    }
  
  if (SelectGraph!="Sankey Diagram")
  {
  plot=plot+theme(legend.position = "bottom",axis.text.x = element_text(angle = 90),text = element_text(size=txt.size))+
    guides(col = guide_legend(ncol = 1, byrow = FALSE,title.position = "top"),fill = guide_legend(ncol = 2, byrow = FALSE,title.position = "top"),
                                                     size = guide_legend(ncol = 2, byrow = F,title.position = "top"))+
    expand_limits(y = 0)
  } else
  {
    plot=plot+theme(legend.position = "none",text = element_text(size=txt.size))
  }
### ajustes finales

   
   plot=plot+scale_color_brewer(palette = palette)+
     scale_fill_brewer(palette = palette)
   
   if (mn.tck==0)
   {plot=plot+scale_y_continuous(breaks=waiver())}else if (mn.tck>0)
   {plot=plot+scale_y_continuous(breaks=seq(0,1e5,mn.tck))}
     
     

}
return(plot)
}