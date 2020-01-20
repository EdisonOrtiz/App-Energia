SUMCALC<-function(dat,agg=c("GAS","Sector","NCAT.IPCC"),
                  it=1000,method="mme",PDF_def="None")
{

   print("Compilando información...")
     dat$UN_EM[is.infinite(dat$UN_EM)]<-0
     dat$UP_EM[is.infinite(dat$UP_EM)]<-0
     dat$SN=dat$EM*dat$UN_EM;dat$SP=dat$EM*dat$UP_EM
dat1=dat
  
  DATG=aggregate(dat1["EM"],dat1[c("Method","ANO",agg)],sum,na.rm=T)
  
  print("Determinando incertidumbre por propagación de errores...")
  ### Propagaciòn de Errores
  SN=aggregate(dat1["SN"],dat1[c("Method","ANO",agg)],inc_sum)
  SP=aggregate(dat1["SP"],dat1[c("Method","ANO",agg)],inc_sum)
  datN=merge(DATG,SN,by=c("Method","ANO",agg))
  datN=merge(datN,SP,by=c("Method","ANO",agg))
  datN$UN=datN$SN/datN$EM;datN$UP=datN$SP/datN$EM
  datN=datN[datN$Method=="PROP",]
  
  nr=nrow(datN)
  RES=datN[c("Method","ANO",agg,"EM","UN","UP")]

  
  #write.csv(RES,paste0(filename,"_PROP.csv"),row.names=F)
  
  print("Determinando incertidumbre por MonteCarlo...")
  ## Montecarlo
  datM=dat1[dat1$Method=="MC",]
  pb=txtProgressBar(min=1,max=it,style=3)
  
  datE=DATG[DATG$Method=="MC",]
  
  for (k in 1:it)
  {  
  for (j in 1:nrow(datM))
  {

    Pij=switch(as.character(datM[j,"DIST_EM"]),
               "NORMAL"=rnorm(1,mean=datM[j,paste0("PAR1","_EM")],sd=datM[j,paste0("PAR2","_EM")]),
               "LOGNORMAL"=rlnorm(1,datM[j,paste0("PAR1","_EM")],datM[j,paste0("PAR2","_EM")]),
               "GAMMA"=rgamma(1,datM[j,paste0("PAR1","_EM")],datM[j,paste0("PAR2","_EM")]),
               "WEIBULL"=rweibull(1,datM[j,paste0("PAR1","_EM")],datM[j,paste0("PAR2","_EM")]),
               "TRIANGULAR"=rtriangle(1,c=datM[j,paste0("PAR1","_EM")],a=datM[j,paste0("PAR2","_EM")],b=datM[j,paste0("PAR3","_EM")]),
               "CONSTANTE"=array(datM[j,paste0("PAR1","_EM")],1)
    )
  
    if (j==1){P=Pij}else{P=c(P,Pij)}
  }
  DATGMCj=aggregate(P,datM[c("Method","ANO",agg)],sum,na.rm=T);names(DATGMCj)=c("Method","ANO",agg,k)
  
  if(k==1){DATMC=DATGMCj}else{DATMC=merge(DATMC,DATGMCj,by=c("Method","ANO",agg))}
  
    setTxtProgressBar(pb,k)
  }
  nm=which(c("Method","ANO",agg)==names(DATMC))
  
  DATMC2=DATMC[-nm]
  
  for (j in 1:nrow(DATMC2))
  {
    pi=FIT_PDF(DATMC2[j,],method=method,PDF_def = PDF_def)
    if (j==1){p=pi}else{p=rbind(p,pi)}
  }
  
  
  nr=nrow(datE)
  p1=cbind(DATMC[nm],p)
  p2=merge(p1,datE,by=nm,all=T)
  RES1=p2[c("Method","ANO",agg,"EM","PFD","par_1","par_2","par_3")]
  INC=Unc_FDP(RES1$PFD,RES1$par_1,RES1$par_2,RES1$par_3)
  RES1[c("UN","UP")]=INC
  #write.csv(RES1,paste0(filename,"_MC.csv"),row.names = F)
  
  RET=list(PE=RES,MC=RES1)
  return(RET)
}