FIT_PDF<-function(x,PDF_def="None",method="mme",dir.s=dir.s)
{
  require(fitdistrplus)
  x=x[x!=0]
  if (PDF_def == "None")
  {
  if (sd(x)!=0&!is.na(sd(x)))
  {
  ### Fitting Normal
  N1<-try(N<-fitdist(x,'norm',method=method),silent=T)
  if("try-error" %in% class(N1))
  {N=NULL;rv1=1}else{N<-fitdist(x,'norm',method=method);rv1=0}
  ### Fitting LogNormal
  N1<-try(fitdist(x,'lnorm',method=method),silent=T)
  if("try-error" %in% class(N1))
  {L=NULL;rv2=2}else{L<-fitdist(x,'lnorm',method=method);rv2=0}
  ### Fitting Weibull
  N1<-try(fitdist(x,'weibull',method=method),silent=T)
  if("try-error" %in% class(N1))
  {W=NULL;rv3=3}else{W<-fitdist(x,'weibull',method=method);rv3=0}
  ### Fitting Gamma
  N1<-try(fitdist(x,'gamma',method=method),silent=T)
  if("try-error" %in% class(N1))
  {G=NULL;rv4=4}else{G<-fitdist(x,'gamma',method=method);rv4=0}
  ### Matrix of good-of-fitness
  rv=c(rv1,rv2,rv3,rv4)
  rv=rv[rv!=0]
  LS<-list(N,L,W,G)
  if (length(rv)!=4)
  {
  if (length(rv)!=0)
  {LS=LS[-rv]}
  if (4-length(rv)==1)
  {GOF<-gofstat(LS[[1]])} else
  {GOF<-gofstat(LS)}
  
  ### Criterio 1, prueba Chi cuadrado
    F1=names(GOF$chisq[GOF$chisq==min(GOF$chisq)])
  ### Criterio 2, Test Kolmogorov-Smirnov
    F2=names(GOF$ks[GOF$ks==min(GOF$ks)])
  ### Criterio 3, Test Anderson-Darling
    F3=names(GOF$ad[GOF$ad==min(GOF$ad)])
  ### Criterio 4, Cramer-von Mises statistics
    F4=names(GOF$cvm[GOF$cvm==min(GOF$cvm)])
    F5=names(GOF$kstest[GOF$kstest=='not rejected'])
  ### Unificación de criterios
    FT<-c(F1,F2,F3,F4)
    FT=names(table(FT)[table(FT)==max(table(FT))])
    FT=unlist(strsplit(FT,split='-'))
    FT=FT[3]
    FT1=unlist(strsplit(F5,split='-'))
    if(!is.null(FT1))
    {
    FT1=data.frame(t(matrix(FT1,3,length(FT1)/3)))
    FT1=FT1[FT1[3]=='norm',]
    }
    if (length(F5)==0)
    {
      min=min(x,na.rm=TRUE)
      max=max(x,na.rm=TRUE)
      RES<-data.frame('TRIANGULAR',getmode(x),min,max,'not rejected')
    } else 
      {
    if (dim(FT1)[1]==0)
        {
      R<-switch(FT,
            'norm'= N,
            'lnorm'= L,
            'weibull'= W,
            'gamma'= G)  
  PFD<-switch(FT,
               'norm'= 'NORMAL',
               'lnorm'= 'LOGNORMAL',
               'weibull'= 'WEIBULL',
               'gamma'=  'GAMMA')
  }else
  {
    R<-N;PFD<-'NORMAL'
  }
  `Hipotesis Nula`=as.character(gofstat(R)$kstest)
  RES<-data.frame(PFD,t(coef(R)),0,`Hipotesis Nula`)
        
      }
  } else {
    min=min(x,na.rm=TRUE)
    max=max(x,na.rm=TRUE)
    RES<-data.frame('TRIANGULAR',getmode(x),min,max,'not rejected')
    }
  }else {RES<-data.frame('CONSTANTE',mean(x),0,0,'not rejected')}
  names(RES)<-c('PFD','par_1','par_2','par_3','Hipotesis de ajuste')
  }
  if (sd(x)!=0&!is.na(sd(x)))
  {
  if (PDF_def != "None"&PDF_def!="TRIANGULAR"&PDF_def!="CONSTANTE")
  {
  R = switch(PDF_def,
         "NORMAL"=fitdist(x,'norm',method=method),
         "GAMMA"=fitdist(x,'gamma',method=method),
         "LOGNORMAL"=fitdist(x,'lnorm',method=method),
         "WEIBULL"=fitdist(x,'weibull',method=method))
  `Hipotesis Nula`=as.character(gofstat(R)$kstest)
  RES<-data.frame(PDF_def,t(coef(R)),0,`Hipotesis Nula`)
  names(RES)<-c('PFD','par_1','par_2','par_3','Hipotesis de ajuste')
  }
  if (PDF_def=="TRIANGULAR"|PDF_def=="CONSTANTE")
  {
  min=min(x,na.rm=TRUE)
  max=max(x,na.rm=TRUE)
  RES<-data.frame(PDF_def,getmode(x),min,max,'Unknown')
  names(RES)<-c('PFD','par_1','par_2','par_3','Hipotesis de ajuste')
  }
  } else
  {
    RES<-data.frame("CONSTANTE",0,0,0,'Unknown')
    names(RES)<-c('PFD','par_1','par_2','par_3','Hipotesis de ajuste')
  }
    
  return(RES)
   
}