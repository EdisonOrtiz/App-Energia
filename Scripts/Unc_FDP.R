Unc_FDP<-function(PFD,par_1,par_2,par_3,u_type='relative')
{
  print("Determinando incertidumbres segun PDF")
  
  pb=txtProgressBar(0,length(PFD),style = 3)
    for (i in 1:length(PFD))
    {
    unc= switch(as.character(PFD[i]),
         'NORMAL' = qnorm(c(0.025,0.5,0.975),par_1[i],par_2[i]),
         'LOGNORMAL' = qlnorm(c(0.025,0.5,0.975),par_1[i],par_2[i]),
         'WEIBULL' = qweibull(c(0.025,0.5,0.975),par_1[i],par_2[i]),
         'GAMMA' = qgamma(c(0.025,0.5,0.975),par_1[i],par_2[i]),
         'TRIANGULAR' = qtriangle(c(0.025,0.5,0.975),c=par_1[i],a=par_2[i],b=par_3[i]),
         'CONSTANTE'=c(1,1,1)
  )
  if (u_type=='relative')
  {
  Un<-(unc[2]-unc[1])/(abs(unc[2]))
  Up<-(unc[3]-unc[2])/(abs(unc[2]))
  }
  if (u_type=='absolute')
  {
  Un<-(unc[2]-unc[1])
  Up<-(unc[3]-unc[2])
  }
    RESi=data.frame(Un,Up)
    if (i==1)
    {RES=RESi}else{RES=rbind(RES,RESi)}
    setTxtProgressBar(pb,i)
    }

  return(RES)

  
}