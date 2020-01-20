#### Estimación de emisiones con Propagación de errores


EMCALC=function(DAT,act="VALOR",fe="VALOR_FE",fc=1e-6,f1=0,f2=0,f3=0,f4=0,f5=0,eq="em=act*fe",suffix=c("_ACT","_FE"),UNC_type="PROP",it=10000,method="mme",PDF_def="None",
                UN="UN",UP="UP",k1=1,k2=1000,k3=12)
{

### Cálculo de emisiones (OJO CORREGIR EN AFOLU 1)
  
EM=switch (eq,
           "em=act*fe" = DAT[act]*DAT[fe]*fc,
           "em=act+k1*fe"=(DAT[act]+k1*DAT[fe])*fc,
           "em=act*fe*f1"=DAT[act]*DAT[fe]*DAT[f1]*fc,
           "em=act*fe*(k1*f1+k2*f2)"=DAT[act]*DAT[fe]*(k1*DAT[f1]+k2*DAT[f2])*fc,
           "em=act*fe+f1"=(DAT[act]*DAT[fe]+DAT[f1])*fc,
           "em=act*(k1-fe)+f1"=(DAT[act]*(k1-DAT[fe])+DAT[f1])*fc,
           "em=act*fe*f1*f2"=DAT[act]*DAT[fe]*DAT[f1]*DAT[f2]*fc,
           "em=act*fe*f1*f2*f3"= DAT[act]*DAT[fe]*DAT[f1]*DAT[f2]*DAT[f3]*fc,
           "em=act*fe*f1*f2-f3"=(DAT[act]*DAT[fe]*DAT[f1]*DAT[f2]-DAT[f3])*fc,
           "em=act*fe*f1*f2*f3*f4"= DAT[act]*DAT[fe]*DAT[f1]*DAT[f2]*DAT[f3]*DAT[f4]*fc,
           "em=act*(k1-fe)"= DAT[act]*(k1-DAT[fe])*fc,
           "em=act*fe*(k1-f1)"= DAT[act]*DAT[fe]*(k1-DAT[f1])*fc,
           "em=act*fe*f1*f2*(k1-f3)"= DAT[act]*DAT[fe]*DAT[f1]*DAT[f2]*(k1-DAT[f3])*fc,
           "em=act*fe*f1*f2*(k1-f3)*f4"= DAT[act]*DAT[fe]*DAT[f1]*DAT[f2]*(k1-DAT[f3])*DAT[f4]*fc,
           'em=act*fe*f1*((f2*f3*(f4+k1)+f5*k2))'=DAT[act]*DAT[fe]*DAT[f1]*((DAT[f2]*DAT[f3]*(DAT[f4]+k1)+DAT[f5]*k2))*fc,
           'em=exp(k1*act)'=exp(k1*DAT[act]),
           'em=exp(k1*act*(k2-fe)/k3)'=exp(k1*DAT[act]*(k2-DAT[fe])/k3)
           
)

names(EM)="EM"
EM1=EM
EM=EM1/fc
### Incertidumbre por propagación de errores
if (UNC_type=="PROP")
{
  print(" -- Calculando Incertidumbre por Propagación de Errores")
UN_EM=switch (eq,
              "em=act*fe" = sqrt((DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2),
              "em=act+k1*fe"=sqrt(((DAT[act]/EM)^2)*(DAT[paste0(UN,suffix[1])]/2)^2+((k1*DAT[fe]/EM)^2)*(DAT[paste0(UN,suffix[2])]/2)^2),
              "em=act*fe*f1"= sqrt((DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2+(DAT[paste0(UN,suffix[3])]/2)^2),
              "em=act*fe*(k1*f1+k2*f2)"=sqrt((DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2+(k1*DAT[f1]*DAT[paste0(UN,suffix[3])]/(2*(DAT[f1]+DAT[f2])))^2+(k2*DAT[f2]*DAT[paste0(UN,suffix[4])]/(2*(DAT[f1]+DAT[f2])))^2),
              "em=act*fe+f1"=sqrt(((DAT[fe]*DAT[act]/EM)^2)*((DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2)+((DAT[f1]/EM)^2)*((DAT[paste0(UN,suffix[3])]/2)^2)),
              "em=act*(k1-fe)+f1"=sqrt((((k1-DAT[fe])*DAT[act]/EM)^2)*((DAT[paste0(UN,suffix[1])]/2)^2+((DAT[fe]/(k1-DAT[fe]))^2)*(DAT[paste0(UN,suffix[2])]/2)^2)+((DAT[f1]/EM)^2)*((DAT[paste0(UN,suffix[3])]/2)^2)),
              "em=act*fe*f1*f2"= sqrt((DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2+(DAT[paste0(UN,suffix[3])]/2)^2+(DAT[paste0(UN,suffix[4])]/2)^2),
              "em=act*fe*f1*f2*f3"= sqrt((DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2+(DAT[paste0(UN,suffix[3])]/2)^2+(DAT[paste0(UN,suffix[4])]/2)^2+(DAT[paste0(UN,suffix[5])]/2)^2),
              "em=act*fe*f1*f2-f3"= sqrt(((1-DAT[f3]/EM)^2)*((DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2+(DAT[paste0(UN,suffix[3])]/2)^2+(DAT[paste0(UN,suffix[4])]/2)^2)+((DAT[f3]/EM)^2)*(DAT[paste0(UN,suffix[5])]/2)^2),
              "em=act*fe*f1*f2*f3*f4"= sqrt((DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2+(DAT[paste0(UN,suffix[3])]/2)^2+(DAT[paste0(UN,suffix[4])]/2)^2+(DAT[paste0(UN,suffix[5])]/2)^2+(DAT[paste0(UN,suffix[6])]/2)^2),
              "em=act*(k1-fe)"= sqrt((DAT[paste0(UN,suffix[1])]/2)^2+((DAT[fe]/(k1-DAT[fe]))*DAT[paste0(UN,suffix[2])]/2)^2),
              "em=act*fe*(k1-f1)"= sqrt((DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2+((DAT[f1]/(k1-DAT[f1]))*DAT[paste0(UN,suffix[3])]/2)^2),
              "em=act*fe*f1*f2*(k1-f3)"= sqrt((DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2+(DAT[paste0(UN,suffix[3])]/2)^2+(DAT[paste0(UN,suffix[4])]/2)^2+((DAT[f3]/(k1-DAT[f3]))*DAT[paste0(UN,suffix[5])]/2)^2),
              "em=act*fe*f1*f2*(k1-f3)*f4"= sqrt((DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2+(DAT[paste0(UN,suffix[3])]/2)^2+(DAT[paste0(UN,suffix[4])]/2)^2+((DAT[f3]/(k1-DAT[f3]))*DAT[paste0(UN,suffix[5])]/2)^2+(DAT[paste0(UN,suffix[6])]/2)^2),
              'em=act*fe*f1*((f2*f3*(f4+k1)+f5*k2))'=sqrt(DAT[paste0(UN,suffix[1])]/2)^2+(DAT[paste0(UN,suffix[2])]/2)^2+(DAT[paste0(UN,suffix[3])]/2)^2+(1/(DAT[f2]*DAT[f3]*(DAT[f4]+k1)+DAT[f5]*k2)^2)*((DAT[paste0(UN,suffix[4])]/2)^2+(DAT[paste0(UN,suffix[5])]/2)^2+(DAT[f4]/(DAT[f4]+k1))^2*(DAT[paste0(UN,suffix[6])]/2)^2+(k2^2)*(DAT[paste0(UN,suffix[7])]/2)^2),
              'em=exp(k1*act)'=sqrt((k1*DAT[act]*DAT[paste0(UN,suffix[1])]/2)^2),
              'em=exp(k1*act*(k2-fe)/k3)'=sqrt(((k1*DAT[act]*(k2-DAT[fe])/k3)*DAT[paste0(UN,suffix[1])]/2)^2+(((DAT[fe])/k3)*DAT[paste0(UN,suffix[2])]/2)^2)
)

UP_EM=switch (eq,
              "em=act*fe" = sqrt((DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2),
              "em=act+k1*fe"=sqrt(((DAT[act]/EM)^2)*(DAT[paste0(UP,suffix[1])]/2)^2+((k1*DAT[fe]/EM)^2)*(DAT[paste0(UP,suffix[2])]/2)^2),
              "em=act*fe*f1"= sqrt((DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2+(DAT[paste0(UP,suffix[3])]/2)^2),
              "em=act*fe*(k1*f1+k2*f2)"=sqrt((DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2+(k1*DAT[f1]*DAT[paste0(UP,suffix[3])]/(2*(DAT[f1]+DAT[f2])))^2+(k2*DAT[f2]*DAT[paste0(UP,suffix[4])]/(2*(DAT[f1]+DAT[f2])))^2),
              "em=act*fe*f1*f2"= sqrt((DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2+(DAT[paste0(UP,suffix[3])]/2)^2+(DAT[paste0(UP,suffix[4])]/2)^2),
              "em=act*fe+f1"=sqrt(((DAT[fe]*DAT[act]/EM)^2)*((DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2)+((DAT[f1]/EM)^2)*((DAT[paste0(UP,suffix[3])]/2)^2)),
              "em=act*(k1-fe)+f1"=sqrt((((k1-DAT[fe])*DAT[act]/EM)^2)*((DAT[paste0(UP,suffix[1])]/2)^2+((DAT[fe]/(k1-DAT[fe]))^2)*(DAT[paste0(UP,suffix[2])]/2)^2)+((DAT[f1]/EM)^2)*((DAT[paste0(UP,suffix[3])]/2)^2)),
              "em=act*fe*f1*f2*f3"= sqrt((DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2+(DAT[paste0(UP,suffix[3])]/2)^2+(DAT[paste0(UP,suffix[4])]/2)^2+(DAT[paste0(UP,suffix[5])]/2)^2),
              "em=act*fe*f1*f2-f3"= sqrt(((1-DAT[f3]/EM)^2)*((DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2+(DAT[paste0(UP,suffix[3])]/2)^2+(DAT[paste0(UP,suffix[4])]/2)^2)+((DAT[f3]/EM)^2)*(DAT[paste0(UP,suffix[5])]/2)^2),
              "em=act*fe*f1*f2*f3*f4"= sqrt((DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2+(DAT[paste0(UP,suffix[3])]/2)^2+(DAT[paste0(UP,suffix[4])]/2)^2+(DAT[paste0(UP,suffix[5])]/2)^2+(DAT[paste0(UP,suffix[6])]/2)^2),
              "em=act*fe*(k1-f1)"= sqrt((DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2+((DAT[f1]/(k1-DAT[f1]))*DAT[paste0(UP,suffix[3])]/2)^2),
              "em=act*(k1-fe)"= sqrt((DAT[paste0(UP,suffix[1])]/2)^2+((DAT[fe]/(k1-DAT[fe]))*DAT[paste0(UP,suffix[2])]/2)^2),
              "em=act*fe*f1*f2*(k1-f3)"= sqrt((DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2+(DAT[paste0(UP,suffix[3])]/2)^2+(DAT[paste0(UP,suffix[4])]/2)^2+((DAT[f3]/(k1-DAT[f3]))*DAT[paste0(UP,suffix[5])]/2)^2),
              "em=act*fe*f1*f2*(k1-f3)*f4"= sqrt((DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2+(DAT[paste0(UP,suffix[3])]/2)^2+(DAT[paste0(UP,suffix[4])]/2)^2+((DAT[f3]/(k1-DAT[f3]))*DAT[paste0(UP,suffix[5])]/2)^2+(DAT[paste0(UP,suffix[6])]/2)^2),
              'em=act*fe*f1*((f2*f3*(f4+k1)+f5*k2))'=sqrt(DAT[paste0(UP,suffix[1])]/2)^2+(DAT[paste0(UP,suffix[2])]/2)^2+(DAT[paste0(UP,suffix[3])]/2)^2+(1/(DAT[f2]*DAT[f3]*(DAT[f4]+k1)+DAT[f5]*k2)^2)*((DAT[paste0(UP,suffix[4])]/2)^2+(DAT[paste0(UP,suffix[5])]/2)^2+(DAT[f4]/(DAT[f4]+k1))^2*(DAT[paste0(UP,suffix[6])]/2)^2+(k2^2)*(DAT[paste0(UP,suffix[7])]/2)^2),
              'em=exp(k1*act)'=sqrt((k1*DAT[act]*DAT[paste0(UP,suffix[1])]/2)^2),
              'em=exp(k1*act*(k2-fe)/k3)'=sqrt(((k1*DAT[act]*(k2-DAT[fe])/k3)*DAT[paste0(UP,suffix[1])]/2)^2+(((DAT[fe])/k3)*DAT[paste0(UP,suffix[2])]/2)^2)
)
EM=EM1
  UN_EM[is.nan(unlist(UN_EM)),1]<-0
  UP_EM[is.nan(unlist(UP_EM)),1]<-0

UNC=data.frame(EM=c(EM),UN=c(UN_EM),UP=c(UP_EM))

## Corrección por incertidumbre grande
UNC$FC=1
Up=unlist(UP_EM+UN_EM)/2

U=unlist(c(Up*100))

FC=((-0.72+1.0921*U-1.63e-3*U^2+1.11e-5*U^3)/U)^2


UNC$FC[U>=100&U<=300]=FC[U>=100&U<=300]

UNC$UN=UNC$UN*UNC$FC;
UNC$UP=UNC$UP*UNC$FC;
UNC$PAR1=UNC$EM;
UNC$PAR2=c(Up)*UNC$EM/2;
UNC$PAR3=0;UNC$DIST="NORMAL"


### Ajuste por asimetría
UNC$PAR2[UNC$EM==0]=0;UNC$PAR2[UNC$EM==0]=0
UNC$DIST[UNC$EM==0]<-"CONSTANTE"

MUG=c(exp(log(UNC$EM)-0.5*log(1+(Up/2)^2)))
SDG=c(exp(sqrt(log(1+(Up/2)^2))))

for (i in 1:nrow(UNC))
{
  if (UNC$UN[i]!=UNC$UP[i]&UNC$EM[i]>0)
  {
    UNC$UN[i]<--1*(exp(log(MUG[i])-1.96*log(SDG[i]))-UNC$EM[i])/UNC$EM[i]
    UNC$UP[i]<-(exp(log(MUG[i])+1.96*log(SDG[i]))-UNC$EM[i])/UNC$EM[i]
    UNC$DIST[i]="LOGNORMAL"
  } else
  {UNC$DIST[i]="NORMAL"}
}

DAT$EM=UNC$EM;DAT$UN_EM=UNC$UN*2;DAT$UP_EM=UNC$UP*2;DAT$PAR1_EM=UNC$PAR1;
DAT$PAR2_EM=UNC$PAR2;DAT$PAR3_EM=UNC$PAR3;DAT$DIST_EM=UNC$DIST

return(DAT)
}




if (UNC_type=="MONTECARLO")
{
require(triangle)
  print(" -- Calculando Incertidumbre por MonteCarlo")
  EM=EM1
  UNC=data.frame(EM=EM)
  UNC$PAR3=UNC$PAR2=UNC$PAR1=0
  UNC$DIST=""
  
  pb=txtProgressBar(min=0,max=nrow(DAT),style=3)
  for (j in 1:nrow(DAT))
  {
    for (k in 1:length(suffix))
    {
    Pij=switch(as.character(DAT[j,paste0("DIST",suffix[k])]),
               "NORMAL"=rnorm(it,mean=DAT[j,paste0("PAR1",suffix[k])],sd=DAT[j,paste0("PAR2",suffix[k])]),
               "LOGNORMAL"=rlnorm(it,DAT[j,paste0("PAR1",suffix[k])],DAT[j,paste0("PAR2",suffix[k])]),
               "GAMMA"=rgamma(it,DAT[j,paste0("PAR1",suffix[k])],DAT[j,paste0("PAR2",suffix[k])]),
               "WEIBULL"=rweibull(it,DAT[j,paste0("PAR1",suffix[k])],DAT[j,paste0("PAR2",suffix[k])]),
               "TRIANGULAR"=rtriangle(it,c=DAT[j,paste0("PAR1",suffix[k])],a=DAT[j,paste0("PAR2",suffix[k])],b=DAT[j,paste0("PAR3",suffix[k])]),
               "CONSTANTE"=array(DAT[j,paste0("PAR1",suffix[k])],it)
               )
    if (k==1)
    {
    Pk=Pij
    } else
    {
    Pk=cbind(Pk,Pij)
    }
    }
    Pk=data.frame(Pk)
    names(Pk)=paste0("VALOR",suffix)
    EMi=switch (eq,
                "em=act*fe" = Pk[act]*Pk[fe]*fc,
                "em=act+k1*fe"=(Pk[act]+k1*Pk[fe]),
                "em=act*fe*f1"= Pk[act]*Pk[fe]*Pk[f1]*fc,
                "em=act*fe*(k1*f1+k2*f2)"= Pk[act]*Pk[fe]*(k1*Pk[f1]+k2*Pk[f2])*fc,
                "em=act*fe+f1"=(Pk[act]*Pk[fe]+Pk[f1])*fc,
                "em=act*(k1-fe)+f1"=(Pk[act]*(k1-Pk[fe])+Pk[f1])*fc,
                "em=act*fe*f1*f2"= Pk[act]*Pk[fe]*Pk[f1]*Pk[f2]*fc,
                "em=act*fe*f1*f2*f3"= Pk[act]*Pk[fe]*Pk[f1]*Pk[f2]*Pk[f3]*fc,
                "em=act*fe*f1*f2-f3"= (Pk[act]*Pk[fe]*Pk[f1]*Pk[f2]-Pk[f3])*fc,
                "em=act*fe*f1*f2*f3*f4"= Pk[act]*Pk[fe]*Pk[f1]*Pk[f2]*Pk[f3]*Pk[f4]*fc,
                "em=act*fe*(k1-f1)"= Pk[act]*Pk[fe]*(k1-Pk[f1])*fc,
                "em=act*(k1-fe)"= Pk[act]*(k1-Pk[fe])*fc,
                "em=act*fe*f1*f2*(k1-f3)"= Pk[act]*Pk[fe]*Pk[f1]*Pk[f2]*(k1-Pk[f3])*fc,
                "em=act*fe*f1*f2*(k1-f3)*f4"= Pk[act]*Pk[fe]*Pk[f1]*Pk[f2]*(k1-Pk[f3])*Pk[f4]*fc,
                'em=act*fe*f1*((f2*f3*(f4+k1)+f5*k2))'=Pk[act]*Pk[fe]*Pk[f1]*((Pk[f2]*Pk[f3]*(Pk[f4]+k1)+Pk[f5]*k2))*fc,
                'em=exp(k1*act)'=exp(k1*Pk[act]),
                'em=exp(k1*act*(k2-fe)/k3)'=exp(k1*Pk[act]*(k2-Pk[fe])/k3)
    )
    EMi=unlist(c(EMi))
    FIT=FIT_PDF(EMi,method=method,PDF_def = PDF_def)
    UNC$PAR1[j]=FIT$par_1;UNC$PAR2[j]=FIT$par_2;UNC$PAR3[j]=FIT$par_3;UNC$DIST[j]=as.character(FIT$PFD)
    setTxtProgressBar(pb,j)
  }
  print("Determinando Incertidumbre según iteraciones")
  INC=Unc_FDP(UNC$DIST,UNC$PAR1,UNC$PAR2,UNC$PAR3)
  
  DAT$EM=UNC$EM;DAT$UN_EM=INC$Un;DAT$UP_EM=INC$Up;DAT$PAR1_EM=UNC$PAR1;
  DAT$PAR2_EM=UNC$PAR2;DAT$PAR3_EM=UNC$PAR3;DAT$DIST_EM=UNC$DIST
  
  return(DAT)
}  

}
