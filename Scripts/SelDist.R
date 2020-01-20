#### Suponer Distribución

SelDist<-function(DAT,VALOR="VALOR",UN="UN",UP="UP",SUFFIX="")
{
  require(nleqslv)
  DAT[is.na(DAT[VALOR]),VALOR]<-0
  DAT[is.na(DAT[UN]),UN]<-0
  DAT[is.na(DAT[UP]),UP]<-0
  DAT[DAT[VALOR]==0,UN]<-0
  DAT[DAT[VALOR]==0,UP]<-0
  Q1=DAT[VALOR]*(1-DAT[UN]);Q2=DAT[VALOR]*(1+DAT[UP])
  MN=DAT[VALOR] 
  
  MNT=data.frame(MN,Q1,Q2)
  
  MID=(Q1+Q2)/2

  
  C1=MN-Q1
  C2=DAT[UN]*MN-DAT[UP]*MN
  C3=Q1+(2*(MN-Q1)^2)/(Q2-Q1)
  C4=Q2-(2*(MN-Q2)^2)/(Q2-Q1)
  C5=(C3>=(Q2+Q1)/2)
  C6=(C4<=(Q2+Q1)/2)
  CT=cbind(C1,C2,C3,C4,C5,C6,Q1,MN,Q2)

  
  
    
  ncol=ncol(DAT)
  
  DAT$PAR3=DAT$PAR2=DAT$PAR1=0
  DAT$DIST=""
  
  print("Determinando tipos de ditribuciones")
  pb=txtProgressBar(0,nrow(DAT),style=3)
  
  for (i in 1:nrow(DAT))
  {
  if (MN[i,1]==0|(DAT[i,UN]==0&DAT[i,UP]==0))
  {DAT$PAR1[i]<-MN[i,1];DAT$DIST[i]="CONSTANTE"} else if (C2[i,1]==0&MN[i,1]!=0)
  {DAT$PAR1[i]<-MN[i,1];DAT$PAR2[i]=abs(MN[i,1])*DAT[i,UN]/2;DAT$DIST[i]="NORMAL"}else if (DAT$DIST[i]=="")
  {DAT$DIST[i]="TRIANGULAR"}
  setTxtProgressBar(pb,i)
  }    
    

 h=which(DAT$DIST=="TRIANGULAR")



 
 if (length(h)>0)
 {
   print("Determinando parámetros de distribuciones triangulares")
   pb=txtProgressBar(0,length(h),style=3)
   for (j in h)
   {
     h1=which(j==h)
   q2.5=Q1[j,1];q50=MN[j,1];q97.5=Q2[j,1]
   q=c(q2.5=Q1[j,1],q50=MN[j,1],q97.5=Q2[j,1])
  
   if(q[1]==q[2]|q[2]==q[3])
   {DAT$PAR1[j]=q[2];DAT$PAR2[j]=q[1];DAT$PAR3[j]=q[3]} else 
   {
     fn <- function(x,q) q-qtriangle(c(0.025, 0.5, 0.975), x[1], x[2], x[3])
     a=try(sol <- nleqslv(c(0,1,1),fn,q=q,control=list(btol=.00001)),silent=TRUE)
     if (class(a)=="try-error")
     {DAT$PAR1[j]=q[2];DAT$PAR2[j]=q[1];DAT$PAR3[j]=q[3]} else if (sol$x[2]>sol$x[3])
     {DAT$PAR1[j]=q[2];DAT$PAR2[j]=q[1];DAT$PAR3[j]=q[3]} else
     {
       if (q[2]>sol$x[3]|q[2]<sol$x[1])
       {DAT$PAR1[j]=q[2];DAT$PAR2[j]=q[1];DAT$PAR3[j]=q[3]} else
       {
       DAT$PAR1[j]=sol$x[2];DAT$PAR2[j]=sol$x[1];DAT$PAR3[j]=sol$x[3]
       }
     }
   }
   setTxtProgressBar(pb,h1)
   }
 }
 names(DAT)[ncol+(1:4)]=paste0(names(DAT)[ncol+(1:4)],SUFFIX)
return(DAT)
}
