n=25;  Eci=2;  Vci=4
ni=1:25
q=(1/2**ni)
p=(1-q)
ES=2*sum(q)
ES
VAR1=4*sum(q)
res=0
for(i in 1:25){
  res <- res+p[i]*q[i]
}
Var2=4*res
VarS=VAR1+Var2
VarS
########################
#Solucion usando simulacion
########################
sim=100000
S <- numeric(sim)
for(j in 1:sim){
Ci=rweibull(25,1,2) #b=1/alpha
Di <- rbinom(25,1,q)
CD <- Ci*Di
S[j] = sum(CD)
}
mean(S)
var(S)
############
#problema 3
#sol. ineficiente
############
t=as.numeric(proc.time())[3]
#inicio cronómetro
error=1.96*sqrt(VarS/25)
l1=ES-error
l2=ES+error
contador=numeric(sim)
for(i in 1:sim){
  if(S[i]>l1&&S[i]<l2){
    contador[i] = 1
  }else{contador[i]=0}
}
aproximacion=mean(contador)
aproximacion
tiempo1=as.numeric(proc.time())[3]-t
#termino cronómetro
##Ejemplos de sentencias logicas
4>1
(4>1)
(4<1)
(4=1)
(4==1)
(4!=1)
(4<=1)
(4<=4)
(1<4)
(1<4<10)
(1<4 && 4<10) #&-ampersant

###################
##################
# Otra forma más eficiente
t=as.numeric(proc.time())[3]
error=1.96*sqrt(VarS/25)
l1=ES-error
l2=ES+error
contador=0
for(i in 1:sim){
  if(S[i]>l1&&S[i]<l2){
    contador = contador + 1
  }
}
aproximacion=contador/sim
aproximacion
tiempo2=as.numeric(proc.time())[3]-t
#fin del cronómetro

###OTRA forma más
t=as.numeric(proc.time())[3]
error=1.96*sqrt(VarS/25)
l1=ES-error
l2=ES+error
contador=0
for(i in 1:sim){
  contador=contador+1*(S[i]>l1&&S[i]<l2)
  }
aproximacion=contador/sim
aproximacion
tiempo3=as.numeric(proc.time())[3]-t

###Aproximacion normal
aux1=(l1-ES)/sqrt(VarS)
aux2=(l2-ES)/sqrt(VarS)
#Aprox
pnorm(aux2)-pnorm(aux1)