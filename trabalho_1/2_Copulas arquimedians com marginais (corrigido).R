library(copula)

#%%%%%%%%%%   Amostrar 10000 obs de (U, V) com dist COPULAS ARQUIMEDIANAS       %%%%%%%%%%

###########     Clayton copula   ###################
cc <- claytonCopula(2)
sample <- rCopula(10000,cc)
#Scatterplot
plot(sample, xlab="U", ylab="V", pch = "../../../Downloads", cex = 1.5)
#contour
contour(cc, dCopula)
#Density
persp(cc,dCopula, xlab="u", ylab="v", zlab="c(u,v)")

###########     Frank copula   ###################
fr <- frankCopula(5)
sample <- rCopula(10000,fr)
#Scatterplot
plot(sample, xlab="U", ylab="V", pch = "../../../Downloads", cex = 1.5)
#contour
contour(fr, dCopula)
#Density
persp(fr,dCopula, xlab="u", ylab="v", zlab="C(u,v)", shade=.0001)

################  Gumbel copula #######################
gu <- gumbelCopula(4)
sample <- rCopula(10000,gu)
#Scatterplot
plot(sample, xlab="U", ylab="V", pch = "../../../Downloads", cex = 1.5)
#contour
contour(gu, dCopula)
#Density
persp(gu,dCopula, xlab="u", ylab="v", zlab="C(u,v)")


############# DADOS PETROBRAS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PBR<-read.csv(file.choose())

PBR$retorno<-log(PBR$High/dplyr::lag(PBR$High))
head(PBR$retorno)

(PBR<-PBR[is.finite(PBR$retorno), ])
(ret<-PBR$retorno)

N<-length(ret)
n<-15
tau<-floor(N/n)
m<-matrix(0,tau,1)
j<-1
for (i in 1:tau){
  m[i]<-max(ret[j:(j+n-1)])
  j<-j+n}
head(m)
sum(is.na(m))
hist(m,n=15, prob=T,ylim=c(0,28))
lines(density(m),lwd=2)

length(m)
m<- m[1:35]
#######################################
N<-length(ret)
n<-15
tau<-floor(N/n)
mi<-matrix(0,tau,1)
j<-1
for (i in 1:tau){
  mi[i]<-min(ret[j:(j+n-1)])
  j<-j+n}
head(mi)
sum(is.na(mi))
hist(mi,n=15, prob=T,ylim=c(0,28))
lines(density(mi),lwd=2)

mi<- mi[1:35]
length(mi)

plot(m, mi, mlab="", milab="")
#%%%%%%%%%%%%%%%%%% DADOS VALE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

VALE<-read.csv(file.choose())

VALE$retorno<-log(VALE$High/dplyr::lag(VALE$High))
head(VALE$retorno)

(VALE<-VALE[is.finite(VALE$retorno), ])
(ret<-VALE$retorno)

N<-length(ret)
n<-7
tau<-floor(N/n)
m2<-matrix(0,tau,1)
j<-1
for (i in 1:tau){
  m2[i]<-max(ret[j:(j+n-1)])
  j<-j+n}
head(m2)
sum(is.na(m2))
hist(m2,n=7, prob=T,ylim=c(0,28))
lines(density(m2),lwd=2)
m2<- m2[1:35]
length(m2)

plot(m, m2, mlab="", milab="")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%   SIMULATION  (x,Y)   AND MARGINS GEV %%%%%%%

n<- 1000
cc <- claytonCopula(1.593)
sample <- rCopula(1000,cc)
plot(sample, xlab="U", ylab="V", pch = ".", cex = 1.5)

#########################
##  MLE for marginal PBR
qsi1=0.327662988
mu1=0.006650574
s1=0.005636182
## MLE for marginal 
qsi2=-0.57438497
mu2= 0.00367867
s2= 0.03569279

x<- numeric(n)
y<- numeric(n)

for ( i in 1 : n ){
  x[i] <- mu1 + s1*( ((-log(sample[i,1]))^(-qsi1) - 1 )/ qsi1 )
  y[i] <- mu2 + s2*( ((-log(sample[i,2]))^(-qsi2) - 1 )/ qsi2 )
  
}

par(mfrow=c(1,1))
plot(sample, xlab="U", ylab="V", pch = ".", cex = 5,col="red" main="Copula Clayton")
plot( x, y, ylab="Y", pch = ". ", cex = 5, col="black", main="Clayton marginal GEV " )

###############################################
library(ggplot2)

ggplot(mapping = aes(x = x, y = y)) +
  stat_density2d() +  geom_point()

###############################################
plot(m, m2, pch = ". ", cex = 5 , col="red", xlab=" " , ylab=" " ) 
points( x, y, ylab="Y", pch = ". ", cex = 5, col="black")
legend("topright", legend=c("Dados simulados", "Dados reais"), col=c("red", "black"), pch = 15)
#########
ggplot() +
  stat_density2d(mapping = aes(x = x, y = y)) +
  stat_density2d(mapping = aes(x = m, y = m2), color = "red")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%















#################### TESTANDO CODIGO DE MARGINAIS %%%%%%%%%%%%

## X~ GEV(.;  loc1, scale1, shape1) : parametros obridos no trabalho 1
## Y~ GEV(.; loc2, scale2, shpe2) : parametros obridos no trabalho 1 

## FDA fornecida
fdagev <- function(x, mu, s, qsi) {
  if (qsi != 0) {
    return( exp(-(1 + qsi*(x-mu)/s)^(-1/qsi)) )
  } else {
    return( exp(-exp(- (x-mu)/s)) )
  }
}

## Inversa da FDA (Quantil)
qtgev <- function(q, mu, s, qsi) {
  if (qsi != 0) {
    return(mu - (s/qsi) * ( 1 - (-log(q))^(-qsi)) )
  } else {
    return(mu - scale*log(-log(q))  )
  }
}

## Derivada da FDA (Densidade)
fdpgev <- function(x, mu, s, qsi) {
  if (qsi != 0) {
    return ( exp(-(1 + qsi*(x-mu)/s)^(-1/qsi)) *
               (1/qsi*(1 + qsi*(x-mu)/s)^(-1/qsi-1) ) * qsi/s )
  } else {
    return ( exp(-exp(- (x-mu)/s)) * (1/s)*(exp(- (x-mu)/s)) )
  }
}


######################### TESTANDO MARGINAIS###
y<- runif(100, 0, 1)
## Note que a funcao foi manualmente definida anteriormente
amostra1 <- qtgev(q = y, mu1, s1, qsi1)
amostra2 <- qtgev(q = y, mu2, s2, qsi2)

hist(amostra1, probability = T) 
curve(fdpgev(x, mu1, s1, qsi1),
      col='blue',lwd=2, add=TRUE)

hist(amostra2, probability = T) 
curve(fdpgev(x, mu2, s2, qsi2),
      col='blue',lwd=2, add=TRUE)
x<-amostra1
y<-amostra2

plot(x, y) # ?

########################################



