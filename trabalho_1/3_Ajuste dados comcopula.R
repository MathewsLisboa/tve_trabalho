
## Estimação dos parâmetros da densidade conjunta por IFM 
library(copula)
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

############  Valor inicial#############
#Parametros das copulas

a.0<-sin(cor(m,m2, method = "kendal")*pi/2) #Correlação de kendal
a.0

cor(m,m2, method = "kendal")*pi/2



start<- c(a.0)
udat<- cbind(pgev(m, xi= 0.327662988, mu=0.006650574, beta=0.005636182),
             pgev(m2, xi= -0.57438497, mu=0.00367867, beta=0.03569279))


myCop.clayton<- claytonCopula(dim=2)
myCop.gumbel<-gumbelCopula(dim=2)
myCop.frank<- frankCopula(dim=2)

fit.if1 <-fitCopula(myCop.clayton,udat, start=a.0)
fit.if1
fit.if2 <- fitCopula(myCop.gumbel,udat, start=a.0)
fit.if2
fit.if3 <-fitCopula(myCop.frank,udat, start=a.0)
fit.if3

library(copula)
cc<- claytonCopula(1.593)
sample1<- rCopula(10000, cc)
gu<- gumbelCopula(1.048 )
sample2<- rCopula(10000,gu)
fr<- frankCopula(2.765)
sample3<- rCopula(10000,fr)


#Aic e BIC 
aicCalyton <- -2*fit.if1@loglik+2
bicCalyton <- -2*fit.if1@loglik+log(10000)

aicGumbel <- -2*fit.if2@loglik+2
bicGumbel <- -2*fit.if2@loglik+log(10000)

aicFrank <- -2*fit.if3@loglik+2
bicFrank <- -2*fit.if3@loglik+log(10000)


aicCalyton
bicCalyton

aicFrank
bicFrank

aicGumbel
bicGumbel

################################# Escolhemos aquela cópula com menor AIC 

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

############################################### COrrelcao empírica

tau<-(cor(m,m2, method = "kendal")
tau

theta<-(2*tau)/(1-tau)
theta


########## OK!

gumbel.cop <- gumbelCopula(1.593)
tau(gumbel.cop)
rho(gumbel.cop)
lambda(gumbel.cop)


#############


