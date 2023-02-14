# Aula 1 Métodos Aplicados - Cópulas -----
# Carregando pacotes -----

install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, hms, scales, ggplot2, lubridate, dplyr, readr, gridExtra,
               copula, fBasics, StableEstim, stabledist, PerformanceAnalytics,
               extRemes, ismev, evmix, extremis, DT, gdtools, kableExtra, VGAM, evd, 
               fExtremes, graphics, ggExtra, patchwork)


# --------------------------------------------------
# Lendo e limpando dados -------
#Definindo Diretório
setwd("D:/Estudos/Métodos Aplicados/Dados")

#Lendo os bancos
fb <- read.csv("~/Estudos/Métodos Aplicados/Dados/FB.CSV")
head(fb)
tw <- read.csv("~/Estudos/Métodos Aplicados/Dados/TWTR.CSV")
head(tw)

# --------------------------------------------------
# Graficos Iniciais (hist e linha) ----
hist1 <- ggplot(fb, aes(x=High)) + geom_histogram(fill="#00688B",bins=30, color = "black")+
  labs(x="Facebook", y="Frequencia") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

linhas1 <- ggplot(fb, aes(x=as.Date(Date), y=High, group=1)) +
  geom_line(size=1, colour="#00688B") +
  labs(x="Data", y="Valores maximos Facebook") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_labels = "%Y")

hist2 <- ggplot(tw, aes(x=High)) + geom_histogram(fill="#00688B",bins=30, color = "black")+
  labs(x="Twitter", y="Frequencia") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

linhas2 <- ggplot(tw, aes(x=as.Date(Date), y=High, group=1)) +
  geom_line(size=1, colour="#00688B") +
  labs(x="Data", y="Valores maximos Twitter") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_x_date(date_labels = "%Y")

grid.arrange(hist1, linhas1, hist2, linhas2)
ggsave("grafinicial.png", arrangeGrob(hist1, linhas1, hist2, linhas2), device = "png",
       width = 8)
dev.off()


# Dispersão bivariada dos maximos iniciais
hist2vertical <- ggplot(tw, aes(x=High)) + geom_histogram(fill="#00688B",bins=30, color = "black")+
  labs(x="Twitter", y="Frequencia") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  coord_flip()

#Pegando apenas dos dados dos maximos para o gráfico de dispersão (os bancos tem a ter o mesmo tamanho)
dadosmax <- data.frame(banco1 = fb$High,
                       banco2 = tw$High[-c(1,2)])

dispercao <- ggplot(dadosmax, aes(banco1, banco2)) + geom_point() + 
             theme_classic() +
             labs(x="Blocos maximos Facebook", y="Blocos maximos Twitter")

hist1 + plot_spacer() + dispercao + hist2vertical + 
  plot_layout(
    ncol = 2, 
    nrow = 2, 
    widths = c(4, 1),
    heights = c(1, 4)
  ) 
ggsave("disphistinicial.png", width = 158, height = 93, units = "mm")


# --------------------------------------------------
# Retorno Facebook -----
fb$retorno<-log(fb$High/dplyr::lag(fb$High))
head(fb$retorno)

(fb<-fb[is.finite(fb$retorno), ])
(ret<-fb$retorno)

ggplot(fb, aes(x=as.Date(Date), y=retorno, group=1)) +
  geom_line(size=0.4, colour="#00688B") +
  labs(x="Data", y="Valores maximos Facebook") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_labels = "%Y")

ggplot(fb, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                            fill="#00688B",bins =50)+
  labs(x="Log Retorno Diario Facebook", y="Densidade") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

media<-mean(ret)
varianc<-var(ret)
dp<-sqrt(varianc)
setNames(c(media,varianc,dp),c("Media "," Variancia "," Desvio Padrao"))
rn<-rnorm(10000,media,dp)
summary(ret)

shapiro.test(fb$retorno)

##Estimacao da estavel 
## alpha=forma, beta=assimetria, gamma=escala, delta=locacao.
## Ajuste dos parametros metodo os quantis
(st1<-stableFit(ret, type= "q",doplot = TRUE))

##Ajuste dos parametros via MLE
#est<-Estim(EstimMethod="ML",ret)
(st2<-stableFit(ret, "mle",doplot = TRUE))
(par.est<-st2@fit$estimate)
alpha.est1<-par.est[1] ; beta.est1<-par.est[2]
gamma.est1<-par.est[3] ; delta.est1<-par.est[4]


ggplot(fb, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                            fill="#00688B",bins =50)+
  labs(x="Log Retorno Diario Facebook", y="Densidade") +
  stat_function(fun = dnorm, args = list(mean = media, sd = dp), 
                size = 1, color = "#000000") +
  stat_function(fun = dstable, args = list(alpha=alpha.est1, beta = beta.est1, 
                                           gamma =gamma.est1, delta = delta.est1), 
                size = 1, color = "#e36414") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


##calculo do VAR (Historico e Normal) 
library("PerformanceAnalytics")##pacote uasado

p<-c(0.90,0.95,0.975,0.99,0.999) 
## VaR historico
V.hist<-sapply(p, PerformanceAnalytics::VaR, R=ret, method="historical")
## VaR normal     
V.normal<-sapply(p, PerformanceAnalytics::VaR, R=ret, method="gaussian")
## VaR alpha-estavel
V.alpha<-sapply(p, qstable,alpha= alpha.est1,beta= beta.est1,gamma= gamma.est1,delta= delta.est1,pm=0)

## Apresentação dos VaRs obtidos
df_info<-data.frame(p,V.hist,V.normal,V.alpha)
names(df_info)<-c("Nivel de confianca","VaR Historico","VaR Normal","VaR Alpha-Estavel")

kable(df_info) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))


# --------------------------------------------------
# Retorno Twitter -----
tw$retorno<-log(tw$High/dplyr::lag(tw$High))
head(tw$retorno)

(tw<-tw[is.finite(tw$retorno), ])
(ret<-tw$retorno)

ggplot(tw, aes(x=as.Date(Date), y=retorno, group=1)) +
  geom_line(size=0.4, colour="#00688B") +
  labs(x="Data", y="Valores maximos Retorno Twitter") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_labels = "%Y")
ggplot(tw, aes(x=retorno)) + geom_histogram(colour="white",aes(y=..density..), 
                                            fill="#00688B",bins =50)+
  labs(x="Log Retorno Diario Twitter", y="Densidade") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

media<-mean(ret)
varianc<-var(ret)
dp<-sqrt(varianc)
setNames(c(media,varianc,dp),c("Media "," Variancia "," Desvio Padrao"))
rn<-rnorm(10000,media,dp)
summary(ret)

shapiro.test(tw$retorno)

##Estiamacao da estavel (Acrescentar!)
## alpha=forma, beta=assimetria, gamma=escala, delta=locacao.
## Ajuste dos parametros metodo os quantis
(st1<-stableFit(ret, "q",doplot = TRUE))

##Ajuste dos parametros via MLE
#est<-Estim(EstimMethod="ML",ret)
(st2<-stableFit(ret, "mle",doplot = TRUE))
(par.est<-st2@fit$estimate)
alpha.est2<-par.est[1] ; beta.est2<-par.est[2]
gamma.est2<-par.est[3] ; delta.est2<-par.est[4]

ggplot(tw, aes(x=retorno)) + geom_histogram(colour="white",aes(y=..density..), 
                                            fill="#00688B",bins =50)+
  labs(x="Log Retorno Diario Twitter", y="Densidade") +
  stat_function(fun = dnorm, args = list(mean = media, sd = dp), 
                size = 1, color = "#000000") +
  stat_function(fun = dstable, args = list(alpha=alpha.est2, beta = beta.est2, 
                                           gamma =gamma.est2, delta = delta.est2), 
                size = 1, color = "#e71d36") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))


##calculo do VAR (Historico e Normal) 
library("PerformanceAnalytics")##pacote uasado

p<-c(0.90,0.95,0.975,0.99,0.999) 
## VaR historico
V.hist<-sapply(p, PerformanceAnalytics::VaR, R=ret, method="historical")
## VaR normal     
V.normal<-sapply(p, PerformanceAnalytics::VaR, R=ret, method="gaussian")
## VaR alpha-estavel
V.alpha<-sapply(p, qstable,alpha= alpha.est2,beta= beta.est2,gamma= gamma.est2,delta= delta.est2,pm=0)

## Apresentação dos VaRs obtidos
df_info<-data.frame(p,V.hist,V.normal,V.alpha)
names(df_info)<-c("Nivel de confianca","VaR Historico","VaR Normal","VaR Alpha-Estavel")

kable(df_info) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# --------------------------------------------------
# Graficos Retornos ------
serie1 <- ggplot(fb, aes(x=as.Date(Date), y=retorno, group=1)) +
  geom_line(size=0.4, colour="#00688B") +
  labs(x="Data", y="Valores maximos Retorno Facebook") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_labels = "%Y")

loghist1 <- ggplot(fb, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                                 fill="#00688B",bins =50)+
  labs(x="Log Retorno Diario Facebook", y="Densidade") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

serie2 <- ggplot(tw, aes(x=as.Date(Date), y=retorno, group=1)) +
  geom_line(size=0.4, colour="#00688B") +
  labs(x="Data", y="Valores maximos Retorno Twitter") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=8),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_date(date_labels = "%Y")

loghist2 <- ggplot(tw, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                                 fill="#00688B",bins =50)+
  labs(x="Log Retorno Diario Twitter", y="Densidade") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

grid.arrange(serie1, loghist1, serie2, loghist2)
ggsave("grafinicialret.png", arrangeGrob(serie1, loghist1, serie2, loghist2), device = "png",
       width = 8)
dev.off()


logret1 <- ggplot(fb, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                                       fill="#00688B",bins =50)+
  labs(x="Log Retorno Diario Facebook", y="Densidade") +
  stat_function(fun = dnorm, args = list(mean = media, sd = dp), 
                size = 1, color = "#000000") +
  stat_function(fun = dstable, args = list(alpha=alpha.est1, beta =beta.est1, 
                                           gamma =gamma.est1, delta = delta.est1), 
                size = 1, color = "#e36414") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"))

logret2 <- ggplot(tw, aes(x=retorno)) + geom_histogram(colour="black",aes(y=..density..), 
                                                 fill="#00688B",bins =50)+
  labs(x="Log Retorno Diario Twitter", y="Densidade") +
  stat_function(fun = dnorm, args = list(mean = media, sd = dp), 
                size = 1, color = "#000000") +
  stat_function(fun = dstable, args = list(alpha=alpha.est2, beta = beta.est2, 
                                           gamma =gamma.est2, delta = delta.est2), 
                size = 1, color = "#e36414") +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")) 


grid.arrange(logret1, logret2)
ggsave("histalfaret.png", arrangeGrob(logret1, logret2), device = "png",
       width = 8)
dev.off()

# --------------------------------------------------
# Bloco Facebook------
ret<-fb$retorno
High <- ret
N<-length(High)
result <- data.frame() 
for (k in 1:10) {  
  n<-k  
  tau<-floor(N/n)  
  m<-numeric(tau) ; j<-1
  for (i in 1:tau){   
    m[i]<-max(ret[j:(j+n-1)])    
    j<-j+n }    
  m<-m[-1]    
  teste<-Box.test(m, lag = 1,              
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)    
  teste$indice <- k   
  teste <- c(teste$indice,teste$p.value)
  result <- rbind(result, teste)
}
result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")
kable(result) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))


#Bloco de tamanho 2
N<-length(ret)  ; n<-2
tau<-floor(N/n)
M<-numeric(tau) ; j<-1

for (i in 1:tau){
  M[i]<-max(ret[j:(j+n-1)])
  j<-j+n }

M<-M[-1]
par(mfrow=c(1,2))
{hist(M, prob=T, col = "#00688B", ylim = c(0,40), ylab = "Densidade")
  lines(density(m))}
plot(M, type="l",main="Some Extremes")
dev.off()

p<-c((1:tau)/(tau+1)) ; ginv<- -log(-log(p))
qqplot(M,ginv,xlab="Quantis empiricos",ylab="Quantis da Gumbel",main="qqplot") ; grid()

#Estimando
library(evd)
library(VGAM)
library(fExtremes)
## R MLE - GEV
fitmv = gevFit(M, type ="mle")
fitmv
par(mfrow = c(2, 2))
summary(fitmv)

## R PWM - GEV
fitpwm = gevFit(M, type ="pwm")
fitpwm
par(mfrow = c(2, 2))
summary(fitpwm)
dev.off()

#Graficos
#Pegar valores dos parametros acima e inserir manualmente
x<-M
hist(x,prob=T,ylim=c(0,35),main='',cex.axis=1.5, cex.lab=1.5, cex=1.5, font.axis=2,lwd=2, col = "#00688B")
curve(dgev(x, xi = -0.033180969, mu = 0.002676859, beta = 0.012677361),col='black', lwd=2, add=TRUE)
curve(dgev(x, xi = 0.045238260, mu = 0.002420717, beta= 0.010798933),col='#e36414',lwd=2,add=TRUE)
legend('topright',legend=c('MLE','PWM'),col=c('black','#e36414'),lwd=2)

fit <- fevd(m,type="GEV")
fit #positive shape estimate but fairly large standard error
plot(fit) #The fit looks reasonable
ci(fit,type="parameter") #As expected the 95% confidence interval includes negative values
return.level(fit,do.ci=T)


# --------------------------------------------------
# Bloco Twitter------
ret<-tw$retorno
High <- ret
N<-length(High)
result <- data.frame() 
for (k in 1:10) {  
  n<-k  
  tau<-floor(N/n)  
  m<-numeric(tau) ; j<-1
  for (i in 1:tau){   
    m[i]<-max(ret[j:(j+n-1)])    
    j<-j+n }    
  m<-m[-1]    
  teste<-Box.test(m, lag = 1,              
                  type = c("Box-Pierce", "Ljung-Box"), 
                  fitdf = 0)    
  teste$indice <- k   
  teste <- c(teste$indice,teste$p.value)
  result <- rbind(result, teste)
}
result <- tibble(result)
names(result) <- c("Tamanho do bloco","P-valor (teste de Ljung-Box)")
kable(result) %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))


#Tamanho de Bloco 2
N<-length(ret)  ; n<-2
tau<-floor(N/n)
MI<-numeric(tau) ; j<-1

for (i in 1:tau){
  MI[i]<-max(ret[j:(j+n-1)])
  j<-j+n }

MI<-MI[-1]
par(mfrow=c(1,2))
{hist(MI, prob=T,col = "#00688B", ylim = c(0,22), ylab = "Densidade")
  lines(density(MI))}
plot(MI, type="l",main="Some Extremes")
dev.off()

p<-c((1:tau)/(tau+1)) ; ginv<- -log(-log(p))
qqplot(MI,ginv,xlab="Quantis empiricos",ylab="Quantis da Gumbel",main="qqplot") ; grid()


#Estimando
library(evd)
library(VGAM)
library(fExtremes)
## R MLE - GEV
fitmv = gevFit(MI, type ="mle")
fitmv
par(mfrow = c(2, 2))
summary(fitmv)
## R PWM - GEV
fitpwm = gevFit(MI, type ="pwm")
fitpwm
par(mfrow = c(2, 4))
summary(fitpwm)
dev.off()

#Graficos
#Pegar valores dos parametros acima e inserir manualmente
x<-MI
hist(x,prob=T,ylim=c(0,22),main='',cex.axis=1.5, cex.lab=1.5, cex=1.5, font.axis=2,lwd=2, col = "#00688B")
curve(dgev(x, xi = -0.06887585, mu = 0.00344904, beta = 0.02431638),col='black', lwd=2, add=TRUE)
curve(dgev(x, xi = 0.028444735, mu = 0.002867182, beta = 0.019107333),col='#e36414',lwd=2,add=TRUE)
legend('topright',legend=c('MLE','PWM'),col=c('black','#e36414'),lwd=2)

fit <- fevd(m,type="GEV")
fit #positive shape estimate but fairly large standard error
plot(fit) #The fit looks reasonable
ci(fit,type="parameter") #As expected the 95% confidence interval includes negative values
return.level(fit,do.ci=T)
