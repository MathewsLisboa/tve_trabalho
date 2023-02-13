pacman::p_load('tidyverse','lubridate','copula',
               'fBasics','StableEstim','stabledist',
               'DT','kableExtra','PerformanceAnalytics',
               'extRemes', 'ismev', 'evmix', 'evd', 'extremis',
               'VGAM', 'fExtremes', 'dplyr', 'ggplot2', 'ggpubr',
               'bgumbel')


ano_inicio <- as.Date("2014-01-02", format = "%Y-%m-%d")
ano_fim <-  as.Date("2022-12-29", format = "%Y-%m-%d")
### Carrega as funções customizadas
source("funcoes.R")


BB <- read_csv("BBSE3.SA.csv") %>%
  summarise(Date = as.Date(Date, format = "%Y-%m-%d"), High = as.numeric(High)) %>%
  mutate(Retorno = log(High/dplyr::lag(High))) %>%
  dplyr::filter(Date >= ano_inicio & Date <= ano_fim)

PORTO <- read_csv("PSSA3.SA.csv") %>%
  summarise(Date = as.Date(Date, format = "%Y-%m-%d"), High = as.numeric(High)) %>%
  mutate(Retorno = log(High/dplyr::lag(High))) %>%
  dplyr::filter(Date >= ano_inicio & Date <= ano_fim)


BB <- BB[is.finite(BB$Retorno), ]
PORTO <- PORTO[is.finite(PORTO$Retorno), ]

### Analise exploratoria
head(BB, 5) %>%
  kbl(caption="Alta e retorno das ações da BB.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)

head(PORTO, 5) %>%
  kbl(caption = "Alta e retorno das ações da PORTO.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)

Serie(data = BB, col_x = "Date", col_y = "High")
ggsave ("imagens/serie_high_bbse3.png", width = 6, height = 4)
Serie(data = PORTO, col_x = "Date", col_y = "High")
ggsave ("imagens/serie_high_porto.png", width = 6, height = 4)


## Exploratoria do log-retorno
BB_ret <- BB$Retorno
PORTO_ret <- PORTO$Retorno

BB_exp <- tab_exp(BB_ret) %>%
  map_df(~format(.x, scientific = FALSE))
BB_exp %>%
  kable(caption = "Medidas resumo do retorno das ações da BB.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)


PORTO_exp <- tab_exp(PORTO_ret) %>%
  map_df(~format(.x, scientific = FALSE))
PORTO_exp %>%
  kbl(caption = "Medidas resumo do retorno das ações da PORTO.", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F)


Serie(BB, col_x = "Date", col_y = "Retorno")
ggsave ("imagens/serie_retorno_bbse3.png", width = 6, height = 4)
Serie(PORTO, col_x = "Date", col_y = "Retorno")
ggsave ("imagens/serie_retorno_bbse3.png", width = 6, height = 4)


Hist_Fit(data = BB, values = 'Retorno')
ggsave ("imagens/hist_retorno_bbse3.png", width = 6, height = 4)
Hist_Fit(data = PORTO, values = 'Retorno')
ggsave ("imagens/hist_retorno_porto.png", width = 6, height = 4)


BB_stF1 <- stableFit(BB_ret, "q", doplot = TRUE)
PORTO_stF1 <- stableFit(PORTO_ret, "q", doplot = TRUE)
# BB_stF2 <- stableFit(BB_ret, "mle", doplot = TRUE)
# PORTO_stF2 <- stableFit(PORTO_ret, "mle", doplot = TRUE)

BB_alpha_params <- BB_stF1@fit[["estimate"]]
Hist_Fit(data = BB, values = 'Retorno',
         fits = c('gaussian', 'stable'), fits_param = list('stable' = BB_alpha_params))
ggsave ("imagens/hist_retorno_alfa_bbse3.png", width = 6, height = 4)


PORTO_alpha_params <- PORTO_stF1@fit[["estimate"]]
Hist_Fit(data = PORTO, values = 'Retorno',
         fits = c('gaussian', 'stable'), fits_param = list('stable' = PORTO_alpha_params))
ggsave ("imagens/hist_retorno_alfa_porto.png", width = 6, height = 4)



### VaR
p <- c(0.95, 0.99, 0.999)
## Historico, Normal, Alfa-Estavel (respectivamente)
BB_VaR <- tibble(
  "VaR Historico" = map_dbl(p, ~PerformanceAnalytics::VaR(BB_ret, p=.x, method = "historical")),
  "VaR Gaussiano" = map_dbl(p, ~PerformanceAnalytics::VaR(BB_ret, p=.x, method = "gaussian")),
  "VaR Alfa-Estavel" = map_dbl(p, qstable, alpha=BB_alpha_params['alpha'], beta=BB_alpha_params['beta'],
                               gamma=BB_alpha_params['gamma'], delta=BB_alpha_params['delta'])) %>%
  map_df(round, 4) %>%
  mutate("Confianca" = paste0(p*100,"%"), .before = "VaR Historico")
BB_VaR %>%
  kbl(booktabs = T, caption = "\\label{tab:BBvar3}VaR (Value at Risk) para os log-retornos diários da BB") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

PORTO_VaR <- tibble(
  "VaR Historico" = map_dbl(p, ~PerformanceAnalytics::VaR(PORTO_ret, p=.x, method = "historical")),
  "VaR Gaussiano" = map_dbl(p, ~PerformanceAnalytics::VaR(PORTO_ret, p=.x, method = "gaussian")),
  "VaR Alfa-Estavel" = map_dbl(p, qstable, alpha=PORTO_alpha_params['alpha'], beta=PORTO_alpha_params['beta'],
                               gamma=PORTO_alpha_params['gamma'], delta=PORTO_alpha_params['delta'])) %>%
  map_df(round, 4) %>%
  mutate("Confianca" = paste0(p*100,"%"), .before = "VaR Historico")
PORTO_VaR %>%
  kbl(booktabs = T, caption = "\\label{tab:PORTOvar3}VaR (Value at Risk) para os log-retornos diários da PORTO") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

BB_VaR[-1] %>%
  map_df(~round(exp(.x)*1000), 0) %>%
  mutate("Confianca" = paste0(p*100,"%"), .before = "VaR Historico") %>%
  rename_all( ~ c("Confiança", "VaR Historico($)", "VaR Gaussiano($)", "VaR Alfa-Estável($)")) %>%
  kbl(booktabs = T, caption = "\\label{tab:BBvar3dol}Retorno máximo esperado de um dia para o outro, para um investimento de \\$1000 em ações da BB.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)


PORTO_VaR[-1] %>%
  map_df(~round(exp(.x)*1000), 0) %>%
  mutate("Confianca" = paste0(p*100,"%"), .before = "VaR Historico") %>%
  rename_all( ~ c("Confiança", "VaR Historico($)", "VaR Gaussiano($)", "VaR Alfa-Estável($)")) %>%
  kbl(booktabs = T, caption = "\\label{tab:PORTOvar3dol}Retorno máximo esperado de um dia para o outro, para um investimento de \\$1000 em ações da PORTO.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)





PORTO_Bmax <- Bloco_maximo(data = PORTO, values = "Retorno")
PORTO_n <- PORTO_Bmax$n
PORTO_n <- 17
PORTO_tab <- PORTO_Bmax$Teste %>%
  summarise(Tamanho = Tamanho,
            P.valor = round(P.valor, 2)) %>%
  as_tibble()

PORTO_ts <- PORTO_Bmax$Serie %>%
  dplyr::select(c("Date", "Retorno"))


BB_Bmax <- Bloco_maximo(data = BB, values = "Retorno", force = 17)
BB_n <- BB_Bmax$n
BB_n <- 17
BB_tab <- BB_Bmax$Teste %>%
  summarise(Tamanho = Tamanho,
            P.valor = round(P.valor, 2)) %>%
  as_tibble()

BB_ts <- BB_Bmax$Serie %>%
  dplyr::select(c("Date", "Retorno"))

save(PORTO_Bmax, file = 'porto_B_max_prontos.rdata')
save(BB_Bmax, file = 'BB_B_max_prontos.rdata')
load()



kbl(BB_tab[(BB_n-5):(BB_n+5),], booktabs = T,
    caption = "P-valores do teste de Ljung-Box para diferentes tamanhos de
     bloco máximo dos retornos das ações da BB") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

kbl(PORTO_tab[(PORTO_n-5):(PORTO_n+5),], booktabs = T,
    caption = "P-valores do teste de Ljung-Box para diferentes tamanhos de
     bloco máximo dos retornos das ações da PORTO") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)


ggarrange(Hist_Fit(data = BB_ts, values = "Retorno", fits = "self", bins = 17),
          Serie(data = BB_ts, col_x = "Date", col_y = "Retorno", 2),
          nrow = 1, ncol = 2)
ggsave ("imagens/hist_retorno_max_bbse3.png", width = 6.5, height = 4)

ggarrange(Hist_Fit(data = PORTO_ts, values = "Retorno", fits = "self", bins = 17),
          Serie(data = PORTO_ts, col_x = "Date", col_y = "Retorno", 2),
          nrow = 1, ncol = 2)
ggsave ("imagens/hist_retorno_max_porto.png", width = 6.5, height = 4)



## Qq plot
BB_q <- c((1:BB_n)/(BB_n+1))
BB_ginv <- -log(-log(BB_q))
qqplot(BB_ts$Retorno, BB_ginv, xlab="Quantil empírico",ylab="Quantil da Fréchet",main="")
ggsave ("imagens/quantis_empiricos_bbse3.png", width = 6, height = 4)
grid()

PORTO_q <- c((1:PORTO_n)/(PORTO_n+1))
PORTO_ginv <- -log(-log(PORTO_q))
qqplot(PORTO_ts$Retorno, PORTO_ginv, xlab="Quantil empírico",ylab="Quantil da Gumbel",main="")
ggsave ("imagens/quantis_empiricos_porto.png", width = 6, height = 4)
grid()

## R MLE - GEV

BB_gevfit1 <- gevFit(BB_ts$Retorno, type ="mle")
BB_gevfit2 <- gevFit(BB_ts$Retorno, type ="pwm")

PORTO_gevfit1 <- gevFit(PORTO_ts$Retorno, type ="mle")
PORTO_gevfit2 <- gevFit(PORTO_ts$Retorno, type ="pwm")

BB_gevmle <- BB_gevfit1@fit[['par.ests']]
BB_gevpwm <- BB_gevfit2@fit[['par.ests']]

PORTO_gevmle <- PORTO_gevfit1@fit[['par.ests']]
PORTO_gevpwm <- PORTO_gevfit2@fit[['par.ests']]

Hist_Fit(data = BB_ts, values = 'Retorno', bins = 10,
         fits = c("gevmle", "gevpwm"), fits_param = list("gevmle" = BB_gevmle,
                                                         "gevpwm" = BB_gevpwm))

Hist_Fit(data = PORTO_ts, values = 'Retorno', bins = 10,
         fits = c("gevmle", "gevpwm"), fits_param = list("gevmle" = PORTO_gevmle,
                                                         "gevpwm" = PORTO_gevpwm))

par(mfrow = c(2, 2))
summary(BB_gevfit1)
summary(BB_gevfit2)
summary(PORTO_gevfit1)
summary(PORTO_gevfit2)


par(mfrow=c(1,1))
BB_fit <- fevd(BB_ts$Retorno, type="GEV")
pars <- data.frame("Locacao" = BB_fit$results$par['location'],
                   "Escala" = BB_fit$results$par['scale'],
                   "Forma" = BB_fit$results$par['shape'])
row.names(pars) <- ""
kbl(pars, booktabs = T,
    caption = "Parâmetros estimados para o ajuste de uma distribuição GEV para dados BB15.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)


PORTO_fit <- fevd(PORTO_ts$Retorno, type="GEV")
pars <- data.frame("Locacao" = PORTO_fit$results$par['location'],
                   "Escala" = PORTO_fit$results$par['scale'],
                   "Forma" = PORTO_fit$results$par['shape'])
row.names(pars) <- ""
kbl(pars, booktabs = T,
    caption = "Parâmetros estimados para o ajuste de uma distribuição GEV para dados PORTO57.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)


BB_fit$call <- ""
plot(BB_fit)

PORTO_fit$call <- ""
plot(PORTO_fit)


ci(BB_fit,type="parameter")
ci(PORTO_fit,type="parameter")


years <- c(2,5,10,20)

return.level(BB_fit, years, do.ci = TRUE)

return.level(PORTO_fit, years, do.ci = TRUE)


BB_Full_ret <- Retorno(BB_fit, years)
BB_Full_ret[-1] %>%
  map_df(round, 4) %>%
  mutate(Tempo = paste(years, "Anos", sep = " "), .before = names(BB_Full_ret)[2]) %>%
  kbl(booktabs = T, caption="\\label{tab:retBB}Intervalos de confiança para os retornos esperados da BB15.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)

PORTO_Full_ret <- Retorno(PORTO_fit, years)
PORTO_Full_ret[-1] %>%
  map_df(round, 4) %>%
  mutate(Tempo = paste(years, "Anos", sep = " "), .before = names(PORTO_Full_ret)[2]) %>%
  kbl(booktabs = T, caption="\\label{tab:retPORTO}Intervalos de confiança para os retornos esperados da PORTO57.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)




BB_Full_ret[-1] %>%
  map_df(~round(exp(.x)*1000), 0) %>%
  mutate(Tempo = paste(years, "Anos", sep = " "), .before = names(BB_Full_ret)[2]) %>%
  rename_all( ~ names(BB_Full_ret)) %>%
  kbl(booktabs = T, caption = "\\label{tab:retBBdol}Retorno máximo para um investimento de \\$1000 em ações da BB15.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)


PORTO_Full_ret[-1] %>%
  map_df(~round(exp(.x)*1000), 0) %>%
  mutate(Tempo = paste(years, "Anos", sep = " "), .before = names(PORTO_Full_ret)[2]) %>%
  rename_all( ~ names(PORTO_Full_ret)) %>%
  kbl(booktabs = T, caption = "\\label{tab:retPORTOdol}Retorno máximo para um investimento de \\$1000 em ações da PORTO30.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)