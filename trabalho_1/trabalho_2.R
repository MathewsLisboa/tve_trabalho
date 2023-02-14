teste.kendall <- cor.test(BB$Retorno, PORTO$Retorno, method = "kendall")


ggplot() +
  geom_point(mapping = aes(x = BB$High, y = PORTO$High), size = 2) +
  theme_bw() + xlab("SSG15") + ylab("APL15")

BB_gevfit1 <- gevFit(BB_ts$Retorno, type ="mle")
PORTO_gevfit1 <- gevFit(PORTO_ts$Retorno, type ="mle")

BB_fit <- fevd(BB_ts$Retorno, type="GEV")
PORTO_fit <- fevd(PORTO_ts$Retorno, type="GEV")

#years <- c(5,10,20,100,1000)


BB_par <- as.list(BB_fit$results$par)
PORTO_par <- as.list(PORTO_fit$results$par)

names(BB_par) <- formalArgs(qgev)[-1]
names(PORTO_par) <- formalArgs(qgev)[-1]

a.0 <- sin(teste.kendall$estimate * pi/2)

udat<- cbind(
  do.call(pgev, c(list("x" = BB_ts$Retorno), BB_par)),
  do.call(pgev, c(list("x" = PORTO_ts$Retorno), PORTO_par))
)

clayton <- claytonCopula(dim=2)
gumbel <- gumbelCopula(dim=2)
frank <- frankCopula(dim=2)


fit.cl <- fitCopula(clayton, udat, start=a.0)
fit.gu <- fitCopula(gumbel, udat, start=a.0)
fit.fr <- fitCopula(frank,udat, start=a.0)

# AIC e BIC
# 1 parametro livre para AIC e BIC (theta)
n <- nrow(udat)
V <- 1
aicCl <- -2*fit.cl@loglik + 2*V
bicCl <- -2*fit.cl@loglik + V*log(n)

aicGu <- -2*fit.gu@loglik + 2*V
bicGu <- -2*fit.gu@loglik + V*log(n)

aicFr <- -2*fit.fr@loglik + 2*V
bicFr <- -2*fit.fr@loglik + V*log(n)

aic <- c(aicCl, aicGu, aicFr)
bic <- c(bicCl, bicGu, bicFr)

daic <- aic - min(aic)
dbic <- bic - min(bic)

# % De cada modelo ser o melhor modelo
waic <- round(exp(-1/2 * daic) / sum( exp(-1/2 * daic) ) * 100, 2)
wbic <- round(exp(-1/2 * dbic) / sum( exp(-1/2 * dbic) ) * 100, 2)

modelos <- c("Clayton", "Gumbel", "Frank")
pars <- c(fit.cl@copula@parameters, fit.gu@copula@parameters, fit.fr@copula@parameters)
copula.ccopula.choosehoose <- tibble(
  "Copula" = modelos,
  "Parametros" = pars,
  "$AIC$" = aic,
  "$\\text{w}(AIC)$" = paste0(waic, "\\%"),
  "$BIC$" = bic,
  "$\\text{w}(BIC)$" = paste0(wbic, "\\%"),
)

xtable::xtable(copula.choose)



set.seed(13)

cc <- gumbelCopula(fit.gu@copula@parameters)
sample <- rCopula(131, cc)
lam.true <- lambda(cc)

sample_BB <- sample[,1]
sample_PORTO <- sample[,2]

x <- do.call(qgev, c(list("q" = sample_BB), BB_par))
y <- do.call(qgev, c(list("q" = sample_PORTO), PORTO_par))


plot(BB_ts$Retorno, PORTO_ts$Retorno, pch = "../Trabalho-1 ", cex = 5 , col="black", xlab="BBSE3" , ylab="PSSA3" )
points(x, y, ylab="Y", pch = "../Trabalho-1 ", cex = 5, col="#00FFFF")
legend("topright", legend=c("Dados Ajustados", "Dados reais"), col=c("#00FFFF", "black"), pch = 15)
ggsave ("imagens/scatter_plot.png", width = 6, height = 4)

ggplot()



colors <- c("Dados Reais" = "black", "Dados Simulados" = "#00FFFF")

ggplot() +
  stat_density_2d(mapping = aes(x = BB_ts$Retorno, y = PORTO_ts$Retorno, colour = "Dados Reais"), geom="polygon", fill="grey") +
  stat_density_2d(mapping = aes(x = x, y = y, colour = "Dados Simulados")) +
  scale_color_manual(values = colors) +
  guides(color = guide_legend(title = "Origem dos dados")) +
  theme_bw() + xlab("BBSE3") + ylab("PSSA3")
ggsave ("imagens/curvas_de_nivel.png", width = 6, height = 4)



alpha <- c(0.50, 0.95, 0.98, 0.99)
theta <- fit.gu@estimate
VaR.b <- data.frame("Confianca" =  paste0(alpha*100, "\\%"),
                    "BBSE3" = NA,
                    "PSSA3" = NA)

set.seed(13)

S <- runif(100, min=0, max=1 )

generator <- gen_gumbel(theta, alpha)
inversas <- map(generator, ~inv_gen_gumbel(theta, S*.x))
estimativas_BB <- map_dbl(inversas, ~mean( do.call(qgev, c(list("q" = .x), BB_par))) )
estimativas_PORTO <- map_dbl(inversas, ~mean( do.call(qgev, c(list("q" = .x), PORTO_par))) )

VaR.b$BBSE3 <- estimativas_BB
VaR.b$PSSA3 <- estimativas_PORTO

kbl(VaR.b, booktabs = T, caption = "\\label{tab:VaRb}VaR bivariado para ações da APL15 e SSG15.") %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = F)
xtable::xtable(VaR.b)

round(exp(estimativas_BB)-1,4)
round(exp(estimativas_PORTO)-1,4)


