### Arquivo contendo todas as funções customizadas utilizadas nos trabalhos 1 e 2
pacman::p_load('dplyr', 'purrr', 'ggplot2', 'stringr', 'stabledist', 'fExtremes', 'RColorBrewer')

tab_exp <- function (arr, decimals=4) {
  data.frame(
    "Media" = mean(arr),
    "Variancia" = var(arr),
    "Desvio.Padrao" = sqrt(var(arr)),
    "Minimo" = range(arr)[1],
    "Maximo" = range(arr)[2],
    "Prim.Quantil" = quantile(arr, 0.25),
    "Seg.Quantil" = quantile(arr, 0.50),
    "Ter.Quantil" = quantile(arr, 0.75)) %>%
    map_df(round, decimals)
}


Serie <- function (data, col_x, col_y, dodge=1) {
  plot <- ggplot(data) +
    geom_line(aes(x = as.Date(.data[[col_x]]), y = .data[[col_y]]), size=1) +
    xlab("Tempo") + ylab("Maximo valor diario alcançado") +
    scale_x_date(date_labels = "%b-%Y", breaks = seq(ano_inicio, ano_fim, 365),
                 guide = guide_axis(n.dodge = dodge)) +
    theme_bw()
  return(plot)
}


Hist_Fit <- function (data, values, fits=c(), fits_param=c(), bins=30) {
  values <- data[[values]]
  fits <- map_chr(fits, str_to_lower)
  
  
  plot <- ggplot() +
    geom_histogram(aes(x = values, y = ..density..),
                   fill = "steelblue", color = "black",
                   bins = bins) +
    xlab("Log-retorno diario") + ylab("Densidade") +
    theme_bw()
  
  if ('self' %in% fits) {
    plot <- plot + geom_density(aes(values), color="turquoise3",
                                size=1.1)
    fits <- fits[!('self' == fits)]
  }
  if (length(fits)) {
    x <- seq(min(values), max(values), length.out = 1000)
    fits_df <- data.frame(matrix(ncol = 0, nrow = 0))
    labels <- data.frame(
      "gaussian" = "Normal",
      "stable" = "Alfa-Estavel",
      "gev" = "GEV",
      "gevpwm" = "GEV(PWM)",
      "gevmle" = "GEV(MLE)"
    )
    for (fit in fits) {
      if (str_detect(fit,'gaussian')) {
        if (!length(fits_param[[fit]])) {
          fits_param[[fit]] <- c('mean' = mean(values), 'sd' = sd(values))
        }
        y <- map_dbl(x, dnorm, fits_param[[fit]][['mean']], fits_param[[fit]][['sd']])
        
      } else if (str_detect(fit,'stable')) {
        if (!length(fits_param[[fit]])) {
          fits_param[[fit]] <- c('alpha' = 2, 'beta' = 0.5, 'gamma' = 1, 'delta' = 0)
        }
        y <- map_dbl(x, dstable, fits_param[[fit]][['alpha']], fits_param[[fit]][['beta']],
                     fits_param[[fit]][['gamma']], fits_param[[fit]][['delta']])
        
      }else if (str_detect(fit,'gev')) {
        if (!length(fits_param[[fit]])) {
          fits_param[[fit]] <- c('xi' = 1, 'mu' = 0, 'beta' = 1)
        }
        y <- map_dbl(x, fExtremes::dgev, fits_param[[fit]][['xi']],
                     fits_param[[fit]][['mu']], fits_param[[fit]][['beta']])
      }
      fit_df <- data.frame(
        "X" = x,
        "Y" = y,
        "Ajuste" = labels[[fit]]
      )
      fits_df <- rbind(fits_df, fit_df)
    }
    colors <- brewer.pal(max(3, length(fits)+1), 'Set1')[-2]
    plot <- plot + geom_line(data = fits_df, aes(x=X, y=Y, group=Ajuste, color=Ajuste), size=1.1) +
      scale_color_manual(values = colors)
  }
  
  return(plot)
}


Bloco_maximo <- function (data, values, x_axis="Date", conf = 0.05, force=0) {
  section <- function (n, data, values) {
    tau <- floor(nrow(data) / n)
    maximos <- data.frame(matrix(nrow=tau, ncol = length(data)))
    inicio <- 1
    for (i in 1:tau) {
      ## Pega-se o maior valor dado um tamanho de bloco
      ## Exemplo, se n = 3, entao pega-se
      ## as observacoes de 1 a 3 (incluso 3), de 4 a 6 (incluso 6), ...
      
      maximos[i,] <-  dplyr::filter(data[inicio: (inicio + n - 1),],
                                    eval(parse(text = values)) == max(data[[values]][inicio: (inicio + n - 1)]))[1,]
      # Atualiza o indice inicial do bloco
      inicio <- inicio + n
    }
    names(maximos) <- names(data)
    return(maximos)
  }
  
  data[[x_axis]] <- as.character(data[[x_axis]])
  # Data frame que acumula os testes para cada tamanho de bloco
  ans_tab <- data.frame(matrix(nrow = 0, ncol = 2))
  
  # n trata-se dos tamanhos de bloco
  for (n in 1:60) {
    maximos <- section(n, data = data, values = values)
    
    ## Teste de Ljung-Box para series historicas
    teste <- Box.test(maximos[[values]], lag = 1, type = "Ljung-Box", fitdf = 0)
    teste <- c(n, teste$p.value)
    # Coloca-se o resultado em um dataframe com todos os p-valores
    # para comparacao futura
    ans_tab <- rbind(ans_tab, teste)}
  
  ans_tab <- ans_tab %>%
    rename_all( ~ c("Tamanho", "P.valor"))
  
  if (TRUE %in% (ans_tab$P.valor < conf)) {
    # Retorna apenas o indice subsequente ao último indice em que o teste rejeitou independencia
    n <- last(dplyr::filter(ans_tab, P.valor < conf))[[1]] + 1
  } else {
    # Retorna apenas o valor de n que fornece o maior p-valor e o bloco é maior que 7 (arbitrario)
    n <- dplyr::filter(ans_tab, P.valor == max(ans_tab$P.valor) & Tamanho > 7)[["Tamanho"]]
    message("Nao foi encontrado nenhum p-valor mais\nsignificativo que o nivel de confiança")
  }
  
  # Para forcar um certo tamanho de bloco e, entre outras coisas, manter os bancos com mesmo tamanho
  if (force > 0){
    force <- floor(force)
    Serie_temp <- section(force, data = data, values = values)
    ans <- list(force, ans_tab, Serie_temp)
  } else {
    Serie_temp <- section(n, data = data, values = values)
    ans <- list(n, ans_tab, Serie_temp)
  }
  names(ans) <- c("n", "Teste", "Serie")
  return(ans)
}


Retorno <- function (fit, years, investimento = 1000, nome = "Data") {
  n <- length(years)
  estimativa <- return.level(fit, years, do.ci = TRUE)[1:(n*3)]
  
  retorno_df <- data.frame(matrix(estimativa, ncol = 3, nrow = n)) %>%
    dplyr::select(X2) %>%
    map_df(~round(exp(.x)*investimento), 0) %>%
    map_df(~paste0("$", .x))
  
  confianca_df <- data.frame(matrix(ncol = n+1, nrow = 1)) %>%
    rename_all(~c("Fonte", paste0(round((1-1/years)*100, 2), "%")))
  confianca_df[1,] <- c(nome, retorno_df$X2)
  
  return(confianca_df)
}


### GEV -----------------------------------------------------------------------------
## FDA fornecida
pgev <- function(x, mu, sigma, xi) {
  if (xi != 0) {
    return( exp(-(1 + xi*(x-mu)/sigma)^(-1/xi)) )
  } else {
    return( exp(-exp(- (x-mu)/sigma)) )
  }
}

## Inversa da FDA (Quantil)
qgev <- function(q, mu, sigma, xi) {
  if (xi != 0) {
    return(mu - (sigma/xi) * ( 1 - (-log(q))^(-xi)) )
  } else {
    return(mu - sigma*log(-log(q))  )
  }
}

## Derivada da FDA (Densidade)
dgev <- function(x, mu, sigma, xi) {
  if (xi != 0) {
    return ( exp(-(1 + xi*(x-mu)/sigma)^(-1/xi)) *
               (1/xi*(1 + xi*(x-mu)/sigma)^(-1/xi-1) ) * xi/sigma )
  } else {
    return ( exp(-exp(- (x-mu)/sigma)) * (1/sigma)*(exp(- (x-mu)/sigma)) )
  }
}


### Copula -----------------------------------------------------------------------------
gen_gumbel <- function (theta, alpha) {
  vphi <- (-log(alpha))^(theta)
  return(vphi)
}
inv_gen_gumbel <- function (theta, t) {
  inv <- exp(-t^(1/theta))
  return(inv)
}