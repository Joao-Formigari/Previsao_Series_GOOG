library(yfR)
library(readr)
library(rugarch)

nome_acao <- "GOOG"   # Codigo no Yahoo Finance
data_ini  <- "2010-01-01" # Data de inicio
data_fim  <- "2023-06-20" # Data de fim
precos <- yf_get(tickers = nome_acao, first_date = data_ini, last_date = data_fim)
fechamento <- precos$price_adjusted

spec <- ugarchspec(mean.model = list(armaOrder = c(3, 1), include.mean = FALSE),
                     variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                     distribution = 'norm')
  
fit_01 <- ugarchfit(spec, fechamento, solver = 'hybrid')
  
previsao <- ugarchforecast(fit_01, fechamento, n.ahead = 1)

write_csv(as.data.frame(list("previsao"=previsao@forecast[["seriesFor"]],
                             "sigma"=previsao@forecast[["sigmaFor"]])),paste0('data/',Sys.Date(),'previsao','.csv'))    

