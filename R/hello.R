library(yfR)
library(readr)
library(rugarch)
library(scales)

nome_acao <- "GOOG"   # Codigo no Yahoo Finance
data_ini  <- "2010-01-01" # Data de inicio
data_fim <- Sys.Date()
precos <- yf_get(tickers = nome_acao, first_date = data_ini, last_date = data_fim)
retorno <- as.data.frame(list(precos$ret_adjusted_prices[-1], precos$ref_date[-1]))
colnames(retorno) <- c("retorno","data")

spec <- ugarchspec(mean.model = list(armaOrder = c(2, 1), include.mean = FALSE),
                     variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                     distribution = 'norm')
  
previsao_antes <- read.csv("https://raw.githubusercontent.com/Joao-Formigari/Teste/master/data/previsao.csv")

if (nrow(previsao_antes) > 0){
  retorno[nrow(retorno)+1,1] <- previsao_antes[nrow(previsao_antes),1]
  retorno[nrow(retorno),2] <- as.Date(previsao_antes[nrow(previsao_antes),3])
  retorno[,3] <- as.Date(retorno$data)
  fit_01 <- ugarchfit(spec, retorno$retorno[-3391], solver = 'hybrid')
  
  previsao <- ugarchforecast(fit_01, retorno[-3391],
                             n.ahead = nrow(previsao_antes)+1)
  previsao_antes[,3]<-as.Date(previsao_antes[,3])
  previsao_antes[nrow(previsao_antes)+1,1] <- previsao@forecast[["seriesFor"]][nrow(previsao_antes)]
  previsao_antes[nrow(previsao_antes),2] <- previsao@forecast[["sigmaFor"]][nrow(previsao_antes)]
  previsao_antes[nrow(previsao_antes),3] <- retorno$data[nrow(retorno)]+1
}

if (nrow(previsao_antes) == 0){
  fit_01 <- ugarchfit(spec, retorno$retorno, solver = 'hybrid')
  
  previsao <- ugarchforecast(fit_01, retorno,
                             n.ahead = 1)
  previsao_antes[,3]<-as.Date(previsao_antes[,3])
  previsao_antes[nrow(previsao_antes)+1,1] <- previsao@forecast[["seriesFor"]]
  previsao_antes[nrow(previsao_antes),2] <- previsao@forecast[["sigmaFor"]]
  previsao_antes[nrow(previsao_antes),3] <- retorno$data[nrow(retorno)]+1
}

write_csv(previsao_antes,paste0('data/','previsao','.csv'))   
