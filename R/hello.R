library(yfR)
library(readr)
library(rugarch)
library(scales)

nome_acao <- "GOOG"   # Codigo no Yahoo Finance
data_ini  <- "2010-01-01" # Data de inicio
data_fim <- "2023-06-23"
precos <- yf_get(tickers = nome_acao, first_date = data_ini, last_date = data_fim)
fechamento <- as.data.frame(list(precos$price_adjusted, precos$ref_date))

spec <- ugarchspec(mean.model = list(armaOrder = c(2, 1), include.mean = FALSE),
                     variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                     distribution = 'norm')
  
previsao_antes <- read.csv("https://raw.githubusercontent.com/Joao-Formigari/Teste/master/data/previsao.csv")

if (nrow(previsao_antes) > 0){
  fechamento[nrow(fechamento)+1,] <- previsao_antes[nrow(previsao_antes),c(1,3)]
  fechamento[,3] <- as.Date(fechamento$data)
  fit_01 <- ugarchfit(spec, fechamento$fechamento[-3391], solver = 'hybrid')
  
  previsao <- ugarchforecast(fit_01, fechamento[-3391],
                             n.ahead = nrow(previsao_antes))[nrow(previsao_antes),]
  previsao_antes[,3]<-as.Date(previsao_antes[,3])
  previsao_antes[nrow(previsao_antes)+1,1] <- previsao@forecast[["seriesFor"]]
  previsao_antes[nrow(previsao_antes),2] <- previsao@forecast[["sigmaFor"]]
  previsao_antes[nrow(previsao_antes),3] <- fechamento$data[nrow(fechamento)]+nrow(previsao_antes)-1
}

if (nrow(previsao_antes) == 0){
  fit_01 <- ugarchfit(spec, fechamento$fechamento, solver = 'hybrid')
  
  previsao <- ugarchforecast(fit_01, fechamento,
                             n.ahead = 1)
  previsao_antes[,3]<-as.Date(previsao_antes[,3])
  previsao_antes[nrow(previsao_antes)+1,1] <- previsao@forecast[["seriesFor"]]
  previsao_antes[nrow(previsao_antes),2] <- previsao@forecast[["sigmaFor"]]
  previsao_antes[nrow(previsao_antes),3] <- fechamento$data[nrow(fechamento)]+1
}

write_csv(previsao_antes,paste0('data/','previsao','.csv'))   
