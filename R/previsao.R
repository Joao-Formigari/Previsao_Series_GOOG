library(yfR)
library(readr)
library(rugarch)
library(scales)

nome_acao <- "GOOG"   # Codigo no Yahoo Finance
data_ini  <- "2010-01-01" # Data de inicio
data_fim <- Sys.Date()
precos <- yf_get(tickers = nome_acao, first_date = data_ini, last_date = data_fim)
retornos <- as.data.frame(list(precos$ret_adjusted_prices[-1], precos$ref_date[-1]))
colnames(retornos) <- c("retornos","data")

spec <- ugarchspec(mean.model = list(armaOrder = c(2, 1), include.mean = FALSE),
                   variance.model = list(model = 'sGARCH', garchOrder = c(1, 1)),
                   distribution = 'std')

previsao_antes <- read.csv("https://raw.githubusercontent.com/Joao-Formigari/Teste/master/data/previsao.csv")

if (nrow(previsao_antes) > 0){
  retornos[nrow(retornos)+1,1] <- previsao_antes[nrow(previsao_antes),1]
  retornos[nrow(retornos),2] <- as.Date(previsao_antes[nrow(previsao_antes),3])
  retornos[,3] <- as.Date(retornos$data)
  fit_01 <- ugarchfit(spec, retornos$retornos, solver = 'hybrid')
  
  previsao <- ugarchforecast(fit_01, retornos,
                             n.ahead = 1)
  previsao_antes[,3]<-as.Date(previsao_antes[,3])
  previsao_antes[nrow(previsao_antes)+1,1] <- previsao@forecast[["seriesFor"]]
  previsao_antes[nrow(previsao_antes),2] <- previsao@forecast[["sigmaFor"]]
  previsao_antes[nrow(previsao_antes),3] <- retornos$data[nrow(retornos)]+1
}

if (nrow(previsao_antes) == 0){
  fit_01 <- ugarchfit(spec, retornos$retornos, solver = 'hybrid')
  
  previsao <- ugarchforecast(fit_01, retornos,
                             n.ahead = 1)
  previsao_antes[,3]<-as.Date(previsao_antes[,3])
  previsao_antes[nrow(previsao_antes)+1,1] <- previsao@forecast[["seriesFor"]]
  previsao_antes[nrow(previsao_antes),2] <- previsao@forecast[["sigmaFor"]]
  previsao_antes[nrow(previsao_antes),3] <- retornos$data[nrow(retornos)]+1
}

write_csv(previsao_antes,paste0('data/','previsao','.csv'))   
