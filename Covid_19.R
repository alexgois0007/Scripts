########## INCERTEZA ECONÔMICA - CORONAVÍRUS ###############################

getwd()
variáveis <- read.csv2("macro.csv", header = TRUE, sep = ";", dec = ',')

###### Instalação e Carregar pacotes #########################

install.packages("agricolae")
install.packages("fBasics")
install.packages("nortest")
install.packages("fUnitRoots")
install.packages("tseries")
install.packages("urca")
install.packages("DataCombine")
install.packages("dplyr")
install.packages("zoo")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("forecast")
install.packages("vars")
install.packages("tsDyn")
install.packages("data.table")
install.packages("seasonal")
install.packages("mnormt")

library(agricolae)
library(fBasics)
library(nortest)
library(fUnitRoots)
library(tseries)
library(urca)
library(DataCombine)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(forecast)
library(vars)
library(tsDyn)
library(data.table)
library(seasonal)
library(xts)

############## Dessazonalizar Série Vendas Varejo ###################
checkX13()

vend_var_ts <- ts(variáveis[,7]) 
vend_var <- as.xts(vend_var_ts)

ajuste_seasonal <- ts(vend_var, frequency = 12, start = c(2002,1))
head(ajuste_seasonal)
tail(ajuste_seasonal)
ajuste_seasonal


plot(vend_var, main = "Série Temporal: Vendas no Varejo", lwd = 2)

ajuste <- seas(x = ajuste_seasonal)
summary(ajuste)

#### Gráfico da Série ajustada #####
plot(ajuste, main = "Vendas no Varejo")

#### Gráfico dos fatores sazonais ####
monthplot(ajuste, col.base = 1, labels = month.abb)


#### Diagnóstico da Sazonalidade #####
qs(ajuste)

vend_var_ajust <- matrix(ajuste$series$s11)

dim(vend_var_ajust)

variáveis1 <- variáveis[,-1]
variaveis <- variáveis1[,-6]

variaveis_macro <- cbind(variaveis, vend_var_ajust)

colnames(variaveis_macro) <- c("ICC", "IPCA","TXCAMBIO", "SELIC", "PRO_IDT", "INCERTEZA", "VEND_VAR")

######## Teste de Dicker- Fuller e Phillipe Perron #####################

adf_test_list <- list()
for (i in 1:ncol(variaveis_macro)) {
  tmp <- adf.test(variaveis_macro[,i])
  p_value <- tmp$p.value
  adf_test_list[[i]] <- p_value
  
}




pp_test_list <- list()
for (i in 1:ncol(variaveis_macro)) {
  tmp <- pp.test(variaveis_macro[,i])
  p_value <- tmp$p.value
  pp_test_list[[i]] <- p_value
  
}

tmp

tmp$p.value

pp_test_list

idx_estacionarias <- unlist(adf_test_list) <= 0.10
idx_não_estacionarias <- unlist(adf_test_list) > 0.10

predict_macro_estacionarias <- variaveis_macro[,idx_estacionarias]
predict_macro_não_estacionarias <- variaveis_macro[,idx_não_estacionarias]


head(predict_macro_estacionarias)
head(predict_macro_não_estacionarias)


############# Teste de 1º diferença das variáveis macroeconômicas - Estacionária/ Não estacionária #####

predict_macro_não_estacionarias_1 <- as.matrix(predict_macro_não_estacionarias)
predict_não_estac_diff <- diff(predict_macro_não_estacionarias_1)



adf_test_list <- list()
for (i in 1:ncol(predict_não_estac_diff)) {
  tmp <- adf.test(predict_não_estac_diff[,i])
  p_value <- tmp$p.value
  adf_test_list[[i]] <- p_value
  
}
adf_test_list


pp_test_list <- list()
for (i in 1:ncol(variaveis_macro)) {
  tmp <- pp.test(variaveis_macro[,i])
  p_value <- tmp$p.value
  pp_test_list[[i]] <- p_value
  
}

tmp

tmp$p.value

pp_test_list





predict_macro_estacionarias_2 <- predict_macro_estacionarias[-1,]

macro_2 <- cbind(predict_não_estac_diff,predict_macro_estacionarias_2)
head(macro_2)
tail(macro_2)



adf_test_list <- list()
for (i in 1:ncol(macro_2)) {
  tmp <- adf.test(macro_2[,i])
  p_value <- tmp$p.value
  adf_test_list[[i]] <- p_value
  
}
adf_test_list


pp_test_list <- list()
for (i in 1:ncol(macro_2)) {
  tmp <- pp.test(macro_2[,i])
  p_value <- tmp$p.value
  pp_test_list[[i]] <- p_value
  
}

tmp

tmp$p.value

pp_test_list


INCERTEZA_ts <- ts(macro_2[,7])
INCERTEZA_xts <- as.xts(INCERTEZA_ts)
index(INCERTEZA_xts) = dates
plot(INCERTEZA_xts, main = "Índice de Incerteza Econômica EPU - Brasil", ylab = "Índice", xlab = "")

dates <- seq.Date(from = as.Date('2002-02-01'), to = as.Date('2020-04-01'), by = 
                    "months")

ICC_ts <- ts(macro_2[,1])
TXCAMBIO_ts <- ts(macro_2[,2])
SELIC_ts <- ts(macro_2[,3])
PRO_IDT_ts <- ts(macro_2[,4])
VEND_VAR_ts <- ts(macro_2[,5])
IPCA_ts <- ts(macro_2[,6])

par(mfrow = c(3,3))
plot(ICC_ts, main = "Índice de Confiança do Consumidor", ylab = "%", xlab = "")
plot(TXCAMBIO_ts, main = "Taxa de Câmbio", ylab = "%", xlab = "")
plot(SELIC_ts, main = "Selic", ylab = "%", xlab = "")
plot(PRO_IDT_ts, main = "Produção Industrial", ylab = "%", xlab = "")
plot(VEND_VAR_ts, main = "Vendas no Varejo", ylab = "%", xlab = "")
plot(IPCA_ts, main = "IPCA", ylab = "%", xlab = "")


################ #VETORES AUTOREGRESSIVOS ########

# PRIMEIRO PASSO
# Selecionar lag do var (critério)
VARselect(macro_2, lag.max = 10, type = "both") # ver o mínimo dentre os critérios

modelo_var <- vars::VAR(macro_2, p = 1)
summary(modelo_var)


# Estimação
testvar = VAR(modelo_var, p = 1)
summary(testvar)

Phi(testvar, nstep = 1)
Psi(testvar, nstep = 1)

plot(testvar)


ser11 = serial.test(modelo_var, lags.pt = 20, type = "PT.asymptotic") # autocorrelçaõ do resíduo # H0 não autocorrelação
ser11$serial

norm1=normality.test(testvar) # NORMALIDADE DO RESÍDUO
norm1$jb.mul

arch1 = arch.test(testvar, lags.multi = 5) # HETEROCEDASTICIDADE
arch1$arch.mul

plot(arch1)
plot(stability(testvar), nc = 2)

# Teste de causalidade de Granger H0: Não granger causa
teste <- grangertest(`INCERTEZA` ~ `ICC`, order = 1, data = macro_2)
teste_1 <- grangertest(`ICC` ~ `INCERTEZA`, order = 1, data = macro_2)

teste_2 <- grangertest(`INCERTEZA` ~ `TXCAMBIO`, order = 1, data = macro_2)
teste_3 <- grangertest(`TXCAMBIO` ~ `INCERTEZA`, order = 1, data = macro_2)

teste_4 <- grangertest(`INCERTEZA` ~ `SELIC`, order = 1, data = macro_2)
teste_5 <- grangertest(`SELIC` ~ `INCERTEZA`, order = 1, data = macro_2)

teste_6 <- grangertest(`INCERTEZA` ~ `PRO_IDT`, order = 1, data = macro_2)
teste_7 <- grangertest(`PRO_IDT` ~ `INCERTEZA`, order = 1, data = macro_2)

teste_8 <- grangertest(`INCERTEZA` ~ `VEND_VAR`, order = 1, data = macro_2)
teste_9 <- grangertest(`VEND_VAR` ~ `INCERTEZA`, order = 1, data = macro_2)

teste_10 <- grangertest(`INCERTEZA` ~ `IPCA`, order = 1, data = macro_2)
teste_11 <- grangertest(`IPCA` ~ `INCERTEZA`, order = 1, data = macro_2)



# Impulso resposta (choques)
chqtestvar = irf(testvar,impulse = "INCERTEZA", response = "ICC", n.ahead = 25, boot = T)
chqtestvar1 = irf(testvar,impulse = "ICC", response = "INCERTEZA", n.ahead = 25, boot = T)

chqtestvar1

plot(chqtestvar, main = "Choque de Incerteza", ylab = "Índice de Confiança do Consumidor", xlab = "")
plot(chqtestvar1, main = "Choque de ICC", ylab = "Incerteza", xlab = "")


chqtestvar2 = irf(testvar,impulse = "INCERTEZA", response = "TXCAMBIO", n.ahead = 25, boot = T)
chqtestvar3 = irf(testvar,impulse = "TXCAMBIO", response = "INCERTEZA", n.ahead = 25, boot = T)
chqtestvar2

plot(chqtestvar2, main = "Choque de Incerteza", ylab = "Taxa de Câmbio", xlab = "")
plot(chqtestvar3, main = "Choque na Taxa de Câmbio", ylab = "Incerteza", xlab = "")


chqtestvar4 = irf(testvar,impulse = "INCERTEZA", response = "SELIC", n.ahead = 25, boot = T)
chqtestvar5 = irf(testvar,impulse = "SELIC", response = "INCERTEZA", n.ahead = 25, boot = T)
chqtestvar4

plot(chqtestvar4, main = "Choque de Incerteza", ylab = "Selic", xlab = "")
plot(chqtestvar5, main = "Choque na Selic", ylab = "Incerteza", xlab = "")


chqtestvar6 = irf(testvar,impulse = "INCERTEZA", response = "PRO_IDT", n.ahead = 25, boot = T)
chqtestvar7 = irf(testvar,impulse = "PRO_IDT", response = "INCERTEZA", n.ahead = 25, boot = T)
chqtestvar7

plot(chqtestvar6, main = "Choque de Incerteza ", ylab = "Produção Industrial", xlab = "")
plot(chqtestvar7, main = "Choque na Produção Industrial ", ylab = "Incerteza", xlab = "")


chqtestvar8 = irf(testvar,impulse = "INCERTEZA", response = "VEND_VAR", n.ahead = 25, boot = T)
chqtestvar9 = irf(testvar,impulse = "VEND_VAR", response = "INCERTEZA", n.ahead = 25, boot = T)
chqtestvar9

plot(chqtestvar8, main = "Choque de Incerteza", ylab = "Vendas no Varejo", xlab = "")
plot(chqtestvar9, main = "Choque nas Vendas no Varejo", ylab = "Incerteza", xlab = "")


chqtestvar10 = irf(testvar,impulse = "INCERTEZA", response = "IPCA", n.ahead = 25, boot = T)
chqtestvar11 = irf(testvar,impulse = "IPCA", response = "INCERTEZA", n.ahead = 25, boot = T)
chqtestvar11

plot(chqtestvar10, main = "Choque de Incerteza", ylab = "IPCA", xlab = "")
plot(chqtestvar11, main = "Choque no IPCA", ylab = "Incerteza", xlab = "")


# Decomposição da variância
dectestvar <- fevd(testvar, n.ahead = 12)
dectestvar
plot(dectestvar, legend = T)

# previsão
predtestvar = predict(testvar, n.ahead = 10, ci = 0.95)
predtestvar
plot(predtestvar)



# cointegração
cotestvar2 = ca.jo(macro_2, type = "trace", ecdet = "trend", K = 8, spec = "longrun")
summary(cotestvar2)


# previsão com cointegração

library(tsDyn)

vectestvar2 = VECM(macro_2, lag = 1, estim ="ML", r=2)
summary(vectestvar2)
plot(vectestvar2)

# Impulso resposta
irtestvar2 = irf(vectestvar2, n.ahead = 30, boot = T)
irtestvar2
plot(irtestvar2)

# decomposição da varincia
dectestvar2 = fevd(vectestvar2, n.ahead = 30); dectestvar2
plot(dectestvar2)


############# Tabelas ##############################

Teste_dick <- rbind,adf_test_list[[2]],adf_test_list[[3]],adf_test_list[[4]],adf_test_list[[5]],adf_test_list[[6]],adf_test_list[[7]])
Teste_perron <- rbind(pp_test_list[[1]],pp_test_list[[2]], pp_test_list[[3]], pp_test_list[[4]], pp_test_list[[5]], pp_test_list[[6]], pp_test_list[[7]])
                                                                                                                                                                       
Teste_dick                    
Teste_perron

names <- c("Índice de Confiança do Consumimdor", "Taxa de Câmbio","Selic","Produção Industrial","Vendas no Varejo", "IPCA",
               "Índice de Incerteza")
teste_geral<- cbind.data.frame(names,Teste_dick, Teste_perron)
teste_geral

colnames(teste_geral) <- c("", "P-valor da estatística do Teste","P-valor da estatística do Teste")

teste_geral
install.packages("xtable")
library(xtable)

xtable(teste_geral)


teste_granger <- rbind(teste_9)
head(teste_9)

names <- c("Vendas no Varejo não granger-causa Produção Industrial")
teste_geral_granger<- cbind.data.frame(names,teste_granger)
teste_geral_granger


colnames(teste_geral) <- c("Confiança do Consumidor", "Taxa de Câmbio", "Selic", "Produção Industrial", "Vendas no Varejo", "IPCA")

teste_geral
install.packages("xtable")
library(xtable)

xtable(teste_geral_granger)





testes <- rbind(ser11$serial,norm1$jb.mul,arch1$arch.mul)
head(decom_var)

names <- c("1", "2","6","12")
decom_var_geral<- cbind.data.frame(names,decom_var)
decom_var_geral


colnames(teste_geral) <- c("Confiança do Consumidor", "Taxa de Câmbio", "Selic", "Produção Industrial", "Vendas no Varejo", "IPCA")

decom_var_geral
install.packages("xtable")
library(xtable)

xtable(decom_var_geral)

