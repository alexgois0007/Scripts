################################################################################

################### Instalação de Pacotes ######################################

install.packages("TextForecast")
install.packages("DoMC")
install.packages("glmnet")
install.packages("pracma")
install.packages("Quandl")
install.packages("gdata")
install.packages("mFilter")
install.packages("caret")
install.packages("stopwords")
install.packages("readxl")
install.packages("XLconnect")
install.packages("fread")
install.packages("base")
install.packages("tseries")
install.packages("caret")
install.packages("rlang")
install.packages("xtable")
install.packages("zoo")
install.packages("xts")
#################### Carregar Pacotes ########################################

library(TextForecast)
library(tseries)
library(forecast)
library(urca)
library(data.table)
library(fread)
library(gdata)
library(XLConnect)
library(readxl)
library(stopwords)
library(doMC)
library(glmnet)
library(caret)
library(rlang)
library(xtable)
registerDoMC(cores=4)
library(zoo)
library(xts)

##################################################################################

############### Contagem de Palavras e Colocações ################################

st_year <- 2002
end_year <- 2019
qt= paste0(sort(rep(seq(from=st_year,to=end_year,by=1),12)),
           c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12"))



news <- get_terms(corpus_dates = qt,
                  ntrms_words = 1000,
                  st = 0,
                  ntrms_collocation = 700,
                  path.name = pathname,
                  ngrams_number = 3,
                  min_freq = 7,
                  language= "portuguese"
)
save.image('/Users/Desktop/MESTRADO/Dados R')

######################### TF-IDF Function ########################################

tfidfsum1<- function(x) {
  xx=as.matrix(x)
  ndoc=nrow(x)
  
  II = xx>0
  II_sum = apply(II,2,FUN=sum)
  nct=II_sum
  idf=log(ndoc/nct)
  
  #xx_log=log(1+xx)
  
  tf=apply(xx,2,FUN=sum)
  
  xx_tfidf_sum=tf*idf
  xx_tfidf_sum[is.nan(xx_tfidf_sum)]=0
  
  return(xx_tfidf_sum)
}


##############################################################################

#################### Função Para Salvar as Imagens ###########################

save_image <- function(x){'C:/Users/Desktop/MESTRADO/ECONOMETRIA II/Imagens do R' 
  path_image <- 'C:/Users/Desktop/MESTRADO/ECONOMETRIA II/Imagens do R'
  file_image <- paste0(path_image,"Imagens_R",Sys.Date(),".Rdata")
  save.image(file_image)
  }


getwd()
setwd('/Users/Desktop/MESTRADO/ECONOMETRIA II/Dados de Séries Macroeconômicas')
#####################################################################################################################################

####### Calcular Taxas de Desemprego e de Crescimento - Produção Industrial #########################################################################


############ Taxa de Desemprego ##############################################
tx_desemp <- read.csv2('Emprego_Formal.csv', header = TRUE, sep = ";", dec = ',')
tx_desemp <- as.numeric(tx_desemp$Índice.do.Emprego.Formal)
###############################################################################

#################### Taxa de Crescimento - Produção Industrial ################
prod_idt <- read.csv2('producao_industrial.csv', header = TRUE, sep = ';', dec = ',')
d_prod_idt <- diff(log(as.numeric(prod_idt$Industria_geral)))
View(news)

################## Excluindo algumas notícias da base de Dados ###############
which(qt == '199901')
which(qt == '200203')

dates = qt
dates = dates[-(1:2)]


################### Combinando as variáveis de noticias #################
cbind(tx_desemp)
cbind(prod_idt)
delta_y = cbind(tx_desemp_g[2:nrow(tx_desemp_g),],d_prod_idt)


################### Os dados iniciam a partir de 2002 a 2019 ##############
as.character(tx_desemp)
dates <- as.character(tx_desemp$Data)
dates_new <- dates[which(dates == "2002.03"):which(dates == "2019.12")]

as.character(prod_idt)
dates <- as.character(prod_idt$Data)
dates_new <- dates[which(dates == "2002.03"):which(dates == "2019.12")]

inicial_news <- which(dates == "2002.03")
final_news <- which(dates == "2019.12")


inicial_news_delta_y <- which(delta_y == "2002.03")
final_news_delta_y <- which(delta_y == "2019.12")


###############################################################################


y <- tx_desemp[,2:ncol(tx_desemp)]
z <- prod_idt[,2:ncol(prod_idt)]



adf.test(delta_y[,2])
adf.test(diff(log(tx_desemp)))
kpss.test(diff(log(tx_desemp)))
kpss.test(delta_y[,2])
diff(log(tx_desemp))* 100


########## Uso do filtro_hp para remover o componente ciclico de uma série temporal ######
tx_desemp <- as.matrix(tx_desemp)
tx_desemp_g <- matrix(NA , ncol = ncol(tx_desemp) , nrow = nrow(tx_desemp))

for(t in Tini:nrow(tx_desemp_g) ){
  
  for(k in 1:ncol(tx_desemp_g) ){ 
    
    if(t == Tini){ 
      
      hp_filter <- mFilter::hpfilter(tx_desemp[1:t,k], type = "lambda", freq = 14400)
      tx_desemp_g[1:t,k] <- hp_filter$cycle[1:t]
      
      
      
    }else {
      
      hp_filter <- mFilter::hpfilter(tx_desemp[1:t,k], type = "lambda", freq = 14400)
      tx_desemp_g[t,k] <- hp_filter$cycle[t]
      
      
    }
    
    
  }
  
  }

adf.test(tx_desemp_g)
kpss.test(tx_desemp_g)



################################### Variável Destino  - Delta_y ####################################################################
delta_y <- diff(log(as.matrix(y)))
delta_y <- delta_y[inicial_news_delta_y:final_news_delta_y,]



################ Set training and test sample ####################################################################################
Tini <- 132
T <- (nrow(delta_y))
TF <- T - Tini
delta_y_actual <- delta_y[(132 + 1):T,]


################################################################################

###################### Teste de Estacionariedade ##############################


adf.y = ur.df(y,type  = 'trend', lags = 2)
adf.z = ur.df(z,type = 'trend', lags = 2)

adf.test(y)
adf.test(z)


################################################################################

###################### Teste de Diferença #####################################

x = diff(log(z))
adf.test(x)
delta_y = cbind(y[2:length(y)],x)


################################################################################

################### Classe e Números do Objeto #################################

class(y)
length(y)

class(x)
length(x)


##################################################################################

####################### Dados Textuais #########################################

z2 <- as.matrix(news[[1]][1:(nrow(news[[1]])),] )
dim(z2)

####################### Início e fim dos dados textuais ######################

inicial_news = which(qt == "2002m4") 
final_news = which(qt == "2019m12")
z2 = z2[inicial_news:final_news,]
dim(z2)

qt = qt[-(1:2)]
qt
length(qt)
    
##############################################################################

################## Modelo Usando Dados Textuais ##############################

delta_news <- matrix(NA, ncol = ncol(delta_y), nrow = TF)

grid_alphas <- seq(from = 0.01, to = 0.99 , by = 0.01)

Tini2 <- Tini
fll1_all <- 1700
kmax_fac <- 8
for (k in 1:ncol(delta_y)){

for(t in Tini:(T-1)){ 
set.seed(12345)
  
  
  if(t == Tini | t == Tini2 ){ 
    
    z <- z2
    if(t >= Tini && t < Tini2) { 
      Tini_tf = Tini
    } else { 
      Tini_tf = Tini2  
    }
    m_data <-  tfidfsum1(z[1:Tini_tf,])
    m_data_srt <- sort(m_data,decreasing=TRUE)
    ntrms <- fll1_all
    meanfilter <- m_data_srt[ntrms]
    II=m_data>=meanfilter
  }
  data_words1 <- as.matrix(z[,II])
  z1 <- data_words1
  print("ncol z")
  print(ncol(z1))

    
    yy <- delta_y[1:t, k]
    zz <-  z1[1:t,]
    
    if(t == Tini | t == Tini2 )
    alphas_optimals <- TextForecast::optimal_alphas(x = zz[1:(t-1),] , 
                                                    y = yy[2:t], 
                                                    grid_alphas = grid_alphas, 
                                                    cont_folds = TRUE,
                                                    family = "gaussian")
    #cv <- cv.glmnet(zz[1:(t-1),],yy[1:(t-1),])
    #eq <- glmnet(zz[1:(t-1),] ,yy[1:(t-1),] , family="gaussian", alpha = 0.5 ,lambda=cv$lambda.min)
    eq <- glmnet(zz[1:(t-1),] ,yy[2:t] , family="gaussian", alpha = alphas_optimals[1] ,lambda = alphas_optimals[2])
    #co <- coef(eq)[2:(ncol(zz)+1)]
    co <- coef(eq)[2:(ncol(zz)+1)]
    II2 <- abs(co)>0
    
    print(sum(II2))
    
    if (sum(II2)==0 | sum(II2) == 1)
    {
      cv=cv.glmnet(zz[1:(t-1),] ,yy[2:t]) 
      eq=glmnet(zz[1:(t-1),],yy[2:t],family="gaussian", alpha=0.15, lambda=cv$lambda[2])
      co=coef(eq)[2:(ncol(zz)+1)]
      
      II2=abs(co)>0
    }
    print("II2 new")
    print(sum(II2))
    

    sx <- as.matrix(subset(zz,select=II2))
    
    
    if (ncol(sx) > kmax_fac) {
      opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
      spc <- as.matrix(opt_fac[[2]])
    } else
    {
      opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
      spc <- as.matrix(opt_fac[[2]])
    }
    
    
    
    
    eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
    I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
    print("I")
    print(I)
    if (sum(I)==0){
      I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
    }
    
    sc <- as.matrix(spc[,I])
    
    eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),]))
    delta_news[(t - Tini + 1), k] <-  c(1,sc[t,])%*%eq2$coef[,1]
    print(t)
  }
  }


####################################################################################

########## Previsão para o modelo de Benchmark de passeio aleatório ############

delta_y_rw <- matrix(NA, ncol = ncol(delta_y), nrow = TF)
for(k in 1:ncol(delta_y)){ 
  
for(t in Tini:(T-1)){ 
  yy <- delta_y[2:t,k]
    eq <- summary( lm(yy ~ 1))
    #co <- coef(eq)[2:(ncol(zz)+1)]
    co <- eq$coef[,1]
    
    delta_y_rw[(t - Tini + 1), k] <- co
    
  }
  }


########### Erro Quadrado Médio da Raiz - delta_news #######################################



delta_y_actual = delta_y[(Tini + 1): T,]
mse_news = RMSE(delta_news[,1],delta_y_actual[,1])
mse_rw = RMSE(delta_y_rw[,1],delta_y_actual[,1])
mse_relativo_news_emprego = mse_news / mse_rw
print(mse_relativo_news_emprego)



mse_news = RMSE(delta_news[,2],delta_y_actual[,2])
mse_rw = RMSE(delta_y_rw[,2],delta_y_actual[,2])
mse_relativo_news_prod_idt = mse_news / mse_rw
print(mse_relativo_news_prod_idt)


delta_y_actual = as.matrix(delta_y_actual)
RMSE(delta_news[,1],delta_y_actual[,1]) 
RMSE(delta_news[,2],delta_y_actual[,2])


RMSE(delta_y_rw[,1],delta_y_actual[,1])
RMSE(delta_y_rw[,2],delta_y_actual[,2])

############## Calib usando Versão antiga ###################################

fll1_all <- seq(from = 500, to = 5000, by = 100)
MSE_ER2 <- matrix(NA,nrow=length(fll1_all), ncol = ncol(delta_y))
MSEs_ER2 <- list(ls())

fll_all <- seq(from = 0.01, to = 0.99, by = 0.01)

for(l in 1:length(fll_all)){ 
    
for (j in 1:length(fll1_all)) {
    
    
    
    delta_news <- matrix(NA, ncol = ncol(delta_y), nrow = TF) 
    grid_alphas <- seq(from = 0.01, to = 0.99 , by = 0.01)
    
    Tini2 <- Tini
    
    kmax_fac <- 8
    
    for(k in 1:ncol(delta_news)){ 
      
      for(t in Tini:(T-1)){ 
        set.seed(12345)
        
        
        if(t == Tini | t == Tini2 ){ 
          
          z <- z2
          if(t >= Tini && t < Tini2) { 
            Tini_tf = Tini
          } else { 
            Tini_tf = Tini2  
          }
          m_data <-  tfidfsum1(z[1:Tini_tf,])
          m_data_srt <- sort(m_data,decreasing=TRUE)
          ntrms <- fll1_all[j]
          meanfilter <- m_data_srt[ntrms]
          II=m_data>=meanfilter
        }
        data_words1 <- as.matrix(z[,II])
        z1 <- data_words1
        print("ncol z")
        print(ncol(z1))
        
        x <- delta_y[1:(t-1),k]
        xx <- delta_y[t,k]
        
        
        yy <- delta_y[1:t,k]
        zz <-  z1[1:t,]
        if(t == Tini | t == Tini2 ){ 
          
          cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
          eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = fll_all[l], lambda = cv$lambda.min)
          co=coef(eq)[2:(ncol(zz)+1)]
          II2 = abs(co)>0
          
          print(sum(II2))
          if (sum(II2)==0 | sum(II2) == 1)
          {
            cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
            eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
            co=coef(eq)[2:(ncol(zz)+1)]
            II2=abs(co)>0
          }
          print("II2 new")
          print(sum(II2))
          
        }
        
        sx <- as.matrix(subset(zz,select=II2))
        
        
        if (ncol(sx) > kmax_fac) {
          opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
          spc <- as.matrix(opt_fac[[2]])
        } else
        {
          opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
          spc <- as.matrix(opt_fac[[2]])
        }
        
        
        
        
        
        eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
        beta_eqq <- eqq$coef[2:nrow(eqq$coef),4] 
        beta_eqq[is.na(beta_eqq)] <- 0
        I  = beta_eqq < 0.01
        print("I")
        print(I)
        if (sum(I)==0){
          beta_eqq <- eqq$coef[2:nrow(eqq$coef),4]
          beta_eqq[is.na(beta_eqq)] <- 0
          I=rank(beta_eqq) < 2
        }
        
        sc <- as.matrix(spc[1:t,I])
        
        eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
        beta <- eq2$coef[,1]
        beta[is.na(beta)] <- 0 
        delta_news[(t - Tini + 1), k] <-  c(1,sc[t,],xx) %*% beta
        
        print(t)
        
        
      }
      print(k)
      }  
  
    
    MSE_ER_tmp <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
    
    
    for(i in 1:ncol(delta_y)){
      MSE_ER_tmp[,i] <- RMSE(delta_news[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
    }
    
    
    MSE_ER2[j,] <- MSE_ER_tmp
    print(MSE_ER_tmp)
    
    file_mse <- paste0('/Users/Desktop/MESTRADO/ECONOMETRIA II/Dados de Séries Macroeconômicas/','MSE_ER' ,as.character(fll_all[l] * 100) ,'.csv')
    write.csv(MSE_ER2, file = file_mse)
    
  }
  
  MSEs_ER2[[l]] <- MSE_ER2
  }





MSEs_ER2[[1]]
MSE_ER <- MSEs_ER2[[1]]


MSE_min_news <- list()
tf_idf_news <- list()

MSE_min <- list()
tf_idf_min <- list()

for (z in 1:length(fll_all)) {
  MSE_ER <- MSEs_ER2[[z]]
  
for(i in 1:ncol(delta_news)){ 
  
  
  
  MSE_min_news[[i]] <- min(MSE_ER[,i])
  idx <- which(MSE_ER[,i] == min(MSE_ER[,i]))
  tf_idf_news[[i]] <- fll1_all[idx[1]]

  }
MSE_min[[z]] <- MSE_min_news
tf_idf_min[[z]] <- tf_idf_news
}

g <- matrix(NA, nrow = length(MSE_min), ncol = ncol(delta_news))
unlist(
  MSE_min[[1]]
)

for (i in 1:nrow(g)) {
  g[i,] <- unlist(MSE_min[[i]])
}



g_tf <- matrix(NA, nrow = length(tf_idf_min), ncol = ncol(delta_news))
unlist(
  MSE_min[[1]]
)

for (i in 1:nrow(g_tf)) {
  g_tf[i,] <- unlist(tf_idf_min[[i]])
}


alpha_min <- list()
tf_min_1 <- list()
idx_min <- list()

for (i in 1:ncol(g)) {
  alpha_min[[i]] <- min(g[,i])
  idx <- which(g[,i] == min(g[,i])) 
  idx <- idx[1]
  tf_min_1[[i]] <- g_tf[idx,i]
  idx_min[[i]] <- idx
  
  }


############# Modelo final de dados textuais  #######################################################

delta_news <- matrix(NA, ncol = ncol(delta_y), nrow = TF) 

grid_alphas <- seq(from = 0.01, to = 0.99 , by = 0.01)

fll1_all <- seq(from = 500, to = 5000, by = 100)


tf_calibrado <- c(3600, 4700)

alphas_min <- c(0.74, 0.23)

Tini2 <- Tini
kmax_fac <- 8

for(k in 1:ncol(delta_news)){ 
  
  for(t in Tini:(T-1)){ 
    set.seed(12345)
    
    
    if(t == Tini | t == Tini2 ){ 
      
      z <- z2
      if(t >= Tini && t < Tini2) { 
        Tini_tf = Tini
      } else { 
        Tini_tf = Tini2  
      }
      m_data <-  tfidfsum1(z[1:Tini_tf,])
      m_data_srt <- sort(m_data,decreasing=TRUE)
      ntrms <- tf_calibrado[k]
      meanfilter <- m_data_srt[ntrms]
      II=m_data>=meanfilter
    }
    data_words1 <- as.matrix(z[,II])
    z1 <- data_words1
    print("ncol z")
    print(ncol(z1))
    
    x <- delta_y[1:(t-1),k]
    xx <- delta_y[t,k]
    
    
    yy <- delta_y[1:t,k]
    zz <-  z1[1:t,]
    if(t == Tini | t == Tini2 ){ 
      
      cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
      eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_min[k], lambda = cv$lambda.min)
      co=coef(eq)[2:(ncol(zz)+1)]
      II2 = abs(co)>0
      
      print(sum(II2))
      if (sum(II2)==0 | sum(II2) == 1)
      {
        cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
        eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
        co=coef(eq)[2:(ncol(zz)+1)]
        II2=abs(co)>0
      }
      print("II2 new")
      print(sum(II2))
      
    }
    
    sx <- as.matrix(subset(zz,select=II2))
    
    
    if (ncol(sx) > kmax_fac) {
      opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
      spc <- as.matrix(opt_fac[[2]])
    } else
    {
      opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
      spc <- as.matrix(opt_fac[[2]])
    }
    
    
    
    
    
    eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
    I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
    print("I")
    print(I)
    if (sum(I)==0){
      I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
    }
    
    sc <- as.matrix(spc[,I])
    
    eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
    delta_news[(t - Tini + 1), k] <-  c(1,sc[t,],xx)%*%eq2$coef[,1]
    
    print(t)
    
    
  }
  print(k)
}  


MSE_ER_news <- matrix(NA,nrow = 1, ncol = ncol(delta_y))

for(i in 1:ncol(delta_y)){
  MSE_ER_news[,i] <- RMSE(delta_news[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
}








save_image()




############## Modelo AR1 ###############################################


delta_y_ar1 <- matrix(NA, ncol = ncol(delta_y), nrow = TF)
for(k in 1:ncol(delta_y)){ 
  
  for(t in Tini:(T-1)){ 
    x <- delta_y[1:(t-1),k]
    xx <- delta_y[t,k]
    yy <- delta_y[2:t,k]
    eq <- summary( lm(yy ~ x))
    #co <- coef(eq)[2:(ncol(zz)+1)]
    co <- eq$coef[,1]
    
    delta_y_ar1[(t - Tini + 1), k] <- c(1,xx) %*% co
    
  }
  }



MSE_ER_ar1 <- matrix(NA,nrow = 1, ncol = ncol(delta_y))

for(i in 1:ncol(delta_y)){
  MSE_ER_ar1[,i] <- RMSE(delta_y_ar1[,i],delta_y_actual[,i])/RMSE( delta_y_rw[,i],delta_y_actual[,i])
}


############# Dados Macroeconômicos ############################################

getwd()
setwd('/Users/Desktop/MESTRADO/ECONOMETRIA II/Dados de Séries Macroeconômicas/Preditores Macroeconômicos')


predict_macro <- read.csv2('Preditores.csv', header = TRUE, sep = ";", dec = ',')
head(predict_macro)
str(predict_macro)
summary(predict_macro[,-1])

class(predict_macro)


predict_macro_1 <- as.matrix(predict_macro[,-1])
predict_macro_1[is.na(predict_macro_1)] <- 0
class(predict_macro_1)

adf_test_list <- list()
for (i in 1:ncol(predict_macro_1)) {
  tmp <- adf.test(predict_macro_1[,i])
  p_value <- tmp$p.value
  adf_test_list[[i]] <- p_value
  
}

tmp

tmp$p.value

adf_test_list

idx_estacionarias <- unlist(adf_test_list) <= 0.10
idx_não_estacionarias <- unlist(adf_test_list) > 0.10

predict_macro_estacionarias <- predict_macro_1[,idx_estacionarias]
predict_macro_não_estacionarias <- predict_macro_1[,idx_não_estacionarias]


head(predict_macro_estacionarias)
head(predict_macro_não_estacionarias)
class(predict_macro_não_estacionarias)


############# Teste de 1º diferença das variáveis macroeconômicas - Estacionária/ Não estacionária #####

predict_não_estac_diff <- diff(predict_macro_não_estacionarias)


predict_macro_não_estacionarias[1,18] <- 717


adf_test_list <- list()
for (i in 1:ncol(predict_não_estac_diff)) {
  tmp <- adf.test(predict_não_estac_diff[,i])
  p_value <- tmp$p.value
  adf_test_list[[i]] <- p_value
  
}

adf_test_list


M <- cbind(predict_macro_estacionarias[-1,],predict_não_estac_diff)
head(M)


############ Modelo com dados macroeconômicos ##################################

delta_macro <- matrix(NA, ncol = ncol(delta_y), nrow = TF) 

grid_alphas <- seq(from = 0.01, to = 0.99 , by = 0.01)

fll1_all <- seq(from = 500, to = 5000, by = 100)


tf_calibrado <- c(700, 4300)

alphas_min <- c(0.85, 0.23)

Tini2 <- Tini
kmax_fac <- 8

for(k in 1:ncol(delta_macro)){ 
  
  for(t in Tini:(T-1)){ 
    set.seed(12345)
    
  
      z <- M
      
    z1 <- M
    print("ncol z")
    print(ncol(z1))
    
    x <- delta_y[1:(t-1),k]
    xx <- delta_y[t,k]
    
    
    yy <- delta_y[1:t,k]
    zz <-  z1[1:t,]
    if(t == Tini | t == Tini2 ){ 
      
      cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
      eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_min[k], lambda = cv$lambda.min)
      co=coef(eq)[2:(ncol(zz)+1)]
      II2 = abs(co)>0
      
      print(sum(II2))
      if (sum(II2)==0 | sum(II2) == 1)
      {
        cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
        eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
        co=coef(eq)[2:(ncol(zz)+1)]
        II2=abs(co)>0
      }
      print("II2 new")
      print(sum(II2))
      
    }
    
    sx <- as.matrix(subset(zz,select=II2))
    
    
    if (ncol(sx) > kmax_fac) {
      opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
      spc <- as.matrix(opt_fac[[2]])
    } else
    {
      opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
      spc <- as.matrix(opt_fac[[2]])
    }
    
    
    
    
    
    eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
    I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
    print("I")
    print(I)
    if (sum(I)==0){
      I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
    }
    
    sc <- as.matrix(spc[,I])
    
    eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
    delta_macro[(t - Tini + 1), k] <-  c(1,sc[t,],xx)%*%eq2$coef[,1]
    
    print(t)
    
    
  }
  print(k)
}  


MSE_ER_macro <- matrix(NA,nrow = 1, ncol = ncol(delta_y))

for(i in 1:ncol(delta_y)){
  MSE_ER_macro[,i] <- RMSE(delta_macro[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
}


################### Teste de Clark West para o Modelo de Macro ###################

CW_test <- function(obs,mod,bench){
  MSPE2 <- (obs-bench)^2 - (obs-mod)^2 + (bench-mod)^2
  eq <- t.test(MSPE2,alternative="greater", conf.level=0.90)
  pvalue2 <- eq$p.value
  return(round(pvalue2,2))
}


 CW_test_macro <- matrix(NA,nrow = 1, ncol = ncol(delta_y))

for(i in 1:ncol(delta_y)){
  CW_test_macro[,i] <- CW_test(obs = delta_y_actual[,i], mod = delta_macro[,i], bench = delta_y_ar1[,i])
}

 
 
############ Teste de Clark West para o modelo de News ###################################
 
 
 CW_test_news <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   CW_test_news[,i] <- CW_test(obs = delta_y_actual[,i], mod = delta_news[,i], bench = delta_y_ar1[,i])
 }
 
 
########### Modelo de Palavras positivas ##############################################
 
 delta_news_pos <- matrix(NA, ncol = ncol(delta_y), nrow = TF) 
 
 grid_alphas <- seq(from = 0.01, to = 0.99 , by = 0.01)
 
 fll1_all <- seq(from = 500, to = 5000, by = 100)
 
 
 tf_calibrado <- c(3600, 4700)
 
 alphas_min <- c(0.74, 0.23)
 
 Tini2 <- Tini
 kmax_fac <- 8
 
 for(k in 1:ncol(delta_news_pos)){ 
   
   for(t in Tini:(T-1)){ 
     set.seed(12345)
     
     
     if(t == Tini | t == Tini2 ){ 
       
       z <- z2
       if(t >= Tini && t < Tini2) { 
         Tini_tf = Tini
       } else { 
         Tini_tf = Tini2  
       }
       m_data <-  tfidfsum1(z[1:Tini_tf,])
       m_data_srt <- sort(m_data,decreasing=TRUE)
       ntrms <- tf_calibrado[k]
       meanfilter <- m_data_srt[ntrms]
       II=m_data>=meanfilter
     }
     data_words1 <- as.matrix(z[,II])
     z1 <- data_words1
     print("ncol z")
     print(ncol(z1))
     
     x <- delta_y[1:(t-1),k]
     xx <- delta_y[t,k]
     
     
     yy <- delta_y[1:t,k]
     zz <-  z1[1:t,]
     if(t == Tini | t == Tini2 ){ 
       
       cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
       eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_min[k], lambda = cv$lambda.min)
       co=coef(eq)[2:(ncol(zz)+1)]
       II2 = co > 0
       
       print(sum(II2))
       if (sum(II2)==0 | sum(II2) == 1)
       {
         cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
         eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
         co=coef(eq)[2:(ncol(zz)+1)]
         II2= co >0
       }
       print("II2 new")
       print(sum(II2))
       
     }
     
     sx <- as.matrix(subset(zz,select=II2))
     
     
     if (ncol(sx) > kmax_fac) {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
       spc <- as.matrix(opt_fac[[2]])
     } else
     {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
       spc <- as.matrix(opt_fac[[2]])
     }
     
     
     
     
     
     eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
     I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
     print("I")
     print(I)
     if (sum(I)==0){
       I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
     }
     
     sc <- as.matrix(spc[,I])
     
     eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
     delta_news_pos[(t - Tini + 1), k] <-  c(1,sc[t,],xx)%*%eq2$coef[,1]
     
     print(t)
     
     
   }
   print(k)
 }  
 
 
 MSE_ER_news_pos <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   MSE_ER_news_pos[,i] <- RMSE(delta_news_pos[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
 }
 


####### Teste de CW para palavras positivas #####################################
 
 
 CW_test_news_pos <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   CW_test_news_pos[,i] <- CW_test(obs = delta_y_actual[,i], mod = delta_news_pos[,i], bench = delta_y_ar1[,i])
 } 
 

######### Modelo de Palavras negativas ################################################
 
 delta_news_neg <- matrix(NA, ncol = ncol(delta_y), nrow = TF) 
 
 grid_alphas <- seq(from = 0.01, to = 0.99 , by = 0.01)
 
 fll1_all <- seq(from = 500, to = 5000, by = 100)
 
 
 tf_calibrado <- c(3600, 4700)
 
 alphas_min <- c(0.74, 0.23)
 
 Tini2 <- Tini
 kmax_fac <- 8
 
 for(k in 1:ncol(delta_news_neg)){ 
   
   for(t in Tini:(T-1)){ 
     set.seed(12345)
     
     
     if(t == Tini | t == Tini2 ){ 
       
       z <- z2
       if(t >= Tini && t < Tini2) { 
         Tini_tf = Tini
       } else { 
         Tini_tf = Tini2  
       }
       m_data <-  tfidfsum1(z[1:Tini_tf,])
       m_data_srt <- sort(m_data,decreasing=TRUE)
       ntrms <- tf_calibrado[k]
       meanfilter <- m_data_srt[ntrms]
       II=m_data>=meanfilter
     }
     data_words1 <- as.matrix(z[,II])
     z1 <- data_words1
     print("ncol z")
     print(ncol(z1))
     
     x <- delta_y[1:(t-1),k]
     xx <- delta_y[t,k]
     
     
     yy <- delta_y[1:t,k]
     zz <-  z1[1:t,]
     if(t == Tini | t == Tini2 ){ 
       
       cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
       eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_min[k], lambda = cv$lambda.min)
       co=coef(eq)[2:(ncol(zz)+1)]
       II2 = co < 0
       
       print(sum(II2))
       if (sum(II2)==0 | sum(II2) == 1)
       {
         cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
         eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
         co=coef(eq)[2:(ncol(zz)+1)]
         II2= co < 0
       }
       print("II2 new")
       print(sum(II2))
       
     }
     
     sx <- as.matrix(subset(zz,select=II2))
     
     
     if (ncol(sx) > kmax_fac) {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
       spc <- as.matrix(opt_fac[[2]])
     } else
     {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
       spc <- as.matrix(opt_fac[[2]])
     }
     
     
     
     
     
     eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
     I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
     print("I")
     print(I)
     if (sum(I)==0){
       I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
     }
     
     sc <- as.matrix(spc[,I])
     
     eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
     delta_news_neg[(t - Tini + 1), k] <-  c(1,sc[t,],xx)%*%eq2$coef[,1]
     
     print(t)
     
     
   }
   print(k)
 }  
 
 
 MSE_ER_news_neg <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   MSE_ER_news_neg[,i] <- RMSE(delta_news_neg[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
 }
 
 
########### CW_teste para palavras negativas #################################
 
 CW_test_news_neg <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   CW_test_news_neg[,i] <- CW_test(obs = delta_y_actual[,i], mod = delta_news_neg[,i], bench = delta_y_ar1[,i])
 } 
 
 
 ################### Modelo de News com fatores ###########################################
 
 delta_news_fac <- matrix(NA, ncol = ncol(delta_y), nrow = TF) 
 
 grid_alphas <- seq(from = 0.01, to = 0.99 , by = 0.01)
 
 fll1_all <- seq(from = 500, to = 5000, by = 100)
 
 
 tf_calibrado <- c(3600, 4700)
 
 alphas_min <- c(0.74, 0.23)
 
 Tini2 <- Tini
 kmax_fac <- 8
 
 for(k in 1:ncol(delta_news_fac)){ 
   
   for(t in Tini:(T-1)){ 
     set.seed(12345)
     
     
     if(t == Tini | t == Tini2 ){ 
       
       z <- z2
       if(t >= Tini && t < Tini2) { 
         Tini_tf = Tini
       } else { 
         Tini_tf = Tini2  
       }
       m_data <-  tfidfsum1(z[1:Tini_tf,])
       m_data_srt <- sort(m_data,decreasing=TRUE)
       ntrms <- tf_calibrado[k]
       meanfilter <- m_data_srt[ntrms]
       II=m_data>=meanfilter
     }
     data_words1 <- as.matrix(z[,II])
     z1 <- data_words1
     print("ncol z")
     print(ncol(z1))
     
     x <- delta_y[1:(t-1),k]
     xx <- delta_y[t,k]
     
     
     yy <- delta_y[1:t,k]
     zz <-  z1[1:t,]
     if(t == Tini | t == Tini2 ){ 
       
       cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
       eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_min[k], lambda = cv$lambda.min)
       co=coef(eq)[2:(ncol(zz)+1)]
       II2 = abs(co)>=0
       
       print(sum(II2))
       if (sum(II2)==0 | sum(II2) == 1)
       {
         cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
         eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
         co=coef(eq)[2:(ncol(zz)+1)]
         II2=abs(co)>=0
       }
       print("II2 new")
       print(sum(II2))
       
     }
     
     sx <- as.matrix(subset(zz,select=II2))
     
     
     if (ncol(sx) > kmax_fac) {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
       spc <- as.matrix(opt_fac[[2]])
     } else
     {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
       spc <- as.matrix(opt_fac[[2]])
     }
     
     
     
     
     
     eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
     I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
     print("I")
     print(I)
     if (sum(I)==0){
       I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
     }
     
     sc <- as.matrix(spc[,I])
     
     eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
     delta_news_fac[(t - Tini + 1), k] <-  c(1,sc[t,],xx)%*%eq2$coef[,1]
     
     print(t)
     
     
   }
   print(k)
 }  
 
 
 MSE_ER_news_fac <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   MSE_ER_news_fac[,i] <- RMSE(delta_news_fac[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
 }
 
 ########### Teste de CW para o modelo de news com fatores ######################

 CW_test_news_fac <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   CW_test_news_fac[,i] <- CW_test(obs = delta_y_actual[,i], mod = delta_news_fac[,i], bench = delta_y_ar1[,i])
 } 
 
 
########### Modelo de news com lasso ############################################
 
 delta_news_lasso <- matrix(NA, ncol = ncol(delta_y), nrow = TF) 
 
 grid_alphas <- seq(from = 0.01, to = 0.99 , by = 0.01)
 
 fll1_all <- seq(from = 500, to = 5000, by = 100)
 
 
 tf_calibrado <- c(3600, 4700)
 
 alphas_min <- c(1,1)
 
 Tini2 <- Tini
 kmax_fac <- 8
 
 for(k in 1:ncol(delta_news_lasso)){ 
   
   for(t in Tini:(T-1)){ 
     set.seed(12345)
     
     
     if(t == Tini | t == Tini2 ){ 
       
       z <- z2
       if(t >= Tini && t < Tini2) { 
         Tini_tf = Tini
       } else { 
         Tini_tf = Tini2  
       }
       m_data <-  tfidfsum1(z[1:Tini_tf,])
       m_data_srt <- sort(m_data,decreasing=TRUE)
       ntrms <- tf_calibrado[k]
       meanfilter <- m_data_srt[ntrms]
       II=m_data>=meanfilter
     }
     data_words1 <- as.matrix(z[,II])
     z1 <- data_words1
     print("ncol z")
     print(ncol(z1))
     
     x <- delta_y[1:(t-1),k]
     xx <- delta_y[t,k]
     
     
     yy <- delta_y[1:t,k]
     zz <-  z1[1:t,]
     if(t == Tini | t == Tini2 ){ 
       
       cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
       eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_min[k], lambda = cv$lambda.min)
       co=coef(eq)[2:(ncol(zz)+1)]
       II2 = abs(co)>0
       
       print(sum(II2))
       if (sum(II2)==0 | sum(II2) == 1)
       {
         cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
         eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
         co=coef(eq)[2:(ncol(zz)+1)]
         II2=abs(co)>0
       }
       print("II2 new")
       print(sum(II2))
       
     }
     
     sx <- as.matrix(subset(zz,select=II2))
     
     
     if (ncol(sx) > kmax_fac) {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
       spc <- as.matrix(opt_fac[[2]])
     } else
     {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
       spc <- as.matrix(opt_fac[[2]])
     }
     
     
     
     
     
     eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
     I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
     print("I")
     print(I)
     if (sum(I)==0){
       I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
     }
     
     sc <- as.matrix(spc[,I])
     
     eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
     delta_news_lasso[(t - Tini + 1), k] <-  c(1,sc[t,],xx)%*%eq2$coef[,1]
     
     print(t)
     
     
   }
   print(k)
 }  
 
 
 MSE_ER_news_lasso <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   MSE_ER_news_lasso[,i] <- RMSE(delta_news_lasso[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
 }
 
 
############### Teste de CW para o modelo de news com lasso ############################

 CW_test_news_lasso <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   CW_test_news_lasso[,i] <- CW_test(obs = delta_y_actual[,i], mod = delta_news_lasso[,i], bench = delta_y_ar1[,i])
 } 
 

############## Modelo macroeconômico com fatores ########################################
 
 delta_macro_fac <- matrix(NA, ncol = ncol(delta_y), nrow = TF) 
 
 grid_alphas <- seq(from = 0.01, to = 0.99 , by = 0.01)
 
 fll1_all <- seq(from = 500, to = 5000, by = 100)
 
 
 tf_calibrado <- c(700, 4300)
 
 alphas_min <- c(0.85, 0.23)
 
 Tini2 <- Tini
 kmax_fac <- 8
 
 for(k in 1:ncol(delta_macro_fac)){ 
   
   for(t in Tini:(T-1)){ 
     set.seed(12345)
     
     
     z <- M
     
     z1 <- M
     print("ncol z")
     print(ncol(z1))
     
     x <- delta_y[1:(t-1),k]
     xx <- delta_y[t,k]
     
     
     yy <- delta_y[1:t,k]
     zz <-  z1[1:t,]
     if(t == Tini | t == Tini2 ){ 
       
       cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
       eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_min[k], lambda = cv$lambda.min)
       co=coef(eq)[2:(ncol(zz)+1)]
       II2 = abs(co)>=0
       
       print(sum(II2))
       if (sum(II2)==0 | sum(II2) == 1)
       {
         cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
         eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
         co=coef(eq)[2:(ncol(zz)+1)]
         II2=abs(co)>=0
       }
       print("II2 new")
       print(sum(II2))
       
     }
     
     sx <- as.matrix(subset(zz,select=II2))
     
     
     if (ncol(sx) > kmax_fac) {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
       spc <- as.matrix(opt_fac[[2]])
     } else
     {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
       spc <- as.matrix(opt_fac[[2]])
     }
     
     
     
     
     
     eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
     I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
     print("I")
     print(I)
     if (sum(I)==0){
       I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
     }
     
     sc <- as.matrix(spc[,I])
     
     eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
     delta_macro_fac[(t - Tini + 1), k] <-  c(1,sc[t,],xx)%*%eq2$coef[,1]
     
     print(t)
     
     
   }
   print(k)
 }  
 
 
 MSE_ER_macro_fac <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   MSE_ER_macro_fac[,i] <- RMSE(delta_macro_fac[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
 }
 
 
################## Teste de CW para o modelo macroeconômico de fatores ###############
 
 CW_test_macro_fac <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   CW_test_macro_fac[,i] <- CW_test(obs = delta_y_actual[,i], mod = delta_macro_fac[,i], bench = delta_y_ar1[,i])
 } 
 

 
 ###############  Modelo Macroeconômico com lasso #####################################

 delta_macro_lasso <- matrix(NA, ncol = ncol(delta_y), nrow = TF) 
 
 grid_alphas <- seq(from = 0.01, to = 0.99 , by = 0.01)
 
 fll1_all <- seq(from = 500, to = 5000, by = 100)
 
 
 tf_calibrado <- c(700, 4300)
 
 alphas_min <- c(1,1)
 
 Tini2 <- Tini
 kmax_fac <- 8
 
 for(k in 1:ncol(delta_macro_lasso)){ 
   
   for(t in Tini:(T-1)){ 
     set.seed(12345)
     
     
     z <- M
     
     z1 <- M
     print("ncol z")
     print(ncol(z1))
     
     x <- delta_y[1:(t-1),k]
     xx <- delta_y[t,k]
     
     
     yy <- delta_y[1:t,k]
     zz <-  z1[1:t,]
     if(t == Tini | t == Tini2 ){ 
       
       cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
       eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_min[k], lambda = cv$lambda.min)
       co=coef(eq)[2:(ncol(zz)+1)]
       II2 = abs(co)>0
       
       print(sum(II2))
       if (sum(II2)==0 | sum(II2) == 1)
       {
         cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
         eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
         co=coef(eq)[2:(ncol(zz)+1)]
         II2=abs(co)>0
       }
       print("II2 new")
       print(sum(II2))
       
     }
     
     sx <- as.matrix(subset(zz,select=II2))
     
     
     if (ncol(sx) > kmax_fac) {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
       spc <- as.matrix(opt_fac[[2]])
     } else
     {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
       spc <- as.matrix(opt_fac[[2]])
     }
     
     
     
     
     
     eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
     I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
     print("I")
     print(I)
     if (sum(I)==0){
       I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
     }
     
     sc <- as.matrix(spc[,I])
     
     eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
     delta_macro_lasso[(t - Tini + 1), k] <-  c(1,sc[t,],xx)%*%eq2$coef[,1]
     
     print(t)
     
     
   }
   print(k)
 }  
 
 
 MSE_ER_macro_lasso <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   MSE_ER_macro_lasso[,i] <- RMSE(delta_macro_lasso[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
 }
 
 
################## Teste de CW para o modelo macroeconômico com lasso ###################
 
 CW_test_macro_lasso <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   CW_test_macro_lasso[,i] <- CW_test(obs = delta_y_actual[,i], mod = delta_macro_lasso[,i], bench = delta_y_ar1[,i])
 } 
 
 
###################### Tabelas ####################################################
 
 MSEs_geral <- rbind(MSE_ER_news,MSE_ER_macro,MSE_ER_news_fac,MSE_ER_news_lasso,
                     MSE_ER_macro_fac,MSE_ER_macro_lasso,MSE_ER_news_pos,MSE_ER_news_neg, MSE_ER_y_sentiment,MSE_ER_tv_sentiment)
 MSEs_geral
 
 names_mse <- c("News", "Macro","News_fact","News_lasso","Macro_fact", "Macro_lasso",
                "News_pos", "News_neg", "y_sentiment", "tv_sentiment")
 MSEs_geral_df <- cbind.data.frame(names_mse,MSEs_geral)
 MSEs_geral_df
 
 colnames(MSEs_geral_df) <- c("Modelo", "Tx_desemp", "pro_idt")
 
 MSEs_geral_df
 library(xtable)
 xtable(MSEs_geral_df)
 
 
 
 
 CW_geral <- rbind(CW_test_news, CW_test_macro,CW_test_news_fac, CW_test_news_lasso, CW_test_macro_fac,CW_test_macro_lasso,
                   CW_test_news_pos, CW_test_news_neg, CW_test_y_sentiment, CW_test_tv_sentiment)
 CW_geral
 
 names_mse <- c("CW_test_news", "CW_test_macro","CW_test_news_fac","CW_test_news_lasso","CW_test_macro_fac", "CW_test_macro_lasso",
                "CW_test_news_pos", "CW_test_news_neg", "CW_test_y_sentiment", "CW_test_tv_sentiment")
 
 CW_geral_df <- cbind.data.frame(names_mse,CW_geral)
 CW_geral_df
 
 colnames(CW_geral_df) <- c("Modelo", "Tx_desemp", "pro_idt")
 
 CW_geral_df
 library(xtable)
 xtable(CW_geral_df)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
###################### Modelo com dicionário fixo ###############################
 
 
 library(tidytext)
 library(SnowballC)
 library(SentimentAnalysis)
 install.packages("textdata")
 library(textdata)
 install.packages("translateR")
 library(translateR)
 
 correa <- read.csv2("correa_dictionary1_pt.csv", header = TRUE, dec = ";", sep = ',')
 head(correa)
 class(correa)
 
 correa$Positivo[is.na(correa$Positivo)] = 0
 correa$Negativo[is.na(correa$Negativo)] = 0
 
 idx_correa_pos <- correa$Positivo == 1
 idx_correa_neg <- correa$Negativo == 1
 
 correa_pos <- as.character(correa$Palavra[idx_correa_pos])
 correa_pos_st <- unique(SnowballC::wordStem(correa_pos,"porter"))
 
 correa_neg <- as.character(correa$Palavra[idx_correa_neg])
 correa_neg_st <- unique(SnowballC::wordStem(correa_neg,"porter"))

 tetlock <- read.csv2("tetlock_pos_neg1.csv", header = TRUE, dec = ";", sep = ',')
 head(tetlock)
 str(tetlock)
 
 tetlock$Positivo[is.na(tetlock$Positivo)] = 0
 tetlock$Negativo[is.na(tetlock$Negativo)] = 0
 
 idx_tetlock_pos <- tetlock$Positivo == 1
 idx_tetlock_neg <- tetlock$Negativo == 1
 
 tetlock_pos <- as.character(tetlock$Palavra[idx_tetlock_pos])
 tetlock_pos_st <- unique(SnowballC::wordStem(tetlock_pos,"porter"))
 
 tetlock_neg <- as.character(tetlock$Palavra[idx_tetlock_neg])
 tetlock_neg_st <- unique(SnowballC::wordStem(tetlock_neg,"porter"))
 

 install.packages("tibble")
 library(tibble)
 
 tetlock_df_positivo <- tibble(positivo = tetlock_pos)
 tetlock_df_negativo <- tibble(negativo = tetlock_neg)
 write.csv(tetlock_df_positivo, file = "tetlock_pos.csv")
 write.csv(tetlock_df_negativo, file = "tetlock_neg.csv")
 
 getwd()
 setwd('/Users/Desktop/MESTRADO/ECONOMETRIA II/Dados de Séries Macroeconômicas/Preditores Macroeconômicos')
 loughran <- read.csv2("loughran_pt.csv", header = TRUE, dec = ";", sep = ',')
 class(loughran)
 head(loughran)
 
 loughran$positivo[is.na(loughran$positivo)] = 0
 loughran$negativo[is.na(loughran$negativo)] = 0
 
 idx_loughran_pos <- loughran$positivo == 1
 idx_loughran_neg <- loughran$negativo == 1
 
 loughran_pos <- as.character(loughran$palavra[idx_loughran_pos])
 loughran_pos_st <- unique(SnowballC::wordStem(loughran_pos,"porter"))
 
 loughran_neg <- as.character(loughran$palavra[idx_loughran_neg])
 loughran_neg_st <- unique(SnowballC::wordStem(loughran_neg,"porter"))
 

 get_sentiments("loughran")

 mcl=get_sentiments("loughran")
 write.csv(mcl,file = 'loughran.csv')
 
 II_positive = mcl$sentiment=="positive"
 II_negative = mcl$sentiment=="negative"
 
 lm_words <- mcl
 
 lm_words_pos <- lm_words$word[II_positive]
 lm_words_neg <- lm_words$word[II_negative]
 
 
 lm_pos_st <- unique(SnowballC::wordStem(lm_words_pos,"porter"))
 lm_neg_st <- unique(SnowballC::wordStem(lm_words_neg,"porter"))
 
 
 
 idx_dic <- unique(c(tetlock_neg_st, tetlock_pos_st, loughran_pos_st, loughran_neg_st, correa_neg_st, correa_pos_st))
 
 idx_dic_num <- colnames(z2) %in% idx_dic
 
 
 ################################ Model using sentiment charged words ##########################################
 delta_y_tv_sentiment <-  matrix(0,nrow=TF,ncol=ncol(delta_y_actual))

 
 fll1_all <- seq(from = 500, to = 5000, by = 100)
 
 
 tf_idf_fac_news <- c(4900 , 600 ,1800  ,500 ,2500 , 700 ,1400, 1500 ,2000)
 
 alphas_fac_news <- alphas_min
 
 Tini2 <- Tini
 kmax_fac <- 8
 
 for(k in 1:ncol(delta_y_tv_sentiment)){ 
   
   for(t in Tini:(T-1)){ 
     set.seed(12345)
     
     
     
     z <- z2[,idx_dic_num]
     z1 <- z
     print("ncol z1")
     print(ncol(z1))
     
     x <- delta_y[1:(t-1),k]
     xx <- delta_y[t,k]
     
     yy <- delta_y[1:t,k]
     zz <-  z1[1:t,]
     if(t == Tini | t == Tini2 ){ 
       
       cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
       eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_fac_news[k], lambda = cv$lambda.min)
       co=coef(eq)[2:(ncol(zz)+1)]
       II2 = abs(co)>0
       
       print(sum(II2))
       if (sum(II2)==0 | sum(II2) == 1)
       {
         cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
         eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
         co=coef(eq)[2:(ncol(zz)+1)]
         II2=abs(co)>0
       }
       print("II2 new")
       print(sum(II2))
       
     }
     
     sx <- as.matrix(subset(zz,select=II2))
     idx_sx <- apply(sx, 2, FUN = sum)
     idx_sx1 <- idx_sx != 0
     sx <- sx[,idx_sx1]
     
     if (ncol(sx) > kmax_fac) {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
       spc <- as.matrix(opt_fac[[2]])
     } else
     {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
       spc <- as.matrix(opt_fac[[2]])
     }
     
     
     
     
     
     eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
     I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
     print("I")
     print(I)
     if (sum(I)==0){
       I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
     }
     
     sc <- as.matrix(spc[,I])
     
     eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
     delta_y_tv_sentiment[(t - Tini + 1), k] <-  c(1,sc[t,],xx)%*%eq2$coef[,1]
     
     print(t)
     
     
   }
   print(k)
 }  
 
 
 
 
 
 save_image()
 
################################################################################
 
 MSE_ER_tv_sentiment <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   MSE_ER_tv_sentiment[,i] <- RMSE(delta_y_tv_sentiment[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
 }
 
#################################################################################
 
 CW_test_tv_sentiment <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   CW_test_tv_sentiment[,i] <- CW_test(obs = delta_y_actual[,i], mod = delta_y_tv_sentiment[,i], bench = delta_y_ar1[,i])
 } 
 
 
 
 
 
 

 
 
 ############################ Delta sentiment charged  Xt ##################################################################
 
 
 delta_y_sentiment <-  matrix(0,nrow=TF,ncol=ncol(delta_y_actual))
 
 
 fll1_all <- seq(from = 500, to = 5000, by = 100)
 
 
 tf_idf_fac_news <- c(4900 , 600 ,1800  ,500 ,2500 , 700 ,1400, 1500 ,2000)
 
 alphas_fac_news <- alpha_min
 
 Tini2 <- Tini
 kmax_fac <- 8
 
 for(k in 1:ncol(delta_y_tv_sentiment)){ 
   
   for(t in Tini:(T-1)){ 
     set.seed(12345)
     
     
     
     z <- z2[,idx_dic_num]
     z1 <- z
     print("ncol z1")
     print(ncol(z1))
     
     x <- delta_y[1:(t-1),k]
     xx <- delta_y[t,k]
     
     
     yy <- delta_y[1:t,k]
     zz <-  z1[1:t,]
     if(t == Tini | t == Tini2 ){ 
       
       cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
       eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_fac_news[k], lambda = cv$lambda.min)
       co=coef(eq)[2:(ncol(zz)+1)]
       II2 = abs(co)>=0
       
       print(sum(II2))
       if (sum(II2)==0 | sum(II2) == 1)
       {
         cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
         eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
         co=coef(eq)[2:(ncol(zz)+1)]
         II2=abs(co)>0
       }
       print("II2 new")
       print(sum(II2))
       
     }
     
     sx <- as.matrix(subset(zz,select=II2))
     idx_sx <- apply(sx, 2, FUN = sum)
     idx_sx1 <- idx_sx != 0
     sx <- sx[,idx_sx1]
     
     if (ncol(sx) > kmax_fac) {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
       spc <- as.matrix(opt_fac[[2]])
     } else
     {
       opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
       spc <- as.matrix(opt_fac[[2]])
     }
     
     
     
     
     
     eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
     I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
     print("I")
     print(I)
     if (sum(I)==0){
       I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
     }
     
     sc <- as.matrix(spc[,I])
     
     eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
     delta_y_sentiment[(t - Tini + 1), k] <-  c(1,sc[t,],xx)%*%eq2$coef[,1]
     
     print(t)
     
     
   }
   print(k)
 }  
 
 
 
 
 
 save_image()
 

################## RMSE DO Y_TV_SENTIMENT ######################################
 
 MSE_ER_y_sentiment <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   MSE_ER_y_sentiment[,i] <- RMSE(delta_y_sentiment[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
 }
 
 
############## CW_TESTE PARA Y_TV_SENTIMENT ####################################
 CW_test_y_sentiment <- matrix(NA,nrow = 1, ncol = ncol(delta_y))
 
 for(i in 1:ncol(delta_y)){
   CW_test_y_sentiment[,i] <- CW_test(obs = delta_y_actual[,i], mod = delta_y_sentiment[,i], bench = delta_y_ar1[,i])
 } 
 
 
 
################### Palavras positivas e Negativas #############################
 
pos_terms <- list()
neg_terms <- list()


#tf_idf_tv_fac_news <- c(3600,4700)
tf_idf_tv_fac_news <- c(3600,4700)


#alphas_idf_tv_fac_news <- c(0.74, 0.23)
alphas_idf_tv_fac_news <- c(0.15, 0.15)

Tini2 <- Tini
kmax_fac <- 8

for(k in 1:ncol(delta_y_actual)){ 
  
  for(t in Tini:(T-1)){ 
    set.seed(12345)
    
    
    if(t == Tini | t == Tini2 ){ 
      
      z <- z2
      if(t >= Tini && t < Tini2) { 
        Tini_tf = Tini
      } else { 
        Tini_tf = Tini2  
      }
      m_data <-  tfidfsum1(z[1:Tini_tf,])
      m_data_srt <- sort(m_data,decreasing=TRUE)
      ntrms <- tf_idf_tv_fac_news[k]
      meanfilter <- m_data_srt[ntrms]
      II=m_data>=meanfilter
    }
    data_words1 <- as.matrix(z[,II])
    z1 <- data_words1
    print("ncol z")
    print(ncol(z1))
    
    
    yy <- delta_y[1:t,k]
    zz <-  z1[1:t,]
    if(t == Tini | t == Tini2 ){ 
      
      cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
      eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_idf_tv_fac_news[k], lambda = cv$lambda.min)
      co=coef(eq)[2:(ncol(zz)+1)]
      II2 = abs(co)>0
      II2_pos = co >0
      II2_neg = co < 0
      
      print(sum(II2))
      if (sum(II2)==0 | sum(II2) == 1)
      {
        cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
        eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
        co=coef(eq)[2:(ncol(zz)+1)]
        II2=abs(co)>0
      }
      print("II2 new")
      print(sum(II2))
      
    }
    
    
    sx <- as.matrix(subset(zz,select=II2))
    sx_pos <- as.matrix(subset(zz,select=II2_pos))
    sx_neg <- as.matrix(subset(zz,select=II2_neg))
    
    if(t == Tini | t == Tini2 ){ 
      pos_terms[[k]] <- colnames(sx_pos)
      neg_terms[[k]] <- colnames(sx_neg)
    }
  }
  print(k)
}  



############# Combinação dos modelos #########################################
 

delta_comb_prev <- matrix(NA, nrow = TF, ncol = ncol(delta_y_actual))
for (i in 1:ncol(delta_y_actual)) {
  matriz_modelo <- cbind(delta_news[,i], delta_macro[,i], delta_news_fac[,i], delta_news_lasso[,i],
                         delta_macro_fac[,i],delta_macro_lasso[,i], delta_news_pos[,i], delta_news_neg[,i],
                         delta_y_sentiment[,i],delta_y_tv_sentiment[,i])
  delta_comb_prev[,i] <- apply(matriz_modelo, 1, mean)
  
}


###############################################################################

MSE_ER_comb_prev <- matrix(NA,nrow = 1, ncol = ncol(delta_y))

for(i in 1:ncol(delta_y)){
  MSE_ER_comb_prev[,i] <- RMSE(delta_comb_prev[,i],delta_y_actual[,i])/RMSE( delta_y_ar1[,i],delta_y_actual[,i])
}
MSE_ER_comb_prev


#################################################################################
CW_test_comb_prev <- matrix(NA,nrow = 1, ncol = ncol(delta_y))

for(i in 1:ncol(delta_y)){
  CW_test_comb_prev[,i] <- CW_test(obs = delta_y_actual[,i], mod = delta_comb_prev[,i], bench = delta_y_ar1[,i])
} 

CW_test_comb_prev


###################################################################################


dates <- seq.Date(from = as.Date("2002-04-01"), to = as.Date("2019-12-31"), by = 
                    "months")
Emprego_ts <- ts(delta_y[,1])
Emprego <- as.xts(emprego_ts)
index(Emprego) = dates
length(dates)
nrow(delta_y)

par(mfrow = c(1,2))

dates <- seq.Date(from = as.Date("2002-04-01"), to = as.Date("2019-12-31"), by = 
                    "months")

Producao_Industrial_ts <- ts(delta_y[,2])
Producao_Industrial <- as.xts(Producao_Industrial_ts)
index(Producao_Industrial) = dates
length(dates)
nrow(delta_y)

plot(Emprego, main = "Unemployment", ylab = "", xlab = "")
plot(Producao_Industrial, main = "Industrial Production", ylab = "", xlab = "")


par(mfrow = c(1,2))
plot(Emprego)
plot(Producao_Industrial)

###################### Seleção de fatores #####################################

delta_news <- matrix(NA, ncol = ncol(delta_y), nrow = TF) 

grid_alphas <- seq(from = 0.01, to = 0.99 , by = 0.01)

fll1_all <- seq(from = 500, to = 5000, by = 100)

fatores <- list()

tf_calibrado <- c(3600, 4700)

alphas_min <- c(0.74, 0.23)

Tini2 <- Tini
kmax_fac <- 8

for(k in 1:ncol(delta_news)){ 
  I_fac1 <- matrix(0,nrow=TF,ncol=kmax_fac)
  
  for(t in Tini:(T-1)){ 
    set.seed(12345)
    
    
    if(t == Tini | t == Tini2 ){ 
      
      z <- z2
      if(t >= Tini && t < Tini2) { 
        Tini_tf = Tini
      } else { 
        Tini_tf = Tini2  
      }
      m_data <-  tfidfsum1(z[1:Tini_tf,])
      m_data_srt <- sort(m_data,decreasing=TRUE)
      ntrms <- tf_calibrado[k]
      meanfilter <- m_data_srt[ntrms]
      II=m_data>=meanfilter
    }
    data_words1 <- as.matrix(z[,II])
    z1 <- data_words1
    print("ncol z")
    print(ncol(z1))
    
    x <- delta_y[1:(t-1),k]
    xx <- delta_y[t,k]
    
    
    yy <- delta_y[1:t,k]
    zz <-  z1[1:t,]
    if(t == Tini | t == Tini2 ){ 
      
      cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
      eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha = alphas_min[k], lambda = cv$lambda.min)
      co=coef(eq)[2:(ncol(zz)+1)]
      II2 = abs(co)>0
      
      print(sum(II2))
      if (sum(II2)==0 | sum(II2) == 1)
      {
        cv <- cv.glmnet(zz[1:(t-1),],yy[2:t])      
        eq=glmnet(zz[1:(t-1),] ,yy[2:t],family="gaussian", alpha=0.15, lambda = cv$lambda[2])
        co=coef(eq)[2:(ncol(zz)+1)]
        II2=abs(co)>0
      }
      print("II2 new")
      print(sum(II2))
      
    }
    
    sx <- as.matrix(subset(zz,select=II2))
    
    
    if (ncol(sx) > kmax_fac) {
      opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = kmax_fac)
      spc <- as.matrix(opt_fac[[2]])
    } else
    {
      opt_fac <- TextForecast::optimal_number_factors(x = sx,kmax = ncol(sx))
      spc <- as.matrix(opt_fac[[2]])
    }
    
    
    
    
    
    eqq <- summary(lm(yy[2:t] ~ spc[1:(t-1),]))
    I  = eqq$coef[2:nrow(eqq$coef),4] < 0.01
    print("I")
    print(I)
    if (sum(I)==0){
      I=rank(eqq$coef[2:nrow(eqq$coef),4])<2
    }
    I_fac1[(t-Tini+1),1:length(I)] <- I
    

    sc <- as.matrix(spc[,I])
    
    eq2 <- summary(lm(yy[2:t]~sc[1:(t-1),] + x))
    delta_news[(t - Tini + 1), k] <-  c(1,sc[t,],xx)%*%eq2$coef[,1]
    
    print(t)
  
    
  }
  print(k)
  fatores[[k]] <- apply(I_fac1,2, FUN = sum) / nrow(I_fac1)
}  



