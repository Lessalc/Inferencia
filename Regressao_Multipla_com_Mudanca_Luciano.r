##############################################################
#  CURSO DE AVALIAÇÃO DE IMÓVEIS POR INFERÊNCIA ESTATÍSTICA	     	

# Prof.: Lutemberg Florencio				     
# Aluno: Luciano Cordeiro Lessa 	       	               		     			 
#							                                 
# EX.: REGRESSÃO LINEAR MÚLTIPLA (COM TRANSFORMAÇÃO)                          
##############################################################

#install.packages('corrplot')
#install.packages('lmtest') #testes RESET, Goldfeld-Quandt, Breusch-Pagan e Koenker 
library(lmtest) 
library(corrplot)
library(ggplot2)

############ MC - Regressão Linear Múltipla

############ 1. Dados Apresentados
setwd('C:\\Users\\luciano\\Avaliação Por Inferência\\Aula 05')
getwd()

df = read.csv('dados.csv', sep = ';')
View(df)
############ 2. Análise Exploratória ##########

############ 2.1 Tratamento dos Dados e Diagrama de Dispersão ##########

# Transformando os dados no tipo certo
df$AP = as.numeric(gsub(",", ".", as.character(df$AP)))
df$PA <- factor(x = df$PA, levels = c("BAIXO", "NORMAL", "ALTO")) # Define Alto = 1, Normal = 2 e Baixo = 3
df$PA_Factor <- df$PA
df$PA <- as.numeric(df$PA)

df$LNPU = log(df$PU)

############ 2.2 Visualizando os Gráficos ##########
ggplot(df, aes(x = AP, y = LNPU, color= PA_Factor)) +
  geom_point(size = 4)+
  geom_smooth(method = lm, se=FALSE)

ggplot(df, aes(x = AP, y = PU)) +
  geom_point(size = 4, color = 'blue')+
  geom_smooth(method = lm, se=FALSE, color = 'red')


ggplot(df, aes(x = AP, y = LNPU, color= PA_Factor, size = factor(APANDAR))) +
  geom_point()

ggplot(df, aes(x = AP, y = LNPU)) +
  geom_point(size = 4, color = 'blue')+
  geom_smooth(method = lm, se=FALSE)

ggplot(df, aes(x = AP, y = LNPU, color= factor(APANDAR))) +
  geom_point(size = 4)+
  geom_smooth(method = lm, se=FALSE)

# ANDAR x PU
ggplot(df, aes(x = APANDAR, y = PU, color= PA_Factor)) +
  geom_point()

ggplot(df, aes(x = APANDAR, y = LNPU, color= PA_Factor)) +
  geom_point()

# PA x PU
ggplot(df, aes(x = PA_Factor, y = PU)) +
  geom_boxplot(color='red')

ggplot(df, aes(x = PA_Factor, y = LNPU)) +
  geom_boxplot()


# COND x PU
ggplot(df, aes(x = COND, y = PU, color= PA_Factor)) +
  geom_point(size = 4)+
  geom_smooth(method = lm, se=FALSE)

ggplot(df, aes(x = COND, y = LNPU, color= PA_Factor)) +
  geom_point(size = 4)+
  geom_smooth(method = lm, se=FALSE)

ggplot(df, aes(x = COND, y = LNPU, color= factor(APANDAR))) +
  geom_point(size = 4)+
  geom_smooth(method = lm, se=FALSE)


ggplot(df, aes(x = COND, y = LNPU)) +
  geom_point(size = 4)+
  geom_smooth(method = lm, se=FALSE)


############ 3. Análise da Regressão ##########
############ 3.1 Análise dos Resultados ##########

attach(df)
modelo = lm(LNPU~AP+APANDAR+PA+COND)

summary(modelo)
desv <- sigma(modelo)

############ 3.2 Análise dos Resíduos ##########
residuo_df <- data.frame("LNPU_Observado" = LNPU)
residuo_df$LNPU_Previsto <- predict(modelo, data.frame(AP = AP, APANDAR = APANDAR, PA = PA , COND = COND))
residuo_df$Residuo <- residuo_df$LNPU_Observado - residuo_df$LNPU_Previsto
residuo_df$Residuo_relativo <- residuo_df$Residuo/residuo_df$LNPU_Observado * 100
residuo_df$Residuo_padronizado <- residuo_df$Residuo/desv

perc_1 <- 0
perc_164 <- 0
perc_196 <- 0

for (res in residuo_df$Residuo_padronizado){
  if(res > -1 & res < 1){
    perc_1 <- perc_1 + 1
  }
  if(res > -1.64 & res < 1.64){
    perc_164 <- perc_164 + 1
  }
  if(res > -1.96 & res < 1.96){
    perc_196 <- perc_196 + 1
  }
}
perc_1 <- perc_1/28*100
perc_164 <- perc_164/28*100
perc_196 <- perc_196/28*100

  residuo_df$PU <- df$PU
  residuo_df$VU <- exp(residuo_df$LNPU_Previsto)
  residuo_df$Residuo_estimat <- residuo_df$PU - residuo_df$VU
  residuo_df$Residuo_estimat_relativo <- residuo_df$Residuo_estimat/residuo_df$PU*100

############ 3.3 Gráfico de Aderência ##########
ggplot(residuo_df, aes(x = PU, y = VU)) +
  geom_point(color = 'blue', size = 4)+
  geom_abline(intercept = 0, slope = 1, color = "red", size = 2)+
  labs(x = 'Valores Observados', y = 'Valores Estimados', 
       title = 'GRÁFICO DE ADERÊNCIA')

############ 3.4 Pressupostos ##########

############ 3.4.1 Pressupostos de Normalidade ##########


shapiro.test(residuo_df$Residuo_padronizado)
# Valor-p >10%; portanto não rejeita Ho (hipótese da normalidade dos resíduos)



############ 3.4.2 Pressupostos de Homocedasticidade ##########

# Pressuposto de Homocedasticidade, autocorrelação e pontos atipicos
ggplot(residuo_df, aes(x = LNPU_Previsto, y = Residuo_padronizado)) +
  geom_point(size=4, color = 'blue')+
  geom_hline(yintercept = 0, color= 'black')+
  geom_hline(yintercept = 1, color= 'yellow')+
  geom_hline(yintercept = 1.5, color= 'green')+
  geom_hline(yintercept = 2, color= 'red')+
  geom_hline(yintercept = -1, color= 'yellow')+
  geom_hline(yintercept = -1.5, color = 'green')+
  geom_hline(yintercept = -2, color= 'red')+
  theme_gray()+
  labs(x = 'Valor Estimado', y = 'Resíduo Padronizado')


bptest(modelo, studentize=TRUE) 
# Valor-p >10%; portanto não rejeita Ho (hipótese de homocedasticidade dos resíduos)  

############ 3.4.3 Multicolinearidade ##########

df_corr <- data.frame('AP' = df$AP,'APANDAR' =  df$APANDAR, 'PA' = df$PA, 'COND' = df$COND, 'LNPU' = df$LNPU)

# Multicolinearidade
corrplot(cor(df_corr), method = 'color')
corrplot.mixed(cor(df_corr), lower = 'number', upper = 'color', lower.col = 'black')


############ 3.4.4 Outliers ##########
ggplot(residuo_df, aes(x = LNPU_Previsto, y = Residuo_padronizado)) +
  geom_point(size = 4, color = 'blue')+
  geom_hline(yintercept = 2, color= 'red', size = 2)+
  geom_hline(yintercept = -2, color = 'red', size = 2)+
  labs(x = 'Valor Estimado', y = 'Resíduo Padronizado')		

############ 4. Resultados da Avaliação ##########
############ 4.1 Modelo ##########
# Valor estimado para Luciano
AP_Luciano <- 107
PA_Luciano <- 3 # Alto =1
APANDAR_Luciano <- 4
COND_Luciano <- 800


############ 4.2 Grau de Fundamentação ##########

# PREENCHER TABELA


############ 4.3 Grau de Precisão ##########

# PREENCHER TABELA

############ 4.4 Intervalo de Confiança ##########

exp(predict(modelo, data.frame(AP = AP_Luciano, APANDAR = APANDAR_Luciano, PA = PA_Luciano , COND = COND_Luciano)))

exp(predict(modelo, data.frame(AP = AP_Luciano, APANDAR = APANDAR_Luciano, PA = PA_Luciano , COND = COND_Luciano), 
            interval = "confidence", level =  0.8))








