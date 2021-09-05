setwd("~/OneDrive/UFC - Doutorado/2021.1/Técnica de Análise de Dados de Transportes/TPs/TP4")
library(tidyverse)
library(psych)
library(wesanderson)
library(lmtest)
library(car)
library(gridExtra)
library(sjPlot)
library(lme4)
library(data.table)

# LER DADOS
dados <- read.csv("dados_tp1.csv", sep = ";")
dados <- dados %>% mutate(genero = as.factor(genero), raca = as.factor(raca), motivo = as.factor(motivo), modo = as.factor(modo))

# PREMISSAS TEMPO GENERALIZADO
renda_media <- 1028
t_valor <- (220*60)/renda_media # 220 horas trabalhadas por mes (min 12 / R$)

vel_oni <- 18.32 # velocidade média onibus
prop_edu <- sum(dados$motivo == "educacao" & dados$modo == "Transporte_Publico")/50

c_oni <- (1.6*prop_edu)+(3.6*(1-prop_edu)) # custo por viagem (fixo por viagem)

vel_car <- vel_oni*1.5 # ferraz e torres (estimativa velocidade media carro)
c_car <- 2.5 + .22*(dados$distancia_media_km/(vel_car/60)) + 1.32*dados$distancia_media_km # custo por km de viagem

# CRIAR VARIAVEL TEMPO GENERALIZADO
dados <- dados %>% mutate(
  tg = ifelse(
    modo == "Transporte_Aplicativo",
    (distancia_media_km/(vel_car/60)) + (c_car*t_valor) ,
    (distancia_media_km/(vel_oni/60)) + (3.6*t_valor)
  )
)


head(dados)

summary(dados)



# ANALISE DESCRITIVA DA VARIAVEL DEPENDENTE
dados %>% summarise(avg = mean(tg), s = sd(tg), coef_var = avg/s, var = s^2, n = n())

# GRAFICOS DESCRITIVOS TODOS OS DADOS
grid.arrange(

ggplot() +
  geom_point(aes(x = c(1:90), y = sort(dados$tg), color = "A"), alpha = .8) +
  geom_boxplot(aes(x = c(1:90), y = dados$tg, color = "B"), alpha = 0) +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "Variável Dependente", breaks = c("A"), label = c("Tempo Generaliado (min)"), guide = "legend") +
  labs(title = "Dispersão e Diagrama de Caixa", subtitle = "n = 90", x = "Observação", y = "Tempo Generalizado (min)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")),

ggplot() +
  geom_histogram(aes(x = dados$tg, y = ..density.., fill = "A"), color = "white", alpha = .8, bins = 9) +
  geom_density(aes(x = dados$tg, fill = "B"), alpha = .7) +
  scale_fill_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "Variável Dependente", breaks = c("A"), label = c("Tempo Generalizado (min)"), guide = "legend") +
  labs(title = "Histograma de Frequência e Densidade de Probabilidade", subtitle = "n = 90", x = "Tempo Generalizado (min)", y = "Probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")),

ggplot() +
  stat_ecdf(aes(x = dados$tg, color = "A"), alpha = .8) +
  stat_ecdf(aes(x = qnorm(seq(0.003, 0.997, length = 90), mean = mean(dados$tg), sd = sd(dados$tg)), color = "B"), alpha = .8) +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "Variável", breaks = c("A", "B"), label = c("Tempo Generalizado (min)", "Distribuição Normal"), guide = "legend") +
  labs(title = "Distribuição de Probabilidade Acumulada", subtitle = "n = 90", x = "Tempo Generalizado (min)", y = "Probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")),

ggplot() +
  geom_qq(aes(sample = scale(dados$tg), color = "A"), alpha = .8) +
  geom_abline(aes(intercept = 0, slope = 1, color = "B")) +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "Variável Dependente", breaks = c("A"), label = c("Tempo Generalizado (min)"), guide = "legend") +
  labs(title = "Gráfico Quantil-Quantil", subtitle = "n = 90", x = "Quantis Teóricos", y = "Quantis Observados") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")),

ncol = 2)

### COMPARAR MEDIAS E VARIANCIAS
dados %>% summarise(avg = mean(tg), s = sd(tg), coef_var = avg/s, var = s^2, n = n())

dadostp <- dados %>% filter(modo == "Transporte_Publico")
dadostp %>% summarise(avg = mean(tg), s = sd(tg), coef_var = avg/s, var = s^2, n = n())
dadosapp <- dados %>% filter(modo == "Transporte_Aplicativo")
dadosapp %>% summarise(avg = mean(tg), s = sd(tg), coef_var = avg/s, var = s^2, n = n())



# graficos Agregando por Modo de Transporte
grid.arrange(
dados %>% ggplot() +
  geom_point(aes(x = c(1:90), y = tg, color = modo), alpha = .8) +
  geom_boxplot(aes(x = c(1:90), y = tg, color = modo), alpha = 0) +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "Modo de Transporte", breaks = c("Transporte_Aplicativo", "Transporte_Publico"), label = c("T. Aplicativo", "T. Público"), guide = "legend") +
  labs(title = "Dispersão e Diagrama de Caixa", subtitle = "n_APP = 40 | n_PUB = 50", x = "Observação", y = "Tempo Generalizado (min)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")),

dados %>% ggplot() +
  geom_histogram(aes(x = tg, y = ..density.., fill = modo), position = "dodge", color = "white", alpha = .8, bins = 5) +
  geom_density(aes(x = tg, y = ..density.., fill = modo), alpha = .4) +
  scale_fill_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "Modo de Transporte", breaks = c("Transporte_Aplicativo", "Transporte_Publico"), label = c("T. Aplicativo", "T. Público"), guide = "legend") +
  labs(title = "Histograma de Frequência e Densidade de Probabilidade", subtitle = "n_APP = 40 | n_PUB = 50", x = "Tempo Generalizado (min)", y = "Probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")),

dados %>% ggplot() +
  stat_ecdf(aes(x = tg, color = modo), alpha = .8) +
  stat_ecdf(aes(x = qnorm(seq(0.003, 0.997, length = 90), mean = mean(tg), sd = sd(tg)), color = "B"), alpha = .8) +
  scale_color_manual(values = wes_palette(n = 3, name = "Darjeeling1"), name = "Variável", breaks = c("Transporte_Aplicativo", "Transporte_Publico", "B"), label = c("T. Aplicativo", "T. Público", "Distribuição Normal"), guide = "legend") +
  labs(title = "Distribuição de Probabilidade Acumulada", subtitle = "n_APP = 40 | n_PUB = 50", x = "Tempo Generalizado (min)", y = "Probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")),

dados %>% ggplot() +
  geom_qq(aes(sample = scale(tg), color = modo), alpha = .8) +
  geom_abline(aes(intercept = 0, slope = 1, color = "B")) +
  scale_color_manual(values = wes_palette(n = 3, name = "Darjeeling1"), name = "Variável", breaks = c("Transporte_Aplicativo", "Transporte_Publico"), label = c("T. Aplicativo", "T. Público"),  guide = "legend") +
  labs(title = "Gráfico Quantil-Quantil", subtitle = "n_APP = 40 | n_PUB = 50", x = "Quantis Teóricos", y = "Quantis Observados") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")),

ncol = 2)


# REGRESSAO LINEAR SIMPLES

# Transporte Publico
dados %>% ggplot(aes(idade, tg)) +
  geom_point(aes(color = modo), size = 2, alpha = .7) +
  labs(title = "Dispersão", subtitle = paste("n =", nrow(dados)), x = "Idade", y = "Tempo Generalizado (min)") +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "Modo de Transporte", breaks = c("Transporte_Publico", "Transporte_Aplicativo"), label = c("Transporte Público", "Transporte por Aplicativo"), guide = "legend") +
  theme_minimal()

dadostp %>% ggplot(aes(idade, tg)) +
  geom_point(size = 2, alpha = .7) +
  labs(title = "Dispersão do Modo Transporte Público", subtitle = paste("n =", nrow(dados$tp)), x = "Idade", y = "Tempo Generalizado (min)") +
  theme_minimal()


cor(dadostp$idade, dadostp$tg, method = "pearson")

cor(dadosapp$idade, dadosapp$tg, method = "pearson")

# Regressão Simples (Distancia VS. Idade)

rs_oni <- lm(data = dadostp, tg~idade)
summary(rs_oni)

rs_app <- lm(data = dadosapp, tg~idade)
summary(rs_app)

# graficos juntos
ggplot() +
  geom_point(data = dadostp, aes(idade, tg, color = "A"), size = 2, alpha = .7) +
  stat_smooth(data = dadostp, aes(y = tg, x = idade, color = "B"), method = lm) +
  geom_point(data = dadosapp, aes(idade, tg, color = "C"), size = 2, alpha = .7) +
  stat_smooth(data = dadosapp, aes(y = tg, x = idade, color = "D"), method = lm) +
  scale_color_manual(values = wes_palette(n = 4, name = "Darjeeling1"), name = "Legenda", breaks = c("A", "B", "C", "D"), label = c("T. Pub", "Regressão T. Pub", "T. App", "Regressão T. App"),  guide = "legend") +
  labs(title = "Dispersão e Regressão Linear", x = "Idade", y = "Tempo Generalizado (min)") +
  theme_minimal()

confint(rs_app)

# graficos separados
predtp <- data.frame(predict(rs_oni, interval = "prediction"))
predapp <- data.frame(predict(rs_app, interval = "prediction"))

grid.arrange(
  
ggplot() +
  geom_point(data = dadostp, aes(idade, tg, color = "A"), size = 2, alpha = .7) +
  stat_smooth(data = dadostp, aes(y = tg, x = idade, color = "B"), method = lm) +
  geom_line(aes(x = dadostp$idade, y = predtp$lwr), color = "red", linetype = "dashed") +
  geom_line(aes(x = dadostp$idade, y = predtp$upr), color = "red", linetype = "dashed") +
  scale_color_manual(values = wes_palette(n = 4, name = "Darjeeling1"), name = "Legenda", breaks = c("A", "B"), label = c("T. Pub.", "Regressão T. Pub."),  guide = "legend") +
  labs(title = "Transporte Público", x = "Idade", y = "Tempo Generalizado (min)") +
  theme_minimal(),

ggplot() +
  geom_point(data = dadosapp, aes(idade, tg, color = "A"), size = 2, alpha = .7) +
  stat_smooth(data = dadosapp, aes(y = tg, x = idade, color = "B"), method = lm) +
  geom_line(aes(x = dadosapp$idade, y = predapp$lwr), color = "red", linetype = "dashed") +
  geom_line(aes(x = dadosapp$idade, y = predapp$upr), color = "red", linetype = "dashed") +
  scale_color_manual(values = wes_palette(n = 4, name = "Darjeeling1"), name = "Legenda", breaks = c("A", "B"), label = c("T. App.", "Regressão T. App."),  guide = "legend") +
  labs(title = "Transporte por Aplicativo", x = "Idade", y = "Tempo Generalizado (min)") +
  theme_minimal(),

ncol = 2)


# Avaliação dos Resíduos
# Identificacao Outlier
grid.arrange(
  
ggplot() +
  geom_point(aes(x = dadostp$idade, y = rs_oni$residuals, color = "A"), alpha = .8) +
  geom_boxplot(aes(x = dadostp$idade, y = rs_oni$residuals, color = "B"), alpha = 0) +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "Variável Dependente", breaks = c("A"), label = c("Resíduos T. Generalizado"), guide = "legend") +
  labs(title = "Resíduos Transporte Público", subtitle = "n = 50", x = "Idade", y = "Resíduo do Tempo Generalizado (min)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")),

ggplot() +
  geom_point(aes(x = dadosapp$idade, y = rs_app$residuals, color = "A"), alpha = .8) +
  geom_boxplot(aes(x = dadosapp$idade, y = rs_app$residuals, color = "B"), alpha = 0) +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "Variável Dependente", breaks = c("A"), label = c("Resíduos T. Generalizado"), guide = "legend") +
  labs(title = "Resíduos Transporte Aplicativo", subtitle = "n = 50", x = "Idade", y = "Resíduo do Tempo Generalizado (min)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold")),

ncol = 2)

# Normalidade Shapiro Wilk
shapiro.test(rs_oni$residuals)
# O teste shapiro-wilk reforça a normalidade dos resíduos do TP ao não rejeitar a
# hipótese nula de normalidade dos resíduos.

shapiro.test(rs_app$residuals)
# O teste shapiro-wilk rejeita a hipotese nula de normalidade.

# Homocedasticidade - Teste Breusch-Pagan
bptest(rs_oni)
bptest(rs_app)
# O teste breusch-pagan rejeita a hipotese nula de homocedasticidade para os modelos de
# regressão do modo transporte publico (p-value 0.07243) e modo transporte por
# app (0.1612).

# Durbin-Watson Autocorrelação
dwtest(rs_oni)
dwtest(rs_app)
# O teste de autocorrelação rejeita a hipotese nula de independência para os modelos de
# regressão do modo transporte publico (p-value 0.1247) e modo transporte por
# app (0.7106).

# Matriz de Correlação
dados_cor <- dados %>% select(idade, raca, genero, motivo)

dados_cor[] <- lapply(dados_cor, as.integer)

sjp.corr(dados_cor, title = "Matriz de Correlação (Pearson)", corr.method = "pearson", p.numeric = TRUE) +
  labs(subtitle = "Coeficiente de Correlação (valor-p)") +
  theme_minimal()

sjp.chi2(dados_cor) # não utilizado


# Regressão Multipla Completa
rm_oni <- lm(data = dadostp, tg~idade+genero+raca+motivo)
summary(rm_oni)
anova(rm_oni)

rm_app <- lm(data = dadosapp, tg~idade+genero+raca+motivo)
summary(rm_app)
anova(rm_app)

# Normalidade Shapiro Wilk
shapiro.test(rm_oni$residuals)
shapiro.test(rm_app$residuals)


# Homocedasticidade - Teste Breusch-Pagan
bptest(rm_oni)
bptest(rm_app)


# Durbin-Watson Autocorrelação
dwtest(rm_oni)
dwtest(rm_app)


# Regressão Multipla Parcial
dados_cor2 <- dados %>% select(idade, raca, motivo)

dados_cor2[] <- lapply(dados_cor2, as.integer)

sjp.corr(dados_cor2, title = "Matriz de Correlação (Pearson)", corr.method = "pearson", p.numeric = TRUE) +
  labs(subtitle = "Coeficiente de Correlação (valor-p)") +
  theme_minimal()



rp_oni <- lm(data = dadostp, tg~idade+raca+motivo)
summary(rp_oni)
anova(rp_oni)

rp_app <- lm(data = dadosapp, tg~idade+raca+motivo)
summary(rp_app)
anova(rp_app)

# Normalidade Shapiro Wilk
shapiro.test(rp_oni$residuals)
shapiro.test(rp_app$residuals)


# Homocedasticidade - Teste Breusch-Pagan
bptest(rp_oni)
bptest(rp_app)


# Durbin-Watson Autocorrelação
dwtest(rp_oni)
dwtest(rp_app)

# Multicolinearidade
vif(rm_oni)
vif(rm_app)
vif(rp_oni)
vif(rp_app)

# Cook's Distance (outlier)
lm <- rm_oni
q <- ggplot(data.frame(cooks.distance(lm))) +
  geom_point(aes(x = c(1:length(cooks.distance.lm.)),y = cooks.distance.lm.)) +
  geom_abline(intercept = 0.25, slope = 0, color = " red") +
  geom_text(aes(label = "Critério Outlier 25%", x = 20, y = 0.26), color = "red") +
  labs(title = "Cook's D - T. Público", subtitle = "Regressão Linear Múltipla Completa", x = "", y = "D") +
  ylim(0, .3) +
  theme_minimal()

lm <- rm_app
p <- ggplot(data.frame(cooks.distance(lm))) +
  geom_point(aes(x = c(1:length(cooks.distance.lm.)),y = cooks.distance.lm.)) +
  geom_abline(intercept = 0.25, slope = 0, color = " red") +
  geom_text(aes(label = "Critério Outlier 25%", x = 20, y = 0.26), color = "red") +
  labs(title = "Cook's D - T. por Aplicativo", subtitle = "Regressão Linear Múltipla Completa", x = "", y = "D") +
  ylim(0, .3) +
  theme_minimal()

grid.arrange(q, p, ncol = 2)


lm <- rp_oni
q <- ggplot(data.frame(cooks.distance(lm))) +
  geom_point(aes(x = c(1:length(cooks.distance.lm.)),y = cooks.distance.lm.)) +
  geom_abline(intercept = 0.25, slope = 0, color = " red") +
  geom_text(aes(label = "Critério Outlier 25%", x = 20, y = 0.26), color = "red") +
  labs(title = "Cook's D - T. Público", subtitle = "Regressão Linear Múltipla Parcial", x = "", y = "D") +
  ylim(0, .3) +
  theme_minimal()

lm <- rp_app
p <- ggplot(data.frame(cooks.distance(lm))) +
  geom_point(aes(x = c(1:length(cooks.distance.lm.)),y = cooks.distance.lm.)) +
  geom_abline(intercept = 0.25, slope = 0, color = " red") +
  geom_text(aes(label = "Critério Outlier 25%", x = 20, y = 0.26), color = "red") +
  labs(title = "Cook's D - T. por Aplicativo", subtitle = "Regressão Linear Múltipla Parcial", x = "", y = "D") +
  ylim(0, .3) +
  theme_minimal()

grid.arrange(q, p, ncol = 2)

# FIM Cook's Distance (outlier)


# INTERVALO DE PREDICAO
pred <- predict(rp_app, interval = "prediction")
fwrite(pred, file = "temp1.csv", sep = ";", dec = ",")



# FIM INTERVALO DE PREDICAO

#Escolha do Modelo BIC
BIC(rs_oni, rs_app, rm_oni, rm_app, rp_oni, rp_app)

# FIM escolha do modelo BIC

# GRAFICO 3D
library(scatterplot3d)
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')

plot3dtp <- scatterplot3d(x = dadostp$tg, y = dadostp$idade, z = dadostp$genero, box = FALSE, color = "red", angle = 60, pch = 20, xlab = "Tempo Generalizado (min)", zlab = "Gênero", ylab = "Idade")
lm3dtp <- lm(data = dadostp, idade ~ tg + genero)
plot3dtp$plane3d(lm3dtp)
addgrids3d(x = dadostp$tg, y = dadostp$idade, z = dadostp$genero, grid = c("Xy","xz","yz"), angle = 60)
