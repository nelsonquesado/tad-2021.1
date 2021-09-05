# UNIVERSIDADE FEDERAL DO CEARÁ
# CENTRO DE TECNOLOGIA - DEPARTAMENTO DE ENGENHARIA DE TRANSPORTES
# TCP7321 – TAD				  		PROF. MANOEL CASTRO 

# Teste 6	- Estimação Intervalar e teste de aderência							03/07/21

# Grupo:Nelson, Renata e Rúbia

# bibliotecas utilizadas
library(tidyverse)
library(data.table)

# 01)  Considerem X uma variável que representa o Índice de Gravidade Global
# (IGG) de um trecho de rodovia. O IGG é um indicador da qualidade do pavimento
# do trecho. Os valores coletados de uma amostra de 196 trechos de uma malha
# rodoviária estão no arquivo Excel, na pasta do teste 6. Esses dados vieram do
# TP1 de uma das equipes da disciplina.

x <- sort(c(135, 3, 3, 6, 8, 3, 3, 3, 5, 22, 22, 9, 16, 0, 0, 0, 25, 5, 0, 90, 86, 70, 53, 53, 53, 51, 53, 53, 13, 3, 5, 3, 3, 3, 38, 53, 2, 2, 8, 2, 4, 3, 2, 83, 49, 83, 73, 73, 56, 62, 47, 42, 49, 49, 46, 45, 49, 40, 75, 56, 47, 55, 40, 57, 45, 48, 54, 72, 56, 60, 58, 60, 58, 108, 2, 40, 39, 35, 117, 148, 153, 173, 213, 189, 186, 132, 40, 48, 47, 118, 126, 146, 135, 164, 121, 174, 157, 121, 117, 185, 53, 89, 91, 123, 64, 134, 29, 71, 78, 84, 32, 41, 148, 173, 58, 36, 122, 83, 83, 54, 24, 56, 34, 62, 33, 95, 99, 25, 47, 92, 93, 29, 108, 82, 144, 258, 184, 184, 175, 163, 229, 205, 160, 102, 100, 150, 158, 242, 192, 146, 150, 189, 169, 180, 144, 139, 130, 119, 111, 115, 116, 156, 150, 145, 138, 158, 122, 146, 180, 196, 209, 79, 58, 55, 42, 65, 0, 63, 70, 94, 138, 86, 95, 74, 136, 119, 88, 126, 81, 90, 119, 81, 3, 14, 17, 6))

# ANALISE DESCRITIVA
# Estatísticas
desc <- data.frame(n = length(x), media = mean(x), desvio_padrao = sd (x), coef_var = sd(x)/mean(x), variancia = var(x))
print(desc) # principais estatísticas descritivas

quartis <- summary(x)
print(quartis) # apresentação dos quartis

# Dispersão e Diagrama de Caixa
data.frame(x) %>% ggplot() +
  geom_boxplot(aes(seq(1, desc$n, 1), x), colour = "#1F766C") +
  geom_point(aes(seq(1, desc$n, 1), x), colour = "#CD3F1C") +
  labs(title = "Dispersão | Diagrama de Caixa", subtitle = paste("n = ", desc$n), x = "Observação", y = "IGG") +
  theme_minimal()

# Histograma e Densidade de Probabilidade
data.frame(x) %>% ggplot(aes(x)) +
  geom_histogram(aes(y = ..density..), fill = "#1D353F", color = "white", bins = 9) +
  geom_density(fill = "#CD3F1C", alpha = .5) +
  labs(title = "Histograma e Densidade de Probabilidade", subtitle = paste("n = ", desc$n), x = "IGG", y = "Probabilidade") +
  theme_minimal()

# Densidade de Probabilidade + Normal
ggplot() +
  geom_density(aes(x, fill = "#1D353F")) +
  geom_density(aes(qnorm(seq(0.01, 0.99, length = desc$n), desc$media, desc$desvio_padrao), fill = "#CD3F1C"), alpha = .3) + # dist normal
  scale_fill_identity(name = "Distribuicão", breaks = c("#1D353F", "#CD3F1C"), label = c("Populacional", "Normal"), guide = "legend") +
  labs(title = "Densidade de Probabilidade Populacional e Normal", subtitle = paste("n = ", desc$n), x = "IGG", y = "Probabilidade") +
  theme_minimal()

# Probabilidade Acumulada
ggplot() +
  stat_ecdf(aes(x, color = "#1D353F")) +
  stat_ecdf(aes(qnorm(seq(0.01, 0.99, length = desc$n), desc$media, desc$desvio_padrao), colour = "#CD3F1C")) + # dist normal
  scale_colour_identity(name = "Distribuicão", breaks = c("#1D353F", "#CD3F1C"), label = c("Populacional", "Normal"), guide = "legend") +
  labs(title = "Distribuição Acumulada de Probabilidade Populacional e Normal", subtitle = paste("n = ", desc$n), x = "IGG", y = "Probabilidade") +
  theme_minimal()

# Gráfico Quantil-Quantil
data.frame(x) %>%
  ggplot(aes(sample = scale(x))) +
  geom_qq(color = "#CD3F1C", alpha = 0.75) +
  geom_abline(color = "#1F766C") +
  labs(title = "Gráfico Quantil-Quantil", subtitle = paste("n = ", desc$n), x = "Quantil Teórico", y = "Quantil Amostral") +
  xlim(-3, 3) +
  ylim(-3, 3) +
  theme_minimal()

# a)	Avalie a hipótese de normalidade de X por meio de um teste qui^2.
# Explicite as hipóteses nula e alternativa, o nível de significância, o erro
# tipo 1, o gráfico da distribuição qui2 e a região de rejeição da hipótese
# nula, o valor-p, o resultado do teste e a sua conclusão. Explicite as
# premissas.

# Teste Qui-Quadrado: comparação da distribuição de frequência amostral com a
# distribuição de frequência normal.

perc <- function(n) { # função percentil
  sum(x <= n)/desc$n
  }

q <- c(seq(20, 200, 20), 260) # base do percentil: 0 a 275, de 25 em 25
q


percentis_observados <- sapply(q, perc)
percentis_teoricos <- pnorm(q, mean = desc$media, sd = desc$desvio_padrao)

freqs <- data.frame(q, freq_obs = percentis_observados*desc$n, freq_teo = percentis_teoricos*desc$n)
freqs <- freqs %>% mutate(
  freq_obs = ifelse(is.na(freq_obs - shift(freq_obs, 1L, type = "lag")), freq_obs, freq_obs - shift(freq_obs, 1L, type = "lag")),
  freq_teo = ifelse(is.na(freq_teo - shift(freq_teo, 1L, type = "lag")), freq_teo, freq_teo - shift(freq_teo, 1L, type = "lag"))
  )

freqs

# Comparação Visual de Histograma Observado vs. Normal
# O histograma dos valores observados apresenta forma semelhante à distribuição
# teórica (normal).

gather(freqs, Percentil, quant, freq_obs:freq_teo) %>%
  ggplot() +
  geom_bar(aes(x = q, y = quant, fill = Percentil), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#CD3F1C", "#1F766C"), labels = c("Observado", "Teórico (Normal)"), name = "Distribuição") +
  labs(title = "Histograma de Frequências Observadas e Teóricas (Normal)", subtitle = paste("n = ", desc$n), x = "IGG", y = "Frequência Relativa") +
  theme_minimal()

# Memorial de Cálculo Qui-Quadrado
freqs <- freqs %>% mutate(qui2 = ((freq_obs - freq_teo)^2)/freq_teo)
freqs



df <- length(q) - 1 # graus de liberdade
df

# H0: a amostra se origina de uma população normal.
# Ha: a amostra não se origina de uma população normal.

# Qui-quadrado crítivo para rejeição da hipótese H0 é
qui_critico <- qchisq(0.05, df = df)
qui_critico

# Qui-quadrado do teste
qui2 <- sum(freqs$qui2)
qui2

# Avaliação critério hipótese nula
qui_critico >= qui2 # Falso, ou seja, hipótese nula é rejeitada. A Amostra não
# se origina de uma população normal.

ggplot() +
  geom_density(aes(qchisq(seq(0.01, 0.99, 0.01), df = df)), fill = "#CD3F1C", alpha = .4) +
  geom_vline(aes(xintercept = qui2), color = "#1F766C", size = 0.8, linetype = 2) +
  labs(title = "Distribuição Qui-quadrado", subtitle = paste("df = ", df), x = "IGG", y = "Densidade") + 
  geom_text(aes(x = 28, y = 0.005, label = round(qui2, 2)), fontface = "bold") +
  xlim(0, 35) +
  theme_minimal()

prob_amostra <- 1-pchisq(qui2, df = df)
prob_amostra # probabilidade da amostra se originar de uma distribuição normal.


# b)	Como você classifica o IGG médio da malha rodoviária, tomando por base a
# classificação da tabela abaixo? Utilize um IC de confiança de 95% e explicite
# as premissas da sua análise.

# Conceitos	      Limites
# Ótimo	          0 < IGG ≤ 20
# Bom             20 < IGG ≤ 40
# Regular	        40 < IGG ≤ 80
# Ruim            80 < IGG ≤ 160
# Péssimo	        IGG > 160


inf <- qt(0.025, desc$n - 1)*desc$desvio_padrao/sqrt(196) + desc$media
sup <- qt(0.975, desc$n - 1)*desc$desvio_padrao/sqrt(196) + desc$media


ggplot() +
  geom_density(aes(qt(seq(0.01, 0.99, 0.01), desc$n - 1)*desc$desvio_padrao/sqrt(196) + desc$media), size = 0.8) +
  geom_vline(aes(xintercept = desc$media), color = "#1F766C", size = 0.8) +
  geom_text(aes(x = desc$media + 2, y = 0.0005, label = round(desc$media, 2)), fontface = "bold") +
  geom_vline(aes(xintercept = inf), color = "#1F766C", size = 0.8, linetype = 2) +
  geom_text(aes(x = inf + 2, y = 0.0005, label = round(inf, 2)), fontface = "bold") +
  geom_vline(aes(xintercept = sup), color = "#1F766C", size = 0.8, linetype = 2) +
  geom_text(aes(x = sup + 2, y = 0.0005, label = round(sup, 2)), fontface = "bold") +
  labs(title = "Distribuição T-Student e Estimativa Intervalar da Média", subtitle = "Intervalo de Confiança 95%", x = "IGG", y = "Densidade") + 
  theme_minimal()

# Classificaria entre regular e ruim.

# c)	Estimem a variância e o desvio padrão de X. Explicite as premissas.

# Resposta:
# Intervalo de confiança da variância (90%)

varinf <- (desc$n-1)*(desc$variancia)/qchisq(0.95, df = desc$n - 1)
varsup <- (desc$n-1)*(desc$variancia)/qchisq(0.05, df = desc$n - 1)

print(c(varinf, varsup)) # Intervalo de confiança da variância (90%)

ggplot() +
  geom_density(aes(qchisq(seq(0.01, 0.99, 0.01), df = desc$n - 1)), fill = "#CD3F1C", alpha = .4) +
  geom_vline(aes(xintercept = desc$variancia), color = "#1F766C", size = 0.8, linetype = 2) +
  geom_vline(aes(xintercept = varinf), color = "red", size = 0.8) +
  geom_vline(aes(xintercept = varsup), color = "blue", size = 0.8) +
  labs(title = "Distribuição Qui-quadrado", subtitle = paste("df = ", desc$n - 1), x = "IGG", y = "Densidade") + 
  theme_minimal()


# Intervalo de confiança do desvio padrao (90%)

dpinf <- sqrt((desc$n-1)*(desc$variancia)/qchisq(0.95, df = desc$n - 1))
dpsup <- sqrt((desc$n-1)*(desc$variancia)/qchisq(0.05, df = desc$n - 1))

print(c(dpinf, dpsup)) # Intervalo de confiança do desvio padrao (90%)

ggplot() +
  geom_density(aes(qchisq(seq(0.01, 0.99, 0.01), df = desc$n - 1)), fill = "#CD3F1C", alpha = .4) +
  geom_vline(aes(xintercept = desc$desvio_padrao), color = "#1F766C", size = 0.8, linetype = 2) +
  geom_vline(aes(xintercept = dpinf), color = "#1F766C", size = 0.8, linetype = 2) +
  geom_vline(aes(xintercept = dpsup), color = "#1F766C", size = 0.8, linetype = 2) +
  labs(title = "Distribuição Qui-quadrado", subtitle = paste("df = ", desc$n -1), x = "IGG", y = "Densidade") + 
  theme_minimal()