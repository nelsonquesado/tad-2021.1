### TRABALHO PRÁTICO 3 – ANÁLISE DESCRITIVA
### ALUNOS: Nelson Quesado, Renata Farias e Rúbia Rodrigues

# bibliotecas utilizadas
library(tidyverse)
library(gridExtra)
library(dplyr)
library(data.table)
library(matrixStats)

# carregar os dados a partir do arquivo dados_tp1.csv
dados_tp1 <- read.table(file = "dados_tp1.csv", sep = ";", header = TRUE, fill = TRUE)

dados <- data.frame(idade = dados_tp1$idade, distancia = dados_tp1$distancia_media_km, genero = dados_tp1$genero, raca = dados_tp1$raca, modo = dados_tp1$modo, motivo = dados_tp1$motivo) %>%
  mutate(idade = as.numeric(idade))
head(dados)

# Variável: Distância de Viagem (km)
populacao <- sort(dados$distancia)
populacao

# ANALISE DESCRITIVA
# Estatísticas
desc <- data.frame(n = length(populacao), media = mean(populacao), desvio_padrao = sd(populacao), coef_var = sd(populacao)/mean(populacao), variancia = var(populacao))
print(desc) # principais estatísticas descritivas

quartis <- summary(populacao)
print(quartis) # apresentação dos quartis

# FORMA

# Dispersão e Diagrama de Caixa
data.frame(populacao) %>% ggplot() +
  geom_boxplot(aes(c(1:90),populacao), colour = "#7d6b7d") +
  geom_point(aes(c(1:90), populacao), colour = "#FF665A") +
  labs(title = "Dispersão | Diagrama de Caixa", subtitle = paste("n = ", desc$n), x = "Observação", y = "Distância de Viagem (km)") +
  theme_minimal()

  # Histograma e Densidade de Probabilidade
data.frame(populacao) %>% ggplot(aes(populacao)) +
  geom_histogram(aes(y = ..density..), fill = "#7D6B7D", color = "white", bins = 10) +
  geom_density(fill = "#FF665A", alpha = .5) +
  labs(title = "Histograma e Densidade de Probabilidade", subtitle = paste("n = ", desc$n), x = "Distância de Viagem (km)", y = "Probabilidade") +
  theme_minimal()

# Densidade de Probabilidade + Normal + Poisson + Exponencial
ggplot() +
  geom_density(aes(populacao, fill = "#7D6B7D")) +
  geom_density(aes(qexp(seq(0.01, 0.99, length = 90), 1/desc$media), fill = "#FF665A"), alpha = .2) + # dist exponencial
  geom_density(aes(qpois(seq(0.01, 0.99, length = 90), desc$media), fill = "cyan"), alpha = .2) + # dist poisson
  geom_density(aes(qnorm(seq(0.01, 0.99, length = 90), desc$media, desc$desvio_padrao), fill = "#FFF587"), alpha = .2) + # dist normal
  scale_fill_identity(name = "Distribuicão", breaks = c("#7D6B7D", "#FFF587", "cyan", "#FF665A"), label = c("Populacional", "Normal", "Poisson", "Exponencial"), guide = "legend") +
  labs(title = " Distribuição de Probabilidade Populacional, Normal, Poisson e Exponencial", subtitle = paste("n = ", desc$n), x = "Distância de Viagem (km)", y = "Probabilidade") +
  theme_minimal()

# Probabilidade Acumulada
ggplot() +
  stat_ecdf(aes(populacao, color = "#7D6B7D")) +
  stat_ecdf(aes(qexp(seq(0.01, 0.99, length = 90), 1/desc$media), colour = "#FC0D85")) + # dist exponencial
  stat_ecdf(aes(qpois(seq(0.01, 0.99, length = 90), desc$media), colour = "#FF8C64")) + # dist poisson
  stat_ecdf(aes(qnorm(seq(0.01, 0.99, length = 90), desc$media, desc$desvio_padrao), colour = "#0BDEA2")) + # dist normal
  scale_colour_identity(name = "Distribuicão", breaks = c("#7D6B7D", "#0BDEA2", "#FF8C64", "#FC0D85"), label = c("Populacional", "Normal", "Poisson", "Exponencial"), guide = "legend") +
  labs(title = " Distribuição de Probabilidade Populacional, Normal, Poisson e Exponencial", subtitle = paste("n = ", desc$n), x = "Distância de Viagem (km)", y = "Probabilidade") +
  theme_minimal()

# SIMULAÇÃO DE MONTE CARLO

B <- 10000 # quantidade de simulações por tamanho amostral

mc <- function(x) { # função replicação de médias (monte carlo)
  replicate(B, {
  sample(populacao, x, replace = TRUE)
  } 
)}

n_5 <- data.frame(t(replicate(B, sample(populacao, 5, replace = TRUE))))
n_5 <- n_5 %>% mutate(media = rowMeans(n_5), var = rowSds(as.matrix(n_5))^2)

n_10 <- data.frame(t(replicate(B, sample(populacao, 10, replace = TRUE))))
n_10 <- n_10 %>% mutate(media = rowMeans(n_10), var = rowSds(as.matrix(n_10))^2)

n_20 <- data.frame(t(replicate(B, sample(populacao, 20, replace = TRUE))))
n_20 <- n_20 %>% mutate(media = rowMeans(n_20), var = rowSds(as.matrix(n_20))^2)

n_30 <- data.frame(t(replicate(B, sample(populacao, 30, replace = TRUE))))
n_30 <- n_30 %>% mutate(media = rowMeans(n_30), var = rowSds(as.matrix(n_30))^2)


# Histogramas Média vs. normal

histn5 <- ggplot() +
  geom_histogram(aes(n_5$media, y = ..density..), fill = "#7D6B7D", color = "gray", bins = 30) +
  geom_density(aes(qnorm(seq(0.01, 0.99, length = B), desc$media, desc$desvio_padrao/sqrt(5))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 20) +
  ylim(0, 0.5) +
  labs(title = "Médias Amostrais vs. Normal", subtitle = "n = 05", x = "", y = "Frequência") +
  theme_minimal()

histn10 <- ggplot() +
  geom_histogram(aes(n_10$media, y = ..density..), fill = "#FFF587", color = "gray", bins = 30) +
  geom_density(aes(qnorm(seq(0.01, 0.99, length = B), desc$media, desc$desvio_padrao/sqrt(10))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 20) +
  ylim(0, 0.5) +
  labs(title = "", subtitle = "n = 10", x = "Distância Média de Viagem (km)") +
  theme_minimal() +
  theme(axis.title.y = element_blank() , axis.text.y = element_blank(), axis.ticks.y = element_blank())

histn20 <- ggplot() +
  geom_histogram(aes(n_20$media, y = ..density..), fill = "#FF8C64", color = "gray", bins = 30) +
  geom_density(aes(qnorm(seq(0.01, 0.99, length = B), desc$media, desc$desvio_padrao/sqrt(20))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 20) +
  ylim(0, 0.5) +
  labs(title = "", subtitle = "n = 20", x = "") +
  theme_minimal() +
  theme(axis.title.y = element_blank() , axis.text.y = element_blank(), axis.ticks.y = element_blank())

histn30 <- ggplot() +
  geom_histogram(aes(n_30$media, y = ..density..), fill = "#FF6654", color = "gray", bins = 30) +
  geom_density(aes(qnorm(seq(0.01, 0.99, length = B), desc$media, desc$desvio_padrao/sqrt(30))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 20) +
  ylim(0, 0.5) +
  labs(title = "", subtitle = "n = 30", x = "") +
  theme_minimal() +
  theme(axis.title.y = element_blank() , axis.text.y = element_blank(), axis.ticks.y = element_blank())

grid.arrange(histn5, histn10, histn20, histn30, ncol = 4)


# Histogramas Variância vs. chiq

histq5var <- ggplot() +
  geom_histogram(aes(n_5$var, y = ..density..), fill = "#7D6B7D", color = "gray", bins = 30) +
  geom_density(aes(qchisq(seq(0.01, 0.99, length = B), df = 4)), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.2) +
  labs(title = "Variâncias Amostrais (distância média de viagem - km) vs. Qui-Quadrado", subtitle = "n = 05", x = "", y = "Frequência") +
  theme_minimal()

histq10var <- ggplot() +
  geom_histogram(aes(n_10$var, y = ..density..), fill = "#FFF587", color = "gray", bins = 30) +
  geom_density(aes(qchisq(seq(0.01, 0.99, length = B), df = 9)), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.2) +
  labs(title = "", subtitle = "n = 10", x = "", y = "") +
  theme_minimal()

histq20var <- ggplot() +
  geom_histogram(aes(n_20$var, y = ..density..), fill = "#FF8C64", color = "gray", bins = 30) +
  geom_density(aes(qchisq(seq(0.01, 0.99, length = B), df = 19)), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.2) +
  labs(title = "", subtitle = "n = 20", x = "", y = "") +
  theme_minimal()

histq30var <- ggplot() +
  geom_histogram(aes(n_30$var, y = ..density..), fill = "#FF6654", color = "gray", bins = 30) +
  geom_density(aes(qchisq(seq(0.01, 0.99, length = B), df = 29)), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.2) +
  labs(title = "", subtitle = "n = 30", x = "", y = "") +
  theme_minimal()

# Histogramas Variância vs. normal

histn5var <- ggplot() +
  geom_histogram(aes(n_5$var, y = ..density..), fill = "#7D6B7D", color = "gray", bins = 30) +
  geom_density(aes(qnorm(seq(0.01, 0.99, length = B), desc$media, desc$desvio_padrao/sqrt(5))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.4) +
  labs(title = "Variâncias Amostrais (distância média de viagem - km) vs. Normal", x = "", y = "Frequência") +
  theme_minimal()

histn10var <- ggplot() +
  geom_histogram(aes(n_10$var, y = ..density..), fill = "#FFF587", color = "gray", bins = 30) +
  geom_density(aes(qnorm(seq(0.01, 0.99, length = B), desc$media, desc$desvio_padrao/sqrt(10))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.4) +
  labs(title = "", x = "", y = "") +
  theme_minimal()

histn20var <- ggplot() +
  geom_histogram(aes(n_20$var, y = ..density..), fill = "#FF8C64", color = "gray", bins = 30) +
  geom_density(aes(qnorm(seq(0.01, 0.99, length = B), desc$media, desc$desvio_padrao/sqrt(20))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.4) +
  labs(x = "", title = "", y = "") +
  theme_minimal()

histn30var <- ggplot() +
  geom_histogram(aes(n_30$var, y = ..density..), fill = "#FF6654", color = "gray", bins = 30) +
  geom_density(aes(qnorm(seq(0.01, 0.99, length = B), desc$media, desc$desvio_padrao/sqrt(30))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.4) +
  labs(title = "", x = "", y = "") +
  theme_minimal()

# Histogramas Variância vs. Exp

histe5var <- ggplot() +
  geom_histogram(aes(n_5$var, y = ..density..), fill = "#7D6B7D", color = "gray", bins = 30) +
  geom_density(aes(qexp(seq(0.01, 0.99, length = B), sqrt(5)/desc$media)), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.5) +
  labs(title = "Variâncias Amostrais (distância média de viagem - km) vs. Exponencial", x = "", y = "Frequência") +
  theme_minimal()

histe10var <- ggplot() +
  geom_histogram(aes(n_10$var, y = ..density..), fill = "#FFF587", color = "gray", bins = 30) +
  geom_density(aes(qexp(seq(0.01, 0.99, length = B), sqrt(10)/desc$media)), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.5) +
  labs(title = "", x = "", y = "") +
  theme_minimal()

histe20var <- ggplot() +
  geom_histogram(aes(n_20$var, y = ..density..), fill = "#FF8C64", color = "gray", bins = 30) +
  geom_density(aes(qexp(seq(0.01, 0.99, length = B), sqrt(20)/desc$media)), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.5) +
  labs(title = "", x = "", y = "") +
  theme_minimal()

histe30var <- ggplot() +
  geom_histogram(aes(n_30$var, y = ..density..), fill = "#FF6654", color = "gray", bins = 30) +
  geom_density(aes(qexp(seq(0.01, 0.99, length = B), sqrt(30)/desc$media)), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.5) +
  labs(title = "", x = "", y = "") +
  theme_minimal()

# Histogramas Variância vs. Poisson

histp5var <- ggplot() +
  geom_histogram(aes(n_5$var, y = ..density..), fill = "#7D6B7D", color = "gray", bins = 30) +
  geom_density(aes(qpois(seq(0.01, 0.99, length = B), desc$media/sqrt(5))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.5) +
  labs(title = "Variâncias Amostrais (distância média de viagem - km) vs. Poisson", x = "", y = "Frequência") +
  theme_minimal()

histp10var <- ggplot() +
  geom_histogram(aes(n_10$var, y = ..density..), fill = "#FFF587", color = "gray", bins = 30) +
  geom_density(aes(qpois(seq(0.01, 0.99, length = B), desc$media/sqrt(10))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.5) +
  labs(title = "", x = "", y = "") +
  theme_minimal()

histp20var <- ggplot() +
  geom_histogram(aes(n_20$var, y = ..density..), fill = "#FF8C64", color = "gray", bins = 30) +
  geom_density(aes(qpois(seq(0.01, 0.99, length = B), desc$media/sqrt(20))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.5) +
  labs(title = "", x = "", y = "") +
  theme_minimal()

histp30var <- ggplot() +
  geom_histogram(aes(n_30$var, y = ..density..), fill = "#FF6654", color = "gray", bins = 30) +
  geom_density(aes(qpois(seq(0.01, 0.99, length = B), desc$media/sqrt(30))), fill = "#A3A1A8", alpha = 0.35) +
  xlim(0, 75) +
  ylim(0, 0.5) +
  labs(title = "", x = "", y = "") +
  theme_minimal()

grid.arrange(histq5var, histq10var, histq20var, histq30var, histn5var, histn10var, histn20var, histn30var, histe5var, histe10var, histe20var, histe30var, histp5var, histp10var, histp20var, histp30var, ncol = 4, nrow = 4)
grid.arrange(histq5var, histq10var, histq20var, histq30var, ncol = 4)
