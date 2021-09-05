## SCRIPT REFERENTE AO TP2 DA DISCIPLINA TECNICA DE ANALISE DE DADOS DE TRANSPORTES 2021.1
## PROF. DR. MANOEL CASTRO NETO
## NELSON QUESADO, RENATA FARIAS E RUBIA RODRIGUES

## CHEGADA DE VEICULOS EM UMA RODOVIA EM VEICULOS / 5 MIN.
## DADOS COLETADOS ENTRE 12:00 E 12:59 DIA UTIL ENTRE 01 E 07 DE ABRIL DE 2021
# FONTE: LAÇO DETECTOR PeMS

library(tidyverse)

dados1 <- read.table(file = "dados1.txt", header = TRUE, fill = TRUE, row.names = as.character(c(1:60)))
dados2 <- read.table(file = "dados2.csv", sep = ";", header = TRUE, fill = TRUE, row.names = as.character(c(1:60)))

flow <- c(dados1$Flow, dados2$Flow)

# ANALISE DESCRITIVA
desc <- data.frame(n = length(flow), media = mean(flow), desvio_padrao = sd(flow), coef_var = sd(flow)/mean(flow), variancia = var(flow))
print(desc) # principais estatísticas descritivas

desc$variancia/desc$media # a variância é  2,22 x a média

quartis <- summary(flow)
print(quartis)  # apresentação dos quartis

# Dispersão
data.frame(flow) %>% ggplot() +
  geom_point(aes(c(1:120),flow)) +
  labs(title = "Dispersão", subtitle = "n = 120", x = "Observação", y = "Fluxo (vei/5 min)") +
  theme_minimal()

# Boxplot
data.frame(flow) %>% ggplot() +
  geom_boxplot(aes(flow)) +
  labs(title = "Diagrama de Caixa", subtitle = "n = 120", x = "Fluxo (vei/5 min)") +
  theme_minimal()

# Histograma + Densidade
data.frame(flow) %>% ggplot(aes(flow)) +
  geom_histogram(aes(y = ..density..), bins = 15) +
  geom_density(fill = "#7AF0E8", alpha = 0.5) +
  labs(title = "Histograma de Frequência", subtitle = "n = 120", x = "Fluxo (vei/5 min)", y = "Probabilidade") +
  theme_minimal()


# Distribuição de Frequência (Comparativo)
data.frame(fluxo = c(flow, qpois(seq(0.01, 0.99, length = 120), desc$media)), Distribuicao = rep(c("Real", "Poisson"), times = c(120, 120))) %>%
ggplot() +
  geom_histogram(aes(fluxo, fill = Distribuicao), position = "dodge", bins = 15) +
  scale_fill_manual(values = c("#666666", "#7AF0E8")) +
  labs(title = "Histograma de Frequência", subtitle = "n = 120", x = "Fluxo (vei/5 min)", y = "Frequência") +
  theme_minimal()


# Distribuição de Probabilidade Acumulada
ggplot() +
  stat_ecdf(aes(flow, color = "#0BDEA2")) +
  stat_ecdf(aes(qpois(seq(0.01, 0.99, length = 120), desc$media), color = "#222222")) +
  labs(title = "Distribuição Acumulada", x = "Fluxo (vei/5 min)", y = "Probabilidade") +
  scale_color_identity(name = "Curva", breaks = c("#222222", "#0BDEA2"), labels = c("Poisson", "Real "), guide = "legend") +
  theme_minimal()

# Gráfico QQ

p <- seq(0.05, 0.95, 0.05) #base do quantil: de 5% a 95% a cada 5%
observed_quantiles <- quantile(flow, p)
theoretical_quantiles <- qnorm(p, mean = desc$media, sd = desc$desvio_padrao)

data.frame(Quantis_Teoricos = theoretical_quantiles, Quantis_Observados = observed_quantiles) %>%
  ggplot(aes(Quantis_Teoricos, Quantis_Observados)) +
  geom_point(color = "#222222") +
  labs(title = "Gráfico Quantil-Quantil", subtitle = "n = 120", x = "Quantis Teóricos", y = "Quantis Observados") +
  geom_abline(slope = 1, color = "#0BDEA2") +
  theme_minimal()

Diferencas <- data.frame(Quantis_Teoricos = theoretical_quantiles, Quantis_Observados = observed_quantiles) %>% mutate(Diferenca = Quantis_Teoricos - Quantis_Observados)

print(Diferencas)

# Critério 1 - Diferença Máxima entre Quantis (aceitar max 5%)
max(Diferencas$Diferenca)/desc$media

# Critério 2 - Soma das Diferenças (aceitar max 25%)
sum(abs(Diferencas$Diferenca))/desc$media

# Critério 3 - Proporção de quantis observados acima de 5% da média em relação aos quantis teóricos (aceitar max 10%)
mean(Diferencas$Diferenca > 0.05*desc$media)
