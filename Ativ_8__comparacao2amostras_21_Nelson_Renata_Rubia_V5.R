# UNIVERSIDADE FEDERAL DO CEARÁ
# PROGRAMA DE PÓS-GRADUAÇÃO EM ENGENHARIA DE TRANSPORTES – PETRAN/UFC
# TCP 7321 – TÉCNICAS DE ANÁLISE DE DADOS EM TRANSPORTES  -  PROF. MANOEL CASTRO

# Atividade 8 - Comparação de duas amostras 							19/07/21

# Grupo: Nelson Quesado, Renata Farias e Rubia Rodrigues

# 1)	Vejam abaixo dados de contagem volumétrica de veículos nas avenidas Santos
# Dumont e Dom Luiz, em dias típicos, próximo à Av. Senador Virgílio Távora.
# Os dados foram coletados para comparar se os volumes veiculares antes da
# implantação do binário (2013) são estatisticamente diferentes dos volumes
# veiculares depois da implantação do binário (2014). Façam um teste de hipóteses
# para realizar essa análise. Indiquem as hipóteses nula e alternativa, o erro
# tipo 1, o valor-p, o resultado do teste e a conclusão da análise. Nunca
# esqueçam de apresentar as premissas da sua análise. 

# Bibliotecas Utilizadas
library(tidyverse)
library(data.table)

# Análise descritiva

v13 <- c(400, 200, 130, 110, 130, 450, 1880, 3980, 3720, 3690, 3900, 4020, 4070, 3940, 4030,
         4050, 4300, 4250, 3800, 3400, 3150, 2600, 2300, 930)

v14 <- c(450, 210, 120, 90, 120, 450, 2100, 4100, 3600, 3540, 3750, 3880, 3850, 3810, 3880, 
         3830, 4000,3960, 3570, 3280, 2930, 2550, 2070, 1050)

desc13 <- data.frame(n = length(v13), media = mean(v13), desvio_padrao = sd(v13), variancia = var(v13), coef_var = sd(v13)/mean(v13))
desc14 <- data.frame(n = length(v14), media = mean(v14), desvio_padrao = sd(v14), variancia = var(v14), coef_var = sd(v14)/mean(v14))
print(desc13) # principais estatísticas descritivas
print(desc14)

desc13$media / desc14$media
desc13$media - desc14$media

desc13$desvio_padrao / desc14$desvio_padrao
desc13$desvio_padrao - desc14$desvio_padrao

# A princípio, as tendências centrais apontam para valores maiores e de maior
# variância em 2013, quando comparado aqueles em 2014. A diferença entre as
# médias é de 3,7% (93,3 vei/h), e entre os devios padrão é de 5,4% (83,5 vei/h).

ggplot() +
  geom_point(aes(x = c(1:24), y = v13, color = "#067D99"), alpha = .6, size = 4) +
  geom_point(aes(x = c(1:24), y = v14, color = "#8E0000"), alpha = .6, size = 4) +
  scale_color_manual(values = c("#067D99", "#8E0000"), name = "Ano da amostra", breaks = c("#067D99", "#8E0000"), label = c("2013", "2014"), guide = "legend") +
  labs(title = "Dispersão", x = "Hora do Dia", y = "Volume (vei/h)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# As amostras parecem se comportar dentro de um mesmo padrão horário, o que pode
# indicar uma dependência temporal e inviabilizar o uso do teste t pareado.

data.frame(volume = c(v13, v14), amostra = rep(c("v13", "v14"), times = c(24, 24))) %>%
  ggplot() +
  geom_boxplot(aes(volume, fill = amostra), colour = "#222222", alpha = .8) +
  scale_fill_manual(values = c("#067D99", "#8E0000"), name = "Ano da amostra", breaks = c("v13", "v14"), label = c("2013", "2014"), guide = "legend") +
  labs(title = "Diagrama de Caixa", x = "Volume (vei/h)") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())

quartis13 <- summary(v13)
quartis14 <- summary(v14)
print(quartis13) # apresentação dos quartis
print(quartis14)

# As amostras parecem apresentar o mesmo padrão de dispersão, sendo 2014 a
# amostra menos dispersa, confirmando o que foi verificado na análise anterior.

data.frame(volume = c(v13, v14), amostra = rep(c("v13", "v14"), times = c(24, 24))) %>%
  ggplot() +
  geom_histogram(aes(volume, fill = amostra), position = "dodge", bins = 5, alpha = .75) +
  scale_fill_manual(values = c("#067D99", "#8E0000"), name = "Ano da amostra", breaks = c("v13", "v14"), label = c("2013", "2014"), guide = "legend") +
  labs(title = "Histograma", x = "Volume (vei/h)", y = "Frequência") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

data.frame(volume = c(v13, v14), amostra = rep(c("v13", "v14"), times = c(24, 24))) %>%
  ggplot() +
  geom_density(aes(volume, fill = amostra), alpha = .5) +
  scale_fill_manual(values = c("#067D99", "#8E0000"),name = "Ano da amostra", breaks = c("v13", "v14"), label = c("2013", "2014"), guide = "legend") +
  labs(title = "Densidade de Probabilidade", x = "Volume (vei/h)", y = "Frequência") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

data.frame(volume = c(v13, v14), amostra = rep(c("v13", "v14"), times = c(24, 24))) %>%
  ggplot() +
  stat_ecdf(aes(volume, color = amostra), size = 1, alpha = .75) +
  scale_color_manual(values = c("#067D99", "#8E0000"),name = "Ano da amostra", breaks = c("v13", "v14"), label = c("2013", "2014"), guide = "legend") +
  labs(title = "Probabilidade Acumulada", x = "Volume (vei/h)", y = "Probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# O histograma, a densidade de probabilidade e aprobabilidade acumulada indicam
# que as duas amostras possuem distribuições aproximadas.

ggplot() +
  geom_abline(aes(intercept = 0, slope = 1), color = "#8E0000") +
  geom_point(aes(x = v13, y = v14), color = "#067D99", alpha = .6, size = 4) +
  labs(title = "Correlação Pareamento", x = "Volumes 2013 (vei/h)", y = "Volumes 2014 (vei/h)") +
  xlim(-250, 4500) +
  ylim(-250, 4500) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# O gráfico anterior denota dependência entre as duas amostras, o que pode
# indicar o uso do teste t pareado.

# Considerando a dependência entre as amostras e supondo as amostragens
# aleatórias e independentes, opta-se por utilizar o teste t pareado para
# avaliar a hipótese em questão.

# Como premissa, as amostras devem ser aleatórias e ter originado de uma
# distribuição normal. Esta premissa será verificada através de teste
# qui-quadrado

f13 <- function(b) {
  x <- sum(v13 <= b)
  }

f14 <- function(b) {
  x <- sum(v14 <= b)
  }


qui <- data.frame(intervalo13 = c(901, 3201, 3936, 4301)) %>% mutate(f13 = sapply(intervalo13, f13), esp_norm13 = pnorm(intervalo13, mean = desc13$media, sd = desc13$desvio_padrao)*24, intervalo14 = c(901, 3201, 3756, 4301), f14 = sapply(intervalo14, f14), esp_norm14 = pnorm(intervalo14, mean = desc14$media, sd = desc14$desvio_padrao)*24)
qui <- qui %>% mutate(f13 = ifelse(is.na(shift(f13, 1L, type = "lag")), f13, f13 - shift(f13, 1L, type = "lag")), f14 = ifelse(is.na(shift(f14, 1L, type = "lag")), f14, f14 - shift(f14, 1L, type = "lag")))
qui <- qui %>% mutate(esp_norm13 = ifelse(is.na(shift(esp_norm13, 1L, type = "lag")), esp_norm13, esp_norm13 - shift(esp_norm13, 1L, type = "lag")), esp_norm14 = ifelse(is.na(shift(esp_norm14, 1L, type = "lag")), esp_norm14, esp_norm14 - shift(esp_norm14, 1L, type = "lag")))
qui <- qui %>% mutate(chi13 = ((f13 - esp_norm13)^2)/esp_norm13, chi14 = ((f14 - esp_norm14)^2)/esp_norm14)

qui

# H0: a amostra se origina de uma população normal. (alfa = 5%)
# Ha: a amostra não se origina de uma população normal.


qui_critico <- qchisq(0.95, df = nrow(qui)-1-2)
# Qui-quadrado crítico para rejeição da hipótese H0 é
qui_critico

qui13 <- sum(qui$chi13)
qui14 <- sum(qui$chi14)

qui13
qui14

shapiro.test(v13)
shapiro.test(v14)

# O teste qui-quadrado, bem como o teste Shapiro-Wilk rejeitam a hipótese de
# normalidade das amostrais.

# A partir daqui, a análise se dá am relação à distribuição das diferenças entre
# os pares amostrais.

dif <- data.frame(v13, v14, dif = v14 - v13)
dif

# Analisa-se as estatísticas e a forma da distribuição.

ggplot() +
  geom_abline(aes(intercept = 0, slope = 0), color = "#8E0000") +
  geom_point(aes(x = c(1:24), y = dif$dif), color = "#067D99", alpha = .6, size = 4) +
  labs(title = "Dispersão das Diferenças", x = "Hora do Dia", y = "Diferença nos Volumes (vei/h)") +
  xlim(-2, 27) +
  ylim(-350, 350) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))


descdif <- data.frame(n = length(dif$dif), media = mean(dif$dif), desvio_padrao = sd(dif$dif), variancia = var(dif$dif), coef_var = sd(dif$dif)/mean(dif$dif))
descdif

dif %>% ggplot() +
  geom_histogram(aes(dif, y = ..density.., fill = "#8E0000"), bins = 6, alpha = .75) +
  geom_density(aes(qnorm(seq(0.003, 0.997, length = 24), mean = descdif$media, sd = descdif$desvio_padrao), fill = "#067D99"), alpha = 0.5) +
  labs(title = "Histograma e Distribuição Normal", x = "Volume (vei/h)", y = "Probabilidade") +
  scale_fill_manual(values = c("#8E0000", "#067D99"),name = "Legenda", breaks = c("#8E0000", "#067D99"), label = c("Frequência das Diferenças", "Distribuição Normal"), guide = "legend") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

dif %>%
  ggplot() +
  stat_ecdf(aes(qnorm(seq(0.003, 0.997, length = 24), mean = descdif$media, sd = descdif$desvio_padrao), color = "#8E0000"), size = 1, alpha = .75) +
  stat_ecdf(aes(dif, color = "#067D99"), size = 1, alpha = .75) +
  scale_color_manual(values = c("#067D99", "#8E0000"),name = "Legenda", breaks = c("#067D99", "#8E0000"), label = c("Diferença", "Normal"), guide = "legend") +
  labs(title = "Probabilidade Acumulada", x = "Volume (vei/h)", y = "Probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

dif %>%
  ggplot() +
  geom_qq(aes(sample = scale(dif)), color = "#067D99", size = 3, alpha = .6) +
  geom_abline(aes(intercept = 0, slope = 1), color = "#8E0000") +
  labs(title = "Gráfico Quantil-Quantil das Diferenças", x = "Quantis Teóricos", y = "Quantis Obeservados") +
  xlim(-2.5, 2.5) +
  ylim(-2.5, 2.5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))


# As estatísticas apontam para uma possível redução dos volume horário veicular
# em 2014, quanto comparado à 2013, com média de 93,3 veic/h, conforme já
# verificado anteriormente.

# As formas da distribuição também apontam para uma distribuição aproximada da
# normal.

# Verifica-se a normalidade da distribuição da difierença atravees de testes
# qui-quadrado e Shapiro-Wilk

fdif <- function(b) {
  x <- sum(dif$dif <= b)
}

quidif <- data.frame(intervalo_dif = c(-220, -100, 0, 400)) %>% mutate(fdif = sapply(intervalo_dif, fdif), esp_normdif = pnorm(intervalo_dif, mean = descdif$media, sd = descdif$desvio_padrao)*24,)
quidif <- quidif %>% mutate(fdif = ifelse(is.na(shift(fdif, 1L, type = "lag")), fdif, fdif - shift(fdif, 1L, type = "lag")))
quidif <- quidif %>% mutate(esp_normdif = ifelse(is.na(shift(esp_normdif, 1L, type = "lag")), esp_normdif, esp_normdif - shift(esp_normdif, 1L, type = "lag")))
quidif <- quidif %>% mutate(chidif = ((fdif - esp_normdif)^2)/esp_normdif)

quidif

qui_critico <- qchisq(0.95, df = nrow(quidif)-1-2)
# Qui-quadrado crítico para rejeição da hipótese H0 é
qui_critico

qui_dif <- sum(quidif$chidif)
qui_dif

shapiro.test(dif$dif)

# A hipote-se de normalidade não pode ser rejeitada, então considera-se que a
# distribuição da diferença pode ser aproximada da normal.

# Ainda, devido à aparente aleatoriedade e independência da variável diferença 
# entre os pares, bem como sua normalidade, decide-se continuar com a aplicação
# do teste t pareado.


# H0: µd = 0 não há diferença entre as amostras (hipótese a ser testada)
# Ha: µd ≠ 0 há diferença entre as amostras 
# Erro tipo I (alfa) 5% - Considerar  os volumes horários diferentes quando
# forem iguais.

med_d <- mean(dif$dif)
n <- 24
dp_med_d <- sd(dif$dif)/sqrt(n)
df <- n-1

td <- med_d/dp_med_d

eixo_x <- ifelse(
  qt(seq(.0001, .9999, .001), df) < qt(.025, df), qt(.025, df), #limite inferior da sombra
  ifelse(
    qt(seq(.0001, .9999, .001), df) > qt(.975, df), qt(.975, df), #limite superior da sombra
    qt(seq(.0001, .9999, .001), df)
  )
)

# Cria-se um objeto para representar o eixo y
eixo_y <- dt(eixo_x, df)

# Cria-se a curva completa
curva <- data.frame(x = qt(seq(.0001, .9999, length = length(eixo_x)), df), y = dt(qt(seq(.0001, .9999, length = length(eixo_x)), df), df))

# Plota-se o gráfico com duas curvas
ggplot() +
  geom_line(aes(x = curva$x, y = curva$y)) +
  geom_area(aes(eixo_x, eixo_y, fill = "#8E0000"), alpha = .5) +
  xlim(-4.5, 4.5) +
  ylim(0, .5) +
  geom_vline(aes(xintercept = qt(0.025, df), color = "black"), linetype = 2) +
  geom_vline(aes(xintercept = 0, color = "#777777")) +
  geom_vline(xintercept = qt(0.975, df), linetype = 2) +
  geom_vline(aes(xintercept = td, color = "#067D99"), size = 1) +
  labs(title = "Representação Gráfica do Teste T Pareado", subtitle = paste("n =", n, "| df =", df, "| alfa = 5%"), x = "Escore t", y = "Probabilidade") +
  scale_fill_identity(name = "", breaks = c("#8E0000"), labels = c("Intervalo de Confiança"), guide ="legend") +
  scale_color_identity(name = "", breaks = c("#067D99", "black", "#777777"), labels = c("Diferença Observada", "Limites do IC", "H0: µd = 0"), guide ="legend") +
  geom_text(aes(x = -1, y = .425, label = paste("t-crítico =", round(qt(0.025, df), 1))), size = 4) +
  geom_text(aes(x = -2.4, y = .475, label = paste("valor-t =", round(td, 1))), color = "#067D99", size = 4) +
  theme_minimal()

# Rejeita-se a hipótese nula
# H0: µd = 0
# considerando que, caso fosse verdadeira, haveria uma probabilidade de:
pt(td, df)*2
# de se obter duas amostras com a diferença observada.

