### TRABALHO PRÁTICO 1 – ANÁLISE DESCRITIVA
### ALUNOS: Nelson Qesado, Renata Farias e Rúbia Rodrigues

# bibliotecas utilizadas
library(ggplot2)
library(ggthemes)
library(tidyverse)

# carregar os dados a partir do arquivo dados_tp1.csv
dados_tp1 <- read.table(file = "dados_tp1.csv", sep = ";", header = TRUE, fill = TRUE)

summary(dados_tp1)

dados <- data.frame(idade = dados_tp1$idade, distancia = dados_tp1$distancia_media_km, genero = dados_tp1$genero, raca = dados_tp1$raca, modo = dados_tp1$modo, motivo = dados_tp1$motivo) %>%
  mutate(idade = as.numeric(idade))
head(dados)

# descrição da base de dados
desc <- summary(dados)
print(desc)

idade <- data.frame(media = mean(dados$idade), desv_pad = sd(dados$idade))
distancia <- data.frame(media = mean(dados$distancia), desv_pad = sd(dados$distancia))

print(idade)
print(distancia)

# caracterização da frequência
# histograma
hist(dados$idade, main = "")
hist(dados$distancia, main = "")

# histograma + densidade
dados %>% ggplot(aes(idade)) +
  geom_histogram(aes(y = ..density..), color = "grey30", fill = "white") +
  geom_density(fill = "pink", alpha = .2) +
  theme_clean()

dados %>% ggplot(aes(distancia)) +
  geom_histogram(aes(y = ..density..), color = "grey30", fill = "white") +
  geom_density(fill = "#80CEE1", alpha = .2) +
  theme_clean()

# boxplot
dados %>% ggplot(aes(idade)) +
  geom_boxplot() +
  theme_clean()

dados %>% ggplot(aes(distancia)) +
  geom_boxplot() +
  theme_clean()

# representação da regressão linear (reta e equação obtidas pelo excel)
# y = -0,0337x + 9,6742

dados %>% ggplot(aes(idade, distancia)) +
  geom_point(alpha = .5, size = 4) +
  geom_segment(aes(x = 18, y = 9, xend = 65, yend = 7.5), color = "#80CEE1", size = 1) +
  theme_clean()

# análise extra (verificando categorias)
dados %>% ggplot(aes(idade, distancia, color = genero)) +
  geom_point(alpha = .5, size = 4) +
  theme_clean()
  
dados %>% ggplot(aes(idade, distancia, color = motivo)) +
  geom_point(alpha = .5, size = 4) +
  theme_clean()

dados %>% ggplot(aes(idade, distancia, color = raca)) +
  geom_point(alpha = .5, size = 4) +
  theme_clean()

dados %>% ggplot(aes(idade, distancia, color = modo)) +
  geom_point(alpha = .5, size = 4) +
  theme_clean()