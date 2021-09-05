## UNIVERSIDADE FEDERAL DO CEARÁ
## CENTRO DE TECNOLOGIA - DEPARTAMENTO DE ENGENHARIA DE TRANSPORTES
## TCP7321 – TAD				  		PROF. MANOEL CASTRO 

## Teste 4 – Distribuição Normal								04/06/21

## Grupo: Nelson Quesado, Renata Farias e Rubia Rodrigues

# Suponha que você foi o(a) engenheiro(a) de tráfego contratado(a) para averiguar
# a necessidade de se implantar um equipamento de controle de velocidade numa seção
# da Avenida Washington Soares, cujo limite de velocidade permitido é de 60 km/h.

# Para tal tarefa, você coletou 100 observações de velocidade instantânea (ver tabela
# abaixo; limite esquerdo do intervalo fechado, e do direito aberto). Como critério
# de tomada de decisão, o equipamento será instalado se mais de 15% dos motoristas
# que ali trafegam excederem a margem de tolerância de 66 km/h.

# Velocidade  Freq. Absoluta
# 52	54	    1
# 54	56	    1
# 56	58	    10
# 58	60	    24
# 60	62	    30
# 62	64	    20
# 64	66	    8
# 66	68	    4
# 68	70	    2

library(tidyverse)
library(ggthemes)

dados <- data.frame(vel_inf = seq(52, 68, 2), vel_med = seq(53, 69, 2), vel_sup = seq(54, 70, 2), freq_abs = c(1, 1, 10, 24, 30, 20, 8, 4, 2))

dados_lista <- rep(seq(53, 69, 2), times = c(1, 1, 10, 24, 30, 20, 8, 4, 2))
print(dados_lista) # lista de valores para cálculo de média e desvio padrão


# a)	Realizem uma análise descritiva da amostra.
dados <- dados %>% mutate(freq_rel = freq_abs/sum(freq_abs), freq_norm = (pnorm(vel_sup, mean(dados_lista), sd(dados_lista))-pnorm(vel_inf, mean(dados_lista), sd(dados_lista))))
print(dados)  # apresentação dos dados incluindo as frequencias relativas da amostra
              # e as frequências teóricas considerando a distribuição normal

quartis <- summary(dados_lista)
print(quartis)  # apresentação dos quartis

desc <- data.frame(n = length(dados_lista), media = mean(dados_lista), desvio_padrao = sd(dados_lista), coef_var = sd(dados_lista)/mean(dados_lista), media_menos_3desv = -3*sd(dados_lista) + mean(dados_lista), media_mais_3desv = 3*sd(dados_lista) + mean(dados_lista))
print(desc) # principais estatísticas descritivas

# Histograma
data.frame(dados_lista) %>% ggplot(aes(dados_lista)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(fill = "#D9023C", alpha = 0.5) +
  xlab("Velocidade") +
  ylab("Probabilidade") +
  theme_clean()

# Boxplot
data.frame(dados_lista) %>% ggplot(aes(dados_lista)) +
  geom_boxplot() +
  theme_clean()

# b)	Vocês utilizariam uma distribuição Normal para aproximar o comportamento populacional
# desta variável? Justifique, comparando as frequências observadas na amostra com
# as frequências esperadas pelo modelo de probabilidade normal.

print(quartis) # aqui é possível ver que a média é muiot próximo da mediana.
print(desc) # percebe-se que limites +- 3 desvios padrão estão coerentes com a amostra

## HISTOGRAMA OBSERVADO VS. TEÓRICO
# O histograma dos valores observados apresenta forma semelhante à distribuição
# normal
gather(dados, Tipo, Valor, freq_rel:freq_norm) %>%
  ggplot() +
  geom_bar(aes(vel_med, Valor, fill = Tipo,), stat = "identity", position = "dodge") +
  xlab("Velocidade") +
  ylab("Probabilidade") +
  theme_clean()

## QQ PLOT
# Apesar da discretização dos dados por um possível arredondamento dos valores,
# percebe-se que os quantis observados seguem os quantis teóricos da distribuição
# normal
data.frame(dados_lista) %>%
  ggplot(aes(sample = scale(dados_lista))) +
  geom_qq(color = "#D9023C", alpha = 0.75) +
  geom_abline()


## DISTRIBUIÇÃO ACUMULADA
# A distribuição acumulada da probabilidade/frequência também se aproxima da normal,
# especialmente quando se leva em consideração possível discretização dos dados.
data.frame(dados_lista) %>%
  ggplot() +
  stat_ecdf(aes(dados_lista), color = "#D9023C") +
  stat_ecdf(aes(qnorm(seq(0.01, 0.99, length = 100), mean(dados_lista), sd(dados_lista)))) +
  xlab("Velocidade") +
  ylab("Probabilidade") +
  theme_clean()

# TESTE SHAPIRO-WILK
print(shapiro.test(dados_lista))

# Conclui-se, a partir das análises numéricias e visuais, que a distribuição da
# amostra é próxima da distribuição normal.

# c)	Seguindo o critério exposto no enunciado, o equipamento de fiscalização deve
# ser instalado? Justifiquem.

prob_acima_66kmph <- 1 - pnorm(66, mean(dados_lista), sd(dados_lista))
print(prob_acima_66kmph)  # frequência de veículos que trafegam acima de 66km/h
                          # é de 4,58%, ou seja, abaixo do limite de implantação.

# A implantação de um equipamento de controle de velocidade não deve ser realizada.