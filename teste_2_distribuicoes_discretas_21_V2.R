# UNIVERSIDADE FEDERAL DO CEARÁ
# PROGRAMA DE MESTRADO EM ENGENHARIA DE TRANSPORTES – PETRAN/UFC
# TCP 7321 – TÉCNICA DE ANÁLISE DE DADOS EM TRANSPORTES  -   PROF. MANOEL CASTRO

# Teste 2 – Probabilidade e variáveis aleatórias discretas						17/05/21

# NOMES:  Nelson Quesado, Renata Farias e Rubia Rodrigues

# 1) Um engenheiro de produção de uma indústria está interessado em avaliar a qualidade
# de uma determinada peça de automóvel, disponível em lotes de 100 unidades. Para que um
# lote esteja dentro das especificações exigidas pelas montadoras, o seu percentual de
# peças defeituosas deve ser de até 25%. Como método de avaliação de um lote, ele propõe
# selecionar uma amostra aleatória de 4 peças e recomendar a rejeição do lote em análise
# se ele encontrar mais de uma peça defeituosa.

library(gtools)
library(ggplot2)
library(ggthemes)
library(tidyverse)

arranjo <- permutations(2, 4, repeats.allowed = TRUE) # possíveis combinações
print(arranjo)

# a. Supondo que o lote analisado tenha exatamente 25% de peças defeituosas, qual é a
# probabilidade de o engenheiro rejeitá-lo se ele realizar uma amostragem com reposição?

prob_a_0 <- 75/100 * 75/100 * 75/100 * 75/100
prob_a_1 <- 25/100 * 75/100 * 75/100 * 75/100 * 4

prob_a_2 <- 25/100 * 25/100 * 75/100 * 75/100 * 6
prob_a_3 <- 25/100 * 25/100 * 25/100 * 75/100 * 4
prob_a_4 <- 25/100 * 25/100 * 25/100 * 25/100

espa_amos_a <- prob_a_0 + prob_a_1 + prob_a_2 + prob_a_3 + prob_a_4
print(espa_amos_a)

prob_a <- prob_a_2 + prob_a_3 + prob_a_4
print(prob_a) # RESPOSTA A (26,17%)


# b. Supondo novamente que o lote analisado tenha exatamente 25% de peças defeituosas,
# qual é a probabilidade de o engenheiro rejeitá-lo se ele realizar uma amostragem sem
# reposição?

prob_b_0 <- 75/100 * 74/99 * 73/98 * 72/97
prob_b_1 <- 25/100 * 75/99 * 74/98 * 73/97 * 4

prob_b_2 <- 25/100 * 24/99 * 75/98 * 74/97 * 6
prob_b_3 <- 25/100 * 24/99 * 23/98 * 75/97 * 4
prob_b_4 <- 25/100 * 24/99 * 23/98 * 22/97

espa_amos_b <- prob_b_0 + prob_b_1 + prob_b_2 + prob_b_3 + prob_b_4
print(espa_amos_b)

prob_b <- prob_b_2 + prob_b_3 + prob_b_4
print(prob_b) # RESPOSTA B (25,95%)

# c. Com base nos resultados dos itens anteriores, o que você acha do método análise para
# tomada de decisão adotado pelo engenheiro?

i <- prob_a/prob_b
print(i) # relação entre as probabilidade com e sem reposição

### RESPOSTA C
### Verifica-se que as duas abordagens, com e sem reposição, apresentam probabilidade de erro praticamente iguais.
### Deverá ser escolhida aquela abordagem mais conveniente para o seu processo executivo.
### Considera-se ~26%~ uma probbilidade de erro alta para o tipo de operação.
### Ao avaliar outros tamanhos de amostragem, verifica-se que a probabilidade de reijeitar o lote aumenta.
### Dessa forma, desconselha-se o aumento da amostra.

# silmulação de monte carlo para amostragens de diferentes tamanhos
lote <- rep(c("defeito", "normal"), times = c(25,75))     # criação do lote
print(lote)       # ver o lote

opt <- seq(4, 100, 4)     # possíveis tamanhos para as amostras

B <- 500000    # número de simulações

prob_n <- function(x) {     # funcao simulação monte carlo sem reposição
  m <- replicate(B, {
    n <- sample(lote, x, replace = FALSE)
    mean(n == "defeito") > 0.25
  })
  mean(m)
}

probs <- sapply(opt, prob_n)     # aplicação simulação monte carlo em todas as possíveis opt

tab <- data.frame(tamanho_amostra = opt, probabilidade_erro = probs)

tab %>% ggplot(aes(tamanho_amostra, probabilidade_erro)) + # gráfico tamanho da amostra VS probabilidade
  geom_point(size = 1.5) +
  geom_line(color = "gray") +
  theme_clean()

print(tab)