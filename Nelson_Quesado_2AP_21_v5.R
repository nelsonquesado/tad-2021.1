### 2a AP 2021.1
### Técnica de Análise de Dados de Transportes
### Prof. Dr. Manoel Mendonça de Castro Neto
### Aluno: Nelson de O. Quesado Filho
### Matrícula: 504117

# Bibliotecas utilizadas
library(tidyverse)
library(wesanderson)
library(lmtest)

#1a Questão:
# (...) avaliar se o agregado (brita) da 1AP está com rugosidade média menor que
# 440, pois isso implicaria na necessidade de buscar outra pedreira. A amostra
# ao lado, posta em ordem crescente,  é a mesma da 1AP. Responda os itens abaixo:

# a) (2,0) Para atingir o objetivo da análise, realize um teste de hipóteses,
# explicitando as hipóteses nula e alternativa, o valor do nível de significância,
# o valor-p, o resultado do teste (se rejeita ou não H0) e a conclusão da análise.
# Indique as premissas da sua análise.

# RESPOSTA a)
# Antes de se avançar no teste de hipótese, realiza-se uma análise descritiva
# da amostra.

rugosidade <- c(57.723 ,76.145 ,101.936 ,110.534 ,131.412 ,147.378 ,159.66 ,162.116 ,168.257 ,169.485 ,182.994 ,195.276 ,197.732 ,198.96 ,205.101 ,205.101 ,219.839 ,222.295 ,240.717 ,241.946 ,260.368 ,267.737 ,278.79 ,283.703 ,283.703 ,292.3 ,299.669 ,310.722 ,310.722 ,325.46 ,336.513 ,342.654 ,354.935 ,359.848 ,375.814 ,393.008 ,401.605 ,426.168 ,434.765 ,438.45 ,458.1 ,459.328 ,463.013 ,475.294 ,478.978 ,498.629 ,507.226 ,515.823 ,533.017 ,546.527 ,550.211 ,555.124 ,555.124 ,573.546 ,587.056 ,591.968 ,601.794 ,605.478 ,623.9 ,648.463 ,663.201 ,688.992 ,713.555 ,766.366 ,794.613 ,800.754 ,837.598 ,918.656 ,937.078 ,945.675 ,949.36)

data.frame(rugosidade) %>% summarise(avg = mean(rugosidade), s = sd(rugosidade), coef_var = avg/s, var = s^2, n = n())

# A amostra apresenta rugosidade média de 422,76, ou seja, abaixo do valor
# desejado para a aplicação. Sabe-se que esta estatística - a média - é
# insuficiente para a análise desejaada.

# Ainda, verifica-se um desvio padrão de 230,74, o que representa um coeficiente
# de variação de 1,83. Interpreta-se este valor como alto, e que provavelmente
# contribuirá para um intervalo de confiança demasiadamente largo.

# A amostra também apresenta uma quantidade de observações razoável (71) para
# o tipo de análise pretendida.

data.frame(rugosidade) %>% ggplot() +
  geom_point(aes(x = c(1:71), y = sort(rugosidade), color = "A"), alpha = .8) +
  geom_boxplot(aes(x = c(1:71), y = rugosidade, color = "B"), alpha = 0) +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "", breaks = c("A"), label = c("Rugosidade"), guide = "legend") +
  labs(title = "Dispersão e Diagrama de Caixa", subtitle = paste("n =", length(rugosidade)), x = "Observação", y = "Rugosidade") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

data.frame(rugosidade) %>% ggplot() +
  geom_histogram(aes(x = rugosidade, y = ..density.., fill = "A"), color = "white", alpha = .8, bins = 9) +
  geom_density(aes(x = rugosidade, fill = "B"), alpha = .7) +
  scale_fill_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "", breaks = c("A"), label = c("Rugosidade"), guide = "legend") +
  labs(title = "Histograma de Frequência e Densidade de Probabilidade", subtitle = paste("n =", length(rugosidade)), x = "Rugosidade", y = "Probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Os gráficas mostram uma certa assimetria à direita. Apesar desta característica,
# decide-se verificar visualmente a proximidade da amostra em comparação à
# distribuição normal

data.frame(rugosidade) %>% ggplot() +
  geom_density(aes(x = rugosidade, fill = "A"), alpha = .5) +
  geom_density(aes(x = qnorm(seq(0.003, 0.997, length = length(rugosidade)), mean = mean(rugosidade), sd = sd(rugosidade)), fill = "B"), alpha = .5) +
  scale_fill_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "Curva", breaks = c("A", "B"), label = c("Amostra", "Normal"), guide = "legend") +
  labs(title = "Densidade de Probabilidade Amostral vs. Normal", subtitle = paste("n =", length(rugosidade)), x = "Rugosidade", y = "Probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

data.frame(rugosidade) %>% ggplot() +
  stat_ecdf(aes(x = rugosidade, color = "A"), alpha = .8) +
  stat_ecdf(aes(x = qnorm(seq(0.003, 0.997, length = length(rugosidade)), mean = mean(rugosidade), sd = sd(rugosidade)), color = "B"), alpha = .8) +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "Curva", breaks = c("A", "B"), label = c("Amostra", "Distribuição Normal"), guide = "legend") +
  labs(title = "Distribuição de Probabilidade Acumulada", subtitle = paste("n =", length(rugosidade)), x = "Rugosidade", y = "Probabilidade") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

data.frame(rugosidade) %>% ggplot() +
  geom_qq(aes(sample = scale(rugosidade), color = "A"), alpha = .8) +
  geom_abline(aes(intercept = 0, slope = 1, color = "B")) +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "", breaks = c("A"), label = c("rugosidade"), guide = "legend") +
  labs(title = "Gráfico Quantil-Quantil", subtitle = paste("n =", length(rugosidade)), x = "Quantis Teóricos", y = "Quantis Observados") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Os gráficos densidade de probabilidade, probabilidade acumulada e quantil-quantil
# apontam certa semelhança entre a amostra e a distribuição normal.

# Aplcia-se o teste Shapiro-Wilk (onde a hipótese nula é que a amostra origina-se
# de uma população normalmente distribuída) para verificar a normalidade.

shapiro.test(rugosidade)

# O resultado do valor-p em 0.0086 aponta para uma probabilidade rara da amostra
# ter se originado de uma distribuição normal, permitindo rejeitar estatísticamente
# tal hipótese.

# Contudo, apoiando-se no Teorema do Limite Central, entende-se ser possível
# realizar uma inferência intervalar da média ao aplicar teste de hipótese com
# amostra de tamanho grande (aponta-se como 30 um tamanho amostral razoável para
# este tipo de abordagem).

# CONSTRUÇÃO DO TESTE DE HPÓTESE

# Ho: µ = 440 Esta hipótese significa utilizar a população que originou a
# amostra, ou seja, não "fazer nada" (hipótese a ser testada)

# Ha: µ < 440 Esta hipótese significar inutilizar a população que originou a
# amostra, ou seja, buscar uma nova jazida.

# Arbitra-se, conforme usualmente estabelece-se nas pesquisas da área, um nível
# de significância de 5% como critério de avaliação do teste. Interpreta-se esse
# nível como a probabilidade do erro tipo I ocorrer, ou seja, rejeitar a pedreira
# sendo ela adequada para o uso.

# O teste em questão, por estimar o desvio padrão populacional a partir do desvio
# padrão amostra, deve ser o teste T-Student com n-1 graus de liberdade.

# Representação visual do teste-t:

df <- length(rugosidade) - 1
med <- mean(rugosidade)
se <- sd(rugosidade)/sqrt(length(rugosidade))
t_rug <- (med-440)/se # Valor T da amostra

# Sombra
eixo_x <- 440 + qt(seq(.05, .9999, .001), df) * se
eixo_y <- dt((eixo_x-440)/se, df)

# Curva T Completa
curva <- data.frame(x = 440 + qt(seq(.0001, .9999, length = length(eixo_x)), df) * se, y = dt(qt(seq(.0001, .9999, length = length(eixo_x)), df), df))

# Plota-se a Curva
ggplot() +
  geom_line(aes(x = curva$x, y = curva$y)) +
  geom_area(aes(eixo_x, eixo_y, fill = "A"), alpha = .5) +
  geom_vline(aes(xintercept = 440 + qt(0.05, df) * se, color = "A"), linetype = 2) +
  geom_vline(aes(xintercept = med, color = "B"), size = 1) +
  labs(title = "Representação Gráfica do Teste T", subtitle = paste("n =", length(rugosidade), "| df =", df, "| alfa = 5%"), x = "Escore t", y = "Probabilidade") +
  scale_fill_manual(values = wes_palette(n = 1, name = "Darjeeling1"), name = "", breaks = c("A"), labels = c("Área Não Rejei. Ho"), guide ="legend") +
  scale_color_manual(values = wes_palette(n = 3, name = "Darjeeling1"), name = "", breaks = c("A", "B"), labels = c("Limite IC Média Pop.", "Valor-T Amostral"), guide ="legend") +
  geom_text(aes(x = 375, y = .425, label = paste("t-crítico =", round(qt(0.05, df), 1))), size = 4, color = wes_palette(n = 1, name = "Darjeeling1")) +
  geom_text(aes(x = 450, y = .475, label = paste("valor-t =", round(t_rug, 1))), color = "#067D99", size = 4, color = wes_palette(n = 1, name = "Darjeeling1")) +
  theme_minimal()


# A partir do teste de hipótese realizado, pode-se dizer que não há evidências
# estatísticas suficientes para se rejeitar a hipótese nula, pois o valor-t da
# média amostral encontra-se na região de não rejeição. Ou seja, entende-se
# que a amostra não justifica a troca de jazida por outra mais adequada.

# É importante apontar que o teste é válido apenas para amostras aleatórias
# e independentes, o que não foi verificado nesta anaálise e assume-se como premissa.
# Ainda, assume-se que o desvio padrão populacional (desconhecido) é bem
# representado pelo desvio padrão amostral. Para esta premissa, entende-se que o
# uso da distribuição t-student no lugar da distribução normal é apropriado.

# FINAL DA RESPOSTA a)


# b) (1,0) Qual é o erro tipo-II da sua análise?

# RESPOSTA b)

# Para o caso em questão, o erro tipo II é aquele onde não rejeita-se a hipótese
# nula, sendo ela falsa, ou seja, àquele passível de ser cometido considerando a
# decisão tomada no item a) desta questão da avaliação.

# Para o cálculo do erro tipo II, assume-se que a população possui uma média
# inferior à 440. Arbitra-se, como premissa da avaliação do erro tipo II, que a
# população possui média igual à média amostral. O desvio padrão populacional
# também é estimado pelo desvio padrão amostral.

med # média amostral = média populacional


# Sombra
eixo_x1 <- med + (
  qt(
    seq(
      dt(((med + qt(0.05, df) * se)-med)/se, df),
      .9999,
      length = length(eixo_x)),
    df)) * se

eixo_y1 <- dt((eixo_x1 - med)/se, df)

# Curva T Completa
curva1 <- data.frame(x = med + qt(seq(.0001, .9999, length = length(eixo_x1)), df) * se, y = dt(qt(seq(.0001, .9999, length = length(eixo_x1)), df), df))


# Plota-se a Curva
ggplot() +
  geom_line(aes(x = curva$x, y = curva$y)) + # Ho
  geom_line(aes(x = curva1$x, y = curva1$y)) + # Ha
  geom_area(aes(eixo_x1, eixo_y1, fill = "A"), alpha = .5) + # ha
  geom_vline(aes(xintercept = c(440, mean(rugosidade)), color = c("B", "C")), linetype = 2) +
  labs(title = "Representação Gráfica do Erro Tipo II", subtitle = paste("n =", length(rugosidade), "| df =", df, "| beta =", 1 - pt(qt(0.05, df) ,df)), x = "Escore t", y = "Probabilidade") +
  scale_fill_manual(values = wes_palette(n = 1, name = "Darjeeling1"), name = "", breaks = c("A"), labels = c("Erro Tipo II"), guide ="legend") +
  scale_color_manual(values = wes_palette(n = 3, name = "Darjeeling1"), name = "", breaks = c("B", "C"), labels = c("µ = 440 (Ho)", paste("µ =", round(mean(rugosidade),1))), guide ="legend") +
  theme_minimal()

# FINAL DA RESPOSTA b)

# c) (1,0) Estime a rugosidade média do agregado utilizando um intervalo de
# confiança de 95%. Para atingir o objetivo da análise posto no enunciado, é
# mais adequado utlizar o teste de hipóteses do item (a) ou esse intervalo de
# confiança do item (c)? Justifique

# Para esta análise, estima-se um intervalo de cofiança da média populacional
# a partir da amostra.

data.frame(inf = mean(rugosidade) + qt(.025, df)*(sd(rugosidade)/sqrt(length(rugosidade))), med = mean(rugosidade), sup = mean(rugosidade) + qt(.975, df)*(sd(rugosidade)/sqrt(length(rugosidade))))

# Entendo que o teste de hipótese é mais adequado pois considera a hipótese da
# tomada de decisão com um grau de certeza conhecida. O intervalo de confiança,
# por sua vez, pode ajudar na tomada de decisão e na interpretação do contexto.

# FINAL DA RESPOSTA c) E DA QUESTÃO 1


# 02) (3,0) O agregado da questão 1 estava em seu estado natural. Nesta questão
# 2, você pegou outra amostra do mesmo agregado e o submeteu a um tratamento de
# polimento, a fim de torná-lo menos rugoso. Veja na coluna "Polido" a nova
# amostra resultante, que também foi colocada em ordem crescente. O objetivo da
# sua análise é avaliar se o tratamento é capaz de diminuir a rugosidade média
# do agregado. 

natural <- rugosidade
polida <- c(27.019 ,73.689 ,87.199 ,99.48 ,112.99 ,116.674 ,119.131 ,121.587 ,126.499 ,144.922 ,152.291 ,153.519 ,153.519 ,160.888 ,162.116 ,163.344 ,163.344 ,174.397 ,180.538 ,181.766 ,184.223 ,185.451 ,195.276 ,198.96 ,201.417 ,210.014 ,211.242 ,251.771 ,251.771 ,257.911 ,266.509 ,277.562 ,282.474 ,289.843 ,289.843 ,299.669 ,311.95 ,315.635 ,318.091 ,319.319 ,325.46 ,346.338 ,351.251 ,361.076 ,365.989 ,370.901 ,377.042 ,385.639 ,385.639 ,386.867 ,405.289 ,411.43 ,412.658 ,420.027 ,424.94 ,428.624 ,437.221 ,483.891 ,491.26 ,499.857 ,515.823 ,535.473 ,578.459 ,593.196 ,594.425 ,601.794 ,604.25 ,676.711 ,697.589 ,714.783 ,760.225 ,770.05 ,783.56 ,846.195 ,962.87)

# a) (0,5) Apresente o seu método de análise.

# RESPOSTA a)

# A análise deste item avaliará se houve mudança significativa na rugosidade
# média entre as duas amostras. Para tanto será realizado um teste T (pois não
# se conhece o desvio padrão da população) unilatetra (pois se presume que a 
# amostra natual deve possuir maior asperesa média) e não pareado (pois há
# independência entre as amostras)

# RESPOSTA a)

# b) (1,5) Apresente os resultados da sua análise.

# RESPOSTA b)

t.test(natural, polida, paired = FALSE, alternative = "greater")

# FINAL DA RESPOSTA b)


# c) (1,0) Apresente a conclusão e as premissas da sua análise.

# RESPOSTA c)

# Considerando que a hipótese nula deste teste assume que a difernça entre
# as médias não é maior que 0, o valor p apresenta a probabilidade das amostras
# terem surgido nesta condição. Avaliando o evento como raro (2,074%), rejeita-se
# a hipótese nula, e apoia-se a afirmativa de que o processo aplicado é capaz
# de reduzir a rugosidade.

# FINAL DA RESPOSTA c)


# 3) (3,0) Um dos indicadores mais comuns para se avaliar a qualidade do tráfego
# é o atraso, que, de forma simples,  pode ser entendido como tempo que os
# condutores perdem parados em congestionamentos. 

# Como o atraso não é uma variável fácil de se coletar, a cidade de Los Angeles
# lhe contratou para avaliar se seria possível estimar o atraso total anual na
# cidade (em horas) com base na sua população.  Com base em uma análise de
# correlação e de regressão, apresente o seu método de análise, os resultados, a
# conclusão e as premissas.

atrasos <- data.frame(populacao = c(9900,9900,9900,10500,10710,10920,11140,11305,11420,11760,11845,11950,12000,12090,12220,12280,12310,12330,12350,12350,12400,12500), atraso = c(185569,202365,212548,250504,308512,428605,502848,585693,609125,604755,603530,614903,549822,589488,659825,640790,695408,681315,649165,632562,641847,623796))

# RESPOSTA QUESTÃO 3

# Para a avaliação proposta - correlação e regressão - aponta-se para as premissas
# de coleta de amostras onde as observações devem ser independentes e aleatórias.

# Método de Análise
# O método de analise proposto é composto pelas seguintes etapas:
# (i) avaliação da correlação entre as variáveis por gráfico de dispersão e
# coeficiente de correlação de Pearson (R).
# (ii) avaliação do modelo de regressão linear simples e da significância dos
# parâmetros estimados.
# (iii) avaliação das premissas do modelo de regressão, no que diz respeito à
# normalidade, homocedasticidade e independência dos resíduos.

# Etapa (i)

atrasos %>% ggplot() + 
  geom_point(aes(populacao, atraso, color = "A"), size = 2, alpha = .8) +
  labs(title = "Dispersão", subtitle = paste("n =", nrow(atrasos)), x = "População", y = "Atraso Médio") +
  scale_color_manual(values = wes_palette(n = 1, name = "Darjeeling1"), name = "", breaks = c("A"), label = c("Amostra"),  guide = "legend") +
  theme_minimal()

# Aparenta haver correlação linear positiva entre as variáveis.

cor(atrasos$populacao, atrasos$atraso, method = "pearson") # correlação alta

# O coeficiente de Regressão de Pearson é 0,95, o que pode indicar forte correlação
# entre as variáveis.


# Etapa (ii)

reg <- lm(atrasos$atraso ~ atrasos$populacao)
summary(reg)

# O modelo de regressão linear apresenta parâmetros B0 e B1 significativos, ambos
# com valor-p menor que 0,1%. Ou seja, estima-se que esses valores são diferentes
# de 0 e influenciam no modelo. O R2 ajustado apresenta valor muito satisfatório,
# sendo igual a 0.905.

atrasos %>% ggplot(aes(populacao, atraso)) + 
  geom_point(aes(color = "A"), size = 2, alpha = .8) +
  stat_smooth(aes(color = "B"), method = lm) +
  labs(title = "Dispersão e Regressão Linear Simples", subtitle = paste("n =", nrow(atrasos)), x = "População", y = "Atraso Médio") +
  scale_color_manual(values = wes_palette(n = 2, name = "Darjeeling1"), name = "", breaks = c("A", "B"), label = c("Amostra", "Regressão"),  guide = "legend") +
  theme_minimal()

# Etapa (iii)

# O gráfico e os testes a seguir avaliam as premissas dos resíduos.

data.frame(reg$residuals) %>% ggplot(aes(c(1:22), reg.residuals)) +
  geom_point(aes(color = "A"), size = 2, alpha = .8) +
  labs(title = "Dispersão dos Resíduos", subtitle = paste("n =", nrow(atrasos)), x = "Observação", y = "Atraso Médio") +
  scale_color_manual(values = wes_palette(n = 1, name = "Darjeeling1"), name = "", breaks = c("A"), label = c("Resíduos"),  guide = "legend") +
  theme_minimal()

# Analisando o gráfico, pode-ser perceber certa heterocedasticidade e, talvez,
# um padrão de dependência. Os testes a seguir estabelecem critérios objetivos
# para nortear essa avaliação.

# Normalidade - Teste Shapiro-Wilk
shapiro.test(reg$residuals)

# O teste shapiro-wilk reforça a normalidade dos resíduos ao não rejeitar a
# hipótese nula de normalidade dos resíduos.

# Homocedasticidade - Teste Breusch-Pagan
bptest(reg)

# O teste Breusch-Pagan reforça a homocedasticidade dos resíduos ao não rejeitar
# a hipótese nula de homocedasticidades dos resíduos.


# Independência - Teste autocorrelação
dwtest(reg)

# O teste de auto-correlação aponta para uma dependência entre os resíduos ao
# rejeitar a hipótese de independência dos resíduos. Essa premissa enfraquece
# o modelo. Sugere-se que medidas corretivas sejam aplicadas ajustar o modelo.


# FINAL RESPOSTA QUESTÃO 3