### 1ª AP 2021.1
### Técnica de Análise de Dados de Transportes
### Prof. Dr. Manoel Mendonça de Castro Neto
### Aluno: Nelson de O. Quesado Filho
### Matrícula: 504117


## 1ª Questão - Avaliação da Rugosidade de Agregado para Pavimentação Asfáltica

library(tidyverse)
dados <- c(57.7, 76.1, 101.9, 110.5, 131.4, 147.4, 159.7, 162.1, 168.3, 169.5, 183.0, 195.3, 197.7, 199.0, 205.1, 205.1, 219.8, 222.3, 240.7, 241.9, 260.4, 267.7, 278.8, 283.7, 283.7, 292.3, 299.7, 310.7, 310.7, 325.5, 336.5, 342.7, 354.9, 359.8, 375.8, 393.0, 401.6, 426.2, 434.8, 438.5, 458.1, 459.3, 463.0, 475.3, 479.0, 498.6, 507.2, 515.8, 533.0, 546.5, 550.2, 555.1, 555.1, 573.5, 587.1, 592.0, 601.8, 605.5, 623.9, 648.5, 663.2, 689.0, 713.6, 766.4, 794.6, 800.8, 837.6, 918.7, 937.1, 945.7 , 949.4)

# a) (0,5) Que problema motivou o seu interesse em analisar esses dados? Qual é o
# objetivo da sua análise?

# RESPOSTA: A geometria dos grãos, por compor de 85% a 94% do peso total das misturas
# asfálticas,influenciam vários aspectos do desempenho da pavimentação, como permeabilidade,
# rigidez, trabalhabilidade, durabilidde, resistência, segurança e custo.
# Ainda, no Brasil, a indústria da construção civil, em particular o setor de infraestrutura
# viária, é a que mais consome agregados minerais. As projeções de crescimento deste
# setor a longo prazo joga luz sobre a insustentabilidade da exploração mineral, 
# demandando um consumo mais racional deste recurso limitado (LAGO, 2018; DYER et.
# al., 2021).
# Reconhecendo a grande importância da correta especificação e seleção dos agregados
# na indústria da pavimentação asfáltica, esta análise se objetiva a caracterizar
# corretamente determinado agregado a partir de uma amostra de 71 observações utilizando
# inferência estatística.


# b) (0,5) Detalhe o seu método de análise, que deve incluir uma análise descritiva
# da amostra e uma análise da adequabilidade de alguma distribuição de probabilidade
# vista em sala para representar o comportamento populacional da variável.

# RESPOSTA: O método de análise é dividido em três etapas: (I) análise descritiva, (II) 
# verificação de aderência a uma distribuição de probabilidade conhecida e (III) verificação
# do atendimento do requisito (rugosidade entre baixa à moderada).

# (I) A análise descritiva apresenta a tendência central (a partir da média e da mediana),
# a dispersão (a partir do desvio padrão, variância e coeviciente de variância),
# e a forma (a partir de gráficos de dispersão, diagrama de caixa, histograma e 
# densidade).

# (II) A partir da análise descritiva, da comparação da distribuição da amostra
# com as distribuições conhecidas (normal ou exponencial) e da reflexão sobre a
# natureza do fenômeno estudado, decide-se sobre a distribuição de probabilidade
# que será considerada como boa representação da população de onde se extraiu a
# amostra. A aderência da amostra à distribuição conhecida escolhida é verificada
# através de análises gráficas (distrbuição de frequências, gráfico de probabilidade
# acumulada e gráfico quantil-quantil), e do teste Shapiro-Wilk. Este método testa
# a hipótese nula em que determinada amostra se origina de uma população normalmente
# distribuída (Royston, 1982).

# (III) Para avaliação do que é solicitado, será considerado a probabilidade da população
# possuir rugosidade entre 440 e 825, considerando a distribuição de probabilidade
# conhecida testada e as estatísticas da amostra (média e desvio padrão, por exemplo).
# O agregado será aceito caso no mínimo 90% da população esteja classificada de
# acordo com a rugosidade desejada. Este é um critério arbitrário definido pelo analista.

# c) (3,0) Apresente e discuta os resultados da sua análise. 

# RESPOSTA:
# (I) Análise Descritiva

# A variável (rugosidade) do agregado é contínua e, caso o agregado não seja beneficiada,
# pode ser considerada de ocorrência natural, o que geralmente é bem representado
# pela distribuição normal (MONTGOMERY e RUNGER, 2016, cap. 5).

desc <- data.frame(n = length(dados), media = mean(dados), desvio_padrao = sd(dados), variancia = var(dados), coef_var = sd(dados)/mean(dados))
print(desc) # principais estatísticas descritivas

# A análise descritiva apresenta as principais estatísticas da amostra, dentre elas
# a média (422,76), desvio padrão (230,75) e coeficiente de variância (0,55).

quartis <- summary(dados)
print(quartis) # apresentação dos quartis

# Os quartis mostram certa proximidade entre média e mediana, o que pode reforçar
# certa normalidade da amostra (MONTGOMERY e RUNGER, 2016, cap. 5)..

# Gráfico de Dispersão
data.frame(dados) %>% ggplot() +
  geom_point(aes(c(1:71),dados), color = "#FC0D85") +
  labs(title = "Dispersão", subtitle = "n = 71", x = "Observação", y = "Rugosidade") +
  theme_minimal()

# Ainda, a análise dos quartis e da dispercão apresentam uma distribuição razoavelmente
# contínua ao longo da amostra com certa assimetria à direita.

# Boxplot
data.frame(dados) %>% ggplot() +
  geom_boxplot(aes(dados), color = "#FC0D85") +
  labs(title = "Diagrama de Caixa", subtitle = "n = 71", x = "Rugosidade") +
  theme_minimal()

# O diagrama de caixa confirma a assimetria à direita, sem a ocorrência de outliers.

# Histograma de Frequência
data.frame(dados) %>% ggplot(aes(dados)) +
  geom_histogram(fill = "#FC0D85", binwidth = 80, alpha = .75) +
  labs(title = "Histograma de Frequência", subtitle = "n = 71", x = "Rugosidade", y = "Frequência") +
  theme_minimal()

# O histograma, quando produzido com classe de tamanho 80 (escolhido arbitrariamente
# pelo analista após tentativas com diferentes valores), apresenta uma distribuição
# de frequência com crescimento até aproximadamente 300, e decrescimento a partir
# de então. A assimetria à direita também é identificada nesta representação

# Densidade de Probabilidade
data.frame(dados) %>% ggplot(aes(dados)) +
  geom_density(fill = "#FC0D85", alpha = 0.75) +
  labs(title = "Densidade de Probabilidade", subtitle = "n = 71", x = "Rugosidade", y = "Probabilidade")+
  theme_minimal()

# O gráfico de densidade de frequência corrobora o que é discutido a partir do hitograma
# de requência

# (II) Comparação da amostra com distribuições de probabilidade conhecidas.

# Histograma + Densidade Normal e Exponencial
data.frame(dados) %>% ggplot() +
  geom_histogram(aes(dados, y = ..density.., fill = "#444444"), bins = 25) +
  geom_density(aes(qnorm(seq(0.001, .99, length = 71), mean(dados), sd(dados)), fill = "#FC0D85"), alpha = 0.3) +
  geom_density(aes(qexp(seq(0.001, .99, length = 71), 1/mean(dados)), fill = "#750CF2"), alpha = 0.3) +
  scale_fill_identity(name = "Distribuição", breaks = c("#444444", "#FC0D85", "#750CF2"), labels = c("Amostra", "Normal", "Exponencial "), guide = "legend") +
  labs(title = "Histograma de Frequência, Densidade Normal e Exponencial", subtitle = "n = 71", x = "Rugosidade", y = "Probabilidade") +
  theme_minimal()

# Densidade Amostra, Normal e Exponencial
data.frame(dados) %>% ggplot() +
  geom_density(aes(dados, fill = "#444444")) +
  geom_density(aes(qnorm(seq(0.001, .99, length = 71), mean(dados), sd(dados)), fill = "#FC0D85"), alpha = 0.3) +
  geom_density(aes(qexp(seq(0.001, .99, length = 71), 1/mean(dados)), fill = "#750CF2"), alpha = 0.3) +
  scale_fill_identity(name = "Distribuição", breaks = c("#444444", "#FC0D85", "#750CF2"), labels = c("Amostra", "Normal", "Exponencial"), guide = "legend") +
  labs(title = "Densidade Amostra, Normal e Exponencial", subtitle = "n = 71", x = "Rugosidade", y = "Probabilidade") +
  theme_minimal()

# Partindo da análise visual do gráfico apresentado, decide-se por testar a aderência
# da amostra à distribuição normal, pois entende-se que esta é a que mais se aproxima
# da distribuição de frequência da amostra e é coerente com os argumentos apresentados
# na análise descritiva realizada até então.

# Teste de aderência à distribuição normal.

# Histograma de frequência observada vs. distribuição normal.

data.frame(dados = c(dados, qnorm(seq(0.01, 0.99, length = 71), desc$media, desc$desvio_padrao)), Distribuicao = rep(c("Amostra", "Normal"), times = c(71, 71))) %>%
  ggplot() +
  geom_histogram(aes(dados, fill = Distribuicao), position = "dodge", bins = 15) + scale_fill_manual(values = c("#666666", "#FC0D85")) +
  labs(title = "Histograma de Frequência", subtitle = "n = 71", x = "Rugosidade", y = "Frequência") +
  theme_minimal()

# Este gráfico mostra certa semelhança estre as distribuições analisadas, com exceção
# aos valores mais centrais. Espera-se que as diferenças encontradas sejam despresíveis.

# Distribuição de Probabilidade Acumulada
ggplot() +
  stat_ecdf(aes(dados, color = "#FC0D85")) +
  stat_ecdf(aes(qnorm(seq(0.01, 0.99, length = 120), desc$media, desc$desvio_padrao), color = "#222222")) +
  labs(title = "Distribuição Acumulada", x = "Rugosidade", y = "Probabilidade") + scale_color_identity(name = "Curva", breaks = c("#222222", "#FC0D85"), labels = c("Normal", "Amostra "), guide = "legend") +
  theme_minimal()

# A distribuição acumulada reforça a proximidade da distribuição amostral com a
# distribuição normal.

# Gráfico QQ
p <- seq(0.05, 0.95, 0.05) #base do quantil: de 5% a 95% a cada 5%
observed_quantiles <- quantile(dados, p)
theoretical_quantiles <- qnorm(p, mean = desc$media, sd = desc$desvio_padrao)

data.frame(Quantis_Teoricos = theoretical_quantiles, Quantis_Observados = observed_quantiles) %>%
  ggplot(aes(Quantis_Teoricos, Quantis_Observados)) +
  geom_point(color = "#222222") +
  labs(title = "Gráfico Quantil-Quantil", subtitle = "n = 71", x = "Quantis Teóricos", y = "Quantis Observados") +
  geom_abline(slope = 1, color = "#FC0D85") +
  xlim(50, 1000) +
  ylim(50, 1000) +
  theme_minimal()

# A proximidade dos pontos à reta de referência no gráfico quantil-quantil reforça
# a análise feita até então

# Teste shapiro-wilko
shapiro.test(dados)

# O teste Shapiro-Wilko confirma que há 95,2% de chance da amostra se originar de
# uma distribuição normal, considerando o valor P de 0.009.


# (III) Verificaçao do requisito: rugosidade entre 440 e 825.
pnorm(825, desc$media, desc$desvio_padrao) - pnorm(440, desc$media, desc$desvio_padrao)

# Considerando que a distribuição normal representa bem a população de onde a amostra
# foi coletada e que possui mesma média e mesmo desvio padrão, pode-se concluir
# que 42,96% da população (agregados) deve possuir rugosidade entre 440 e 825.

# A partir do critério definido, o agregado não deve ser utilizado.


# d) (0,5) Explicite as premissas (suposições) da sua análise.

# RESPOSTA: Como premissas do método aplicado, pode-se citar as seguintes:
# (i) A amostra representa bem a população.
# (ii) A população pode ser bem representada pela distribuição normal.
# (iii) As pequenas diferenças encontradas entre a distribuição amostral e a distribuição
# normal são despresíveis para a compreensão da população.
# (iv) O critério estabelecido para aceitação do agregado é adequado.

# Referências Bibligráficas
# Dyer, P. P., Silva, S. A., Klinsky, L. M. G., Coppio, G. L., & de Lima, M. G. (2021). Caracterização de areia descartada de fundição como agregado para pavimentação. TRANSPORTES, 29(1), 148–160. https://doi.org/10.14295/transportes.v29i1.2241
# Lago, L. N. Avaliação geológico-geotécnica de agregados de rocha granulítica para uso em pavimentação asfáltica. Dissertação (mestrado) – Pontifícia Universidade Católica do Rio de Janeiro, Departamento de Engenharia Civil, 2018.
# Montgomery, D.C. e Runger, G.C. (2016) Estatística Aplicada e Probabilidade para Engenheiros. LTC – Livros Técnicos e Científicos.
# Royston, P. (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.

#### FIM DA 1A QUESTÃO

## 2ª Questão - Dimensionamento de Assentos em Ponto de Embarque de Ônibus

dados2 <- c(146.8, 34.7, 170, 24.3, 28, 74.2, 48.6, 75.1, 119.1, 38.3, 24.9, 72, 65.5, 214.7, 7.2, 10.2, 23.5, 90.1, 57.5, 54.7, 37.3, 249.9, 30.3, 22.2, 96.7, 32.5, 152.6, 10, 13.1, 46.1, 15.1, 6.8, 62.1, 50, 0.7, 132.5, 17.5, 7.7, 72, 1.7, 105.4, 43.1, 110.4, 158.8, 0.4, 123.3, 85.1, 30.9, 176.2, 23.4, 89.8, 85.8, 11.3, 91.4, 8, 96.6, 15.2, 30.3, 83.8, 14.4, 114.7, 28.1, 5.4, 27.3, 91.6, 11.1, 9, 28.9, 238, 98.8, 19.6, 20, 34, 3, 6.4, 22.9, 132.8, 42.6, 109.8, 90.6, 146, 23, 219.6, 164.2, 57.7, 28.3, 22.4, 166.7, 1.6, 223.8, 18.7, 76.1, 18.3, 99.8, 92.8, 93.4, 276.1, 10.3, 98.5, 101.2, 42.9, 16.4, 33, 4.1, 11.9, 36.2, 2.2, 2.3, 107.4, 21.3, 103.4, 19.2, 56, 176.5, 41.2, 98.6, 6, 3.1, 5.3, 35.4, 14.2, 37.6, 24.8, 69.7, 46.6, 30.4, 13.8, 93.7, 43, 73, 17.2, 51.2, 43.1, 54.9, 42, 88.4, 15.6, 3.9, 39.3, 1.8, 17.9, 16.9, 3.1, 49, 80.9, 61.5, 108.8, 7.2, 86.7, 32.5, 70.4, 1.3, 6.2, 40.1, 103.5, 110.2, 50.1, 69.8, 20.7, 60.3, 53.2, 19.9, 58.4, 158.5, 71.9, 266, 31.7, 3.4, 99.4, 11, 14.3, 108.3, 30.3, 101.1, 60.2, 33.1, 103, 109.7, 34.2, 8.1, 20.7, 85.3, 37.8, 80.7, 13, 34.2, 59.7, 104.7, 39.3, 69.3, 31.8, 13.7, 34, 64.6, 62.5, 26.3, 100.7, 104, 134.2, 80.5)

# a) (0,5) Que problema motivou o seu interesse em analisar esses dados?
# Qual é o objetivo da sua análise?

# O objetivo desta análise é dimensionar a quantidade de assentos em um ponto de
# embarque de ônibus de maneira a garantir que, em 95% das vezes, as chegadas de
# usuários seja igual ou menor que a quantidade de assentos disponíveis.

# Dessa forma, espera-se otimizar o espaço disponível para os pedestre que ali circulam,
# garantindo conforto os usuários que aguardam o ônibus e mantendo o uso racional
# dos recursos disponíveis para a construção do abrigo de espera,

# b) (0,5)  Detalhe o seu método de análise, que deve incluir a análise descritiva
# da amostra e a análise da adequabilidade de alguma distribuição de probabilidade
# vista em sala para representar o comportamento populacional da variável.

# O método se inicia com a descrição da amostra, através das medidas de tendência
# central, dispersão e forma. Em seguida, utilizando análise visual-gráfica e comparando
# a média com a variância, verifica-se a proximidade da distribuição da amostra com
# a distribuição de poisson.

# Por fim, caso poisson represente adequadamente a amostra, utiliza-se esta distribuição
# teórica para estimar a quantidade de chegada de usuários dentro do itnervalo de tempo
# de chegada de ônibus.

# c) (3,0) Apresente e discuta os resultados da sua análise. 

minuto_chegada <- cumsum(dados2/(60))

qtdclass <- function(x) { # função que conta ocorrências na classe (de 5 em 5 minutos)
  sum(minuto_chegada < x) - sum(minuto_chegada < x -5)
}

chegada <- data.frame(tempo = seq(5, 205, 5)) %>% mutate(ocorrencia = sapply(tempo, qtdclass))

print(chegada)
# ANALISE DESCRITIVA

desc2 <- data.frame(n = length(chegada$ocorrencia), media = mean(chegada$ocorrencia), desvio_padrao = sd(chegada$ocorrencia), coef_var = sd(chegada$ocorrencia)/mean(chegada$ocorrencia), variancia = var(chegada$ocorrencia))

print(desc2) # principais estatísticas descritivas

desc2$variancia/desc2$media # a variância é muito próxima da média, reforçando poisson.

quartis2 <- summary(chegada$ocorrencia)
print(quartis2) # apresentação dos quartis

# Dispersão
chegada %>% ggplot() +
  geom_point(aes(c(1:41), ocorrencia), color = "#E6B900") +
  ylim(0, 11) +
  labs(title = "Dispersão", subtitle = "n = 41", x = "Observação", y = "Ocorrências (usuários/5 min)") +
  theme_minimal()

# Boxplot
chegada %>% ggplot() +
  geom_boxplot(aes(ocorrencia), color = "#E6B900") +
  labs(title = "Diagrama de Caixa", subtitle = "n = 41", x = "Ocorrências (usuários/5 min)") +
  theme_minimal()

# Histograma + Densidade
chegada %>% ggplot(aes(ocorrencia)) +
  geom_histogram(aes(y = ..density..), bins = 10) +
  geom_density(fill = "#E6B900", alpha = 0.5) +
  labs(title = "Histograma de Frequência e Densidade", subtitle = "n = 41", x = "Ocorrências (usuários/5 min)", y = "Probabilidade") +
  theme_minimal()

# Distribuição de Frequência (Comparativo)
data.frame(ocorrencia = c(chegada$ocorrencia, qpois(seq(0.01, 0.99, length = 41), desc2$media)), Distribuicao = rep(c("Amostra", "Poisson"), times = c(41, 41))) %>%
  ggplot() +
  geom_histogram(aes(ocorrencia, fill = Distribuicao), position = "dodge", bins = 15) + scale_fill_manual(values = c("#666666", "#E6B900")) +
  labs(title = "Histograma de Frequência", subtitle = "n = 41", x = "Ocorrências (usuários/5 min)", y = "Frequência") +
  theme_minimal()

# Densidade de Probabilidade (Comparativo)

chegada %>% ggplot() +
  geom_density(aes(ocorrencia, fill = "#444444")) +
  geom_density(aes(qpois(seq(0.001, .99, length = 41), desc2$media), fill = "#E6B900"), alpha = 0.3) +
  scale_fill_identity(name = "Distribuição", breaks = c("#444444", "#E6B900"), labels = c("Amostra", "Poisson"), guide = "legend") +
  labs(title = "Densidade Amostra e Poisson", subtitle = "n = 41", x = "Ocorrência (usuários/5 min)", y = "Probabilidade") +
  theme_minimal()

# Distribuição de Probabilidade Acumulada
ggplot() +
  stat_ecdf(aes(chegada$ocorrencia, color = "#E6B900")) +
  stat_ecdf(aes(qpois(seq(0.01, 0.99, length = 41), desc2$media), color = "#222222")) +
  labs(title = "Distribuição Acumulada", x = "Ocorrência (usuários/5 min)", y = "Probabilidade") + scale_color_identity(name = "Curva", breaks = c("#222222", "#E6B900"), labels = c("Poisson", "Amostra "), guide = "legend") +
  theme_minimal()

# Gráfico QQ
p2 <- seq(0.05, 0.95, 0.05) #base do quantil: de 5% a 95% a cada 5%
observed_quantiles2 <- quantile(chegada$ocorrencia, p2)
theoretical_quantiles2 <- qpois(p2, desc2$media)
data.frame(Quantis_Teoricos = theoretical_quantiles2, Quantis_Observados = observed_quantiles2) %>%
  ggplot(aes(Quantis_Teoricos, Quantis_Observados)) +
  geom_point(color = "#222222") +
  labs(title = "Gráfico Quantil-Quantil", subtitle = "n = 41", x = "Quantis Teóricos", y = "Quantis Observados") +
  geom_abline(slope = 1, color = "#E6B900") +
  theme_minimal()

# A análise visual aponte que poison pode representar bem a distribuição da amostra observada.
# Calcula-se, através da distribuição acumulada de poisson, quantas pessoas chegam,
# no máximo, em 95% das vezes, durante o período de 5 minutos.

qpois(.95, desc2$media) # 9 assentos garantem que, em 95% das vezes, nenhuma pessoa
# precise agaurdar em pé pela chegada do ônibus.

# d) (0,5) Explicite as premissas (suposições) da sua análise.

# RESPOSTA: Como premissas do método aplicado, pode-se citar as seguintes:
# (i) A amostra representa bem a população.
# (ii) A população pode ser bem representada pela distribuição de poisson
# (iii) As pequenas diferenças encontradas entre a distribuição amostral e a distribuição
# de poisson são despresíveis para a compreensão da população.
# (iv) O critério estabelecido para dimensionamento é adequado.
# (v) Qualquer ônibus que chega atende à todos os usuários que estão aguardando.

#### FIM DA 2A QUESTÃO

## 3ª Questão - O papel da Analise Descritiva e da Probabilidade como ferramenta de trabalho.

# RESPOSTA:
# A análise descritiva e de probabilidade permite estimar a distribuição de uma
# população a partir de um conjunto de observações. Na engenharia, esta competência
# é extramente útil pelas limitações de se observar a população como um todo: impossibilidade
# de experimentação, escassez de dados, recursos limitados para coleta de dados, entre outros.
# Assim, o ferramental estatística ajuda a estimar o comportamento de variáveis e
# fenômenos com segurança conhecida a partir de amostras destas populações.
# Modelos de engenharia podem se basear nessas estiamtivas e ajudar a prover analistas
# com ferramentas de previsão úeis para os problemas comumente encontrados.
# Como exemplo das questões apresentadas neste exame, foi possível tomar decisões
# sobre o aceite de um estoque de agregados (população) a partir de uma amostra,
# bem como estimar e dimensionar, com segurança conhecida, o nível de serviço de
# determinado abrigo de embarque do transporte público, a partir de uma amostra.

#### FIM DA 3A QUESTÃO

#### FIM DA RESOLUCAO DA AVALIACAO

# Subindo no github