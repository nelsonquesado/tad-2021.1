##### UNIVERSIDADE FEDERAL DO CEARÁ
##### CENTRO DE TECNOLOGIA - DEPARTAMENTO DE ENGENHARIA DE TRANSPORTES
##### TCP7321 – TAD				  		PROF. MANOEL CASTRO 

##### Atividade 5 - Inferência da Média							27/06/21

##### Grupo: Nelson, Renata e Rúbia.

# Bibliotecas utilizadas
library(tidyverse)
library(gridExtra)

# 01)  Considerem X uma variável que representa o tempo, em minutos, que um
# veículo tem que aguardar no semáforo da interseção da Av. 13 de Maio com a Av.
# da Universidade. Os dados a seguir foram obtidos por amostragem aleatória
# simples, ao longo da hora de pico da tarde, de um dia útil típico. 

# 0,2	0,5	0,7	1,1	1,2	1,3	1,4	1,4	1,5	1,5
# 1,6	1,6	1,7	1,9	2,0	2,1	2,2	2,5	2,6	2,9		


# a) Qual a distribuição de probabilidades que melhor pode representar o
# comportamento da média de cada amostra aleatória (X̅) que poderia ser
# retirada da população de X? Qual é a média e o desvio padrão de X̅?
# Indiquem as premissas.

# RESPOSTA: De acordo com o Teorema do Limite Central, a distribuição de médias
# amostrais aleatórias e idenpendentes de uma população, a partir de determinado
# tamanho (n), deve se aproximar de uma distribuição normal.
# Ainda, independente do Teorema do Limite Central, é sabido que a distribuição
# de médias amostrais aleatórias de uma população possui média igual à µ (média
# populacional) e S (desvio padrão das médias) igual à ∂/√n.

# Como premissas para o que foi apresentado, temos que (i) o tamanho amostral deve
# ser suficientemente grande - usualmente maior que 30 -, (ii) a amostras devem
# ser independentes e aleatórias e (iii) as observações da população devem ser
# independentes.


# b) Com base na amostra coletada, estimem o tempo médio de espera dos veículos
# para passarem pela interseção, com grau de confiança de 90%. Suponha que o
# desvio padrão populacional de X é igual a 1,0 minuto. Indiquem as premissas.

dados <- c(0.2, 0.5, 0.7, 1.1, 1.2, 1.3, 1.4, 1.4, 1.5, 1.5, 1.6, 1.6, 1.7, 1.9, 2.0, 2.1, 2.2, 2.5, 2.6, 2.9)

n <- length(dados)
media <- mean(dados)
dp <- 1/sqrt(n)

inf <- qnorm(0.05, media, dp)
sup <- qnorm(0.95, media, dp)

graf_1 <- data.frame(normal = qnorm(seq(0.05, 0.95, length = 20), media, dp, lower.tail = TRUE)) %>% ggplot() +
  geom_density(aes(normal, fill = "#FC0D85"), alpha = .5) +
  geom_vline(aes(xintercept = inf, color = "#FC0D85"), size = 1, linetype = 2) +
  geom_vline(aes(xintercept = media, color = "#444444"), size = 0.5) +
  geom_vline(aes(xintercept = sup, color = "#750CF2"), size = 1, linetype = 2) +
  xlim(1, 2.2) +
  scale_fill_identity(name = "Distribuição", breaks = "#FC0D85", labels = "Normal", guide = "legend") +
  scale_color_identity(name = "Intervalo de Confiança", breaks = c("#FC0D85", "#444444", "#750CF2"), labels = c(paste("Inferior - (", round(inf, 2), " min)"),paste("Média - (", round(media, 2), " min)"), paste("Superior - (", round(sup,2), " min)")), guide = "legend") + 
  labs(title = "Média e Intervalo de Confiança (90%)", subtitle = paste("n = ", n, " S = ", round(dp, 4)), x = "Atraso Médio (min)") +
  ylim(0, 3) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

graf_1

# O tempo médio de espera (atraso) estimado para a interseção é de 1,59 minutos,
# com intervalo  de 1,23 a 1,96 minutos para um grau de confiança de 90%.
# Premissas: o tempo de espera é uma variável independente; (ii) o tamanho da
# amostras (20) é fuciente para a aplicação do TLC; (iii) a amostra é bem
# representatitva da população. 


# c) Refaça o item b, mas considerando que o desvio padrão populacional de X é
# desconhecido. Indiquem as premissas.

dp2 <- sd(dados)/sqrt(n)

inf2 <- qt(0.05, df = n -1) * dp2 + media
sup2 <- qt(0.95, df = n - 1) * dp2 + media



graf_2 <- data.frame(student = qt(seq(0.05, 0.95, length = 20), df = n - 1) * dp2 + media) %>% ggplot() +
  geom_density(aes(student, fill = "yellow", y = ..density..), alpha = .5) +
  geom_vline(aes(xintercept = inf2, color = "orange"), size = 1, linetype = 2) +
  geom_vline(aes(xintercept = media, color = "#444444"), size = 0.5) +
  geom_vline(aes(xintercept = sup2, color = "dark orange"), size = 1, linetype = 2) +
  scale_fill_identity(name = "Distribuição", breaks = "yellow", labels = "T-Student", guide = "legend") +
  scale_color_identity(name = "Intervalo de Confiança", breaks = c("orange", "#444444", "dark orange"), labels = c(paste("Inferior - (", round(inf2, 2), " min)"),paste("Média - (", round(media, 2), " min)"), paste("Superior - (", round(sup2,2), " min)")), guide = "legend") + 
  labs(title = "Média e Intervalo de Confiança (90%)", subtitle = paste("n = ", n, " S = ", round(dp2, 4)), x = "Atraso Médio (min)") +
  xlim(1, 2.2) +
  ylim(0, 3) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

graf_2

# RESPOSTA
# O tempo médio de espera (atraso) estimado para a interseção não se altera
# (1,59 min). O entervalo, entretanto, apresenta novos valores de 1,33 a 1,86
# minutos para um grau de confiança de 90%.

# Percebe-se que, apesar da distribuição t-student teórica apresentar uma forma
# mais dispersa do que a normal, este item considera o desvio populacional em
# função do desvio padrão amostral, resultando em um valor de dispersão menor do
# aquele aplicado no item b) deste exercício e, por conseqência, um intervalo
# de confiança menor.

# Premissas: o tempo de espera é uma variável independente; (ii) o tamanho da
# amostras (20) é fuciente para a aplicação do TLC; (iii) a amostra é bem
# representatitva da população; (iv)o desvio padrão considerado é o amostral;
# e (v) a distribuição t-student representa bem esta a variável estudada.

# Comparação entre os resultados encontrados nos comandos b) e c).
grid.arrange(graf_1, graf_2)

# 02)  Suponha que um tempo médio acima de 1,0 min requer melhorias na
# interseção.Façam um teste de hipóteses para verificar a necessidade de se
# implantar melhorias. 

# Premissas: intervalo de confiança de 90%, desvio padrão da população em função 
# do desvio padrão amostral e distribuição t-student para estimativa de probabilidade.

# Hipóteses:
# H0 -> µ > 1 min
# Ha -> µ <= 1 min (a ser testada)

dp3 <- sd(dados)/sqrt(n)

inf3 <- qt(0.1, df = n -1) * dp2 + media


data.frame(student = qt(seq(0.05, 0.95, length = 20), df = n - 1) * dp2 + media) %>% ggplot() +
  geom_density(aes(student, fill = "green", y = ..density..), alpha = .5) +
  geom_vline(aes(xintercept = inf3, color = "green"), size = 1, linetype = 2) +
  geom_vline(aes(xintercept = media, color = "#444444"), size = 0.5) +
  geom_vline(aes(xintercept = 1, color = "red"), size = 0.5) +
  scale_fill_identity(name = "Distribuição", breaks = "green", labels = "T-Student", guide = "legend") +
  scale_color_identity(name = "Intervalo de Confiança", breaks = c("green", "#444444", "red"), labels = c(paste("Inferior - (", round(inf2, 2), " min)"),paste("Média - (", round(media, 2), " min)"), "1 min"), guide = "legend") + 
  labs(title = "Média e Intervalo de Confiança (90%)", subtitle = paste("n = ", n, " S = ", round(dp2, 4)), x = "Atraso Médio (min)") +
  xlim(0.5, 2.5) +
  ylim(0, 3) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

# Abordagem escore T

t_inf <- (inf3 - media)/dp3
t_1min <- (1 - media)/dp3

print(c(t_inf, t_1min))

t_inf <= t_1min

# Não há evidência amostral suiciente para apoiar a afirmativa de que a média
# média populacional do atraso dos veículos na interseção indicada seja igual 
# ou menor que 1 minuto. Logo, a hipótese alterantiva (Ha) pode ser rejeitada.
# Deve decidir a favor da implantação de melhorias.
