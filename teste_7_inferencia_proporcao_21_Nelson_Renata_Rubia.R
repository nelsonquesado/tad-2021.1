### UNIVERSIDADE FEDERAL DO CEARÁ
### PROGRAMA DE PÓS-GRADUAÇÃO EM ENGENHARIA DE TRANSPORTES
### TCP7321 – TÉCNICA DE ANÁLISE DE DADOS     		PROF. MANOEL CASTRO NETO

### Teste 7 – Estimação da proporção										

### Grupo: Nelson, Renata e Rubia
### Data: 11/07/21

# bibliotecas utilizadas
library(tidyverse)

# Explicite as premissas das suas análises

# 

# 01)	Você foi contratada para realizar um diagnóstico de segurança viária de
# uma ciclovia. Um dos indicadores utilizados foi a proporção de ciclistas
# trafegando fora da ciclovia (ciclistas vulneráveis). Responda cada item e
# sempre apresente as premissas da sua análise.


# a)	Você quer estimar a proporção de ciclistas vulneráveis por meio de um
# intervalo de confiança com erro de 2,5% (para mais ou para menos) e um grau de
# confiança de 95%. Dimensione a amostra.

conf <- .975
w <- 0.05 # amplitude de +- 2,5%
z <- qnorm(conf)

n <- ((z^2)*.25)/(w^2) # pelo Triola Cap 7
print(paste("São necessárias ", n, "observações.")) # Resposta item a)

# b)	Você coletou uma amostra de 100 ciclistas e observou que 28 deles
# trafegaram fora da ciclovia. Estime a proporção de ciclistas vulneráveis por
# meio de um intervalo com grau de confiança de 95%.

n <- 100
p <- 28/n
q <- 1 - p
conf <- .975
z <- qnorm(conf)
dp <- sqrt((p*q)/n)


inf <- p - z*dp
sup <- p + z*dp

print(paste("Com um intervalo de confança de 95%, estima-se que a proporção de ciclistas vulneráveis é de ", inf, " a ", sup)) # Resposta item b)

# 02)	A prefeitura lhe contratou para realizar um novo estudo, cujo objetivo era
# inferir se a proporção de ciclistas vulneráveis era maior do que 10%, pois se
# esse for o caso, melhorias na ciclovia seriam realizadas, como repavimentação
# e retirada de obstáculos. Para essa análise, você decidiu coletar uma amostra
# n=300 ciclistas, dos quais cinquenta foram observados trafegando fora da
# ciclovia. Utilize um teste de hipóteses para a sua análise, com erro tipo I,
# ou seja, nível de significância () de 5%. Estabeleça as premissas da sua
# análise.

n <- 300
p <- 50/n
q <- 1 - p
conf <- .95
z <- qnorm(conf)
dp <- sqrt(p*q/n)


# H0: P <= .1 hipótese a ser testada
# H1: P > .1

sup <- p + z*sqrt((p*q)/n)

ggplot() +
geom_density(aes(qnorm(seq(0.01, 0.99, 0.01), .1, )))