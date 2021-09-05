# UNIVERSIDADE FEDERAL DO CEARÁ
# PROGRAMA DE MESTRADO EM ENGENHARIA DE TRANSPORTES – PETRAN/UFC
# TCP 7321 – TÉCNICA DE ANÁLISE DE DADOS EM TRANSPORTES  -   PROF. MANOEL CASTRO

# Teste 3 – Distribuição de Poisson 			 					31/05/21

# NELSON, RENATA E RUBIA

# 01)	Suponha que você foi contratado para avaliar a segurança viária de uma interseção
# semaforizada. Esta interseção foi palco de 73 acidentes nos últimos 20 anos.
# Suponha que você decide instalar um equipamento de fiscalização eletrônica na interseção
# e que irá observar a ocorrência de acidentes nos próximos seis meses. Responda:

library(tidyverse)

dados <- data.frame(semestre = c(1:40), No_Acidentes = c(1, 1, 2, 2, 2, 5, 0, 2, 1, 1, 0, 3, 1, 4, 2, 4, 5, 3, 2, 4, 2, 1, 1, 1, 2, 1, 0, 3, 1, 1, 1, 2, 3, 0, 2, 1, 3, 1, 1, 1))
plot(dados)

# (a) Qual é a probabilidade de, no semestre de avaliação, nenhum acidente ocorrer.
# Estabeleça as premissas da sua análise.

# Premissas:
# 1 - será considerada apenas os sinistros ocorridos nos últimos 4 anos, pois
# entende-se que dados muito antigos podem não representar bem a realidade atual.
# 2 - os eventos são considerados independenes.
# 3 - para que seja possivel utilizar o mesmo lambda, considera-se que o equipamento
# de fiscalização eletrônica é instalado fora do alcance de visão do motorista, de
# maneira a não influenciar o seu comportamento.

dados_4anos <- dados %>% filter(!semestre < 33) # filtro para os ultimos 8 semestres
lambda_4anos <- mean(dados_4anos$No_Acidentes) # lambda para os ultimos 8 semestres
print(lambda_4anos)

prob_0 <- dpois(0, lambda_4anos) # calculo da probabilidade de 0 ocorrências
print(prob_0) # RESPOSTA 22,3%
  
# (b)	O que a resposta do item (a) sugere sobre o seu método de avaliação da
# eficácia do equipamento na redução de acidentes?

# RESPOSTA: A não ocorrência de acidentes no 1o semestre após a intervenção não aponta
# eficácia (ou sequer efeito) da sua instalação. Mesmo sem a instalação da fiscalização
# eletrônica, as chances de não ocorrer acidentes é de 22,3%, valor considerado ainda alto
# pela equipe de avaliação. A falta de um contrafactual dificulta ainda mais esta análise.
# Consideramos este método de análise inadequado.# 

# (c)	Proponha outro método de avaliação, cujo critério seja baseado em probabilidade?

# RESPOSTA: Sugerimos aqui duas abordagens:

# Abordagem 1 - ampliar o período de avaliação para 1 ano, considerando 0 acidentes.
lambda_anual_4anos <- 2 * lambda_4anos # lambda anual
prob_0 <- dpois(0, lambda_anual_4anos) # probabilidade de 0 ocorrências em 1 ano
print(prob_0) # 5% de probabilidade de ocorrer um acidente 
# Percebe-se que a chance de não ocorrer acidentes em 1 ano é de 5%, valor baixo na 
# visão dos analistas. Assim, a equipe entende que pode-se suspeitar de um efeito
# benéfico da intervenção, caso não ocorram acidentes no período de 1 ano.

# Abordagem 2 - incluir outras interseções, considerando dois grupos, sendo um grupo
# de controle e outro grupo de teste, montando um "quase-experimento", e avaliando
# a diferença na diferença entre os dois grupos, considerando  antes e depois
# da intervenção.
     