library(tidyverse)
library(wesanderson)
library(viridis)
library(RColorBrewer)

# UNIVERSIDADE FEDERAL DO CEARÁ
# PROGRAMA DE PÓS-GRADUAÇÃO EM ENGENHARIA DE TRANSPORTES – PETRAN/UFC
# TCP 7321 – TÉCNICAS DE ANÁLISE DE DADOS EM TRANSPORTES  -   PROF. MANOEL CASTRO

# Atividade 9 – Análise de experimentos com ANOVA 											

# Grupo: Nelson Quesado e Rubia Rodrigues     Data: 02/08/21

# Seguem abaixo dados coletados de um experimento que tem como objetivo estudar
# o efeito do teor de modificante (0%, 10% e 30%) na adesividade (POTS) de
# ligantes modificados com seiva de aveloz. Avalie somente para a condição seca.
seca <- data.frame(modificante = rep(c("0%", "10%", "30%"), times = c(4, 4, 4)), cp = rep(c(1, 2, 3, 4), times = 3), pots = c(3.0, 3.4, 3.6, 3.2, 2.8, 2.7, 2.7, 2.6, 3.8, 3.4, 4.1, 3.4))

# Premissas:
# Amostras aleatórias, independentes e se origine de distribuição normal.
# Os desvios padrão entre grupos são "iguais" = homocedasticidade (maior < 2x menor)

# Análise Descritiva
seca %>% ggplot() +
  geom_boxplot(aes(x = pots, y = modificante, fill = modificante), color = "#222222") +
  geom_point(aes(x = pots, y = modificante, fill = modificante), size = 2, alpha = 0.7) +
  labs(title = "Diagrama de Caixa da Adesividade", subtitle = "n = 4 (por amostra)", x = "POTS (Psi)") +
  scale_fill_manual(values = wes_palette(name = "Royal1", n = 3)) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

seca %>% ggplot() +
  geom_histogram(aes(x = pots, fill = modificante, y = ..density..), position = "dodge", color = "#222222", bins = 9) +
  geom_density(aes(x = pots, fill = modificante), color = "#222222", alpha = .5) +
  labs(title = "Histograma da Adesividade", subtitle = "n = 4 (por amostra)", x = "POTS (Psi)") +
  scale_fill_manual(values = wes_palette(name = "Royal1", n = 3)) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

# Variâncias
mod0 <- seca %>% filter(modificante == "0%") %>% .$pots
mod10 <- seca %>% filter(modificante == "10%") %>% .$pots
mod30 <- seca %>% filter(modificante == "30%") %>% .$pots

sd0 <- sd(mod0)
sd10 <- sd(mod10)
sd30 <- sd(mod30)

print(c(sd0, sd10, sd30))   # um dos desvios padrão é muito difernete?
                            # quebra de premissa?

# Normalidade (apresentam)
shapiro.test(mod0)  # Nao rejeita normalidade
shapiro.test(mod10) # Nao rejeita normalidade
shapiro.test(mod30) # Nao rejeita normalidade

# Realizando o Teste ANOVA
anova <- aov(pots ~ modificante, data = seca)
q <-summary(anova)
print(q)
str(q[[1]])

# Representação Gráfica
# Ho : não há diferença entre as médias (hipótese a ser testada)
# Ha : há diferença entre as médias
# Erro tipo I (alfa): 5% (significância)

# Parâmetros
n <- nrow(seca)
k <- length(unique(seca$modificante))
df1 <- k - 1
df2 <- n - k

# Area de nao rejeicao
alfa <- .05
sombra <- data.frame(x = qf(seq(0.0001, 1 - alfa, .001), df1,df2)) %>%
  mutate(y = df(x, df1, df2))
curva <- data.frame(x = qf(seq(0.0001, 0.9999, .001), df1,df2)) %>%
  mutate(y = df(x, df1, df2))

# Plotagem
ggplot() +
  geom_line(data = curva, aes(x, y, color = "D")) +
  geom_area(data = sombra, aes(x, y, fill = "A"), alpha = 0.4) +
  geom_vline(aes(xintercept = q[[1]]$`F value`[1], color = "B"), size = 0.85) +
  geom_vline(aes(xintercept = max(sombra$x), color = "C"), linetype = 2, size = 0.85) +
  labs(title = "Representação Gráfica do Teste ANOVA", subtitle = paste("df1 =", df1, "| df2 =", df2, "| alfa =", alfa), x = "POTS (Psi)") + 
  scale_fill_manual(values = wes_palette(name = "Royal1", n = 4)[4], breaks = c("A"), label = c("Área de não rejeição"), name = "", guide = "legend") +
  scale_color_manual(values = c(wes_palette(name = "Royal1", n = 2), "#222222"), breaks = c("B", "C", "D"), label = c("Valor-F", "Limite IC", "Distribuição F"), name = "", guide = "legend") +
  geom_text(aes(x = 13, y = .9, label = paste("Valor-F =", round(q[[1]]$`F value`)[1], 1)), size = 4, color = wes_palette(name = "Royal1", n = 1)) + 
  geom_text(aes(x = 3.5, y = .9, label = paste("F-Crítico =", round(max(sombra$x), 1))), size = 4, color = wes_palette(name = "Royal1", n = 2)[2]) + 
  theme_minimal() +
  xlim(0, 17) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

# Teste Anova no Braco
v_entre <- var(c(mean(mod0), mean(mod10), mean(mod30)))*4 # nao esquecer de multiplicar por n, pois se trata do quadrado do erro padrão, uma vez que estamos tratando da variância das médias
v_dentro <- mean(c(var(mod0), var(mod10), var(mod30)))

valor_f <- v_entre/v_dentro
print(valor_f)

1 - pf(valor_f, df1, df2) # valor-p no braco

# Tukey Honest Differences
tk_test <- TukeyHSD(anova)
tk_test
plot(tk_test) # refazer bonito ggplot!!!!!

str(tk_test)
view(tk_test)

# Tratando os dados para plotar com GGPLOT2
ic_dif <- data.frame(tk_test[1]) %>%
  mutate(modificante = rownames(data.frame(tk_test[1])))

print(ic_dif)
view(ic_dif)

ic_dif %>% ggplot() +
  geom_line(aes(x = c(modificante.lwr[1], modificante.diff[1], modificante.upr[1]), y = modificante[1], color = "A"), size = 1) + # modificante  0% ~ 10%
  geom_point(aes(x = c(modificante.lwr[1], modificante.diff[1], modificante.upr[1]), y = modificante[1]), size = 1) + # modificante  0% ~ 10%
  geom_line(aes(x = c(modificante.lwr[2], modificante.diff[2], modificante.upr[2]), y = modificante[2], color = "B"), size = 1) + # modificante  0% ~ 30%
  geom_point(aes(x = c(modificante.lwr[2], modificante.diff[2], modificante.upr[2]), y = modificante[2]), size = 1) + # modificante  0% ~ 30%
  geom_line(aes(x = c(modificante.lwr[3], modificante.diff[3], modificante.upr[3]), y = modificante[3], color = "C"), size = 1) + # modificante  30% ~ 10%
  geom_point(aes(x = c(modificante.lwr[3], modificante.diff[3], modificante.upr[3]), y = modificante[3]), size = 1) + # modificante  30% ~ 10%
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "#222222") +
  scale_color_manual(values = brewer.pal(n = 3, name = "Set2"), breaks = c("A", "B", "C"), label = c("0% ~ 10%", "0% ~ 30%", "30% ~ 10%"), name = "Par", guide = "legend") +
  labs(title = "Representação do Tukey Honest Test", subtitle = "alfa = 5% | n = 4 (por grupo)", x = "Diferença Entre Grupos", y = "Par de Modificantes") +
  xlim(-1.75, 1.75) +
  theme_minimal()

# Pode-se concluir que o modificante 10% é o diferente

# Distribuições Normal | Considerando Ho a dispersão é homogênea

# variância_dentro
ggplot() +
  geom_line(aes(x = qnorm(seq(.001, .999, .001))*sqrt(v_dentro) + mean(mod0), y = dnorm(qnorm(seq(.001, .999, .001)))), color = "#222222") +
  geom_area(aes(x = qnorm(seq(.025, .975, .001))*sqrt(v_dentro) + mean(mod0), y = dnorm(qnorm(seq(.025, .975, .001))), fill = "A"), alpha = .5) +
  geom_vline(aes(xintercept = mean(mod0), color = "A"), linetype = "dashed", size = 1) +
  geom_line(aes(x = qnorm(seq(.001, .999, .001))*sqrt(v_dentro) + mean(mod10), y = dnorm(qnorm(seq(.001, .999, .001)))), color = "#222222") +
  geom_area(aes(x = qnorm(seq(.025, .975, .001))*sqrt(v_dentro) + mean(mod10), y = dnorm(qnorm(seq(.025, .975, .001))), fill = "B"), alpha = .5) +
  geom_vline(aes(xintercept = mean(mod10), color = "B"), linetype = "dashed", size = 1) +
  geom_line(aes(x = qnorm(seq(.001, .999, .001))*sqrt(v_dentro) + mean(mod30), y = dnorm(qnorm(seq(.001, .999, .001)))), color = "#222222") +
  geom_area(aes(x = qnorm(seq(.025, .975, .001))*sqrt(v_dentro) + mean(mod30), y = dnorm(qnorm(seq(.025, .975, .001))), fill = "C"), alpha = .5) +
  geom_vline(aes(xintercept = mean(mod30), color = "C"), linetype = "dashed", size = 1) +
  scale_fill_manual(values = brewer.pal(n = 3, name = "Set2"), breaks = c("A", "B", "C"), label = c("0%", "10%", "30%"), name = "Modificante", guide = "legend") +
  scale_color_manual(values = brewer.pal(n = 3, name = "Set2"), breaks = c("A", "B", "C"), guide = NULL) +
  labs(title = "Estimativas das Médias", subtitle = "n = 4 (por grupo)", x = "POTS (Psi)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  xlim(1.8, 4.7) +
  ylim(0, .5)
  
# erro padrão n(12)
ggplot() +
  geom_line(aes(x = qnorm(seq(.001, .999, .001))*(sd(seca$pots)/sqrt(12)) + mean(mod0), y = dnorm(qnorm(seq(.001, .999, .001)))), color = "#222222") +
  geom_area(aes(x = qnorm(seq(.025, .975, .001))*(sd(seca$pots)/sqrt(12)) + mean(mod0), y = dnorm(qnorm(seq(.025, .975, .001))), fill = "A"), alpha = .5) +
  geom_vline(aes(xintercept = mean(mod0), color = "A"), linetype = "dashed", size = 1) +
  geom_line(aes(x = qnorm(seq(.001, .999, .001))*(sd(seca$pots)/sqrt(12)) + mean(mod10), y = dnorm(qnorm(seq(.001, .999, .001)))), color = "#222222") +
  geom_area(aes(x = qnorm(seq(.025, .975, .001))*(sd(seca$pots)/sqrt(12)) + mean(mod10), y = dnorm(qnorm(seq(.025, .975, .001))), fill = "B"), alpha = .5) +
  geom_vline(aes(xintercept = mean(mod10), color = "B"), linetype = "dashed", size = 1) +
  geom_line(aes(x = qnorm(seq(.001, .999, .001))*(sd(seca$pots)/sqrt(12)) + mean(mod30), y = dnorm(qnorm(seq(.001, .999, .001)))), color = "#222222") +
  geom_area(aes(x = qnorm(seq(.025, .975, .001))*(sd(seca$pots)/sqrt(12)) + mean(mod30), y = dnorm(qnorm(seq(.025, .975, .001))), fill = "C"), alpha = .5) +
  geom_vline(aes(xintercept = mean(mod30), color = "C"), linetype = "dashed", size = 1) +
  scale_fill_manual(values = brewer.pal(n = 3, name = "Set2"), breaks = c("A", "B", "C"), label = c("0%", "10%", "30%"), name = "Modificante", guide = "legend") +
  scale_color_manual(values = brewer.pal(n = 3, name = "Set2"), breaks = c("A", "B", "C"), guide = NULL) +
  labs(title = "Estimativas das Médias", subtitle = "n = 4 (por grupo)", x = "POTS (Psi)") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  xlim(1.8, 4.7) +
  ylim(0, .5)
