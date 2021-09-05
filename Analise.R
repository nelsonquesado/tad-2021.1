library(tidyverse)
library(wesanderson)

amostra <- c(0.5,	1.2,	1.2,	1.5,	1.6,	2.0, 2.2,	2.6,	3.0,	3.7,	4.1,	5.3)
print(amostra)

tab <- data.frame(media = mean(amostra), desvpad = sd(amostra))
print(tab)

hist(amostra)
boxplot(amostra)

ggplot(data = data.frame(amostra),aes(x = c(1:length(amostra)), y = amostra)) +
  geom_point(aes(color = "A")) +
  geom_boxplot(aes(color = "B"), alpha = 0) +
  scale_color_manual(values = wes_palette(name = "Royal1", n = 2), guide = NULL) +
  labs(title = "Dispersão e Diagrama de Caixa", subtitle = paste("n = ", length(amostra)), x = "Amostra", y = "") +
  theme_minimal()
